use super::{Err, Kind, Record, Value, DB};
use crate::crypt;
use crate::lex::Token;
use std::collections::HashSet;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref FIELD_RX: Regex = Regex::new("[a-zA-Z%][a-zA-Z0-9_]*").unwrap();
    static ref ENUM_RX: Regex = Regex::new("[a-zA-Z0-9][a-zA-Z0-9_-]*").unwrap();
}

#[derive(Debug, Default)]
pub(crate) struct Parser {
    db: DB,
    current_record: Option<Record>,
}

impl Parser {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn parse(mut self, tokens: Vec<Token>) -> Result<DB, Err> {
        let mut records = Vec::new();

        for token in tokens.iter() {
            match token {
                Token::Keyword(keyword, value) => {
                    let args = value.split_whitespace().collect();
                    self.parse_keyword(keyword, args)?
                }
                Token::Field(key, value) => self.parse_field(key, value)?,
                Token::Blank => {
                    if let Some(rec) = self.current_record.take() {
                        records.push(rec);
                    }
                }
            }
        }

        if let Some(rec) = self.current_record.take() {
            records.push(rec);
        }

        self.db.records = records;

        Ok(self.db)
    }

    fn parse_keyword(&mut self, key: &str, args: Vec<&str>) -> Result<(), Err> {
        match key {
            "rec" => self.db.rectype = Some(args.get(0).ok_or("expected rec type")?.to_string()),
            "key" => self.db.primary_key = Some(args.get(0).ok_or("expected key")?.to_string()),
            "doc" => self.db.doc = Some(args.join(" ")),
            "sort" => {
                let field = args.get(0).ok_or("expected sort field")?.to_string();
                self.db.sort_field = Some(field)
            }
            "type" => {
                let field_name = args.get(0).ok_or("expected field name")?.to_string();
                let kind = parse_type(args)?;

                let meta = self.db.types.entry(field_name).or_default();

                meta.kind = kind;
            }
            "confidential" => {
                for field in args {
                    let meta = self.db.types.entry(field.to_owned()).or_default();
                    if !matches!(meta.kind, Kind::Line) {
                        return Err("confidential fields are always lines".into());
                    }
                    meta.kind = Kind::Confidential;
                }
            }
            "mandatory" | "allowed" | "prohibited" => {
                for field in args {
                    let meta = self.db.types.entry(field.to_owned()).or_default();
                    meta.constraint = Some(key.parse()?);
                }
            }
            "unique" => {
                for field in args {
                    let meta = self.db.types.entry(field.to_owned()).or_default();
                    meta.unique = true
                }
            }

            key => todo!("{}", key),
        };

        Ok(())
    }

    // TODO use Cow
    fn parse_field(&mut self, key: &str, value: &str) -> Result<(), Err> {
        let mut rec = self.current_record.take().unwrap_or_default();

        if !self.db.fields.contains(&key.to_owned()) {
            self.db.fields.insert(rec.len(), key.to_owned());
        }

        let meta = self.db.types.entry(key.to_owned()).or_default();
        let val = parse_value(&meta.kind, value)?;

        rec.insert(key.to_owned(), val);

        // put it back
        self.current_record = Some(rec);
        Ok(())
    }
}

fn parse_value(kind: &Kind, val: &str) -> Result<Value, Err> {
    use Value::*;

    Ok(match kind {
        Kind::Line => Line(val.to_owned()),
        Kind::Int => Int(val.parse()?),
        Kind::Real => Real(val.parse()?),
        Kind::Bool => match val {
            "true" | "yes" | "1" => Bool(true),
            "false" | "no" | "0" => Bool(false),
            _ => return Err(format!("unexpected boolean value: {}", val).into()),
        },
        Kind::Date => todo!("date parsing"),
        Kind::Email => {
            if !val.contains('@') {
                // yes
                return Err(format!("invalid email adress: {}", val).into());
            }
            Email(val.to_owned())
        }
        Kind::UUID => UUID(val.parse()?),
        Kind::Confidential => {
            if !val.starts_with(crypt::ENCRYPTED_PREFIX) {
                return Err("possibly unencrypted confidential value".into());
            }
            Confidential(val.to_owned())
        }
        Kind::Range(min, max) => {
            let n = val.parse()?;
            if n < *min || n > *max {
                return Err("value is out of range".into());
            }
            Range(n)
        }
        Kind::Regexp(rx) => {
            if !rx.is_match(val) {
                return Err(format!("{} does not match required format", val).into());
            }
            Regexp(val.to_owned())
        }
        Kind::Viz(_) => Viz(val.to_owned()), // We can't validate that other database has the key
        Kind::Enum(variants) => {
            if !variants.contains(&val.to_lowercase()) {
                return Err(format!("invalid enum value: {}", val).into());
            }
            Enum(val.to_lowercase())
        }
    })
}

fn parse_type(args: Vec<&str>) -> Result<Kind, Err> {
    use Kind::*;

    let &tt = args.get(1).ok_or("expected field name")?;

    if !FIELD_RX.is_match(&tt) {
        return Err(format!("invalid field name: {}", tt).into());
    }

    Ok(match tt {
        "line" => Line,
        "int" => Int,
        "real" => Real,
        "bool" => Bool,
        "date" => Date,
        "email" => Email,
        "uuid" => UUID,
        "range" => {
            let from;
            let to;

            if args.len() > 3 {
                from = parse_bound(args.get(2).ok_or("expected start range index")?)?;
                to = parse_bound(args.get(3).ok_or("expected end range index")?)?;
            } else {
                from = 0;
                to = parse_bound(args.get(2).ok_or("expected end range index")?)?;
            }

            if from > to {
                return Err("impossible range".into());
            }

            Kind::Range(from, to)
        }
        "regexp" => {
            let rx = args
                .get(2)
                .ok_or("expected regexp definition as third field")?
                .strip_prefix("/")
                .ok_or("expected regexp to begin with slash")?
                .strip_suffix("/")
                .ok_or("expected regexp to end with slash")?;

            let compiled = Regex::new(rx)?;
            Regexp(compiled)
        }
        "viz" => {
            let key = args
                .get(2)
                .ok_or("expected field reference as second field")?;

            match FIELD_RX.is_match(key) {
                true => Kind::Viz(key.to_string()),
                false => return Err(format!("invalid viz value: {}", key).into()),
            }
        }
        "enum" => {
            if args.len() < 3 {
                return Err("expected at least one enum variant".into());
            }

            let mut variants = HashSet::with_capacity(args.len() - 2);

            for v in args.iter().skip(2).map(|s| s.to_string()) {
                match ENUM_RX.is_match(&v) {
                    true => variants.insert(v.to_lowercase()),
                    false => return Err(format!("invalid enum value: {}", v).into()),
                };
            }

            Enum(variants)
        }
        _ => return Err(format!("unknown type: {}", tt).into()),
    })
}

fn parse_bound(v: &str) -> Result<isize, Err> {
    Ok(match v {
        "MIN" => isize::MIN,
        "MAX" => isize::MAX,
        v => v.parse()?,
    })
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Meta, Constraint};

    #[test]
    fn parser_rec_type() {
        let db = DB::new("%rec: Book").unwrap();
        assert_eq!(db.rectype, Some("Book".to_owned()));
    }

    #[test]
    fn parser_type_line() {
        let db = DB::new("%type: Book line").unwrap();
        let meta = db.types.get(&"Book".to_owned()).unwrap();
        assert!(matches!(meta.kind, Kind::Line))
    }

    #[test]
    fn parser_regex() {
        let db = DB::new("%type: Phone regexp /^[0-9]{10}$/").unwrap();
        let meta = db.types.get(&"Phone".to_owned()).unwrap();

        assert!(matches!(meta.kind, Kind::Regexp(_)));

        assert!(parse_value(&meta.kind, &"0123456789".to_owned()).is_ok());
        assert!(parse_value(&meta.kind, &"blah".to_owned()).is_err());
    }

    #[test]
    fn parser_enum() {
        let db = DB::new("%type: Status enum Loading Done Error").unwrap();
        let meta = db.types.get(&"Status".to_owned()).unwrap();

        if let Kind::Enum(ref variants) = meta.kind {
            assert_eq!(variants.len(), 3);
            assert!(variants.contains("loading"));
            assert!(variants.contains("done"));
            assert!(variants.contains("error"));
        } else {
            panic!("unexpected type")
        }

        assert!(parse_value(&meta.kind, &"Done".to_owned()).is_ok());
        assert!(parse_value(&meta.kind, &"blah".to_owned()).is_err());
    }

    #[test]
    fn parser_range() {
        let kind = parse_type(vec!["field", "range", "MIN", "MAX"]).unwrap();
        assert!(matches!(kind, Kind::Range(isize::MIN, isize::MAX)));

        let kind = parse_type(vec!["field", "range", "0", "15"]).unwrap();
        assert!(matches!(kind, Kind::Range(0, 15)));

        let kind = parse_type(vec!["field", "range", "15"]).unwrap();
        assert!(matches!(kind, Kind::Range(0, 15)));

        // Impossible range
        assert!(parse_type(vec!["field", "range", "30", "15"]).is_err());
    }

    #[test]
    fn parser_confidential() {
        let db = DB::new("%confidential: Password Token").unwrap();

        assert!(matches!(
            db.types.get("Password").unwrap(),
            Meta {
                kind: Kind::Confidential,
                ..
            }
        ));
        assert!(matches!(
            db.types.get("Token").unwrap(),
            Meta {
                kind: Kind::Confidential,
                ..
            }
        ));

        assert!(matches!(
            parse_value(&Kind::Confidential, "encrypted-AABBCC").unwrap(),
            Value::Confidential(_)
        ));
        assert!(parse_value(&Kind::Confidential, "blah").is_err());
    }

    #[test]
    fn parser_fields_order() {
        const TEXT: &str = "
%type: Login line
%type: Password line

Login: blah
Password: chickens
Website: example.com

Login: bruh
Password: qwerty
Notes: very secure password
";
        let db = DB::new(TEXT).unwrap();
        let expected = vec!["Login", "Password", "Notes", "Website"]
            .iter()
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>();

        assert_eq!(db.fields, expected)
    }

    /// see manual 1.2 A Litte Example
    #[test]
    fn parser_1_2_a_little_example() {
        const TEXT: &str = "
# -*- mode: rec -*-

%rec: Book
%mandatory: Title
%type: Location enum loaned home unknown
%doc:
+ A book in my personal collection.

Title: GNU Emacs Manual
Author: Richard M. Stallman
Publisher: FSF
Location: home

Title: The Colour of Magic
Author: Terry Pratchett
Location: loaned

Title: Mio Cid
Author: Anonymous
Location: home

Title: chapters.gnu.org administration guide
Author: Nacho Gonzalez
Author: Jose E. Marchesi
Location: unknown

Title: Yeelong User Manual
Location: home

# End of books.rec
";
        let db = DB::new(TEXT).unwrap();

        // check fields
        let fields_expected = vec!["Title", "Author", "Publisher", "Location"]
            .iter()
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>();
        assert_eq!(db.fields, fields_expected);

        // check primary key
        assert_eq!(db.primary_key, None);

        // check sort field
        assert_eq!(db.sort_field, None);

        // check doc
        assert_eq!(db.doc, Some("A book in my personal collection.".to_owned()));

        // check types and constraints
        let type_title = db.types.get("Title");
        let type_location = db.types.get("Location");
        let type_author = db.types.get("Author");
        let type_publisher = db.types.get("Publisher");

        assert!(type_title.is_some());
        assert!(type_location.is_some());
        assert!(type_author.is_some());
        assert!(type_publisher.is_some());

        //Title constraint mandatory
        let type_title2 = type_title.unwrap();
        assert!(!type_title2.unique);
        assert!(type_title2.constraint.is_some());
        assert!(matches!(type_title2.constraint.as_ref().unwrap(), Constraint::Mandatory));
        assert!(matches!(type_title2.kind, Kind::Line));

        //Location enum loaned home unknown
        let type_location2 = type_location.unwrap();
        assert!(!type_location2.unique);
        assert!(type_location2.constraint.is_none());
        assert!(matches!(type_location2.kind, Kind::Enum(_)));
        match &type_location2.kind {
            Kind::Enum(set) => {
                let mut left = set.iter().collect::<Vec<_>>();
                left.sort();
                let mut right = vec!["loaned", "home", "unknown"];
                right.sort();
                assert_eq!(left, right);
            }
            _ => { assert!(false); }
        }

        //Author optional no-constraints
        let type_author2 = type_author.unwrap();
        assert!(!type_author2.unique);
        assert!(type_author2.constraint.is_none());
        assert!(matches!(type_author2.kind, Kind::Line));

        //Publisher optional no-constraints
        let type_publisher2 = type_publisher.unwrap();
        assert!(!type_publisher2.unique);
        assert!(type_publisher2.constraint.is_none());

        // check record values
        assert_eq!(db.records.len(), 5);

        assert_eq!(db.records[0].len(), 4);
        assert!(db.records[0].contains_key("Title"));
        assert!(db.records[0].contains_key("Author"));
        assert!(db.records[0].contains_key("Publisher"));
        assert!(db.records[0].contains_key("Location"));
        assert_eq!(db.records[0].get("Title").unwrap(), &Value::Line("GNU Emacs Manual".to_owned()));
        assert_eq!(db.records[0].get("Author").unwrap(), &Value::Line("Richard M. Stallman".to_owned()));
        assert_eq!(db.records[0].get("Publisher").unwrap(), &Value::Line("FSF".to_owned()));
        assert_eq!(db.records[0].get("Location").unwrap(), &Value::Enum("home".to_owned()));

        assert_eq!(db.records[1].len(), 3);
        assert!(db.records[1].contains_key("Title"));
        assert!(db.records[1].contains_key("Author"));
        assert!(db.records[1].contains_key("Location"));
        assert_eq!(db.records[1].get("Title").unwrap(), &Value::Line("The Colour of Magic".to_owned()));
        assert_eq!(db.records[1].get("Author").unwrap(), &Value::Line("Terry Pratchett".to_owned()));
        assert_eq!(db.records[1].get("Location").unwrap(), &Value::Enum("loaned".to_owned()));

        assert_eq!(db.records[2].len(), 3);
        assert!(db.records[2].contains_key("Title"));
        assert!(db.records[2].contains_key("Author"));
        assert!(db.records[2].contains_key("Location"));
        assert_eq!(db.records[2].get("Title").unwrap(), &Value::Line("Mio Cid".to_owned()));
        assert_eq!(db.records[2].get("Author").unwrap(), &Value::Line("Anonymous".to_owned()));
        assert_eq!(db.records[2].get("Location").unwrap(), &Value::Enum("home".to_owned()));

        assert_eq!(db.records[3].len(), 3);
        assert!(db.records[3].contains_key("Title"));
        assert!(db.records[3].contains_key("Author"));
        assert!(db.records[3].contains_key("Location"));
        assert_eq!(db.records[3].get("Title").unwrap(), &Value::Line("chapters.gnu.org administration guide".to_owned()));
        assert_eq!(db.records[3].get_vec("Author").unwrap()[0], Value::Line("Nacho Gonzalez".to_owned()));
        assert_eq!(db.records[3].get_vec("Author").unwrap()[1], Value::Line("Jose E. Marchesi".to_owned()));
        assert_eq!(db.records[3].get("Location").unwrap(), &Value::Enum("unknown".to_owned()));

        assert_eq!(db.records[4].len(), 2);
        assert!(db.records[4].contains_key("Title"));
        assert!(db.records[4].contains_key("Location"));
        assert_eq!(db.records[4].get("Title").unwrap(), &Value::Line("Yeelong User Manual".to_owned()));
        assert_eq!(db.records[4].get("Location").unwrap(), &Value::Enum("home".to_owned()));
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_field_example() {
        const TEXT: &str = "
Name: Ada Lovelace
";
        let db = DB::new(TEXT).unwrap();

        // untyped recordset
        assert!(db.rectype.is_none());
        // 1 record
        assert_eq!(db.records.len(), 1);
        // 1 field on that record
        assert_eq!(db.records[0].len(), 1);
        // contains just field "Name"
        assert_eq!(db.records[0].contains_key("Name"), true);
        // field "Name" has just 1 value
        assert_eq!(db.records[0].is_vec("Name"), false);
        // name is a Line type
        let name = db.records[0].get("Name").unwrap();
        match &name {
            Value::Line(thestr) => {
                // Name is Ada Lovelace
                assert_eq!(*thestr, "Ada Lovelace".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_field_name_regular_expression1() {
        const TEXT: &str = "
$rec-omatic: Customer
";
        match DB::new(TEXT) {
            Ok(_) => {
                // not good, should not return Ok
                assert!(false);
            },
            Err(_) => {
                // that is OK, should return Err
                assert!(true);
            }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_field_name_regular_expression2() {
        const TEXT: &str = "
1test: Customer
";
        match DB::new(TEXT) {
            Ok(_) => {
                // not good, should not return Ok
                assert!(false);
            },
            Err(_) => {
                // that is OK, should return Err
                assert!(true);
            }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_field_name_regular_expression3() {
        const TEXT: &str = "
-notgood: Customer
";
        match DB::new(TEXT) {
            Ok(_) => {
                // not good, should not return Ok
                assert!(false);
            },
            Err(_) => {
                // that is OK, should return Err
                assert!(true);
            }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_field_name_regular_expression4() {
        const TEXT: &str = "
good-but-not-good: Customer
";
        match DB::new(TEXT) {
            Ok(_) => {
                // not good, should not return Ok
                assert!(false);
            },
            Err(_) => {
                // that is OK, should return Err
                assert!(true);
            }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_field_name_regular_expression5() {
        const TEXT: &str = "
underscores_ok: Customer
";
        match DB::new(TEXT) {
            Ok(_) => {
                // that is OK, should return Ok
                assert!(true);
            },
            Err(_) => {
                // not good, should not return Err
                assert!(false);
            }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_field_name_regular_expression6() {
        const TEXT: &str = "
FooNotgood!: Customer
";
        match DB::new(TEXT) {
            Ok(_) => {
                // not good, should not return Ok
                assert!(false);
            },
            Err(_) => {
                // that is OK, should return Err
                assert!(true);
            }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_field_name_regular_expression7() {
        const TEXT: &str = "
!: Customer
";
        match DB::new(TEXT) {
            Ok(_) => {
                // not good, should not return Ok
                assert!(false);
            },
            Err(_) => {
                // that is OK, should return Err
                assert!(true);
            }
        }
    }
}
