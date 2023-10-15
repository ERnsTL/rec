use super::{Err, Kind, Record, Value, DB};
use crate::{crypt, RecordSet};
use crate::lex::Token;
use std::collections::HashSet;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref FIELD_RX: Regex = Regex::new("^[a-zA-Z%][a-zA-Z0-9_]*$").unwrap();   //TODO performance or accuracy with ^ and $ compared to without?
    static ref ENUM_RX: Regex = Regex::new("^[a-zA-Z0-9][a-zA-Z0-9_-]*$").unwrap();
}

#[derive(Debug, Default)]
pub(crate) struct Parser {
    db: DB,
    current_record: Option<Record>,
    current_recordset: usize,   // index of recordset vector that we are currently filling into
    have_something: bool,
}

impl Parser {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn parse(mut self, tokens: Vec<Token>) -> Result<DB, Err> {
        self.have_something = false;

        for token in tokens.iter() {
            match token {   //TODO recognize beginning of untyped recordset
                Token::Keyword(keyword, value) => {
                    let args = value.split_whitespace().collect();
                    self.parse_keyword(keyword, args)?;
                    self.have_something = true;
                },
                Token::Field(key, value) => {
                    self.parse_field(key, value)?;
                    self.have_something = true;
                }
                Token::Blank => {
                    if let Some(rec) = self.current_record.take() {
                        self.have_something = true;
                        self.db.recordsets[self.current_recordset].records.push(rec);   //TODO optimize push into this long chain of indirections or have a local empty records = Vec::new() ? or keep a reference to db.recordsets[self.current_recordset].records ?
                    }
                }
            }
        }

        if let Some(rec) = self.current_record.take() {
            self.have_something = true;
            self.db.recordsets[self.current_recordset].records.push(rec);   //TODO optimize same as above - indirections or indirect first and insert into a resolved records variable?
        }

        Ok(self.db)
    }

    fn parse_keyword(&mut self, key: &str, args: Vec<&str>) -> Result<(), Err> {
        // first recordset can be triggered by any keyword
        if !self.have_something {
            // add first recordset, any keyword can trigger this
            self.db.recordsets.push(RecordSet::default());
            // current_recordset is already 0
        }

        match key {
            "rec" => {
                if self.have_something {
                    // add additional recordset
                    self.db.recordsets.push(RecordSet::default());
                    self.current_recordset += 1;
                }
                self.db.recordsets[self.current_recordset].rectype = Some(args.get(0).ok_or("expected rec type")?.to_string())
            },
            "key" => self.db.recordsets[self.current_recordset].primary_key = Some(args.get(0).ok_or("expected key")?.to_string()),
            "doc" => self.db.recordsets[self.current_recordset].doc = Some(args.join(" ")),
            "sort" => {
                let field = args.get(0).ok_or("expected sort field")?.to_string();
                self.db.recordsets[self.current_recordset].sort_field = Some(field)
            }
            "type" => {
                let field_name = args.get(0).ok_or("expected field name")?.to_string();
                let kind = parse_type(args)?;

                let meta = self.db.recordsets[self.current_recordset].types.entry(field_name).or_default();

                meta.kind = kind;
            }
            "confidential" => {
                for field in args {
                    let meta = self.db.recordsets[self.current_recordset].types.entry(field.to_owned()).or_default();
                    if !matches!(meta.kind, Kind::Line) {
                        return Err("confidential fields are always lines".into());
                    }
                    meta.kind = Kind::Confidential;
                }
            }
            "mandatory" | "allowed" | "prohibited" => {
                for field in args {
                    let meta = self.db.recordsets[self.current_recordset].types.entry(field.to_owned()).or_default();
                    meta.constraint = Some(key.parse()?);
                }
            }
            "unique" => {
                for field in args {
                    let meta = self.db.recordsets[self.current_recordset].types.entry(field.to_owned()).or_default();
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

        // check for field name regular expression, see manual 2.1 Fields - field name regular expression
        if !FIELD_RX.is_match(key) {
            return Err("non-conforming field name".into());
        }

        if !self.have_something {
            // add first recordset
            self.db.recordsets.push(RecordSet::default());
            // current_recordset is already 0
        }

        if !self.db.recordsets[self.current_recordset].fields.contains(&key.to_owned()) {
            self.db.recordsets[self.current_recordset].fields.insert(rec.len(), key.to_owned());
        }

        let meta = self.db.recordsets[self.current_recordset].types.entry(key.to_owned()).or_default();
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
    use crate::{Meta, Constraint, FlatSize};

    #[test]
    fn parser_rec_type() {
        let db = DB::new("%rec: Book").unwrap();
        assert_eq!(db.recordsets[0].rectype, Some("Book".to_owned()));
    }

    #[test]
    fn parser_type_line() {
        let db = DB::new("%type: Book line").unwrap();
        let meta = db.recordsets[0].types.get(&"Book".to_owned()).unwrap();
        assert!(matches!(meta.kind, Kind::Line))
    }

    #[test]
    fn parser_regex() {
        let db = DB::new("%type: Phone regexp /^[0-9]{10}$/").unwrap();
        let meta = db.recordsets[0].types.get(&"Phone".to_owned()).unwrap();

        assert!(matches!(meta.kind, Kind::Regexp(_)));

        assert!(parse_value(&meta.kind, &"0123456789".to_owned()).is_ok());
        assert!(parse_value(&meta.kind, &"blah".to_owned()).is_err());
    }

    #[test]
    fn parser_enum() {
        let db = DB::new("%type: Status enum Loading Done Error").unwrap();
        let meta = db.recordsets[0].types.get(&"Status".to_owned()).unwrap();

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
            db.recordsets[0].types.get("Password").unwrap(),
            Meta {
                kind: Kind::Confidential,
                ..
            }
        ));
        assert!(matches!(
            db.recordsets[0].types.get("Token").unwrap(),
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

        assert_eq!(db.recordsets[0].fields, expected);
    }

    /// see manual 1.2 A Litte Example
    #[test]
    fn parser_1_2_a_little_example() {
        const TEXT: &str = "# -*- mode: rec -*-

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
        let rs = &db.recordsets[0];

        // check fields
        let fields_expected = vec!["Title", "Author", "Publisher", "Location"]
            .iter()
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>();
        assert_eq!(rs.fields, fields_expected);

        // check primary key
        assert_eq!(rs.primary_key, None);

        // check sort field
        assert_eq!(rs.sort_field, None);

        // check doc
        assert_eq!(rs.doc, Some("A book in my personal collection.".to_owned()));

        // check types and constraints
        let type_title = rs.types.get("Title");
        let type_location = rs.types.get("Location");
        let type_author = rs.types.get("Author");
        let type_publisher = rs.types.get("Publisher");

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
        assert_eq!(rs.records.len(), 5);

        assert_eq!(rs.records[0].len(), 4);
        assert!(rs.records[0].contains_key("Title"));
        assert!(rs.records[0].contains_key("Author"));
        assert!(rs.records[0].contains_key("Publisher"));
        assert!(rs.records[0].contains_key("Location"));
        assert_eq!(rs.records[0].get("Title").unwrap(), &Value::Line("GNU Emacs Manual".to_owned()));
        assert_eq!(rs.records[0].get("Author").unwrap(), &Value::Line("Richard M. Stallman".to_owned()));
        assert_eq!(rs.records[0].get("Publisher").unwrap(), &Value::Line("FSF".to_owned()));
        assert_eq!(rs.records[0].get("Location").unwrap(), &Value::Enum("home".to_owned()));

        assert_eq!(rs.records[1].len(), 3);
        assert!(rs.records[1].contains_key("Title"));
        assert!(rs.records[1].contains_key("Author"));
        assert!(rs.records[1].contains_key("Location"));
        assert_eq!(rs.records[1].get("Title").unwrap(), &Value::Line("The Colour of Magic".to_owned()));
        assert_eq!(rs.records[1].get("Author").unwrap(), &Value::Line("Terry Pratchett".to_owned()));
        assert_eq!(rs.records[1].get("Location").unwrap(), &Value::Enum("loaned".to_owned()));

        assert_eq!(rs.records[2].len(), 3);
        assert!(rs.records[2].contains_key("Title"));
        assert!(rs.records[2].contains_key("Author"));
        assert!(rs.records[2].contains_key("Location"));
        assert_eq!(rs.records[2].get("Title").unwrap(), &Value::Line("Mio Cid".to_owned()));
        assert_eq!(rs.records[2].get("Author").unwrap(), &Value::Line("Anonymous".to_owned()));
        assert_eq!(rs.records[2].get("Location").unwrap(), &Value::Enum("home".to_owned()));

        assert_eq!(rs.records[3].len(), 3);
        assert!(rs.records[3].contains_key("Title"));
        assert!(rs.records[3].contains_key("Author"));
        assert!(rs.records[3].contains_key("Location"));
        assert_eq!(rs.records[3].get("Title").unwrap(), &Value::Line("chapters.gnu.org administration guide".to_owned()));
        assert_eq!(rs.records[3].get_vec("Author").unwrap()[0], Value::Line("Nacho Gonzalez".to_owned()));
        assert_eq!(rs.records[3].get_vec("Author").unwrap()[1], Value::Line("Jose E. Marchesi".to_owned()));
        assert_eq!(rs.records[3].get("Location").unwrap(), &Value::Enum("unknown".to_owned()));

        assert_eq!(rs.records[4].len(), 2);
        assert!(rs.records[4].contains_key("Title"));
        assert!(rs.records[4].contains_key("Location"));
        assert_eq!(rs.records[4].get("Title").unwrap(), &Value::Line("Yeelong User Manual".to_owned()));
        assert_eq!(rs.records[4].get("Location").unwrap(), &Value::Enum("home".to_owned()));
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_field_example() {
        const TEXT: &str = "Name: Ada Lovelace
";
        let db = DB::new(TEXT).unwrap();
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 record
        assert_eq!(rs.records.len(), 1);
        // 1 field on that record
        assert_eq!(rs.records[0].len(), 1);
        // contains just field "Name"
        assert_eq!(rs.records[0].contains_key("Name"), true);
        // field "Name" has just 1 value
        assert_eq!(rs.records[0].is_vec("Name"), false);
        // name is a Line type
        let name = rs.records[0].get("Name").unwrap();
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
        const TEXT: &str = "$rec-omatic: Customer
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
        const TEXT: &str = "1test: Customer
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
        const TEXT: &str = "-notgood: Customer
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
        const TEXT: &str = "good-but-not-good: Customer
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
        const TEXT: &str = "underscores_ok: Customer
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
        const TEXT: &str = "FooNotgood!: Customer
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
        const TEXT: &str = "!: Customer
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
    fn parser_2_1_field_name_case_sensitive() {
        const TEXT: &str = "Name: Foo1
name: Foo2
nAmE: Foo3
";
        let db = DB::new(TEXT).unwrap();
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 records
        assert_eq!(rs.records.len(), 1);
        // 3 fields on that record
        assert_eq!(rs.records[0].len(), 3);

        // contains field "Name"
        assert_eq!(rs.records[0].contains_key("Name"), true);
        assert_eq!(rs.records[0].contains_key("name"), true);
        assert_eq!(rs.records[0].contains_key("nAmE"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs.records[0].is_vec("Name"), false);
        assert_eq!(rs.records[0].is_vec("name"), false);
        assert_eq!(rs.records[0].is_vec("nAmE"), false);
        // Name is a Line type
        match &rs.records[0].get("Name").unwrap() {
            Value::Line(thestr) => {
                // Name matches
                assert_eq!(*thestr, "Foo1".to_owned());
            }
            _ => { assert!(false); }
        }
        // name is a Line type
        match &rs.records[0].get("name").unwrap() {
            Value::Line(thestr) => {
                // Name matches
                assert_eq!(*thestr, "Foo2".to_owned());
            }
            _ => { assert!(false); }
        }
        // nAmE is a Line type
        match &rs.records[0].get("nAmE").unwrap() {
            Value::Line(thestr) => {
                // Name matches
                assert_eq!(*thestr, "Foo3".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_valid_field_name_examples() {
        const TEXT: &str = "Foo:
foo:
A23:
ab1:
A_Field:
";
        let db = DB::new(TEXT).unwrap();
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 records
        assert_eq!(rs.records.len(), 1);
        // 3 fields on that record
        assert_eq!(rs.records[0].len(), 5);

        // contains field "Name"
        assert_eq!(rs.records[0].contains_key("Foo"), true);
        assert_eq!(rs.records[0].contains_key("foo"), true);
        assert_eq!(rs.records[0].contains_key("A23"), true);
        assert_eq!(rs.records[0].contains_key("ab1"), true);
        assert_eq!(rs.records[0].contains_key("A_Field"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs.records[0].is_vec("Foo"), false);
        assert_eq!(rs.records[0].is_vec("foo"), false);
        assert_eq!(rs.records[0].is_vec("A23"), false);
        assert_eq!(rs.records[0].is_vec("ab1"), false);
        assert_eq!(rs.records[0].is_vec("A_Field"), false);
        // Foo is a Line type
        let empty = "".to_owned();
        match &rs.records[0].get("Foo").unwrap() {
            Value::Line(thestr) => {
                // value matches
                assert_eq!(*thestr, empty);
            }
            _ => { assert!(false); }
        }
        // foo is a Line type
        match &rs.records[0].get("foo").unwrap() {
            Value::Line(thestr) => {
                // Name matches
                assert_eq!(*thestr, empty);
            }
            _ => { assert!(false); }
        }
        // A23 is a Line type
        match &rs.records[0].get("A23").unwrap() {
            Value::Line(thestr) => {
                // Name matches
                assert_eq!(*thestr, empty);
            }
            _ => { assert!(false); }
        }
        // ab1 is a Line type
        match &rs.records[0].get("ab1").unwrap() {
            Value::Line(thestr) => {
                // Name matches
                assert_eq!(*thestr, empty);
            }
            _ => { assert!(false); }
        }
        // A_Field is a Line type
        match &rs.records[0].get("A_Field").unwrap() {
            Value::Line(thestr) => {
                // Name matches
                assert_eq!(*thestr, empty);
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_value_of_a_field1() {
        const TEXT: &str = "Name: Mr. Customer";    // NOTE: no newline at end of field's value on purpose
        match DB::new(TEXT) {
            Ok(_) => {
                // that is OK, should return Ok
                // see manual 2.1 Fields - Value of a field: "value of a field as a sequence of characters terminated by a single newline character"
                // but GNU recutils accepts it at the end of the file, recfix does not complain, but recsel adds newline at end of file - TODO submit clarification to GNU recutils
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
    fn parser_2_1_value_of_a_field2() {
        const TEXT: &str = "Name: Mr. Customer
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 records
        assert_eq!(rs.records.len(), 1);
        // 1 fields on that record
        assert_eq!(rs.records[0].len(), 1);

        // contains field "Name"
        assert_eq!(rs.records[0].contains_key("Name"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs.records[0].is_vec("Name"), false);
        // Name is a Line type
        match &rs.records[0].get("Name").unwrap() {
            Value::Line(thestr) => {
                // value matches
                assert_eq!(*thestr, "Mr. Customer".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_value_of_a_field3() {
        const TEXT: &str = "Name: Mr. Customer says \"So much wow!\", yet it seems fun, ain't it? Smells like Mötör's Head 😂
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 records
        assert_eq!(rs.records.len(), 1);
        // 1 fields on that record
        assert_eq!(rs.records[0].len(), 1);

        // contains field "Name"
        assert_eq!(rs.records[0].contains_key("Name"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs.records[0].is_vec("Name"), false);
        // Name is a Line type
        match &rs.records[0].get("Name").unwrap() {
            Value::Line(thestr) => {
                // value matches
                assert_eq!(*thestr, "Mr. Customer says \"So much wow!\", yet it seems fun, ain't it? Smells like Mötör's Head 😂".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_escaping_a_newline1() {
        const TEXT: &str = "Foo: bar1
+ bar2
+  bar3
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 records
        assert_eq!(rs.records.len(), 1);
        // 1 fields on that record
        assert_eq!(rs.records[0].len(), 1);

        // contains field "Foo"
        assert_eq!(rs.records[0].contains_key("Foo"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs.records[0].is_vec("Foo"), false);
        // Name is a Line type
        match &rs.records[0].get("Foo").unwrap() {
            Value::Line(thestr) => {
                // value matches
                assert_eq!(*thestr, "bar1\nbar2\n bar3".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_escaping_a_newline2() {
        const TEXT: &str = "Foo:
+ bar2
+  bar3
";  // NOTE: no space after "Foo:"
        match DB::new(TEXT) {
            Ok(_) => {
                // that is OK, should return Ok
                // NOTE: should be accepted as GNU recutils accepts this
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
    fn parser_2_1_escaping_a_newline3() {
        const TEXT: &str = "Foo: 
+ bar2
+  bar3
";  // NOTE: difference to previous test is the space after "Foo:"
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 records
        assert_eq!(rs.records.len(), 1);
        // 1 fields on that record
        assert_eq!(rs.records[0].len(), 1);

        // contains field "Foo"
        assert_eq!(rs.records[0].contains_key("Foo"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs.records[0].is_vec("Foo"), false);
        // Name is a Line type
        match &rs.records[0].get("Foo").unwrap() {
            Value::Line(thestr) => {
                // value matches
                // NOTE: GNU recutils also does not return the " " after "Foo:"
                assert_eq!(*thestr, "bar2\n bar3".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.1 Fields
    #[test]
    fn parser_2_1_escaping_a_newline4() {
        const TEXT: &str = "Foo:  
+ bar2
+  bar3
";  // NOTE: difference to previous test is two spaces after "Foo:" the last of which should be preserved
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 records
        assert_eq!(rs.records.len(), 1);
        // 1 fields on that record
        assert_eq!(rs.records[0].len(), 1);

        // contains field "Foo"
        assert_eq!(rs.records[0].contains_key("Foo"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs.records[0].is_vec("Foo"), false);
        // Name is a Line type
        match &rs.records[0].get("Foo").unwrap() {
            Value::Line(thestr) => {
                // value matches
                // NOTE: GNU recutils preserves the space
                assert_eq!(*thestr, " \nbar2\n bar3".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.2 Records
    #[test]
    fn parser_2_2_record_example_and_multi_fields() {
        const TEXT: &str = "Name1: Value1
Name2: Value2
Name2: Value3
";
        let db = DB::new(TEXT).unwrap();
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 records
        assert_eq!(rs.records.len(), 1);
        // 2 unique fields on that record
        assert_eq!(rs.records[0].len(), 2);

        // contains given fields
        assert_eq!(rs.records[0].contains_key("Name1"), true);
        assert_eq!(rs.records[0].contains_key("Name2"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs.records[0].is_vec("Name1"), false);
        assert_eq!(rs.records[0].is_vec("Name2"), true);
        // Name1 is a Line type
        match &rs.records[0].get("Name1").unwrap() {
            Value::Line(thestr) => {
                // Name matches
                assert_eq!(*thestr, "Value1".to_owned());
            }
            _ => { assert!(false); }
        }
        // Name2 is a Line type
        let name2 = rs.records[0].get_vec("Name2").unwrap();
        match &name2[0] {
            Value::Line(thestr) => {
                // Name matches
                assert_eq!(*thestr, "Value2".to_owned());
            }
            _ => { assert!(false); }
        }
        match &name2[1] {
            Value::Line(thestr) => {
                // Name matches
                assert_eq!(*thestr, "Value3".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.2 Records
    #[test]
    fn parser_2_2_size_of_a_record() {
        const TEXT: &str = "Name: John Smith
Email: john.smith@foomail.com
Email: john@smith.name
";
        let db = DB::new(TEXT).unwrap();
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 records
        assert_eq!(rs.records.len(), 1);
        // 2 unique fields on that record
        assert_eq!(rs.records[0].len(), 2);
        // size of the record is 3
        assert_eq!(rs.records[0].size(), 3);
    }

    /// see manual 2.2 Records
    #[test]
    fn parser_2_2_record_separation() {
        const TEXT: &str = "Name: Ada Lovelace
Age: 36

Name: Peter the Great
Age: 53

Name: Matusalem
Age: 969
";
        let db = DB::new(TEXT).unwrap();
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // number of records
        assert_eq!(rs.records.len(), 3);
        // total number of fields
        assert_eq!(rs.fields.len(), 2);
        // number of unique fields on records
        assert_eq!(rs.records[0].len(), 2);
        assert_eq!(rs.records[1].len(), 2);
        assert_eq!(rs.records[2].len(), 2);
        // size of each record
        assert_eq!(rs.records[0].size(), 2);
        assert_eq!(rs.records[1].size(), 2);
        assert_eq!(rs.records[2].size(), 2);
    }

    /// see manual 2.3 Comments
    #[test]
    fn parser_2_3_comment_lines_example() {
        const TEXT: &str = "Name: Jose E. Marchesi
# Occupation: Software Engineer
# Severe lack of brain capacity
# Fired on 02/01/2009 (without compensation)
Occupation: Unoccupied
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 records
        assert_eq!(rs.records.len(), 1);
        // 1 fields on that record
        assert_eq!(rs.records[0].len(), 2);

        // contains field "Foo"
        assert_eq!(rs.records[0].contains_key("Name"), true);
        assert_eq!(rs.records[0].contains_key("Occupation"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs.records[0].is_vec("Name"), false);
        assert_eq!(rs.records[0].is_vec("Occupation"), false);
        // Name is a Line type
        match &rs.records[0].get("Name").unwrap() {
            Value::Line(thestr) => {
                // value matches
                assert_eq!(*thestr, "Jose E. Marchesi".to_owned());
            }
            _ => { assert!(false); }
        }
        // Occupation is a Line type
        match &rs.records[0].get("Occupation").unwrap() {
            Value::Line(thestr) => {
                // value matches
                assert_eq!(*thestr, "Unoccupied".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.3 Comments
    #[test]
    fn parser_2_3_comments_example_for_headers_and_footers() {
        const TEXT: &str = "# -*- mode: rec -*-
#
# TODO
#
# This file contains the Bugs database of GNU recutils.
#
# Blah blah…

Name: Road Runner

# End of TODO
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 records
        assert_eq!(rs.records.len(), 1);
        // 1 fields on that record
        assert_eq!(rs.records[0].len(), 1);

        // contains field "Foo"
        assert_eq!(rs.records[0].contains_key("Name"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs.records[0].is_vec("Name"), false);
        // Name is a Line type
        match &rs.records[0].get("Name").unwrap() {
            Value::Line(thestr) => {
                // value matches
                assert_eq!(*thestr, "Road Runner".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.3 Comments
    #[test]
    fn parser_2_3_comments_must_be_complete_lines() {
        const TEXT: &str = "Name: Peter the Great # Russian Tsar
Age: 53
";
        // should return Ok - but the value of Name field must include the false comment
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");
        let rs = &db.recordsets[0];

        // untyped recordset
        assert!(rs.rectype.is_none());
        // 1 records
        assert_eq!(rs.records.len(), 1);
        // 1 fields on that record
        assert_eq!(rs.records[0].len(), 2);

        // contains field "Name"
        assert_eq!(rs.records[0].contains_key("Name"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs.records[0].is_vec("Name"), false);
        // Name is a Line type
        match &rs.records[0].get("Name").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Peter the Great # Russian Tsar".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.4.1 Record Sets
    #[test]
    fn parser_2_4_1_recordset_type_example() {
        const TEXT: &str = "%rec: Entry

Id: 1
Name: Entry 1

Id: 2
Name: Entry 2
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");
        let rs = &db.recordsets[0];

        // typed recordset
        assert!(rs.rectype.is_some());
        // type of recordset is Entry
        assert_eq!(rs.rectype.as_deref().unwrap(), "Entry");
        // 2 records
        assert_eq!(rs.records.len(), 2);
        // 2 fields on that record
        assert_eq!(rs.records[0].len(), 2);
        assert_eq!(rs.records[1].len(), 2);

        // contains fields
        assert_eq!(rs.records[0].contains_key("Id"), true);
        assert_eq!(rs.records[0].contains_key("Name"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs.records[0].is_vec("Id"), false);
        assert_eq!(rs.records[0].is_vec("Name"), false);
        // Id is a Line type
        match &rs.records[0].get("Id").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "1".to_owned());
            }
            _ => { assert!(false); }
        }
        // Name is a Line type
        match &rs.records[0].get("Name").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Entry 1".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.4.1 Record Sets
    #[test]
    fn parser_2_4_1_two_record_descriptors_in_the_same_database() {
        const TEXT: &str = "%rec: Article

Id: 1
Title: Article 1

Id: 2
Title: Article 2

%rec: Stock

Id: 1
Type: sell
Date: 20 April 2011

Id: 2
Type: stock
Date: 21 April 2011
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 2);
        // prepare recordsets
        let rs0 = &db.recordsets[0];
        let rs1 = &db.recordsets[1];

        // check recordset[0]

        // typed recordset
        assert!(rs0.rectype.is_some());
        // type of recordset is Entry
        assert_eq!(rs0.rectype.as_deref().unwrap(), "Article");
        // 2 records
        assert_eq!(rs0.records.len(), 2);
        // 2 fields on that record
        assert_eq!(rs0.records[0].len(), 2);
        assert_eq!(rs0.records[1].len(), 2);

        // check record[0]
        // contains fields
        assert_eq!(rs0.records[0].contains_key("Id"), true);
        assert_eq!(rs0.records[0].contains_key("Title"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs0.records[0].is_vec("Id"), false);
        assert_eq!(rs0.records[0].is_vec("Title"), false);
        // Id is a Line type
        match &rs0.records[0].get("Id").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "1".to_owned());
            }
            _ => { assert!(false); }
        }
        // Title is a Line type
        match &rs0.records[0].get("Title").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Article 1".to_owned());
            }
            _ => { assert!(false); }
        }

        // check record[1]
        // contains fields
        assert_eq!(rs0.records[1].contains_key("Id"), true);
        assert_eq!(rs0.records[1].contains_key("Title"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs0.records[1].is_vec("Id"), false);
        assert_eq!(rs0.records[1].is_vec("Title"), false);
        // Id is a Line type
        match &rs0.records[1].get("Id").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "2".to_owned());
            }
            _ => { assert!(false); }
        }
        // Title is a Line type
        match &rs0.records[1].get("Title").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Article 2".to_owned());
            }
            _ => { assert!(false); }
        }

        // check recordset[1]

        // typed recordset
        assert!(rs1.rectype.is_some());
        // type of recordset is Entry
        assert_eq!(rs1.rectype.as_deref().unwrap(), "Stock");
        // 2 records
        assert_eq!(rs1.records.len(), 2);
        // 2 fields on that record
        assert_eq!(rs1.records[0].len(), 3);
        assert_eq!(rs1.records[1].len(), 3);

        // check records[0]
        // contains fields
        assert_eq!(rs1.records[0].contains_key("Id"), true);
        assert_eq!(rs1.records[0].contains_key("Type"), true);
        assert_eq!(rs1.records[0].contains_key("Date"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs1.records[0].is_vec("Id"), false);
        assert_eq!(rs1.records[0].is_vec("Type"), false);
        assert_eq!(rs1.records[0].is_vec("Date"), false);
        // Id is a Line type
        match &rs1.records[0].get("Id").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "1".to_owned());
            }
            _ => { assert!(false); }
        }
        // Type is a Line type
        match &rs1.records[0].get("Type").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "sell".to_owned());
            }
            _ => { assert!(false); }
        }
        // Date is a Line type
        match &rs1.records[0].get("Date").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "20 April 2011".to_owned());
            }
            _ => { assert!(false); }
        }

        // check records[1]
        // contains fields
        assert_eq!(rs1.records[1].contains_key("Id"), true);
        assert_eq!(rs1.records[1].contains_key("Type"), true);
        assert_eq!(rs1.records[1].contains_key("Date"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs1.records[1].is_vec("Id"), false);
        assert_eq!(rs1.records[1].is_vec("Type"), false);
        assert_eq!(rs1.records[1].is_vec("Date"), false);
        // Id is a Line type
        match &rs1.records[1].get("Id").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "2".to_owned());
            }
            _ => { assert!(false); }
        }
        // Type is a Line type
        match &rs1.records[1].get("Type").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "stock".to_owned());
            }
            _ => { assert!(false); }
        }
        // Date is a Line type
        match &rs1.records[1].get("Date").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "21 April 2011".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.4.1 Record Sets
    #[test]
    fn parser_2_4_1_two_record_descriptors_in_the_same_database2() {
        const TEXT: &str = "# comment 1
# comment 2
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 0); //TODO cross-check with GNU recutils - does it expect 1 recordset with 0 records as marker for "yes the recfile is non-empty but it was all comments" or 0 recordsets?
        // number of records
        //assert_eq!(db.recordsets[0].records.len(), 0);
    }

    /// see manual 2.4.1 Record Sets
    #[test]
    fn parser_2_4_1_two_record_descriptors_in_the_same_database3() {
        const TEXT: &str = "";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 0);
        // number of records
        //assert_eq!(db.recordsets[0].records.len(), 0);
    }

    /// see manual 2.4.1 Record Sets
    #[test]
    fn parser_2_4_1_two_record_descriptors_in_the_same_database4() {
        const TEXT: &str = "


";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 0);
        // number of records
        //assert_eq!(db.recordsets[0].records.len(), 0);
    }

    /// see manual 2.4.1 Record Sets
    #[test]
    fn parser_2_4_1_empty_recordset_example() {
        const TEXT: &str = "%rec: Article

%rec: Stock
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 2);
        // prepare recordsets
        let rs0 = &db.recordsets[0];
        let rs1 = &db.recordsets[1];

        // check recordset[0]

        // typed recordset
        assert!(rs0.rectype.is_some());
        // type of recordset is Entry
        assert_eq!(rs0.rectype.as_deref().unwrap(), "Article");
        // 0 records
        assert_eq!(rs0.records.len(), 0);
        // 0 fields
        assert_eq!(rs0.fields.len(), 0);

        // check recordset[1]

        // typed recordset
        assert!(rs1.rectype.is_some());
        // type of recordset is Entry
        assert_eq!(rs1.rectype.as_deref().unwrap(), "Stock");
        // 0 records
        assert_eq!(rs1.records.len(), 0);
        // 0 fields
        assert_eq!(rs1.fields.len(), 0);
    }
}