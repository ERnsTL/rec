use super::{Err, Kind, Record, Value, DB};
use crate::{crypt, RecordSet};
use crate::lex::Token;
use std::collections::HashSet;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref FIELD_RX: Regex = Regex::new("^[a-zA-Z%][a-zA-Z0-9_]*$").unwrap();   //TODO does it really allow a percent sign? //TODO performance or accuracy with ^ and $ compared to without?
    static ref RECTYPE_RX: Regex = Regex::new("^([a-zA-Z][a-zA-Z0-9_]*)((?:,)([a-zA-Z][a-zA-Z0-9_]*))?$").unwrap();   //TODO performance or accuracy with ^ and $ compared to without?
    static ref ENUM_RX: Regex = Regex::new("^[a-zA-Z0-9][a-zA-Z0-9_]*$").unwrap();
}

#[derive(Debug, Default)]
pub(crate) struct Parser {
    db: DB,
    current_record: Option<Record>,
    current_recordset: usize,   // index of recordset vector that we are currently filling into
    have_something: bool,
    inside_record_descriptor: bool,
    record_descriptor_has_rec: bool,
}

impl Parser {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn parse(mut self, tokens: Vec<Token>) -> Result<DB, Err> {
        self.have_something = false;
        self.inside_record_descriptor = false;
        self.record_descriptor_has_rec = false;

        for token in tokens.iter() {
            match token {   //TODO recognize beginning of untyped recordset
                Token::Keyword(keyword, value) => {
                    let args = value.split_whitespace().collect();
                    match self.parse_keyword(keyword, args) {
                        Ok(_) => {},
                        Err(err) => return Err(format!("parsing keyword '{}': {}", keyword, err).into()),
                    }
                    self.have_something = true;
                },
                Token::Field(key, value) => {
                    if self.inside_record_descriptor {
                        // inside record descriptor, non-special fields have no effect (see manual 2.4.4 Record Sets Properties)
                        continue;
                    }
                    self.parse_field(key, value)?;
                    self.have_something = true;
                }
                Token::Blank => {
                    // end of record descriptor
                    if self.inside_record_descriptor {
                        // now we are outside of record descriptor
                        self.inside_record_descriptor = false;
                        // check if we got the mandatory rec special field
                        if !self.record_descriptor_has_rec {
                            return Err("missing rec special field".into());
                        }
                    }
                    // push current record into current recordset
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

        if !self.inside_record_descriptor {
            self.inside_record_descriptor = true;
            if self.have_something {
                // add additional recordset
                self.db.recordsets.push(RecordSet::default());
                self.current_recordset += 1;
            }
        }
        match key { //TODO optimize - if ordering of match arms makes a difference, then order them by frequency of use
            "rec" => {
                self.record_descriptor_has_rec = true;
                let rectype = args.get(0).ok_or("expected rec type")?;  //TODO optimize possible allocation - compare performance with assign, then check the .rectype value since most DBs are expected to be compliant
                match RECTYPE_RX.is_match(rectype) {
                    true => self.db.recordsets[self.current_recordset].rectype = Some(rectype.to_string()),
                    false => return Err(format!("invalid rec type name value: {}", rectype).into()),
                }
            },
            "mandatory" | "allowed" | "prohibit" => {
                for field in args {
                    let meta = self.db.recordsets[self.current_recordset].types.entry(field.to_owned()).or_default();
                    match key.parse() {
                        Ok(constraint) => meta.constraint = Some(constraint),
                        Err(err) => return Err(format!("parsing mandatory|allowed|prohibit constraint '{}': {}", key, err).into()),
                    }
                }
            }
            "unique" => {
                for field in args {
                    let meta = self.db.recordsets[self.current_recordset].types.entry(field.to_owned()).or_default();
                    meta.unique = true
                }
            }
            "key" => self.db.recordsets[self.current_recordset].primary_key = Some(args.get(0).ok_or("expected key")?.to_string()),
            "doc" => self.db.recordsets[self.current_recordset].doc = Some(args.join(" ")),
            "typedef" => {
                // typedef is parsed the same way as type, but the typedef is like a declaration and does not have to be used
                //TODO does typedef really have the same fields as type? TODO optimize code duplication
                let type_name = args.get(0).ok_or("expected type name")?.to_string();
                match self.parse_type(args) {
                    Ok(kind) => {
                        let meta = self.db.recordsets[self.current_recordset].typedefs.entry(type_name).or_default();
                        meta.kind = kind;
                    },
                    Err(err) => return Err(format!("parsing typedef: {}", err).into()),
                }
            }
            "type" => {
                let field_name = args.get(0).ok_or("expected field name")?.to_string();
                match self.parse_type(args) {
                    Ok(kind) => {
                        let meta = self.db.recordsets[self.current_recordset].types.entry(field_name).or_default();
                        meta.kind = kind;
                    },
                    Err(err) => return Err(format!("parsing type: {}", err).into()),
                }
            }
            "auto" => {
                self.db.recordsets[self.current_recordset].auto_fields = Some(args.iter().map(|s| s.to_string()).collect());
            }
            "sort" => {
                let field = args.get(0).ok_or("expected sort field")?.to_string();
                self.db.recordsets[self.current_recordset].sort_field = Some(field)
            }
            "size" => {
                if self.db.recordsets[self.current_recordset].size.is_some() {
                    return Err("only one %size field shall appear in a record descriptor".into());
                }
                match args.len() {
                    2 => {
                        let operator = args.get(0).ok_or("expected operator string")?.to_string();
                        let size = args.get(1).ok_or("expected size integer")?.parse().expect("failed to parse size as integer");
                        self.db.recordsets[self.current_recordset].size = Some((operator, size));
                    },
                    1 => {
                        let size = args.get(0).ok_or("expected size integer")?.parse().expect("failed to parse size as integer");
                        self.db.recordsets[self.current_recordset].size = Some(('='.into(), size));
                    },
                    _ => return Err("%size field must have 1 or 2 arguments".into()),
                }
            }
            "constraint" => {
                //TODO implement
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
            // unknown special field
            key => {
                return Err(format!("unknown special field {}", key).into()); //TODO optimize
            },
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

    fn parse_type(&self, args: Vec<&str>) -> Result<Kind, Err> {
        use Kind::*;

        let &type_name = args.get(0).ok_or("expected type name")?;
        if !RECTYPE_RX.is_match(&type_name) {
            return Err(format!("invalid type name: {}", type_name).into());
        }

        //TODO implement enumerated types like %type: Start,End date
        let &tt = args.get(1).ok_or("expected type")?;
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
            type_name => {
                // check for type alias
                if self.db.recordsets[self.current_recordset].typedefs.contains_key(type_name) {
                    // type alias
                    return Ok(Kind::Alias(type_name.to_string()));
                } else {
                    // unknown
                    return Err(format!("unknown type: {}", tt).into())
                }
            }
        })
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
        Kind::Alias(_) => unreachable!("type alias values are not parsed"),
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
        let p = Parser::new();

        let kind = p.parse_type(vec!["field", "range", "MIN", "MAX"]).unwrap();
        assert!(matches!(kind, Kind::Range(isize::MIN, isize::MAX)));

        let kind = p.parse_type(vec!["field", "range", "0", "15"]).unwrap();
        assert!(matches!(kind, Kind::Range(0, 15)));

        let kind = p.parse_type(vec!["field", "range", "15"]).unwrap();
        assert!(matches!(kind, Kind::Range(0, 15)));

        // Impossible range
        assert!(p.parse_type(vec!["field", "range", "30", "15"]).is_err());
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
        const TEXT: &str = "%rec: Logins
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
        const TEXT: &str = "Foo:\n+ bar2\n+  bar3\n";  // NOTE: no space after "Foo:"
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
    fn parser_2_1_escaping_a_newline3() {
        const TEXT: &str = "Foo: \n+ bar2\n+  bar3\n";  // NOTE: difference to previous test is the space after "Foo:"
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
        const TEXT: &str = "Foo:  \n+ bar2\n+  bar3\n";  // NOTE: difference to previous test is two spaces after "Foo:" the last of which should be preserved
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

    /// see manual 2.4.1 Record Sets
    #[test]
    fn parser_2_4_1_default_record_type_mixed_with_typed_record_types() {
        const TEXT: &str = "Id: 1
Title: Blah

Id: 2
Title: Bleh

%rec: Movement

Date: 13-Aug-2012
Concept: 20

Date: 24-Sept-2012
Concept: 12
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
        assert!(rs0.rectype.is_none());
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
                assert_eq!(*thestr, "Blah".to_owned());
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
                assert_eq!(*thestr, "Bleh".to_owned());
            }
            _ => { assert!(false); }
        }

        // check recordset[1]

        // typed recordset
        assert!(rs1.rectype.is_some());
        // type of recordset is Entry
        assert_eq!(rs1.rectype.as_deref().unwrap(), "Movement");
        // 2 records
        assert_eq!(rs1.records.len(), 2);
        // 2 fields on that record
        assert_eq!(rs1.records[0].len(), 2);
        assert_eq!(rs1.records[1].len(), 2);

        // check records[0]
        // contains fields
        assert_eq!(rs1.records[0].contains_key("Date"), true);
        assert_eq!(rs1.records[0].contains_key("Concept"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs1.records[0].is_vec("Date"), false);
        assert_eq!(rs1.records[0].is_vec("Concept"), false);
        // Id is a Line type
        match &rs1.records[0].get("Date").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "13-Aug-2012".to_owned());
            }
            _ => { assert!(false); }
        }
        // Type is a Line type
        match &rs1.records[0].get("Concept").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "20".to_owned());
            }
            _ => { assert!(false); }
        }

        // check records[1]
        // contains fields
        assert_eq!(rs1.records[1].contains_key("Date"), true);
        assert_eq!(rs1.records[1].contains_key("Concept"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs1.records[1].is_vec("Date"), false);
        assert_eq!(rs1.records[1].is_vec("Concept"), false);
        // Id is a Line type
        match &rs1.records[1].get("Date").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "24-Sept-2012".to_owned());
            }
            _ => { assert!(false); }
        }
        // Type is a Line type
        match &rs1.records[1].get("Concept").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "12".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.4.2 Naming Record Types
    #[test]
    fn parser_2_4_2_allowed_characters_in_record_types1() {
        const TEXT: &str = "%rec: le666_Baronesse123
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 1);
        // prepare recordsets
        let rs0 = &db.recordsets[0];

        // check recordset[0]

        // typed recordset
        assert!(rs0.rectype.is_some());
        // type of recordset
        assert_eq!(rs0.rectype.as_deref().unwrap(), "le666_Baronesse123");
        // number of records
        assert_eq!(rs0.records.len(), 0);
        // number of fields
        assert_eq!(rs0.fields.len(), 0);
    }

    /// see manual 2.4.2 Naming Record Types
    #[test]
    fn parser_2_4_2_allowed_characters_in_record_types2() {
        const TEXT: &str = "%rec: _1badname
";
        // should return Err
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

    /// see manual 2.4.2 Naming Record Types
    #[test]
    fn parser_2_4_2_allowed_characters_in_record_types3() {
        const TEXT: &str = "%rec: 2badname
";
        // should return Err
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

    /// see manual 2.4.2 Naming Record Types
    #[test]
    fn parser_2_4_2_allowed_characters_in_record_types4() {
        const TEXT: &str = "%rec: abad-name
";
        // should return Err
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

    /// see manual 2.4.3 Documenting Records
    #[test]
    fn parser_2_4_3_recordset_documentation_field() {
        const TEXT: &str = "%rec: Contact
%doc: A more verbose description! Yes: This is the way.
";
        // should return Ok
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

    /// see manual 2.4.3 Documenting Records
    #[test]
    fn parser_2_4_3_recordset_documentation_character_restrictions() {
        const TEXT: &str = "%rec: Contact
%doc: More verbose :+?%\"' it cannot get!
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 1);
        // prepare recordsets
        let rs0 = &db.recordsets[0];

        // check recordset[0]

        // typed recordset
        assert!(rs0.rectype.is_some());
        // type of recordset
        assert_eq!(rs0.rectype.as_deref().unwrap(), "Contact");
        // number of records
        assert_eq!(rs0.records.len(), 0);
        // number of fields
        assert_eq!(rs0.fields.len(), 0);
        // documentation field
        assert_eq!(rs0.doc.is_some(), true);
        // value of documentation
        assert_eq!(rs0.doc.as_deref().unwrap(), "More verbose :+?%\"' it cannot get!");
    }

    /// see manual 2.4.3 Documenting Records
    #[test]
    fn parser_2_4_3_two_record_sets_with_rec_and_doc_fields_example() {
        const TEXT: &str = "%rec: Contact
%doc: Family, friends and acquaintances (other than business).

Name: Granny
Phone: +12 23456677

Name: Edwina
Phone: +55 0923 8765


%rec: Associate
%doc: Colleagues and other business contacts

Name: Karl Schmidt
Phone: +49 88234566

Name: Genevieve Curie
Phone: +33 34 87 65
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
        // type of recordset
        assert_eq!(rs0.rectype.as_deref().unwrap(), "Contact");
        // has documentation field
        assert!(rs0.doc.is_some());
        // value of documentation field
        assert_eq!(rs0.doc.as_deref().unwrap(), "Family, friends and acquaintances (other than business).");
        // 2 records
        assert_eq!(rs0.records.len(), 2);
        // 2 fields on records
        assert_eq!(rs0.records[0].len(), 2);
        assert_eq!(rs0.records[1].len(), 2);

        // check record[0]
        // contains fields
        assert_eq!(rs0.records[0].contains_key("Name"), true);
        assert_eq!(rs0.records[0].contains_key("Phone"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs0.records[0].is_vec("Name"), false);
        assert_eq!(rs0.records[0].is_vec("Phone"), false);
        // Name is a Line type
        match &rs0.records[0].get("Name").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Granny".to_owned());
            }
            _ => { assert!(false); }
        }
        // Phone is a Line type
        match &rs0.records[0].get("Phone").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "+12 23456677".to_owned());
            }
            _ => { assert!(false); }
        }

        // check record[1]
        // contains fields
        assert_eq!(rs0.records[1].contains_key("Name"), true);
        assert_eq!(rs0.records[1].contains_key("Phone"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs0.records[1].is_vec("Name"), false);
        assert_eq!(rs0.records[1].is_vec("Phone"), false);
        // Name is a Line type
        match &rs0.records[1].get("Name").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Edwina".to_owned());
            }
            _ => { assert!(false); }
        }
        // Phone is a Line type
        match &rs0.records[1].get("Phone").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "+55 0923 8765".to_owned());
            }
            _ => { assert!(false); }
        }

        // check recordset[1]

        // typed recordset
        assert!(rs1.rectype.is_some());
        // type of recordset is Entry
        assert_eq!(rs1.rectype.as_deref().unwrap(), "Associate");
        // has documentation field
        assert!(rs1.doc.is_some());
        // value of documentation field
        assert_eq!(rs1.doc.as_deref().unwrap(), "Colleagues and other business contacts");
        // 2 records
        assert_eq!(rs1.records.len(), 2);
        // 2 fields on that record
        assert_eq!(rs1.records[0].len(), 2);
        assert_eq!(rs1.records[1].len(), 2);

        // check records[0]
        // contains fields
        assert_eq!(rs1.records[0].contains_key("Name"), true);
        assert_eq!(rs1.records[0].contains_key("Phone"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs1.records[0].is_vec("Name"), false);
        assert_eq!(rs1.records[0].is_vec("Phone"), false);
        // Name is a Line type
        match &rs1.records[0].get("Name").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Karl Schmidt".to_owned());
            }
            _ => { assert!(false); }
        }
        // Phone is a Line type
        match &rs1.records[0].get("Phone").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "+49 88234566".to_owned());
            }
            _ => { assert!(false); }
        }

        // check records[1]
        // contains fields
        assert_eq!(rs1.records[1].contains_key("Name"), true);
        assert_eq!(rs1.records[1].contains_key("Phone"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs1.records[1].is_vec("Name"), false);
        assert_eq!(rs1.records[1].is_vec("Phone"), false);
        // Name is a Line type
        match &rs1.records[1].get("Name").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Genevieve Curie".to_owned());
            }
            _ => { assert!(false); }
        }
        // Phone is a Line type
        match &rs1.records[1].get("Phone").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "+33 34 87 65".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_example_for_mandatory() {
        const TEXT: &str = "%rec: Item
%type: Id int
%mandatory: Title

Id: 10
Title: Notebook (big)

Id: 11
Title: Fountain Pen
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 1);
        // prepare recordsets
        let rs0 = &db.recordsets[0];

        // check recordset[0]

        // typed recordset
        assert!(rs0.rectype.is_some());
        // type of recordset
        assert_eq!(rs0.rectype.as_deref().unwrap(), "Item");
        // recordset types
        assert_eq!(rs0.types.len(), 2);
        // type of fields and constraints
        // type of field Id
        if let Some(thetype) = rs0.types.get("Id") {
            // check for type of field
            //NOTE: cannot implement PartialEq for Kind, so we have to do it manually
            match &thetype.kind {
                Kind::Int => {
                    // this is OK
                    assert!(true);
                }
                _ => {
                    // this is not OK
                    assert!(false);
                }
            }
            // check for uniqueness constraint
            assert_eq!(&thetype.unique, &false);
            // check for a mandatory constraint
            assert!(&thetype.constraint.is_none());
        } else {
            assert!(false);
        }
        // type of field Title
        if let Some(thetype) = rs0.types.get("Title") {
            // check for type of field
            //NOTE: cannot implement PartialEq for Kind, so we have to do it manually
            match &thetype.kind {
                Kind::Line => {
                    // this is OK
                    assert!(true);
                }
                _ => {
                    // this is not OK
                    assert!(false);
                }
            }
            // check for uniqueness constraint
            assert_eq!(&thetype.unique, &false);
            // check for a constraint
            if let Some(constraint) = &thetype.constraint {
                match constraint {
                    Constraint::Mandatory => {
                        // this is OK
                        assert!(true);
                    }
                    _ => {
                        // this is not OK
                        assert!(false);
                    }
                }
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
        // has documentation field
        assert!(rs0.doc.is_none());
        // value of documentation field
        //assert_eq!(rs0.doc.as_deref().unwrap(), "");
        // 2 records
        assert_eq!(rs0.records.len(), 2);
        // 2 fields on records
        assert_eq!(rs0.records[0].len(), 2);
        assert_eq!(rs0.records[1].len(), 2);

        // check record[0]
        // contains fields
        assert_eq!(rs0.records[0].contains_key("Id"), true);
        assert_eq!(rs0.records[0].contains_key("Title"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs0.records[0].is_vec("Id"), false);
        assert_eq!(rs0.records[0].is_vec("Title"), false);
        // Id is an Int type
        match &rs0.records[0].get("Id").unwrap() {
            Value::Int(theint) => {
                // value matches - this is not a comment
                assert_eq!(*theint, 10);
            }
            _ => { assert!(false); }
        }
        // Title is a Line type
        match &rs0.records[0].get("Title").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Notebook (big)".to_owned());
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
        // Id is an Int type
        match &rs0.records[1].get("Id").unwrap() {
            Value::Int(theint) => {
                // value matches - this is not a comment
                assert_eq!(*theint, 11);
            }
            _ => { assert!(false); }
        }
        // Title is a Line type
        match &rs0.records[1].get("Title").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Fountain Pen".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_start_with_percent() {
        const TEXT: &str = "%rec: Item
%type: Id int
%mandatory: Title

Id: 10
Title: Notebook (big)

Id: 11
Title: Fountain Pen
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 1);
        // prepare recordsets
        let rs0 = &db.recordsets[0];

        // check recordset[0]

        // typed recordset
        assert!(rs0.rectype.is_some());
        // type of recordset
        assert_eq!(rs0.rectype.as_deref().unwrap(), "Item");
        // recordset types
        assert_eq!(rs0.types.len(), 2);
        // type of fields and constraints
        // type of field Id
        if let Some(thetype) = rs0.types.get("Id") {
            // check for type of field
            //NOTE: cannot implement PartialEq for Kind, so we have to do it manually
            match &thetype.kind {
                Kind::Int => {
                    // this is OK
                    assert!(true);
                }
                _ => {
                    // this is not OK
                    assert!(false);
                }
            }
            // check for uniqueness constraint
            assert_eq!(&thetype.unique, &false);
            // check for a mandatory constraint
            assert!(&thetype.constraint.is_none());
        } else {
            assert!(false);
        }
        // type of field Title
        if let Some(thetype) = rs0.types.get("Title") {
            // check for type of field
            //NOTE: cannot implement PartialEq for Kind, so we have to do it manually
            match &thetype.kind {
                Kind::Line => {
                    // this is OK
                    assert!(true);
                }
                _ => {
                    // this is not OK
                    assert!(false);
                }
            }
            // check for uniqueness constraint
            assert_eq!(&thetype.unique, &false);
            // check for a constraint
            if let Some(constraint) = &thetype.constraint {
                match constraint {
                    Constraint::Mandatory => {
                        // this is OK
                        assert!(true);
                    }
                    _ => {
                        // this is not OK
                        assert!(false);
                    }
                }
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
        // has documentation field
        assert!(rs0.doc.is_none());
        // value of documentation field
        //assert_eq!(rs0.doc.as_deref().unwrap(), "");
        // 2 records
        assert_eq!(rs0.records.len(), 2);
        // 2 fields on records
        assert_eq!(rs0.records[0].len(), 2);
        assert_eq!(rs0.records[1].len(), 2);

        // check record[0]
        // contains fields
        assert_eq!(rs0.records[0].contains_key("Id"), true);
        assert_eq!(rs0.records[0].contains_key("Title"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs0.records[0].is_vec("Id"), false);
        assert_eq!(rs0.records[0].is_vec("Title"), false);
        // Id is an Int type
        match &rs0.records[0].get("Id").unwrap() {
            Value::Int(theint) => {
                // value matches - this is not a comment
                assert_eq!(*theint, 10);
            }
            _ => { assert!(false); }
        }
        // Title is a Line type
        match &rs0.records[0].get("Title").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Notebook (big)".to_owned());
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
        // Id is an Int type
        match &rs0.records[1].get("Id").unwrap() {
            Value::Int(theint) => {
                // value matches - this is not a comment
                assert_eq!(*theint, 11);
            }
            _ => { assert!(false); }
        }
        // Title is a Line type
        match &rs0.records[1].get("Title").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Fountain Pen".to_owned());
            }
            _ => { assert!(false); }
        }
    }

        /// see manual 2.4.4 Record Sets Properties
        #[test]
        fn parser_2_4_4_nonspecial_fields_in_a_record_descriptor_have_no_effect() {
            const TEXT: &str = "%rec: Item
%type: Id int
%mandatory: Title
Id: 123
Title: has no effect

Id: 10
Title: Notebook (big)

Id: 11
Title: Fountain Pen
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 1);
        // prepare recordsets
        let rs0 = &db.recordsets[0];

        // check recordset[0]

        // typed recordset
        assert!(rs0.rectype.is_some());
        // type of recordset
        assert_eq!(rs0.rectype.as_deref().unwrap(), "Item");
        // recordset types
        assert_eq!(rs0.types.len(), 2);
        // type of fields and constraints
        // type of field Id
        if let Some(thetype) = rs0.types.get("Id") {
            // check for type of field
            //NOTE: cannot implement PartialEq for Kind, so we have to do it manually
            match &thetype.kind {
                Kind::Int => {
                    // this is OK
                    assert!(true);
                }
                _ => {
                    // this is not OK
                    assert!(false);
                }
            }
            // check for uniqueness constraint
            assert_eq!(&thetype.unique, &false);
            // check for a mandatory constraint
            assert!(&thetype.constraint.is_none());
        } else {
            assert!(false);
        }
        // type of field Title
        if let Some(thetype) = rs0.types.get("Title") {
            // check for type of field
            //NOTE: cannot implement PartialEq for Kind, so we have to do it manually
            match &thetype.kind {
                Kind::Line => {
                    // this is OK
                    assert!(true);
                }
                _ => {
                    // this is not OK
                    assert!(false);
                }
            }
            // check for uniqueness constraint
            assert_eq!(&thetype.unique, &false);
            // check for a constraint
            if let Some(constraint) = &thetype.constraint {
                match constraint {
                    Constraint::Mandatory => {
                        // this is OK
                        assert!(true);
                    }
                    _ => {
                        // this is not OK
                        assert!(false);
                    }
                }
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
        // has documentation field
        assert!(rs0.doc.is_none());
        // value of documentation field
        //assert_eq!(rs0.doc.as_deref().unwrap(), "");
        // 2 records
        assert_eq!(rs0.records.len(), 2);
        // 2 fields on records
        assert_eq!(rs0.records[0].len(), 2);
        assert_eq!(rs0.records[1].len(), 2);

        // check record[0]
        // contains fields
        assert_eq!(rs0.records[0].contains_key("Id"), true);
        assert_eq!(rs0.records[0].contains_key("Title"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs0.records[0].is_vec("Id"), false);
        assert_eq!(rs0.records[0].is_vec("Title"), false);
        // Id is an Int type
        match &rs0.records[0].get("Id").unwrap() {
            Value::Int(theint) => {
                // value matches - this is not a comment
                assert_eq!(*theint, 10);
            }
            _ => { assert!(false); }
        }
        // Title is a Line type
        match &rs0.records[0].get("Title").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Notebook (big)".to_owned());
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
        // Id is an Int type
        match &rs0.records[1].get("Id").unwrap() {
            Value::Int(theint) => {
                // value matches - this is not a comment
                assert_eq!(*theint, 11);
            }
            _ => { assert!(false); }
        }
        // Title is a Line type
        match &rs0.records[1].get("Title").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Fountain Pen".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_every_recordset_must_contain_exactly_one_special_field_rec1() {
        const TEXT: &str = "%type: Id int
%mandatory: Title

Id: 10
Title: Notebook (big)
";
        // should return Err
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

    /// see manual 2.4.4 Record Sets Properties
    fn parser_2_4_4_every_recordset_must_contain_exactly_one_special_field_rec2() {
        const TEXT: &str = "%rec: Item
%type: Id int
%rec: Item
%mandatory: Title

Id: 10
Title: Notebook (big)
";
        // should return Err
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_it_is_not_mandated_that_the_rec_special_field_must_occupy_the_first_position() {
        const TEXT: &str = "%type: Id int
%rec: Item
%mandatory: Title

Id: 10
Title: Notebook (big)

Id: 11
Title: Fountain Pen
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 1);
        // prepare recordsets
        let rs0 = &db.recordsets[0];

        // check recordset[0]

        // typed recordset
        assert!(rs0.rectype.is_some());
        // type of recordset
        assert_eq!(rs0.rectype.as_deref().unwrap(), "Item");
        // recordset types
        assert_eq!(rs0.types.len(), 2);
        // type of fields and constraints
        // type of field Id
        if let Some(thetype) = rs0.types.get("Id") {
            // check for type of field
            //NOTE: cannot implement PartialEq for Kind, so we have to do it manually
            match &thetype.kind {
                Kind::Int => {
                    // this is OK
                    assert!(true);
                }
                _ => {
                    // this is not OK
                    assert!(false);
                }
            }
            // check for uniqueness constraint
            assert_eq!(&thetype.unique, &false);
            // check for a mandatory constraint
            assert!(&thetype.constraint.is_none());
        } else {
            assert!(false);
        }
        // type of field Title
        if let Some(thetype) = rs0.types.get("Title") {
            // check for type of field
            //NOTE: cannot implement PartialEq for Kind, so we have to do it manually
            match &thetype.kind {
                Kind::Line => {
                    // this is OK
                    assert!(true);
                }
                _ => {
                    // this is not OK
                    assert!(false);
                }
            }
            // check for uniqueness constraint
            assert_eq!(&thetype.unique, &false);
            // check for a constraint
            if let Some(constraint) = &thetype.constraint {
                match constraint {
                    Constraint::Mandatory => {
                        // this is OK
                        assert!(true);
                    }
                    _ => {
                        // this is not OK
                        assert!(false);
                    }
                }
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
        // has documentation field
        assert!(rs0.doc.is_none());
        // value of documentation field
        //assert_eq!(rs0.doc.as_deref().unwrap(), "");
        // 2 records
        assert_eq!(rs0.records.len(), 2);
        // 2 fields on records
        assert_eq!(rs0.records[0].len(), 2);
        assert_eq!(rs0.records[1].len(), 2);

        // check record[0]
        // contains fields
        assert_eq!(rs0.records[0].contains_key("Id"), true);
        assert_eq!(rs0.records[0].contains_key("Title"), true);
        // fields are no multi-fields, but recognized as separate fields
        assert_eq!(rs0.records[0].is_vec("Id"), false);
        assert_eq!(rs0.records[0].is_vec("Title"), false);
        // Id is an Int type
        match &rs0.records[0].get("Id").unwrap() {
            Value::Int(theint) => {
                // value matches - this is not a comment
                assert_eq!(*theint, 10);
            }
            _ => { assert!(false); }
        }
        // Title is a Line type
        match &rs0.records[0].get("Title").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Notebook (big)".to_owned());
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
        // Id is an Int type
        match &rs0.records[1].get("Id").unwrap() {
            Value::Int(theint) => {
                // value matches - this is not a comment
                assert_eq!(*theint, 11);
            }
            _ => { assert!(false); }
        }
        // Title is a Line type
        match &rs0.records[1].get("Title").unwrap() {
            Value::Line(thestr) => {
                // value matches - this is not a comment
                assert_eq!(*thestr, "Fountain Pen".to_owned());
            }
            _ => { assert!(false); }
        }
    }

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_rec() {
        const TEXT: &str = "%rec: Item
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_mandatory() {
        const TEXT: &str = "%rec: Item
%mandatory: Title
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_allowed() {
        const TEXT: &str = "%rec: Item
%allowed: Title
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_prohibit() {
        const TEXT: &str = "%rec: Item
%prohibit: Title
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_unique() {
        const TEXT: &str = "%rec: Item
%unique: Id
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_key() {
        const TEXT: &str = "%rec: Item
%key: Id
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_doc() {
        const TEXT: &str = "%rec: Item
%doc: Some nice doc string
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_typedef() {
        const TEXT: &str = "%rec: Item
%typedef: Id_t int
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_type() {
        const TEXT: &str = "%rec: Item
%type: Id int
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_auto() {
        const TEXT: &str = "%rec: Item
%key: Id
%auto: Id
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_sort() {
        const TEXT: &str = "%rec: Item
%type: Date date
%sort: Date
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_size() {
        const TEXT: &str = "%rec: Item
%size: <= 100
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_constraint1() {
        const TEXT: &str = "%rec: Item
%type: Start date
%type: End date
%constraint: Start << End
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_constraint2() {
        const TEXT: &str = "%rec: Item
%type: Start,End date
%constraint: Start << End
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_special_fields_defined_in_the_recutils_format_confidential() {
        const TEXT: &str = "%rec: Account
%confidential: Password
";
        // should return Ok
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

    /// see manual 2.4.4 Record Sets Properties
    #[test]
    fn parser_2_4_4_unknown_special_fields() {
        const TEXT: &str = "%rec: Item
%type: Id int
%mandatory: Title
%special: SomethingInvalid
%another: SpecialInvalid
";
        // should return Err
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

    /// see manual 6.1 Declaring Types
    #[test]
    fn parser_6_1_the_typedef_syntax() {
        const TEXT: &str = "%rec: Item
%typedef: Test_t int
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 1);
        // prepare recordsets
        let rs0 = &db.recordsets[0];

        // check recordset[0]

        // typed recordset
        assert!(rs0.rectype.is_some());
        // type of recordset
        assert_eq!(rs0.rectype.as_deref().unwrap(), "Item");
        // recordset types
        assert_eq!(rs0.types.len(), 0);
        // recordset typedefs
        assert_eq!(rs0.typedefs.len(), 1);
        // type of fields and constraints
        // typedef Test_t
        if let Some(thetypedef) = rs0.typedefs.get("Test_t") {
            // check for type of field
            //NOTE: cannot implement PartialEq for Kind, so we have to do it manually
            match &thetypedef.kind {
                Kind::Int => {
                    // this is OK
                    assert!(true);
                }
                _ => {
                    // this is not OK
                    assert!(false);
                }
            }
            // check for uniqueness constraint
            assert_eq!(&thetypedef.unique, &false);
            // check for a mandatory constraint
            assert!(&thetypedef.constraint.is_none());
        } else {
            assert!(false);
        }
        // check documentation field
        assert!(rs0.doc.is_none());
        // value of documentation field
        //assert_eq!(rs0.doc.as_deref().unwrap(), "");
        // 0 records
        assert_eq!(rs0.records.len(), 0);
    }

    /// see manual 6.1 Declaring Types
    #[test]
    fn parser_6_1_define_age_t_as_numbers_in_the_range_0_to_120() {
        const TEXT: &str = "%rec: Item
%typedef: Age_t range 0 120
";
        // should return Ok
        let db = DB::new(TEXT).expect("DB::new() returned Err - should return Ok");

        // number of recordsets
        assert_eq!(db.recordsets.len(), 1);
        // prepare recordsets
        let rs0 = &db.recordsets[0];

        // check recordset[0]

        // typed recordset
        assert!(rs0.rectype.is_some());
        // type of recordset
        assert_eq!(rs0.rectype.as_deref().unwrap(), "Item");
        // recordset types
        assert_eq!(rs0.types.len(), 0);
        // recordset typedefs
        assert_eq!(rs0.typedefs.len(), 1);
        // type of fields and constraints
        // typedef Test_t
        if let Some(thetypedef) = rs0.typedefs.get("Age_t") {
            // check for type of field
            //NOTE: cannot implement PartialEq for Kind, so we have to do it manually
            match &thetypedef.kind {
                Kind::Range(min, max) => {
                    // this is OK
                    assert_eq!(*min, 0);
                    assert_eq!(*max, 120)
                }
                _ => {
                    // this is not OK
                    assert!(false);
                }
            }
            // check for uniqueness constraint
            assert_eq!(&thetypedef.unique, &false);
            // check for a mandatory constraint
            assert!(&thetypedef.constraint.is_none());
        } else {
            assert!(false);
        }
        // check documentation field
        assert!(rs0.doc.is_none());
        // value of documentation field
        //assert_eq!(rs0.doc.as_deref().unwrap(), "");
        // 0 records
        assert_eq!(rs0.records.len(), 0);
    }

    /// see manual 6.1 Declaring Types
    #[test]
    fn parser_6_1_type_names_are_identifiers_having_the_following_syntax1() {
        const TEXT: &str = "%rec: Item
%typedef: age_t int
";
        // should return Ok
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

    /// see manual 6.1 Declaring Types
    #[test]
    fn parser_6_1_type_names_are_identifiers_having_the_following_syntax2() {
        const TEXT: &str = "%rec: Item
%typedef: age-t int
";
        // should return Err
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

    /// see manual 6.1 Declaring Types
    #[test]
    fn parser_6_1_type_names_are_identifiers_having_the_following_syntax3() {
        const TEXT: &str = "%rec: Item
%typedef: 9age_t int
";
        // should return Err
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

    /// see manual 6.1 Declaring Types
    #[test]
    fn parser_6_1_type_names_are_identifiers_having_the_following_syntax4() {
        const TEXT: &str = "%rec: Item
%typedef: age_t9 int
";
        // should return Ok
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

    /// see manual 6.1 Declaring Types
    #[test]
    fn parser_6_1_type_names_are_identifiers_having_the_following_syntax5() {
        const TEXT: &str = "%rec: Item
%typedef: _Age_t int
";
        // should return Err
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

    /// see manual 6.1 Declaring Types
    #[test]
    fn parser_6_1_type_names_are_identifiers_having_the_following_syntax6() {
        const TEXT: &str = "%rec: Item
%typedef: Ag👎e_t int
";
        // should return Err
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

    /// see manual 6.1 Declaring Types
    #[test]
    fn parser_6_1_type_names_are_identifiers_having_the_following_syntax7() {
        const TEXT: &str = "%rec: Item
%typedef: Ageäöü_t int
";
        // should return Err
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

    /// see manual 6.1 Declaring Types
    #[test]
    fn parser_6_1_a_type_can_be_declared_to_be_an_alias_for_another_type() {
        const TEXT: &str = "%rec: Item
%typedef: Id_t          int
%typedef: Item_t        Id_t
%typedef: Transaction_t Id_t
";
        // should return Ok
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
}