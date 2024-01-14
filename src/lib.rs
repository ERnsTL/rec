#![feature(test)]
#![allow(dead_code)]

mod crypt;
pub use crypt::{decrypt_field, encrypt_field, ENCRYPTED_PREFIX};

mod lex;
mod sx;
use sx::Sx;

mod parser;
use parser::Parser;

use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fmt::{self, Display, Formatter},
    fs,
    path::Path,
    str::FromStr,
};
use multimap::MultiMap;

use chrono::{DateTime, Utc};
use regex::Regex;
use uuid::Uuid;

pub type Key = String;
pub type Record = MultiMap<Key, Value>;

trait FlatSize {
    fn size(self: &Self) -> usize;
}

impl FlatSize for Record {
    /// Returns the "size of a record" defined in manual, 2.2 Records
    fn size(self: &Record) -> usize {
        return self.flat_iter().count();
    }
}

#[derive(Debug, Default)]
pub struct Meta {
    unique: bool,
    constraint: Option<Constraint>,
    kind: Kind,
}

#[derive(Debug)]
enum Constraint {
    Mandatory,
    Allowed,
    Prohibited,
}

impl FromStr for Constraint {
    type Err = Err;

    // so we don't pollute parser code
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "mandatory" => Ok(Constraint::Mandatory),
            "allowed" => Ok(Constraint::Allowed),
            "prohibit" => Ok(Constraint::Prohibited),   //NOTE: inconsistency in file format spec, should rightly be "prohibited"
            _ => Err(format!("unknown constraint: {}", s).into()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Kind {
    Line, // TODO size
    Int,
    Real,
    Bool,
    Date,
    Email,
    UUID,
    Confidential,
    Range(isize, isize),
    Regexp(Regex),  //NOTE: does not implement PartialEq e.g. for assert_eq!, see https://github.com/rust-lang/regex/issues/178
    Viz(String),
    Enum(HashSet<String>),
}

impl Default for Kind {
    fn default() -> Self {
        Kind::Line
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Value {
    Line(String),
    Int(isize),
    Real(f64),
    Bool(bool),
    Date(DateTime<Utc>),
    Email(String),
    UUID(Uuid),
    Confidential(String),
    Range(isize),
    Regexp(String),
    Viz(String),
    Enum(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            Line(s) => write!(f, "{}", s),
            Int(s) => write!(f, "{}", s),
            Real(s) => write!(f, "{}", s),
            Bool(s) => write!(f, "{}", s),
            Date(s) => write!(f, "{}", s),
            Email(s) => write!(f, "{}", s),
            UUID(s) => write!(f, "{}", s),
            Confidential(s) => write!(f, "{}", s),
            Range(s) => write!(f, "{}", s),
            Regexp(s) => write!(f, "{}", s),
            Viz(s) => write!(f, "{}", s),
            Enum(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Default, Debug)]
pub struct DB {
    recordsets: Vec<RecordSet>,
}

#[derive(Default, Debug)]
pub struct RecordSet {
    pub rectype: Option<String>,
    pub primary_key: Option<String>,
    pub sort_field: Option<String>,
    pub doc: Option<String>,
    pub types: HashMap<Key, Meta>,
    pub typedefs: HashMap<Key, Meta>,   //TODO able to merge with types?
    pub fields: Vec<Key>,
    records: Vec<Record>,
}

type Err = Box<dyn Error>;

impl DB {
    pub fn new(s: &str) -> Result<Self, Err> {
        let tokens = lex::lex(s)?;
        let db = Parser::new().parse(tokens)?;

        Ok(db)
    }

    fn new_from_file(path: &Path) -> Result<Self, Err> {
        let file = fs::read_to_string(path).map_err(|_| "failed to open file")?;
        DB::new(&file)
    }
}

pub struct QueryBuilder<'a> {
    rs: &'a RecordSet,
    sx: Option<Sx>,
    contains: Option<String>,
    sort: Option<String>,
    unique: bool,
}

impl<'a> QueryBuilder<'a> {
    pub fn new(rs: &'a RecordSet) -> Self {
        Self {
            rs,
            sx: None,
            contains: None,
            sort: None,
            unique: false,
        }
    }

    pub fn where_sx(&mut self, s: &str) -> Result<&mut Self, Err> {
        self.sx = Some(Sx::new(s)?);
        Ok(self)
    }

    pub fn sort_by(&mut self, field: &str) -> &mut Self {
        self.sort = Some(field.to_owned());
        self
    }

    pub fn contains(&mut self, query: &str) -> &mut Self {
        self.contains = Some(query.to_owned());
        self
    }

    pub fn find(&self) -> Result<impl Iterator<Item = &'a Record>, Err> {
        let mut results = Vec::new();

        for rec in &self.rs.records {
            if let Some(ref sx) = self.sx {
                if !sx.eval(&rec)? {
                    continue;
                }
            }

            if let Some(ref query) = self.contains {
                if !rec.flat_iter().any(|f| f.1.to_string().contains(query)) {
                    continue;
                }
            }

            results.push(rec)
        }

        if let Some(ref field) = self.sort.clone().or(self.rs.sort_field.clone()) {
            results.sort_by(|x, y| x[field].partial_cmp(&y[field]).unwrap());
        }

        Ok(results.into_iter())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use std::hint::black_box;
    use test::Bencher;

    #[test]
    fn it_works() -> Result<(), Err> {
        let buf = include_str!("test.rec");
        let db = DB::new(buf).unwrap();

        let result = QueryBuilder::new(&db.recordsets[0])
            .where_sx("Login = 'foo'")?
            .contains("Hello")
            .sort_by("Name")
            .find()?;

        assert_eq!(result.count(), 1);
        Ok(())
    }

    #[test]
    fn parser_records() {
        let db = DB::new("hello: world\nblah: blah\n\nhello: mom\nblah: bruh").unwrap();
        assert_eq!(db.recordsets[0].records.len(), 2);
    }

    #[bench]
    fn bench_parse(b: &mut Bencher) {
        let buf = include_str!("test.rec");
        b.iter(|| DB::new(black_box(buf)).unwrap());
    }

    #[bench]
    fn bench_find(b: &mut Bencher) {
        let buf = include_str!("test.rec");
        let db = DB::new(black_box(buf)).unwrap();
        b.iter(|| {
            QueryBuilder::new(&db.recordsets[0])
                .where_sx("Login = 'foo'")
                .unwrap()
                .sort_by("Name")
                .find()
                .unwrap()
        });
    }
}
