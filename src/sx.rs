use super::{Err, Record};
use regex::Regex;
use std::str::FromStr;
use std::{cmp, iter};

// (1 + 1) > 1 ✔
// Age > 1
// ((Email ~ "foomail\.com") || (Age <= 18)) && !#Fixed

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Op {
    Mul,
    Div,
    Mod,
    Not,

    Add,
    Sub,

    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,

    //Before,
    //After,
    //Same,
    Concat,
    Match,

    And,
    Or,
}

macro_rules! num_ops {
    ($name:literal, $func:ident, $arg:ident, $rt:ident, $eq:expr) => {
        fn $func(&self, x: $arg, y: $arg) -> Value {
            use Op::*;
            use Value::*;

            match self {
                Mul => $rt(x * y),
                Div => $rt(x / y),
                Add => $rt(x + y),
                Sub => $rt(x - y),
                Mod => $rt(x % y),
                Eq => Bool($eq(x, y)),
                Ne => Bool(!$eq(x, y)),
                Gt => Bool(x > y),
                Ge => Bool(x >= y),
                Lt => Bool(x < y),
                Le => Bool(x <= y),
                _ => unreachable!("tried to perform {:?} on {} value", self, $name),
            }
        }
    };
}

impl Op {
    fn from(c: char, s: char) -> Self {
        use Op::*;

        match (c, s) {
            ('!', '=') => Ne,
            ('>', '=') => Ge,
            ('<', '=') => Le,
            ('&', '&') => And,
            ('|', '|') => Or,

            ('=', _) => Eq,
            ('>', _) => Gt,
            ('<', _) => Lt,
            ('&', _) => Concat,

            ('*', _) => Mul,
            ('/', _) => Div,
            ('%', _) => Mod,
            ('!', _) => Not,
            ('+', _) => Add,
            ('-', _) => Sub,
            ('~', _) => Match,

            _ => panic!("unknown operator: {}{}", c, s),
        }
    }

    fn eval(&self, rec: &Record, children: &Vec<Node>) -> Result<Value, Err> {
        let x = children
            .get(0)
            .ok_or("expected at least one children")?
            .eval(rec)?;

        Ok(match x {
            Value::Int(x) => {
                let y = children[1].eval(rec)?.try_into()?;
                Self::op_int(self, x, y)
            }
            Value::Real(x) => {
                let y = children[1].eval(rec)?.try_into()?;
                Self::op_real(self, x, y)
            }
            Value::Bool(x) => {
                let y = match children.get(1) {
                    Some(node) => node.eval(rec)?.try_into()?,
                    None => false,
                };
                Self::op_bool(self, x, y)
            }
            Value::Str(x) => {
                let y: String = children[1].eval(rec)?.try_into()?;
                Self::op_str(self, &x, &y)
            }
        })
    }

    num_ops! {"int", op_int, isize, Int,  |x, y| x == y}
    num_ops! {"real", op_real, f64, Real, |x: f64, y: f64| (x - y).abs() < f64::EPSILON }

    fn op_bool(&self, x: bool, y: bool) -> Value {
        use Op::*;
        use Value::*;
        match self {
            Or => Bool(x || y),
            And => Bool(x && y),
            Not => Bool(!x),
            _ => unreachable!("tried to perform {:?} on bool value", self),
        }
    }

    fn op_str(&self, x: &str, y: &str) -> Value {
        use Op::*;
        use Value::*;

        match self {
            Eq => Bool(x == y),
            Ne => Bool(x != y),
            Concat => Str([x, y].concat()),
            Match => {
                // TODO compiled regex cache
                let rx = Regex::new(y).expect("compile regexp");
                Bool(rx.is_match(x))
            }
            _ => unreachable!("tried to perform {:?} on str value", self),
        }
    }
}

impl cmp::PartialOrd for Op {
    // operator precedence
    // taken from Go spec :p
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        use Op::*;

        fn ord(op: &Op) -> u8 {
            match op {
                Concat | Match => 6,
                Mul | Div | Mod | Not => 5,
                Add | Sub => 4,
                Eq | Ne | Gt | Ge | Lt | Le => 3,
                And => 2,
                Or => 1,
            }
        }

        Some(ord(self).cmp(&ord(other)))
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Int(isize),
    Real(f64),
    Str(String),
    Field(String, usize),
    Count(String),
    Op(Op),
    Paren(bool), // true if opening
}

#[derive(Debug, PartialEq, Clone)]
struct Node {
    item: Token,
    children: Vec<Node>,
}

impl Node {
    /// create node without children
    fn new(t: Token) -> Self {
        Node {
            item: t,
            children: Vec::new(),
        }
    }

    fn eval(&self, rec: &Record) -> Result<Value, Err> {
        use self::Value::*;
        use super::Value as RecValue;

        Ok(match &self.item {
            Token::Int(n) => Int(*n),
            Token::Real(n) => Real(*n),
            Token::Str(s) => Str(s.clone()),

            Token::Field(field, _i) => {
                let value = rec
                    .get(field)
                    .ok_or(format!("expected field {} to be present", field))?;

                match value {
                    RecValue::Line(s) => Str(s.clone()),
                    RecValue::Int(n) => Int(*n),
                    _ => todo!(),
                }
            }
            Token::Count(field) => match rec.get(field) {
                Some(_) => Int(1),
                None => Int(0),
            },

            Token::Op(op) => op.eval(&rec, &self.children)?,

            token => todo!("eval: {:?}", token),
        })
    }
}

#[derive(Debug)]
enum Value {
    Int(isize),
    Real(f64),
    Bool(bool),
    Str(String),
}

impl TryFrom<Value> for isize {
    type Error = Err;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(i) => Ok(i),
            Value::Str(s) => Ok(s.parse().map_err(|_| "failed to convert string to int")?),
            _ => Err("expected int".into()),
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = Err;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Real(i) => Ok(i),
            _ => Err("expected f64".into()),
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = Err;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Bool(b) => Ok(b),
            _ => Err("expected bool".into()),
        }
    }
}

impl TryFrom<Value> for String {
    type Error = Err;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Str(s) => Ok(s),
            _ => Err("expected string".into()),
        }
    }
}

#[derive(Debug)]
pub struct Sx(Node);

impl Sx {
    pub fn new(e: &str) -> Result<Self, Err> {
        Self::parse(lex(e)?)
    }

    fn parse(tokens: Vec<Token>) -> Result<Self, Err> {
        let mut nodes: Vec<Node> = tokens.iter().map(|t| Node::new(t.clone())).collect();

        // Fold expressions until there is only one node left
        while let Some(i) = find_first(&nodes) {
            fold_expr(&mut nodes, i)?;
        }
        assert!(nodes.len() == 1);

        let root = nodes.remove(0);

        if !matches!(root.item, Token::Op(_)) {
            return Err("root node is not op".into());
        }

        Ok(Sx(root))
    }

    pub fn eval(&self, rec: &Record) -> Result<bool, Err> {
        match self.0.eval(rec)? {
            Value::Bool(b) => Ok(b),
            Value::Int(n) => Ok(n != 0),
            _ => Ok(false),
        }
    }
}

fn fold_expr(nodes: &mut Vec<Node>, i: usize) -> Result<(), Err> {
    let mut i = i;
    let mut children = Vec::with_capacity(2);

    match nodes[i].item {
        // Not op only has one child
        Token::Op(Op::Not) => {
            children.push(nodes.remove(i + 1));
        }
        _ => {
            children.push(nodes.remove(i - 1));
            children.push(nodes.remove(i));
            i -= 1;
        }
    }

    nodes[i].children = children;

    if i == 0 {
        return Ok(());
    }

    // remove parentheses around single expression
    let before = nodes.get(i - 1).map(|n| n.item.clone());
    let after = nodes.get(i + 1).map(|n| n.item.clone());

    if let Some((Token::Paren(true), Token::Paren(false))) = before.zip(after) {
        nodes.remove(i + 1);
        nodes.remove(i - 1);
    }

    Ok(())
}

/// returns index of first operation to execute
fn find_first(nodes: &[Node]) -> Option<usize> {
    let mut first_op = None;
    let mut idx = 0;

    let mut parens: isize = 0; // we are balancing parentheses here

    struct Paren {
        lvl: usize,
        idx: usize,
    }

    let mut max_paren = Paren { lvl: 0, idx: 0 };

    for (i, node) in nodes.iter().enumerate() {
        if !node.children.is_empty() {
            continue;
        }

        match node.item {
            Token::Op(op) if first_op.is_none() => {
                first_op = Some(op);
                idx = i;
            }
            Token::Op(op) if op > first_op.unwrap() => {
                first_op = Some(op);
                idx = i;
            }
            Token::Paren(opening) => {
                parens += if opening { 1 } else { -1 };

                if parens as usize > max_paren.lvl {
                    max_paren.lvl = parens as usize;
                    max_paren.idx = i;
                }
            }
            _ => continue,
        }
    }

    if parens != 0 {
        panic!("unmatched number of parens: {:?}", parens)
    }

    // parens override evaluation order
    if max_paren.lvl > 0 {
        let closing_pos = find_closing_paren(&nodes[max_paren.idx..]).unwrap();

        let start = max_paren.idx + 1;
        let end = start + closing_pos - 1;

        idx = start + find_first(&nodes[start..end]).unwrap();
    }

    first_op.map(|_| idx)
}

fn find_closing_paren(nodes: &[Node]) -> Option<usize> {
    let mut parens = 0;
    for (i, node) in nodes.iter().enumerate() {
        match node.item {
            Token::Paren(true) => parens += 1,
            Token::Paren(false) if parens - 1 == 0 => return Some(i),
            Token::Paren(false) => parens -= 1,
            _ => continue,
        }
    }
    None
}

fn lex(e: &str) -> Result<Vec<Token>, Err> {
    let mut tokens = Vec::new();
    let mut it = e.chars().peekable();

    while let Some(c) = it.peek() {
        match c {
            '*' | '/' | '%' | '!' | '+' | '-' | '=' | '>' | '<' | '&' | '|' | '~' => {
                let cur = it.next().unwrap();
                let next = it.peek().unwrap_or(&' ');

                let op = Op::from(cur, *next);

                // skip one more char if operator had 2 symbols
                if matches!(next, '=' | '&' | '|') {
                    it.next();
                }

                tokens.push(Token::Op(op));
            }

            '0'..='9' => {
                let num = parse_num(&mut it)?;
                tokens.push(num);
            }
            '#' => {
                it.next();

                let (field, idx) = parse_field(&mut it);
                assert!(idx.is_none());

                tokens.push(Token::Count(field));
            }
            'A'..='Z' | 'a'..='z' => {
                let (field, idx) = parse_field(&mut it);
                tokens.push(Token::Field(field, idx.unwrap_or(0)));
            }
            '(' | ')' => {
                tokens.push(Token::Paren(c == &'('));
                it.next();
            }
            '\'' | '"' => {
                let s = parse_string(&mut it);
                tokens.push(Token::Str(s))
            }
            ' ' => {
                it.next();
            }
            _ => todo!("lex: {}", c),
        };
    }

    Ok(tokens)
}

fn parse_num<I: Iterator<Item = char>>(it: &mut iter::Peekable<I>) -> Result<Token, Err> {
    let mut buf = String::new();

    while let Some(c) = it.peek() {
        if c.is_digit(16) || *c == '-' || *c == '.' || *c == 'x' {
            buf.push(it.next().unwrap());
        } else {
            break;
        }
    }

    let mut radix = 10;

    if (buf.len() > 1 && buf.starts_with("0")) || (buf.len() > 2 && buf.starts_with("-0")) {
        radix = 8;
    }

    if buf.starts_with("0x") || buf.starts_with("-0x") {
        radix = 16;
        buf = buf.replacen("0x", "", 1);
    }

    Ok(match buf.contains('.') {
        true => Token::Real(f64::from_str(&buf)?),
        false => Token::Int(isize::from_str_radix(&buf, radix)?),
    })
}

fn parse_string<I: Iterator<Item = char>>(it: &mut iter::Peekable<I>) -> String {
    let quote = it.next().unwrap();
    debug_assert!(quote == '\'' || quote == '"');

    let mut result = String::new();

    while let Some(c) = it.next() {
        match c {
            '\\' if it.peek().unwrap_or(&' ') == &quote => {
                // ignore escaped quotes
                result.push(it.next().unwrap());
            }
            _ if c == quote => {
                break;
            }
            c => result.push(c),
        }
    }

    result
}

fn parse_field<I: Iterator<Item = char>>(it: &mut iter::Peekable<I>) -> (String, Option<usize>) {
    let mut result = String::new();
    let mut idx = None;

    while let Some(c) = it.peek() {
        match c {
            c if c.is_ascii_alphanumeric() || *c == '_' => {
                result.push(*c);
                it.next();
            }
            '[' => {
                it.next();
                if let Token::Int(n) = parse_num(it).unwrap() {
                    idx = Some(n as usize);
                }
                assert_eq!(it.next().unwrap(), ']');
                break;
            }
            _ => break,
        }
    }

    (result, idx)
}

#[cfg(test)]
mod tests {
    use super::super::Value;
    use super::*;
    use multimap::MultiMap;

    macro_rules! expect {
        ($a:expr) => {
            let sx = Sx::new($a).expect("parse");
            let empty = MultiMap::new();

            if !sx.eval(&empty).expect("eval") {
                panic!("{} evaluated to false. \nast: {:#?}", $a, sx.0)
            }
        };
        ($a:expr, $v:ident) => {
            let sx = Sx::new($a).expect("parse");
            if !sx.eval(&$v).expect("eval") {
                panic!("{} evaluated to false. \nast: {:#?}", $a, sx.0)
            }
        };
    }

    #[test]
    fn sexy() {
        // some basic stuff
        expect!("33 > 22");
        expect!("13 < '37'");
        expect!("2 + 2 * 2 = 6");
        expect!("(2 + 2) * 2 = 8");
        expect!("(1 + 2) * (3 + 4) = 21");
        expect!("(1 + 1 < 1) || (2 + 2 >= 4)");
        expect!("(1 + 1 <= 2) && (2 + 2 > 2)");
        expect!("!(0x1 = 0x2)");
        expect!("0xFF = 255");

        // strings
        expect!("'foo' = 'foo'");
        expect!("'bar' != 'baz'");
        expect!("'foo' & 'bar' = 'foobar'");
        expect!("'bar' ~ 'b.r'");
        expect!("!('bus' ~ 'b.r')");

        // floats
        expect!("0.2 + 0.1 = 0.3");

        // working with records
        let mut record: Record = MultiMap::new();
        record.insert("Email".to_owned(), Value::Line("foo@bar.com".to_owned()));
        record.insert("Age".to_owned(), Value::Int(18));

        expect!("Email = 'foo@bar.com'", record);
        expect!("Age >= 18", record);
        expect!("#Email = 1", record);
    }

    #[test]
    fn parse_number() {
        fn parse(s: &str) -> Token {
            parse_num(&mut s.chars().peekable()).unwrap()
        }

        assert_eq!(Token::Int(10000), parse("10000"));
        assert_eq!(Token::Int(0), parse("0"));
        assert_eq!(Token::Int(255), parse("0xFF"));
        assert_eq!(Token::Int(-10), parse("-0xa"));
        assert_eq!(Token::Int(10), parse("012"));
        assert_eq!(Token::Int(-7), parse("-07"));
        assert_eq!(Token::Int(-1337), parse("-1337"));
        assert_eq!(Token::Real(0.12), parse(".12"));
        assert_eq!(Token::Real(-3.14), parse("-3.14"));
    }

    #[test]
    fn parse_str() {
        let mut it = "'hello \\'mom'".chars().peekable();
        assert_eq!("hello 'mom", parse_string(&mut it));

        let mut it = "\"this also\nworks\"".chars().peekable();
        assert_eq!("this also\nworks", parse_string(&mut it));
    }
}
