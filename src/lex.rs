use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub(crate) enum Token {
    Keyword(String, String), // %blah: blah
    Field(String, String),   // Field: bruh
    Blank,                   // Empty line
}

pub(crate) fn lex(s: &str) -> Result<Vec<Token>, &'static str> {
    let mut tokens: Vec<Token> = Vec::new();

    let lines: Vec<String> = s.lines().map(|s| s.to_string()).collect();
    let mut it = lines.iter().peekable();

    while let Some(&line) = it.peek() {
        if line.trim().is_empty() {
            match tokens.last() {
                Some(Token::Blank) => (),
                _ => tokens.push(Token::Blank),
            };
            it.next();
            continue;
        }

        match line.chars().next().unwrap() {
            '#' => {
                it.next();
            }
            '%' => {
                let (key, value) = read_line(&mut it)?;
                let key = key.trim_start_matches('%').to_string();
                tokens.push(Token::Keyword(key, value));
            }
            _ => {
                let (key, value) = read_line(&mut it)?;
                tokens.push(Token::Field(key, value));
            }
        }
    }

    Ok(tokens)
}

/// returns not Some or None, but simply if the prefi is found, removed and if not found, the original string
fn trim_start_space(line: &str) -> &str {
  if !line.starts_with(" ") {
    return line;
  }
  // trim one space
  return &line[1..];
}

fn read_line<'a, I: Iterator<Item = &'a String>>(
    it: &mut Peekable<I>,
) -> Result<(String, String), &'static str> {
    let line = it.next().unwrap();

    match line.split_once(":") {
        // error case
        None => Err("expected a colon"),

        // multi-line value with empty value on first line
        Some((key, value)) if trim_start_space(value).is_empty() => {
            let mut text = String::new();
            if value.len() > 1 {
                text.push('\n');
            }

            while let Some(&line) = it.peek() {
                if !line.starts_with("+ ") {
                    break;
                }

                text.push_str(line.strip_prefix("+ ").unwrap());
                text.push('\n');
                it.next();
            }

            text.pop(); // Remove final newline

            Ok((key.to_string(), text))
        },

        // multi-line value with value on first line
        //TODO optimize the if condition and merge with above block? also the if len conditions are clunky - space after colon should be removed on split_once-level already
        Some((key, value)) if it.peek().and_then(|line| if line.starts_with("+ ") { Some(()) } else { None } ).is_some() => {
            let mut text = String::from(trim_start_space(value));
            if text.len() > 0 {
                text.push('\n');
            }

            while let Some(&line) = it.peek() {
                if !line.starts_with("+ ") {
                    break;
                }

                text.push_str(line.strip_prefix("+ ").unwrap());
                text.push('\n');
                it.next();
            }

            text.pop(); // Remove final newline

            Ok((key.to_string(), text))
        }

        // normal single-line case
        Some((key, value)) => Ok((key.to_string(), value.trim().to_string())),
    }
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;

    #[test]
    fn tokenizer_simple() {
        assert_eq!(
            lex("%doc: My Books").unwrap(),
            vec![Keyword("doc".to_owned(), "My Books".to_owned())]
        );
    }

    #[test]
    fn tokenizer_multiline() {
        assert_eq!(
            lex("Field:\n+ Multi\n+ Line").unwrap(),
            vec![Field("Field".to_owned(), "Multi\nLine".to_owned())]
        );
    }

    #[test]
    fn tokenizer_blank() {
        assert_eq!(
            lex("Field: 1\n\n\n\n\nField: 2").unwrap(),
            vec![
                Field("Field".to_owned(), "1".to_owned()),
                Blank,
                Field("Field".to_owned(), "2".to_owned())
            ]
        );
    }
}
