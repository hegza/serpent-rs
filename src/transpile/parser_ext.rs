//! Parser extensions required{Location::new(), location: (), node: ()}Python to
//! Rust.

use rustpython_parser::ast::{Located, Location};

/// Parses Python single-line comments, anything after '#' per line.
pub fn parse_comments(source: &str) -> Vec<Located<String>> {
    let mut comments = vec![];

    let mut row = 1;
    let mut col = 1;
    let mut comment: Option<(Location, String)> = None;
    for c in source.chars() {
        match c {
            // "#" starts a new comment
            '#' => {
                comment = Some((Location::new(row, col), String::new()));
            }
            '\n' => {
                // Newline ends a comment, take it from the option
                if let Some((location, mut content)) = comment.take() {
                    // Also add the trailing newline as part of the comment
                    // EOF comments don't have this
                    content.push('\n');

                    // Then store the comment
                    comments.push(Located {
                        location,
                        node: content,
                    });
                }
                row += 1;
                col = 0; // gets incremented to 1 after the match statement
            }
            c => {
                // Push the character into the comment, if we're currently in one
                if let Some((_, comment)) = &mut comment {
                    comment.push(c);
                }
            }
        }
        col += 1;
    }
    // EOF ends a comment, without a newline
    if let Some((location, content)) = comment {
        comments.push(Located {
            location,
            node: content,
        });
    }

    comments
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn comments_parse_right() {
        let src = "\
# Test comment

    # Indented comment\
";

        let correct = vec![
            Located {
                location: Location::new(1, 1),
                node: " Test comment\n".to_owned(),
            },
            Located {
                location: Location::new(3, 5),
                node: " Indented comment".to_owned(),
            },
        ];

        let comments = parse_comments(src);
        assert_eq!(&comments, &correct);
    }
}
