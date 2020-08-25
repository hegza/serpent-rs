//! Parser extensions required{Location::new(), location: (), node: ()}Python to
//! Rust.

use rustpython_parser::ast::{Located, Location};

/// Parses Python comments, anything after '#' per line.
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
                // Newline ends a comment
                if let Some((location, mut content)) = comment {
                    // Also add the trailing newline to the comment
                    content.push('\n');

                    comments.push(Located {
                        location,
                        node: content,
                    });

                    comment = None;
                }
                row += 1;
                col = 0; // gets incremented to 1 after match
            }
            c => {
                if let Some((_, comment)) = &mut comment {
                    comment.push(c);
                }
            }
        }
        col += 1;
    }
    // EOF ends a comment
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
                node: " Test comment".to_owned(),
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
