use ego_tree::iter::Edge;
use scraper::{ElementRef, Node};

pub trait StrExt {
    fn is_first_letter_lowercase(self) -> bool;
}

impl<'a> StrExt for &'a str {
    fn is_first_letter_lowercase(self) -> bool {
        self.chars().next().map(|c| c.is_lowercase()).unwrap()
    }
}

pub trait ElementRefExt {
    fn plain_text(&self) -> String;
}

impl ElementRefExt for ElementRef<'_> {
    fn plain_text(&self) -> String {
        self.traverse()
            .into_iter()
            .filter_map(|edge| {
                if let Edge::Open(node) = edge {
                    return match node.value() {
                        Node::Text(text) => Some(text.as_ref()),
                        Node::Element(elem) if elem.name() == "img" => elem.attr("alt"),
                        _ => None,
                    };
                }

                None
            })
            .collect()
    }
}
