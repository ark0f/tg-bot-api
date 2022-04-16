use crate::{parser::make_url_from_fragment, CORE_TELEGRAM_URL};
use html2md::{common::get_tag_attr, Handle, StructuredPrinter, TagHandler, TagHandlerFactory};
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use std::collections::HashMap;

pub(crate) enum TagsHandlerFactory {
    Anchor,
    Image,
}

impl TagsHandlerFactory {
    pub(crate) fn new_in_map() -> HashMap<String, Box<dyn TagHandlerFactory>> {
        let mut map = HashMap::new();
        map.insert("a".to_string(), Box::new(TagsHandlerFactory::Anchor) as _);
        map.insert("img".to_string(), Box::new(TagsHandlerFactory::Image) as _);
        map
    }
}

impl TagHandlerFactory for TagsHandlerFactory {
    fn instantiate(&self) -> Box<dyn TagHandler> {
        match self {
            TagsHandlerFactory::Anchor => Box::new(AnchorHandler::default()),
            TagsHandlerFactory::Image => Box::new(ImageHandler),
        }
    }
}

#[derive(Default)]
struct AnchorHandler {
    inner: Option<(usize, String)>,
}

impl TagHandler for AnchorHandler {
    fn handle(&mut self, tag: &Handle, printer: &mut StructuredPrinter) {
        self.inner = get_tag_attr(tag, "href")
            .map(|value| {
                if value.starts_with('#') {
                    make_url_from_fragment(value)
                } else if value.starts_with('/') {
                    [CORE_TELEGRAM_URL, &value].concat()
                } else {
                    value
                }
            })
            .map(|value| (printer.data.len(), value))
    }

    fn after_handle(&mut self, printer: &mut StructuredPrinter) {
        let (pos, value) = self.inner.as_ref().unwrap();
        if *pos != printer.data.len() {
            printer.insert_str(*pos, "[");
            printer.append_str(&format!("]({})", value));
        }
    }
}

struct ImageHandler;

impl TagHandler for ImageHandler {
    fn handle(&mut self, tag: &Handle, printer: &mut StructuredPrinter) {
        // almost all of code taken from html2md source code
        // because html2md::ImgHandler is not public

        const FRAGMENT: &AsciiSet = &CONTROLS.add(b' ').add(b'"').add(b'<').add(b'>').add(b'`');

        // try to extract attrs
        let src = get_tag_attr(tag, "src");
        let alt = get_tag_attr(tag, "alt");
        let title = get_tag_attr(tag, "title");
        let height = get_tag_attr(tag, "height");
        let width = get_tag_attr(tag, "width");
        let align = get_tag_attr(tag, "align");

        if let Some(alt) = alt {
            printer.append_str(&alt);
            return;
        }

        if height.is_some() || width.is_some() || align.is_some() {
            // need to handle it as inline html to preserve attributes we support
            printer.append_str(&format!(
                "<img{} />",
                alt.map(|value| format!(" alt=\"{}\"", value))
                    .unwrap_or_default()
                    + &src
                        .map(|value| format!(" src=\"{}\"", value))
                        .unwrap_or_default()
                    + &title
                        .map(|value| format!(" title=\"{}\"", value))
                        .unwrap_or_default()
                    + &height
                        .map(|value| format!(" height=\"{}\"", value))
                        .unwrap_or_default()
                    + &width
                        .map(|value| format!(" width=\"{}\"", value))
                        .unwrap_or_default()
                    + &align
                        .map(|value| format!(" align=\"{}\"", value))
                        .unwrap_or_default()
            ));
        } else {
            // need to escape URL if it contains spaces
            // don't have any geometry-controlling attrs, post markdown natively
            let mut img_url = src.unwrap_or_default();
            if img_url.contains(' ') {
                img_url = utf8_percent_encode(&img_url, FRAGMENT).to_string();
            }

            printer.append_str(&format!(
                "![{}]({}{})",
                alt.unwrap_or_default(),
                &img_url,
                title
                    .map(|value| format!(" \"{}\"", value))
                    .unwrap_or_default()
            ));
        }
    }

    fn after_handle(&mut self, _printer: &mut StructuredPrinter) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::BOT_API_DOCS_URL;

    #[test]
    fn empty_link_skipped() {
        let map = TagsHandlerFactory::new_in_map();
        let md = html2md::parse_html_custom(r#"<a href=""></a>"#, &map);
        assert_eq!(md, "");
    }

    #[test]
    fn make_absolute_a_href() {
        let map = TagsHandlerFactory::new_in_map();
        let md = html2md::parse_html_custom(r##"<a href="#fragment">This is a link</a>"##, &map);
        assert_eq!(
            md,
            format!("[This is a link]({}#fragment)", BOT_API_DOCS_URL)
        );
        let md =
            html2md::parse_html_custom(r##"<a href="/bots/webapps">This is a link</a>"##, &map);
        assert_eq!(
            md,
            format!("[This is a link]({}/bots/webapps)", CORE_TELEGRAM_URL)
        )
    }

    #[test]
    fn extract_img_alt() {
        let map = TagsHandlerFactory::new_in_map();
        let md = html2md::parse_html_custom(
            r#"<img alt="🎲" src="//telegram.org/img/emoji/40/F09F8EB2.png" height="20" width="20" />, <img alt="🎯" src="//telegram.org/img/emoji/40/F09F8EAF.png" height="20" width="20" />"#,
            &map,
        );
        assert_eq!(md, "🎲, 🎯");

        const TAGS: &str = r#"<img src="//telegram.org/img/emoji/40/F09F8EB2.png" height="20" width="20" />, <img src="//telegram.org/img/emoji/40/F09F8EAF.png" height="20" width="20" />"#;
        let md = html2md::parse_html_custom(TAGS, &map);
        assert_eq!(md, TAGS);
    }
}
