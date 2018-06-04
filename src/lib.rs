extern crate rustc_data_structures;
extern crate syntax;

use std::ops::Deref;
use std::rc::Rc;
use syntax::ast;
use syntax::codemap;
use syntax::errors::{emitter::Emitter as EmitterT, Diagnostic, DiagnosticBuilder, Handler};
use syntax::parse::parser::Parser;
use syntax::parse::{self, ParseSess};
use syntax::visit::Visitor as VisitorT;

pub mod errors;
pub mod idtree;
pub mod types;
use errors::{Error, Result};

#[derive(Debug, Default)]
pub struct Emitter {
    diag: Vec<Diagnostic>,
}
impl EmitterT for Emitter {
    fn emit(&mut self, db: &DiagnosticBuilder) {
        let diag: &Diagnostic = db.deref();
        self.diag.push(diag.clone())
    }
}
unsafe impl Send for Emitter {}
unsafe impl Sync for Emitter {}

/// Get parser from string s and then apply closure f to it
pub fn parse_str<F, T>(source: String, f: F) -> Result<T>
where
    F: FnOnce(&mut Parser) -> Result<T>,
{
    syntax::with_globals(|| {
        let codemap = Rc::new(codemap::CodeMap::new(codemap::FilePathMapping::empty()));
        let handler = Handler::with_emitter(false, false, Box::new(Emitter::default()));
        let parse_sess = ParseSess::with_span_handler(handler, codemap);
        let mut p = parse::new_parser_from_source_str(
            &parse_sess,
            codemap::FileName::Custom("bogofile".to_owned()),
            source,
        );
        f(&mut p)
    })
}

/// parse string source_str as statement and then apply f to it
/// return false if we can't parse s as statement
// TODO: make F FnOnce(&ast::Stmt) -> Result<Something, Error>
pub fn with_stmt<F>(source_str: String, f: F) -> Result<()>
where
    F: FnOnce(&ast::Stmt) -> Result<()>,
{
    parse_str(source_str, |p| {
        let stmt = match p.parse_stmt() {
            Ok(Some(stmt)) => stmt,
            _ => return Err(Error {}),
        };
        f(&stmt)
    })
}

#[test]
fn parse_test() {
    use std::fs::File;
    use std::io::prelude::*;
    let fname = std::env::var("PARSE_FILE").unwrap();
    let mut f = File::open(fname).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    // with_stmt(s, |stmt| {
    //     Ok(())
    // });
}
