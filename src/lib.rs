use std::fmt::Write;
use std::{borrow::Cow, fmt};

const INDENT: &str = " ";
const CLOSE: &str = "}";

#[macro_export]
macro_rules! fmt {
    ($wrt:ident, $($arg:tt)*) => {
        let _ = write!($wrt, $($arg)*);
    };
}

macro_rules! fmtln {
    ($wrt:ident, $($arg:tt)*) => {
        let _ = writeln!($wrt, $($arg)*);
    };
}

#[derive(Debug, Clone)]
pub struct FileDef {
    pub documentation: Option<String>,
    pub package: Option<String>,
    pub imports: Vec<String>,
    pub classes: Vec<ClassDef>,
}

#[derive(Debug, Clone)]
pub struct ClassDef {
    pub documentation: Option<String>,
    pub annotations: Vec<Call>,
    pub modifiers: &'static str,
    pub typ: &'static str,
    pub name: String,
    pub members: Vec<Member>,
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Member {
    Def(Def),
    Class(ClassDef),
    Method(MethodDef),
}

#[derive(Debug, Clone)]
pub struct MethodDef {
    pub documentation: Option<String>,
    pub modifiers: &'static str,
    pub typ: &'static str,
    pub ident: String,
    pub params: &'static [Param],
    pub code: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub typ: &'static str,
    pub ident: String,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub name: Cow<'static, str>,
    pub args: Vec<Arg>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub typ: &'static str,
    pub ident: &'static str,
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub ident: Option<&'static str>,
    pub arg: Expr,
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Stmt {
    Def(Def),
    Assign {
        lhs: Expr,
        rhs: Expr,
        op: Option<BinOp>,
    },
    Return {
        value: Expr,
    },
    Assert {
        assertion: Expr,
        message: Option<Expr>,
    },
    Expr(Expr),
    Comment(String),
    If {
        cond: Expr,
        then: Box<Stmt>,
        ells: Option<Box<Stmt>>,
    },
    Block(Vec<Stmt>),
    For {
        init: Box<Stmt>,
        cond: Box<Expr>,
        incr: Box<Stmt>,
        body: Box<Stmt>,
    },
    Break,
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Expr {
    NoOp,
    Literal(u32),
    HexLiteral(u64),
    StringLit(String),
    Ident(&'static str),
    Var(String),
    Group(Box<Expr>),
    ArrayInit(Vec<Expr>),
    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Call(Call),
    MethodRef(Call),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[non_exhaustive]
pub enum BinOp {
    Index,
    Add,
    Sub,
    Mul,
    Shl,
    Shr,
    Gt,
    Lt,
    Gte,
    Lte,
    And,
    Or,
}

impl Def {
    pub fn new(typ: &'static str, ident: impl Into<String>) -> Self {
        Def {
            typ,
            ident: ident.into(),
            value: None,
        }
    }

    pub fn assign(typ: &'static str, ident: impl Into<String>, value: Expr) -> Self {
        Def {
            typ,
            ident: ident.into(),
            value: Some(value),
        }
    }

    pub fn optimize(&mut self) {
        if let Some(value) = &mut self.value {
            value.optimize();
        }
    }

    pub fn print(self, wrt: &mut FileWriter) {
        fmtln!(wrt, "{}", Stmt::Def(self));
    }
}

impl Call {
    pub fn new(callee: Expr, name: impl Into<Cow<'static, str>>, args: Vec<Arg>) -> Self {
        Self {
            callee: Box::new(callee),
            name: name.into(),
            args,
        }
    }

    pub fn optimize(&mut self) {
        self.callee.optimize();
        for arg in &mut self.args {
            arg.arg.optimize();
        }
    }
}

impl Arg {
    pub fn new(arg: Expr) -> Self {
        Self { ident: None, arg }
    }

    pub fn optimize(&mut self) {
        self.arg.optimize();
    }
}

impl Expr {
    pub fn bin(lhs: Self, op: BinOp, rhs: Self) -> Self {
        Self::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn optimize(&mut self) {
        match self {
            Expr::Group(expr) => {
                expr.optimize();
                match &mut **expr {
                    Expr::Binary { .. } => {}
                    otherwise => {
                        let expr = otherwise.take();
                        *self = expr
                    }
                }
            }
            Expr::ArrayInit(exprs) => {
                for expr in exprs {
                    expr.optimize();
                }
            }
            Expr::Binary { op, lhs, rhs } => {
                lhs.optimize();
                rhs.optimize();

                match op {
                    BinOp::Add => match (&mut **lhs, &mut **rhs) {
                        (Expr::Literal(lhs), Expr::Literal(rhs)) => {
                            *self = Expr::Literal(lhs.wrapping_add(*rhs));
                            return;
                        }
                        (Expr::Literal(0), rhs) => {
                            let rhs = rhs.take();
                            *self = rhs;
                            return;
                        }
                        (lhs, Expr::Literal(0)) => {
                            let lhs = lhs.take();
                            *self = lhs;
                            return;
                        }
                        _ => {}
                    },
                    BinOp::Sub => match (&mut **lhs, &mut **rhs) {
                        (Expr::Literal(lhs), Expr::Literal(rhs)) => {
                            *self = Expr::Literal(lhs.wrapping_sub(*rhs));
                            return;
                        }
                        (lhs, Expr::Literal(0)) => {
                            let lhs = lhs.take();
                            *self = lhs;
                            return;
                        }
                        _ => {}
                    },
                    BinOp::Mul => match (&mut **lhs, &mut **rhs) {
                        (Expr::Literal(lhs), Expr::Literal(rhs)) => {
                            *self = Expr::Literal(lhs.wrapping_mul(*rhs));
                            return;
                        }
                        (Expr::Literal(0), _) | (_, Expr::Literal(0)) => {
                            *self = Expr::Literal(0);
                            return;
                        }
                        (Expr::Literal(1), rest) | (rest, Expr::Literal(1)) => {
                            let rest = rest.take();
                            *self = rest;
                            return;
                        }
                        _ => {}
                    },
                    BinOp::Shl => match (&mut **lhs, &mut **rhs) {
                        (Expr::Literal(lhs), Expr::Literal(rhs)) => {
                            *self = Expr::Literal(lhs.wrapping_shl(*rhs));
                            return;
                        }
                        (lhs, Expr::Literal(0)) => {
                            let lhs = lhs.take();
                            *self = lhs;
                            return;
                        }
                        (Expr::Literal(0), _) => {
                            *self = Expr::Literal(0);
                            return;
                        }
                        _ => {}
                    },
                    BinOp::Shr => match (&mut **lhs, &mut **rhs) {
                        (Expr::Literal(lhs), Expr::Literal(rhs)) => {
                            *self = Expr::Literal(lhs.wrapping_shr(*rhs));
                            return;
                        }
                        (lhs, Expr::Literal(0)) => {
                            let lhs = lhs.take();
                            *self = lhs;
                            return;
                        }
                        (Expr::Literal(0), _) => {
                            *self = Expr::Literal(0);
                            return;
                        }
                        _ => {}
                    },
                    BinOp::And => match (&mut **lhs, &mut **rhs) {
                        (Expr::Literal(lhs), Expr::Literal(rhs)) => {
                            *self = Expr::Literal(*lhs & *rhs);
                            return;
                        }
                        (_, Expr::Literal(0)) | (Expr::Literal(0), _) => {
                            *self = Expr::Literal(0);
                            return;
                        }
                        (lhs, Expr::HexLiteral(u64::MAX)) => {
                            let lhs = lhs.take();
                            *self = lhs;
                            return;
                        }
                        _ => {}
                    },
                    BinOp::Or => match (&mut **lhs, &mut **rhs) {
                        (Expr::Literal(lhs), Expr::Literal(rhs)) => {
                            *self = Expr::Literal(*lhs | *rhs);
                            return;
                        }
                        (other, Expr::Literal(0)) | (Expr::Literal(0), other) => {
                            let other = other.take();
                            *self = other;
                            return;
                        }
                        _ => {}
                    },
                    BinOp::Index | BinOp::Gt | BinOp::Lt | BinOp::Gte | BinOp::Lte => {}
                }
                if let Expr::Binary { op: lhs_op, .. } = &**lhs {
                    if lhs_op > op {
                        let inner_lhs = lhs.take();
                        *lhs = Box::new(Expr::Group(Box::new(inner_lhs)));
                    }
                }
                if let Expr::Binary { op: rhs_op, .. } = &**rhs {
                    if rhs_op > op && *op != BinOp::Index {
                        let inner_rhs = rhs.take();
                        *rhs = Box::new(Expr::Group(Box::new(inner_rhs)));
                    }
                }
            }
            Expr::Call(call) => {
                call.optimize();
            }
            Expr::MethodRef(call) => {
                call.optimize();
            }
            Expr::NoOp
            | Expr::Literal(_)
            | Expr::HexLiteral(_)
            | Expr::StringLit(_)
            | Expr::Ident(_)
            | Expr::Var(_) => {}
        }
    }

    fn take(&mut self) -> Self {
        std::mem::replace(self, Self::NoOp)
    }
}

impl Stmt {
    pub fn assign(lhs: Expr, rhs: Expr) -> Self {
        Self::assign_op(lhs, rhs, None)
    }

    pub fn add_assign(lhs: Expr, rhs: Expr) -> Self {
        Self::assign_op(lhs, rhs, BinOp::Add)
    }

    pub fn sub_assign(lhs: Expr, rhs: Expr) -> Self {
        Self::assign_op(lhs, rhs, BinOp::Sub)
    }

    pub fn and_assign(lhs: Expr, rhs: Expr) -> Self {
        Self::assign_op(lhs, rhs, BinOp::And)
    }

    pub fn or_assign(lhs: Expr, rhs: Expr) -> Self {
        Self::assign_op(lhs, rhs, BinOp::Or)
    }

    pub fn assign_op(lhs: Expr, rhs: Expr, op: impl Into<Option<BinOp>>) -> Self {
        Self::Assign {
            lhs,
            rhs,
            op: op.into(),
        }
    }

    pub fn optimize(&mut self) {
        match self {
            Stmt::Def(def) => {
                def.optimize();
            }
            Stmt::Assign { lhs, rhs, op: _ } => {
                lhs.optimize();
                rhs.optimize();
            }
            Stmt::Return { value } => {
                value.optimize();
            }
            Stmt::Assert { assertion, message } => {
                assertion.optimize();
                if let Some(message) = message {
                    message.optimize();
                }
            }
            Stmt::Expr(expr) => {
                expr.optimize();
            }
            Stmt::Comment(_) => {}
            Stmt::If { cond, then, ells } => {
                cond.optimize();
                then.optimize();
                if let Some(e) = ells {
                    e.optimize()
                }
            }
            Stmt::Block(stmts) => {
                stmts.iter_mut().for_each(|stmt| stmt.optimize());
            }
            Stmt::For {
                init,
                cond,
                incr,
                body,
            } => {
                init.optimize();
                cond.optimize();
                incr.optimize();
                body.optimize();
            }
            Stmt::Break => {}
        }
    }
}

impl MethodDef {
    pub fn optimize(&mut self) {
        if let Some(code) = &mut self.code {
            for stmt in code {
                stmt.optimize();
            }
        }
    }

    pub fn print(self, wrt: &mut FileWriter) {
        let docs = self.documentation.as_deref().unwrap_or_default();
        print_docs(wrt, "**", docs);

        let mut sig = String::new();
        for item in [self.modifiers, self.typ, &self.ident] {
            if !item.is_empty() {
                sig.push(' ');
                sig.push_str(item);
            }
        }
        fmt!(wrt, "{}(", sig.trim());
        print_list(wrt, self.params);
        fmt!(wrt, ")");

        match self.code {
            Some(code) => {
                fmtln!(wrt, " {{");
                for stmt in code {
                    fmtln!(wrt, "{stmt}");
                }
                fmtln!(wrt, "{CLOSE}");
            }
            None => {
                fmtln!(wrt, ";");
            }
        }
    }
}

impl Member {
    pub fn optimize(&mut self) {
        match self {
            Member::Def(def) => def.optimize(),
            Member::Class(class) => class.optimize(),
            Member::Method(method) => method.optimize(),
        }
    }
}

impl ClassDef {
    pub fn optimize(&mut self) {
        for member in &mut self.members {
            member.optimize();
        }
    }

    pub fn print(self, wrt: &mut FileWriter) {
        let docs = self.documentation.as_deref().unwrap_or_default();
        print_docs(wrt, "**", docs);

        for annotation in self.annotations {
            let name = annotation.name;
            if annotation.args.is_empty() {
                fmtln!(wrt, "@{name}");
            } else {
                fmt!(wrt, "@{name}(");
                print_list(wrt, &annotation.args);
                fmtln!(wrt, ")");
            }
        }
        fmtln!(wrt, "{} {} {} {{", self.modifiers, self.typ, self.name);

        for member in self.members {
            wrt.nl();
            match member {
                Member::Def(def) => def.print(wrt),
                Member::Class(class) => class.print(wrt),
                Member::Method(method) => method.print(wrt),
            }
        }

        fmtln!(wrt, "{CLOSE}");
    }
}

impl FileDef {
    pub fn new(
        documentation: impl Into<String>,
        package: impl Into<String>,
        imports: Vec<String>,
        class: ClassDef,
    ) -> Self {
        Self {
            documentation: Some(documentation.into()),
            package: Some(package.into()),
            imports,
            classes: vec![class],
        }
    }

    pub fn optimize(&mut self) {
        for class in &mut self.classes {
            class.optimize();
        }
    }

    pub fn print(self, wrt: &mut FileWriter) {
        let docs = self.documentation.as_deref().unwrap_or_default();
        print_docs(wrt, "*", docs);
        if let Some(package) = self.package {
            fmtln!(wrt, "package {};", package);
        }

        for import in self.imports {
            wrt.nl();
            fmtln!(wrt, "import {};", import);
        }

        for class in self.classes {
            wrt.nl();
            class.print(wrt);
        }
    }
}

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.typ, self.ident)
    }
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ident {
            Some(ident) => write!(f, "{}={}", ident, self.arg),
            None => write!(f, "{}", self.arg),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Def(Def {
                typ,
                ident,
                value: Some(value),
            }) => write!(f, "{typ} {ident} = {value};"),
            Self::Def(Def {
                typ,
                ident,
                value: _,
            }) => write!(f, "{typ} {ident};"),
            Self::Assign { lhs, rhs, op } => {
                let sigil = op
                    .as_ref()
                    .and_then(|op| op.sigil().between())
                    .unwrap_or_default();
                write!(f, "{lhs} {sigil}= {rhs};")
            }
            Self::Return { value } => write!(f, "return {value};"),
            Self::Assert { assertion, message } => {
                write!(f, "assert {assertion}")?;
                if let Some(message) = message {
                    write!(f, " : {}", message)?;
                }
                write!(f, ";")
            }
            Self::Expr(e) => write!(f, "{e};"),
            Self::Comment(comment) => write!(f, "// {}", comment),
            Stmt::If {
                cond,
                then,
                ells: Some(ells),
            } => write!(f, "if ({cond}) {then} else {ells}"),
            Stmt::If {
                cond,
                then,
                ells: None,
            } => write!(f, "if ({cond}) {then}"),
            Stmt::Block(stmts) => {
                writeln!(f, "{{")?;
                for stmt in stmts {
                    writeln!(f, "{stmt}")?;
                }
                write!(f, "}}")
            }
            Stmt::For {
                init,
                cond,
                incr,
                body,
            } => {
                let incr = if let Stmt::Block(incrs) = &**incr {
                    incrs
                        .iter()
                        .map(|inc| inc.to_string())
                        .map(|inc| inc.trim_end_matches(';').to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                } else {
                    incr.to_string().trim_end_matches(';').to_owned()
                };

                write!(f, "for ({init} {cond}; {incr}) {body}")
            }
            Stmt::Break => write!(f, "break;"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoOp => Ok(()),
            Self::Literal(lit) => write!(f, "{lit}"),
            Self::HexLiteral(lit) => write!(f, "0x{lit:X}L"),
            Self::StringLit(lit) => write!(f, r#""{lit}""#),
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Var(ident) => write!(f, "{ident}"),
            Self::Group(expr) => write!(f, "({expr})"),
            Self::ArrayInit(exprs) => {
                writeln!(f, "{{")?;
                for expr in exprs {
                    writeln!(f, "{expr},")?;
                }
                write!(f, "}}")
            }
            Self::Binary { op, lhs, rhs } => match op.sigil() {
                Sigil::Between(sigil) => write!(f, "{lhs} {sigil} {rhs}"),
                Sigil::Around(l, r) => write!(f, "{lhs}{l}{rhs}{r}"),
            },
            Self::Call(Call { callee, name, args }) => {
                write!(f, "{callee}.{name}(")?;
                if let Some((last, args)) = args.split_last() {
                    for arg in args {
                        write!(f, "{arg}, ")?;
                    }
                    write!(f, "{last}")?;
                }
                write!(f, ")")
            }
            Self::MethodRef(call) => write!(f, "{}::{}", call.callee, call.name),
        }
    }
}

enum Sigil {
    Between(&'static str),
    Around(&'static str, &'static str),
}

impl Sigil {
    fn between(&self) -> Option<&'static str> {
        match self {
            Self::Between(sigil) => Some(*sigil),
            _ => None,
        }
    }
}

impl BinOp {
    const fn sigil(&self) -> Sigil {
        match self {
            Self::Add => Sigil::Between("+"),
            Self::Sub => Sigil::Between("-"),
            Self::Mul => Sigil::Between("*"),
            Self::Shl => Sigil::Between("<<"),
            Self::Shr => Sigil::Between(">>>"),
            Self::And => Sigil::Between("&"),
            Self::Or => Sigil::Between("|"),
            Self::Gt => Sigil::Between(">"),
            Self::Lt => Sigil::Between("<"),
            Self::Gte => Sigil::Between(">="),
            Self::Lte => Sigil::Between("<="),
            Self::Index => Sigil::Around("[", "]"),
        }
    }
}

pub struct FileWriter {
    file: String,
    line: String,
    indent: usize,
}

impl FileWriter {
    pub fn new(indent: usize) -> Self {
        Self {
            file: String::new(),
            line: String::new(),
            indent,
        }
    }

    pub fn nl(&mut self) {
        self.file.extend(self.line.drain(..));
        self.file.push('\n');
    }

    fn indent(&mut self) {
        self.indent += 4;
    }

    fn outdent(&mut self) {
        self.indent -= 4;
    }

    pub fn write(&mut self, s: &str) {
        // lines would remove the trailing newline from writeln!
        let mut lines = s.split('\n');
        if let Some(line) = lines.next() {
            self.write_one_line(line);
        }
        for line in lines {
            self.nl();
            self.write_one_line(line);
        }
    }

    fn write_one_line(&mut self, s: &str) {
        if s.is_empty() {
            return;
        }

        let trim_line = s.trim();
        if trim_line.starts_with('}') {
            self.outdent();
        }

        if self.line.is_empty() {
            self.line.push_str(&INDENT.repeat(self.indent));
        }

        if trim_line.ends_with('{') {
            self.indent();
        }

        self.line.push_str(s);
    }

    fn write_fmt_args(&mut self, args: fmt::Arguments) {
        if let Some(s) = args.as_str() {
            self.write(s);
        } else {
            self.write(&args.to_string());
        }
    }

    pub fn into_inner(mut self) -> String {
        if !self.line.is_empty() {
            self.nl();
        }
        self.file
    }
}

impl Write for FileWriter {
    fn write_str(&mut self, s: &str) -> Result<(), std::fmt::Error> {
        self.write(s);
        Ok(())
    }

    fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> fmt::Result {
        self.write_fmt_args(args);
        Ok(())
    }
}

fn print_docs(wrt: &mut FileWriter, start: &str, docs: &str) {
    let docs = docs.trim();
    if docs.is_empty() {
        return;
    }
    fmtln!(wrt, "/{start}");
    for line in docs.lines() {
        if line.is_empty() {
            fmtln!(wrt, " *");
        } else {
            fmtln!(wrt, " * {line}");
        }
    }
    fmtln!(wrt, " */");
}

fn print_list<T: fmt::Display>(wrt: &mut FileWriter, items: &[T]) {
    if let Some((last, items)) = items.split_last() {
        for item in items {
            fmt!(wrt, "{item}, ");
        }
        fmt!(wrt, "{last}");
    }
}
