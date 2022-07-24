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
pub enum Stmt {
    Def(Def),
    Assign {
        lhs: Expr,
        rhs: Expr,
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
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Shl,
    Shr,
    And,
    Or,
    OrAssign,
    Index,
    Lte,
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
                    BinOp::Add => {
                        if let Expr::Literal(0) = &**lhs {
                            let rhs = rhs.take();
                            *self = rhs;
                        } else if let Expr::Literal(0) = &**rhs {
                            let lhs = lhs.take();
                            *self = lhs;
                        }
                    }
                    BinOp::Sub | BinOp::Shl | BinOp::Shr => {
                        if let Expr::Literal(0) = &**rhs {
                            let lhs = lhs.take();
                            *self = lhs
                        }
                    }
                    BinOp::And => {
                        if let Expr::HexLiteral(u64::MAX) = &**rhs {
                            let lhs = lhs.take();
                            *self = lhs;
                        } else if let Expr::Binary { .. } = &**lhs {
                            let inner_lhs = lhs.take();
                            **lhs = Expr::Group(Box::new(inner_lhs));
                        }
                    }
                    BinOp::Or | BinOp::OrAssign | BinOp::Index | BinOp::Lte => {}
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
    pub fn optimize(&mut self) {
        match self {
            Stmt::Def(def) => {
                def.optimize();
            }
            Stmt::Assign { lhs, rhs } => {
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
                wrt.indent();
                for stmt in code {
                    fmtln!(wrt, "{stmt}");
                }
                wrt.outdent();
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
        wrt.indent();

        for member in self.members {
            wrt.nl();
            match member {
                Member::Def(def) => def.print(wrt),
                Member::Class(class) => class.print(wrt),
                Member::Method(method) => method.print(wrt),
            }
        }

        wrt.outdent();
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
            Self::Assign { lhs, rhs } => write!(f, "{lhs} = {rhs};"),
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
                    writeln!(f, "    {expr},")?;
                }
                write!(f, "}}")
            }
            Self::Binary { op, lhs, rhs } => match op {
                BinOp::Add => write!(f, "{lhs} + {rhs}"),
                BinOp::Sub => write!(f, "{lhs} - {rhs}"),
                BinOp::Shl => write!(f, "{lhs} << {rhs}"),
                BinOp::Shr => write!(f, "{lhs} >>> {rhs}"),
                BinOp::And => write!(f, "{lhs} & {rhs}"),
                BinOp::Or => write!(f, "{lhs} | {rhs}"),
                BinOp::OrAssign => write!(f, "{lhs} |= {rhs}"),
                BinOp::Index => write!(f, "{lhs}[{rhs}]"),
                BinOp::Lte => write!(f, "{lhs} <= {rhs}"),
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

    pub fn indent(&mut self) {
        self.indent += 4;
    }

    pub fn outdent(&mut self) {
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
        if self.line.is_empty() {
            self.line.push_str(&INDENT.repeat(self.indent));
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
