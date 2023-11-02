pub mod compiler;
pub mod parser;
pub mod tokenizer;
pub mod util;
pub mod vm;

#[macro_export]
macro_rules! bail_out {
    ($msg:expr) => {{
        eprintln!("{}", $msg);
        std::process::exit(1);
    }};
    ($from:tt, $msg:expr) => {{
        eprintln!("{} error: {}", stringify!($from), $msg);
        std::process::exit(1);
    }};
    ($from:tt, $msg:expr, $($args:expr),*) => {{
        let mut result = format!("{} error: ", stringify!($from));
        let mut parts = $msg.split("{}").peekable();
        let mut args_iter = vec![$($args.to_string()),*].into_iter();

        while let Some(part) = parts.next() {
            result.push_str(part);
            if let Some(arg) = args_iter.next() {
                result.push_str(&arg.to_string());
            }
        }

        while let Some(part) = parts.next() {
            result.push_str(part);
        }

        eprintln!("{}", result);

        std::process::exit(1);
    }};
}
