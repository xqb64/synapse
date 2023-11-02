use assert_cmd;
use std::collections::VecDeque;
use synapse::vm::Object;

macro_rules! object_vec {
    ( $($obj:expr),* ) => {
        {
            let mut v: Vec<Object> = vec![];
            $(
                v.push($obj.into());
            )*
            v
        }
    }
}

fn fetch_stdout(path: &str) -> (VecDeque<String>, VecDeque<String>) {
    let mut cmd = assert_cmd::Command::cargo_bin("synapse").unwrap();
    let assert = cmd.arg(path).assert();
    let output = assert.get_output();
    let stdout = String::from_utf8(output.stdout.clone()).unwrap();
    let stdout_split: VecDeque<String> = stdout
        .split('\n')
        .filter_map(|l| (!l.is_empty()).then_some(l.to_owned()))
        .collect();
    let filtered: VecDeque<String> = stdout_split
        .iter()
        .filter_map(|l| (l.starts_with("dbg:")).then_some(l.to_owned()))
        .collect();
    (stdout_split, filtered)
}

fn fetch_stderr(path: &str) -> VecDeque<String> {
    let mut cmd = assert_cmd::Command::cargo_bin("synapse").unwrap();
    let assert = cmd.arg(path).assert();
    let output = assert.get_output();
    let stderr = String::from_utf8(output.stderr.clone()).unwrap();
    let split: VecDeque<String> = stderr
        .split('\n')
        .filter_map(|l| (!l.is_empty()).then_some(l.to_owned()))
        .collect();
    split
}

macro_rules! run_test {
    ($path:expr, $expected:expr) => {{
        let (mut stdout, mut filtered) = fetch_stdout($path);
        for e in $expected {
            assert!(filtered.pop_front().unwrap() == format!("dbg: {:?}", e));
        }
        assert!(stdout.pop_back().unwrap() == "current instruction: Halt");
        assert!(stdout.pop_back().unwrap() == "stack: []");
    }};
}

macro_rules! run_test_error {
    ($path:expr, $type:tt, $expected:expr) => {{
        let mut stderr = fetch_stderr($path);
        assert!(
            stderr.pop_back().unwrap() == format!("{} error: {}", stringify!($type), $expected)
        );
    }};
}

#[test]
fn add() {
    let (path, expected) = ("tests/cases/add.syn", object_vec![15.0]);
    run_test!(path, expected);
}

#[test]
fn add_error() {
    let (path, expected) = ("tests/cases/add_error.syn", "You can only + numbers.");
    run_test_error!(path, runtime, expected);
}

#[test]
fn sub() {
    let (path, expected) = ("tests/cases/sub.syn", object_vec![2.0]);
    run_test!(path, expected);
}

#[test]
fn mul() {
    let (path, expected) = ("tests/cases/mul.syn", object_vec![360.0]);
    run_test!(path, expected);
}

#[test]
fn div() {
    let (path, expected) = ("tests/cases/div.syn", object_vec![20.0]);
    run_test!(path, expected);
}

#[test]
fn eq() {
    let (path, expected) = (
        "tests/cases/eq.syn",
        object_vec![
            true, false, false, false, false, true, true, true, false, true, true, true, true,
            false, false, false
        ],
    );
    run_test!(path, expected);
}

#[test]
fn relational() {
    let (path, expected) = (
        "tests/cases/relational.syn",
        object_vec![true, false, true, false],
    );
    run_test!(path, expected);
}

#[test]
fn relational_error() {
    let (path, expected) = (
        "tests/cases/relational_error.syn",
        "You can only <, >, <=, >= numbers.",
    );
    run_test_error!(path, runtime, expected);
}

#[test]
fn fib10() {
    let (path, expected) = ("tests/cases/fib10.syn", object_vec![55.0]);
    run_test!(path, expected);
}

#[test]
fn _while() {
    let (path, expected) = (
        "tests/cases/while.syn",
        object_vec![5.0, 4.0, 3.0, 2.0, 1.0, 0.0],
    );
    run_test!(path, expected);
}

#[test]
fn _while_pop() {
    let (path, expected) = (
        "tests/cases/while_pop.syn",
        object_vec![5.0, 4.0, 3.0, 2.0, 1.0, 0.0],
    );
    run_test!(path, expected);
}

#[test]
fn strcat() {
    let (path, expected) = (
        "tests/cases/strcat.syn",
        object_vec!["Hello, world!".to_string()],
    );
    run_test!(path, expected);
}

#[test]
fn neg() {
    let (path, expected) = ("tests/cases/neg.syn", object_vec![-5.0]);
    run_test!(path, expected);
}

#[test]
fn minus_number() {
    let (path, expected) = ("tests/cases/minus_number.syn", object_vec![3.14]);
    run_test!(path, expected);
}

#[test]
fn neg_error() {
    let (path, expected) = ("tests/cases/neg_error.syn", "You can only - numbers.");
    run_test_error!(path, runtime, expected);
}

#[test]
fn not() {
    let (path, expected) = ("tests/cases/not.syn", object_vec![true]);
    run_test!(path, expected);
}

#[test]
fn not_error() {
    let (path, expected) = ("tests/cases/not_error.syn", "You can only ! booleans.");
    run_test_error!(path, runtime, expected);
}

#[test]
fn tokenizer_error() {
    let (path, expected) = ("tests/cases/tokenizer_error.syn", "got unexpected token: $");
    run_test_error!(path, tokenizer, expected);
}

#[test]
fn grouping() {
    let (path, expected) = ("tests/cases/grouping.syn", object_vec![14.0]);
    run_test!(path, expected);
}

#[test]
fn structs() {
    let (path, expected) = ("tests/cases/structs.syn", object_vec!["Hello, world!"]);
    run_test!(path, expected);
}
