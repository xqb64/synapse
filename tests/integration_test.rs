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
    ($path:expr, $expected:expr) => {{
        let mut stderr = fetch_stderr($path);
        assert!(stderr.pop_back().unwrap() == $expected);
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
    run_test_error!(path, expected);
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
