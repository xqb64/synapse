use std::fs::File;
use std::io::Read;

pub fn read_file(file: &str) -> Result<String, std::io::Error> {
    let mut f = File::open(file)?;
    let mut buffer = String::new();
    f.read_to_string(&mut buffer)?;
    Ok(buffer)
}
