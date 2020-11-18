use std::io;
use some_error::*;

#[derive(Debug, Clone, Copy)]
struct NotZeroError(u32);

#[some_error]
fn my_func() -> Result<(), io::Error + NotZeroError>{
    let x = 3;
    if x != 0 {
        Err(NotZeroError(x))?;
    }

    Ok(())
}

fn main() {
    match my_func() {
        Ok(_) => {
            println!("Worked ok!");
        }
        Err(my_func::NotZeroError(NotZeroError(x))) => {
            println!("{} is not zero!!", x);
        }
        Err(my_func::io::Error(io_err)) => {
            println!("io error: {:?}", io_err);
        }
    }
}
