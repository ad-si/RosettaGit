fn main() {
    let a: f64 = 0.1;
    let b: f64 = 0.2;
    println!("{:.17}", a + b);    // 0.30000000000000004
    println!("{}", a + b == 0.3); // false
}
