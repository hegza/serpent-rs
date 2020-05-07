fn test_print() {
    println!("{}", "Hello World!");
}

fn main() {
    let a = 2;
    let b = 4;
    let c = a + b;

    test_print();
    println!("{} {} {} {} {} {}", "The result of", a, "+", b, "is", c);
}
