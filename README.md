# Starburst - A new language

Starburst is a language that attempts to build off of Rust!

## About

## Development

### Goals

- [ ] Interop with Rust and C (using the `extern` directive)
- [ ] Built-in formatter (like Golang)
- [ ] Self hosting

### Todo

- [ ] Amazing CLI 
- [ ] Lexer
- [ ] Parser
- [ ] Codegen
- [ ] Create our own backend (possibly LLVM but LLVM is slow)

### Working

- Nothing yet!

## Contributing

Anyone can contribute to this project! (And *please* do!) Any sort of help is greatly appreciated!

Just submit a PR and I'll review it as soon as I can! If I think that it deserves integration... boom! You're in and your name will be on the contributes page!

## Examples (Very much subject to change!)

### Hello World

```rust
use std::io::println;

// Woah, what is going on? See Functions for more info about this syntax
func main : {
  println("Hello world!");
}
```

### Math

```rust
use std::io::println;

use extern::c::c_math;             // Get cmath.h
use c_math::sin_f as sin;          // set sin_f to sin
use c_math::cos_f as cos;          // set cos_f to cos
use c_math::tan_f as tan;          // set tan_f to tan
use c_math::square_root_f as sqrt; // set square_root_f to sqrt

func main : {
  // Sin of 1 and cos of 1
  let x = sin(1.0);
  let y = cos(1.0);

  // Tangent of square root of 2 over 2
  let z = tan(sqrt(2)/2);

  println("sin(1.0): {}", .{x});
  println("cos(1.0): {}", .{y});
  println("tan(1.0): {}", .{z});
  
  let res = {
    // Equivalent to ( ( 2 / 3 ) * 6 ) + 1
    1 + 2 / 3 * 6
  };

  println("1 + 2 / 3 * 6: {}", .{res});

  let pow = 2^^3; // 2 to the power of 3

  println("2^^3: {}", .{pow});
  
  // Let's explain this syntax
  // Since `!` usually cannot be a function call, we 'alias' it with `@`
  // Yes, this is *slightly* stolen from Zig, but I call it innovation!
  let factorial = 8.@!(); // 8 factorial

  println("8!: {}", .{factorial});

}
```

### Functions

#### The anatomy of a function...

Functions are defined as so:
`func <NAME_OF_FUNCTION> [([field : type]...)] : [return_type]`

```rust
use std::io::println;

func main : {
    println("Hello world!");
}
```

#### Do you even parameter, bro?

Starburst Functions are not *required* to have a function parameters!
If you don't need it, why even bother!

```rust
// Function foo takes in no parameters and returns a string!
func foo : string {
  "I have no return type!"
}

// Function bar takes in a whole number (u8, u16, i32, etc) and returns nothing!
func bar(x: impl WholeNumber) : {
  println!("Take off in...");
  for i : x..0 {
    println("{}...", .{i});
  }
  println("BLOOSSSHHH");
}

// Function that takes in another function that takes in a floating number (f8, f16, uf64)
// and returns nothing and executes it
func bazz(f: |x: impl FloatNumber|) : impl FloatNumber {
  f(10)
}
```

#### Wait... I can put in multiple types??

```rust
func add<T>(x: T, y: T) : T
  // T can either have the WholeNumber or FloatNumber traits and does not need both
  where T : impl WholeNumber ? FloatNumber
{
  x + y
}

func add_and_print<T>(x: T, y: T) :
  // Traits uses PEMDAS to be evaluated, so to explain to the compiler that
  // T can either be WholeNumber OR FloatNumber AND Display, you **must** use parentheses
  where T : impl (WholeNumber ? FloatNumber) & Display
{
  let res = add(x, y);
  println!("{} + {} = {}", .{x, y, res});
}
```

### Structs

```rust
use std::io::println;

const Person <- struct {
  name         : string,
  age          : u8,
  bank_ammount : f32,
}

Person <- impl {
  func new(name: string = "Unknown", age: u8 = 0, bank_ammount: f32 = 0) -> Self {
    Self {
      name, age, bank_ammount,
    }
  }
}

Person <- impl Display {
  func fmt(&self, f: &mut Formatter) : Result {
    f.write("Person:\n")?;
    f.write("  Name: {}\n", .{name})?;
    f.write("  Age: {}\n", .{age})?;
    f.write("  Bank ammount: {}\n", .{bank_ammount})?;
    return Ok;
  }
}

func main : {
  let person = Person::new(name: "Billy Bob", 7, .12);
  
  println("{}", .{person});
}
```

