<h1 align="center">synapse</h1>

This project is my second venture into the field of programming language design. Similarly to [my initial attempt](https://github.com/xqb64/venom), the main idea was not to engineer a production-quality system, but to have fun trying to streamline performance and broaden my horizons. Given the educational nature of the project, I'm aware it has its fair share of limitations, but despite this, I'd argue that the features listed off below make it lean towards being somewhat useful:

- basic data types
  - numbers (double-precision floating point)
  - booleans
  - strings
  - structures
  - vectors
  - pointers
  - null
- operators for the said types
  - `==`, `!=`, `<`, `>`, `<=`, `>=`
  - `+`, `-`, `*`, `/`, `%`
  - `+=`, `-=`, `*=`, `/=`, `%=` (compound assignment)
  - `&`, `|`, `^`, `~`, `<<`, `>>` (bitwise and/or/xor/not/shift (left|right))
  - `&=`, `|=`, `^=`, `<<=`, `>>=` (bitwise compound assignment)
  - `&&`, `||`, `!` (logical and/or/not)
  - `++` (string concatenation)
  - `&`, `*`, `->` (for pointers)
  - `.` (member access)
- control flow
  - `if`, `else`
  - `while`
  - `for`
  - `break` and `continue`
- functions
  - `main` is the entry-point
  - `return` is mandatory
  - recursion!
- methods (taking in `self` as the first parameter)
- `print` statement

Global scope is **NOT** allowed.

The entire system consists of:

  - a tokenizer
  - a recursive-descent parser
  - a bytecode compiler
  - a virtual machine

## Let's talk numbers

```rust
fn fib(n) {
    if (n < 2) return n;
    return fib(n-1)+fib(n-2);
}

fn main() {
    print fib(40);
    return 0;
}
```

NOTE: The above program has been the go-to benchmark throughout the development cycle. The running time on my system (AMD Ryzen 3 3200G with Radeon Vega Graphics) varied wildly throughout the development cycle, from somewhere around 13s to, currently, 21s:

```
$ hyperfine --runs 5 'target/release/synapse benches/cases/fib40.syn'
Benchmark 1: target/release/synapse benches/cases/fib40.syn
  Time (mean ± σ):     19.352 s ±  0.069 s    [User: 19.327 s, System: 0.007 s]
  Range (min … max):   19.252 s … 19.434 s    5 runs
```

A mandatory comparison to Python is in order:

```
$ cat fib.py
def fib(n):
    if n < 2: return n
    return fib(n-1)+fib(n-2)

print(fib(40))
$ python3 --version
Python 3.10.12
$ hyperfine --runs 5 'python3 fib.py'
Benchmark 1: python3 fib.py
  Time (mean ± σ):     27.132 s ±  0.152 s    [User: 27.057 s, System: 0.038 s]
  Range (min … max):   26.964 s … 27.335 s    5 runs
```

Other than this, it's pointless to compare the two, because obviously, besides being a lot more useful, Python comes with `@functools.lru_cache()` and could execute this code in a blink of an eye with it.

## Examples

```rust
struct node {
    next,
    value,
}

fn list_print(list) {
    current = list;
    while (*current != null) {
      print current->value;
      current = &current->next;
    }

    return 0;
}

fn list_insert(list, item) {
    new_node = node { next: null, value: item };

    if (*list == null) {
        *list = new_node;
    } else {
        current = list;
        while (current->next != null) {
            current = &current->next;
        }
        current->next = new_node;
    }

    return 0;
}

fn main() {
    list = null;
    list_insert(&list, 3.14);
    list_insert(&list, false);
    list_insert(&list, "Hello, world!");
    list_print(&list);

    return 0;
}
```

## Compiling

Clone the repository and run:

```
cargo build --release
```

## Tests

To run the test suite, run:

```
cargo test
```

## Design notes

### Philosophy

The design of the system is a balance among performance and RISC-alikeness.

For example, when string concatenation was introduced into the language, there was a choice whether to overload the current opcode (`Opcode::Add`) or have a separate one. I decided to have a separate opcode `Opcode::Strcat` (and the `++` operator) at the expense of extending the instruction set with an additional instruction. However, this decision has kept the `Opcode::Add` implementation fast and simple and I avoided introducing a performance regression.

At other times, I tried to keep the instruction set minimal at the expense of performance. For example, the `>=` (greater or equal) operator is implemented with two instructions: `[Opcode::Less, Opcode::Not]`.

### Hand-rolled tokenizer vs `Logos`

Initially, `synapse` used a hand-rolled regex tokenizer, but I found it hard to e.g. report which character was not able to be tokenized, so I rewrote the tokenizer to use `Logos` which could do this trivially -- and, I'm sure it's faster than what I had.

## Contributing

Contributors to this project are very welcome -- specifically, suggestions (and PRs) as for how to make the whole system even faster, because I suspect there's still more performance left to be squeezed out.

## See also

[venom](https://github.com/xqb64/venom) - My first attempt, written in C

## Licensing

Licensed under the [MIT License](https://opensource.org/licenses/MIT). For details, see [LICENSE](https://github.com/xqb64/synapse/blob/master/LICENSE).
