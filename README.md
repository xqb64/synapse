# synapse

A handcrafted virtual stack machine capable of executing a reduced instruction set consisting of only 31 microinstructions. The programs for the VM are written in a minimal, dynamically-typed, Turing-complete programming language featuring basic data types, functions, pointers, structures, and flow control. Besides the VM, the system includes a tokenizer, a recursive-descent parser, and a bytecode compiler.

![synapse](synapse.png)

## Examples

Fibonacci:

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

NOTE: The above program has been the go-to benchmark throughout the development cycle. The running time on my system (AMD Ryzen 3 3200G with Radeon Vega Graphics) is currently:

```
$ hyperfine --runs 5 './target/release/synapse benches/cases/fib40.syn'
Benchmark 1: ./target/release/synapse benches/cases/fib40.syn
  Time (mean ± σ):     16.039 s ±  0.233 s    [User: 15.995 s, System: 0.008 s]
  Range (min … max):   15.864 s … 16.444 s    5 runs
```

Let's see how long it takes Python to compute `fib(40)`:

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

To be fair, Python could execute this code in a blink of an eye with `@functools.lru_cache()`.

Linked list:

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

The design of the system is a balance among performance, RISC-alikeness, and keeping a dynamic type system.

For example, when string concatenation was introduced into the language, there was a choice whether to overload the current opcode (`Opcode::Add`) or have a separate one. I decided to have a separate opcode `Opcode::Strcat` (and the `++` operator) at the expense of extending the instruction set with an additional instruction. However, this decision has kept the `Opcode::Add` implementation fast and simple and I avoided introducing a performance regression.

At other times, I tried to keep the instruction set minimal at the expense of performance. For example, the `>=` (greater or equal) operator is implemented with two instructions: `[Opcode::Less, Opcode::Not]`.

### Hand-rolled tokenizer vs `Logos`

Initially, `synapse` used a hand-rolled regex tokenizer, but I found it hard to e.g. report which character was not able to be tokenized, so I rewrote the tokenizer to use `Logos` which could do this trivially -- and, I'm sure it's faster than what I had.

### Hand-rolled stack vs Vec

Initially, `synapse` used the `Vec` as the stack, but as I introduced pointers, this was no longer feasible because `Vec` would reallocate and thus invalidate the pointers -- so a fixed-size stack had to be used.
