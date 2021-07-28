# Typescript implementation

The typescript implementation expects the
[Deno](https://github.com/denoland/deno) runtime, which you can install with:

```sh
curl -fsSL https://deno.land/x/install/install.sh | sh
```

Run with:

```sh
deno run --allow-read lox.ts <path to lox file>
```

## Extensions to base lox in typescript implementation

- [x] dynamic property access
- [x] lambdas (anonymous functions)

## Additional builtins

- [x] str
- [x] strlen
- [x] strslice
- [x] parse_float
- [x] readfile
- [x] process_exit
- [x] process_args
- [x] eprint (output to stderr)

## Lox example demonstrating extensions

```lox
class Object { }
class Stack {
  init() {
    this.items = Object();
    this.count = 0;
  }
  push(item) {
    this.items.(this.count) = item;
    this.count = this.count + 1;
  }
  pop() {
    this.count = this.count - 1;
    var item = this.items.(this.count);
    this.items.(this.count) = nil;
    return item;
  }
  forEach(callback) {
    for (var i = 0; i < this.count; i = i + 1) {
      callback(this.items.(i));
    }
  }
}
var stack = Stack();
stack.push("foo");
stack.push("bar");
stack.forEach(fun(item) { print(item); });
// foo
// bar
```

## Testing

With the official repo set up, run the following command:

```sh
dart tool/bin/test.dart jlox --interpreter <path to repo>/tslox
```

Link to repo: (https://github.com/munificent/craftinginterpreters)[]

# Lox implementation

The lox implementation uses the dynamic property access extension, and is
expected to be run using it, like so:

```sh
deno run --allow-read lox.ts lox.lox -- <path to lox file>
```

The `--` is needed since `process_args` builtin is coded to remove the first two
command line arguments (to skip past the `--`).

## Testing

With the official repo set up as above, run the following command to test the
lox implementation of lox:

```sh
dart tool/bin/test.dart jlox --interpreter <path to repo>/loxlox
```

Link to repo: (https://github.com/munificent/craftinginterpreters)[]

## Extensions to base lox in lox implementation

- [x] dynamic property access

## Additional builtins

The same builtins in the typescript implementation are made available in the lox
implementation.

## Meta Usage

The string escaping is improved to allow running `lox.lox` with the lox
implementation (`lox.lox` itself).

The command to run lox with the lox implementation of lox:

```sh
deno run --allow-read lox.ts lox.lox -- lox.lox -- <path to lox file>
```

# Shortcuts

- The REPL prompt in the book is not implemented.
