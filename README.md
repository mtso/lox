# typescript implementation

typescript implementation expects the [Deno](https://github.com/denoland/deno)
runtime, which you can install with:

```
curl -fsSL https://deno.land/x/install/install.sh | sh
```

# extensions to base lox

- [v] dynamic property access
- [ ] anonymous functions
- [ ] exceptions // maybe not needed with class Result(ok, value)
- [-] builtins: strlen, substr, readfile, stdout
- [ ] improved print? (make builtin rather than stmt)

## extension example

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
    for (var i = 0; i < this.count; i++) {
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
