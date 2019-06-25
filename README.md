# Monkey Language Interpreter

Monkey is a language that supports:

- integers
- booleans
- strings
- arrays
- hashes
- prefix-, infix- and index operators
- conditionals
- global and local bindings
- first-class functions
- return statements
- closures

This example interpreter is written in Go, using the materials of the [Writing An Interpreter In Go](https://interpreterbook.com) book.

## Example of Monkey Code

```
let name = "Monkey";
let age = 1;
let inspirations = ["Scheme", "Lisp", "JavaScript", "Closure"];
let book = {
    "title": "Writing An Interpreter In Go",
    "author": "Thorsten Ball"
};

let printBookName = fn(book) {
    let title = book["title"];
    let author = book["author"];
    puts(author + " - " + title);
}

printBookName(book);
// => prints: "Thorsten Ball - Writing An Interpreter In Go"

let fibonacci = fn(x) {
    if (x == 0) {
        0
    } else {
        if (x == 1) {
            return 1;
        } else {
            fibonacci(x - 1) + fibonacci(x - 2);
        }
    }
};

let map = fn(arr, f) {
    let iter = fn(arr, accumulated) {
        if (len(arr) == 0) {
            accumulated
        } else {
            iter(rest(arr), push(accumulated, f(first(arr))));
        }
    };

    iter(arr, []);
};

let numbers = [1, 1 + 1, 4 - 1, 2 * 2, 2 + 3, 12 / 2];
map(numbers, fibonacci);
// => returns: [1, 1, 2, 3, 5, 8]
```

# Running the REPL

To run the REPL (run-eval-print loop) interpreter, run

    go run main.go

# Running the Tests

To run automated tests in all of the packages, run

    go test ./...