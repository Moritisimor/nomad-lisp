# The Nomad Lisp Language Standard

Edition 1, August 2026

## 1. Purpose

Nomad is a small Lisp for embedding in applications and for writing scripts. The language is deliberately compact: a reader, a small evaluator, host-defined native functions, and a source prelude are enough to build a useful implementation.

This document defines the portable language. It is the reference for authors of Nomad implementations, embedders, standard libraries, and conformance tests. The OCaml implementation in this repository is the original implementation, but where an implementation and this document disagree, the disagreement should be treated as a bug and discussed before either one is changed.

The words **must**, **must not**, **should**, **should not**, and **may** are normative. Examples are normative when they state a result with `=>`.

## 2. Conformance

Nomad has three conformance levels.

- A **Nomad Core implementation** provides the source reader, values, lexical environments, evaluator, structured errors, and the native-function interface described here.
- A **Nomad Standard implementation** is a Core implementation with the standard native environment and source prelude from sections 10 and 11.
- A **Nomad Script implementation** is a Standard implementation with the file, process, directory, and environment functions from section 12.

An implementation must state which level it supports. It may omit Script facilities when the host cannot provide them, such as in a browser, a sandbox, or a database extension. It must not silently install a function with incompatible behavior under a standard name.

Implementations may add values, functions, and embedding features. Extensions must not change the meaning of a conforming program.

## 3. Source text

Nomad source is UTF-8. An implementation must reject malformed source text or replace malformed input before tokenization in a documented way. Source files conventionally use the `.nomad` extension.

Whitespace separates tokens. The portable whitespace characters are space, tab, line feed, and carriage return. Vertical tab and form feed are also treated as whitespace by string trimming functions, but need not separate source tokens.

A comment begins with `#` where a new token could begin and continues through the end of the line.

```lisp
# a complete line
(+ 1 # the second operand follows on the next line
   2)
```

Names are case-sensitive. `name`, `Name`, and `NAME` are different symbols.

### 3.1 Grammar

The grammar below is descriptive EBNF. Lexical rules for strings and numbers are given afterward.

```text
program     = { expression } ;
expression  = number | string | boolean | unit | symbol | list ;
list        = "(" { expression } ")" ;
boolean     = "true" | "false" ;
unit        = "unit" ;
symbol      = symbol-char { symbol-char } ;
symbol-char = any non-whitespace character except "(" and ")" ;
```

`true`, `false`, and `unit` are recognized only as complete tokens. Names such as `truest` and `units` are ordinary symbols.

Parentheses must balance. There is no quote reader syntax, dotted-pair syntax, quasiquote, or reader macro in Edition 1.

### 3.2 Strings

Strings are enclosed in double quotes. These escapes are required:

| Source | Character |
|---|---|
| `\n` | line feed |
| `\t` | tab |
| `\r` | carriage return |
| `\b` | backspace |
| `\"` | double quote |

An unknown escape is preserved, including its backslash. A string may contain Unicode text directly.

### 3.3 Numbers

Numbers are IEEE 754 binary64 values. Implementations must accept decimal integers, decimal fractions, decimal exponents, underscores between numeric characters, and OCaml-style hexadecimal numbers. A decimal or hexadecimal literal must begin with an ASCII digit after an optional minus; `.5` is therefore not a number literal, while `0.5` is.

```lisp
42
-2.5
1e6
1_000
0x1A
0xA.8p-2
0x.8
```

A sign is portable only when it is a leading minus immediately followed by an ASCII digit. Positive numbers do not use a leading `+`, because `+` is normally a symbol.

When a token begins like a number but is not a valid number, tokenization must fail. `nan` and `inf` are not source literals, although calculations may produce those values.

## 4. Values

A conforming implementation has these value kinds:

| Kind | Meaning |
|---|---|
| number | IEEE 754 binary64 number |
| string | Unicode string |
| bool | `true` or `false` |
| list | immutable sequence of values |
| record | mutable collection of named fields |
| lambda | function written in Nomad with a captured lexical environment |
| native | function supplied by the implementation or host |
| macro | unevaluated syntax template |
| unit | a single value meaning that no useful result exists |

The source expression `()` evaluates to an empty list. It is not the same value as `unit`.

Lists are persistent values. `cons`, `cdr`, and `append` must not mutate an existing list. Records are reference values: assigning a record to another name shares the same record, and a mutation through either name is visible through the other.

## 5. Programs and evaluation

A program is a sequence of zero or more top-level expressions. They are evaluated from left to right in one environment. The result of the program is the result of its last expression. An empty program returns `unit`.

Evaluation stops at the first error. Later top-level expressions must not run.

Literal expressions evaluate to their corresponding values. A symbol is resolved in the current lexical environment and then through its parents. Resolving an unbound symbol is an evaluation error.

For a non-empty list:

1. Evaluate its first expression.
2. If the result is a lambda, evaluate its arguments as described in section 6 and invoke it.
3. If the result is a macro, expand it as described in section 7 and evaluate the expansion in the caller's environment.
4. If the result is a native function, pass the remaining expressions to the native callback without evaluating them first.
5. Any other result is an evaluation error.

That fourth rule is central to Nomad. Native callbacks receive syntax, not an eager list of values. It allows `if`, `let`, `and`, `try`, and host extensions to control evaluation without adding special grammar.

There are no evaluator keywords hidden from the host API. The usual core forms are native functions installed in the initial environment. They may be passed around or shadowed like other bindings, provided their observable evaluation rules are preserved.

Except where a function explicitly says otherwise, arguments that are evaluated are evaluated from left to right and evaluation stops at the first error. `cons` is the deliberate exception: it evaluates its list argument before its element argument, matching the original implementation.

## 6. Environments and functions

Environments are lexical scopes linked to optional parent scopes. A binding belongs to one environment.

`let` creates a binding in the current environment. It is an error to bind the same name twice in that environment. A child environment may shadow a name from its parent. `mut` searches from the current environment outward and replaces the first matching binding. Mutating an unknown name is an error.

A lambda captures the environment in which it was created. Lambda arguments are evaluated from left to right in the caller's environment. A fresh child of the captured environment is then created and the parameter values are bound there. The body is evaluated in that child environment.

Lambda arity is exact. Too few or too many arguments are errors. Duplicate parameter names are rejected when the lambda is invoked.

`letfun` must support direct recursion. The captured environment therefore observes the function's own binding after `letfun` installs it. Lexical capture also makes closures and manually written currying work:

```lisp
(let add (lambda (x) (lambda (y) (+ x y))))
(let add10 (add 10))
(add10 20) # => 30
```

### 6.1 Tail calls

A Standard implementation must run proper tail calls without consuming an unbounded host call stack. This guarantee applies to direct and mutual lambda calls in these tail positions:

- a lambda body;
- the selected branch of `if`;
- the selected arm of `switch`;
- the last expression of `do`;
- the body of `scoped`;
- both the attempted expression and handler of `try`.

A loop of at least one million simple tail calls must not overflow the host stack. An implementation may use a trampoline, an explicit evaluator loop, bytecode, or native tail calls.

An optimized evaluator must identify a core form by both its binding name and the identity of its registered native callback. Shadowing `if` with another callable must not accidentally give that callable `if` semantics.

## 7. Macros

Nomad macros are deliberately simple, unhygienic syntax templates.

```lisp
(letmac when (condition body)
  if condition body unit)
```

Macro arguments are not evaluated. Invocation requires the exact declared arity. The expressions written after the parameter list are the elements of one application template, not a sequence of statements. Each occurrence of a parameter symbol in that template is replaced with the corresponding argument expression. Substitution descends recursively through nested lists. The resulting list is evaluated in the caller's environment.

Nomad macros do not rename bindings, preserve lexical hygiene, destructure input, or run a separate compile-time language. Authors must choose names with the usual care required by an unhygienic macro system.

## 8. Equality and display

`=` follows these rules:

- numbers, strings, and booleans compare by value;
- `unit` equals `unit`;
- lists compare element by element using these same rules;
- records compare by identity, not by their fields;
- lambdas, native functions, and macros compare by callable identity;
- values of different kinds are not equal.

Human-readable value formatting is part of the portable behavior used by `print`, `sprint`, and `to_string`:

| Value | Text |
|---|---|
| integer-valued finite number | no decimal suffix, for example `42` |
| other finite number | two digits after the decimal point, for example `2.50` |
| not-a-number | `nan` |
| positive/negative infinity | `inf` / `-inf` |
| bool | `true` / `false` |
| list | values separated by one space inside parentheses |
| unit | `<UNIT>` |
| lambda | `<FUNCTION>` |
| native | `<NATIVEFUNCTION>` |
| macro | `<MACRO>` |
| record | `<RECORD>` |

Strings are displayed as their contents without quotes. Formatting must not depend on the host's locale.

## 9. Errors and control signals

The embedding API must distinguish at least these outcomes:

- tokenization error;
- parse error;
- evaluation error;
- I/O error;
- requested process exit with an integer status.

Errors are values of the host API, not Nomad values. `try` catches evaluation errors only. It must not catch tokenization errors, parse errors, I/O errors, or exit requests.

`throw` evaluates one argument. The result must be a string, which becomes the evaluation error message.

`exit` and `bye` must return an exit request to an embedding host. They must not terminate the host process from inside the evaluator. A command-line frontend should translate the request into its own process exit status.

Exact diagnostic wording may be retained for compatibility, but portable programs must rely on error categories rather than parsing messages.

## 10. Standard native environment

A Standard implementation provides the names in this section. These are native callbacks even when their behavior resembles syntax in another Lisp.

### 10.1 Bindings, callables, and control flow

| Form | Required behavior |
|---|---|
| `(let name expression)` | Evaluate `expression`, bind it in the current scope, return `unit`. |
| `(mut name expression)` | Evaluate and replace the nearest existing binding, return `unit`. |
| `(lambda (parameters...) body)` | Create a lexical lambda. |
| `(letfun name (parameters...) body)` | Bind a recursive lexical lambda, return `unit`. |
| `(letmac name (parameters...) body...)` | Bind a macro template, return `unit`. |
| `(if condition yes no)` | Evaluate only `condition` and the selected branch. The condition must be a bool. |
| `(do expressions...)` | Evaluate in order and return the last result; `(do)` returns `unit`. |
| `(scoped ((name value)...) body)` | Evaluate each value in the outer environment, bind it in a fresh child scope, then evaluate `body` there. |
| `(switch value (matcher result)...)` | Evaluate `value` once, test matchers in order, and evaluate only the first matching result. `_` is an unconditional arm. No match returns `unit`. At least one arm is required. |
| `(try attempted handler)` | Return `attempted`, or evaluate `handler` if `attempted` raises an evaluation error. |
| `(throw message)` | Raise an evaluation error carrying a string message. |

Switch matcher errors propagate. They are not treated as failed matches. Each arm must contain exactly two expressions.

### 10.2 Records

| Function | Required behavior |
|---|---|
| `(record (field value)...)` | Evaluate fields in source order and return a new record. |
| `(. record field)` | Return an existing field. The field name is syntax and is not evaluated. |
| `(record_mut record field value)` | Mutate an existing field and return `unit`. It must not create a missing field. |

### 10.3 Arithmetic and logic

| Function | Required behavior |
|---|---|
| `(+ a b)` | Add two numbers or concatenate two strings. |
| `(- a b)` | Numeric subtraction. |
| `(* a b)` | Numeric multiplication, or repeat a string by a numeric factor in either order. |
| `(/ a b)` | Numeric division. Division by zero is an evaluation error. |
| `(mod a b)` | IEEE 754 floating-point remainder. |
| `(= a b)` | Equality from section 8. |
| `(> a b)`, `(>= a b)`, `(< a b)`, `(<= a b)` | Numeric comparison. |
| `(and a b)` | Require booleans; do not evaluate `b` when `a` is false. |
| `(or a b)` | Require booleans; do not evaluate `b` when `a` is true. |

String repetition converts the factor to an integer by truncating toward zero. A factor below one returns the empty string. Implementations should impose a documented allocation limit; the reference implementations cap the repetition count at 1,000,000.

### 10.4 Lists

| Function | Required behavior |
|---|---|
| `(list expressions...)` | Evaluate from left to right and return a list. |
| `(append left right)` | Return the concatenation of two lists. |
| `(cons value list)` | Evaluate `list` first, then `value`, and return a new list. |
| `(car list)` | Return the first element, or `unit` for an empty list. |
| `(cdr list)` | Return the list without its first element, or `unit` for an empty list. |

### 10.5 Strings and conversion

| Function | Required behavior |
|---|---|
| `(sprint expressions...)` | Evaluate in order, concatenate their display forms, return a string. |
| `(chars string)` | Return one string per Unicode scalar value. |
| `(lower string)` | Lowercase ASCII `A` through `Z` only. |
| `(trim string)` | Remove leading and trailing ASCII space, tab, LF, CR, vertical tab, and form feed. |
| `(splitws string)` | Split on ASCII space and discard pieces that are empty after ASCII trimming. |
| `(to_string value)` | Return the display form from section 8 as a string. |
| `(string_to_num string)` | Parse a number using the source number rules or raise an evaluation error. |

`chars` must not split a UTF-8 sequence or a UTF-16 surrogate pair. For example, `(strlen "a😀b")` is `3` when the standard prelude is loaded.

### 10.6 Type predicates

The required predicates are `isunit`, `isstr`, `isnum`, `islist`, `isfun`, `isnative`, `ismac`, `isbool`, and `isrecord`.

Each accepts exactly one expression. It returns `true` when the evaluated value has the named kind and `false` otherwise. For compatibility with the original implementation, an evaluation error while checking the expression produces `false`; other error classes and exit requests propagate.

### 10.7 Console functions

| Function | Required behavior |
|---|---|
| `(print expressions...)` | Display values consecutively on standard output and return `unit`. |
| `(println expressions...)` | Do the same and append one newline. |
| `(readln prompt)` | Display and flush a string prompt, read one line from standard input, and return it without the line ending. End of input is an evaluation error. |

`print` and `println` evaluate every argument before producing output. This prevents partial output when a later argument fails.

`print_env` is an optional diagnostic extension. Its output format is not portable.

## 11. Standard source prelude

A Standard implementation loads the following source-defined names into each new interpreter:

| Name | Meaning |
|---|---|
| `not` | boolean negation |
| `inc`, `dec` | add or subtract one |
| `unless`, `when`, `!=` | convenience macros |
| `typeof` | standard type name as a string |
| `foldl` | left fold |
| `begins_with`, `ends_with` | list prefix and suffix tests |
| `has_prefix`, `has_suffix` | string prefix and suffix tests |
| `list_init` | build a list from a length and index function |
| `map`, `mapi` | transform a list, optionally with indexes |
| `filter` | retain values for which a predicate returns true |
| `rev` | reverse a list |
| `len` | list length |
| `strlen` | Unicode scalar count via `chars` |
| `foreach`, `foreachi` | visit values for side effects |
| `nth`, `nth_unit` | indexed access, with error or `unit` when absent |
| `range` | inclusive index range from a list |

The canonical definitions are in [`lib/stdlib.ml`](../lib/stdlib.ml). Implementations may implement them natively, but observable behavior must remain the same.

## 12. Script environment

A Nomad Script implementation provides these additional functions:

| Function | Required behavior |
|---|---|
| `(include path-symbol)` | Read and evaluate a source file in the current environment, return `unit`. |
| `(read_file path)` | Read a text file into a string. |
| `(write_file path content)` | Replace or create a text file and return `unit`. |
| `(remove_file path)` | Remove a file and return `unit`. |
| `(read_dir path)` | Return immediate entry names as a lexicographically sorted list of strings. |
| `(mkdir path)` | Create a directory and return `unit`. |
| `(remove_dir path)` | Remove an empty directory and return `unit`. |
| `(chdir path)` | Change the process working directory and return `unit`. |
| `(cwd)` | Return the process working directory. |
| `(get_env name)` | Return an environment variable or raise an evaluation error. |
| `(get_env_unit name)` | Return an environment variable or `unit` when absent. |
| `(exec command)` | Run one command string through the platform shell and return its status as a number. |
| `(exit code)` | Return an exit request carrying a 32-bit integer status. |
| `(bye)` | Equivalent to `(exit 0)`. |

Filesystem and process failures are evaluation errors when called from Nomad. Failure to open the initial file passed to the embedding API is an I/O error. Platform-specific status encoding from `exec` may differ and must be documented.

Script facilities act with the permissions and working directory of the host process. An embedder may replace or omit them for security.

## 13. Embedding contract

Embedding is a primary use of Nomad, not an afterthought. A conforming Core API must make the following operations possible, even if their names differ between host languages:

1. Create an isolated interpreter with a fresh global environment.
2. Supply the initial `args` list as host strings.
3. Parse and evaluate a string containing any number of top-level forms, returning the last value.
4. Evaluate a parsed expression without converting it back to source.
5. Read and install global bindings from the host.
6. Register a native function under a name.
7. Distinguish the structured outcomes listed in section 9.

A Script API must also allow a file to be evaluated in an existing interpreter so definitions remain available afterward.

The `args` binding is a list containing exactly the strings supplied by the host, in order. Whether a command-line frontend includes the script path is a frontend convention, not a language rule.

### 13.1 Native functions

A registered native function receives:

- an ordered view of the unevaluated argument expressions; and
- the caller's current environment.

It returns either a Nomad value or a structured host error. The API should provide helpers for evaluating an expression and requiring a number, string, bool, list, record, or other common kind.

Native functions may choose evaluation order, skip arguments, evaluate an argument more than once, introduce a scope, or return an error. They must not be forced through an eager value-only interface.

### 13.2 Host control and isolation

The evaluator must not call the host's process-exit primitive. It must not turn a Nomad evaluation error into an uncaught host exception during ordinary use. Unrecoverable allocation failure and host defects are outside this guarantee.

Interpreter instances must have separate global environments. Records, closures, and other values intentionally shared through the host API keep their ordinary identity semantics. Thread safety may be opt-in, but it must be documented.

Hosts may impose limits on execution time, recursion steps, source size, string repetition, process access, or memory. A limit should produce a structured error rather than corrupt interpreter state.

## 14. Implementation guidance

This section is not normative, but it records choices that have worked well in the existing implementations.

- Parse standard-library source once, then evaluate the cached syntax in each fresh global environment.
- Represent lists as persistent linked structures so `cons` and `cdr` are constant-time and safe to share.
- Keep records behind shared references.
- Use compact environment frames for lambda parameters and a map for dynamic bindings.
- Implement evaluation as a loop or trampoline. Relying on the host stack makes otherwise ordinary Nomad scripts fragile.
- Build strings with a buffer rather than repeated concatenation.
- Compare optimized core forms by callback identity as well as name.
- Keep the command-line frontend separate from the embedding library.

The reader, evaluator, and native environment should remain usable without a REPL or operating-system module. That separation is what lets the same language fit into a command-line tool, editor, game, server, or application configuration layer.

## 15. Portable examples

All Standard implementations must agree on these results:

```lisp
(+ (* 10 5) (- 1000 250))
# => 800

(let truest 55)
truest
# => 55

(let add (lambda (x) (lambda (y) (+ x y))))
(let add10 (add 10))
(add10 20)
# => 30

(= unit unit)
# => true

(chars "a😀b")
# => (a 😀 b)

(switch 9
  (1 "one")
  (_ "many"))
# => many

(try (throw "problem") "recovered")
# => recovered
```

The conformance tests in this repository are executable examples of the same contract. New language behavior should be added to this document and to those tests together.

## 16. Versioning

This is Edition 1 of the language standard. Clarifications that do not change valid program behavior may be made within the edition. A change that makes a previously conforming program behave differently requires a new edition or an explicitly named extension.

Implementations should publish the highest edition and conformance level they support, for example: `Nomad Edition 1 — Standard + Script`.
