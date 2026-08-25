# Nomad Lisp Reference
This markdown document serves as an informal reference to define what nomad is.

## What makes nomad unique?
Nomad is unique because it is designed to be TINY, like, even more so than most embeddable languages.

A nomad lisp interpreter usually consists of these parts:
- A lexer for turning source code into tokens
- A parser for constructing S-Expressions of tokens
- An evaluator that knows how to call callable objects (lambdas, macros and native functions) and resolve atoms to values
- A prelude for common programming cases
- A strong embedding API for the host language

### About the prelude
Generally, a nomad implementation's prelude should include the following:
- Constructs for declaring variables, functions, macros and lexical scopes, such as `let`, `letfun`, `letmac`, `scoped` etc.
- Conditionals, such as `if`, `=`, `>`, `<`, `>=`, `<=` etc.
- Arithmetic operations, such as `+`, `-`, `*`, `/`, `modulo` etc.
- Functions for printing to stdout/reading from stdin, such as `print`, `println`, `readln` etc.
- Mechanisms for exception catching/throwing, `try` and `throw`
- An explicit way of mutating variables, `mut` and `record_mut`

What makes the prelude special is that it is not part of the language's grammar!

The prelude is a collection of functions that are native callbacks.

What's important about nomad callbacks is that they receive syntax/S-Expressions, not evaluated values!

This is important because if nomad callbacks were to evaluate their functions eagerly, constructs like `if` would not be possible.

You can find documentation about concrete prelude implementations [here](https://github.com/Moritisimor/nomad-lisp/tree/main/docs).

### About the embedding API
As was mentioned in the prelude section, nomad callbacks must receive S-Expressions.

An example of how such an API may look like (gomad implementation):
```go
package main

import (
	"fmt"
	"log"

	"github.com/Moritisimor/gomad/expr"
	"github.com/Moritisimor/gomad/interpreter"
	"github.com/Moritisimor/gomad/value"
	"github.com/Moritisimor/gomad/eval"
)

func main() {
	interp := interpreter.New() // Create new interpreter
	interp.RegisterNative("log", func(
        e []expr.Expression, 
        env *value.Env,
    ) (value.Value, error) {
		if len(e) != 1 { // Check argument length
			return value.NewUnit(), fmt.Errorf("Error in call to log: Expected one argument, got %d", len(e))
		}

		// We expect a string as the argument, anything else is an error.
		logString, err := eval.GetString(e[0], env)
		if err != nil {
			return value.NewUnit(), fmt.Errorf("Error in call to log: %s", err.Error())
		}

		log.Println(logString)
		return value.NewUnit(), nil
	})

	interp.DoString("(log \"Gomad interpreter running!\")")
}
```

The key is to let the API user have access to the AST/S-Expressions, while giving comfortable functions such as `GetString` when lazy evaluation of callback arguments is not necessary.

### The type system
Nomad's type system is relatively small. Its types are:
- string
- number (best implemented as a 64-bit floating point integer)
- bool
- list
- records
- lambda (functions that are written in nomad itself)
- native functions
- macros
- unit (the terminal type, similar to null/nil in other languages)

### Macros
Macros are kept deliberately small and simple in nomad. They are more like templates in which syntax is injected upon invocation.

An example:
```lisp
(letmac when (cond body) if cond body unit)
```

Here, a macro is defined that takes 2 arguments, `cond` and `body`.

Let's invoke it:
```lisp
(when true (println "Hello World!"))
```

This invocation should expand into this:
```lisp
(if true (println "Hello World!") unit)
```

It's also important that, when a macro argument appears nested inside another list, it should be replaced as well.

This was a small bug my first implementation had that I would like to warn you about.

## Any questions/did I forget something?
If you have any questions or if I forgot something, feel free to let me know by sending me an email at `devMoritisimor@proton.me` or by posting an issue!
