# Satori

## The plan
Satori is a statically typed Lisp that aims to have a small core language that's
extensible through a macro system. Currently the following special forms are
planned:
* `lambda`: the heart of the language ✓
* `let`: since sequential local variables are easy to represent in LLVM ✓
* `if`: the basic conditional
* `eq`: for testing equality between atoms
* `atom`: because `car` and `cdr` are defined for lists only
* `define`: for recursion that doesn't rely on the Y-Combinator
* `quote`: to represent literal s-expressions
* `car`: for returning the first half of a cons cell
* `cdr`: for returning the second half of a cons cell

In addition to these special forms will be operators defined on integers,
floating point numbers, characters, strings and arrays. Structures and unions
will also be part of the core language.

From here the language will be written in itself. Taking advantage of the easy
interoperation with C, Satori will be bootstrapped by writing a compiler
directly instead of an interpreter then a compiler.
