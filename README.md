# Satori

## The language
Satori is a statically typed Lisp that aims to have a small core language that's
extensible through a macro system.

### Core language
The following special forms and functions make up the core language:
* `lambda`: the heart of the language ✓
* `let`: sequential local variables and local, possibly recursive function
  definitions
* `if`: the basic conditional ✓
* `eq`: for testing equality between atoms ✓
* `define`: for recursion that doesn't rely on the Y-Combinator ✓
* `quote`: literal s-expressions
* `atom`: because `car` and `cdr` are defined for lists only
* `car`: for returning the first half of a cons cell
* `cdr`: for returning the second half of a cons cell

In addition to these special forms and functions will be operators defined on
integers, floating point numbers, characters, strings and arrays. Structures,
unions and anything easily representable in LLVM IR will also be part of the
core language.
