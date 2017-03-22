# Satori

## The language
Satori is a statically typed Lisp that aims to have a small core language that's
extensible through a macro system.

### Core language
The following special forms and functions make up the core language:
* `lambda`: the heart of the language ✓
* `let`: sequential local variables and local, possibly recursive, function
  definitions ✓
* `if`: the basic conditional ✓
* `eq`: for testing equality between atoms ✓
* `define`: for recursion that doesn't rely on the Y-Combinator ✓
* `quote`: literal s-expressions
* `atom`: because `car` and `cdr` are defined for lists only
* `car`: for returning the first half of a cons cell
* `cdr`: for returning the second half of a cons cell

In addition to these special forms and functions will be operators defined on
integers, floating point numbers, characters, strings and arrays.

## Ideas

### Cons creates a new type
* Cons creates a new structure, taking multiple arguments
* Type inference analyses conses and it's expected you'll match on all top-level
  types
* Nested conses are pointers to structures, this allows lists, trees and even
  graphs to be created using the one operator

> In computer science, tuples are directly implemented as product types in most
> functional programming languages. More commonly, they are implemented as
> record types...
https://en.wikipedia.org/wiki/Tuple

> [Cons] is loosely related to the object-oriented notion of a constructor,
> which creates a new object given arguments, and *more closely related to the
> constructor function of an algebraic data type system*.
https://en.wikipedia.org/wiki/Cons

> Support for labeled records allow structural typing to regain the flexibility
> nominative typing possesses in distinguishing similar types. The 'label' isn't
> a declared name implicit to the environment or unique to a given type;
> instead, it is part of the structure and must be part of each value and
> included for pattern-matching.
http://wiki.c2.com/?NominativeAndStructuralTyping
