# Satori

## What is Satori?
Satori is a statically typed functional language with type inference. There are
two types of data in Satori: atoms and structures. Satori's central focus is
providing, and allowing the creation of, macros and functions that operate on
structures; similar to how APL focuses on multidimensional arrays and Lisp
focuses on cons cells.

## Core language
The following special forms and functions make up the core language:
* `lambda`: the heart of the language ✓
* `let`: sequential local variables and local, possibly recursive, function
  definitions ✓
* `if`: the basic conditional ✓
* `eq`: for testing equality between atoms ✓
* `define`: for recursion that doesn't rely on the Y-Combinator ✓
* `cons`: to create new structures ✓
* `n`, where n is some integer: index into a structure ✓
* `quote`: to create a symbol
* `atom`: predicate for atoms, because `n` isn't defined on atoms

In addition to these special forms and functions will be operators defined on
integers, floating point numbers, characters, strings, and arrays.

## Notes

### Cons creates a new type
Cons creates a new structure, and subsequently type, taking multiple arguments
to specify the initial value and type for each element. Structural typing means
that any two conses that have the same element types are equal. Symbols can be
used to differentiate conses as the type of a symbol is the symbol itself.

Conses can be nested allowing for recursive structures such as lists, trees, and
graphs. For example:

```
(define list
  (lambda (x)
    (let ((f (lambda (x i))
               (if (eq i (arity x))
                   ()
                   (cons (i x) (list x (add 1 i))))))
      (f x 0))))
```

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
