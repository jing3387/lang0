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
* `nth`: index into a structure ✓
* `arity`: get the number of elements in a structure ✓
* `cast`: type-safe casting of a union to one of its member types
* `quote`: to create literal data
* `atom`: predicate for atoms, because `n` isn't defined on atoms

## Macros
* `bind`: destructuring bind, implemented in terms of `let` and `nth`
* `match`: destructuring bind across a union's cases, implemented in terms of
  `bind` and `if`

In addition to these special forms and functions will be operators defined on
integers, floating point numbers, characters, strings, and arrays.

## Examples

The classic factorial function demonstrating recursion:
```
(define factorial
  (lambda (x)
    (if (eq x 0)
        1
        (mul x (factorial (sub x 1))))))
```

Compute the length of a list:
```
(let ((list (lambda rest
              (if (not rest)
                  ()
                  (cons (nth 0 rest) (list rest)))))
      (length (lambda (lst)
                (cast lst lst
                  (() 0)
                  ((_ _) (let ((xs (nth 1 lst)))
                          (add 1 (length xs))))))))
  (length (list 1 2 3)))
```

## Notes

### Cons creates a new type
Cons creates a new structure, and subsequently type, taking multiple arguments
to specify the initial value and type for each element. Structural typing means
that any two conses that have the same element types are equal. Symbols can be
used to differentiate conses as symbols evaluate to themselves in the type
system.

Note: Conses can be nested allowing for recursive structures such as lists,
trees, and graphs.

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

## Branches create a new type
Conditionals, casts and anything that has multiple cases can return different
types from each branch which results in a union being created.
