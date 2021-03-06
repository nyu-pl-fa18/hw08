# Homework 8 (20 Points)

The deadline for Homework 8 is Monday, November 26, 6pm. The late
submission deadline is Thursday, November 29, 6pm.

## Getting the code template

Before you perform the next steps, you first need to create your own
private copy of this git repository. To do so, click on the link
provided in the announcement of this homework assignment on
Piazza. After clicking on the link, you will receive an email from
GitHub, when your copy of the repository is ready. It will be
available at
`https://github.com/nyu-pl-fa18/hw08-<YOUR-GITHUB-USERNAME>`.
Note that this may take a few minutes.

* Open a browser at `https://github.com/nyu-pl-fa18/hw08-<YOUR-GITHUB-USERNAME>` with your Github username inserted at the appropriate place in the URL.
* Choose a place on your computer for your homework assignments to reside and open a terminal to that location.
* Execute the following git command: <br/>
  ```git clone https://github.com/nyu-pl-fa18/hw08-<YOUR-GITHUB-USERNAME>.git```<br/>
  ```cd hw08```


## Preliminaries

We assume that you have installed a working OCaml distribution,
including the packages `ocamlfind`, `ocamlbuild`, and `ounit`. Follow
the instructions in the notes
for
[Class 7](https://github.com/nyu-pl-fa18/class08#installation-build-tools-and-ides) if
you haven't done this yet.

## Submitting your solution

Once you have completed the assignment, you can submit your solution
by pushing the modified code template to GitHub. This can be done by
opening a terminal in the project's root directory and executing the
following commands:

```bash
git add .
git commit -m "solution"
git push
```

You can replace "solution" by a more meaningful commit message.

Refresh your browser window pointing at
```
https://github.com/nyu-pl-fa18/hw08-<YOUR-GITHUB-USERNAME>/
```
and double-check that your solution has been uploaded correctly.

You can resubmit an updated solution anytime by reexecuting the above
git commands. Though, please remember the rules for submitting
solutions after the homework deadline has passed.

## Preliminaries

The goal of this homework is to implement a small library for
arbitrary precision integer arithmetic in OCaml. The code template
consists of two files

* [`arithmetic.ml`](src/arithmetic.ml): the implementation of the
  library
  
* [`tests.ml`](src/tests.ml): a set of unit tests for testing your
  code.
  
As in the last homework assignment, the repository is configured to
work with Merlin. Also, there is again a build script that you can run
as

```bash
./build.sh
```

to compiler your code. Once compiled, you can run the unit tests with

```bash
./tests.native
```

## Problem 1: Big Natural Numbers (7 Points)

We start with an implementation of an OCaml module `BigNat` for
manipulating arbitrarily large natural numbers. The
file [`arithmetic.ml`](src/arithmetic.ml) defines the signature
`BigNatType` of this module, as well as an incomplete implementation
of `BigNat`.

1. Understand the partial implementation of big natural numbers in the
   module `BigNat` and complete the missing functions `sub`
   (subtraction with carry) and `( ** )` (exponentiation). When you
   implement `sub`, pay attention to the elimination of leading zeros
   to preserve the uniqueness of the representation of natural
   numbers. The predefined function `zcons` is useful for this.
   
   Your implementation of `sub` should raise the exception
   `Invalid_argument "result is negative"` if the result of the
   subtraction is negative. You can do this with
   
   ```ocaml
   raise (Invalid_argument "result is negative")
   ```
   
   **(5 Points)**


1. Write a function 

   ```ocaml
   fac: BigNat.bignat -> BigNat.bignat
   ```
   that computes the factorial function on big natural numbers using
   `BigNat`. Further, write a function 
   
   ```ocaml
   digits: BigNat.bignat -> int
   ```
   
   that computes the number of decimal digits of the given big natural number. 

   **(2 Points)**

**Hint:** You can open a module `M` locally using the syntax

```ocaml
let open M in
e
```

Then the members `x` of `M` can be referred to in expression `e`
directly as `x` rather than prefixing the name with the module
qualifier `M.x`. Equivalently, you can also use the notation `M.(e)`
which has the same effect.

The definition

```ocaml
let (+) x y = add x y 0
```

redefines the infix operator `+` to work on the type `bigint` rather
than `int`. Note that OCaml does not support operator
overloading. That is, the above definition shadows the standard
definition of `+` on the type `int` in the remainder of the module
implementation. Subsequent expressions `x + y` within the module scope
will refer to the new definition of `+` and, hence, `x` and `y` will
be expected to be `bigint` values rather than `int` values. If you
need to use `+` on `int` values `x` and `y`, you can do this by
writing `Pervasives.(x + y)`. This will locally open the `Pervasives`
module, which contains all standard inbuilt operators and functions of
OCaml including `+` on `int`. The same restrictions apply to the other
operators like `(-)`, `( * )`, `(/)`, etc.


## Problem 2: Big Integers (13 Points)

Next, we want to build a module `BigInt` that provides arbitrary precision
integer arithmetic operations by building on top of our module
`BigNat`. The resulting module should have the signature `BigIntType`.

Rather than using `BigNat` directly, we parameterize our
implementation of `BigInt` by a module of the signature `BigNatType`
so that we can easily swap out one implementation of `BigNat` with
another. That is, we implement a functor

```ocaml
module MakeBigInt (BigNat : BigNatType) : BigIntType
```

In our functor, we implement the type `bigint` of `BigIntType` as follows

```ocaml
type bigint = bool * BigNat.bignat
```

where a Boolean marking `true` indicates that the represented number
is greater than or equal to 0. 

Complete the missing implementations of the functions in `MakeBigInt`
using the functions provided by `BigNat`. The function `to_string`
should use the symbol `-` as the sign for negative numbers. The
exponentiation function `( ** )` should raise an exception
`Invalid_argument "negative exponent"` if the given exponent is
negative.

**Hints:** Be careful to ensure that the representation of big integers
is unique so that the meaning of equality of two `bigint` values is
correct. That is, the number 0 must be represented uniquely. You may
want to write a helper function

```ocaml
val norm: bigint -> bigint
```

that normalizes a value of type `bigint` to maintain the uniqueness
invariant.

Also, make sure that your implementation of `mod` is consistent with
OCaml's `mod` operator on `int`, which computes the *integer
remainder*. That is for two integers `x` and `y`, we must have the
following properties: `x = (x / y) * y + x mod y` and `abs(x mod y) <=
abs(y) - 1`. If `y = 0`, `x mod y` raises `Division_by_zero`. Note
that `x mod y` is negative only if `x < 0`.
