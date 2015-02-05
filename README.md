# SICP in Haskell and Racket #

This is my attempt at working through _Structure and Interpretation of Computer
Programs_ in Haskell and Racket, implementing each example and exercise that
appears in the text. I am using Racket mostly as a Scheme, the language the book
is targeted at.

I plan to skip over the "picture language" example and exercises
(pages 172-192).

I recommend reading and working through the book yourself before looking at the
implementations here, especially if you have never taken a class using SICP
(which was the case for me).

The section, exercise, and example numbers are contained in the comments in the
sources themselves, and the files are named roughly by what concepts/examples
are covered by each file.

You should be able to `grep` for any number, e.g., `grep -r 1.19 hs racket` and
find which files contain each exercise (if it exists).

### How do I get set up? ###

* For running the Haskell examples, installing GHC via your favorite package
  manager or the [Haskell Platform](https://www.haskell.org/platform/) should be
  sufficient.
* For the Racket examples, I used [the installer from the Racket
  website](http://download.racket-lang.org) but your package manager may have it
  as well.

All of the exercises can be run in their language's respective REPL.

For the Haskell implementations:

`ghci Factorial.hs`

For the Racket implementations:

`racket -if factorial.rkt`
