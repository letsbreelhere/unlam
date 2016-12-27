# SKI-calculus toy

This is a toy REPL that converts lambda calculus
expressions (using a pseudo-version of
[Unlambda's notation](http://www.madore.org/~david/programs/unlambda/))
into point-free SKI-calculus expressions. The algorithm is reasonably
simple, but requires several AST traversals to get right, so I used it
as an exercise in working with `Fix` and (a hacky, self-built version
of) *Datatypes à la Carte*-style functorial construction of ADTs.
