# Description
A Haskell implementation of the untyped lambda calculus from
Chapter 7.
Run with `cabal run < fileName`.

# Incompatabilities
+ It seems the reference implementation allows something along the lines of
`var/;` to add a variable permanently to the context.
As this is not documented in the preceding chapters and is a bit weird I have
not included this feature.
