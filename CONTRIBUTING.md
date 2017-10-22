# Contributing

I welcome pull requests, either for the Madlang template or the Miso/Haskell
frontend.

## Rules etc.
We follow the [rust standards of
conduct](https://www.rust-lang.org/en-US/conduct.html), with the addendum that
we are committed to providing a friendly, safe and welcoming environment
regardless of sex worker status or previous sex worker status.

In addition, please be aware that not everyone speaks English as a first
language.

## Builds

You should install [just](https://github.com/casey/just). Then:

```haskell
 $ cabal update
 $ cabal install shake
 $ just script
 $ ./shake
```
