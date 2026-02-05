## cryptopals-hs
My Haskell-based solutions to the [cryptopals challenges](https://cryptopals.com/).

### Goal
Familiarise / refamiliarise myself with Haskell, which I first dabbled with back in my university days. I've read snippets of Haskell since then, like a few chapters of the Learn You a Haskell book, but now I want to learn it for real. Why? Many reasons: to push myself outside my comfort zone; to learn about mysterious concepts like monoids and monads; to master functional programming without an escape hatch; to finally try the cryptopals challenges, which I've had my eye on for years; and to see how much I like strict static typing & laziness.

### Usage
Run `cabal repl`, then import an individual challenge like so:

```
ghci> import Cryptopals.Set1.Challenge01
ghci> hexToBase64 "48656c6c6f"
"EhlbGxv"
```

See the challenge files for what functions to call.
