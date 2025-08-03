# A Haskell Package of Many Theoretic Calculi

Build / Repl:

```sh
cabal build
cabal repl
```

## Untyped $\lambda$ Calculus

Higher-Order Abstract Syntax

Reduction Strategies:

- call by value
- call by name
- ...

Usage:

```sh
:m UTLC
reduceListInfiAuto $ fct @ 3 @ "s" @ "z"
```

will give a reduction list to `(s (s (s (s (s (s z))))))` in 1638 steps.



## UTLC Extended

`let` binding & primitive `Y` combinator

- `let` for CBV and $\lambda$ for CBN

Usage:

```sh
:m UTLCext
reduceListInfiAuto $ fct @ 3 @ "s" @ "z"
```

will give a reduction list to `(s (s (s (s (s (s z))))))` in 1621 steps.



## Parametric HOAS

Simple implementation and experiments of PHOAS:

[Parametric higher-order abstract syntax for mechanized semantics](https://dl.acm.org/doi/10.1145/1411204.1411226)



## $\lambda \mu \tilde{\mu}$ Calculus

From this paper:

[Grokking the Sequent Calculus (Functional Pearl)](https://dl.acm.org/doi/10.1145/3674639)

- $\mu$ and $\tilde{\mu}$ constructions
- critical pairs
- data/codata
- higher-order function as codata
- focusing and CPS transformation

Usage:

```sh
cabal repl
:m LMM
reduceFunList $ fcall "fct" [6] []
```

will give a reduction list to `⟨[720]|⋆⟩`, representing that the calculation result of $6!=720$.



## More Calculi to Come

...
