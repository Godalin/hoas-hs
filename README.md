# `hoas-hs`: A Haskell Package for Many Theoretic Calculi, Taking Advantage of *Higher-Order Abstract Syntax*

Environment:
- ghc 9.12.2 (or adjust the `cabal.project` file)

Build / Repl:

```sh
cabal build
cabal repl
```

For Nix users, this project provides a [`shell.nix`](/shell.nix) file that prepares you with a development environment
with `ghc`, `cabal`, and `hls`. It should be out of the box. To start with `nix`, just run

```sh
nix-shell
```

and the enter the cabal repl.



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

will give a reduction list to `⟨[720]|⋆⟩`, representing that the calculation result is $6!=720$.



## (TODO) Call by Push Value (CBPV)

...

## (Maybe) Algebraic Effects and Handlers (AE&T)

...

## More Calculi to Come

...
