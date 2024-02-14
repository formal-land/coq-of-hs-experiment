# coq-of-hs-experiment

Experiment on translation of Haskell code to Coq&nbsp;🐓

> This project is experimental. We put it there as it might be of interest for others.

## 🚀 Run

Run the project using [Cabal](https://www.haskell.org/cabal/) with:

```sh
cabal run
```

It will translate the example file [test/Main.hs](test/Main.hs) into [coq_translation/test/Main.v](coq_translation/test/Main.v). For example, the Haskell function:

```haskell
fixObvious :: (a -> a) -> a
fixObvious f = f (fixObvious f)
```

is translated to the Coq code:

```coq
CoFixpoint fixObvious : Val.t :=
  (Val.Lam (fun (f : Val.t) => (Val.App f (Val.App fixObvious f)))).
```

## 🎯 Goal

The goal of this project is to formally verify Haskell programs.

Formal verification is using mathematical reasoning over a program to show that it is correct for all possible inputs. This contrasts with:

- Testing, which only shows that a program is correct for a limited set of inputs.
- Typing, which only verify a limited set of properties. For example, Haskell's type system cannot verify that the total amount of money stays constant for internal transactions, if one implements a bank system.

## ❓ What

The tool `coq-of-hs` translates Haskell code to [Coq](https://coq.inria.fr/), a proof system. Once the code is translated to this proof system, one can use Coq's language to specify and prove properties about the code.

## 🔍 How

We translate Haskell code by reading the intermediate language Haskell Core of the [GHC](https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler) compiler. This intermediate language is a small and simple, so we hope this is an ideal target to translate Haskell code to Coq. The blog post [Haskell to Core: Understanding Haskell Features Through Their Desugaring](https://serokell.io/blog/haskell-to-core) gives a nice introduction to Haskell Core.

To read Haskell Core representation, we use the plugin system of GHC. This allows us to see the code how GHC sees it, with all the compilation options that a user may have activated for a project. See this Stack Overflow's question [How to test GHC plugins?](https://stackoverflow.com/questions/55878912/how-to-test-ghc-plugins) for a nice introduction to GHC plugins.

We translate everything to an untyped version of the λ-calculus. We remove all the type annotations and only keep the values. Everything is of type `Val.t`, a co-inductive type (for potentially infinite lazy terms) defined as:

```coq
Module Val.
  #[bypass_check(positivity)]
  CoInductive t : Set :=
  | Lit (_ : Lit.t)
  | Con (_ : string) (_ : list t)
  | App (_ _ : t)
  | Lam (_ : t -> t)
  | Case (_ : t) (_ : t -> list (Case.t t))
  | Impossible.
End Val.
```

These constructors correspond to the ones that are in the Haskell Core language. Note that we remove the constructors for the type annotations. We also add an `Impossible` constructor to represent impossible branches in the `Case` constructor. We might add later a special constructor for the exceptions, such as division by zero.

We disable the positivity checks `#[bypass_check(positivity)]` for the `Lam` constructor, because it takes a function with a parameter of type `t`. This is forbidden in Coq as it can lead to non-termination, but we still disable it here for convenience. We do not know if this is an issue in our case, as the programs that we consider are well-typed according to the type-checker of Haskell.

We have not yet defined a semantics for these terms. It should be interesting to see how to reason about these potentially non-terminating terms.

## 🎁 Contribute

You can open a pull request or create an issue. The code is under AGPL license.

## 🌍 Related

This project is only an experiment. There are more complete projects:

- [🐓 hs-to-coq](https://github.com/plclub/hs-to-coq) another project to translate Haskell code to Coq. This translation is more hight-level: for example it keeps the types.
- [💧 Liquid Haskell](https://en.wikipedia.org/wiki/Liquid_Haskell) using [SMT solvers](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories) to verify Haskell programs.
