# TAPL
Notes and exercises from “Types and Programming Languages” by Pierce.

To run:

```
stack build
stack exec typed
```

This should fire the repl for the typed λ-calculus. You can also do

```
stack test
```

which should serve as documentation for the implentations. Right now I have
written tests only for the typed λ-calculus; `stack test` gives the following
output.

```
Typed.Parser can parse variables:
  correctly parses x
  correctly parses ayberk
Typed.Parser can parse abstractions:
  correctly parses lambda x:Bool. x
  correctly parses lambda at:Bool. at
  correctly parses lambda x:Bool -> Bool. x
  correctly parses lambda x:Bool -> Bool -> Bool. x
  correctly parses lambda x : Bool. x
  correctly parses lambda x : Bool . x
Typed.Parser can parse applications:
  correctly parses (lambda x:Bool. x) (lambda x:Bool. x)
  correctly parses f g x
  correctly parses (lambda x:Bool. x) (lambda x:Bool. x) (lambda x:Bool. x)
  correctly parses (lambda x:Bool. x) x
  correctly parses f g h x
Typed.Semantics --- removeNames:
  can handle NmAbs "x" TyBool (NmVar "x")
  can handle NmAbs "y" TyBool (NmAbs "x" TyBool (NmApp (NmVar "x") (NmVar "y")))
  can handle NmAbs "y" TyBool (NmAbs "x" TyBool (NmApp (NmVar "y") (NmVar "x")))
  can handle NmAbs "y" TyBool (NmAbs "x" TyBool (NmApp (NmVar "y") (NmVar "x")))
  can handle NmApp (NmVar "x") (NmApp (NmVar "y") (NmVar "z"))
  can handle NmAbs "w" TyBool (NmApp (NmVar "y") (NmVar "w"))
  can handle NmAbs "w" TyBool (NmAbs "a" TyBool (NmVar "x"))
  can handle NmAbs "w" TyBool (NmAbs "a" TyBool (NmVar "x"))
Type checker
  accepts TmAbs "x" TyBool (TmVar 0)
  accepts TmApp (TmAbs "x" TyBool (TmVar 0)) (TmVar 1)
  accepts TmAbs "y" TyBool (TmAbs "x" TyBool (TmApp (TmAbs "x" TyBool (TmVar 0)) (TmVar 0)))
  rejects TmIf (TmVar 0) (TmAbs "x" TyBool (TmVar 0)) (TmAbs "x" TyBool (TmVar 0))
  rejects TmAbs "x" TyBool (TmIf (TmVar 0) (TmAbs "x" TyBool (TmVar 0)) (TmVar 1))
Evaluator
  correctly evaluates TmVar 0
  correctly evaluates TmVar 0
  correctly evaluates TmApp (TmAbs "x" (TyArr TyBool TyBool) (TmVar 0)) (TmAbs "foo" TyBool (TmVar 0))
  correctly evaluates TmAbs "foo" TyBool (TmVar 0)

Finished in 0.0018 seconds
30 examples, 0 failures
```
