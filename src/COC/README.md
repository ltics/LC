`x : A` is a judgement that we can say x has a type of A or x is a proof of A

```
x : A ∈ Γ
—————————— (var)
Γ ⊢ x : A
```

```
—————————— (type)
Γ ⊢ Type.{n} : Type.{n+1}
```

```
Γ, x : A ⊢ b : B   Γ ⊢ A : Type
———————————————————————————————— (abs)
Γ ⊢ λ x : A . b : ∀ (x : A) . B
```

example of derivation

```
———————————————————————————————— (var)

y : Type, x : y ⊢ x : y  ———————————————————— (var)
                          y : Type ⊢ y : Type
—————————————————————————————————————————————— (abs)

y : Type ⊢ λ x : y . x : ∀ (x : y) . y  ————————————————— (type)
                                         ∅ ⊢ Type : Type1
—————————————————————————————————————————————————————————— (abs)
∅ ⊢ λ y : Type λ x : y . x : ∀ (y : Type) (x : y) . y
```

and [here](https://github.com/zjhmale/LC/blob/master/src/COC/lib/misc.coc#L12-L13) is a proof for the derivation

```
Γ ⊢ a : ∀ (x : A) . B
Γ ⊢ b : A
—————————————————————— (app)
Γ ⊢ a b : [x → b] B
```

the substitution of app-rule is [here](https://github.com/zjhmale/LC/blob/master/src/COC/COC.hs#L126)
