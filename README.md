# Polynomial Arithmetic in Prolog

A Prolog program that works with polynomials: print, read, evaluate, differentiate, simplify, add, subtract and multiply them. It also defines its own operators so you can write a whole expression in one line.

A polynomial is a list of `[coefficient, power]` pairs. For example `[[3,2],[-1,1],[5,0]]` is 3x² − x + 5.

## Predicates

| Predicate | What it does |
| --- | --- |
| `parcourireAffiche(P)` | Print a polynomial in x^n form |
| `lireListe(P)` | Read a polynomial from the keyboard, term by term |
| `evalu(X, P, R)` | Evaluate `P` at `x = X` |
| `drive(P, D)` | Derivative of `P` |
| `smplifier(P, S)` | Combine terms with the same power |
| `somme(P1, P2, S)` | Sum |
| `sustraction(P1, P2, S)` | Difference |
| `produit(P1, P2, S)` | Product |

## Expression operators

The second part of the file defines the operators `est`, `+`, `-`, `*`, `simp` (simplify) and `driv` (derive), so you can combine operations:

```prolog
?- P est simp driv [[3,7],[4,8]] + [[3,8]] - [[1,2]] * [[4,6]].
P = [[21, 6], [24, 7]].
```

## Examples

```prolog
?- evalu(1, [[-3,1],[0,9],[0.4,1],[16,1]], R).
R = 13.399999999999999.

?- drive([[4,8],[0,7],[-3,8],[2,3]], D).
D = [[32, 7], [0, 6], [-24, 7], [6, 2]].

?- smplifier([[4,7],[-3,7],[6,3],[2,2],[4,2]], S).
S = [[1, 7], [6, 3], [6, 2]].
```

## Run it

You need [SWI-Prolog](https://www.swi-prolog.org/).

```bash
git clone https://github.com/hamzakhalem/Prolog-polyomial-arithmitics.git
cd Prolog-polyomial-arithmitics
swipl polynoOPP.pl
```

Then type the queries above at the `?-` prompt.
