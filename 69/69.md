First attempted using the coprime generation ternary trees describe on the
Wikipedia entry. Unfortunately the number of tree branches (i.e. coprime pairs) is way too large.

Looking further into the problem we can reduce the search space drastically.
We want to maximize n/φ(n). One form is φ(n) = n * product(1 - 1/p, forall primes dividing n).
Thus we want to minimize product(1 - 1/p, forall p|n).
When is that minimal? When n has the most distinct prime factors.

And the first n < 10^6 for which has the greatest number of distinct prime factors is the product of the first 8 primes ([OEIS A002110](https://oeis.org/A002110)).

Interesting Mathematica tables to look at:
- `TableForm[Table[{n,StringRiffle[ResourceFunction["CoprimeIntegerList"][n],","]},{n,1,30}]]`
- `TableForm[Table[{n,Length[ResourceFunction["CoprimeIntegerList"][n]], PrimePi[n]/n},{n,1,300}]]`