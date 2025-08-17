# Probability Computation

One often wants to compute the probability of an event given some conditions.
- How likely are two D4 dices in sum larger than a D6?
- Roll four dices what the probability of getting at least two sixes?
- Who is more likely to win in Risk
- Monty Hall problem
- ...

There are many ways to compute these probabilities.
- Purely symbolical (often manually). But this is not feasible for complex problems and even for smaller problem it is error-prone and tedious.
- Monte Carlo simulation. This is often used in practice, but it is not exact and can be slow (especially if one wants to make sure it converges to the correct probability).
- Computing the probability by keeping track of each event and its probability combining them using the laws of probability. 

We will focus on the last approach.
In its basic form, this approach requires knowledge about the laws of probability and causes quite some boilerplate code.
We abstract this boilerplate code away using a monad.
Furthermore, the monad allows us to naturally construct and operate with events in a natural way. No knowledge about the laws of probability is required.

We build on top of previous blog posts and papers in this area:
- Code in Haskell: https://github.com/dennybritz/probability-monads/blob/main/src/ProbabilityMonads.hs
- Blog post: https://dennybritz.com/posts/probability-monads-from-scratch/
- Functional pearl "Probabilistic Functional Programming in Haskell": https://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf


## Limits

The Python code uses the conceptually same idea as the OCaml code.
However, it only includes the operator notation not the monadic notation.

Infinite/unbounded events and non-discrete distributions are not supported.
E.g. it is not easily possible to model something like "the expected amount of rools until you roll five sixes with more than 50% probability" or
"the probability of the length of a run until five heads a rolled in a row to be even".
One can fix these by adding a bound or extending the model to a tree like structure accumulating the probabilities until a threshold is reached.

Furthermore, the code can quickly become slow for complex problems.
We compute (more or less) all possible events and their probabilities.
By applying `dedup` eagerly in the bind, some operations become cheaper
as we combine the events and compute the probabilities in a DP-like fashion.
However, this does not help for cases where events are only grouped at the end or can not be grouped at all.



