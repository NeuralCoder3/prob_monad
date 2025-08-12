from collections import defaultdict
from typing import Callable, Dict, TypeVar, Union
import operator

Self = TypeVar('Self', bound='Distribution')

class Distribution:
    def __init__(self, p: Dict[Union[int, float], float] = None):
        if p is None:
            p = {}
        self.p = defaultdict(lambda: 0.0, p)
        self.normalize()
        
    def normalize(self) -> None:
        for k in list(self.p.keys()):
            if self.p[k] < 0:
                raise ValueError(f"Negative probability for key {k}: {self.p[k]}")
        
        total = sum(self.p.values())
        if total > 0:
            for k in self.p:
                self.p[k] /= total
        else:
            self.p = defaultdict(lambda: 0.0)
        
    def convolve(self, other: Self, op: Callable[[Union[int, float], Union[int, float]], Union[int, float]]) -> Self:
        p = defaultdict(lambda: 0.0)
        for v1, p1 in self.p.items():
            for v2, p2 in other.p.items():
                p[op(v1, v2)] += p1 * p2
        return Distribution(dict(p))
    
    def map(self, op: Callable[[Union[int, float]], Union[int, float]]) -> Self:
        q = defaultdict(lambda: 0.0)
        for v, p in self.p.items():
            q[op(v)] += p
        return Distribution(dict(q))

    def _dunder_impl(self, other, op):
        if isinstance(other, Distribution):
            return self.convolve(other, op)
        else:
            return self.map(lambda k: op(k, other))
        
    def ensure(self):
        if not self.p:
            raise ValueError("Distribution is empty, cannot perform operations.")
        if any(v < 0 for v in self.p.values()):
            raise ValueError("Distribution contains negative probabilities.")
        if sum(self.p.values()) != 1.0:
            raise ValueError("Distribution does not sum to 1.0.")
    
    def __str__(self) -> str:
        dict_str = ', '.join(f"{k}: {v:.2f}" for k, v in self.p.items())
        return f"Distribution({dict_str})"
    
    def __repr__(self) -> str:
        return f"Distribution({dict(self.p)})"
# Dynamically create all the dunder methods
for name, op in [
    ('__add__', operator.add),
    ('__sub__', operator.sub),
    ('__mul__', operator.mul),
    ('__truediv__', lambda a, b: a / b if b != 0 else float('inf')),
    ('__lt__', operator.lt),
    ('__le__', operator.le),
    ('__gt__', operator.gt),
    ('__ge__', operator.ge),
    ('__eq__', operator.eq),
    ('__ne__', operator.ne),
]:
    setattr(Distribution, name, lambda self, other, op=op: self._dunder_impl(other, op))


# Standard dice distributions
# Coin = Distribution({0: 0.5, 1: 0.5})  # Fair coin
Coin = Distribution({"Heads": 0.5, "Tails": 0.5})
D4 = Distribution({i: 1.0 for i in range(1, 5)})
D6 = Distribution({i: 1.0 for i in range(1, 7)})
D8 = Distribution({i: 1.0 for i in range(1, 9)})
D10 = Distribution({i: 1.0 for i in range(1, 11)})
D12 = Distribution({i: 1.0 for i in range(1, 13)})
D20 = Distribution({i: 1.0 for i in range(1, 21)})

def main():
    print("D6 (6-sided die):", D6)
    print("D6 > 3:", D6 > 3)
    print("D6 + D6 (sum of two dice):", D6 + D6)
    print("D6 * 2 (double the die roll result):", D6 * 2)


if __name__ == "__main__":
    main()
    
# TODO: Paar Events (monade?)