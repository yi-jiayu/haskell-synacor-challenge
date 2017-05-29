import itertools

"""
_ + _ * _^2 + _^3 - _ = 399

- red coin          2
- corroded coin     3
- shiny coin        5
- concave coin      7
- blue coin         9

9 2 5 7 3
blue red shiny concave corroded
"""

def check(a, b, c, d, e):
    return a + b * c ** 2 + d ** 3 - e

coins = [2, 9, 5, 7, 3]

for perm in itertools.permutations(coins):
    print(check(*perm) == 399, check(*perm), perm)
