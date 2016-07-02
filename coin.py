import itertools

def check(a, b, c, d, e):
    return a + b * c ** 2 + d ** 3 - e

coins = [2, 9, 5, 7, 3]

for perm in itertools.permutations(coins):
    print(check(*perm) == 399, check(*perm), perm)
