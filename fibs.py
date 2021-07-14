from __future__ import annotations
from operator import add
from itertools import islice, tee

from more_itertools import iterate, take, first, nth


def fibonacci_recursive(n):
    if n == 0:
        return 0
    if n == 1:
        return 1
    return fibonacci(n - 1) + fibonacci(n - 2)


def fibonacci_iterative(n):
    current, nxt = 0, 1
    for _ in range(n):
        tmp = nxt
        nxt = current + nxt
        current = tmp
    return current
















def next_fib(pair: 'tuple[int, int]') -> 'tuple[int, int]':
    fst, snd = pair
    return (snd, fst + snd)

def fibs():
    return map(first, iterate(next_fib, (0,1)))


def fibonacci(n):
    return nth(fibs(), n)


def every_other_fibonacci_element(n):
    elements = []
    current = 0
    nxt = 1
    for i in range(n):
        current, nxt = (nxt, current + nxt)
        if i % 2 == 0:
            elements.append(current)
    return elements


def every_n(iterable, n, start=0):
    return islice(iterable, start, None, n)


def every_other_fibonacci_element_():
    return every_n(fibs(), n=2)


def tail(iterable):
    return islice(iterable, 1, None)

def fibs_():
    yield 0
    yield 1
    fibs1, fibs2 = tee(fibs_())
    yield from map(add, fibs1, tail(fibs2))
