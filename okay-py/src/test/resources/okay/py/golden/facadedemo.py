"""A module for the facade's golden test (foreign-module-trait)."""
from typing import Optional


def mean(xs: list[float]) -> float:
    """The arithmetic mean."""
    return sum(xs) / len(xs)


def label(n: int, prefix: str = "item") -> str:
    """A labelled number."""
    return "%s-%d" % (prefix, n)


def first(xs: list[int]) -> Optional[int]:
    return xs[0] if xs else None


def echo(value):
    return value


def _private(x):
    return x
