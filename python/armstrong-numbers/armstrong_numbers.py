from math import log10


def is_armstrong_number(number):
    num_digits = 0 if number <= 0 else 1 + int(log10(number))
    armstrong_sum = 0
    _number = number
    while _number > 0:
        _number, d = _number // 10, _number % 10
        armstrong_sum += d**num_digits

    return number == armstrong_sum
