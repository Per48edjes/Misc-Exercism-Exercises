def square(number):
    if 1 <= number <= 64:
        return 1 << (number - 1)
    else:
        raise ValueError("square must be between 1 and 64")


def total():
    return sum(square(n) for n in range(1, 64 + 1))
