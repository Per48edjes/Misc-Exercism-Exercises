def valid_triangle_sides(func):
    def wrapper(sides):
        perimeter = sum(sides)
        return len(sides) == 3 and perimeter > 2 * max(sides) and func(sides)

    return wrapper


@valid_triangle_sides
def equilateral(sides):
    return len(set(sides)) == 1


@valid_triangle_sides
def isosceles(sides):
    return len(set(sides)) < 3


@valid_triangle_sides
def scalene(sides):
    return len(set(sides)) == 3
