import math


# http://projecteuler.net/problem=3
def is_prime(n):
    for x in xrange(2, int(math.sqrt(n))):
        if n % x == 0:
            return False
    return True


def max_prime_divisor(n):
    result = None
    for x in xrange(2, int(math.sqrt(n))):
        if n % x == 0 and is_prime(x):
            result = x

    return result

EULER_ANSWER_3 = max_prime_divisor(600851475143)


# http://projecteuler.net/problem=6
EULER_ANSWER_6 = sum(range(1, 100)) ** 2 - sum(map(lambda x: x ** 2, range(1, 100)))
