# mariamyzz
# https://gist.github.com/mariamyzz/33cb851ece73edcfc5340271118fe4d0
# decorators.py

from functools import wraps
from time import time

def cancelled(func):
    """Cancels the execution of a function"""
    @wraps(func)
    def wrapped(*args, **kwargs):
        print(func.__name__, 'is cancelled!')
    return wrapped


def count_execution_time(func):
    @wraps(func)
    def wrapped(*args, **kwargs):
        start_time = time()
        func(*args, *kwargs)
        end_time = time()
        print('This function took {} seconds to execute.'
              .format(end_time - start_time))
    return wrapped


class Counter(object):
    counts = {}

    @staticmethod
    def count(func):
        """Counts time the func is executed"""
        def wrapped(*args, **kwargs):
            if func.__name__ in Counter.counts.keys():
                Counter.counts[func.__name__] += 1
                print(Counter.counts[func.__name__])
            else:
                Counter.counts[func.__name__] = 1
                print(Counter.counts[func.__name__])
            return func(*args, **kwargs)
        return wrapped


def log(func):
    print('The decorator function is invoked')

    @wraps(func)
    def wrapped(*args, **kwargs):
        print('The initial function will execute now.')
        func(*args, **kwargs)
        print('The initial function was executed.')
    print('Finished decorating the initial function.')
    return wrapped


def exception_wrapper(func):
    @wraps(func)
    def wrapped(*args, **kwargs):
        try:
            func(*args, **kwargs)
        except Exception as err:
            print('Exception occured: {}'.format(err))
    return wrapped
	
# mariamyzz
# https://gist.github.com/mariamyzz/33cb851ece73edcfc5340271118fe4d0
# map-filter-reduce.py

# При помощи map посчитать остаток от деление на 5 для чисел: [1, 4, 5, 30, 99]
list(map(lambda x: x % 5, [1, 4, 5, 30, 99]))


# При помощи map превратить все числа из массива [3, 4, 90, -2] в строки
list(map(str, [3, 4, 90, -2]))


# При помощи filter убрать из массива ['some', 1, 'v', 40, '3a', str] все строки
list(filter(lambda x: not isinstance(x, str), ['some', 1, 'v', 40, '3a', str]))


# При помощи reduce посчитать количество букв в словах: ['some', 'other', 'value']
from functools import reduce
reduce(lambda x, y: x + y, list(map(lambda x: len(x), ['some', 'other', 'value'])))
