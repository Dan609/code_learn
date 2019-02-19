# IPython log file

get_ipython().run_line_magic('logstart', '')
s = []
for i in 'ser':
    s.append(i)
    
s
s = [i for i in 'asdfgh']
s
s = [w for w in range(1,14)]
s
s = [w for w in range(1,14) if w % 2 == 0]
s
s = [w for w in range(1,14) if w % 2 != 0]
s
words = ['The', 'quick', 'brown', 'fox', 'jumps', 'over', 'the', 'lazy', 'dog']
stuff = [[w.upper(), w.lower(), len(w)] for w in words]
stuff
l = [j for v in range(2, 5) for i in range(2, v) for j in range(i*2, 10, i)]
l
country = ['India', 'Pakistan', 'Nepal', 'Bhutan', 'China', 'Bangladesh']
capital = ['New Delhi', 'Islamabad','Kathmandu', 'Thimphu', 'Beijing', 'Dhaka']
print({ country[i]: capital[i] for i in range(len(country)) })
def x():
    print('Hey')
    
p = lambda: print('Hey')
p()
ten = lambda x: 10
print(ten())
ten = lambda x: 10
print(ten(7))

n = lambda x, y: x + y
print(n(3, 4))
l = lambda *args: print(args)
l(list(range(1, 10)))
kw_lambda = lambda **kwargs: print(kwargs)
kw_lambda(**{'a': 1, 'b': 2})
def work(value):
    return value * 2
t = [1, 2, 10]
m = map(work, t)
m
list(m)
list(map(lambda x: x * 2, t))
list(map(lambda x: x * 3, t))
list(filter(lambda v: v > 0, [-1, -5, -9, 20, 3, 0]))
list(filter(lambda v: v > 3, [-1, -5, -9, 20, 3, 0]))
r = [1, 4, 2, 3]
r = [1, 2, 3, 4]
result = reduce(lambda x, y: x + y, r)
from functools import reduse
from functools import reduce
result = reduce(lambda x, y: x + y, r)
result
reduce(lambda x, y: x + y, [1, 2])
reduce(lambda x, y: x + y, [1, 2, 3])
reduce(lambda x, y: x + y, [1, 2, 3, 4])
reduce(lambda x, y: x + y, [1, 2, 3, 4], 9)
reduce(lambda x, y: x + y, [1, 2, 3, 4], -1)
reduce(lambda x, y: x + y, [1,], 0)
reduce(lambda x, y: x + y, [1,])
def logger(function):
    def inner(x, y):
        result = function(x, y)
        print('Result is', result)
        return result
    return inner

def sum(x, y):
    return x + y
s = logger(sum)
sum(9, 12)
s(9, 12)
def mult(x, y):
    return x * y
m = logger(mult)
m(2, 5)
def action_decorator(func):
    def inner(text):
        print('Someone is going to', func.__name__)
        func(text)

    return inner
@action_decorator  # try to uncomment me
def shout(text):
    print(text.upper(), '!!!!')
    
shout()
shout('HHH')
#action_decorator(shout)

def action_decorator(func):
    def inner(text):
        print('Someone is going to', func.__name__)
        func(text)

    return inner

def shout(text):
    print(text.upper(), '!!!!')
    
action_decorator(shout)('TEXT')
new_funciton = action_decorator(shout)
new_function('TEXT')
new_funciton('TEXT')
shout = action_decorator(shout)
shout('AAA')

def shout(text):
    print(text.upper(), '!!!!')
    
@action_decorator  # shout = action_decorator(shout)
def shout(text):
    print(text.upper(), '!!!!')
    
shout('JJJ')
@action_decorator  # try to uncomment me
def shout(text):
    print(text.upper(), '!!!!')
    

@action_decorator
def whisper(text):
    print(text.lower(), '...')
    
whisper('HHHH')
@action_decorator
def say(something):
    something += '; was said.'
    print(something)
    
say('Hello')
def endless_function():
    print('I am endless')

    # Functions are objects!
    # Function can return a function.
    return endless_function


endless_function()()()()()()()()()()()()

x = endless_function()
print(x, x == endless_function)
import time
time.time()
start = time.time()
time.time() - start()
time.time() - start
import time
time.time()
def switch(enabled=True):
    def decor(func):
        def wrapper(*args, **kwargs):
            if enabled is True:
                print('Function {} enabled'.format(func.__name__))
                return func(*args, **kwargs)
            else:
                print('Function {} disabled'.format(func.__name__))
                return None
        return wrapper
    return decor
@switch()
def some_logic(x, y):
    return x + y
def switch(enabled=True):
    print(1)
    def decor(func):
        print(2)
        def wrapper(*args, **kwargs):
            print(3)
            if enabled is True:
                print('Function {} enabled'.format(func.__name__))
                return func(*args, **kwargs)
            else:
                print('Function {} disabled'.format(func.__name__))
                return None
        return wrapper
    return decor
@switch()
def some_logic(x, y):
    return x + y
some_logic(1,2)
some_logic(1,4)
@switch(enabled=False)
def some_logic(x, y):
    return x + y
@switch(enabled=False)
def another_logic(x, y):
    return x + y

another_logic(3,4)
def my_decorator(f):
    def wrapper(*args, **kwds):
        print('Calling decorated function')
        return f(*args, **kwds)
    return wrapper


def better_decorator(f):
    @wraps(f)
    def wrapper(*args, **kwds):
        print('Calling decorated function')
        return f(*args, **kwds)
    return wrapper

from functools import wraps
def my_decorator(f):
    def wrapper(*args, **kwds):
        print('Calling decorated function')
        return f(*args, **kwds)
    return wrapper


def better_decorator(f):
    @wraps(f)
    def wrapper(*args, **kwds):
        print('Calling decorated function')
        return f(*args, **kwds)
    return wrapper
@my_decorator
def example():
    """Docstring"""
    print('Called example function')


@better_decorator
def example_2():
    """Docstring"""
    print('Called example function')
    
print('example name', example.__name__)
print('example doc', example.__doc__)
print('example_2 name', example_2.__name__)
print('example_2 doc', example_2.__doc__)
@my_decorator
def example():
    """Docstring"""
    print('Called example function')


@timer
@better_decorator
def example_2():
    """Docstring"""
    print('Called example function')
    
def time_this(original_function):
    def new_function(*args,**kwargs):
        import datetime
        before = datetime.datetime.now()
        x = original_function(*args,**kwargs)
        after = datetime.datetime.now()
        print('Elapsed Time = {0}'.format(after-before))
        return x
    return new_function
class ImportantStuff:
    @time_this
    def do_stuff_1(self):
        pass

    @time_this
    def do_stuff_2(self):
        pass

    @time_this
    def do_stuff_3(self):
        pass
    
