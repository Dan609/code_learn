# IPython log file

get_ipython().run_line_magic('logstart', '')
def my_function():
    print('I am a function')


print(my_function)
print('Functions are objects', isinstance(my_function, object))
l = [0, 'str', True]
l
l.append(my_function)
l
l[-1]
l[-1]()
c = l[-1]
c()
def call_passed_function(incoming):
    print('Calling!')
    incoming()
    print('Called!')
    
call_passed_function(my_function)
def incr(x):
    return x+1
def call_passed_function(incoming, y):
    print('Calling!')
    incoming(y)
    print('Called!')
    
    
#call_passed_function(incr(1))
#call_passed_function(incr, 1)
# You can not call uncallable things:

try:
    d = 2
    d()  # but you can try
except TypeError as e:
    print('It is not a function', e)
    
f = 5
f()
print(callable(len), callable(45), callable(callable))
def return_min_function():
    return min
test = return_min_function()
min_value = test(4, 5, -9, 12)
print('Min values is', min_value)
sum(1, 3)
def custom_sum(x, y):
    return x + y
def curry(custom_sum, x):
    def new_custom_sum(y):
        custom_sum(x, y)
        
def curry(custom_sum, x):
    def new_custom_sum(y):
        custom_sum(x, y)
    return new_custom_sum
    
custom_sum(5, 6)
custom_sum(5, 8)
sum_5 = curry(custom_sum, 5)
sum_5
sum_5(7)
def curry(custom_sum, x):
    def new_custom_sum(y):
        return custom_sum(x, y)
    return new_custom_sum
    
sum_5 = curry(custom_sum, 5)
sum_5(7)
sum_6 = curry(custom_sum, 6)
sum_6
sum_6(9)
sum_5(9)
# new_sum(x, y) -> x+y
# new_5(y) -> 5 + y
def curry(incoming_fun, x):
    def n_fun(y):
        return incoming_fun(x, y)
    return n_fun

    
curry(new_sum, 10)
curry(custom_sum, 10)
curry(custom_sum, 11)
incr_10 = curry(custom_sum, 10)
incr_20 = curry(custom_sum, 20)
incr_10
incr_20
incr_10(1)
incr_20(1)
incr_10 == incr_20
x = 5
def foo():
    x = 7
    return x
x
foo()
def scoped_function(arg):
    value = arg * 10
    print(value)

scoped_function(2)
SOME_VAR = 'value'

def print_var():
    print(SOME_VAR)

print_var()
SOME_VAR = 'value'

def modify_var():
    SOME_VAR = SOME_VAR
    try:
        SOME_VAR += '_extra'
    except UnboundLocalError as e:
        print('Error', e)

modify_var()
print(SOME_VAR)


# BUT if you really want....
def modify_var():
    global SOME_VAR
    try:
        SOME_VAR += '_extra'  # Do not do this. REALLY, JUST DO NOT THIS!!!
    except UnboundLocalError as e:
        print('Error', e)

modify_var()
print(SOME_VAR)


# BUT if you really want....
def modify_var():
    global SOME_VAR
    try:
        SOME_VAR += '_extra'  # Do not do this. REALLY, JUST DO NOT THIS!!!
    except UnboundLocalError as e:
        print('Error', e)

modify_var()
print(SOME_VAR)
GLOBAL_LIST = []

def append_to_list(item):
    print('Adding', item)
    GLOBAL_LIST.append(item)
    
append_to_list(1)
append_to_list(2)
print(GLOBAL_LIST)
Z = 5


def bar(y):  # y - bounded
    x = 1      # x - bounded

    print(Z)   # Z - free

    def foo():
      print(x + y + Z)  # x, y, z - free here

    foo()
    
x = 5
def ll()"
Z = 5


def bar(y):  # y - bounded
    x = 1      # x - bounded

    print(Z)   # Z - free

    def foo():
      print(x + y + Z)  # x, y, z - free here

    foo()
    
Z = 5


def bar(y):  # y - bounded
    x = 1      # x - bounded

    print(Z)   # Z - free

    def foo():
      print(x + y + Z)  # x, y, z - free here

    foo()
    
x
bar()
bar(2)
Z = 5


def bar(y):  # y - bounded
    x = 1      # x - bounded

    print(Z)   # Z - free

    def foo():
      x = 9
      print(x + y + Z)  # x, y, z - free here

    val = foo()
    
def outer_function(value):
    def some_inner():
        print('Value was', value)
    return some_inner

v = outer_function('some')
print('it is a function', v, callable(v))
v()
def pretty_print(arg):
    def print_stars():
        print('-' * 8)
        print('*' * 8)

    print_stars()
    print(arg)
    print_stars()

pretty_print(12)
x = 5
y = len
print(callable(x), callable(y))
globals
globals()
def foo():
    x = 11
    print(locals())
    
foo()
def foo():
    x = 11
    def bar():
        y = 12
        print(locals())
    return bar
foo()()
foo()
def foo():
    x = 11
    def bar():
        y = 12
        print(locals())
        def baz():
            z = 20
            pint("deep - deep rabbit hoooooole")
        return baz    
    return bar
foo()
foo()()
foo()()()
def foo():
    x = 11
    def bar():
        y = 12
        print(locals())
        def baz():
            z = 20
            print("deep - deep rabbit hoooooole")
        return baz    
    return bar

foo()()
foo()()()
def factorial(n):  # This function is recursive, it calls itself (but this is VERY-VERY-VERY-VERY STUPID EXAMPLE).
    if n == 0:
        return 1
    return n * factorial(n - 1)

print(factorial(4))
def factorial(n):  # This function is recursive, it calls itself (but this is VERY-VERY-VERY-VERY STUPID EXAMPLE).
    if n == 0:
        return 1
    return n * factorial(n - 1)

print(factorial(2000))

print(factorial(4000))
print(factorial(3990))
print(factorial(3900))
print(factorial(2000))
print(factorial(2500))
print(factorial(300))
print(factorial(3000))
print(factorial(2900))
print(factorial(2990))
import sys
sys.getrecursionlimit()
sys.setrecursionlimit(6000)
print(factorial(4000))
print(factorial(6000))
def double_all_elements(lst):
    """ Double all elements in list
    :param lst: incoming list
    :return: result list
    """

    if len(lst) == 0:
        return []
    else:
        updated_element = lst[0] * 2
        print(updated_element, len(lst))
        result = [updated_element, ] + double_all_elements(lst[1:])
    return result
double_all_elements([])
double_all_elements([1])
l = [1,2,3,4]
l[1:]
l = [1]
l]
l[1:]
def double_all_elements(lst, result_lst=None):
    """ Double all elements in list (tail recursion example)
    :param lst: incoming list
    :return: result list
    """

    if result_lst == None:
        result_lst = []

    if len(lst) == 0:
        return []
    else:
        updated_element = lst[0] * 2
        print(updated_element, len(lst), result_lst)
        result_lst.append(updated_element)
        result = double_all_elements(lst[1:], result_lst)
    return result
l = [1,2,3,4]
double_all_elements(l)
double_all_elements([])
double_all_elements([1])

def double_all_elements(lst, result_lst=None):
    """ Double all elements in list (tail recursion example)
    :param lst: incoming list
    :return: result list
    """

    if result_lst == None:
        result_lst = []

    if len(lst) == 0:
        return []
    else:
        updated_element = lst[0] * 2
        result_lst.append(updated_element)
        print(updated_element, len(lst), result_lst)
        result = double_all_elements(lst[1:], result_lst)
        print(result)
    return result
double_all_elements([1])
double_all_elements([1])
def double_all_elements(lst, result_lst=None):
    """ Double all elements in list (tail recursion example)
    :param lst: incoming list
    :return: result list
    """

    if result_lst == None:
        result_lst = []

    if len(lst) == 0:
        return result_lst
    else:
        updated_element = lst[0] * 2
        result_lst.append(updated_element)
        print(updated_element, len(lst), result_lst)
        result = double_all_elements(lst[1:], result_lst)
        print(result)
    return result
double_all_elements([1])
double_all_elements([])
double_all_elements([1,2,3])
double_all_elements([])
double_all_elements([1, ])
def double_all_elements(lst, result_lst=None):
    """ Double all elements in list (tail recursion example)
    :param lst: incoming list
    :return: result list
    """

    if result_lst == None:
        result_lst = []

    if len(lst) == 0:
        return result_lst
    else:
        updated_element = lst[0] * 2
        result_lst.append(updated_element)
        print(updated_element, len(lst), result_lst)
        result = double_all_elements(lst[1:], result_lst)
        print(result)
    return result
double_all_elements([1, 2])
def double_all_elements(lst, result_lst=None):
    """ Double all elements in list (tail recursion example)
    :param lst: incoming list
    :return: result list
    """

    if result_lst == None:
        result_lst = []

    if len(lst) == 0:
        return result_lst
    else:
        updated_element = lst[0] * 2
        result_lst.append(updated_element)
        print(updated_element, lst, result_lst)
        result = double_all_elements(lst[1:], result_lst)
    return result

double_all_elements([1, 2])
double_all_elements([1, 2], [])
def double_all_elements(lst, result_lst=None):
    """ Double all elements in list (tail recursion example)
    :param lst: incoming list
    :return: result list
    """

    if result_lst == None:
        result_lst = []

    if len(lst) == 0:
        print(lst, result_lst)
        return result_lst
    else:
        updated_element = lst[0] * 2
        result_lst.append(updated_element)
        print(updated_element, lst, result_lst)
        result = double_all_elements(lst[1:], result_lst)
    return result

double_all_elements([1, 2])
s, k = (1, 2)
s, k
s
def foo():
    return 1, 2
def bar():
    return (1, 2)

s, k = foo()
s
k
s, k = bar()
k
first, *second = 'My string'
second
first, *mid, last = 'some letters'
print(first, mid, last)
tup = bar()
s = tup[0]
s
k = tup[1]
def accepts_args(*args):
    print(args)
    return sum(args)
l = [1, 2, 3, 4]
try:
    accepts_args(l)
except TypeError as e:
    print('Error', e)

accepts_args(*l)
def accepts_args(*kwargs):
    print(kwargs)
    
def accepts_args(**kwargs):
    print(kwargs)
    

values = {'day': 'wed', 'month': 'may'}
accepts_values(**values)
accepts_args(**values)
list(range(2, 10))
list(range(10))
list(range(0, 10, 3))
list(range(0, 10, 2))
if True:
    print(x)
    
if not False:
    print(x)
    
k, v = 1
'' == 0
''.tolower()
''.lower()
'FDGDFGDFG'.lower()
