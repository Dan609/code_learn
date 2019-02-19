# agavrish
# https://gist.github.com/agavrish/edde1a0c4d219035b3a4f26d4e4c3583

# Задачи на обработку ошибок:
# 1.Пользователь вводит число, если оно четное - выбрасываем исключение ValueError, если оно меньше 0 - TypeError, если оно больше 10 - IndexError.
# Обрабатываем ошибку, говорим пользователю, в чем его ошибка

vvod = input('Введите любое число:')
n = int(vvod)
try:
    if n < 0:
        raise TypeError ('Число меньше нуля')
    if n > 10:
        raise IndexError ('Число больше десяти')
    if n % 2 == 0:
        raise ValueError ('Число четное')
except TypeError :
    print('Число меньше нуля')
except IndexError :
    print('Число больше десяти')
except ValueError :
    print('Число четное')


# 2. Создайте список произвольной длины. Пользователь должен ввести индекс объекта, который хочет посмотреть.
#  Если все хорошо - пишите объект в консоль. Если нет - обработайте возмозможные ошибки и скажите ему, что не так

list = ['qwr', '2345', 'rt']
try:
    vvod = input('Enter index:')
    n = int(vvod)
    if n >= 0:
        if n >= len(list):
            raise IndexError('Не верный индекс')
    print(list[n])
except IndexError:
    print('Не верный индекс!')
except ValueError:
    print('Введено не корректное значение')

# Задачи на закрепление функций:
# 1.Написать функцию, которая принимает на вход два аргумента. Если аргументы больше нуля, возвращаем их сумму.
# Если оба меньше - разность. Если знаки разные - возвращаем 0

def function(x,y):
    if x > 0 and y > 0:
        return x + y
    if x < 0 and y < 0:
        return x - y
    #if x < 0 and y > 0:
    return 0
   # if x > 0 and y < 0:
    #    return 0
print(function(7,-8))

# 2.Написать функцию, которая принимает 3 аргумента - числа, найти среди них два максимальных, вывести в консоль

def function(x,y,z):
    lst = [x,y,z]
    lst.sort()
    print(lst[1],lst[2])
function(8,5,10)

# 3.Написать функцию, которая принимает два аргумента. Первый - список чисел, второй - булевый флаг. Если флаг действителен -
# возвращаем новый список с нечетными числами из входного списка, если флаг отрицателен - возвращаем новый список с четными числами из входного списка

def function(*numbers,flag):
    if flag:
        return list_num1
        print(function)
    else:
        return list_num2
    print(function)


def function1(*numbers,flag):
    list_num1 = []
    if flag==False:
        for number in numbers:
            if number % 2 == 0:
                list_num1.append(number)
    return list_num1

def function2(*numbers,flag):
    list_num2 = []
    if flag==True:
        for number1 in numbers:
            if number1 % 2 != 0:
                list_num2.append(number1)
    return list_num2

# Задачи на закрепление типов аргументов:
# 1.Написать функцию, которая принимает любое количество аргументов чисел. Среди них она находит максимальное и минимальное. И возвращает оба

def function(*number):
    list = []
    list.append(max(number))
    list.append(min(number))
    return list
print(function(1,2,3,4,10))

# 2.Написать функцию, которая принимает два аргумента: строка и булевый флаг case по-умолчанию равный True.
# Если флаг действителен: возвращаем новую строку, где каждый символ входной приведен к верхнему регистру, иначе - к нижнему

def fun(string, case=True):
    if case:
        return string.upper()
    else:
        return string.lower()
print(fun('Hello', True))

# 3.Написать функцию, которая принимает любое количество позиционных аргументов - строк и один парматер по-умолчанию glue, который равен ':'.
# Соединить все строки таким образом, чтобы в результат попали все строки, длинее 3 символов. Для соединения между любых двух строк вставлять glue

def fun(*strings, glue=':'): #b
    list = []
    for string in strings: #begin
        if len(string) > 3: #b
            list.append(string)
        #end
    #e
    return glue.join(list)

 #end
print(fun('hello','www','re','good'))