# Boris Rubin
# https://gist.github.com/BorisRubin

# Задачи на обработку ошибок:

# Пользователь вводит число,
# если оно четное - выбрасываем исключение ValueError,
# если оно меньше 0 - TypeError,
# если оно больше 10 - IndexError.
# Обрабатываем ошибку, говорим пользователю, в чем его ошибка

print('День 3. Обработка ошибок. Задача 1.\n')

def get_number(i):
    if i % 2 == 0:
        raise ValueError
    elif i < 0:
        raise TypeError
    elif i > 10:
        raise IndexError

for i in [-1,2,11]:
    try:
        get_number(i)
    except ValueError as e:
        print('Возникла ошибка значения', e)
    except TypeError as e:
        print('Возникла ошибка типа', e)
    except IndexError as e:
        print('Возникла ошибка индекса', e)


# Создайте список произвольной длины.
# Пользователь должен ввести индекс объекта, который хочет посмотреть.
# Если все хорошо - пишите объект в консоль.
# Если нет - обработайте возмозможные ошибки и скажите ему, что не так
print('\n\nДень 3. Обработка ошибок. Задача 2.\n')
lst = ['as', 1, -1, 'русский', None]

exitcode = False
while not exitcode:
    try:
        index = int(input('Введите индекс списка (999 для выхода): '))
        if index == 999:
            print('...выходим...')
            break
        print(lst[index])
    except ValueError as e:
        print('Вы ввели не число: {} \n'.format(e))
    except IndexError as e:
        print('Элемента с таким индексом не существует: {} \n'.format(e))
		
	
	
# Задачи на закрепление функций:

# Написать функцию, которая принимает на вход два аргумента.
# Если аргументы больше нуля, возвращаем их сумму.
# Если оба меньше - разность.
# Если знаки разные - возвращаем 0
print('Домашнее задание. День 3. Закрепление функций. Задание 1\n')

def some_func(a, b):
    if (a > 0) and (b > 0):
        return a + b
    elif (a < 0) and (b < 0):
        return a - b
    elif a * b <= 0:
        return 0

print(some_func(1, 1))
print(some_func(-3, -1))
print(some_func(-3, 1))


# Написать функцию, которая принимает 3 аргумента - числа,
# найти среди них два максимальных, вывести в консоль
print('\n\nДомашнее задание. День 3. Закрепление функций. Задание 2\n')

def some_func2(arg1, arg2, arg3):
    lst = [arg1, arg2, arg3]
    lst.sort()
    print(lst[1], lst[2])

some_func2(12, 1, 2121)

# Написать функцию, которая принимает два аргумента.
# Первый - список чисел, второй - булевый флаг.
# Если флаг действителен - возвращаем новый список с нечетными числами из входного списка,
# если флаг отрицателен - возвращаем новый список с четными числами из входного списка
print('\n\nДомашнее задание. День 3. Закрепление функций. Задание 3\n')
def some_func3(flag,*numbers):

    odds = []
    evens = []

    for num in numbers:
        if num == 0:
            continue
        if num % 2 == 0:
            evens.append(num)
        else:
            odds.append(num)

    if flag:
        return odds
    else:
        return evens

print(some_func3(True, *[1,2,3,4,5,6,7,8,9,10,121213,145,51,0]))
print(some_func3(False, *[1,2,3,4,5,6,7,8,9,10,121213,145,51,0]))



# Задачи на закрепление типов аргументов:

# Написать функцию, которая принимает любое количество аргументов чисел.
# Среди них она находит максимальное и минимальное. И возвращает оба
print('Домашнее задание. День 3. Типы аргументов. Задание 1\n')
def extremum_func(*numbers):
    lst = [i for i in numbers]
    lst.sort()
    print('Min', lst[0], '\nMax', lst[len(lst)-1])

extremum_func(1,2,3,4,5,6,7,8,9,0,-12,-1)


# Написать функцию, которая принимает два аргумента: строка и булевый флаг case по-умолчанию равный True.
# Если флаг действителен: возвращаем новую строку, где каждый символ входной приведен к верхнему регистру, иначе - к нижнему
print('\n\nДомашнее задание. День 3. Типы аргументов. Задание 2\n')
def case_switcher(string, case):
    if isinstance(string, str):
        if case:
            return string.upper()
        else:
            return string.lower()

print(case_switcher('I\'m your father Luke!',True))


# Написать функцию, которая принимает любое количество позиционных аргументов - строк
# и один парматер по-умолчанию glue, который равен ':'.
# Соединить все строки таким образом, чтобы в результат попали все строки, длинее 3 символов.
# Для соединения между любых двух строк вставлять glue
print('\n\nДомашнее задание. День 3. Типы аргументов. Задание 3\n')
def smart_concat(*text, glue=':'):
    result_string = ''
    for s in text:
        if len(s) > 3:
            result_string += glue + s

    return result_string

print(smart_concat('abc','metallica','may','unforgiven 2'))