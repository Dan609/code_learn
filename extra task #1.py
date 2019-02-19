# Homework Day 1 - extras
# BorisRubin

# http://codegists.com/snippet/python/hw_day1_extra1py_borisrubin_python
# Task 1 Напишите программу, которая считает площадь прямоугольника, спрашивая у пользователя длину двух сторон 
 
side_a = input('Введите длину стороны А прямоугольника: ')
side_b = input('Введите длину стороны B прямоугольника: ')
 
print('Площадь прямоугольника: ', int(side_a)*int(side_b))

# http://codegists.com/snippet/python/hw_day1_extra2py_borisrubin_python
# Task 2 Напишите программу, которая спрашивает у пользователя два числа и знак: "+" или "-". В зависимости от знака выводит их сумму или разницу 
 
def all_numbers(*numbers):
    is_numbers = True
    for num in numbers:
        if isinstance(num, float):
            is_numbers = is_numbers and True
        else:
            is_numbers = is_numbers and False
 
    return is_numbers
 
 
def operation(x, y, sign):
    if sign == '+':
        return x + y
    elif sign == '-':
        return x - y
 
num1 = float(input('Введите 1ое число : '))
num2 = float(input('Введите 2ое число : '))
 
sign = ''
while (sign != '+') and (sign != '-'):
    sign = input('Введите знак "+" или "-" : ')
 
if all_numbers(num1, num2):
    print('Результат {} {} {} = {}'.format(num1, sign, num2, operation(num1, num2, sign)))
	
# http://codegists.com/snippet/python/hw_day1_extra3-4py_borisrubin_python
# Task 3 Напишите программу, которая находит все простые числа между 0 и пользовательским числом

# введем число, заранее знаем, что оно должно быть типа int
print('Задание 1:   \nРасчитываем все простые числа от 0 до выбранного Вами числа\n')
limit = int(input('Введите число: '))
 
numbers = list()
 
# обойдем все числа от 2 до указанного числа + 1
for n in range(2, limit+1):
    # обойдем все числа от 2 до текущего числа в обходе
    for m in range(2, n):
        # если делится без остатка то число НЕ простое - прерываем внутренний цикл для перехода к след. числу
        if n % m == 0:
            break
    else:
        # в противном случае оно простое и добавляем его в список
        numbers.append(n)
 
print(numbers)
 
# http://codegists.com/snippet/python/hw_day1_extra3-4py_borisrubin_python
# Task 4 Напишите программу, которая выводит все кратные 5 числа между двумя пользовательскими числами
 
# вводим два числа типа int так как должны быть кратны 5
print('\n\nЗадание 2:   \nНайдем все числа в заданном 2 числами диапазоне кратные 5\n')
num1 = int(input('Введите 1ое число: '))
num2 = int(input('Введите 2ое число: '))
 
numbers = list(x for x in range(num1, num2) if x % 5 == 0)
print(numbers)