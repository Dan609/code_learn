# AlexanderKrivoshein
# https://gist.github.com/AlexanderKrivoshein/b7908f2412ba1a7a21e105c7030f7d9d

# Пользователь вводит число, если оно четное - выбрасываем исключение ValueError, если оно меньше 0 - TypeError, если оно больше 10 - IndexError. Обрабатываем ошибку, 
# говорим пользователю, в чем его ошибка

user_number = float(input('Please, inmput a number: '))
try:
	if user_number % 2 == 0 and user_number > 0 and user_number < 10:
		raise ValueError('Exception: Your value is even')
	if user_number > 10:
		raise IndexError('Exception: Your value is greater then 10')
	if user_number < 0:
		raise TypeError('Exception: Your value is less then 0')
except ValueError as e:
	print(e)
except IndexError as e:
	print(e)
except TypeError as e:
	print(e)


# Создайте список произвольной длины. Пользователь должен ввести индекс объекта, который хочет посмотреть. Если все хорошо - пишите объект в консоль. 
# Если нет - обработайте возмозможные ошибки и скажите ему, что не так

user_list = [1,2,3,4,5,6,7,8,9,10]
user_index = int(input('Please, input index: '))
try:
	print('Item with index {} is {}'.format(user_index, user_list[user_index]))
except IndexError as e:
	print('{}. Your index is out of range'.format(e))

# Написать функцию, которая принимает на вход два аргумента. Если аргументы больше нуля, возвращаем их сумму. Если оба меньше - разность. Если знаки разные - возвращаем 0 

def my_custom_func(arg1,arg2):
	if arg1 > 0 and arg2 > 0:
		return arg1 + arg2
	if arg1 < 0 and arg2 < 0:
		return arg1 - arg2
	if (abs(arg1) != arg1) or (abs(arg2) != arg2):
		return 0

# print(my_custom_func(10,2))

# Написать функцию, которая принимает 3 аргумента - числа, найти среди них два максимальных, вывести в консоль

def my_custom_func2(arg1,arg2,arg3):
	my_list = list(locals().values())
	list_of_max = []
	
	while len(list_of_max) < 2:
		list_of_max.append(max(my_list))
		my_list.remove(max(my_list))

	print('List of two maximums: {}'.format(list_of_max))
	
my_custom_func2(10,2,5)

#Написать функцию, которая принимает два аргумента. Первый - список чисел, второй - булевый флаг. 
#Если флаг действителен - возвращаем новый список с нечетными числами из входного списка, если флаг отрицателен - возвращаем новый список с четными числами из входного списка

def my_custom_func3(list_of_numbers,case):
	temp_list = []
	if case:
		for number in list_of_numbers:
			if number % 2 == 0:
				temp_list.append(number)
	else:
		for number in list_of_numbers:
			if number % 2 != 0:
				temp_list.append(number)
	return temp_list

print(my_custom_func3([1,2,3,4,5],False))


# Написать функцию, которая принимает любое количество аргументов чисел. Среди них она находит максимальное и минимальное. И возвращает оба

def my_custom_func4(*args):
	temp2 = []
	temp2.append(max(args))
	temp2.append(min(args))
	return temp2

# print(my_custom_func4(1,2,3,4,5,6,7,8,9))

# Написать функцию, которая принимает два аргумента: строка и булевый флаг case по-умолчанию равный True. 
# Если флаг действителен: возвращаем новую строку, где каждый символ входной приведен к верхнему регистру, иначе - к нижнему

def my_custom_func5(string,case=True):
	if case:
		return string.upper()
	else:
		return string.lower()

print(my_custom_func5('test', False))