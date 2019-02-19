# chenikita
# https://github.com/chenikita/4course_Homework

# 1. Создать лист из 6 любых чисел. Отсортировать его по возрастанию
l = [1, 6, 12, 5.6, 13, 90]
l.sort()
print(l)

# 3. Создать tuple из 10 любых дробных чисел, найти максимальное и минимальное значение в нем
T = (2.4, 3.7, 4.7, 6.2, 9.3, 10.34, 34.98, 2324.23536, 1.33, 34.19)
print(max(T))
print(min(T))

# 4. Создать лист из 3 слов: ['Earth', 'Russia', 'Moscow'], 
# соеденить все слова в единую строку, чтобы получилось: 'Earth -> Russia -> Moscow'

l = ['Earth', 'Russia', 'Moscow']

a = str(l[0] + ' -> ' + l[1] + ' -> ' + l[2])
print(a)

# 5. Взять строку '/bin:/usr/bin:/usr/local/bin' и разбить ее в список по символу ':'
a = '/bin:/usr/bin:/usr/local/bin'
print(a.split(':'))

# 6. Пройти по всем числам от 1 до 100, написать в консоль, какие из них делятся на 7, 
# а какие - нет

for i in range(1, 100):
	if i % 7 == 0:
		print('and {} is!'.format(i))
	else:
		print('{} is not multiple of 7'.format(i))

# 7. Создать матрицу любых чисел 3 на 4, сначала вывести все строки, потом все столбцы
matrix = [
    [2, 25, 8.9],
    [4, 19, 6],
    [4, 8, 0],
    [10, 11, 136],
]
a = []
b = []
c = []
for row in matrix:
    print(row)
for col in matrix:
    a.append(col[0])
    b.append(col[1])
    c.append(col[2])
    pass
print('1st collumn:', a)
print('2nd collumn:', b)
print('3rd collumn:', c)		

# 8. Создать список любых объектов, в цикле напечатать в консоль: объект и его индекс
any_obj = [1, '1231', [1,2,3,], 3.345, str]
for i in any_obj:
	print(i, any_obj.index(i))

# 9. Создать список с тремя значениями 'to-delete' и нескольми любыми другими, удалить из него 
# все значения 'to-delete'

to_del  = ['to-delete', 123, str, float, 'to-delete', 'smth', [1,2,3,4,5], 'to-delete']

for i in to_del:
	if i == 'to-delete':
		to_del.remove(i)
print(to_del)

# 10. Пройти по всем числам от 1 до 10 в обратную сторону (то есть: от 10 до 1), 
# напечатать их в консоль
for i in range(10, 0, -1):
	print(i)