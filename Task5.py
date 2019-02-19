# AlexanderKrivoshein
# https://gist.github.com/AlexanderKrivoshein/6bbb49f00abe1ad5455f902dbb1078fc

# Course 4. Home work 5. Krivoshein Alexander

# # 1
# Реализовать класс Person, у которого должно быть два публичных поля: age и name. Также у него должен быть следующий набор методов: know(person), 
# который позволяет добавить другого человека в список знакомых. И метод is_known(person), который возвращает знакомы ли два человека

class Person():
	age = None
	name = ''
	list_of_known_persons = {}
	def __init__ (self, name, age):
		self.name = name
		self.age = age

	def is_known(self, person):
		try: # в данном случае ловим KeyError, если персона не находится в словаре, думаю что описание исключения внутри метода класса уместнее.
			if self.list_of_known_persons[person.name] == person.age:
				return True
			else:
				return False
		except KeyError:
			return False
	def known(self, person):
		self.list_of_known_persons[person.name] = person.age

Alex = Person('Alexander',32)
Sergey = Person('Sergey', 30)
Nikita = Person('Nikita', 27)
Dima = Person('Dima', 25)

Alex.known(Sergey)
Alex.known(Nikita)

print("Check {}'s relationship with {} : {} \n ".format(Alex.name, Sergey.name, Alex.is_known(Sergey)))
print("Check {}'s relationship with {} : {} \n ".format(Alex.name, Nikita.name, Alex.is_known(Nikita)))
print("Check {}'s relationship with {} : {} \n ".format(Alex.name, Dima.name, Alex.is_known(Dima)))


# # 2
# Есть класс, который выводит информацию в консоль: Printer, у него есть метод: log(*values). 
# Написать класс FormattedPrinter, который выводит в консоль информацию, окружая ее строками из *

class Printer():
	def log(self, *values):
			print(values)

class FormattedPrinter(Printer):
	def log(self, *values):
		print('*'*(len(values)*3+2))
		print('*{}*'.format(values))
		print('*'*(len(values)*3+2))

a = Printer()
a.log(1, 2, 3, 4, 5, 6, 7, 8, 9)

b = FormattedPrinter()
b.log(1, 2, 3, 4, 5, 6, 7, 8, 9)


# # 3
# Написать класс Animal и Human, сделать так, чтобы некоторые животные были опасны для человека (хищники, ядовитые). 
# Другие - нет. За что будет отвечать метод is_dangerous(animal)

class Animal():
	animal_type = ''
	def __init__ (self, animal_type):
		self.animal_type = animal_type

class Human():
	dangerous_animals = ['poisonous', 'predator']

	def is_dangerous(self, animal):
		return animal.animal_type.lower() in self.dangerous_animals

MyAnimal = Animal('herbivore')
MyAnimal1 = Animal('predator')
MyAnimal2 = Animal('poisonous')

MyHuman = Human()

print('Is {} animal dangerous for human? : {}'.format(MyAnimal.animal_type, MyHuman.is_dangerous(MyAnimal)))
print('Is {} animal dangerous for human? : {}'.format(MyAnimal1.animal_type, MyHuman.is_dangerous(MyAnimal1)))
print('Is {} animal dangerous for human? : {}'.format(MyAnimal2.animal_type, MyHuman.is_dangerous(MyAnimal2)))