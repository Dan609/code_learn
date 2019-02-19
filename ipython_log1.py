# IPython log file

get_ipython().run_line_magic('logstart', '')
class Car:
    speed = 5
    
car1 = Car()
car2 = Car()
car1.engine
car2.engine
car1.engine = 'V8 Turbo'
car1.engine
car2.engine
Car.wheels = 'Wagon wheels ('
c1.wheels
car1.wheels
Car = Car()
Car
new_car = Car()
Car.speed
car1.speed
car2.wheels
car2.engine
Car.engine
# Classes can have both methods and fields
class Window:
    is_opened = False

    def open(self):
        self.is_opened = True
        print('Window is now', self.is_opened)

    def close(self):
        self.is_opened = False
        print('Window is now', self.is_opened)
        
window1 = Window()
window2 = Window()
window1.open()
window2.is_opened
dir(window2)
class Car:
    speed = 0
    
Car.engine = 'V8'
class Car:
    engine = 'V8'
    speed = 0
    
window2.__class__
class Car:
    speed = 0
    
    def __init__(self, color):
        print('Constructor is called!')
        print('Self is the object itself!', self)
        self.color = color
        
car1 = Car()
car1 = Car('red')
car2 = Car('black')
car1.speed
car2.speed
car2.color
car1.color

class Car:
    speed = 0

    def __init__(self, color, transmission='Auto'):
        print('Constructor is called!')
        print('Self is the object itself!', self)
        self.color = color
        self.transmission = transmission
        
car1 = Car('black')
car2 = Car('yellow', 'Manual')
car1.transmission
car1.color
car1.transmission
class Car:
    speed = 0

    def __init__(self, color='black', transmission='Auto'):
        print('Constructor is called!')
        print('Self is the object itself!', self)
        self.color = color
        self.transmission = transmission
        
car1 = Car(transmission='Manual')
car1 = Car(,'Manual')
Car()
c3 = Car()
car1.speed
class Test(object):  # it is the same as the code above (py3 only)
    pass
class Test:  # it is the same as the code above (py3 only)
    pass
class Person(object):
    biological_name = 'homo sapiens'

    def __init__(self, name, age):
        self.name = name
        self.age = age

    def walk(self):
        print('Walking...')

    def say_hello(self):
        print('I am a person', self.name, self.age)
        
person = Person('Ivan', 25)
person.walk()
person.say_hello()

class Student(Person):
    def say_hello(self):
        print('I am a student', self.name, self.age)
        
student = Student('Fedor', 19)
student.walk()
student.say_hello()
class Car:
    pass
class CarWithManualTransmission(Car):
    pass
class CarWithAutoTransmission(Car):
    pass
type(student)
isinstance(student, Student)
isinstance(student, Person)
class Child(Person):
    def walk(self):
        raise ValueError('Can not walk')

    def say_hello(self):
        raise ValueError('Can not talk')

    def crawl(self):
        print('Haha!')
        
new_child = Child()
new_child = Child('Petr', 1)
new_child.walk()
new_child.say_hello()
new_child.crawl()
student.crawl()
person.crawl()
class Example(object):
    def __init__(self):
        self.a = 1
        self._b = 2
        self.__c = 3
        print('{} {} {}'.format(
            self.a, self._b, self.__c))

    def call(self):
        print('Called!')

    def _protected_method(self):
        pass

    def __private_method(self):
        pass
    
try:
    print(example.__c)
except AttributeError as ex:
    print(ex)
    
example = Example()
try:
    print(example.__c)
except AttributeError as ex:
    print(ex)
dir(example)
example._Example__c
class Parent(object):
    def call(self):
        print('Parent')


class Child(Parent):
    def call(self):
        print('Child')
len((1,3,4,))
len(['a', 'g', 'b'])
len("string")
def call_obj(obj):
    obj.call()
    


call_obj(Child())
call_obj(Parent())
