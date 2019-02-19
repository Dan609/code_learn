import random

print('Угадай число от 1 до 20!')

answer = random.randint(1, 20)
guess = 21

while guess != answer:
    guess = int(input('Введите число!'))
    if guess > answer:
        print('Многовато!')
    elif guess < answer:
        print('Маловато!')
    else:
        print('Поздравляю, победа!')