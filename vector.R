my_vector[is.na(my_vector)] <- mean(my_vector, na.rm = T)
print(my_vector)


# Ниже небольшой код, который может создать случайный вектор (выборка из нормального распределения) с пропущенными значениями.

my_vector <- rnorm(30)

my_vector[sample(1:30, 10)] <- NA # на десять случайных позиций поместим NA

fixed_vector <- replace(my_vector, my_vector[is.na(my_vector)], mean(my_vector, na.rm = T))
