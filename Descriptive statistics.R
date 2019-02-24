#Step 2: Data preprocessing

?mtcars

df  <- mtcars

str(df)

df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))


#Step 3: Descriptive statistics

median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)

mean_disp  <- mean(df$disp)

mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == "V"])

sd(df$hp[df$cyl != 3 & df$am == "Auto"])

#Step 5: Aggregation

?aggregate
aggregate(x = df$hp, by = list(df$vs), FUN = mean)
mean_hp_vs  <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)

colnames(mean_hp_vs)  <- c("VS", "Mean HP")

aggregate(hp ~ vs, df, mean)

aggregate(hp ~ vs + am, df, mean)
aggregate(x = df$hp, by = list (df$vs, df$am), FUN = mean)

aggregate(x = df[,-c(8,9)], by = list(df$am), FUN = median)

aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd)

aggregate(cbind(mpg, disp) ~ am + vs, df, sd)
cbind(df$mpg, df$disp)
my_stats  <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd)


#Step 8, 9: Library "psych"


library(psych)

?describe

describe(x = df)

descr  <- describe(x = df[,-c(8,9)])

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs)

descr2$V
descr2$S

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1)

descr3  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1, fast = T)

describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, fast = T)

describeBy(airquality, group = airquality$Month)

#Step 10: NA values
is.na(df$mpg)
sum(is.na(df))

df$mpg[1:10]  <- NA
mean(df$mpg)
mean(df$mpg, na.rm = T)

aggregate(mpg ~am, df, sd)

describe(na.rm = )

update.packages()

describe(iris)

describeBy(iris, group = iris$Species)

?replace

qqq <- c(1:10)
zzz <- 88
rrr <- replace(qqq, qqq[2:3], 88)


my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA
my_vector[is.na(my_vector)]

mean(my_vector, na.rm = T)

my_vector[is.na(my_vector)] <- mean(my_vector, na.rm = T)

my_vector
fixed_vector

fixed_vector <- my_vector
fixed_vector[is.na((fixed_vector))] <- mean(my_vector, na.rm = TRUE)
fixed_vector


my_vector[is.na(my_vector)] <- mean(my_vector, na.rm = TRUE)

fixed_vector <- replace(my_vector, my_vector[is.na(my_vector)], mean(my_vector, na.rm = T))


