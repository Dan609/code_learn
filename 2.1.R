HairEyeColor
dim(HairEyeColor)
dimnames(HairEyeColor)
HairEyeColor[ , ,'Male']

red_men <- sum(HairEyeColor[ 'Red',"Blue",'Male'])/sum(HairEyeColor[ ,"Blue",'Male'])
red_men
print(sum(HairEyeColor[ ,"Green",'Female']))

library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
fem <- subset(mydata, Sex=='Female')
obj <- ggplot(data = fem, aes(x = fem$Hair, y = fem$Freq, fill = fem$Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
obj

chisq.test(HairEyeColor["Brown",,"Female"])

diamonds

main_stat <- chisq.test(table(diamonds$cut,diamonds$color))[1]

###2.1

mydata <- as.data.frame(diamonds)
factor_price <- ifelse(mydata$price >= mean(mydata$price), 1, 0)
factor_carat <- ifelse(mydata$carat >= mean(mydata$carat), 1, 0)
table_1 <- table(factor_price, factor_carat)
main_stat <- chisq.test(table_1)[1]

##

fisher_test <- fisher.test(mtcars$am, mtcars$vs)$p.value
