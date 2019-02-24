x <- read.csv("D:\\test.csv", header=TRUE)

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

library(biwavelet)
library(Rwave)

z <- ts(calcium$ROI543, start = calcium$Time[1])
z1 <- ts(calcium$ROI543, start = calcium$Time[1])
z2 <- ts(calcium$ROI333, start = calcium$Time[1])
RoverS(z, n.block.min = 2, scale.ratio = 2, scale.min = 8)

##

i <- c(1:10)

for (i in 1:10) {
  print(i)
}


## no lines for long series
z <- ts(calcium$ROI410, start = calcium$Time[1])
?ts
z3 <- ts(cbind(calcium$ROI147, calcium$ROI543, calcium$ROI333),
         start = calcium$Time[1])


par(mfrow=c(1,1))

plot(z)

plot(z3, plot.type = 's', col = 1:3)

z_mult <- ts(cbind(calcium[,410:420]),
         start = calcium$Time[1])

plot(z_mult, plot.type = 's', col = 1:10)


####

z3 <- ts(cbind(calcium$ROI147, calcium$ROI416, calcium$ROI409),
         start = calcium$Time[1])

plot(z3, plot.type = 's', col = 1:3)

plot(z3, plot.type = 'm', col = 1:3)

plot(z3, plot.type = 'm', mar=c(gap=0.3, 5.1, gap=0.3, 2.1))

####
wf(z, 14.29745)
####

oscillo(z, 14.29745)


#####

install.packages("biwavelet")

plot(ts(z), xlab = NA, ylab = NA)
dim_z <- dim(z)

plot(z, type = "power")

plot(z, type = "power.corr.norm", plot.cb=TRUE, plot.phase=FALSE)

## returns the cepstrum of a time wave

ceps(z, 14.29745)

##

coh(z1, z2, 14.29745)


library(fracdiff)
library(arfima)
library(nonlinearTseries)
library(tseriesChaos)
library(scatterplot3d)

z <- ts(calcium$ROI147, start = calcium$Time[1])

##

fracdiff(z, nar = 0, nma = 0,
         ar = rep(NA, max(1)), ma = rep(NA, max(1)),
         dtol = NULL, drange = c(0, 0.5), M = 100, trace = 0)

##

estimateEmbeddingDim(z, number.points = length(z),
                     time.lag = 1, max.embedding.dim = 10, threshold = 0.95,
                     max.relative.change = 0.1, do.plot = TRUE,
                     main = "Computing the embedding dimension", xlab = "dimension (d)",
                     ylab = "E1(d) & E2(d)", ylim = NULL, xlim = NULL)

##

xyz <- embedd(z, m=3, d=8)

scatterplot3d(xyz, type = "p")

?scatterplot3d

### Для регулярных движений L<=0
# В хаотических режимах L>0

z <- ts(calcium$ROI543, start = calcium$Time[1])
output <- lyap_k(z, m=5, d=3, s=1500, t=10, ref=1000, k=3, eps = 10)
plot(output, ylim = c(1, 4))

lyap(output, 0.73, 2.47)

?lyap_k

## Space-time separation plot
z <- ts(calcium$ROI410, start = calcium$Time[1])
stplot(z, m=3, d=10, idt=1, 500)

##Plot time series against lagged versions of themselves
?lag.plot

lag.plot(z, 1, layout = NULL, set.lags = 1:lags,
         main = NULL, asp = 1,
         diag = TRUE, diag.col = "gray", type = "p", oma = NULL,
         ask = NULL, do.lines = (n <= 150), labels = do.lines)

lag.plot(z, 4, diag.col = "forest green", type = "l", main = "Lag plot")

lag.plot(z, 5)

lag.plot(z, 6, layout = c(2,1), asp = NA, col.main = "blue")

lag.plot(calcium[,409:417], lags = 1, type = "l")

## no lines for long series
z <- ts(calcium$ROI415, start = calcium$Time[1])

lag.plot(sqrt(z), set = c(1:4, 9:12), pch = ".", col = "black")

calcium[,2:41]

2 + 2

k <- 2 + 2

plot(k)

par(mfrow=c(1,1), mar=c(3,3,1,1))

boxplot(calcium[,2:41])

###

voltage <- read.csv2('di-8-anepps.csv')


xvol <- voltage$ROI18
voltage_ts <- ts(xvol, start = calcium$Time[1])

del <- 0.1
x.spec <- spectrum(voltage_ts, log = "no", span = 10, plot = FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec

par(mfrow=c(2,3))  # plot results

plot(xvol ~ voltage$Time,
     xlab = "time (s)", ylab = "fluorescence intensity (a.u.)", t="l",
     xlim = c(0, 110), ylim = c(0, 250), main = "Total signal")

plot(xvol ~ voltage$Time,
     xlab = "time (s)", ylab = "fluorescence intensity (a.u.)", t="l",
     xlim = c(0, 20), ylim = c(0, 250), main = "Signal segment")

boxplot(xvol, ylab = "fluorescence intensity (a.u.)",
        main = "Mean intensity", ylim = c(0, 250))

plot(spy ~ spx, xlab="frequency (Hz)", ylab="spectral density",
     type="l", lwd = 2)

acf(xcal, lag.max = 100, main = "Autocorrelation")

w <- as.integer(xvol)  # Entropy measure

Srho(w, lag.max = 10, stationary = TRUE, plot = TRUE,
     version = "FORTRAN")
