library(ggplot2)
library(mosaicData)
library("ggplot2")
library("grid")
library("units")
#install.packages("units")
data("airquality")

#1 A table of one variable
tab = table(airquality$Ozone)
tab

#2 The mean, the median and the standard deviation for two complementary subsets
mean_tpm <- tapply(airquality$Temp, airquality$Month, mean)
mean_tpm

median_tpm <- tapply(airquality$Temp, airquality$Month, median)
median_tpm

sd_tpm <- tapply(airquality$Temp, airquality$Month, sd)
sd_tpm

#3 A histogram
hist(airquality$Temp,
     main="Maximum Daily Temperature at La Guardia Airport",
     col="pink",
     xlab="Temperature in degrees Fahrenheit",
     xlim=c(60,100),
     breaks=20)

#4 Display 2 histograms in one graphic comparing 2 subsets of the data
May <- subset(airquality, Month==5)
Sep <- subset(airquality, Month==9)

par(mfrow=c(1,2))
hist(May$Temp,
     main="Maximum Daily Temperature in May",
     col="pink",
     xlab="Temperature in degrees Fahrenheit",
     xlim=c(60,70),
     breaks=60)

hist(Sep$Temp,
     main="Maximum Daily Temperature in September",
     col="pink",
     xlab="Temperature in degrees Fahrenheit",
     xlim=c(60,100),
     breaks=20)

#5 Provide quartile, decile and percentile measurements of one variable
quantile(airquality$Wind)

quantile(airquality$Wind, probs = seq(.1, .9, by = .1))

#6 Show “binning” of the data for one variable
breakPoints = quantile(airquality$Temp, prob=seq(0,1,length.out=11),
                       names=FALSE)
bin = cut(airquality$Temp, breakPoints,labels=FALSE,include.lowest=TRUE)
table(bin, airquality$Month)

hist(airquality$Temp, breaks=breakPoints)

#7 Show cumulative sums for one variable using cumsum
cumsum(airquality$Wind)

#8 Display multiple histograms (at least 4) in one graphic using a for loop
bands <- c("May","June","July","August")

par(mfrow=c(2,2))

for(i in 5:8){
  newData <- airquality[which(airquality$Month == i),names(airquality) %in% c("Temp")]
  xName <- paste("Temp in", bands[i-4])
  hist(newData, xlab = xName, main = "Histograms of Temperature")
}

#9 A scatterplot with colors and labels for which you used the unique function

uniAir <- unique(airquality)

ggplot(uniAir,aes(x=Temp,y=Wind)) + geom_point(aes(colour = factor(Wind)))

#10 A scatterplot using cex, pch and col ; set labels with text and titles with main, and a key using legend (see the links below for detailed guides to these functions)
par(mfrow=c(1,1))
plot(uniAir$Temp, uniAir$Wind, main = "Scatterplot of Temp-Wind", pch = 18, cex = 1, col="pink", xlab="Temperature", ylab = "Wind Speed")
+ text (x=91, y = 20.2, labels = "Temp-Wind")

#11 A line plot with labels

plot(May$Day, May$Temp, type = "l", main="Maximum Daily Temperature in May", xlab="Day", ylab="Temperature")

#12 for loop -> scatter plot matrix
par(mfrow=c(4,4))
matrixData <- airquality[,c(1:4)]
names <- c("Ozone", "Solar.R", "Wind", "Temp")
for(i in 1:4){
  for (j in 1:4){
    plot(matrixData[,c(i)], matrixData[,c(j)], pch = 19, cex = 1, col="pink", xlab=names[i], ylab = names[j])
    box()
    if(i == 1) title(main = colnames(matrixData)[j])
  }
}
dev.off
graphics.off()

#13 ggplot 2 colors
colors2A <- airquality[which(airquality$Month == 5),names(airquality) %in% c("Temp", "Wind", "Month")]
colors2B <- airquality[which(airquality$Month == 9),names(airquality) %in% c("Temp", "Wind", "Month")]
colors2 <- rbind(colors2A, colors2B)
ggplot(colors2,aes(x=Temp,y=Wind)) + geom_point(aes(colour = factor(Month))) + 
  scale_color_manual(labels = c("May", "Sep"), values = c("slateblue", "indianred2"))

#14 ggplot range of color
ggplot(colors2,aes(x=Temp,y=Wind)) + geom_point(aes(colour = factor(Temp)))+guides(colour = guide_legend())

