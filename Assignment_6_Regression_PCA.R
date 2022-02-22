#install.packages("corrplot")
library(corrplot)
data("USJudgeRatings")

#1. Mathematical correlations for the dataset
cor(USJudgeRatings)

#2. Corrgrams / Corrplots for the dataset (corrplots are better)
corrplot(cor(USJudgeRatings), method="circle")

#3. Scatterplots with regression lines for the dataset
plot(USJudgeRatings)
pairs(USJudgeRatings, panel=panel.smooth)

#5. Provide the output from a T-test for the fields you chose.
t.test(USJudgeRatings$INTG, USJudgeRatings$RTEN)

#6. Create boxplots (showing outliers) and density plots for the fields you chose
boxplot(RTEN~INTG,data=USJudgeRatings, main="US Judge Ratings",
        xlab="Judicial integrity", ylab="Worthy of retention")

d <- density(USJudgeRatings$INTG)
plot(d, main='Density Plot for Judicial Integrity')

d <- density(USJudgeRatings$RTEN)
plot(d, main='Density Plot for Retention Rating')

#7. Create one linear regression model for a single predictor variable in the formula, 
#and one model with multiple predictor variables in the formula.
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)

ggplot(USJudgeRatings, aes(x=INTG,y=RTEN)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

judge_lm <- lm(RTEN ~ INTG, data=USJudgeRatings)
summary(judge_lm)

ggplt2 <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,shape=Species))+
  geom_point()+
  theme_classic()

ggplt2+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
                  aes(color=Species))

ggplot(USJudgeRatings, aes(x=RTEN,y=INTG)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

fit1 <- lm(RTEN ~ INTG, data = USJudgeRatings)
summary(fit1)

fit2 <- lm(INTG ~ RTEN, data = USJudgeRatings)
summary(fit2)

#8. Create a training model using 80% of the dataset and then use it on the other 20%.
#Use it to produce a CVlm visualization and provide the mean square.

library(DAAG)
trainingRowIndex <- sample(1:nrow(USJudgeRatings), 0.8*nrow(USJudgeRatings)) 
trainingData <- USJudgeRatings[trainingRowIndex, ]  
testData  <- USJudgeRatings[-trainingRowIndex, ]  

lmMod <- lm(RTEN ~ INTG, data=trainingData) 
RTENPred <- predict(lmMod, testData)

actuals_preds <- data.frame(cbind(actuals=testData$RTEN, predicteds=RTENPred))
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy # 98.99% !

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape # 0.02087056

cvResults <- suppressWarnings(CVlm(data=USJudgeRatings, form.lm=formula(RTEN ~ INTG), m=5, dots=FALSE, seed=29, legend.pos="topleft",  plotit=TRUE, printit=FALSE)); 

attr(cvResults, 'ms') # 0.17086

#9. The summary of the dataset’s PCA model showing the standard deviations,
# proportions of variance and cumulative proportions.

USJ_pca <- prcomp(USJudgeRatings, center = TRUE,scale. = TRUE)
summary(USJ_pca)

#10. The ggibiplot of the PCA model, with arrows plus rownames as labels
ggbiplot(USJ_pca)

ggbiplot(USJ_pca, labels=rownames(USJudgeRatings), labels.size=2)

#11. The ggbiplot of the PCA model with groups marked with colored ellipses
iris_pca <- prcomp(iris[, 1:4], center = TRUE,scale. = TRUE)

ggbiplot(iris_pca,ellipse=TRUE,  labels=rownames(iris), groups=iris$Species)

