library(ggplot2)
library(colorspace)
library(RColorBrewer)
library(dplyr)
library(magrittr) 
library(vcd)
library(treemapify)
library(scales)
library(ggpie)
library(rbokeh)

data(iris)
data("USJudgeRatings")
data(Arthritis)

#install.packages("devtools")                 
#devtools::install_github("rkabacoff/ggpie")
#install.packages("rbokeh")

#1 A basic scatterplot using colors and a legend
sID <- as.numeric(iris$Species)
plot(iris$Sepal.Length, iris$Petal.Length, main="Scatterplot of Sepal and Petal Length", 
     pch=sID, cex=1, col=heat.colors(3)[sID], xlab="Sepal Length", ylab="Petal Length") + legend("topleft", levels(iris$Species), col=heat.colors(3), pch=1:3, cex=0.7)

#2 A scatterplot using a gradient of colors allocated as buckets/bins
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(color = Petal.Width), shape=15) +
  scale_color_viridis_c()

#3 A scatterplot using colored text labels in the place of points
plot(iris$Sepal.Length, iris$Sepal.Width, col="white", pch=19, ylim=c(2,5),xlab="Sepal Length", ylab="Sepal Width") + 
  text(iris$Sepal.Length, iris$Sepal.Width, iris$Species, col=rainbow_hcl(3)[sID], cex=0.8)

#4 A stacked bar chart with colors; A grouped bar chart with colors; A filled bar chart with colors
barplot(table(iris$Species,iris$Sepal.Length),col = brewer.pal(3,"Set3"), main="Stacked Bar Chart of Sepal Length", xlab="Sepal Length", ylab="Frequency")

table(iris$Species, iris$Sepal.Width)
ggplot(iris, aes(x=Sepal.Width, fill=Species)) + 
  geom_bar(position = "dodge") + labs(title="Grouped Bar chart of Sepal Length",
                                      x="Sepal Length", y="Frequency")

ggplot(iris, aes(x=Sepal.Width, fill=Species)) + 
  geom_bar(position = "fill") + labs(title="Grouped Bar chart of Sepal Length (Filled)",
                                      x="Sepal Length", y="Frequency")

#5 A mean bar chart & standard error rate (≈reliability) as error bars
plotdata <- iris %>% 
  group_by(Species) %>% 
  summarize(mean = mean(Petal.Length))

ggplot(plotdata, aes(x=reorder(Species, mean), y=mean)) + 
  geom_bar(stat="identity", fill="pink3") +
  labs(x="Species",
       y="",
       title = "Mean Petal Length")

plotdata2 <- iris %>% 
  group_by(Species) %>% 
  summarize(n=n(),
            mean = mean(Petal.Length), 
            se = sd(Petal.Length)/sqrt(n))

ggplot(plotdata2, aes(x=reorder(Species, mean), y=mean)) + 
  geom_bar(stat="identity", fill="pink3") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) + 
  labs(x="Species",
       y="",
       title = "Mean Petal Length", subtitle = "with standard error bars")

#6 A bar chart with 10+ bars and angled labels for each bar
ggplot(USJudgeRatings, aes(x=row.names(USJudgeRatings),y=RTEN)) +
  geom_bar(stat="identity", fill="pink3") +
  labs(title="Retention rate in US Judge Ratings",
       y="Retention rating", x="") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=5))

#7 2 comparative pie charts side-by-side with labels and percentages
ggpie(Arthritis,Improved,Treatment, legend=FALSE, offset=1.3, title="Effectiveness of Arthritis Treatment")

#8 A basic treemap
plotdata <- Arthritis %>% count(Improved)
ggplot(plotdata, 
       aes(fill = Improved,
           area = n,
           label = Improved)) + 
  geom_treemap() + 
  geom_treemap_text(color="white", place="center") +      
  theme(legend.position = 0)

#9 A treemap divided into 2 or more sections
plotdata <- Arthritis %>% count(Improved, Sex)
plotdata$Sex <- factor(plotdata$Sex, levels=c("Female", "Male"),
                       labels=c("Female", "Male"))

ggplot(plotdata, 
       aes(fill = Improved,
           area = n,
           label = Improved, subgroup=Sex)) +
  geom_treemap() + 
  geom_treemap_subgroup_border() + 
  geom_treemap_subgroup_text(place = "middle", colour = "red", alpha = 0.5, grow = FALSE) +
  geom_treemap_text(colour = "white", place = "centre", grow=FALSE) + 
  theme(legend.position = "none")

#10 A histogram with a density curve
ggplot(iris, aes(x=Sepal.Width, y=..density..)) + 
  geom_histogram(bins=20, color="white", fill="pink3") +
  scale_y_continuous(labels=scales::percent) +
  geom_density(color="red", size=1) +
  labs(title="Histogram of sepal width with density curve", 
       y="Percent", 
       x="Sepal Width")

#11 2 kernel density plots with different bandwidth settings
ggplot(iris, aes(x=Sepal.Width), xlab="Sepal Width") + 
  geom_density(fill="pink3") + 
  labs(title="Kernel density plot of sepal width")

ggplot(iris, aes(x=Sepal.Width), xlab="Sepal Width") + 
  geom_density(fill="pink3", bw=0.5) + 
  labs(title="Kernel density plot of sepal width")

#12 A kernel density plot visualizing 3 or more overlapping divisions from the data
ggplot(iris, aes(x=Sepal.Width, color=Species, linetype=Species)) + 
  geom_density(size=1) +
  labs(title="Sepal width by species",
       x = "sepal width")

#13 A notched box plot
ggplot(iris, aes(x=Species, y=Sepal.Length)) + 
  geom_boxplot(notch=TRUE,
               fill="pink3",
               varwidth=TRUE) + 
  labs(x="Species",
       y="Sepal Length", 
       title="Sepal length by species")

#14 A violin plot for the same data as in L
ggplot(iris, aes(x=Species, y=Sepal.Length)) + 
  geom_boxplot(width=0.2,
               fill="gold") + 
  geom_violin(fill="lightblue",
              alpha=0.3) + 
  labs(x="Species",
       y="Sepal Length",
       title="Violin Plots of Sepal Length by Species")

#15 A sorted/ordered dot plot
ggplot(USJudgeRatings, aes(x=INTG, y=reorder(row.names(USJudgeRatings), INTG))) +
  geom_point(color="pink4", shape=18) +
  labs(x="Integrity Rating", 
       y="",
       title="Integrity Rating for US Judges") +
  theme(axis.text.y = element_text(size=5)) 

#Extra credit
sub <- USJudgeRatings[,c(6, 12)]
lin_reg <- lm(RTEN~DECI, data = sub)

judge_plot <- figure(width = 600, height = 600, title = "Prompt decisions and retention ratings of US judges") %>%
  ly_points(sub, hover = sub, color="orange") %>%
  ly_lines(lowess(sub), legend = "lowess") %>%
  ly_abline(lin_reg, type = 2, legend = "lm")

judge_plot



