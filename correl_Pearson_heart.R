# Statistical Analysis with R #
# Pearson Correlation coef - example heart #

# load the libraries that I'll need 
library(readxl); library(pastecs);library(gmodels); library("ggpubr")

# set working directory #
setwd("~/Classes/University Courses/Data Analysis with R/R materials")

# import, name and view the dataset 
heart <- read_excel("Rkurssi1.xlsx", sheet = "heart")
View(heart)

# uncomment to save output to a separate file
#sink('correl_output2.doc', append=TRUE)

cor(heart[c("age","rrsyst","rrdias","height","weight")], use="complete.obs", method="pearson")

plot(heart$age, heart$cholest, main="Scatterplot",xlab="age ", ylab=" cholest", pch=19)

ggscatter(heart, x = "age", y = "cholest", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "age", ylab = "cholestrol")

pairs(~age + rrsyst + rrdias + height + weight,data=heart,main="Scatterplot Matrix")

#sink()