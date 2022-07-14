# Statistical Analysis with R #
# Normality tests - Table 7.2 #

# load the libraries that I'll need 
library(readxl); library(pastecs);library(gmodels)

# set working directory #
setwd("~/Classes/University Courses/Data Analysis with R/R materials")

# import, name and view the dataset 
Table7.2 <- read_excel("Rkurssi1.xlsx", sheet = "Table7.2")
View(Table7.2)

# Normality Tests to see whether we can use the t-test
shapiro.test(subset(Table7.2$Ewe_weight, Table7.2$Treatment=="c"))
shapiro.test(subset(Table7.2$Ewe_weight, Table7.2$Treatment=="t"))

# QQ plot: the closer the dots are to the line, the more normally distributed they are
qqnorm(subset(Table7.2$Ewe_weight, Table7.2$Treatment=="c"),main="QQ plot of normal data - control",pch=19); qqline(subset(Table7.2$Ewe_weight, Table7.2$Treatment=="c"))
qqnorm(subset(Table7.2$Ewe_weight, Table7.2$Treatment=="t"),main="QQ plot of normal data - treatment",pch=19); qqline(subset(Table7.2$Ewe_weight, Table7.2$Treatment=="c"))

# histograms to see the shape of the distribution (qualitative confirmation of normality)
hist(subset(Table7.2$Ewe_weight, Table7.2$Treatment=="c"),probability=T)
hist(subset(Table7.2$Ewe_weight, Table7.2$Treatment=="t"),probability=T)

# some basic statistics
library(readxl); library(pastecs);library(gmodels) 
library(gplots);library(writexl);library(car);library(psych)
library(FREQ); library(epiDisplay)

describe(subset(Table7.2$Ewe_weight, Table7.2$Treatment=="c"))
describe(subset(Table7.2$Ewe_weight, Table7.2$Treatment=="t"))

# T-test
#t.test(subset(Table7.2$Ewe_weight, Table7.2$Treatment=="t"),mu=67.37, alternative = "two.sided",level=0.95)

# ANOVA 1
# 2 independent samples

library(readxl); library(pastecs);library(gmodels); library(gplots); library(car);library(psych)

describeBy(Table7.2$Ewe_weight, Table7.2$Treatment)

#normally distributed?
tapply(Table7.2$Ewe_weight, Table7.2$Treatment,shapiro.test)

#variances equal?
leveneTest(Table7.2$Ewe_weight,factor(Table7.2$Treatment),location="mean")

#if yes, then var.equal=T
t.test(formula= Table7.2$Ewe_weight ~ Table7.2$Treatment,data=Table7.2,paired =F,var.equal=T,conf.level=0.95) 

#or with ANOVA
model <- aov(Table7.2$Ewe_weight ~ Table7.2$Treatment, data= Table7.2); summary(model)


plotmeans(Table7.2$Ewe_weight ~ Table7.2$Treatment,p=0.95)

#if ANOVA used
pairwise.t.test(Table7.2$Ewe_weight,Table7.2$Treatment ,p.adjust.method = "bonferroni")
