# Statistical Analysis with R #
# One way ANOVA - Table 8.1 #

# load the libraries that I'll need 
library(readxl); library(pastecs); library(gmodels); library(gplots)
library(car); library(psych);library(DescTools)

# set working directory #
setwd("~/Classes/University Courses/Data Analysis with R/R materials")

# import, name and view the dataset 
table81 <- read_excel("Rkurssi1.xlsx", sheet = "Table8.1")
View(table81)

# descriptive basic statistics
describeBy(table81$Calculus, table81$Group)

# testing whether the data is normally distributed; if Ho accepted then normal
tapply(table81$Calculus, table81$Group,shapiro.test)

# Levene's test to study the groupwise variances; if Ho accepted then similar
leveneTest(table81$Calculus,factor(table81$Group),location="mean")

# aov = ANalysis Of VAriance (ANOVA)
model<-aov(Calculus~factor(Group),data=table81)
summary(model)

plotmeans(table81$Calculus ~ table81$Group)

# post-hoc test (Bonferroni)  to study pairwise comparison since at least 2 groups were different
pairwise.t.test(table81$Calculus,table81$Group,p.adjust.method = "bonferroni")

# pairwise with Tukey:
tukey<-TukeyHSD(model);tukey;plot(tukey)


boxplot(table81$Calculus ~ table81$Group)

# non-parametric approach because of the low number of data points (corresponds to ANOVA table)
kruskal.test(table81$Calculus ~ table81$Group,data=table81)

# post-hoc test for pairwise comparison
pairwise.wilcox.test(table81$Calculus, table81$Group, p.adjust.method = "bonferroni",paired=F)
