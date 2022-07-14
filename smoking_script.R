# Statistical Analysis with R #
# Crosstabulation example 1 #

# load the libraries that I'll need 
library(readxl); library(pastecs);library(gmodels)
library(gplots);library(car);library(psych)

# set working directory #
setwd("~/Classes/University Courses/Data Analysis with R/R materials")

# import, name and view the dataset 
smoking <- read_excel("Rkurssi1.xlsx", sheet = "smoking")
View(smoking)

# uncomment to save output to a separate file
#sink('crosstab_output.doc', append=TRUE)

CrossTable(smoking$lungcancer,smoking$smoking, digits=3, 
max.width = 5, expected=T, prop.r=FALSE, 
prop.c=TRUE,prop.t=FALSE, chisq=TRUE, fisher=TRUE, 
format="SPSS")

#Test for proportions ("Z-test" for 2 proportions)
# columns 1,2 and their sumswhen row=cancer
prop12<-prop.test(x = c(170, 330), n = c(560,440),correct=T);prop12

#sink()