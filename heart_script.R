# Statistical Analysis with R #
# descriptive statistics #

# load the libraries that I'll need 
library(readxl); library(pastecs);library(gmodels) 
library(gplots);library(writexl);library(car);library(psych)
library(FREQ); library(epiDisplay); library(summarytools)

# set working directory #
setwd("~/Classes/University Courses/Data Analysis with R/R materials")

# import, name and view the dataset 
heart <- read_excel("Rkurssi1.xlsx", sheet = "heart")
View(heart)

# uncomment to save output to a separate file
#sink('descr_output.doc', append=TRUE)

# basic descriptive statistics - see Help for details 
describe(heart$age)

# inside the vector c I specify that I want the 
# stats for those 4 variables 
describe(heart[c("age","cholest","rrsyst","rrdias")])

# calculating basic stats for age separately by gender
describeBy(heart$age, heart$gender)

# histogram for the case of age
hist(heart$age)

# histogram in the case of age when gender is male
hist(subset(heart$age,heart$gender=="male")) 

# boxplot for ages
boxplot(heart$age)

# 2 boxplots for age in same figure, for each gender
boxplot(heart$age ~ heart$gender)

# frequency table for gender (cumulative percentages)
tab1(heart$gender, cum.percent = TRUE) 
# frequency table for diabetes (cumulative percentages)
tab1(heart$diab, cum.percent = TRUE)
# sorted by decreasing order
tab1(heart$diab, sort.group="decreasing", cum.percent = TRUE) 

# to create bins (turn continuous variables into categorical variables)
agebinned<-cut(heart$age,c(0,50,60,100),labels=1:3, right=FALSE);agebinned

# bar plots to display the newly created bins
barplot(table(agebinned))
# version of the bar plot with a title
barplot(table(agebinned),main = "Binned ages", font.main = 4)

tab1(agebinned, cum.percent = TRUE)

# uncomment to stop sink()
# sink()

# write an output to excel to save  our new variable
write_xlsx(data.frame(agebinned),"agebinned.xlsx") 

# partial data sets: (easier to do in excel using the filter command)
# only age < 60
s1 <-subset(heart,age<60)

#only age>70 or age<50 and diab=no 
s2 <-subset(heart,age>70&diab=="no"|age<50&diab=="no") 

#only smoking=yes, and variables cholest and bloodpr 
s3 <-subset(heart,smoking=="yes",select=c("cholest","bloodpr"))