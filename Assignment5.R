# Statistical Analysis with R #
# One way ANOVA - Table 8.1 #

# load the libraries that I'll need 
library(readxl); library(pastecs);library(gmodels)
library(car); library(multcomp); library(ggplot2)
library(emmeans); library(StepReg); library(olsrr)
library("ggpubr"); library(writexl)

# set working directory #
setwd("~/Classes/University Courses/Data Analysis with R/R materials")

# import, name and view the dataset 
donkey <- read_excel("Rkurssi1.xlsx", sheet = "donkey")
View(donkey)

# simple scatter plot of heart girth versus bodyweight
plot(donkey$Heartgirth, donkey$Bodywt)

# scatter plot fitted with a regression line
ggscatter(donkey, x = "Heartgirth", y = "Bodywt", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Heartgirth", ylab = "Bodywt")

model <-lm(Bodywt~Age+Heartgirth+Height+Length,data=donkey);summary(model)

ols_vif_tol(model)# studying the collinearity

vif(model)# (not used) another way of studying collinearity

# saving the predicted values as the variable pred
pred <- model$fitted.values;write_xlsx(data.frame(pred),"donkeypred.xlsx")

ggscatter(cbind(donkey,pred), x = "Bodywt", y = "pred", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Bodywt", ylab ="pred" )

plot(donkey$Bodywt, pred)#not used


e<-model$residuals #residuals

e_st<-rstandard(model) #standard residuals

plot(model)#not used

plot(donkey$Bodywt,e_st)

shapiro.test(e)

qqnorm(e,main="QQ plot of normal data",pch=19); qqline(e)


#stepwise method (excluding the nr of the donkey and the sex); based on adjRsq = adjusted R squared approach
stepwise(donkey,y="Bodywt",exclude=c("Donkey","Sex"),selection="bidirection",select="adjRsq")
stepwise(donkey,y="Bodywt",exclude=c("Donkey","Sex"),selection="forward",select="adjRsq")


#final model 
model2 <-lm(Bodywt~Heartgirth+Length,data=donkey);summary(model2)

ols_vif_tol(model2) # studying the collinearity

pred2 <- model2$fitted.values;write_xlsx(data.frame(pred),"donkeypred2.xlsx")

ggscatter(cbind(donkey,pred2), x = "Bodywt", y = "pred2", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Bodywt", ylab ="pred" )


e<-model2$residuals # residuals

e_st<-rstandard(model2) #the corresponding standard residuals

plot(model2)

plot(donkey$Bodywt,e_st)

shapiro.test(e)

qqnorm(e,main="QQ plot of normal data",pch=19); qqline(e)