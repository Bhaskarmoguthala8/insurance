insurance=read.csv("insurance.csv")
insurance$sex <- factor(insurance$sex, levels=c("male","female"), ordered=FALSE)
insurance$smoker <- factor(insurance$smoker, levels=c("yes","no"), ordered=FALSE)
insurance$region <- factor(insurance$region,levels=c("southwest","southeast","northwest","northeast"))
View(insurance)
str(insurance$smoker)
str(insurance)
table(insurance$sex)
table(insurance$smoker)
table(insurance$region)
install.packages("psych")
library(psych)

pairs.panels(insurance, smooth=FALSE, density=TRUE, ellipses=FALSE, method="pearson",pch=21,
             lm=FALSE,cor =TRUE, jiggle=FALSE,)
attach(insurance)
model=lm(charges~ age+sex+bmi+children+smoker+region)
summary(model)

charges~10818.63+ 256.86*age+339.19*bmi+475.50*children-23848.53*smokerno-174.97*regionnortheast
model2=lm(charges~ age+bmi+children+smoker+region, data=insurance)
model2
summary(model2)
charges~10886.66 + 338.66  * age + bmi * 338.66 +    474.57 * children - 23836.30 * smoker + 959.37 * regionnortheast
AIC(model)
AIC(model2)
BIC(model)
BIC(model2)
paste("AIC for model1 ",(AIC(model)))
paste("AIC for model2 ",(AIC(model2)))
paste("BIC for model1 ",(BIC(model)))
paste("BIC for model2 ",(BIC(model2)))
