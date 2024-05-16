states=as.data.frame(state.x77)
states
str(states)
View(states)
attach(states)
colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"
str(states)
windows(16,20)
pairs.panels(states, smooth=FALSE, density=TRUE, ellipses=FALSE, method="pearson",pch=21,
             lm=FALSE,cor =TRUE, jiggle=FALSE)
windows(20,12)
par(mfrow=c(3,3))
scatter.smooth(x=states$Population, y=states$Murder, main="Population~Murder")
scatter.smooth(x=states$Income, y=states$Murder, main="Income ~Murder")
scatter.smooth(x=states$Illiteracy, y=states$Murder, main="Illiteracy~Murder")
scatter.smooth(x=states$Life_Exp, y=states$Murder, main="Life_Exp ~Murder")
scatter.smooth(x=states$HS_Grad , y=states$Murder, main="HS_Grad~Murder")
scatter.smooth(x=states$Frost, y=states$Murder, main="Frost ~Murder")
scatter.smooth(x=states$Area, y=states$Murder, main="Area ~Murder")

cor_matrix <- cor(states)
print(cor_matrix)

paste("Correlation for murder and forst:",round(cor(states$Murder,states$Frost),2))
paste("Correlation for murder and population:",round(cor(states$Murder,states$Population),2))
paste("Correlation for murder and income:",round(cor(states$Murder,states$Income),2))
paste("Correlation for murder and illiteracy:",round(cor(states$Murder,states$Illiteracy),2))
paste("Correlation for murder and life_exp:",round(cor(states$Murder,states$Life_Exp),2))
paste("Correlation for murder and hs_grad:",round(cor(states$Murder,states$HS_Grad),2))
paste("Correlation for murder and area:",round(cor(states$Murder,states$Area),2))

windows(16,20)
par(mfrow = c(3, 3))
boxplot(states$Population, main = "Population")
boxplot(states$Income, main = "Income")
boxplot(states$Illiteracy, main = "Illiteracy")
boxplot(states$Life_Exp, main = "Life Expectancy")
boxplot(states$Murder, main = "Murder")
boxplot(states$HS_Grad, main = "HS Graduation")
boxplot(states$Frost, main = "Frost")
boxplot(states$Area, main = "Area")

outliers_population <- boxplot.stats(states$Population)$out
outliers_population
outliers_Income= boxplot.stats(states[["Income"]])$out
outliers_Income
outliers_Area= boxplot.stats(states[["Area"]])$out
outliers_Area
states<- subset(states,
                states$Population!=21998
               & states$Population!=11197
               & states$Population!=18076
               & states$Population!=11860
              & states$Population!=12237)
 
outliers_population2 <- boxplot.stats(states[["Population"]])$out
outliers_population2
states<- subset(states,
                 states$Population!=21198
                 & states$Population!=9111)

outliers_Income= boxplot.stats(states[["Income"]])$out
outliers_Income

states=subset(states,states$Income!=6315)
states

outliers_Area= boxplot.stats(states[["Area"]])$out
outliers_Area
library(e1071)
windows(20,12)
par(mfrow=c(4,3))
plot(density(states$Murder),
     main = "Density plot: Murder",
     ylab = "Frequency", xlab = "Murder",
     sub = paste0("Skewness:", round(e1071::skewness(states$Murder), 2)))
polygon(density(states$Murder), col = "red")
plot(density(states$Population),
     main = "Density plot: Population",
     ylab = "Frequency", xlab = "Population",
     sub = paste0("Skewness:", round(e1071::skewness(states$Population), 2)))
polygon(density(states$Population), col = "red")
plot(density(states$Income),
     main = "Density plot: Income",
     ylab = "Frequency", xlab = "Income",
     sub = paste0("Skewness:", round(e1071::skewness(states$Income), 2)))
polygon(density(states$Income), col = "red")
plot(density(states$Illiteracy),
     main = "Density plot: Illiteracy",
     ylab = "Frequency", xlab = "Illiteracy",
     sub = paste0("Skewness:", round(e1071::skewness(states$Illiteracy), 2)))
polygon(density(states$Illiteracy), col = "red")
plot(density(states$Life_Exp),
     main = "Density plot: Life Exp",
     ylab = "Frequency", xlab = "Life Exp",
     sub = paste0("Skewness:", round(e1071::skewness(states$Life_Exp), 2)))
polygon(density(states$Life_Exp), col = "red")
plot(density(states$HS_Grad),
     main = "Density plot: HS_Grad",
     ylab = "Frequency", xlab = "HS_Grad",
     sub = paste0("Skewness:", round(e1071::skewness(states$HS_Grad), 2)))
polygon(density(states$HS_Grad), col = "red")
plot(density(states$Frost),
     main = "Density plot: Frost",
     ylab = "Frequency", xlab = "Frost",
     sub = paste0("Skewness:", round(e1071::skewness(states$Frost), 2)))
polygon(density(states$Frost), col = "red")
plot(density(states$Area),
     main = "Density plot: Area",
     ylab = "Frequency", xlab = "Area",
     sub = paste0("Skewness:", round(e1071::skewness(states$Area), 2)))
polygon(density(states$Area), col = "blue")
shapiro.test(states$Population)
shapiro.test(states$Income)
shapiro.test(states$Illiteracy)
shapiro.test(states$Life_Exp)
shapiro.test(states$Murder)
shapiro.test(states$HS_Grad)
shapiro.test(states$Frost)
shapiro.test(states$Area)

install.packages("MASS")
windows(20,12)
library(MASS)
attach(states)
box_cox_transform<- boxcox(Murder~Population)
box_cox_transform
lamda <- box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_population<-(Murder^lamda-1)/lamda
normalised_population

hist(normalised_population)
shapiro.test(normalised_population)

states$Population_new <- normalised_population
shapiro.test(states$Population_new)
states$Illiteracy_new <- normalised_population
shapiro.test(states$Illiteracy_new)
states$Income_new <- normalised_population
shapiro.test(states$Income_new)
states$Murder_new <- normalised_population
shapiro.test(states$Murder_new)
states$HS_Grad_new <- normalised_population
shapiro.test(states$HS_Grad_new)
states$Frost_new <- normalised_population
shapiro.test(states$Frost_new)
states$Area_new=normalised_population
shapiro.test(states$Area_new)
str(states)
attach(states)
model_1=lm(Murder~ Population_new + Income + Illiteracy_new + Life_Exp + HS_Grad_new + Area + Frost )
summary(model_1)
