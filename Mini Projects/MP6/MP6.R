#setting the env to the location of the project
setwd("/Users/sahuankit010/Desktop/Repo/CS-6313-Stats/Mini Projects/MP6")
#checks whether it is set or not
getwd()

#loading the data into the cancer_data variable
cancer_data =  read.csv("prostate_cancer.csv")
#printing the data
cancer_data

#installing the corrplot, it provides a visual exploratory tool on correlation matrix that supports automatic variable reordering to help detect hidden patterns among variables.
install.packages("corrplot")

#loading the corrplot library
library(corrplot)

#finding correlation
cor.data = cor(cancer_data)
corrplot(cor.data)

#attaching the data so that we can use the variables
attach(cancer_data)

#plotting with plot, histogram and boxplot for psa.
plot(psa)
hist(psa)
boxplot(psa)

psa.log = log(psa)
plot(psa.log)
boxplot(psa.log)

cancer_data$vesinv = as.factor(cancer_data$vesinv)

#Fitting the linear models

#model 1
fit1 = lm(psa.log ~ cancervol + vesinv + capspen + gleason + weight + age + benpros)
summary(fit1)

#model 2
#reduced model
fit2 = update(fit1,.~. - capspen - age - weight)
summary(fit2)

#model 3
fit3 = update(fit2,.~. + capspen)
summary(fit3)

#analyzing all the three models

anova(fit1)
anova(fit2)
anova(fit3)

anova(fit2,fit3)
anova(fit1,fit2,fit3)
#checking the best model using the AI
fit.full = fit1 = lm(psa.log ~ cancervol + vesinv + capspen + gleason + weight + age + benpros)
for.aic = step(lm(psa.log ~ 1), direction = "forward", scope = formula(fit.full), k = 2, trace = 0) # forward AIC
for.bic = step(lm(psa.log ~ 1), direction = "forward", scope = formula(fit.full), k = log(32), trace = 0) # forward BIC
back.aic = step(fit.full, direction = "backward", k = 2, trace = 0) # backward AIC
back.bic = step(fit.full, direction = "backward", k = log(32), trace = 0) # backward BIC
(Adjusted_R.square = data.frame("Method"=c("for.aic", "for.bic", "back.aic", "back.bic"),"Adj.r.square"=c(summary(for.aic)$adj.r.square,
                                                  summary(for.bic)$adj.r.square,
                                                  summary(back.aic)$adj.r.square, summary(back.bic)$adj.r.square)))


l1 <- glm(fit2)
l2 <- glm(fit1)
l3 <- glm(fit3)
l1$aic
l2$aic
l3$aic

#model evaluation

plot(fitted(fit2),resid(fit2), main = "Residual Plot")
abline(h=0)

qqnorm(resid(fit2))
qqline(resid(fit2))

plot(resid(fit2),type = "l")
abline(h=0)

#summary of fit2
summary(fit2)

#print the table of various variables
table(gleason)
table(vesinv)
mean(cancervol)
mean(benpros)










