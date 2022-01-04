## MLM Example Constrcted by Jasmin
## Based on this vid
# https://www.youtube.com/watch?v=1rTb72pCPT8

## Read Data 

setwd("C:/Users/cogps/Desktop/Psyc 204/Skill Share/MultiLevel Analysis")
library(haven)

SD<-read_sav("SkillDemo7-data-Delinquency.sav")

#Factor Grouping Variable 
names(SD)
levels(as.factor(SD$School))

SD$School<-as.factor(SD$School)

levels(SD$School)

table(SD$School)

# Exploring the data

summary(SD) # gives us basic descriptives such as Min, median, Max etc.
# it also gives us number of NA's if there are any. 

#     # delete unecessary columns 1 and 3:5

library(dplyr)

names(SD)
SD<-SD %>% dplyr::select(-1, -(4:6))
names(SD)

# checking for outliers for none categorical variables
names(SD)
mahal <-mahalanobis(SD[, -c(1)], colMeans(SD[, -c(1)], na.rm = TRUE), 
                    cov(SD[, -c(1)], use = "pairwise.complete.obs"))

summary(mahal)

# figuring out the cut off score for the Mahal Centroid scores
cutoff<-qchisq(1-.001, ncol(SD[, -c(1)]))
cutoff
# this is 18.47

# figuring out how many people are above the cutoff

summary(mahal> cutoff)
    # there are 3 people that are above the cut off 

# Keep all rows of people who do not go beyond the cuttoff
SD<-SD[mahal< cutoff, ]
# we go from 3000 observations to 2997
# Check for collinear hints

cor(SD[, -c(1)])

    # i mean, the correlations are pretty acceptable - there is no 
# indication of collinearity from the correlations themselves


# checking assumptions through means of a regression 

random <- rchisq(nrow(SD), 7) 
fake <- lm(random~., data = SD)
fitted<-scale(fake$fitted.values)
standardized<-rstudent(fake)

# linearity 
# a qq norm plot of the standardized residuals 
qqnorm(standardized)
abline(0,1)
        # it seems so far that there is a violation of 
# linearity? or normality?


# normality - histogram

hist(standardized)
        # definitely not normal -- rightly skewed 
# checking for skewness and Kurtosis with moments lib
# install.packages("moments")
library(moments)
# skewness 
skewness(standardized)
# 1.0453
#calculate kurtosis
kurtosis(standardized)
# 4.6369
      # kurtotic distribution
jarque.test(standardized)
      # significantly kurtotic and skewed in comparison to 
# the normal distribution

# look for homogeneity of variance 
plot(fitted, standardized)
abline(0,0)
abline(v=0)
      # i am unsure how to interpret this graph
      # but since the data seems evenly spread i am assuming
      # its homogenous?

# running the MLM 


## intercept only model 
names(SD)
#install.packages("nlme")
library(nlme)
names(SD)
model1<-gls(Delinquency~1, data=SD, 
            method = "REML", na.action="na.omit")

summary(model1)

#Now we are switching to LME function
# adding a random intercept based on School Variable 

model2<-lme(Delinquency~1, data=SD, 
            method = "REML", na.action="na.omit", 
            random = ~1|School)

summary(model2)

# did letting the intercept be random improve the model fit?
anova(model1, model2)
    # it did make it sig. 
    


# now we add first level fixed predictors to the model 

names(SD)
model3<-lme(Delinquency~cCPI_Responsibility+cCPI_Socialization, 
            data=SD, 
            method = "REML", na.action="na.omit", 
            random = ~1|School)

summary(model3)

# try to compare the model fit and get this warning.
anova(model2, model3)
# Warning message:
#   In anova.lme(model2, model3) :
#   fitted objects with different fixed effects. REML comparisons are not meaningful.


# now we add the second level fixed predictor to the model 
names(SD)
model4<-lme(Delinquency~cCPI_Responsibility+cCPI_Socialization+cAvgCrimeSchoolZone, 
            data=SD, 
            method = "REML", na.action="na.omit", 
            random = ~1|School)

summary(model4)

anova(model3, model4)
      # same warning 



## model 5, including random slopes for Responsibility

model5<-lme(Delinquency~cCPI_Responsibility+cCPI_Socialization+cAvgCrimeSchoolZone, 
            data=SD, 
            method = "REML", na.action="na.omit", 
            random = ~1+cCPI_Responsibility|School,
            control = lmeControl(msMaxIter = 200))

summary(model5)


## model 6, including random slopes for Socialization

model6<-lme(Delinquency~cCPI_Responsibility+cCPI_Socialization+cAvgCrimeSchoolZone, 
            data=SD, 
            method = "REML", na.action="na.omit", 
            random = ~1+cCPI_Socialization|School,
            control = lmeControl(msMaxIter = 200))

summary(model6)


# MOdel 7, cross level interaction check for responsibility

model7<-lme(Delinquency~cCPI_Responsibility+cCPI_Socialization+cAvgCrimeSchoolZone+
              cCPI_Responsibility*cAvgCrimeSchoolZone, 
            data=SD, 
            method = "REML", na.action="na.omit", 
            random = ~1+cCPI_Responsibility|School,
            control = lmeControl(msMaxIter = 200))

summary(model7)

# Graphing the interaction by saving the predicted values 
# and splitting the second level variables into 3 Quantiles 

# saving predicted values 
SD$Pred_7  <- predict(model7)

View(SD$Pred_7)

# splitting temporarily the second level variable as categorical
# https://statisticsglobe.com/quantile-function-in-r-example

# Quantiles_3<-quantile(SD$cAvgCrimeSchoolZone, probs = seq(0, 1, 1/3)) 
# 
# 
# SDC<-SD
# SDC$quantile <- with(SDC, cut(SDC, 
#                                 breaks=quantile(SDC$cAvgCrimeSchoolZone, probs = seq(0, 1, 1/3), na.rm = TRUE), 
#                                 include.lowest=TRUE))

# https://rdrr.io/cran/gtools/man/quantcut.html

# install.packages("gtools")

library (gtools)

SD$quantile<-quantcut(SD$cAvgCrimeSchoolZone, q = 3, na.rm = TRUE)
table(as.factor(SD$quantile))

## Doing the scatterplot
## https://r-coder.com/scatter-plot-r/
#adjust plot margins
par(mar = c(1, 1, 1, 1))
plot(y=SD$Pred_7, x=SD$cCPI_Responsibility, pch = as.numeric(SD$quantile), col = SD$quantile)

library(ggplot2)

SD$quantile<-as.factor(SD$quantile)
levels(SD$quantile)

ggplot(data=SD, aes(x = cCPI_Responsibility, y= Pred_7, colour = quantile))+
  geom_point()+ geom_smooth(method = 'lm', se = F)+
  scale_color_manual(labels=c("[-2.21,-0.741]", "(-0.741,0.869]", "(0.869,1.57]"), 
                     values= c("red", "blue", "green"))



# Model 8 Test the interaction with Socialization 

model8<-lme(Delinquency~cCPI_Responsibility+cCPI_Socialization+cAvgCrimeSchoolZone+
              cCPI_Socialization*cAvgCrimeSchoolZone, 
            data=SD, 
            method = "REML", na.action="na.omit", 
            random = ~1+cCPI_Socialization|School,
            control = lmeControl(msMaxIter = 200))

summary(model8)


# Save its predicted values for plot 

SD$Pred_8  <- predict(model8)

View(SD$Pred_8)

# plot the interaction 

par(mar = c(1, 1, 1, 1))
plot(y=SD$Pred_8, x=SD$cCPI_Socialization, pch = as.numeric(SD$quantile), col = SD$quantile)

library(ggplot2)

ggplot(data=SD, aes(x = cCPI_Socialization, y= Pred_8, colour = quantile))+
  geom_point()+ geom_smooth(method = 'lm', se = F)+
  scale_color_manual(labels=c("[-2.21,-0.741]", "(-0.741,0.869]", "(0.869,1.57]"), 
                     values= c("red", "blue", "green"))



##### Code Below is For Future TroubleShooting #### 

## Figuring out how to change the Covariance Structure 
# I get an error for this one
# 
# Error in solve.default(estimates[dimE[1] - (p:1), dimE[2] - (p:1), drop = FALSE]) : 
#   system is computationally singular: reciprocal condition number = 1.41212e-16

# attempt to trouble shoot
# https://stackoverflow.com/questions/42511770/lme-error-in-solve-defaultestimatesdime1-p1-dime2-p1-drop
# https://stackoverflow.com/questions/26408700/how-can-i-specify-unstructured-in-lme-function
# https://stackoverflow.com/questions/69712324/specifying-the-identity-variance-covariance-matrix-for-nlmelme-in-r



# 
mahal <-mahalanobis(SD[, -c(1)], colMeans(SD[, -c(1)], na.rm = TRUE), 
                    cov(SD[, -c(1)], use = "pairwise.complete.obs"))

# i get this error
# Error in solve.default(cov, ...) : 
#   Lapack routine dgesv: system is exactly singular: U[4,4] = 0
# https://statisticsglobe.com/r-error-in-solve-system-is-exactly-singular
## apparently this errors is indicative of collinearity in the matrix

# https://stackoverflow.com/questions/22134398/mahalonobis-distance-in-r-error-system-is-computationally-singular
mahal <-mahalanobis(SD[, -c(1)], colMeans(SD[, -c(1)]), 
                    cov(SD[, -c(1)]))
# same error
