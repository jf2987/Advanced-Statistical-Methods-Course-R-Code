## Multicollinearity Skill Demo
## Psyc 204
## Jasmin (pronounced: Hazz-Mean). Fernandez Castillo
## Pronouns: they, them, theirs
## 11/5/21

# https://www.youtube.com/watch?v=I4z3yjoEADY
# http://www.regorz-statistik.de/en/collinearity_diagnostics_table_SPSS.html

setwd("C:/Users/cogps/Desktop/Psyc 204/Skill Share/Multicollinearity")

library(haven)

PerClckThr <- read_sav("PersonalClickThrough.sav")

names(PerClckThr)

# get rid of the first columns, which is ID, and not needed

PerClckThr<-PerClckThr[,-1]

names(PerClckThr)

## get correlations of the all the remaining variables 

round(cor(PerClckThr),2)


## Different Ways of Scaling Variables 
## https://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/

center_scale <- function(x) {
  scale(x, scale = FALSE)
}


## Function Check 

## Creating an Object that Replicates Original Object 
TestRun<-PerClckThr

## Mean Centering the long Way
TestRun$MedianScrollActivity_C <- PerClckThr$MedianScrollActivity - mean(PerClckThr$MedianScrollActivity)

## Resulting Mean of Long Way for this Variable
mean(TestRun$MedianScrollActivity_C)


## Curent Uncentered Mean for the Variable Previous to Function Mean Centering
mean(PerClckThr$MedianScrollActivity)


## Mean Centering with center_Scale()
names(PerClckThr)
PerClckThr[,(2:7)]<-center_scale(PerClckThr[,(2:7)])

## Mean Centered Mean should be identical as replica
mean(PerClckThr$MedianScrollActivity)
mean(TestRun$MedianScrollActivity_C)
        ## it is -- mean centering the short way is succesful

## Running the regression with the Mean centered Variables 
names(PerClckThr)

model_a<- lm(PersonalClickThrough~MedianTimePerPage+ MedianScrollActivity
             + MedianMouseClicks,PerClckThr)
summary(model_a)

## Looking at Tolerance and VIF coefficients 
## https://www.youtube.com/watch?v=2HQxVmrX57M

library(olsrr)
library(faraway)

round(vif(model_a),2)

ols_vif_tol(model_a)

round(ols_eigen_cindex(model_a),2)


## Model including the Personality Questionnaire Variables 

model_b<- lm(PersonalClickThrough~.,PerClckThr)

summary(model_b)

vif(model_b)

ols_vif_tol(model_b)

round(ols_eigen_cindex(model_b),3)

## Checking for R squared change 

Rsq_Delta <- summary(model_a)$r.squared - summary(model_b)$r.squared

Rsq_Delta # -.05

## test of change in R squared

anova(model_a, model_b, test = "F")
      ## significant

# Running a Linear regression with Collinear Variables Excluded  
names(PerClckThr)

PerClckThr<-PerClckThr[,-(2:3)]

names(PerClckThr)


## Rerunning first step of Hierarchical Regression without the Collinear Variables 

model_c<- lm(PersonalClickThrough~ MedianMouseClicks,PerClckThr)
summary(model_c)

## wont tell us much because there is only 1 IV
# vif(model_c)
# ols_vif_tol(model_c)
# round(ols_eigen_cindex(model_c),2)

## Rerunning second step of Hierarchal Regression without the Collinear Variables 

model_d<- lm(PersonalClickThrough~.,PerClckThr)

summary(model_d)

vif(model_d)

ols_vif_tol(model_d)

round(ols_eigen_cindex(model_d),3)


## inspecting R squared change 

Rsq_Delta <- summary(model_c)$r.squared - summary(model_d)$r.squared

Rsq_Delta # -.05

## test of change in R squared

anova(model_c, model_d, test = "F")

    ## significant
