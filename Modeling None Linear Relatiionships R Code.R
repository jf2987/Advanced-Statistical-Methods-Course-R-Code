### Non Linear Regressions

## https://medium.com/analytics-vidhya/spline-regression-in-r-960ca82aa62c

## change working directory

setwd("C:/Users/cogps/Desktop/Psyc 204/Skill Share/Modelling Non Linear Relationships_Skill Demo 4")

## read data 

library(haven)

NoneLinear <- read_sav("SkillDemo4_data-SocialMediaAndMood.sav")


## Get Descriptives of the variables

names(NoneLinear)

str(NoneLinear)

range(NoneLinear$PctTime)
range(NoneLinear$Mood)


## Get rid of the first column

NoneLinear<-NoneLinear[,-1]

names(NoneLinear)


## See if there is missing values 

colSums(is.na(NoneLinear))

## is this a data frame?

is.data.frame(NoneLinear)

    ## yes

## Get Correlations

cor(NoneLinear)

## Creating a Mean Center predictor variable PctTime

center_scale <- function(x) {
  scale(x, scale = FALSE)
}

NoneLinear$PctTime_C<-center_scale(NoneLinear$PctTime)

names(NoneLinear)

# Plot Data 

library(ggplot2)


ggplot(NoneLinear, aes(x=PctTime_C, y=Mood))+ geom_point()+ geom_smooth(method = 'lm', se = F)

# fitting a curved line to the model 
ggplot(NoneLinear, aes(x=PctTime_C, y=Mood))+ geom_point()+ geom_smooth(se = F)

# fitting a quadratic line to the model
# https://stats.stackexchange.com/questions/104216/fitting-curved-lines-to-scatterplots-in-r

ggplot(NoneLinear,aes(x=PctTime_C,y=Mood))+      
  stat_smooth(method='glm',family='poisson',
              formula=y~scale(x)+I(scale(x)^2))+geom_point()         

## fitting a cubic line to the model

ggplot(NoneLinear,aes(x=PctTime_C,y=Mood))+      
  stat_smooth(method='glm',family='poisson',
              formula=y~scale(x)+I(scale(x)^3))+geom_point()         

## Linear regression of percent time using social media predicting mood

names(NoneLinear)

model<- lm(Mood~PctTime_C,NoneLinear)
summary(model)

## Creating a Linear Reggression with a Quadratic Polynomial
## https://www.youtube.com/watch?v=ZYN0YD7UfK4&t=185s

model_a<- lm(Mood~PctTime_C+I(PctTime_C^2),NoneLinear)
summary(model_a)

## Creating a Linear regression with a cubic polynomial 

model_b<- lm(Mood~PctTime_C+I(PctTime_C^2)+I(PctTime_C^3),NoneLinear)
summary(model_b)

## See if R squared change is significant with the inclusion of the Quadratic
## polynomial
      ## quadratic vs linear

Rsq_Delta <- summary(model_a)$r.squared - summary(model)$r.squared

Rsq_Delta # -.05

## test of change in R squared

anova(model_a, model, test = "F")
      ## significant


## See if R squared change is significant with the inclusion of the Cubic
## polynomial
## quadratic vs linear

Rsq_Delta <- summary(model_a)$r.squared - summary(model_b)$r.squared

Rsq_Delta # -.05

## test of change in R squared

anova(model_a, model_b, test = "F")
        ## not significant



## Creating groups based on the potential Knot value

NoneLinear$D1 <- with(NoneLinear, ifelse(PctTime_C <= 0 , 0,
                                   ifelse(PctTime_C > 0, 1, NA)))

table(NoneLinear$D1)


## Creating a scatterplot with a line fitted per group 
# https://datavizpyr.com/add-regression-line-per-group-to-scatterplot-in-r/

NoneLinear$D1<-as.factor(NoneLinear$D1)

NoneLinear %>%
  ggplot(aes(x=PctTime_C, 
             y=Mood, 
             color=D1))+
  geom_point()+geom_smooth(method="lm", se=F)
ggsave("scatterplot_with_multiple_groups_ggplot2.png")

## Creating Deviation Score

NoneLinear$Dev1 <-NoneLinear$D1*(NoneLinear$PctTime_C-0)

## Spline Regression

model_c<- lm(Mood~PctTime_C+Dev1,NoneLinear)
summary(model_c)


## See if R squared change is significant with the inclusion of the Deviation
## score from the regular regression ran initially 

Rsq_Delta <- summary(model_c)$r.squared - summary(model)$r.squared

Rsq_Delta 

## test of change in R squared

anova(model, model_c, test = "F")
      ## significant

## check on the residuals 
plot(model_c)
