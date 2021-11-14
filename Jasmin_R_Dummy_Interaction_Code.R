#### Dummy and Interaction with mean centered variable Skill Demonstration

setwd("C:/Users/cogps/Desktop/Psyc 204/Skill Share/Dummy Coding_Skill_Demo_2")

# install.packages("haven")
library(haven)

DummyInteraction <- read_sav("CategoricalByContinuousInteractionTest.sav")


## Figuring out the Variable Names and Properties
names(DummyInteraction)
str(DummyInteraction)

table(as.factor(DummyInteraction$Group))
mean(DummyInteraction$Level, na.rm = T)
mean(DummyInteraction$Score, na.rm = T)

## Renaming Variables

names(DummyInteraction) <- c("Participant", "CatDog", "TimeVideoGames", "Happiness")

table(as.factor(DummyInteraction$CatDog))
mean(DummyInteraction$TimeVideoGames, na.rm = T)
mean(DummyInteraction$Happiness, na.rm = T)

## Is this object a data.frame?

is.data.frame(DummyInteraction)
      #yes


## Checking for NA's in R

colSums(is.na(DummyInteraction))
      # no NA's

## Dummy Coding Categorical Variable

DummyInteraction$CatDogDummy <- ifelse(DummyInteraction$CatDog == 'A', 0, 1)

      # A is now equal 0 and all else is coded as 1. 
      # Cats are the reference category

## Mean Centering Continuous Variable

DummyInteraction$TimeVideoGames_Centered <- DummyInteraction$TimeVideoGames - mean(DummyInteraction$TimeVideoGames)


## Multiple Linear Regression without the Mean centered predictor

model_0 <- lm(Happiness~TimeVideoGames + CatDogDummy , data = DummyInteraction)

summary(model_0)
# Create the relationship model.
model <- lm(Happiness~TimeVideoGames_Centered + CatDogDummy , data = DummyInteraction)

# Show the model.
summary(model)
  # R squares .1487

## Model linear regression with interaction 
## https://www.youtube.com/watch?v=h_laZh8B5IU

model_2 <- lm(Happiness~TimeVideoGames_Centered + CatDogDummy + TimeVideoGames_Centered*CatDogDummy , data = DummyInteraction)

# Show the model.
summary(model_2)
  # r squared .1892

## Assumption check of the model 

par(mar=c(5, 4, 3, 3))
plot(model_2)

## R squared Change from initial model to the second

Rsq_Delta <- summary(model_2)$r.squared - summary(model)$r.squared

Rsq_Delta # .04053

## test of change in R squared

anova(model, model_2, test = "F")

    # significant p < .001

## Saving the predicted values for model_2
## https://stackoverflow.com/questions/20907583/how-to-return-predicted-values-residuals-r-square-from-lm

DummyInteraction$Predicted_Outcome<-predict(model_2)

## 3 variable Graph in R
## https://www.youtube.com/watch?v=8Vj71-2PCpY
## https://www.youtube.com/watch?v=1ahg7h5DRP4
## https://www.youtube.com/watch?v=i3FKMtlQCPQ

library(ggplot2)

DummyInteraction$CatDogDummy<-as.factor(DummyInteraction$CatDogDummy) 
  
ggplot(data=DummyInteraction, aes(x = TimeVideoGames_Centered, y= Happiness, colour = CatDogDummy))+
  geom_point()+ geom_smooth(method = 'lm', se = F)+
  scale_color_manual(labels=c("cat", "dog"), values= c("orange", "blue"))


## https://bookdown.org/carillitony/bailey/chp6.html

