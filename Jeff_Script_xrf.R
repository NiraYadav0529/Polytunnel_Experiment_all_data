## load libraries
library(tidyverse)
library(car)
library(emmeans)
library(ggplot2)
setwd("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File")

xrf<- read.csv("Pot_ExP_Data_For_ph_ec_soil.csv")
head(xrf)

# Check the scope of 'Plant.type' variable within ggplot
# Make sure it's included in the aes() function
# and that it's correctly spelled and formatted
#ph and EC
ggplot(xrf, aes(x = Dose, y = apH_beforefertilization, colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type)

ggplot(xrf, aes(x = Dose, y = apH_First.7th , colour = Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type)
# Fit the model after ensuring the 'species' variable is present
m1 <- lm(P_.ppm. ~ Dose * Plant.type , data = xrf)

#ph and EC
ggplot(xrf, aes(x = Dose, colour = Dose)) + 
  geom_boxplot(aes(y = apH_beforefertilization), position = position_dodge(width = 0.75)) +
  geom_boxplot(aes(y = apH_First.7th), position = position_dodge(width = 0.75)) +
  facet_wrap(~Plant.type)

ggplot(xrf, aes(x = Dose)) + 
  geom_boxplot(aes(y = apH_beforefertilization, fill = "Before Fertilization"), position = position_dodge(width = 0.75)) +
  geom_boxplot(aes(y = apH_First.7th, fill = "First 7th"), position = position_dodge(width = 0.75)) +
  facet_wrap(~Plant.type) +
  scale_fill_manual(values = c("Before Fertilization" = "blue", "First 7th" = "green")) +
  theme(legend.position = "bottom")

m1 <- lm(P_.ppm. ~ Dose * Plant.type + apH_beforefertilization + apH_First.7th, data = xrf)
summary(m1)

library(ggplot2)
#Ph for second fertilization before and after 

ggplot(xrf, aes(x = Dose, colour = Dose)) + 
  geom_boxplot(aes(y = epH_before_Secondfertilization), position = position_dodge(width = 0.75)) +
  geom_boxplot(aes(y = pH.Harvest.day), position = position_dodge(width = 0.75)) +
  facet_wrap(~Plant.type)

ggplot(xrf, aes(x = Dose)) + 
  geom_boxplot(aes(y = epH_before_Secondfertilization, fill = "Before second Fertilization"), position = position_dodge(width = 0.75)) +
  geom_boxplot(aes(y = pH.Harvest.day, fill = "Harvest Day"), position = position_dodge(width = 0.75)) +
  facet_wrap(~Plant.type) +
  scale_fill_manual(values = c("Before second Fertilization" = "blue", "pH.Harvest.day" = "green")) +
  theme(legend.position = "bottom")
 

# Load necessary packages
library(car)
install.packages("emmeans")
library(emmeans)
# This will also load the 'lmerTest' package which is required by 'emmeans'
install.packages("multcomp") 
# Install if not installed
library(multcomp)

# Plot residuals
residualPlot(m1)

# QQ plot
qqPlot(m1)
Anova(m1)
# Test significance
Anova(m1)

# Comparing means by dose within species
m1.emm <- emmeans(m1, ~ Dose |Plant.type)
multcomp::cld(m1.emm)

residualPlot(m1)
qqPlot(m1)
# test significance
Anova(m1)
# comparing means by dose within species (ignores sample.identity)
m1.emm <- emmeans(m1, ~ Dose | species)
multcomp::cld(m1.emm)

par(mfrow=c(1,2))
plot(m1)
residualPlot(m1)
qqPlot(m1)
aov1<- aov(P_.ppm. ~ Dose * Plant.type , data = xrf)
summary(aov1)
TukeyHSD(aov1)
## plot P data by species, tissue and treatment
ggplot(xrf, aes(x=sample.identity, y=P_.ppm., colour=Dose)) + 
  geom_boxplot() + 
  facet_wrap(~species)

## fit model and compare treatments
# fit model
m1 <- lm(P_.ppm. ~ Dose * species * sample.identity, data=xrf)
# check residuals to see if transformation needed
residualPlot(m1)
qqPlot(m1)
# test significance
Anova(m1)
# comparing means by dose within species (ignores sample.identity)
m1.emm <- emmeans(m1, ~ Dose | species)
multcomp::cld(m1.emm)

