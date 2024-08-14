#setting working directory 
setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\Phd File\\Phosphate absorption\\Data")


#reading CSV file 
overall<- read.csv("overall_data.csv")

hist(overall)
m1 <- lm(log10(Result+0.001) ~ ph * Treatment * Concentration.mg.P.L. * site, data=overall)
m2 <- lm()
summary(m1)
hist(resid(m1))
install.packages("car")

plot(m1)
Anova(m1)
summary(m1)
install.packages("car")
library(car)
car::qqPlot(m1)
residualPlot(m1)

head(overall)
head(overall,100)
library(tidyverse)
ggplot(overall, aes(x= site)) +
  geom_histogram()




#anova with Treatment
boxplot(Result~Treatment, data=overall)
aov1<-aov(Result~Treatment, data=overall)
summary(aov1)

#anova with addative effect of treatmemnt and ph 
aov2<-aov(Result~ph+Treatment, data=overall)
summary(aov2)

#anova with interaction effect of treatmemnt and ph 
aov3<-aov(Result~ph*Treatment, data=overall)
summary(aov3)

head(data)
install.packages("ggplot2")
library(ggplot2)



#anova with site 
boxplot(Result~site, data=overall)
aov4<-aov(Result~site, data=overall)
summary(aov4)

#anova with site and Treatment 
aov5<-aov(Result~site+Treatment, data=overall)
summary(aov5)

#anova with site and Treatment and PH
aov6<-aov(Result~site+Treatment+ph, data=overall)
summary(aov6)

boxplot(Result~Concentration.mg.P.L., data=overall)
#anova with site and Treatment, PH and concentratipon 
aov7<-aov(Result~site+Treatment+ph+Concentration.mg.P.L., data=overall)
summary(aov7)

#with(overall, boxplot(Result~ph, col=as.factor(ph)))


#filtering data c1

C1<-  overall%>%  filter(Concentration.mg.P.L.==1 | Concentration.mg.P.L.==0)

aov8<-aov(Result~site+Treatment+ph+Concentration.mg.P.L., data=C1)
summary(aov8)


ggplot(data = C1, aes(x = site, y = Result, fill = Treatment))+geom_boxplot()

ggplot(data = C1, aes(x = site), y = Result, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Result - sd, ymax = Result + sd), width = 0.2,
                position = position_dodge(.9)) +
  xlab("Sample") +
  ylab("Phosphate absorbed")



#filtering data c10 
C10<-  overall%>%  filter(Concentration.mg.P.L.==10 |Concentration.mg.P.L.==0)

aov9<-aov(Result~site+Treatment+ph+Concentration.mg.P.L., data=C10)
summary(aov9)

ggplot(data = C10, aes(x = site, y = Result, fill = Treatment))+geom_boxplot()


#filtering data c10, c1 and c0 
C10<-  overall%>%  filter(Concentration.mg.P.L.==10 |Concentration.mg.P.L.==0 |Concentration.mg.P.L.==0)

aov9<-aov(Result~site+Treatment+ph+Concentration.mg.P.L., data=C10,)
summary(aov9)

ggplot(data = C10, aes(x = site, y = Result, fill = Treatment))+geom_boxplot()

