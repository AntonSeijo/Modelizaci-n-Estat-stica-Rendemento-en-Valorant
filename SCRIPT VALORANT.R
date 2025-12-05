#-----------------------------------------------------------------#
#-------------------- ANÁLISE MODELO VALORANT --------------------#
#-----------------------------------------------------------------#

#-----------------------------------------------------------#
#---------------- MODELO REGRESIÓN MULTIPLE ----------------#
#-----------------------------------------------------------#

datos=read.table("players_stats_valorant.txt",header=TRUE,sep=",")
datos
head(datos)
x1=datos$Kpr
x1
x2=datos$Dpr
x2
x3=datos$HsPerc
x3
x4=datos$Tier
x4
y=datos$Rating
y
plot(y~x1)
plot(y~x2)
plot(y~x3)
modelo=lm(y~x1+x2+x3)
summary(modelo)
step(modelo) #All variables are temporarily retained

#--------------------------------------------------------#
#------------------ DIAGNOSE DO MODELO ------------------#
#--------------------------------------------------------#

#Scatter plot of the adjusted values and fitted line
windows()
plot(y~fitted(modelo),xlab="Valores axustados")
abline(lm(y~fitted(modelo)))

#---- 1) COLINEARIDADE ----#
z=cbind(x1,x2,x3)
colnames(z)=c("KPR","DPR","HsPerc")
round(cor(z),3) #Correlation matrix between explanatory variables
#No strong correlation is observed between them => no variables are removed.

#---- 2) APALANCAMENTOS ----#

#Define the hat matrix
model.matrix(modelo) 
hat(model.matrix(modelo))
p=4
n=length(y); n
hat(model.matrix(modelo))>2*p/n
which(hat(model.matrix(modelo))>2*p/n) #leverages h_ii
hat(model.matrix(modelo))[which(hat(model.matrix(modelo))>2*p/n)]

#Plot
#Blue points are leverage observations

plot(y~x1)
points(x1[which(hat(model.matrix(modelo))>2*p/n)],
	 y[which(hat(model.matrix(modelo))>2*p/n)],
	 col="blue",pch=19)
plot(y~x2)
points(x2[which(hat(model.matrix(modelo))>2*p/n)],
	 y[which(hat(model.matrix(modelo))>2*p/n)],
	 col="blue",pch=19)
plot(y~x3)
points(x3[which(hat(model.matrix(modelo))>2*p/n)],
	 y[which(hat(model.matrix(modelo))>2*p/n)],
	 col="blue",pch=19)

#---- 3) ATÍPICOS ----#

res1=modelo$residuals #raw residuals
res2=rstandard(modelo) #standardized residuals
res3=rstudent(modelo) #studentized residuals

abs(res2)>2
which(abs(res2)>2)
res2[which(abs(res2)>2)] #Outliers

#Plot
#Outliers are represented in orange

plot(y~x1)
points(x1[which(abs(res2)>2)],
	 y[which(abs(res2)>2)],
	 pch=19, col="orange")
plot(y~x2)
points(x2[which(abs(res2)>2)],
	 y[which(abs(res2)>2)],
	 pch=19, col="orange")
plot(y~x3)
points(x3[which(abs(res2)>2)],
	 y[which(abs(res2)>2)],
	 pch=19, col="orange")

#---- 4) INFLUENTES ----#

cooks.distance(modelo)
which(cooks.distance(modelo)>0.5) #influential points with Cook's distance greater than 0.5
which(cooks.distance(modelo)>1)   #influential points with Cook's distance greater than 1

#There are no observations with Cook's distance greater than 0.5 or 1.
#This means the model does not present influential data.

#----------------------------------------------------------#
#------------------ VALIDACIÓN DO MODELO ------------------#
#----------------------------------------------------------#

#First, install several libraries that will be used throughout the script.

install.packages("car")
install.packages("lmtest") 
install.packages("sm")
library(lmtest)
library(sm)

#---- MODELO INICIAL ----#

#The initial model will not be used. The model we continue with will be
#without outliers. The explanation for this is in the PDF. Anyway, we run
#several tests with all the data to check the assumptions of the multiple
#linear regression model and see how it behaves with all observations.

windows()
par(mfrow=c(2,2))
plot(modelo)

#---- a) Normalidade ----#
#H0: Residuals are normal; Ha: Residuals are not normal
shapiro.test(res2)

#---- b) Homocedasticidade ----#
#H0: The model is homoscedastic; Ha: The model is heteroscedastic
bptest(modelo)
hmctest(modelo)

#---- c) Linearidade ----#
#RESET: H0: model is well specified and linear; Ha: misspecified, non-linear
#Harvey-Collier: H0: linear relationship; Ha: non-linear relationship
#sm.regression: H0: linear model; Ha: nonparametric relationship
reset(modelo)
harvtest(modelo)
sm.regression(modelo$fitted.values[1:200],y[1:200], model="linear")
 

#----------- MODELO.SA || ELIMÍNANSE OS ATÍPICOS -----------#

#This will improve the significance of x3 and of the model in general.

#Create the model without outliers
indices_out=which(abs(res2)>2)
length(indices_out)
datos_new = datos[-indices_out, ]
x1_new=datos_new$Kpr
x1_new
x2_new=datos_new$Dpr
x2_new
x3_new=datos_new$HsPerc
x3_new
x4_new=datos_new$Tier
x4_new
y_new=datos_new$Rating
y_new
modeloSA = lm(y_new~x1_new+x2_new+x3_new) #SA: without outliers
summary(modeloSA)
step(modeloSA) #All variables are retained

windows()
par(mfrow=c(2,2))
plot(modeloSA)

#---- a) Normalidade ----#
#H0: Residuals are normal; Ha: Residuals are not normal
shapiro.test(rstandard(modeloSA))
#It is expected to reject normality because there are many data points
#and the test is sensitive. However, residuals are approximately normal,
#as can be seen in the QQ plot.
hist(rstandard(modeloSA)) #Distribution of residuals in a histogram

#---- b) Homocedasticidade ----#
#H0: The model is homoscedastic; Ha: The model is heteroscedastic
bptest(modeloSA)
hmctest(modeloSA)
#Homoscedasticity is reasonable; H0 is not rejected.

#---- c) Linearidade ----#
#RESET: H0: model is well specified and linear; Ha: misspecified, non-linear
#Harvey-Collier: H0: linear relationship; Ha: non-linear relationship
#sm.regression: H0: linear model; Ha: nonparametric relationship

reset(modeloSA) 
harvtest(modeloSA) 
windows()
sm.regression(modeloSA$fitted.values[1:200],y_new[1:200], model="linear")
#The null hypothesis H0 is rejected.

#Next, we study where the problem behind this rejection comes from.

#------- ESTUDO DO PROBLEMA DA NON LINEARIDADE DAS COLAS -------#

#Look at data with rating lower than 0.5, which is approximately where
#model linearity fails, as seen in the sm.regression plot.

ind = which(y_new<0.5)
ind

indices_in=which(y_new<0.5)
datosF = datos_new[indices_in, ] #problematic data (tail)
datosF
x1_F=datosF$Kpr
x1_F
x2_F=datosF$Dpr
x2_F
x3_F=datosF$HsPerc
x3_F
x4_F=datosF$Tier
x4_F
y_F=datosF$Rating
y_F

modeloF = lm(y_F~x1_F+x2_F+x3_F)
summary(modeloF)
step(modeloF) 
#The variable x3 would be removed from the model due to lack of
#significance when only tail values are considered.

datosN = datos_new[-indices_in, ] #Remove those data
datosN
x1_N=datosN$Kpr
x1_N
x2_N=datosN$Dpr
x2_N
x3_N=datosN$HsPerc
x3_N
x4_N=datosN$Tier
x4_N
y_N=datosN$Rating
y_N
modeloN = lm(y_N~x1_N+x2_N+x3_N)
summary(modeloN)

sm.regression(modeloN$fitted.values[1:200],y_N[1:200], model="linear")
#The p-value p = 0.733 is very high, H0 is not rejected and
#it is confirmed that the tails were causing the lack of linearity.


#------------------- MODELO CON TERMO CUADRÁTICO -------------------#

#Although the linearity problem was in the tail, we chose not to remove
#those values since they are real data, and instead we look for a model
#with a quadratic term that fits better.

#Add the term x1^2 to the model; the reason is explained in the PDF.
x5_new=x1_new*x1_new
modelo1 = lm(y_new~x1_new+x2_new+x3_new+x5_new)
summary(modelo1)

z=cbind(x1_new,x2_new,x3_new,x5_new)
colnames(z)=c("KPR","DPR","HsPerc","KPR^2")
round(cor(z),3) #It is normal to have collinearity between x1 and x1^2 
step(modelo1) #No variable is removed

windows()
par(mfrow=c(2,2))
plot(modelo1)

#---- a) Normalidade ----#
shapiro.test(rstandard(modelo1)) 
#Same situation as in the previous model: H0 is rejected due to the
#number of observations, but residuals are approximately normal.
#Check the QQ plot.
hist(rstandard(modelo1))

#---- b) Homocedasticidade ----#
bptest(modelo1) #This rejects H0 because of its high sensitivity
hmctest(modelo1) #This does not reject H0

#We can state that the model is homoscedastic.

#---- c) Linearidade ----#
reset(modelo1) #Linearity is NOT rejected
harvtest(modelo1) #Linearity is rejected

sm.regression(modelo1$fitted.values[1:200],y_new[1:200], model="linear")
#Linearity is accepted.

#The residual pattern in the Rating vs Fitted plot improves and
#linearity is achieved. We will use this model for the ANOVA / ANCOVA.

#-------------------------------------------------------#
#------------------------ ANOVA ------------------------#
#-------------------------------------------------------#

#To perform ANOVA, use Tier as a factor.

head(datos_new)
t=datos_new$Tier
t=factor(t)
t=relevel(t,ref="S"); t

#----------- RATING ~ TIERS -----------#
#Within each group (Tier), data should be approximately normal with
#stable variance; these are the assumptions before doing ANOVA.
#We check normality and also remove outliers detected by the boxplot.

#Normality
shapiro.test(y_new[t=="S"])
qqnorm(y_new[t == "S"]); qqline(y_new[t == "S"])
shapiro.test(y_new[t=="A"])
qqnorm(y_new[t == "A"]); qqline(y_new[t == "A"])
shapiro.test(y_new[t=="B"])
qqnorm(y_new[t == "B"]); qqline(y_new[t == "B"])
shapiro.test(y_new[t=="C"])
qqnorm(y_new[t == "C"]); qqline(y_new[t == "C"])
#QQ plots by Tier show that the distribution is reasonably normal
#to apply ANOVA.

#Remove outliers
windows()
boxplot(y_new~t)

outliers = c()
for (k in levels(t)) {
  grupo_idx = which(t == k)
  grupo = y_new[grupo_idx]
  
  Q1 = quantile(grupo, 0.25)
  Q3 = quantile(grupo, 0.75)
  IQR = Q3 - Q1
  
  lower = Q1 - 1.5*IQR
  upper = Q3 + 1.5*IQR

  idx_out = grupo_idx[grupo < lower | grupo > upper]
  
  outliers = c(outliers, idx_out)
}
outliers
datos_new_anova1 = datos_new[-outliers, ]

x1_new1=datos_new_anova1$Kpr
x2_new1=datos_new_anova1$Dpr
x3_new1=datos_new_anova1$HsPerc
x4_new1=datos_new_anova1$Tier
y_new1=datos_new_anova1$Rating

t1=datos_new_anova1$Tier
t1=factor(t1)
t1=relevel(t1,ref="S"); t1

anova1_SA = aov(y_new1 ~ t1)
summary(anova1_SA) #NO rating differences between tiers

rat_local<-numeric(4)
rat_local[1]<-mean(y_new1[t1=="S"]) 
rat_local[2]<-mean(y_new1[t1=="A"])
rat_local[3]<-mean(y_new1[t1=="B"])
rat_local[4]<-mean(y_new1[t1=="C"])
rat_local

#We can see that Tier C has slightly lower rating on average,
#but it is NOT significant enough to reject the F-test.


#----------- KPR ~ TIERS -----------#

#Normality
shapiro.test(x1_new[t=="S"])
qqnorm(x1_new[t == "S"]); qqline(x1_new[t == "S"])
shapiro.test(x1_new[t=="A"])
qqnorm(x1_new[t == "A"]); qqline(x1_new[t == "A"])
shapiro.test(x1_new[t=="B"])
qqnorm(x1_new[t == "B"]); qqline(x1_new[t == "B"])
shapiro.test(x1_new[t=="C"])
qqnorm(x1_new[t == "C"]); qqline(x1_new[t == "C"])

#QQ plots by Tier show that the distribution is reasonably normal
#to apply ANOVA.

#Remove outliers
windows()
boxplot(x1_new~t)

outliers2 = c()
for (k in levels(t)) {
  grupo_idx = which(t == k)
  grupo = x1_new[grupo_idx]
  
  Q1 = quantile(grupo, 0.25)
  Q3 = quantile(grupo, 0.75)
  IQR = Q3 - Q1
  
  lower = Q1 - 1.5*IQR
  upper = Q3 + 1.5*IQR

  idx_out = grupo_idx[grupo < lower | grupo > upper]
  
  outliers2 = c(outliers2, idx_out)
}
outliers2
datos_new_anova2 = datos_new[-outliers2, ]

x1_new2=datos_new_anova2$Kpr
x2_new2=datos_new_anova2$Dpr
x3_new2=datos_new_anova2$HsPerc
x4_new2=datos_new_anova2$Tier
y_new2=datos_new_anova2$Rating

t2=datos_new_anova2$Tier
t2=factor(t2)
t2=relevel(t2,ref="S"); t2

anova2_SA = aov(x1_new2 ~ t2)
summary(anova2_SA) #There are KPR differences between tiers
TukeyHSD(anova2_SA) #Tier C players have more kills

kpr_local<-numeric(4)
kpr_local[1]<-mean(x1_new2[t2=="S"]) 
kpr_local[2]<-mean(x1_new2[t2=="A"])
kpr_local[3]<-mean(x1_new2[t2=="B"])
kpr_local[4]<-mean(x1_new2[t2=="C"])
kpr_local


#-----------------DPR ~ TIERS-------------

#Normality
shapiro.test(x2_new[t=="S"])
qqnorm(x2_new[t == "S"]); qqline(x2_new[t == "S"])
shapiro.test(x2_new[t=="A"])
qqnorm(x2_new[t == "A"]); qqline(x2_new[t == "A"])
shapiro.test(x2_new[t=="B"])
qqnorm(x2_new[t == "B"]); qqline(x2_new[t == "B"])
shapiro.test(x2_new[t=="C"])
qqnorm(x2_new[t == "C"]); qqline(x2_new[t == "C"])

#QQ plots by Tier show that the distribution is reasonably normal
#to apply ANOVA.

#Remove outliers
windows()
boxplot(x2_new~t)

outliers3 = c()
for (k in levels(t)) {
  grupo_idx = which(t == k)
  grupo = x2_new[grupo_idx]
  
  Q1 = quantile(grupo, 0.25)
  Q3 = quantile(grupo, 0.75)
  IQR = Q3 - Q1
  
  lower = Q1 - 1.5*IQR
  upper = Q3 + 1.5*IQR

  idx_out = grupo_idx[grupo < lower | grupo > upper]
  
  outliers3 = c(outliers3, idx_out)
}
outliers3
datos_new_anova3 = datos_new[-outliers3, ]

x1_new3=datos_new_anova3$Kpr
x2_new3=datos_new_anova3$Dpr
x3_new3=datos_new_anova3$HsPerc
x4_new3=datos_new_anova3$Tier
y_new3=datos_new_anova3$Rating

t3=datos_new_anova3$Tier
t3=factor(t3)
t3=relevel(t3,ref="S"); t3

anova3_SA = aov(x2_new3 ~ t3)
summary(anova3_SA) #There are DPR differences between tiers
TukeyHSD(anova3_SA) 

#Tier C players die more, which theoretically makes them worse players.
#This pattern corresponds to a very aggressive and not very efficient
#playstyle. Players get some eliminations but expose themselves too much,
#lose duels and die more frequently.

dpr_local<-numeric(4)
dpr_local[1]<-mean(x2_new3[t3=="S"]) 
dpr_local[2]<-mean(x2_new3[t3=="A"])
dpr_local[3]<-mean(x2_new3[t3=="B"])
dpr_local[4]<-mean(x2_new3[t3=="C"])
dpr_local


#------------ %HEADSHOT ~ TIERS----------

#Normality
windows()
shapiro.test(x3_new[t=="S"])
qqnorm(x3_new[t == "S"]); qqline(x3_new[t == "S"])
shapiro.test(x3_new[t=="A"])
qqnorm(x3_new[t == "A"]); qqline(x3_new[t == "A"])
shapiro.test(x3_new[t=="B"])
qqnorm(x3_new[t == "B"]); qqline(x3_new[t == "B"])
shapiro.test(x3_new[t=="C"])
qqnorm(x3_new[t == "C"]); qqline(x3_new[t == "C"])

#Remove outliers
windows()
boxplot(x3_new~t)

outliers4 = c()
for (k in levels(t)) {
  grupo_idx = which(t == k)
  grupo = x3_new[grupo_idx]
  
  Q1 = quantile(grupo, 0.25)
  Q3 = quantile(grupo, 0.75)
  IQR = Q3 - Q1
  
  lower = Q1 - 1.5*IQR
  upper = Q3 + 1.5*IQR

  idx_out = grupo_idx[grupo < lower | grupo > upper]
  
  outliers4 = c(outliers4, idx_out)
}
outliers4
datos_new_anova4 = datos_new[-outliers4, ]

x1_new4=datos_new_anova4$Kpr
x2_new4=datos_new_anova4$Dpr
x3_new4=datos_new_anova4$HsPerc
x4_new4=datos_new_anova4$Tier
y_new4=datos_new_anova4$Rating

t4=datos_new_anova4$Tier
t4=factor(t4)
t4=relevel(t4,ref="S"); t4

anova4_SA = aov(x3_new4 ~ t4)
summary(anova4_SA) 

#There are no headshot percentage differences between tiers.

heads_local<-numeric(4)
heads_local[1]<-mean(x3_new4[t4=="S"]) 
heads_local[2]<-mean(x3_new4[t4=="A"])
heads_local[3]<-mean(x3_new4[t4=="B"])
heads_local[4]<-mean(x3_new4[t4=="C"])
heads_local


#--------------------------------------------------------#
#------------------------ ANCOVA ------------------------#
#--------------------------------------------------------#

#----- INFLUENCIA CATEGÓRICA MODELO MULTIPLE -----#
#Does the categorical variable (TIER) influence the adjusted multiple model?

#Multiple model
modelo1 = lm(y_new~x1_new+x2_new+x3_new+x5_new)
summary(modelo1)

#ANCOVA model
mod.ancova1=lm(y_new~x1_new+x2_new+x3_new+x5_new+t)
summary(mod.ancova1)

#H0: Tier does NOT contribute to the model
#H1: Tier DOES contribute once the continuous covariates are adjusted for
anova(modelo1,mod.ancova1)
#Tier IS significant for the multiple model.


#What happens in the model explained only by KPR and DPR?

modelo2=lm(y_new~x1_new+x2_new)
summary(modelo2)

mod.ancova2=lm(y_new~x1_new+x2_new+t)
summary(mod.ancova2)

anova(modelo2,mod.ancova2)

#Tier IS significant in the model explained only by KPR and DPR.


#What happens in the model explained only by %HS?

modelo3=lm(y_new~x3_new)
summary(modelo3)

mod.ancova3=lm(y_new~x3_new+t)
summary(mod.ancova3)

anova(modelo3,mod.ancova3)
#Tier is NOT significant in the model explained only by %HS.

#---- INTERACCIÓN VARIABLE CONTINUA * DISCRETA ----#

#--- Does any Tier "convert Kills into Rating" more efficiently than another? ---#

mod.ancova1=lm(y_new~x1_new+x2_new+x3_new+x5_new+t)
mod.int1=lm(y_new~x1_new*t+x2_new+x3_new+x5_new)
summary(mod.int1)

anova(mod.ancova1,mod.int1)
#There is no interaction, indicating that the relationship between
#Rating and KPR is similar across all Tiers.

#Slopes are parallel.
#Classical ANCOVA is valid.


#--- Does any Tier "convert Deaths into Rating" more efficiently than another? ---#
 
mod.ancova1=lm(y_new~x1_new+x2_new+x3_new+x5_new+t)
mod.int2=lm(y_new~x1_new+x2_new*t+x3_new+x5_new)
summary(mod.int2)

anova(mod.ancova1,mod.int2)
#There IS interaction, indicating that the relationship between
#Rating and DPR is not similar across all Tiers.


#-- Does any Tier "convert %HS into Rating" more efficiently than another? ---#

mod.ancova1=lm(y_new~x1_new+x2_new+x3_new+x5_new+t)
mod.int3=lm(y_new~x1_new+x2_new+x3_new*t+x5_new)
summary(mod.int3)

anova(mod.ancova1,mod.int3)
#There is no interaction, indicating that the relationship between
#Rating and %HS is similar across all Tiers.

#%HS had almost no influence on Rating in the ANOVA,
#so it was expected that it would not have a different effect by Tier.
``` :contentReference[oaicite:0]{index=0}
::contentReference[oaicite:1]{index=1}
