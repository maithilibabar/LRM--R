# LRM--R
Linear Regression Model By Partial Variable Selection Method in R
rm( list=ls())
setwd("C:/CSUF Spring 2019/ISDS 574")
dat=read.csv("Data_Cleaningnew5.csv",head=TRUE, stringsAsFactors = F)


dat1=dat[, -which(colnames(dat) %in% c('X'))]


# Data partition into training=60% & Validation=40%
set.seed(1)
id.train = sample(1:nrow(dat1), nrow(dat1)*.6)
id.validate = setdiff(1:nrow(dat1), id.train)

#linear regression model
obj.null = lm(Price ~ 1, data = dat1[id.train, ])

obj.full = lm(Price ~ ., data = dat1[id.train, ])


#Forward Selection Method
obj1 = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='forward') 
summary(obj1)
names(obj1)

part1 = names(obj1$coef)[-1]
part2 = paste(part1, collapse = ' + ')
part3 = paste0('Price ~ ', part2)

#Model:Price=5464.24+(-144.82)*Age-08_04+2352.26*Automatic_airco+50.46*HP+9.282*Weight+372.344*PoweredWindows
#           +721.45*Bovag_Guarnatee+(-3.0639)*Cc+12.5062*Quarterly_Tax+90.3255*Guarantee_Period+(-491.3987)*Boardcomputer
#           +468.7497*grey+505.4193*Airbag_2+307.8550*Airco+241.2743*Metallic_Rim+206.5018*Mfr_Guarantee  
#           +292.3398*Sport_Model+(-465.1757)*Backseat_Divider+403.7278*Automatic+205.5633*CD_Player

#Checking if all the assumptions are satisfied by the above forward selection model

par(mfrow = c(2, 2))
plot(obj1) 


yhat = predict(obj1, newdata = dat1[id.validate, ])

install.packages('hydroGOF')
require(hydroGOF)
rmse(dat1[id.validate, 'Price'], yhat)

#Multiple R-squared:  0.8755,	Adjusted R-squared:  0.8713. 87% Vaiability in Price is explained by this model.F-significance- p-value: < 2.2e-16
# RMSE= 1231.609

#Backward Elimination Method
obj2 = step(obj.full, scope=list(lower=obj.null, upper=obj.full), direction='backward')
summary(obj2) 


#Linearity

plot(dat1$Age_08_04,dat1$Price)#linear
plot(dat1$HP,dat$Price)#linear
plot(dat1$Weight,dat$Price)#linear

plot(dat1$CC,dat1$Price)# linear
plot(dat1$Quarterly_Tax,dat1$Price)#linear
plot(dat1$Guarantee_Period,dat1$Price)#linear
plot(dat1$Doors,dat1$Price)#non linear

#Normality
hist(obj2$residuals)#normality


#Homoscadesticity
plot(obj2$residuals,obj2$fitted.values)# Homoscadesticity

#Model By Backward Elimination Method:
#Price = 5464.2433+(-144.8224)*Age_08_04 +50.4633*HP +403.7278*Automatic +(-3.0639)*CC +12.5062 *Quarterly_Tax + 
#        9.2820*Weight +206.5018 *Mfr_Guarantee +721.4587*BOVAG_Guarantee +90.3255*Guarantee_Period + 
#        505.4193 *Airbag_2 +307.8550*Airco +2352.2635*Automatic_airco + (-491.398)*Boardcomputer +205.5633*CD_Player + 
#        372.3442*Powered_Windows +292.3398*Sport_Model +(-465.1757)*Backseat_Divider +241.2743*Metallic_Rim + 
#        468.7497*grey

#RMSE
yhat1 = predict(obj2, newdata = dat1[id.validate, ])
require(hydroGOF)
rmse(dat1[id.validate, 'Price'], yhat1)

#Multiple R-squared:  0.8755,	Adjusted R-squared:  0.8713 F-statistics:  p-value: < 2.2e-16
#RMSE=1231.609
#87% Vaiability in Price is explained by this model


#Stepwise Selection Method
obj3 = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='both') # start with full and end with null
summary(obj3)

#To check if all the assumptions of MLR are satisfied 
par(mfrow = c(2, 2))
plot(obj3) 


#Price =5464.2433+(-144.8224)*Age_08_04 +2352.2635*Automatic_airco +50.4633*HP + 9.2820* Weight + 
#        372.3442*Powered_Windows +721.4587*BOVAG_Guarantee + (-3.0639) *CC + 12.5062 *Quarterly_Tax + 
#        90.3255 *Guarantee_Period +(-491.3987)*Boardcomputer +468.7497*grey +505.4193 *Airbag_2 +307.8550*Airco + 
#        241.2743*Metallic_Rim +206.5018*Mfr_Guarantee +292.3398*Sport_Model +(-465.1757)*Backseat_Divider + 
#         403.7278*Automatic +205.5633*CD_Player

#RMSE
yhat2 = predict(obj3, newdata = dat1[id.validate, ])
require(hydroGOF)
rmse(dat1[id.validate, 'Price'], yhat2)

#Multiple R-squared:  0.8755,	Adjusted R-squared:  0.8713 F-Statistics-p-value: < 2.2e-16
#RMSE=1231.609
#87% Vaiability in Price is explained by this model

#RMSE are same. So we can choose any of the three.

# Principal Component Analysis(PCA of Advanced feature of Product)
#Advanced features includes sports model, backseat_divider, CD_player, tow bar
dat=read.csv("Data_Cleaningnew5.csv",head=TRUE, stringsAsFactors = F)
dat1=dat[, -which(colnames(dat) %in% c('X'))]
obj4 = prcomp(dat1[(,c('Sport_Model','Backseat_Divider','CD_Player','Tow_Bar'))], scale=T)

obj4=prcomp(dat1[, c('Sport_Model', 'Backseat_Divider','CD_Player', 'Tow_Bar')], scale=T)


obj4$rotation
obj4$x

summary(obj4)

# Three PCs cover more than 70% of the total variance.
