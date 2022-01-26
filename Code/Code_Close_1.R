library(lubridate)
library(MASS)
library(moments)

#import data
datClose1 <- read.csv('D:/study/Y3.1/AUCC/FE+SML+EDA/dataset/Priceclose_2y_1.csv',header = T)
head(as.data.frame(datClose1))

#change chr -> date
datClose1 = datClose1 %>% mutate(Date = mdy(Date)) %>% arrange(Date)
head(datClose1)
str(datClose1)

#split data
index <- nrow(datClose1)*0.8
train <- datClose1[(1:index),]     
test <- datClose1[-(1:index),] 

#model (not select)
reg <- lm(Close ~ Close_1+Open_1+High_1+Low_1+Vol_1+Change_1,data = train)
summary(reg)

par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

#transform
#model with log()
reg_l <- lm(log(Close)~ log(Close_1)+log(Open_1)+log(High_1)+log(Low_1)+log(Vol_1),data = train)
summary(reg_l)

par(mfrow=c(2,2))
plot(reg_l)
par(mfrow=c(1,1))

#model (selected features)

step.model <- stepAIC(reg_l, direction = "both", trace = FALSE)
summary(step.model)

par(mfrow=c(2,2))
plot(step.model)
par(mfrow=c(1,1))

reg_l <- lm(log(Close)~ log(Close_1)+log(High_1)+log(Vol_1),data = train)
summary(reg_l)

#predict
pred.d1 <- predict(reg_l,newdata=test) 

#result
MSE <- mean(summary(step.model)$residuals^2)
R2 <- cor(test$Close,pred.d1)^2 
RMSE <- sqrt(MSE)

MSE; R2; RMSE

#plot
pred.d1.table <- data.frame(test$Date,test$Close, pred.d1)
names(pred.d1.table)[1] = 'Date' ;names(pred.d1.table)[2] = 'Actual' ;names(pred.d1.table)[3] = 'Predict'

gp_d1 <- test %>% ggplot(aes(Date, Close)) + theme_bw()
gd1 <- gp_d1 + geom_line(data=test, aes(x = Date, y = log(Close))) + 
  geom_line(data = pred.d1.table, aes(x = Date, y = Predict),size = 1,color ="red")+ 
  labs(title = 'Change Closing Price - d1', y = "", x = "") + 
  scale_y_continuous(breaks = c(0, 1250, 2500 , 3750, 5000), 
                     labels = c('$0', '$1,250','2,500', '$3,750', '$5,000'))
gd1

