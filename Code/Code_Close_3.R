library(lubridate)
library(MASS)
library(moments)

#import data
datClose3 <- read.csv('D:/study/Y3.1/AUCC/FE+SML+EDA/dataset/Priceclose_2y_3.csv',header = T)
head(as.data.frame(datClose3))

#change chr -> date
datClose3 = datClose3 %>% mutate(Date = mdy(Date)) %>% arrange(Date)
head(datClose3)
str(datClose3)

#split data
index <- nrow(datClose3)*0.8
train <- datClose3[(1:index),]     
test <- datClose3[-(1:index),] 

#model (not select)
reg <- lm(Close ~ Close_1+Open_1+High_1+Low_1+Vol_1+Change_1+Close_2+Open_2+High_2+Low_2+Vol_2+Change_2+Close_3+Open_3+High_3+Low_3+Vol_3+Change_3,data = train)
summary(reg)

par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

#transform
#model with log()
reg_l <- lm(log(Close)~ log(Close_1)+log(Open_1)+log(High_1)+log(Low_1)+log(Vol_1)+log(Close_2)+log(Open_2)+log(High_2)+log(Low_2)+log(Vol_2)++log(Close_3)+log(Open_3)+log(High_3)+log(Low_3)+log(Vol_3),data = train)
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

#predict
pred.d3 <- predict(step.model,newdata=test) 

#result
MSE <- mean(summary(step.model)$residuals^2)
R2 <- cor(test$Close,pred.d3)^2 
RMSE <- sqrt(MSE)

MSE; R2; RMSE

#plot
pred.d3.table <- data.frame(test$Date,test$Close, pred.d3)
names(pred.d3.table)[1] = 'Date' ;names(pred.d3.table)[2] = 'Actual' ;names(pred.d3.table)[3] = 'Predict'

gp_d3 <- test %>% ggplot(aes(Date, Close)) + theme_bw()
gd3 <- gp_d3 + geom_line(data=test, aes(x = Date, y = log(Close))) + 
  geom_line(data = pred.d3.table, aes(x = Date, y = Predict),size = 1,color ="red")+ 
  labs(title = 'Change Closing Price - d3', y = "", x = "") + 
  scale_y_continuous(breaks = c(0, 1250, 2500 , 3750, 5000), 
                     labels = c('$0', '$1,250','2,500', '$3,750', '$5,000'))
gd3