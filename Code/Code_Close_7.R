library(lubridate)
library(MASS)
library(moments)
library(nortest)

#import data
datClose7 <- read.csv('D:/study/Y3.1/AUCC/FE+SML+EDA/dataset/Priceclose_2y_7.csv',header = T)
head(as.data.frame(datClose7))

#change chr -> date
datClose7 = datClose7 %>% mutate(Date = mdy(Date)) %>% arrange(Date)
head(datClose7); str(datClose7)

#split data
index <- nrow(datClose7)*0.8
train <- datClose7[(1:index),]     
test <- datClose7[-(1:index),] 

#model (not select)
reg <- lm(Close ~ Close_1+Open_1+High_1+Low_1+Vol_1+Change_1+Close_2+Open_2+High_2+Low_2+Vol_2+Change_2+Close_3+Open_3+High_3+Low_3+Vol_3+Change_3+Close_4+Open_4+High_4+Low_4+Vol_4+Change_4+Close_5+Open_5+High_5+Low_5+Vol_5+Change_5+
            Close_6+Open_6+High_6+Low_6+Vol_6+Change_6+Close_7+Open_7+High_7+Low_7+Vol_7+Change_7,data = train)
summary(reg)

par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

#transform
#model with log()
reg_l <- lm(log(Close)~ log(Close_1)+log(Open_1)+log(High_1)+log(Low_1)+log(Vol_1)+log(Close_2)+log(Open_2)+log(High_2)+log(Low_2)+log(Vol_2)+log(Close_3)+log(Open_3)+log(High_3)+log(Low_3)+log(Vol_3)+log(Close_4)+log(Open_4)+log(High_4)+log(Low_4)+log(Vol_4)+
              log(Close_5)+log(Open_5)+log(High_5)+log(Low_5)+log(Vol_5)+log(Close_6)+log(Open_6)+log(High_6)+log(Low_6)+log(Vol_6)+log(Close_7)+log(Open_7)+log(High_7)+log(Low_7)+log(Vol_7),data = train)
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
pred.d7 <- predict(step.model,newdata=test) 

#result
MSE <- mean(summary(step.model)$residuals^2)
R2 <- cor(test$Close,pred.d7)^2 
RMSE <- sqrt(MSE)

MSE; R2; RMSE

#plot
pred.d7.table <- data.frame(test$Date,test$Close, pred.d7)
names(pred.d7.table)[1] = 'Date' ;names(pred.d7.table)[2] = 'Actual' ;names(pred.d7.table)[3] = 'Predict'

gp_d7 <- test %>% ggplot(aes(Date, Close)) + theme_bw()
gd7 <- gp_d7 + geom_line(data=test, aes(x = Date, y = log(Close))) + 
  geom_line(data = pred.d7.table, aes(x = Date, y = Predict),size = 1,color ="red")+ 
  labs(title = 'Change Closing Price - d7', y = "", x = "") + 
  scale_y_continuous(breaks = c(0, 1250, 2500 , 3750, 5000), 
                     labels = c('$0', '$1,250','2,500', '$3,750', '$5,000'))
gd7
