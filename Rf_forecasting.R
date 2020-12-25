rm(list = ls())

library(caret)
library(ggplot2)
library(randomForest)
library(zoo)

train = read.csv("train_0irEZ2H.csv")
train = train[,-1]
test = read.csv("test_nfaJ3J5.csv")


samp <- sample(nrow(train), 0.8 * nrow(train))
train <- train[samp, ]
test <- train[-samp, ]

# rfGrid <- expand.grid(.mtry = 1:7, n.trees = (1:30)*20)
# 
# ctrl <- trainControl(## 10-fold CV
#                                method = "repeatedcv",
#                                number = 10,
#                                ## repeated ten times
#                                repeats = 2)




# rfTune <- train(units_sold ~ ., data = train,
#                                          method = "rf",
#                                          tuneGrid = rfGrid,
#                                          trControl = ctrl,
#                                          verbose = FALSE
#                                          )
train$units_sold = as.numeric(train$units_sold)
train$week = as.numeric(train$week)

x = test

test$units_sold = as.numeric(test$units_sold)
test$week = as.numeric(test$week)


rfMod <- randomForest(units_sold ~ . , data = train, mtry=3)

test$pred =  predict(rfMod, newdata = test)
pred <- predict(rfMod, newdata = test)

test$pred = pred

lm.fit = lm(units_sold ~ pred, data = test)

sku1 = test %>% filter(sku_id == 300021)

sku1 = sku1 %>% arrange(by = week)

sku1 = sku1 %>% mutate(units_sold = rollsumr(units_sold, 28, fill=NA), pred = rollsumr(pred, 28, fill=NA))

sku1 = sku1 %>% mutate(lw = pred - sd(pred, na.rm = T), hw = pred + sd(pred, na.rm = T))

ggplot() + geom_line(data = sku1, aes(week, units_sold, color = "Real")) + 
           geom_line(data = sku1, aes(week, pred, color = "Predicted")) +
  geom_ribbon(data=sku1, aes(x=week, ymin=lw, 
                                       ymax=hw), 
              alpha=0.2, inherit.aes=F, fill="red")+
           labs(y = "Demand", x = "Time") +
           theme_bw() +
           theme(axis.text.x=element_blank())
           


