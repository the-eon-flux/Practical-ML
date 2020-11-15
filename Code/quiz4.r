# Q1       # Ans : 63 > 61 > 51
      library(caret)
      vowel.train <- read.csv("Code/Data/vowel.train.txt", header = T)
      vowel.train <- vowel.train[,-1]
      vowel.train$y <- as.factor(vowel.train$y)
      
      vowel.test <- read.csv("Code/Data/vowel.test.txt", header = T)
      vowel.test <- vowel.test[,-1] 
      vowel.test$y <- as.factor(vowel.test$y)
      
      set.seed(33833)
      mRF <- train(y~., data = vowel.train, method = "rf")
      mGBM <- train(y~., data = vowel.train, method = "gbm", verbose= FALSE)
      
      
      
      # Testing
      pred1 <- predict(mRF, vowel.test[,-1]) 
      pred2 <- predict(mGBM, vowel.test[,-1])
      
      predDF <- data.frame(pred1,pred2,y=vowel.test$y)
      mCombined <- train(y ~.,data=predDF)
      
      pred3 <- predict(mCombined, vowel.test[,-1])
      
      confusionMatrix(pred1, vowel.test$y)
      confusionMatrix(pred2, vowel.test$y)
      confusionMatrix(pred3, vowel.test$y)
      
      # Combined >  RF > GBM (66 > 58 > 51) 



# Q2       # Ans : Choosen : eq to boost but > other 2. My :88 & better than all
      library(gbm); library(AppliedPredictiveModeling)
      set.seed(3433)
      data(AlzheimerDisease)
      adData = data.frame(diagnosis,predictors)
      inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
      
      training = adData[ inTrain,]
      testing = adData[-inTrain,]
      
      set.seed(62433)
      
      mRF <- train(diagnosis~., data = training, method = "rf")
      mGBM <- train(diagnosis~., data = training, method = "gbm", verbose= FALSE)
      mLDA <- train(diagnosis~., data = training, method = "lda")
      
      pred1 <- predict(mRF, testing[,-1])
      pred2 <- predict(mGBM, testing[,-1])
      pred3 <- predict(mLDA, testing[,-1])
      
      predDF <- data.frame(pred1,pred2, pred3, "diagnosis"=testing$diagnosis)
      mCombined <- train(diagnosis ~.,data=predDF, "rf")
      pred4 <-  predict(mCombined, testing[,-1])
      
      confusionMatrix(pred1, testing$diagnosis)[[3]][1]
      confusionMatrix(pred2, testing$diagnosis)[[3]][1]
      confusionMatrix(pred3, testing$diagnosis)[[3]][1]
      confusionMatrix(pred4, testing$diagnosis)[[3]][1]
      
      # Stacked accuracy better than all
      
# Q3  Ans : cement
      
      set.seed(3523)
      library(AppliedPredictiveModeling)
      data(concrete)
      inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
      training = concrete[ inTrain,]
      testing = concrete[-inTrain,]

      set.seed(233)  
      mod <- train(CompressiveStrength ~., data = training, method="lasso")
      plot(mod$finalModel)

# Q4 # Ans : 96%
      
      library(lubridate) # For year() function below
      library(forecast)
      
      dat = read.csv("./Code/Data/gaData.csv", header = T)
      
      training = dat[year(dat$date) < 2012,]
      testing = dat[(year(dat$date)) > 2011,]
      tstrain = ts(training$visitsTumblr)
      fcast <- forecast(mod, level=95, h=nrow(testing))

      mod <- bats(tstrain)
      plot(fcast); lines(testing,col="red")

      c = 0
      for(i in 1:length(testing$date)){
            if(testing$visitsTumblr[i] > fcast$lower[i] && testing$visitsTumblr[i] < fcast$upper[i] ){
                  c = c+1
            }
      }

      c / length(testing$visitsTumblr)


# Q5. # 6.72
      library(e1071)
      set.seed(325)  
      
      Mod <- svm(CompressiveStrength ~., data = training)
      pred <- predict(Mod, testing)      

      Error <- testing$CompressiveStrength - pred
      sqrt(sum((Error^2))/256)
      
      confusionMatrix(Mod, testing$CompressiveStrength)


