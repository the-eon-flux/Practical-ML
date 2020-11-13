# Q1

      library(AppliedPredictiveModeling)
      data(segmentationOriginal)
      library(caret)
      #  Subset the data to a training set and testing set based on the Case variable in the data set.
      set.seed(125)
      training<-segmentationOriginal[which(segmentationOriginal$Case=='Train'),]
      testing<-segmentationOriginal[which(segmentationOriginal$Case=='Test'),]

      #  fit a CART model with the rpart method using all predictor variables and default caret settings.
      modelFit <- train(Class~., method="rpart", data = training)
      print(modelFit$finalModel)
      
      library(rattle)
      fancyRpartPlot(modelFit$finalModel)
      
      # what would be the final model prediction for cases with the following variable values: 
            '
            a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2

            b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
            
            c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
            
            d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
            '
# Q2 k = sample size & large bias with small var if k is small            
# Q3
            library(pgmm)
            load("./Code/Data/olive.rda")
            olive = olive[,-1]
            'These data contain information on 572 different Italian olive oils from multiple regions in Italy. Fit a classification tree where Area is the outcome variable. Then predict the value of area for the following data frame using the tree command with all defaults'     
            
            modFit <- train(Area~., method ="rpart", data = olive)
            
            # Test data
            testing <- as.data.frame(t(colMeans(olive)))
            predict(modFit, newdata = testing)

            # Ans : 2.78 ; Strange since area is qual variable
            # best model tree
            library(rattle)
            fancyRpartPlot(modFit$finalModel)   
            
# Q4
      load("./Code/Data/SAheart.RData")
      SAheart <- read.table("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data", sep = ",", header = T)
      SAheart <- saheart[, -1]
      RNGversion("3.5.3")
      
      set.seed(8484)
      train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
      trainSA = SAheart[train,]
      testSA = SAheart[-train,]
      
      'set the seed to 13234 and fit a logistic regression model (method="glm", be sure to specify family="binomial") with Coronary Heart Disease (chd) as the outcome and age at onset, current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors.'
      
      set.seed(13234)
      modFit <-train(chd ~ age+alcohol+obesity+tobacco+typea+ldl, data=trainSA, method="glm", family="binomial")

      missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
      
      predictTrain<-predict(modFit,trainSA)
      predictTest<-predict(modFit,testSA)
      missClassTrain<-missClass(trainSA$chd,predictTrain)
      missClassTrain # 0.2727273

      # For test
      missClassTest<-missClass(testSA$chd,predictTest)
      missClassTest # 0.3116883

# Q5  
      library(randomForest)
      
      vowel.train <- read.csv("Code/Data/vowel.train.txt", header = T)
      vowel.train <- vowel.train[,-1]
      vowel.train$y <- as.factor(vowel.train$y)
      
      vowel.test <- read.csv("Code/Data/vowel.test.txt", header = T)
      vowel.test <- vowel.test[,-1] 
      vowel.test$y <- as.factor(vowel.test$y)

      set.seed(33833)
      # Old method
      modFit <- train(y ~ .,data=vowel.train, method="rf")

      'Calculate the variable importance using the varImp function in the caret package. What is the order of variable importance?'
      varImp(modFit)
      
      # Quiz alternative
      modFit <- randomForest(y ~ .,data=vowel.train)
      order(varImp(modFit),decreasing = TRUE)
      