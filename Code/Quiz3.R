# Q1

      library(AppliedPredictiveModeling)
      data(segmentationOriginal)
      library(caret)
      #  Subset the data to a training set and testing set based on the Case variable in the data set.
      set.seed(125)
      inTrain <- createDataPartition(y=segmentationOriginal$Case, p=0.7, list= FALSE)
      training <- segmentationOriginal[inTrain,]
      testing <- segmentationOriginal[-inTrain,]

      #  fit a CART model with the rpart method using all predictor variables and default caret settings.
      modelFit <- train(Case~., method="rpart", data = training)
      print(modelFit$finalModel)
      
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
      
      predictors <- trainSA[,c(2,3,6:9)]
      modFit <- train(factor(trainSA$chd) ~ trainSA[,c(2,3,6:9)], method="glm", family="binomial")
      
      set.seed(13234)
