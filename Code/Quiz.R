'-----------------------########### Quiz 2 #############-----------------------'

# Q1
      library(AppliedPredictiveModeling); library(caret)
      data(AlzheimerDisease)
      
      # Ans : 1
      adData = data.frame(diagnosis,predictors)
      trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
      training = adData[trainIndex,]
      testing = adData[-trainIndex,]

# Q2
      data(concrete)
      set.seed(1000)
      inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
      training = mixtures[ inTrain,]
      testing = mixtures[-inTrain,]
      
      # plot the outcome (CompressiveStrength) versus the index of the samples.
      library(ggplot2)
      library(Hmisc)
      g <- ggplot( data=training, aes(y=CompressiveStrength, x =seq(1:nrow(training)) ))
      
      ## Cement var
      cement_Cuts <- cut2(training$Cement, g =2)
      g <- g + geom_point(aes(color=cement_Cuts)) + labs(title = "CompressiveStrength v/s Index", x= "Index")
      g
            # Smaller cement values at the end; 
      
      ## BlastFurnaceSlag
      Cuts <- cut2(training$BlastFurnaceSlag, g =2)
      g <- g + geom_point(aes(color=Cuts)) + labs(title = "CompressiveStrength v/s Index", x= "Index")
      g
            # Smaller Blast Furnace values at the end; . 
      
      ## FlyAsh
      Cuts <- cut2(training$FlyAsh, g =2)
      g <- ggplot( data=training, aes(y=CompressiveStrength, x =seq(1:nrow(training)) ))
      g <- g + geom_point(aes(color=Cuts)) + labs(title = "CompressiveStrength v/s Index", x= "Index")
      g
            # Smaller FlyAsh values at the beginning; Decreasing value trend seen. 
      
      ## Water
      Cuts <- cut2(training$Water, g =2)
      g <- ggplot( data=training, aes(y=CompressiveStrength, x =seq(1:nrow(training)) ))
      g <- g + geom_point(aes(color=Cuts)) + labs(title = "CompressiveStrength v/s Index", x= "Index")
      g
            # Smaller Water values at the beginning; Decreasing value trend seen. 
      
      ## Superplasticizer
      Cuts <- cut2(training$Superplasticizer, g =4)
      g <- ggplot( data=training, aes(y=CompressiveStrength, x =seq(1:nrow(training)) ))
      g <- g + geom_point(aes(color=Cuts)) + labs(title = "CompressiveStrength v/s Index", x= "Index")
      g
            # Huge cluster of smallest values seen from 50% to 45% data
      
      ## CoarseAggregate 
      Cuts <- cut2(training$CoarseAggregate, g =4)
      g <- ggplot( data=training, aes(y=CompressiveStrength, x =seq(1:nrow(training)) ))
      g <- g + geom_point(aes(color=Cuts)) + labs(title = "CompressiveStrength v/s Index", x= "Index")
      g
            # Decreasing trend but no obvious coloring; Somewhat smaller CoarseAggr. values are seen at the beginning & end; 
      
      ## FineAggregate
      Cuts <- cut2(training$FineAggregate, g =2)
      g <- ggplot( data=training, aes(y=CompressiveStrength, x =seq(1:nrow(training)) ))
      g <- g + geom_point(aes(color=Cuts)) + labs(title = "CompressiveStrength v/s Index", x= "Index")
      g
            # Even distribution
      
      ## Age
      Cuts <- cut2(training$Age, g =3)
      g <- ggplot( data=training, aes(y=CompressiveStrength, x =seq(1:nrow(training)) ))
      g <- g + geom_point(aes(color=Cuts)) + labs(title = "CompressiveStrength v/s Index", x= "Index")
      g
            # Perfect explanation by age
      
      # Ans : Non-random pattern in the plot of outcome v/s index ; no var can perfectly explain it
      
      
# Q3
      #  Make a histogram and confirm the SuperPlasticizer variable is skewed.
      qplot(data = training, x =Superplasticizer, bins = 40 )
      
      # Ans : Large no. of Values are 0, thus log gives -inf
      
# Q4
      # load data
      library(caret)
      library(AppliedPredictiveModeling)
      
      set.seed(3433)
      data(AlzheimerDisease)
      adData = data.frame(diagnosis,predictors)
      inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
      training = adData[ inTrain,]
      testing = adData[-inTrain,]
      
      # Find all the predictor variables in the training set that begin with IL
      IL_PredictorI <- grep("^IL", names(predictors))
      ILs <- c(names(predictors)[IL_PredictorI])
      
      #  Perform principal components on these variables with the preProcess() function from the caret package. 
      MainCol <- ((training$diagnosis == "Impaired")*1+1)
      prComponents <- preProcess(training[,ILs], method = "pca",thresh = 0.9)
      trainPC <- predict(prComponents, newdata = training[,ILs])
      names(trainPC)
 
      
# Q5
      # Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. 
      newTrain <- training[,c("diagnosis",ILs)]; newTest <- testing[,c("diagnosis",ILs)]
  
      # normal fit
      fit <- train(y = newTrain$diagnosis, x = newTrain[,-1], preProcess=c("center","scale"),method = "glm")
      
      # PCA fit
      prComponents <- preProcess(newTrain[,-1], method = "pca",thresh = 0.8)
      trainPC <- predict(prComponents, newdata = newTrain[,-1])
      PCfit <- train(y = newTrain$diagnosis, x =trainPC, method = "glm")

      # Testing
      confusionMatrix(newTest$diagnosis, predict(fit, newTest[,-1])) # normal fit res
      
      testPC <- predict(prComponents, newdata = newTest[,-1])
      confusionMatrix(newTest$diagnosis, predict(PCfit, newdata = testPC)) # PC fit res

      
      ' Ans  :  Non-PCA Accuracy: 0.65; PCA Accuracy: 0.72. for 80% '
      
      