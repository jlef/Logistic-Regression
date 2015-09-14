
trainPredictions <- function (){
  
  ####################################
  # Cross Validation
  ####################################
  
  
  # Create folds
  set.seed(117)
  folds <<- createFolds(train.set$GB_12, k=10, list=T) #10 fold cross validation
  
  foldIndex <<- 0;
  
  pred_1 <<- data.frame(user_id=train.set$user_id, GB_12=NA) #container for cross validation
  
  
  ####################################
  # Train Over The Folds
  ####################################
  
  for(i in 1:length(folds)){
    
    
    #train and test data for the current fold
    fold.X.train <<- train.set[-folds[[i]],-c(1, 2)]
    fold.X.test <<- train.set[folds[[i]],-c(1, 2)]
    
    fold.y.train <<- train.set[-folds[[i]],c(1, 2)]
    fold.y.test <<- train.set[folds[[i]],c(1, 2)]
    
    #target rows for this fold
    trgtRows <<- c((foldIndex+1):(foldIndex+length(folds[[i]])))
    
    ##TRAINING AND PREDICTION
    #set up the ids for the results
    pred_1[trgtRows, "user_id"] <<- fold.y.test[,"user_id"]
    
    
    
    glm_1.fit <- glm('GB_12 ~ .', weights=(fold.y.train[,2]*glmWeights)+1
                     , data.frame(GB_12=as.factor(fold.y.train[,2]), data.matrix(fold.X.train))
                     , family = binomial(link="logit"))
    pred_1[trgtRows, -1] <<- predict(glm_1.fit, newdata=fold.X.test, type='response')
    
    #next fold
    foldIndex <<- foldIndex + length(folds[[i]])  
  }
  
  #order the results
  train.set <<- arrange(train.set, user_id)
  pred_1 <<- arrange(pred_1, user_id)
  
  confusionMatrix(round(pred_1[,2]+glmBias), train.set$GB_12)
  
  
}



testPredictions <- function (){
  glm_1.fit <<- glm('GB_12 ~ .', weights=(train.set[,2]*glmWeights)+1
                   , data.frame(GB_12=as.factor(train.set[,2]), data.matrix(train.set[,-c(1, 2)]))
                   , family = binomial(link="logit"))
  pred_test <<- predict(glm_1.fit, newdata=test.set[,-c(1,2)], type='response')
  
  
  confusionMatrix(round(pred_test+glmBias), test.set$GB_12)

}