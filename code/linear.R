###CHANGE THE PATH###
setwd("...Logistic-Regression/code")

#install.packages('caret')
library(caret) #predictive model tools (used data splitting, feature selection etc.)

#install.packages('dplyr')
library(plyr) #ordering selecting etc 
library(dplyr)

#install.packages('missForest')
library(missForest) #Impute missing using random forest

#install.packages('VIM')
library(VIM) #Visualization and Imputation of Missing Values

#install.packages('corrgram')
library(corrgram) #Plot a Correlogram



####################################
# Load Dataset
####################################
ad <- read.csv(file='../input/AssessmentDataset.csv', head=T, stringsAsFactors=F)


####################################
# Replace Missing Values With NAs
####################################

#user_id
#GB_12
#loan_amount  #missing values already NAs
ad$PP.Average.Current.DBT[ad$PP.Average.Current.DBT == -9999999] <- NA
ad$PP.Average.Current..Company.DBT...Industry.DBT.[ad$PP.Average.Current..Company.DBT...Industry.DBT. == -9999999] <- NA
ad$PP.Worst..Company.DBT...Industry.DBT..In.The.Last.12.Months[ad$PP.Worst..Company.DBT...Industry.DBT..In.The.Last.12.Months == -9999999] <- NA
ad$Ave.No..Of.DBT...0.1000.[ad$Ave.No..Of.DBT...0.1000. == -999] <- NA
ad$DBT.most.recent.month[ad$DBT.most.recent.month == -99999] <- NA
ad$Number.unpaid.accounts.in.past.year[ad$Number.unpaid.accounts.in.past.year == -9999999] <- NA
#Number.Of.Previous.Searches..last.3m.
#Number.Of.Previous.Searches..last.6m.
#Number.Of.Previous.Searches..last.12m.
#Number.Of.Meeting.Of.Creditors # no information REMOVE
ad$Total.Fixed.Assets.As.A.Percentage.Of.Total.Assets[ad$Total.Fixed.Assets.As.A.Percentage.Of.Total.Assets == -99999999.9] <- NA #only two missing values
ad$Current.Liabilities.As.A.Percentage.Of.Total.Assets[ad$Current.Liabilities.As.A.Percentage.Of.Total.Assets == -99999999.9] <- NA #only two missing values
ad$Capital.Employed.As.A.Percentage.Of.Total.Assets[ad$Capital.Employed.As.A.Percentage.Of.Total.Assets == -99999999.9] <- NA #only two missing values
ad$Equity.Gearing....[ad$Equity.Gearing.... == -999999.9] <- NA #only 5 missing values
#G3...Directors.Consent.with.Full.CAIS..Consumer...Commercial....Commercial.Delphi.score
#submitted_profit_after_tax_1 #1 missing value already an NA
#calculated_profit_after_tax_1 #no NAs
#submitted_profit_after_tax_2 #no NAs
#calculated_profit_after_tax_2 #no NAs
ad$Score.1..SCORE.[ad$Score.1..SCORE. == 9999] <- NA
ad$Number.of.active.SHARE.records[ad$Number.of.active.SHARE.records == -999999] <- NA; ad$Number.of.active.SHARE.records[ad$Number.of.active.SHARE.records == -999997] <- NA
ad$Number.of.all.SHARE.accounts[ad$Number.of.all.SHARE.accounts == -999999] <- NA; ad$Number.of.all.SHARE.accounts[ad$Number.of.all.SHARE.accounts == -999997] <- NA
ad$Number.of.active.SHARE.accounts[ad$Number.of.active.SHARE.accounts == -999999] <- NA; ad$Number.of.active.SHARE.accounts[ad$Number.of.active.SHARE.accounts == -999997] <- NA
ad$Total.value.of.active.accounts[ad$Total.value.of.active.accounts == -999999] <- NA; ad$Total.value.of.active.accounts[ad$Total.value.of.active.accounts == -999997] <- NA
ad$payment.status.less.2.3mos[ad$payment.status.less.2.3mos == -999999] <- NA; ad$payment.status.less.2.3mos[ad$payment.status.less.2.3mos == -999997] <- NA
ad$payment.status.less.2.12mos[ad$payment.status.less.2.12mos == -999999] <- NA; ad$payment.status.less.2.12mos[ad$payment.status.less.2.12mos == -999997] <- NA
ad$payment.status.less.2.24mos[ad$payment.status.less.2.24mos == -999999] <- NA; ad$payment.status.less.2.24mos[ad$payment.status.less.2.24mos == -999997] <- NA
ad$payment.status.less.1.3mos[ad$payment.status.less.1.3mos == -999999] <- NA; ad$payment.status.less.1.3mos[ad$payment.status.less.1.3mos == -999997] <- NA
ad$payment.status.less.1.12mos[ad$payment.status.less.1.12mos == -999999] <- NA; ad$payment.status.less.1.12mos[ad$payment.status.less.1.12mos == -999997] <- NA
ad$payment.status.less.1.24mos[ad$payment.status.less.1.24mos == -999999] <- NA; ad$payment.status.less.1.24mos[ad$payment.status.less.1.24mos == -999997] <- NA
ad$payment.status.0.3mos[ad$payment.status.0.3mos == -999999] <- NA; ad$payment.status.0.3mos[ad$payment.status.0.3mos == -999997] <- NA
ad$payment.status.0.12mos[ad$payment.status.0.12mos == -999999] <- NA; ad$payment.status.0.12mos[ad$payment.status.0.12mos == -999997] <- NA
ad$payment.status.0.24mos[ad$payment.status.0.24mos == -999999] <- NA; ad$payment.status.0.24mos[ad$payment.status.0.24mos == -999997] <- NA
ad$Total.value.accounts.in.sector.2[ad$Total.value.accounts.in.sector.2 == -999999] <- NA; ad$Total.value.accounts.in.sector.2[ad$Total.value.accounts.in.sector.2 == -999997] <- NA
ad$Total.balances.active.accounts[ad$Total.balances.active.accounts == -999999] <- NA; ad$Total.balances.active.accounts[ad$Total.balances.active.accounts == -999997] <- NA
ad$Total.limits.rev.acct.active[ad$Total.limits.rev.acct.active == -999997] <- NA
ad$Mthly.rpymt.actv.fxd.term.accts[ad$Mthly.rpymt.actv.fxd.term.accts == -999997] <- NA
ad$Value.actv.fxd.term.accts[ad$Value.actv.fxd.term.accts == -999999] <- NA; ad$Value.actv.fxd.term.accts[ad$Value.actv.fxd.term.accts == -999997] <- NA
ad$Num.pymt.status.3plus.48.updates[ad$Num.pymt.status.3plus.48.updates == -999997] <- NA
ad$current.balance.to.Annual.Income[ad$current.balance.to.Annual.Income == -999999] <- NA; ad$current.balance.to.Annual.Income[ad$current.balance.to.Annual.Income == -999997] <- NA
ad$Curr.bal.rev.crd.to.curr.limits[ad$Curr.bal.rev.crd.to.curr.limits == -999999] <- NA; ad$Curr.bal.rev.crd.to.curr.limits[ad$Curr.bal.rev.crd.to.curr.limits == -999997] <- NA
ad$bal.rev.crd.to.curr.limits.3.mo[ad$bal.rev.crd.to.curr.limits.3.mo == -999999] <- NA; ad$bal.rev.crd.to.curr.limits.3.mo[ad$bal.rev.crd.to.curr.limits.3.mo == -999997] <- NA
ad$bal.rev.crd.to.curr.limits.6.mo[ad$bal.rev.crd.to.curr.limits.6.mo == -999999] <- NA; ad$bal.rev.crd.to.curr.limits.6.mo[ad$bal.rev.crd.to.curr.limits.6.mo == -999997] <- NA
ad$Num.Rev.Acct.util.ge.75[ad$Num.Rev.Acct.util.ge.75 == -999997] <- NA
ad$Unemployment.Among.25_39[ad$Unemployment.Among.25_39 == -999997] <- NA
ad$Avg.number.of.Court.Judgments[ad$Avg.number.of.Court.Judgments == -999997] <- NA
ad$Credit.Card.Repayment.Amount[ad$Credit.Card.Repayment.Amount == -999997] <- NA
ad$Num.Cash.Advances.3.Month[ad$Num.Cash.Advances.3.Month == -999997] <- NA

plot(ad$Number.Of.Meeting.Of.Creditors)
ad$Number.Of.Meeting.Of.Creditors <- NULL #remove since it has no information



####################################
# Remove 'Perfectly' Correlated Variables
####################################
res <- cor(ad, use = "pairwise.complete.obs")
res <- round(res, digits = 3)
res[lower.tri(res, diag = T)] <- NA

pc<- apply(
  res, 1, 
  function(u) paste( names(which(u > 0.99 | u < -0.99)), collapse="," ) 
)

correlatedValues1 <- as.vector(pc[pc!=""])
correlatedValues2 <- names(pc[pc!=""])

totalLength <- length(unlist(strsplit(correlatedValues1, ',')))
crDF <- data.frame(id=1:totalLength, value=NA, col1=1, col2=NA, cor=NA)
currentIndex <- 1
for(i in 1:length(correlatedValues1)){
  cvs <- unlist(strsplit(correlatedValues1[i], ','))
  for(j in 1:length(cvs)){
    value <- 0;
    if (sum(complete.cases(ad[,cvs[j]])) < sum(complete.cases(ad[,correlatedValues2[i]]))){
      value<-sum(complete.cases(ad[,cvs[j]]))
    } else {
      value<-sum(complete.cases(ad[,correlatedValues2[i]]))
    }
    
    crDF[currentIndex,-1]<- c(sum(ad[,cvs[j]] == ad[,correlatedValues2[i]], na.rm = T)/value, cvs[j], correlatedValues2[i], cor(ad[,cvs[j]], ad[,correlatedValues2[i]],use="na.or.complete"))
    currentIndex<- currentIndex + 1
  }
}


crDF <- arrange(crDF, desc(value))
#Drop colums
ad <- ad[ , -which(names(ad) %in% crDF[crDF$value > 0.95,'col1'])]

####################################
# Missing Data
####################################
propMissing <- aggr(ad)
summary(propMissing)


sum(complete.cases(ad))/dim(ad)[1] #only 32.25% of data is complete (no missing values)

# data is missing at random
# Impune Data
#id and target variable not included
imputeData <- missForest(ad[,-c(1,2)], verbose = TRUE, ntree = 80, maxiter = 2)
imputeData[[2]] #OOBerror

imputeData <- imputeData[[1]]


####################################
# Handle Correlated Features
####################################
# corrgram(ad[,-c(1,2)], order=NULL, lower.panel=panel.shade,
#          upper.panel=panel.pts, text.panel=panel.txt,
#          main="Assesment Dataset")


res <- cor(imputeData, use = "everything")
res <- round(res, digits = 3)
res[lower.tri(res, diag = T)] <- NA

pc<- apply(
  res, 1, 
  function(u) paste( names(which(u > 0.85|u < -0.85)), collapse="," ) #0.85 cut-off
)

# remove highly correlated variables
correlatedVaribales <- unlist(strsplit(as.vector(pc[pc!=""]), ','))
reducedImputedData <- imputeData[,!(colnames(imputeData) %in% correlatedVaribales)]
removedImputedData <- imputeData[,(colnames(imputeData) %in% correlatedVaribales)]


# corrgram(reducedImputedData, order=NULL, lower.panel=panel.shade,
#          upper.panel=NULL, text.panel=panel.txt,
#          main="Assesment Dataset")
# 
# 
# corrgram(removedImputerData, order=NULL, lower.panel=panel.shade,
#          upper.panel=NULL, text.panel=panel.txt,
#          main="Assesment Dataset")

#PCA
pcaImputedData <- prcomp(removedImputedData, scale = TRUE)
plot(pcaImputedData
     , main='Variances'
     ,xlab = 'Principal Components')
pcaRes <- pcaImputedData$x[,1:4] #Retain 


####################################
# New features
####################################
mData <- cbind(ad[,c(1,2)], reducedImputedData, pcaRes)

mData$loanAmnt_missing <- ad$loan_amount/ad$loan_amount
mData$loanAmnt_missing[is.na(mData$loanAmnt_missing)] <- 0

mData$current.balance.to.Annual.Income_missing <- ad$current.balance.to.Annual.Income/ad$current.balance.to.Annual.Income
mData$current.balance.to.Annual.Income_missing[is.na(mData$current.balance.to.Annual.Income_missing)] <- 0

mData$loan_amount_log <- log1p(mData$loan_amount)


####################################
# Create Train and Test Sets
####################################
set.seed(99)
inTrain <- createDataPartition(y=mData$GB_12, p=0.75, list=FALSE);   # 75% for the trainset

train.set <- mData[inTrain,]
test.set <- mData[-inTrain,] #Holdout


####################################
# Feature selection
####################################


glmWeights <- 12; #set the weights variable. Will be used for tuning and training of the final model.
glmBias <- 0.025;


glmGrid <-  data.frame(.weights = (train.set[,2]*glmWeights)+1)
y <- as.factor(train.set[,2])
X <- train.set[,-c(1, 2)]

set.seed(399)
ctrl_rfe <- rfeControl(method = "repeatedcv",
                       rerank=T,
                       repeats = 3,
                       verbose = TRUE,
                       number = 10,
                       p = .75,
                       functions = lrFuncs)

glm_rfe <- rfe(X, y,  sizes      = c(7:10, 12, 15, 20, 28),
                      rfeControl = ctrl_rfe,
                      method     = "glm",
                      metric     = "Accuracy",
                      tuneGrid   = glmGrid)

trellis.par.set(caretTheme())
plot(glm_rfe, type = c("g", "o"))

train.set <- train.set[,c("user_id", "GB_12", predictors(glm_rfe))]
test.set <- test.set[,c("user_id", "GB_12", predictors(glm_rfe))]

rm(X, y)

####################################
# Cross Validation
####################################


# Create folds
set.seed(117)
folds <- createFolds(train.set$GB_12, k=10, list=T) #10 fold cross validation

foldIndex <- 0;

pred_1 <- data.frame(user_id=train.set$user_id, GB_12=NA) #container for cross validation


####################################
# Train Over The Folds
####################################

for(i in 1:length(folds)){
  
  print(paste0("i: ", i))
  
  #train and test data for the current fold
  fold.X.train <- train.set[-folds[[i]],-c(1, 2)]
  fold.X.test <- train.set[folds[[i]],-c(1, 2)]
  
  fold.y.train <- train.set[-folds[[i]],c(1, 2)]
  fold.y.test <- train.set[folds[[i]],c(1, 2)]
  
  #target rows for this fold
  trgtRows <- c((foldIndex+1):(foldIndex+length(folds[[i]])))
  
  ##TRAINING AND PREDICTION
  #set up the ids for the results
  pred_1[trgtRows, "user_id"] <- fold.y.test[,"user_id"]
  
  
  
  glm_1.fit <- glm('GB_12 ~ .', weights=(fold.y.train[,2]*glmWeights)+1
                   , data.frame(GB_12=as.factor(fold.y.train[,2]), data.matrix(fold.X.train))
                   , family = binomial(link="logit"))
  pred_1[trgtRows, -1] <- predict(glm_1.fit, newdata=fold.X.test, type='response')
  
  #next fold
  foldIndex <- foldIndex + length(folds[[i]])  
}

#order the results
train.set <- arrange(train.set, user_id)
pred_1 <- arrange(pred_1, user_id)

confusionMatrix(round(pred_1[,2]+glmBias), train.set$GB_12)



####################################
# Final Model 
####################################


#Train And Predict On Holdout Data
glm_1.fit <- glm('GB_12 ~ .', weights=(train.set[,2]*glmWeights)+1
                 , data.frame(GB_12=as.factor(train.set[,2]), data.matrix(train.set[,-c(1, 2)]))
                 , family = binomial(link="logit"))
pred_test <- predict(glm_1.fit, newdata=test.set[,-c(1,2)], type='response')


confusionMatrix(round(pred_test+glmBias), test.set$GB_12)


