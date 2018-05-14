################
library(caret)
###install.packages("randomForest")
###library(randomForest)
###library(ggplot2)
#####3install.packages("ggplot2")
#read the train and test data
##
train_raw<-read.csv("E:/mayuri kenjale/dataset/Group Project on R-Data Set-4.csv")
test_raw<-read.csv("E:/mayuri kenjale/dataset/Group Project on R-Data Set-3.csv")
str(train_raw)
#dim(train_raw) dim(test_raw) summary(train_raw) summary(test_raw)
########check which column has greater than 40% na values.
##colnames(train_raw)[colSums(is.na(train_raw)) > 584]
###colnames(test_raw)[colSums(is.na(test_raw)) > 584]
#match("Alley",names(test_raw) match("FireplaceQu",names(test_raw))match("PoolQC",names(test_raw))match("Fence",names(test_raw))match("MiscFeature",names(test_raw))
      
#############
####delete the columns which has the more than 40% error
train_df<-train_raw[,-c(1,7,58,73,74,75)]
test_df<-test_raw[,-c(1,7,58,73,74,75)]

#######na values relaced by mean (continuous data)[train data]
train_df$GarageYrBlt[is.na(train_df$GarageYrBlt)]<-mean(train_df$GarageYrBlt,na.rm=TRUE)
train_df$LotFrontage[is.na(train_df$LotFrontage)]<-mean(train_df$LotFrontage,na.rm=TRUE)
train_df$MasVnrArea[is.na(train_df$MasVnrArea)]<-mean(train_df$MasVnrArea,na.rm=TRUE)

######################
####categorical data replaced by mode
train_df$GarageQual[is.na(train_df$GarageQual)]<-"TA"
train_df$GarageFinish[is.na(train_df$GarageFinish)]<-"Unf"
train_df$GarageType[is.na(train_df$GarageType)]<-"Attchd"
train_df$BsmtFinType1[is.na(train_df$BsmtFinType1)]<-"Unf"
train_df$BsmtFinType2[is.na(train_df$BsmtFinType2)]<-"Unf"
train_df$BsmtExposure[is.na(train_df$BsmtExposure)]<-"No"
train_df$BsmtCond[is.na(train_df$BsmtCond)]<-"TA"
train_df$BsmtQual[is.na(train_df$BsmtQual)]<-"TA"
train_df$MasVnrType[is.na(train_df$MasVnrType)]<-"None"
train_df$Electrical[is.na(train_df$Electrical)]<-"SBrkr"
train_df$GarageCond[is.na(train_df$GarageCond)]<-"TA"
######################
sum(is.na(train_df))

#############missing value replaced by mean and mode (test data)
test_df$GarageYrBlt[is.na(test_df$GarageYrBlt)]<-mean(test_df$GarageYrBlt,na.rm=TRUE)
test_df$LotFrontage[is.na(test_df$LotFrontage)]<-mean(test_df$LotFrontage,na.rm=TRUE)
test_df$MasVnrArea[is.na(test_df$MasVnrArea)]<-mean(test_df$MasVnrArea,na.rm=TRUE)
test_df$BsmtUnfSF[is.na(test_df$BsmtUnfSF)]<-mean(test_df$BsmtUnfSF,na.rm=TRUE)
test_df$BsmtFinSF1[is.na(test_df$BsmtFinSF1)]<-mean(test_df$BsmtFinSF1,na.rm=TRUE)
test_df$BsmtFinSF2[is.na(test_df$BsmtFinSF2)]<-mean(test_df$BsmtFinSF2,na.rm=TRUE)
test_df$TotalBsmtSF[is.na(test_df$TotalBsmtSF)]<-mean(test_df$TotalBsmtSF,na.rm=TRUE)
test_df$BsmtFullBath[is.na(test_df$BsmtFullBath)]<-mean(test_df$BsmtFullBath,na.rm=TRUE)
test_df$BsmtHalfBath[is.na(test_df$BsmtHalfBath)]<-mean(test_df$BsmtHalfBath,na.rm=TRUE)
test_df$GarageCars[is.na(test_df$GarageCars)]<-mean(test_df$GarageCars,na.rm=TRUE)
test_df$GarageArea[is.na(test_df$GarageArea)]<-mean(test_df$GarageArea,na.rm=TRUE)
######################
####categorical data replaced by mode
test_df$GarageCond[is.na(test_df$GarageCond)]<-"TA"
test_df$GarageQual[is.na(test_df$GarageQual)]<-"TA"
test_df$GarageFinish[is.na(test_df$GarageFinish)]<-"Unf"
test_df$GarageType[is.na(test_df$GarageType)]<-"Attchd"
test_df$BsmtFinType1[is.na(test_df$BsmtFinType1)]<-"Unf"
test_df$BsmtFinType2[is.na(test_df$BsmtFinType2)]<-"Unf"
test_df$BsmtExposure[is.na(test_df$BsmtExposure)]<-"No"
test_df$BsmtCond[is.na(test_df$BsmtCond)]<-"TA"
test_df$BsmtQual[is.na(test_df$BsmtQual)]<-"TA"
test_df$MasVnrType[is.na(test_df$MasVnrType)]<-"None"
test_df$KitchenQual[is.na(test_df$KitchenQual)]<-"TA"
test_df$SaleType[is.na(test_df$SaleType)]<-"WD"
test_df$Functional[is.na(test_df$Functional)]<-"Typ"
test_df$Exterior1st[is.na(test_df$Exterior1st)]<-"VinylSd"
test_df$Exterior2nd[is.na(test_df$Exterior2nd)]<-"VinylSd"
test_df$Utilities[is.na(test_df$Utilities)]<-"AllPub"
test_df$MSZoning[is.na(test_df$MSZoning)]<-"RL"
######################
sum(is.na(test_df))

##############model
lmFit <- train( SalePrice ~ ., data = train_df, method ="lm")
summary(lmFit)
lmFit_aov<-aov(SalePrice ~ ., data = train_df)
summary(lmFit_aov)
predictedVal<-predict(lmFit,test_df)     
predictedVal_df<-data.frame(predictedVal)  
predictedVal_df
###################################################################################
residuals<-resid(lmFit)
plot(train_df$SalePrice,residuals)
abline(0,0)
plot(train_df$SalePrice,predictedValues)
varImp(lmFit)
plot(varImp(lmFit))
############################3
modfit <- train(SalePrice~.,method="rpart",data=train_df) 
summary(modfit)
###########################
lmfit_model<-train(SalePrice~X2ndFlrSF+X1stFlrSF+RoofMatlWdShake+Condition2PosN+BsmtFinSF1
                   +KitchenQualGd+LotArea+OverallCond
                   +OverallQual+KitchenQualTA         
                   +BsmtQualGd+NeighborhoodStoneBr+BsmtExposureGd+PoolArea+`RoofMatlTar&Grv`+
                     RoofMatlMembran+RoofMatlRoll+RoofMatlMetal,data=train_df,methods="lm")


