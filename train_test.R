
#### read the train and test data

train_raw<-read.csv("E:/mayuri kenjale/dataset/Group Project on R-Data Set-4.csv")
test_raw<-read.csv("E:/mayuri kenjale/dataset/Group Project on R-Data Set-3.csv")
dim(train_raw)
dim(test_raw)
summary(train_raw)

#### delete the columns which have the more than 40% error

train_raw_nan<-train_raw[,-c(7,58,73,74,75)]
sum(is.na(train_raw_nan))
dim(train_raw_nan)

####checking for errors

####  na values relaced by mean (continuous data)

train_raw_nan$GarageYrBlt[is.na(train_raw_nan$GarageYrBlt)]<-mean(train_raw_nan$GarageYrBlt,na.rm=TRUE)
train_raw_nan$LotFrontage[is.na(train_raw_nan$LotFrontage)]<-mean(train_raw_nan$LotFrontage,na.rm=TRUE)
train_raw_nan$MasVnrArea[is.na(train_raw_nan$MasVnrArea)]<-mean(train_raw_nan$MasVnrArea,na.rm=TRUE)
sum(is.na(train_raw_nan))

####categorical data replaced by mode

train_raw_nan$GarageQual[is.na(train_raw_nan$GarageQual)]<-"TA"
train_raw_nan$GarageFinish[is.na(train_raw_nan$GarageFinish)]<-"Unf"
train_raw_nan$GarageType[is.na(train_raw_nan$GarageType)]<-"Attchd"
train_raw_nan$BsmtFinType1[is.na(train_raw_nan$BsmtFinType1)]<-"Unf"
train_raw_nan$BsmtFinType2[is.na(train_raw_nan$BsmtFinType2)]<-"Unf"
train_raw_nan$BsmtExposure[is.na(train_raw_nan$BsmtExposure)]<-"No"
train_raw_nan$BsmtCond[is.na(train_raw_nan$BsmtCond)]<-"TA"
train_raw_nan$BsmtQual[is.na(train_raw_nan$BsmtQual)]<-"TA"
train_raw_nan$MasVnrType[is.na(train_raw_nan$MasVnrType)]<-"None"
train_raw_nan$Electrical[is.na(train_raw_nan$Electrical)]<-"SBrkr"
train_raw_nan$GarageCond[is.na(train_raw_nan$GarageCond)]<-"TA"
sum(is.na(train_raw_nan))
summary(train_raw_nan)

