###################################
######  WALMART Trip Type  ######## 
###################################

#Multi-class logarithmic loss achieved: 0.72908 in Private Leaderboard. Almost top 25%

##Read files

        setwd("Your working directory")
        train <- read.csv("train.csv",stringsAsFactors = F)
        test <- read.csv("test.csv",stringsAsFactors = F)
        test$TripType <- rep(NA,dim(test)[1])
        walmart <- rbind(train,test)
        
##What is he buying?
        
        #Get first five digits and the sixth digit of UPC
        
        library(devtools)
        library(stringr)
        walmart$Upc <- with_options(c(scipen = 999), str_pad(walmart$Upc, 12, pad = "0")) #Fill with zeros
        
        walmart$FiveDigitsUpc <- sapply(walmart$Upc,function(x) substr(x,1,5))
        walmart$SixthDigitUpc <- sapply(walmart$Upc,function(x) substr(x,6,6))
        
        MissingUPC <- which(is.na(walmart$Upc))
        walmart[MissingUPC,c("FiveDigitsUpc","SixthDigitUpc")] <- "MISSING"
        walmart[MissingUPC,"FinelineNumber"] <- "MISSING"
        
        #Aggregate by VisitNumber using data table (faster than aggregate function)
        
        walmart$FiveDigitsUpc <- factor(walmart$FiveDigitsUpc)
        MUpc <- walmart[,c("VisitNumber","FiveDigitsUpc")]
        MUpc <- data.table(MUpc)
        MUpc <- MUpc[, lapply(.SD, function(x)levels(x)[which.max(table(x))]), by = VisitNumber]#Most repeated
        MUpc <- data.frame(VisitNumber=MUpc$VisitNumber,MostFiveDigit=MUpc$FiveDigitsUpc)
        
        walmart$SixthDigitUpc <- factor(walmart$SixthDigitUpc)
        SixthDigit <- walmart[,c("VisitNumber","SixthDigitUpc")]
        SixthDigit <- data.table(SixthDigit)
        SixthDigit <- SixthDigit[, lapply(.SD, function(x)levels(x)[which.max(table(x))]), by = VisitNumber]
        SixthDigit <- data.frame(VisitNumber=SixthDigit$VisitNumber,MostSixthDigit=SixthDigit$SixthDigitUpc)
        
        #Get the most repeated Fineline number for each visit number
        
        walmart$FinelineNumber <- factor(walmart$FinelineNumber)
        MaxFineline <- walmart[,c("VisitNumber","FinelineNumber")]
        MaxFineline <- data.table(MaxFineline)
        MaxFineline <- MaxFineline[, lapply(.SD, function(x)levels(x)[which.max(table(x))]), by = VisitNumber]
        MaxFineline <- data.frame(VisitNumber=MaxFineline$VisitNumber,MaxFineline= MaxFineline$FinelineNumber)
        
##How is he buying?
        
        #Number of visits
        
        Visits <- data.frame(table(walmart$VisitNumber))
        colnames(Visits) <- c("VisitNumber","VisitsCountperVNum")
        Visits$VisitNumber <- as.numeric(levels(Visits$VisitNumber))[Visits$VisitNumber] #factor to numeric 
        walmart <- merge(walmart,Visits,by="VisitNumber") 
        
        #Number of purchases - Number of returns
        
        ScanCountSum <- aggregate(walmart$ScanCount ~ walmart$VisitNumber,FUN = sum)
        colnames(ScanCountSum) <- c("VisitNumber","SumScanCount")
        walmart <- merge(walmart,ScanCountSum,by = "VisitNumber")
        
        #Number of returns
        
        SumSCReturns <-aggregate(abs(walmart$ScanCount < 0) ~ walmart$VisitNumber,FUN = sum)
        colnames(SumSCReturns) <- c("VisitNumber","SumReturns")
        walmart <- merge(walmart,SumSCReturns,by = "VisitNumber")

        
        #Visits - Purchases
        
        walmart$DiffVisitScan <- walmart$VisitsCountperVNum - walmart$SumScanCount
        
        #Purchases/Visits
        
        walmart$BuyPercentage <- walmart$SumScanCount / walmart$VisitsCountperVNum 
        
        #Returns/Visits
        
        walmart$ReturnPercentage <- walmart$SumReturns / walmart$VisitsCountperVNum
        
        #Returns/Purchases ratio
        
        walmart$ReturnPercentageScan <- walmart$SumReturns / walmart$SumScanCount
        
        #Sum unique categories(Fineline) for each visit number
        
         walmart$FinelineNumber <- factor(walmart$FinelineNumber)
         walmart2 <- rbind(train,test)
         library(data.table)
         Fl <- walmart2[,c("VisitNumber","FinelineNumber")]
         Fl <- data.table(Fl)
         Fl <- Fl[, lapply(.SD, function(x)length(unique(x))), by = VisitNumber]
         Fl <- data.frame(VisitNumber=Fl$VisitNumber,SumFineLine=Fl$FinelineNumber)
         
         #Sum of unique UPC (unique products)
         
         walmart[MissingUPC,"Upc"] <- "MISSING"
         walmart$Upc<- factor(walmart$Upc)
         
         walmart2 <- rbind(train,test)
         library(data.table)
         Upc <- walmart2[,c("VisitNumber","Upc")]
         Upc <- data.table(Upc)
         Upc <- Upc[, lapply(.SD, function(x)length(unique(x))), by = VisitNumber]
         Upc <- data.frame(VisitNumber=Upc$VisitNumber,SumUpc = Upc$Upc)
         
        #Aggregate the whole dataset by Visit number and add the new variables
         
         y <- unique(walmart[,c(1,3,10,11,12,13,14,15,16)])
         walmart <- merge(y,Upc,by = "VisitNumber")
         walmart <- merge(walmart,Fl,by = "VisitNumber")
         walmart <- merge(walmart,MUpc,by="VisitNumber")
         walmart <- merge(walmart,SixthDigit,by="VisitNumber")
         walmart <- merge(walmart,MaxFineline,by="VisitNumber")
         
         #Unique products / Total
         
         walmart$UniqueProductsPerc <- walmart$SumUpc/walmart$SumScanCount
         
         #Purchases - Unique products
         
         walmart$UniqueProductsDiff <- walmart$SumScanCount - walmart$SumUpc
         
         #Unique products/ Visits
         
         walmart$UniqueProductVisit <- walmart$SumUpc/walmart$VisitsCountperVNum
         
         #Unique categories / Purchases
         
         walmart$UniqueFinelinePerc <- walmart$SumFineLine/walmart$SumScanCount
         
         #Purchases - Unique categories
         
         walmart$UniqueFinelineDiff <- walmart$SumScanCount - walmart$SumFineLine
         
         #Categories / Visits
         
         walmart$UniqueFinelineVisit <- walmart$SumFineLine/walmart$VisitsCountperVNum
         
         #Unique products - Unique categories
         
         walmart$CatProdDiff <- walmart$SumUpc - walmart$SumFineLine
         
         #Most visited department per Visit Number
         
         walmart2$DepartmentDescription <- factor(walmart2$DepartmentDescription)
         MostVisitedDepartment <- aggregate(DepartmentDescription~VisitNumber,FUN = function(x)levels(x)[which.max(table(x))],data=walmart2)
         colnames(MostVisitedDepartment) <- c("VisitNumber","MostVisitedDepartment")
         walmart <- merge(walmart,MostVisitedDepartment,by="VisitNumber") #add
         
         #Sum of purchases by deparment
         
         walmart2 <- rbind(train,test)
         library(data.table)
         library(Matrix)
         VisitNumber <- walmart2$VisitNumber
         Dep <- walmart2[,c("VisitNumber","DepartmentDescription")]
         Depmatrix <- sparse.model.matrix(VisitNumber ~ .-1,data = Dep)
         Depdf <- data.table(VisitNumber,as.matrix(Depmatrix))
         Depdf <- Depdf[, lapply(.SD, sum), by = VisitNumber]
         walmart <- merge(walmart,Depdf,by="VisitNumber")
         rm(walmart2)
         rm(Dep)
         rm(Depdf)
         rm(Depmatrix)
         
         #Sum of rows
         
         walmart$DepSum <- rowSums(walmart[,23:91])
         
         #Other variables
         
         walmart$SumFin <- walmart$SumUpc /walmart$SumFineLine
         walmart$DiffRatio <- walmart$CatProdDiff / walmart$VisitsCountperVNum
         xtemp <- walmart[,23:91]/walmart$VisitsCountperVNum
         walmart[,23:91] <- xtemp
         rm(xtemp)
         
         #Final join
         
         walmart2 <- rbind(train,test)
         walmartTrip <- walmart2[,c(1,2)]
         rm(walmart2)
         Trips <- unique(merge(walmart,walmartTrip,by = "VisitNumber"))
         walmart <- Trips
         
##Index for data split
         
         Idx <- which(is.na(Trips$TripType))
         
##Text analysis in order to identificate products
         
         #Clean department description,one word=one product
         
         walmart$MostVisitedDepartment <- gsub("-","",walmart$MostVisitedDepartment)
         walmart$MostVisitedDepartment <- gsub("1","",walmart$MostVisitedDepartment)
         walmart$MostVisitedDepartment <- gsub("/"," ",walmart$MostVisitedDepartment)
         walmart$MostVisitedDepartment <- gsub("  "," ",walmart$MostVisitedDepartment)
         walmart$MostVisitedDepartment <- gsub(","," ",walmart$MostVisitedDepartment)
         walmart$MostVisitedDepartment <- gsub("AND","",walmart$MostVisitedDepartment)
         walmart$MostVisitedDepartment <- gsub("&","",walmart$MostVisitedDepartment)
         walmart$MostVisitedDepartment <- sub("SUPP","SUPPLIES",walmart$MostVisitedDepartment)
         walmart$MostVisitedDepartment <- sub("SUPPLIESLIES","SUPPLIES",walmart$MostVisitedDepartment)
         walmart$MostVisitedDepartment <- gsub(" 46X  74","",walmart$MostVisitedDepartment)
         walmart$MostVisitedDepartment <- gsub("WEAR"," WEAR",walmart$MostVisitedDepartment)
         
         #
         
         library(RTextTools)
         TM <- create_matrix(walmart$MostVisitedDepartment, language="english",minWordLength=1,stemWords=TRUE,toLower=TRUE,weighting = tm::weightTfIdf )
         TMm <- as.matrix(TM)
         #
         
         n1 <- rowSums(TMm)
         n2 <- rowMeans(TMm)
         TMm <- cbind(TMm,n1)
         TMm <- cbind(TMm,n2)
         n3 <- apply(TMm[,1:102],1,nnzero)#number of non-zeros
         TMm <- cbind(TMm,n3)
         
         
##Model
         #Labels
         
         num.class <- length(levels(factor(train$TripType)))
         labels <- factor(Trips[-Idx,"TripType"])
         levels(labels) <- 1:num.class
         y <- as.matrix(as.integer(labels)-1) #xgboost [0,X]
         
         #Sparse matrix
         
         library(Matrix)
         walmart[which(is.na(walmart$TripType)),"TripType"] <- 111111
         walmart$VisitNumber <- NULL
         colnames(walmart) <- make.names(colnames(walmart))
         matrix <- sparse.model.matrix(TripType ~ .-1 ,data = walmart)
         
         #Final traininig set and test set
         
         matrix.train <- matrix[-Idx,]
         matrix.test <- matrix[Idx,]
         matrix.train <- cbind(matrix.train,TMm[-Idx,])
         matrix.test <- cbind(matrix.test,TMm[Idx,])
         
         #Parameters
         
         library(xgboost)
         param <- list("objective" = "multi:softprob",
                       "num_class" = num.class,
                       "eval_metric"="mlogloss",
                       "eta" = 0.05, #0.025
                       "gamma" = 0,
                       "colsample_bytree" = 0.5)
         
         #Cross validation
         
         set.seed(111)
         nround.cv =1500
         cv <- xgb.cv(param = param,data=matrix.train,label=y,nfold=3,nrounds= nround.cv,predictions = T,verbose = T)
         cv
         min.error <- which.min(cv[,test.mlogloss.mean])
         
         #Model and predictions
         
         model1 <- xgboost(param = param,data = matrix.train,label = y,nrounds = min.error)
         pred <- predict(model1,matrix.test)
         pred <- matrix(pred,nrow=num.class,ncol = length(pred)/num.class)
         pred <- t(pred)
         
         #Submission
         
         submission <- data.frame(Trips[Idx,1],pred)
         colnames(submission) <- c("VisitNumber","TripType_3","TripType_4","TripType_5","TripType_6","TripType_7","TripType_8","TripType_9","TripType_12","TripType_14","TripType_15","TripType_18","TripType_19","TripType_20","TripType_21","TripType_22","TripType_23","TripType_24","TripType_25","TripType_26","TripType_27","TripType_28","TripType_29","TripType_30","TripType_31","TripType_32","TripType_33","TripType_34","TripType_35","TripType_36","TripType_37","TripType_38","TripType_39","TripType_40","TripType_41","TripType_42","TripType_43","TripType_44","TripType_999")
         dim(submission)[1] == 95674 #check
         write.csv(submission,"name.csv",row.names = FALSE)
         
##Variable importance plot
         
         model <- xgb.dump(model1,with.stats = TRUE)
         names <- dimnames(matrix.train)[[2]]
         importance <- xgb.importance(names,model = model1)
         p <- xgb.plot.importance(importance[1:10])
         print(p)         
         