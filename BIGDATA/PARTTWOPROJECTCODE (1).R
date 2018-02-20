setwd("C:/Users/Yicheng/Desktop/SASData")
data <- read.csv("ClaimsDataFull2017.csv", stringsAsFactors = FALSE)
str(data)
head(data$ReturnToWorkDate)
factordata <- read.csv("ClaimsDataFull2017.csv")
str(factordata)
data1 <- data[data$ClaimantAge_at_DOI != "NULL",]
#data$TotalTime <- data$ClaimClosedDate - data$ClaimOpenedDate
as.Date(data$ClaimOpenedDate, "%m/%d/%Y")
data$TotalTime <- as.Date(data$ClaimClosedDate, "%m/%d/%Y") - as.Date(data$ClaimOpenedDate, "%m/%d/%Y")
data$TotalTime <- as.numeric(data$TotalTime)
write.csv(data, file = "TotalData", sep = ",")
data$TotalLoss <- ((data$TotalTime/7)*(as.numeric(data$AverageWeeklyWage))) - data$TotalPaid
length(which(!is.na(data$TotalLoss)))
data$TotalCostPerDay <- data$TotalPaid/data$TotalTime
data2 <- data[which(!is.na(data$TotalLoss)),]
data2$TotalCostPerDay
data3 <- data2[which(!is.na(data2$TotalCostPerDay)),]
data4 <- data3[data3$ClaimantAge_at_DOI != "NULL",]
data5 <- data4[data4$ClaimantAge_at_DOI > 17,]
write.csv(data5, file = "TotalData", sep = ",")
summary(data5$TotalTime)
data5$Critical <- rep(0, 33983)
crits <- which(data5$TotalPaid > 3470.0)
data5$Critical[crits] <- 1
selected <- data5[which(data5$Critical == 1),]
selected$BodyPart
library(rpart)
fit <- rpart(Critical ~ BodyPart + InjuryNature + TotalTime,
             method="class", data=data5)

printcp(fit)  
plotcp(fit) 
summary(fit)
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

