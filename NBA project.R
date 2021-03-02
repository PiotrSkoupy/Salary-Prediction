library(randomForest)
library(Metrics)
#Import Data and prepare it for further evaluation
nba <- read.csv('/Users/Piotrek/Desktop/nbaData.csv')
head(nba)
pliz <-read.csv('/Users/Piotrek/Desktop/nbaData.csv')

nba = subset(nba, select = -c(X,Â.,Â..1, Player))
head(nba)
is.na(nba)
nba <- na.omit(nba)

#Deleting Team Column as it has no influence on salary
nba = subset(nba, select = -c(Tm.X3PAr,DBPM))

#Encoding Positions to numbers and cleaning data from fake positions.
table(nba[["Pos"]], encode_ordinal(nba[["Pos"]]), useNA = "ifany")
nba<-nba[!(nba$Pos=="C-PF" | nba$Pos=="PF-C" | nba$Pos=="PF-SF" | nba$Pos=="PG-SG" | 
nba$Pos=="SF-C"| nba$Pos=="SF-PF"| nba$Pos=="SF-SG"| nba$Pos=="SG-PG"),]
table(nba[["Pos"]], encode_ordinal(nba[["Pos"]]), useNA = "ifany")
nba$Pos = factor(nba$Pos, levels = c('PG','SG', 'SF', 'PF','C'), labels =c(1,2,3,4,5))
nba$Pos <-as.numeric(nba$Pos)
nba = subset(nba, select = -c(Pos,DRB.,ORB.))


#redundant features
set.seed(7)
library(mlbench)
library(caret)
fortestsonly = subset(nba, select = -c(X2020.21))
correlationMatrix <- cor(fortestsonly)
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
print(highlyCorrelated)

#Split
dt = sort(sample(nrow(nba), nrow(nba)*.8))
train<-nba[dt,]
test<-nba[-dt,]

#Modelowanie
model <- randomForest(X2020.21 ~ ., data=train,mtry=12, proximity=TRUE, ntree=900, importance=TRUE)
prediction<-predict(model, newdata = test, proximties = TRUE)
prediction
plot(test$X2020.21,prediction, main="Wynik", sub='R2=0.6827',xlab="Kontrakt rzeczywisty[$]", ylab='Kontrakt przewidywany[$]', col='red')
print(importance)
model
cor(test$X2020.21,prediction)^2
mae(test$X2020.21,prediction)
rmse(test$X2020.21,prediction)
hist(abs(test$X2020.21-prediction), main='Histogram bledu', xlab='Wartosc pomylki', ylab='Ilosc pomylek', col='blue')
summary(prediction)
summary(model)
