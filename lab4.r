require('e1071')
library('caret')


path = "F:/Google Drive/USACH/Nivel 8/Analisis de datos/lab3/hepatitis.data"
#path = "~/Documentos/AnalisisDatosLab4/hepatitis.data"
hepatitis <- read.table(path,sep=",", na.strings = c("?"))

names <- c("CLASS","AGE","SEX","STEROID","ANTIVIRALS","FATIGUE","MALAISE",
           "ANOREXIA","LIVER_BIG","LIVER_FIRM","SPLEEN_PALPABLE","SPIDERS",
           "ASCITES","VARICES","BILIRUBIN","ALK_PHOSPHATE","SGOT","ALBUMIN",
           "PROTIME","HISTOLOGY")

colnames(hepatitis) <- names

hepatitis.without.na <- na.omit(hepatitis)
data = as.data.frame(hepatitis.without.na)
data[names] <- lapply(data[names], factor)

#data = select(hepatitis.without.na,-CLASS)
print("factor")
set.seed(2019)

trainIndex=createDataPartition(data$CLASS, p=0.7)$Resample1
train=data[trainIndex, ]
test=data[-trainIndex, ]

## check the balance
print(table(data$CLASS))
print(table(train$CLASS))

print("fin tabla")
model = naiveBayes(CLASS ~ ., train)

trainPred=predict(model, train)
trainTable=table(train$CLASS, trainPred)
testPred=predict(model,test)
testTable=table(test$CLASS, testPred)
print("predict listo")
trainAcc=(trainTable[1,1]+trainTable[2,2])/sum(trainTable)
testAcc=(testTable[1,1]+testTable[2,2])/sum(testTable)
message("Contingency Table for Training Data")
print(trainTable)
message("Contingency Table for Test Data")
print(testTable)
message("Accuracy")
print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
