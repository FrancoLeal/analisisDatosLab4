require('e1071')

#path = "F:/Google Drive/USACH/Nivel 8/Analisis de datos/lab2/hepatitis.data"
path = "~/Documentos/AnalisisDatosLab4/hepatitis.data"
hepatitis <- read.table(path,sep=",", na.strings = c("?"))

names <- c("CLASS","AGE","SEX","STEROID","ANTIVIRALS","FATIGUE","MALAISE",
           "ANOREXIA","LIVER_BIG","LIVER_FIRM","SPLEEN_PALPABLE","SPIDERS",
           "ASCITES","VARICES","BILIRUBIN","ALK_PHOSPHATE","SGOT","ALBUMIN",
           "PROTIME","HISTOLOGY")

colnames(hepatitis) <- names

hepatitis.without.na <- na.omit(hepatitis)
data = hepatitis.without.na
#data = select(hepatitis.without.na,-CLASS)
naiveBayes(CLASS ~ ., data)
