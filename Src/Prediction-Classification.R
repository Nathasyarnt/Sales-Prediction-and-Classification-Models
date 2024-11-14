library(readxl)
training <- read_xlsx("C:/Users/IKA ARIANTO/OneDrive - Institut Teknologi Sepuluh Nopember/College.7/Machine Learnig/Project Roro Jongrang/training.xlsx")
View(training)
head(training)


#uji signifikansi
cor.test(training$Amount,training$V1,method = "pearson")
cor.test(training$Amount,training$V2,method = "pearson")
cor.test(training$Amount,training$V3,method = "pearson")
cor.test(training$Amount,training$V4,method = "pearson")
cor.test(training$Amount,training$V5,method = "pearson")
cor.test(training$Amount,training$V6,method = "pearson")
cor.test(training$Amount,training$V7,method = "pearson")
cor.test(training$Amount,training$V8,method = "pearson")
cor.test(training$Amount,training$V9,method = "pearson")
cor.test(training$Amount,training$V10,method = "pearson")
cor.test(training$Amount,training$V11,method = "pearson")
cor.test(training$Amount,training$V12,method = "pearson")
cor.test(training$Amount,training$V13,method = "pearson")
cor.test(training$Amount,training$V14,method = "pearson")
cor.test(training$Amount,training$V15,method = "pearson")
cor.test(training$Amount,training$V16,method = "pearson")
cor.test(training$Amount,training$V17,method = "pearson")
cor.test(training$Amount,training$V18,method = "pearson")
cor.test(training$Amount,training$V19,method = "pearson")
cor.test(training$Amount,training$V20,method = "pearson")
cor.test(training$Amount,training$V21,method = "pearson")
cor.test(training$Amount,training$V22,method = "pearson")
cor.test(training$Amount,training$V23,method = "pearson")
cor.test(training$Amount,training$V24,method = "pearson")
cor.test(training$Amount,training$V25,method = "pearson")
cor.test(training$Amount,training$V26,method = "pearson")
cor.test(training$Amount,training$V27,method = "pearson")
cor.test(training$Amount,training$V28,method = "pearson")

#panggil data baru (variabel tidak signifikan sudah dihapus)
trainingsigni <- subset(training, select = -c(No., Time, V9, V12, V13, V14, V16, V17, V18, V19, V21, V22, V24, V25, V28))
View(trainingsigni)
head(trainingsigni)
str(trainingsigni)

#cek jumlah fraud
p <- table(trainingsigni$Class)
prop.table(table(trainingsigni$Class))
histogram(trainingsigni$Class)

#mengatasi imbalanced dataa
library(DMwR)
set.seed(20)
smote_dataset <- as.data.frame(trainingsigni)
smote_dataset$Class <- as.factor(smote_dataset$Class)
smote_round1 <- DMwR::SMOTE(form = Class ~., data = smote_dataset, perc.over = 10000, perc.under = 110)
smote_round1

#cek jumlah fraud setelah imbalanced diatasi
q <- table(smote_round1$Class)
prop.table(q)
histogram(smote_round1$Class)

#membagi data menjadi 80:20
library(caret)
sample_size <- 0.8
train_index <- createDataPartition(smote_round1$Amount, p = sample_size, list = FALSE)
train_data <- smote_round1[train_index, ]
test_data <- smote_round1[-train_index, ]
View(train_data)
View(test_data)

#no 1 - regresi linear 
trainpred <- subset(train_data, select = -c(Class))
library(lmtest)
rlb <- lm(trainpred$Amount~V1 + V2+ V3 + V4 + V5 + V6 + V7 + V8 + V10 + V11 + V15 + V20 + V23 + V26 + V27, data = trainpred )
rlb
summary(rlb)

#data testing regresi
testpred <- subset(test_data, select = -c(Class))
View(testpred)
pred_Reg <- predict(rlb, testpred)
eror <- (pred_Reg-testpred$Amount)
MSE <- mean((eror^2))
RMSE <- sqrt(MSE)
MSE
RMSE

SSR <- sum((testpred$Amount - pred_Reg)^2)
SST <- sum((testpred$Amount - mean(testpred$Amount))^2)
RSQ <- 1-(SSR/SST)
RSQ

#nomor 2 - support vector regression
library(e1071)
svr <- svm(trainpred$Amount~V1 + V2+ V3 + V4 + V5 + V6 + V7 + V8 + V10 + V11 + V15 + V20 + V23 + V26 + V27,data=trainpred,kernel="linear", cost = 1)
svr
summary(svr)

#data testing svr
pred_Reg <- predict(svr, testpred)
eror <- (pred_Reg-testpred$Amount)
MSE <- mean((eror^2))
RMSE <- sqrt(MSE)
MSE
RMSE

SSR <- sum((testpred$Amount - pred_Reg)^2)
SST <- sum((testpred$Amount - mean(testpred$Amount))^2)
RSQ <- 1-(SSR/SST)
RSQ


#soal 1 - random forest regression
install.packages("randomForest")
library(randomForest)
rfr <- randomForest(trainpred$Amount~V1 + V2+ V3 + V4 + V5 + V6 + V7 + V8 + V10 + V11 + V15 + V20 + V23 + V26 + V27,data=trainpred,kernel="linear", cost = 1)
rfr
summary(rfr)
plot(rfr)

#data testing RFR
pred_Reg <- predict(rfr, testpred)
eror <- (pred_Reg-testpred$Amount)
MSE <- mean((eror^2))
RMSE <- sqrt(MSE)
MSE
RMSE

SSR <- sum((testpred$Amount - pred_Reg)^2)
SST <- sum((testpred$Amount - mean(testpred$Amount))^2)
RSQ <- 1-(SSR/SST)
RSQ

#=======================================================================================================================================================================


#soal 2 - regresi logistik biner
install.packages("glmnet")
trainklas <- subset(train_data, select = -c(Amount))
View(trainklas)
library(glmnet)
reglogbin <- glm(trainklas$Class~V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + V11 + V15 + V20 + V23 + V26 + V27,data=trainklas,family = "binomial")
reglogbin

#conf matrix regresi logistik biner
testklas <- subset(test_data, select = -c(Amount))
View(testklas)
library(caret)
prediksi <- predict(reglogbin,newdata = testklas, type = "response")
threshold <- 0.5
prediksi_class <- ifelse(prediksi>threshold,1,0)
faktor1 <- as.factor(prediksi_class)
faktor2 <- as.factor(testklas$Class)
conf_matrix <- confusionMatrix(faktor1,faktor2)
conf_matrix

#akurasi dari regresi logistik biner
akurasi <- conf_matrix$overall[["Accuracy"]]
cat("akurasi model:", akurasi, "\n")

#soal 2 - support vector machine
install.packages("e1071")
library(e1071)
klasifikasi <- svm(formula =  trainklas$Class~., data = trainklas, type = 'C-classification', kernel = 'linear')
klasifikasi
  
#conf matrix svm
library(caret)
prediksi1 <- predict(klasifikasi,newdata = testklas)
con_mat <- confusionMatrix(prediksi1, testklas$Class)
con_mat

#akurasi dari support vector machine
akurasi1 <- con_mat$overall[["Accuracy"]]
cat("akurasi model:", akurasi1, "\n")


#soal nomor 2 - Decision Tree
install.packages("partykit")
library(partykit)
dt <- ctree(trainklas$Class~., data = trainklas)
plot(dt)

#conf matrix Decision Tree
library(caret)
prediksi3 <- predict(dt,newdata = testklas)
con_mat <- confusionMatrix(prediksi3, testklas$Class, positive = "0")
con_mat

#akurasi dari Decision Tree
akurasi3 <- con_mat$overall[["Accuracy"]]
cat("akurasi model:", akurasi3, "\n")

#soal 2 - random forest classification
library(randomForest)
rfc <- randomForest(trainklas$Class~., data = trainklas, importance = TRUE, proximinity = TRUE)
rfc
plot(rfc)


prediksi2 <- predict(rfc, testklas)
akurasi2 <- mean(prediksi2 == testklas$Class)

cat("akurasi moden random forest:", akurasi2*100, "%\n")



#========================================================================================================================================================================#
#memanggil data testing yang variabelnya sudah dihapus
testing <- read_excel("C:/Users/IKA ARIANTO/OneDrive - Institut Teknologi Sepuluh Nopember/College.7/Machine Learnig/Project Roro Jongrang/Testing.xlsx")
View(testing)
testingsigni <- subset(testing, select = -c(No., Time, V9, V12, V13, V14, V16, V17, V18, V19, V21, V22, V24, V25, V28))
View(testingsigni)


# random forest terpilih menjadi model dengan RSQ paling tinggi
# cek rabdom forest menggunakan data testing yang variabelnya sudah dihapus
#soal 1 - random forest regression
install.packages("randomForest")
library(randomForest)
rfr <- randomForest(trainpred$Amount~V1 + V2+ V3 + V4 + V5 + V6 + V7 + V8 + V10 + V11 + V15 + V20 + V23 + V26 + V27,data=trainpred,kernel="linear", cost = 1)
rfr
summary(rfr)
plot(rfr)

#data testing RFR
testactpred <- subset(testingsigni, select = -c(Class))
View(testactpred)
pred_Reg <- predict(rfr, testactpred)
eror <- (pred_Reg-testactpred$Amount)
MSE <- mean((eror^2))
RMSE <- sqrt(MSE)
MSE
RMSE

SSR <- sum((testactpred$Amount - pred_Reg)^2)
SST <- sum((testactpred$Amount - mean(testactpred$Amount))^2)
RSQ <- 1-(SSR/SST)
RSQ

#hasil prediksi
hasil <- data.frame(Actual = testactpred$Amount, Predicted = pred_Reg)
View(hasil)
write.csv(hasil, file = "hasil_prediksi regresii.csv", row.names = FALSE)

#metode klasifikasi terpilih adalah reglog biner, SVM, decision tree dan random forest, nilai akurasi pada metode tersebut sebesar 1

# cek metode reglog biner menggunakan data testing yang variabelnya sudah dihapus
library(glmnet)
reglogbin <- glm(trainklas$Class~V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + V11 + V15 + V20 + V23 + V26 + V27,data=trainklas,family = "binomial")
reglogbin

#conf matrix regresi logistik biner
library(caret)
testactklas <- subset(testingsigni, select = -c(Amount))
View(testactklas)
prediksiy <- predict(reglogbin,newdata = testactklas, type = "response")
threshold <- 0.5
prediksi_class <- ifelse(prediksiy>threshold,1,0)
faktor1 <- as.factor(prediksi_class)
faktor2 <- as.factor(testactklas$Class)
conf_matrix <- confusionMatrix(faktor1,faktor2)
conf_matrix

# cek metode SVM menggunakan data testing yang variabelnya sudah dihapus
library(e1071)
klasifikasi <- svm(formula =  trainklas$Class~., data = trainklas, type = 'C-classification', kernel = 'linear')
klasifikasi

#conf matrix svm
library(caret)
prediksi1 <- predict(klasifikasi,newdata = testactklas)
faktor3 <- as.factor(prediksi1)
con_mat <- confusionMatrix(faktor3, faktor2)
con_mat

#akurasi dari support vector machine
akurasi1 <- con_mat$overall[["Accuracy"]]
cat("akurasi model:", akurasi1, "\n")


# cek metode randomforest menggunakan data testing yang variabelnya sudah dihapus
library(randomForest)
rfc <- randomForest(trainklas$Class~., data = trainklas, importance = TRUE, proximinity = TRUE)
rfc
plot(rfc)

library(caret)
prediksirfc <- predict(rfc,newdata = testactklas)
faktor5 <- as.factor(prediksirfc)
con_matrfc <- confusionMatrix(faktor5, faktor2)
con_matrfc

#akurasi dari support vector machine
akurasi5 <- con_matrfc$overall[["Accuracy"]]
cat("akurasi model:", akurasi5, "\n")

# cek metode decision tree menggunakan data testing yang variabelnya sudah dihapus
library(partykit)
dt <- ctree(trainklas$Class~., data = trainklas)
plot(dt)

#conf matrix Decision Tree
library(caret)
prediksi3 <- predict(dt,newdata = testactklas)
con_mat <- confusionMatrix(prediksi3, faktor2)
con_mat

#akurasi dari Decision Tree
akurasi3 <- con_mat$overall[["Accuracy"]]
cat("akurasi model:", akurasi3, "\n")





#setelah dicek menggunakan data testing, diperoleh hasil akurasi terbesar pada metode reglog biner
# hasil klasifikasi model reglog biner
hasilc <- data.frame(Actual = testactklas$Class, Predicted = faktor1)
View(hasilc)
write.csv(hasilc, file = "hasil_prediksi klasifikasi.csv", row.names = FALSE)
