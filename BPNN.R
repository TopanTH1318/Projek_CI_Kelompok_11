# Membaca data 
data_mentah <- read.table("breast-cancer-wisconsin.data.txt", header = F, sep = ",")

# Praproses data
## Menghapus kolom ID
data <- data_mentah[,-1]

## Menghapus "?" pada kolom V7
x<-c()
j<-1
for(i in 1:nrow(data)){
  if(data[i,]$V7 == "?"){
    x[j]<-i
    j<-j+1
  }
}
data<-data[-c(x),]

## Mengubah kolom V7 dari vector menjadi integer
data$V7 <- as.integer(data$V7)
str(data)

# Normalisasi data
datatotal <- data[,-10]
kelas <- data[,10]
scaleddata <- scale(datatotal)
datatemp <- cbind(scaleddata, kelas)

# Pembagian data train dan data test
ind <- floor(0.8 * nrow(datatemp))
index <- sample(seq_len(nrow(datatemp)), size = ind)
train <- datatemp[index,]
test <- datatemp[-index,]

# BPNN dengan variasi hidden
library(neuralnet)
nn <- neuralnet(kelas ~ V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10, data = train, hidden = 10, threshold = 0.01, learningrate = 0.001, algorithm = "backprop", stepmax = 1e06)
nn$result.matrix
#plot(nn)

#Testing dengan model hasil neuralnett()
temp_test <- subset(test, select = c("V2","V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = test[,10], prediction = nn.results$net.result)

#output dan confusion matriks
results$prediction <- round(results$prediction)

table(results$actual,results$prediction)
