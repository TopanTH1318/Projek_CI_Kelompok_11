# Membaca data

# Praproses data
## Menghapus kolom ID
data_mentah <- read.table("breast-cancer-wisconsin.data.txt", header = F, sep = ",")
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

## Memisahkan kelas benign dan malignant dari data
benign <- subset(data_mentah, V11 == 2)
malignant <- subset(data_mentah, V11 == 4)

## Melakukan pemisahan data train dan data test
set.seed(1234)
jumlah_index <- floor(0.5 * nrow(data))
index <- sample(seq_len(nrow(data)), size = jumlah_index)
train <- data[index,]
test <- data[-index,]
benign <- subset(train, V11 == 2)
malignant <- subset(train, V11 == 4)

## Vektor pewakil
## Pewakil = Mengambil salah satu baris acak dari setiap kelas data train untuk menjadi vektor pewakil
## Pewakil2 = Mengambil sebanyak 3 baris acak dari seluruh data train untuk menjadi vektor pewakil
indvec_benign <- sample(seq_len(nrow(benign)), 1)
indvec_malignant <- sample(seq_len(nrow(malignant)), 1)

vec_benign <- benign[indvec_benign,]
vec_malignant <- malignant[indvec_malignant,]
pewakil <- rbind(vec_benign, vec_malignant)

ind_acak <- sample(seq_len(nrow(train)),3)
pewakil2 <- train[ind_acak,]

## Inisiasi 
hasil <- c()
a <- c()
index_hasil <- c()
alfa <- 0.4
beta <- 0.04
iterasi <- 100
benar <- 0
index_hasiltest <- c()

# LVQ Training
for (k in 1:iterasi) {
  for (i in 1:nrow(train)) {
    # Perhitungan Euclid satu baris data train dengan setiap vektor pewakil
    for (j in 1:2) {
      sum = ((train[i,]$V2 - pewakil[j,]$V2)^2)+
        ((train[i,]$V3 - pewakil[j,]$V3)^2)+
        ((train[i,]$V4 - pewakil[j,]$V4)^2)+
        ((train[i,]$V5 - pewakil[j,]$V5)^2)+
        ((train[i,]$V6 - pewakil[j,]$V6)^2)+
        ((train[i,]$V7 - pewakil[j,]$V7)^2)+
        ((train[i,]$V8 - pewakil[j,]$V8)^2)+
        ((train[i,]$V9 - pewakil[j,]$V9)^2)+
        ((train[i,]$V10 - pewakil[j,]$V10)^2)
      hasil[j] = sqrt(sum)
    }
    
    # Mengambil index dari hasil perhitungan Euclid yang minimum
    index_hasil[i] <- which.min(hasil)
    a[i] <- hasil[index_hasil[i]] 
    
    # Kondisi jika kelas data train sama dengan kelas vektor pewakil yang perhitungan Euclid nya minimum 
    if (train[i,]$V11 == pewakil[index_hasil[i],]$V11){
      
      # Update vector pewakil yang perhitungan Euclid nya minimum 
      pewakil[index_hasil[i],]$V2 <- pewakil[index_hasil[i],]$V2 + 
        (alfa*(train[i,]$V2 - pewakil[index_hasil[i],]$V2))
      pewakil[index_hasil[i],]$V3 <- pewakil[index_hasil[i],]$V3 + 
        (alfa*(train[i,]$V3 - pewakil[index_hasil[i],]$V3))
      pewakil[index_hasil[i],]$V4 <- pewakil[index_hasil[i],]$V4 + 
        (alfa*(train[i,]$V4 - pewakil[index_hasil[i],]$V4))
      pewakil[index_hasil[i],]$V5 <- pewakil[index_hasil[i],]$V5 + 
        (alfa*(train[i,]$V5 - pewakil[index_hasil[i],]$V5))
      pewakil[index_hasil[i],]$V6 <- pewakil[index_hasil[i],]$V6 + 
        (alfa*(train[i,]$V6 - pewakil[index_hasil[i],]$V6))
      pewakil[index_hasil[i],]$V7 <- pewakil[index_hasil[i],]$V7 + 
        (alfa*(train[i,]$V7 - pewakil[index_hasil[i],]$V7))
      pewakil[index_hasil[i],]$V8 <- pewakil[index_hasil[i],]$V8 + 
        (alfa*(train[i,]$V8 - pewakil[index_hasil[i],]$V8))
      pewakil[index_hasil[i],]$V9 <- pewakil[index_hasil[i],]$V9 + 
        (alfa*(train[i,]$V9 - pewakil[index_hasil[i],]$V9))
      pewakil[index_hasil[i],]$V10 <- pewakil[index_hasil[i],]$V10 + 
        (alfa*(train[i,]$V10 - pewakil[index_hasil[i],]$V10))
    } 
    
    # Kondisi jika kelas data train tidak sama dengan kelas vektor pewakil yang perhitungan Euclid nya minimum 
    else {
      
      # Update vector pewakil yang perhitungan Euclid nya minimum 
      pewakil[index_hasil[i],]$V2 <- pewakil[index_hasil[i],]$V2 - 
        (alfa*(train[i,]$V2 - pewakil[index_hasil[i],]$V2))
      pewakil[index_hasil[i],]$V3 <- pewakil[index_hasil[i],]$V3 - 
        (alfa*(train[i,]$V3 - pewakil[index_hasil[i],]$V3))
      pewakil[index_hasil[i],]$V4 <- pewakil[index_hasil[i],]$V4 - 
        (alfa*(train[i,]$V4 - pewakil[index_hasil[i],]$V4))
      pewakil[index_hasil[i],]$V5 <- pewakil[index_hasil[i],]$V5 - 
        (alfa*(train[i,]$V5 - pewakil[index_hasil[i],]$V5))
      pewakil[index_hasil[i],]$V6 <- pewakil[index_hasil[i],]$V6 - 
        (alfa*(train[i,]$V6 - pewakil[index_hasil[i],]$V6))
      pewakil[index_hasil[i],]$V7 <- pewakil[index_hasil[i],]$V7 - 
        (alfa*(train[i,]$V7 - pewakil[index_hasil[i],]$V7))
      pewakil[index_hasil[i],]$V8 <- pewakil[index_hasil[i],]$V8 - 
        (alfa*(train[i,]$V8 - pewakil[index_hasil[i],]$V8))
      pewakil[index_hasil[i],]$V9 <- pewakil[index_hasil[i],]$V9 - 
        (alfa*(train[i,]$V9 - pewakil[index_hasil[i],]$V9))
      pewakil[index_hasil[i],]$V10 <- pewakil[index_hasil[i],]$V10 - 
        (alfa*(train[i,]$V10 - pewakil[index_hasil[i],]$V10))
    }
  }
  
  # Update alfa
  alfa <- beta * alfa
}

index_hasil

# Testing data test
for (i in 1:nrow(test)) {
  # Perhitungan Euclid satu baris data train dengan setiap vektor pewakil
  for (j in 1:2) {
    sum = ((test[i,]$V2 - pewakil[j,]$V2)^2)+
      ((test[i,]$V3 - pewakil[j,]$V3)^2)+
      ((test[i,]$V4 - pewakil[j,]$V4)^2)+
      ((test[i,]$V5 - pewakil[j,]$V5)^2)+
      ((test[i,]$V6 - pewakil[j,]$V6)^2)+
      ((test[i,]$V7 - pewakil[j,]$V7)^2)+
      ((test[i,]$V8 - pewakil[j,]$V8)^2)+
      ((test[i,]$V9 - pewakil[j,]$V9)^2)+
      ((test[i,]$V10 - pewakil[j,]$V10)^2)
    hasil[j] = sqrt(sum)
  }
  
  # Mengambil index dari hasil perhitungan Euclid yang minimum
  index_hasiltest[i] <- which.min(hasil)
  a[i] <- hasil[index_hasiltest[i]]
  
  # Kondisi jika kelas data test sama dengan kelas vektor pewakil yang perhitungan Euclid nya minimum
  if (pewakil[index_hasiltest[i],]$V11== test[i,]$V11){
    benar = benar + 1
  }
}

# Menghitung jumlah benar terklasifikasi dan akurasi
benar
akurasi <- (benar / nrow(test))*100
akurasi
