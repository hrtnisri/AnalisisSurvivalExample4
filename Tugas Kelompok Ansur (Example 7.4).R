# install package
library(KMsurv) # dipakai untuk load data
library(matlib) # dipakai untuk mencari invers matriks

# load data Bone Marrow Transplantation dari package KMsurv
data(bmt)

# bagi data menjadi 3 populasi berbeda yaitu Grup 1, Grup 2, dan Grup 3
dataGrup1 <- bmt[which(bmt$group==1),]
dataGrup2 <- bmt[which(bmt$group==2),]
dataGrup3 <- bmt[which(bmt$group==3),]

# death or relapsed time dari Grup 1, Grup 2, dan Grup 3
dr_time1 <- sort(dataGrup1[which(dataGrup1$d3==1), ]$t2)
dr_time2 <- sort(dataGrup2[which(dataGrup2$d3==1), ]$t2)
dr_time3 <- sort(dataGrup3[which(dataGrup3$d3==1), ]$t2)

# alive or disease free time (censored time) dari Grup 1, Grup 2, dan Grup 3
ct_time1 <- sort(dataGrup1[which(dataGrup1$d3==0),]$t2)
ct_time2 <- sort(dataGrup2[which(dataGrup2$d3==0),]$t2)
ct_time3 <- sort(dataGrup3[which(dataGrup3$d3==0),]$t2)

# cari t_i
t_i <- sort(unique(c(dr_time1, dr_time2, dr_time3)))

# inisialisasi tabel
df <- data.frame(ti = integer(),
                 Yi_1 = integer(), 
                 di_1 = integer(), 
                 Yi_2 = integer(), 
                 di_2 = integer(), 
                 Yi_3 = integer(), 
                 di_3 = integer(), 
                 Yi = integer(), 
                 di = integer(), 
                 Yi1_di_Yi = integer(), 
                 statujiZ1 = integer(), 
                 variansiZ1 = integer(), 
                 Yi2_di_Yi = integer(), 
                 statujiZ2 = integer(), 
                 variansiZ2 = integer(), 
                 Yi3_di_Yi = integer(), 
                 statujiZ3 = integer(), 
                 variansiZ3 = integer(), 
                 kovarians12 = integer(), 
                 kovarians13 = integer(), 
                 kovarians21 = integer(), 
                 kovarians23 = integer(), 
                 kovarians31 = integer(), 
                 kovarians32 = integer(), 
                 stringsAsFactors=FALSE)

# masukkan nilai ti ke kolom pertama tabel
i <- 1
while (i <= length(t_i)) {
  df[i,] <- c(t_i[i], 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  i <- i+1
}

# cari Yi_1
n <- length(dr_time1) + length(ct_time1)
df$Yi_1[1] <- n
w <- 2
while (w <= length(t_i)) {
  n <- length(dr_time1) + length(ct_time1)
  h <- 0
  for (j in 1:length(ct_time1)) {
    if (df$ti[w] >= ct_time1[j]) {
      h <- h + 1
    }
  }
  for (k in 1:length(dr_time1)) {
    if (df$ti[w] >= dr_time1[k]) {
      h <- h + 1
    }
  }
  df$Yi_1[w] <- n-h
  w <- w + 1
}

# cari Yi_2
n <- length(dr_time2) + length(ct_time2)
df$Yi_2[1] <- n
w <- 2
while (w <= length(t_i)) {
  n <- length(dr_time2) + length(ct_time2)
  h <- 0
  for (j in 1:length(ct_time2)) {
    if (df$ti[w] >= ct_time2[j]) {
      h <- h + 1
    }
  }
  for (k in 1:length(dr_time2)) {
    if (df$ti[w] >= dr_time2[k]) {
      h <- h + 1
    }
  }
  df$Yi_2[w] <- n-h
  w <- w + 1
}

# cari Yi_3
n <- length(dr_time3) + length(ct_time3)
df$Yi_3[1] <- n
w <- 2
while (w <= length(t_i)) {
  n <- length(dr_time3) + length(ct_time3)
  h <- 0
  for (j in 1:length(ct_time3)) {
    if (df$ti[w] >= ct_time3[j]) {
      h <- h + 1
    }
  }
  for (k in 1:length(dr_time3)) {
    if (df$ti[w] >= dr_time3[k]) {
      h <- h + 1
    }
  }
  df$Yi_3[w] <- n-h
  w <- w + 1
}

# cari di_1
i <- 1
while (i <= length(t_i)) {
  h <- 0
  for (k in 1:length(dr_time1)) {
    if (df$ti[i] == dr_time1[k]) {
      h <- h + 1
    }
  } 
  df$di_1[i] <- h
  i <- i+1
}

# cari di_2
i <- 1
while (i <= length(t_i)) {
  h <- 0
  for (k in 1:length(dr_time2)) {
    if (df$ti[i] == dr_time2[k]) {
      h <- h + 1
    }
  } 
  df$di_2[i] <- h
  i <- i+1
}

# cari di_3
i <- 1
while (i <= length(t_i)) {
  h <- 0
  for (k in 1:length(dr_time3)) {
    if (df$ti[i] == dr_time3[k]) {
      h <- h + 1
    }
  } 
  df$di_3[i] <- h
  i <- i+1
}

# cari Yi dan di
for (i in 1:76) {
  df$Yi[i] <- df$Yi_1[i] + df$Yi_2[i] + df$Yi_3[i]
  df$di[i] <- df$di_1[i] + df$di_2[i] + df$di_3[i]
}

# cari Yi1_di_Yi, Yi2_di_Yi, Yi3_di_Yi
for (i in 1:76) {
  df$Yi1_di_Yi[i] <- df$Yi_1[i] * (df$di[i] / df$Yi[i])
  df$Yi2_di_Yi[i] <- df$Yi_2[i] * (df$di[i] / df$Yi[i])
  df$Yi3_di_Yi[i] <- df$Yi_3[i] * (df$di[i] / df$Yi[i])
}

# cari statujiZ1, statujiZ2, statujiZ3
for (i in 1:76) {
  df$statujiZ1[i] <- df$di_1[i] - df$Yi1_di_Yi[i]
  df$statujiZ2[i] <- df$di_2[i] - df$Yi2_di_Yi[i]
  df$statujiZ3[i] <- df$di_3[i] - df$Yi3_di_Yi[i]
}

# cari variansiZ1, variansiZ2, variansiZ3
for (i in 1:76) {
  df$variansiZ1[i] <- (df$Yi_1[i] / df$Yi[i]) * (1-(df$Yi_1[i] / df$Yi[i])) * ((df$Yi[i]-df$di[i])/(df$Yi[i]-1)) * df$di[i]
  df$variansiZ2[i] <- (df$Yi_2[i] / df$Yi[i]) * (1-(df$Yi_2[i] / df$Yi[i])) * ((df$Yi[i]-df$di[i])/(df$Yi[i]-1)) * df$di[i]
  df$variansiZ3[i] <- (df$Yi_3[i] / df$Yi[i]) * (1-(df$Yi_3[i] / df$Yi[i])) * ((df$Yi[i]-df$di[i])/(df$Yi[i]-1)) * df$di[i]
}

# cari nilai statistik uji Z1(2204), Z2(2204), Z3(2204)
Z1_2204 <- sum(df$statujiZ1)
Z2_2204 <- sum(df$statujiZ2)
Z3_2204 <- sum(df$statujiZ3)

# perhatikan bahwa jumlah Z1(2204), Z2(2204), Z3(2204) sama dengan 0
jumlah <- Z1_2204 + Z2_2204 + Z3_2204
jumlah

# cari variansi Z1, Z2, Z3
variansiZ1_2204 <- sum(df$variansiZ1)
variansiZ2_2204 <- sum(df$variansiZ2)
variansiZ3_2204 <- sum(df$variansiZ3)

# cari kovariansi
for (i in 1:76) {
  df$kovarians12[i] <- -1 * ((df$Yi_1[i] / df$Yi[i]) * (df$Yi_2[i] / df$Yi[i]) * ((df$Yi[i]-df$di[i])/(df$Yi[i]-1)) * df$di[i])
  df$kovarians13[i] <- -1 * ((df$Yi_1[i] / df$Yi[i]) * (df$Yi_3[i] / df$Yi[i]) * ((df$Yi[i]-df$di[i])/(df$Yi[i]-1)) * df$di[i])
  df$kovarians21[i] <- -1 * ((df$Yi_2[i] / df$Yi[i]) * (df$Yi_1[i] / df$Yi[i]) * ((df$Yi[i]-df$di[i])/(df$Yi[i]-1)) * df$di[i])
  df$kovarians23[i] <- -1 * ((df$Yi_2[i] / df$Yi[i]) * (df$Yi_3[i] / df$Yi[i]) * ((df$Yi[i]-df$di[i])/(df$Yi[i]-1)) * df$di[i])
  df$kovarians31[i] <- -1 * ((df$Yi_3[i] / df$Yi[i]) * (df$Yi_1[i] / df$Yi[i]) * ((df$Yi[i]-df$di[i])/(df$Yi[i]-1)) * df$di[i])
  df$kovarians32[i] <- -1 * ((df$Yi_3[i] / df$Yi[i]) * (df$Yi_2[i] / df$Yi[i]) * ((df$Yi[i]-df$di[i])/(df$Yi[i]-1)) * df$di[i])
}

# cari kovariansi Zj() dan Zg() dimana j!=g
kovariansiZ1_Z2 <- sum(df$kovarians12)
kovariansiZ1_Z3 <- sum(df$kovarians13)
kovariansiZ2_Z1 <- sum(df$kovarians21)
kovariansiZ2_Z3 <- sum(df$kovarians23)
kovariansiZ3_Z1 <- sum(df$kovarians31)
kovariansiZ3_Z2 <- sum(df$kovarians32)

# bentuk matriks sigma
# perhatikan matriks sigma merupakan matriks singular
matriks_sigma <- matrix(
  c(variansiZ1_2204, kovariansiZ1_Z2, kovariansiZ1_Z3, 
    kovariansiZ2_Z1, variansiZ2_2204, kovariansiZ2_Z3, 
    kovariansiZ3_Z1, kovariansiZ3_Z2, variansiZ3_2204), 
  nrow=3, ncol=3)
matriks_sigma

# konstruksi statistik uji menggunakan Z1 dan Z2
statistik_uji_Z1Z2 = matrix(c(Z1_2204, Z2_2204), nrow=1, ncol=2) %*% inv(matriks_sigma[1:2, 1:2]) %*% t(matrix(c(Z1_2204, Z2_2204), nrow=1, ncol=2))
statistik_uji_Z1Z2

# konstruksi statistik uji menggunakan Z2 dan Z3
statistik_uji_Z2Z3 = matrix(c(Z2_2204, Z3_2204), nrow=1, ncol=2) %*% inv(matriks_sigma[2:3, 2:3]) %*% t(matrix(c(Z2_2204, Z3_2204), nrow=1, ncol=2))
statistik_uji_Z2Z3

# perhatikan bahwa statistik_uji_Z1Z2 dan statistik_uji_Z2Z3 sama
# hasil yang sama juga akan diperoleh ketika menggunakan dua pasang Z yang lain

# survdiff berguna untuk 'Test Survival Curve Differences'
# Uji hipotesis dengan menggunakan fungsi survdiff
# akan dilihat apakah terdapat perbedaan fungsi survival disease-free dari ketiga populasi
survdiff(Surv(t2, d3) ~ group, data=bmt)

# berdasarkan uji tersebut, diperoleh kesimpulan bahwa H0 ditolak
