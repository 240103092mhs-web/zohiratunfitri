x <- 0.5
nilai_eksak <- cos(x)
maks_suku <- 10
suku_list <- 1:maks_suku
galat_list <- numeric(maks_suku)
hampiran <- 0

for (n in 0:(maks_suku - 1)) {
  term <- ((-1)^n * x^(2*n)) / factorial(2*n)
  hampiran <- hampiran + term
  galat_list[n + 1] <- abs(nilai_eksak - hampiran)
}
data_grafik <- data.frame(
  Jumlah_Suku = suku_list,
  Galat_Absolut = galat_list
)

print(data_grafik)

plot(data_grafik$Jumlah_Suku, data_grafik$Galat_Absolut,
     type="o", log="y",
     main="galat cos(0.5) vs jumlah suku",
     xlab="jumlah suku", ylab="galat")