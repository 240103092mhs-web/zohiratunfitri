x <- 0.5
tol <- 1e-6
max_iter <- 100
true_val <- cos(x) # nilai eksak dari R untuk perbandingan

approx <- 0
error <- Inf

cat("Menghitung cos(0.5) dengan deret Maclaurin\n")
cat(sprintf("Target toleransi: %e\n\n", tol))
cat(sprintf("%-7s | %-15s | %-15s | %-15s\n", "Iterasi", "Suku", "Nilai Hampiran", "Galat"))
cat(paste0(rep("-", 60), collapse=""), "\n")

for (n in 0:(max_iter - 1)) {
  approx_old <- approx
  
  # RUMUS YANG BENAR UNTUK COSINUS
  term <- ((-1)^n * x^(2*n)) / factorial(2*n)
  approx <- approx + term
  
  # Hitung galat (selisih dengan iterasi sebelumnya)
  if (n > 0) {
    error <- abs(approx - approx_old)
  }
  
  # Cetak perkembangan setiap iterasi
  cat(sprintf("n = %-3d | %-15.10f | %-15.10f | %-15.10e\n", n, term, approx, error))
  
  # Berhenti jika galat sudah lebih kecil atau sama dengan toleransi
  if (error < tol) {
    cat("\n[Sukses] Toleransi galat terpenuhi!\n")
    break
  }
  if (error >= tol) {
    cat("\n[Peringatan] Iterasi maksimum tercapai sebelum toleransi terpenuhi.\n")
  }
  
  cat("\n========== HASIL AKHIR ==========\n")
  cat(sprintf("Nilai hampiran : %.10f\n", approx))
  cat(sprintf("Nilai eksak R : %.10f\n", true_val))
  cat(sprintf("Galat akhir : %.10e\n", abs(true_val - approx)))
  cat(sprintf("Total iterasi : %d (n = 0 sampai %d)\n", n+1, n))
  cat(sprintf("Jumlah suku minimal yang dibutuhkan : %d suku\n", n+1))
}