 secant <- function(f, x0, x1, tol = 1e-5, max_iter = 100) {
  cat(sprintf("%-6s %-15s %-15s %-15s\n", "Iter", "x_baru", "f(x_baru)", "|selisih|"))
  cat("--------------------------------------------------------\n")
  
  for (i in 1:max_iter) {
    fx0 <- f(x0)
    fx1 <- f(x1)
    
    # Perbaikan 1: pembagi dicek hampir nol, bukan == 0
    # Perbaikan 2: cek kalau f(x) nya tidak valid (Inf / NA)
    pembagi <- fx1 - fx0
    if (!is.finite(fx0) || !is.finite(fx1) || abs(pembagi) < 1e-12) {
      cat("Pembagi nol / tidak valid, proses dihentikan! Coba x0, x1 lain.\n")
      return(NULL)
    }
    
    x_baru <- x1 - fx1 * (x1 - x0) / pembagi
    selisih <- abs(x_baru - x1)
    
    cat(sprintf("%-6d %-15.6f %-15.6f %-15.6f\n", i, x_baru, f(x_baru), selisih))
    
    if (selisih < tol) {
      return(list(akar = x_baru, iterasi = i, konvergen = TRUE))
    }
    
    # Geser titik: x0 jadi x1 lama, x1 jadi x_baru
    x0 <- x1
    x1 <- x_baru
  }
  
  cat("PERINGATAN: Tidak konvergen sampai 100 iterasi!\n")
  return(list(akar = x1, iterasi = max_iter, konvergen = FALSE))
}

# === CONTOH: x^2 - 4 = 0, akar = 2 ===
f <- function(x) {
  return(x^2 - 4)
}

hasil <- secant(f, 1, 3)

# Perbaikan 3: cek NULL dulu sebelum print, biar gak error
if (!is.null(hasil) && hasil$konvergen) {
  cat(sprintf("\nAkar ditemukan: %.6f dalam %d iterasi\n", hasil$akar, hasil$iterasi))
}