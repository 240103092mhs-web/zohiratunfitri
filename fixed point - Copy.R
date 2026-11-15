fixed_point <- function(g, x0, tol = 1e-5, max_iter = 100) {
  x_lama <- x0
  
  # Cek awal: x0 harus angka yang valid
  if (!is.finite(x_lama)) {
    cat("Error: x0 tidak valid!\n")
    return(NULL)
  }
  
  cat(sprintf("%-6s %-15s %-15s\n", "Iter", "x_baru", "|selisih|"))
  cat("------------------------------------------\n")
  
  for (i in 1:max_iter) {
    x_baru <- g(x_lama)
    
    # Pengaman: kalau g(x) error / Inf / NA langsung berhenti
    if (!is.finite(x_baru)) {
      cat(sprintf("Berhenti di iterasi %d: g(x) = %s (tidak valid)\n", i, x_baru))
      return(NULL)
    }
    
    selisih <- abs(x_baru - x_lama)
    cat(sprintf("%-6d %-15.6f %-15.6f\n", i, x_baru, selisih))
    
    # Kriteria berhenti yang benar
    if (selisih < tol) {
      cat("------------------------------------------\n")
      return(list(akar = x_baru, iterasi = i, konvergen = TRUE))
    }
    
    x_lama <- x_baru
  }
  
  cat("------------------------------------------\n")
  cat("PERINGATAN: Maksimum iterasi tercapai, TIDAK konvergen!\n")
  cat("Cek lagi g(x) kamu, syaratnya |g'(x)| < 1 di dekat akar.\n")
  return(list(akar = x_lama, iterasi = max_iter, konvergen = FALSE))
}

# === CONTOH YANG BENAR DAN PASTI KONVERGEN ===
# Soal: x^2 - 4 = 0, akar = 2
# Kita ubah jadi: x = (x + 4/x) / 2  -> ini pasti konvergen
g <- function(x) {
  return((x + 4 / x) / 2)
}

hasil <- fixed_point(g, 1.5)

if (!is.null(hasil) && hasil$konvergen) {
  cat(sprintf("\nAkar ditemukan: %.6f dalam %d iterasi\n", hasil$akar, hasil$iterasi))
}