newton_raphson <- function(f, df, x0, tol = 1e-5, max_iter = 100) {
  x <- x0
  
  cat(sprintf("%-6s %-15s %-15s %-15s\n", "Iter", "x_baru", "f(x)", "|selisih|"))
  cat("--------------------------------------------------------\n")
  
  for (i in 1:max_iter) {
    fx <- f(x)
    dfx <- df(x)
    
    # Pengaman: kalau turunan hampir nol, jangan dibagi!
    if (!is.finite(fx) || !is.finite(dfx) || abs(dfx) < 1e-12) {
      cat("Turunan nol / tidak valid, proses dihentikan! Coba x0 lain.\n")
      return(NULL)
    }
    
    x_baru <- x - fx / dfx
    selisih <- abs(x_baru - x)
    
    cat(sprintf("%-6d %-15.6f %-15.6f %-15.6f\n", i, x_baru, fx, selisih))
    
    if (selisih < tol) {
      return(list(akar = x_baru, iterasi = i, konvergen = TRUE))
    }
    x <- x_baru
  }
  
  cat("PERINGATAN: Tidak konvergen sampai 100 iterasi!\n")
  return(list(akar = x, iterasi = max_iter, konvergen = FALSE))
}

# CONTOH: x^2 - 4 = 0
f <- function(x) { return(x^2 - 4) }
df <- function(x) { return(2 * x) }

hasil <- newton_raphson(f, df, 1.5)
if (!is.null(hasil) && hasil$konvergen) {
  cat(sprintf("\nAkar ditemukan: %.6f dalam %d iterasi\n", hasil$akar, hasil$iterasi))
}