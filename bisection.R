bisection <- function(f, a, b, tol = 1e-5, max_iter = 100) {
  fa <- f(a)
  fb <- f(b)
  
  # 1. Kalau ujungnya udah akar
  if (fa == 0) {
    return(list(akar = a, iterasi = 0))
  }
  if (fb == 0) {
    return(list(akar = b, iterasi = 0))
  }
  
  # 2. Syarat biseksi: harus beda tanda
  if (fa * fb > 0) {
    cat("Syarat tidak terpenuhi: f(a) dan f(b) harus beda tanda!\n")
    return(NULL)
  }
  
  cat(sprintf("%-6s %-12s %-12s %-12s %-12s\n", "Iter", "a", "b", "x", "f(x)"))
  cat("-------------------------------------------------------\n")
  
  for (i in 1:max_iter) {
    x <- (a + b) / 2
    fx <- f(x)
    
    cat(sprintf("%-6d %-12.6f %-12.6f %-12.6f %-12.6f\n", i, a, b, x, fx))
    
    # 3. Berhenti kalau sudah cukup dekat
    if (abs(fx) < tol || (b - a) / 2 < tol) {
      return(list(akar = x, iterasi = i))
    }
    
    # 4. Persempit selang
    if (fa * fx < 0) {
      b <- x
      fb <- fx
    } else {
      a <- x
      fa <- fx
    }
  }
  
  cat("Mencapai batas iterasi maksimum!\n")
  return(list(akar = (a + b) / 2, iterasi = max_iter))
}

# === CONTOH PENGGUNAAN ===
f <- function(x) {
  return(x^2 - 4)
}

hasil <- bisection(f, 1, 3)
cat(sprintf("\nAkar ditemukan: %.6f dalam %d iterasi\n", hasil$akar, hasil$iterasi))