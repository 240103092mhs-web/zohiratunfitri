central <- function(f, x, h) {
  (f(x + h) - f(x-h)) / (2*h)
}

f <- function(x) {
  x^2
}

central(f, 2, 0.1)