bacward_difference <- function(f, x, h) {
  (f(x) - f(x-h)) / h
}

f <- function(x) {
  x^2
}

bacward_difference(f, 2, 0.1)