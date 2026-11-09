f <- function(x) x^3 - x - 2
f_prime <- function(x) 3*x^2 - 1
tol <- 1e-6
max_iter <- 100

curve(f, from=0, to=3, col="blue", lwd=2, main="f(x)=x^3 - x - 2")
abline(h=0, v=0, lty=2); grid()

a <- 1; b <- 2
cat("\n--- BISECTION ---\n")
for(i in 1:max_iter){
  c <- (a+b)/2
  cat(sprintf("Iter %d: a=%.6f b=%.6f c=%.6f f(c)=%.6f\n", i, a, b, c, f(c)))
  if(f(c)==0 || (b-a)/2 < tol) break
  if(f(a)*f(c) < 0) b <- c else a <- c
}
cat(sprintf("Akar Bisection: %.8f, Iterasi: %d\n", c, i))

x <- 1.5
cat("\n--- NEWTON-RAPHSON ---\n")
for(i in 1:max_iter){
  x_new <- x - f(x)/f_prime(x)
  cat(sprintf("Iter %d: x=%.8f f(x)=%.8f\n", i, x_new, f(x_new)))
  if(abs(x_new - x) < tol) break
  x <- x_new
}
cat(sprintf("Akar Newton: %.8f, Iterasi: %d\n", x_new, i))

x0 <- 1; x1 <- 2
cat("\n--- SECANT ---\n")
for(i in 1:max_iter){
  x2 <- x1 - f(x1)*(x1-x0)/(f(x1)-f(x0))
  cat(sprintf("Iter %d: x=%.8f f(x)=%.8f\n", i, x2, f(x2)))
  if(abs(x2 - x1) < tol) break
  x0 <- x1; x1 <- x2
}
cat(sprintf("Akar Secant: %.8f, Iterasi: %d\n", x2, i))