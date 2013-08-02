cai_gamma <- function(x) {
  m <- mean(x)
  v <- var(x)
  print(paste(m))
  print(paste(v))
  print('GAMMA')
  k <- ks.test(x, 'pgamma', scale=v/m, shape=m^2/v)
  print(k)
  
  print('EXP')
  k <- ks.test(x, 'pexp', 1/m)
  print(k)
}