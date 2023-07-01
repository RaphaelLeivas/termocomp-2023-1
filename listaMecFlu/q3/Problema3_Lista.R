rm(list = ls())

dpdx <- -178800
mu <- 1.49
h <- 5 * 10^-3
U <- 1
N <- 10 # numero de pontos analisados 
dy <- 2 * h/N # passo no espaço

N <- N + 1 # ajuste posicao incial

y_seq <- seq(from = -h, to = h, by = dy)
y_seq_an <- seq(from = -h, to = h, by = 10^-6)
u_analytical <- (U/2) * (1 + y_seq_an/h) + ((-dpdx * h^2) / (2 * mu * U)) * (1 - y_seq_an^2 / h^2) * U

plot(
  NULL,
  main = "Velocidade (m/s) x Posição vertical (mm)",
  xlab = "Posição vertical (mm)",
  ylab = "Velocidade (m/s)",
  xlim = c(-h, h),
  ylim = c(0, max(u_analytical))
)

lines(y_seq_an, u_analytical, lwd = 2, col = "red")

# solucao numerica

A <- matrix(0, ncol = N, nrow = N)
b <- rep(0, N)

line <- 0

for (yi in y_seq) {
  line <- line + 1
  
  if (yi == -h) {
    # condição de contorno placa de baixo
    # u0 = 0 ==> 1 * u0 = 0
    
    A[line, line] <- 1
    b[line] <- 0
  } else if (yi == h) {
    # condição de contorno placa de cima
    # uN = U ==> 1 * uN = U
    
    A[line, line] <- 1
    b[line] <- U
  } else {
    # no interno, usa  a equacao de diferenças finitas deduzida
    A[line, line + 1] <- 1
    A[line, line] <- -2
    A[line, line - 1] <- 1
    
    b[line] <- (dpdx / mu) * dy^2
  }
}

u_numerical <- solve(A) %*% b
lines(y_seq, u_numerical, lwd = 2, col = "blue", lty = 1)

