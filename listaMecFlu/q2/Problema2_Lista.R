rm(list = ls())

library(flextable)

dpdz <- -12.928
mu <- 1.01 * 10^-3
R <- 25 * 10^-3
N <- 50 # numero de pontos analisados 
dr <- R/N # passo no espaço

N <- N + 1

r_seq <- seq(from = 0.0, to = R, by = dr)
u_analytical <- dpdz / (4 * mu) * (r_seq^2 - R^2)

plot(
  NULL,
  main = "Velocidade (m/s) x Posição radial (mm)",
  xlab = "Posição radial (mm)",
  ylab = "Velocidade (m/s)",
  xlim = c(0, max(r_seq)),
  ylim = c(0, 1.5 * max(u_analytical))
)

lines(r_seq, u_analytical, lwd = 2, col = "red")

# agora é a analise numerica

# matriz que representa os coeficientes do sistema linear
A <- matrix(0, ncol = N, nrow = N)
line <- 0

# matriz coluna (vetor vertical) que representa os termos independentes
b <- rep(0, N)

for (ri in r_seq) {
  line <- line + 1
  
  if (ri == 0) {
    # condição de simetria
    A[line, line + 1] <- 1
    A[line, line] <- -1
    
    b[line] <- (dpdz / mu) * (1/4) * dr^2
  } else if (ri == R) {
    # na borda do cano, velocidade nula (cond de contorno)
    # assim temos 1 * u_R = 0 ==> u_R = 0
    
    A[line, line] <- 1
    b[line] <- 0
  } else {
    # no interno
    A[line, line + 1] <- 1 + ri / dr
    A[line, line] <- -1 - 2 * ri / dr
    A[line, line - 1] <- ri / dr
    
    b[line] <- (dpdz / mu) * ri * dr
  }
}

u_numerical <- solve(A) %*% b
lines(r_seq, u_numerical, lwd = 2, col = "blue")





