rm(list = ls())

library("pracma")
library(flextable)

U0 <- 1
mu <- 0.29
rho <- 891
H <- 0.1 # tamanho vertical do dominio (m)
T <- 1 # dominio de tempo de analise
u_initial <- 0 # condicao inicial

N <- 50 # numero de malhas (intervalos) da direção y, unidimensional
M <- 10 # numeros de pontos no tempo
dy <- H / N # metros por divisao
dt <- T / M # segundos por divisao

y <- seq(from = 0, to = H, by = dy)
t <- seq(from = 0 + dt, to = T, by = dt)

solutionsList <- list()
solutionsList[[1]] <- rep(u_initial, length(y))

i = 1
for (ti in t) {
  i = i + 1
  currentU <-  U0 * (1 - erf(y/(2 * sqrt(mu * ti / rho))))
  solutionsList[[i]] <- currentU
}

desiredIndexes <- c(1, 3, 6, 9, 11)
colors <- c("red", "blue", "purple", "black", "orange")
linesToShow <- list()

i <- 1
for (index in desiredIndexes) {
  linesToShow[[i]] <- solutionsList[[index]]
  i = i + 1
}

plot(
  NULL,
  main = "Velocidade (m/s) x Posição vertical (m)",
  xlab = "Posição vertical (m)",
  ylab = "Velocidade (m/s)",
  xlim = c(0, H),
  ylim = c(0, U0)
)

for (j in 1:length(linesToShow)) {
  lines(y, linesToShow[[j]], lwd = 2, col = colors[j])
}

legend(x = "topright", box.lwd = 2 , title="Instantes de tempo", 
       legend=c("t = 0 s", "t = 0.2 s", "t = 0.5 s", "t = 0.8 s", "t = 1.0 s"), 
       fill = colors)
