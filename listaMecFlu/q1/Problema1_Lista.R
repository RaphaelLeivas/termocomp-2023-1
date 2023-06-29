rm(list = ls())

library("pracma")
library(flextable)

U0 <- 1
mu <- 0.29
rho <- 891
# y <- seq(from = 0, to = 10, by = 1) * 10^-2
# t <-seq(from = 0.25, to = 1, by = 0.25)
u_initial <- 0 # repouso

# solutionsList <- list()
# solutionsList[[1]] <- rep(u_initial, length(y))
# 
# i = 1 
# for (ti in t) {
#   i = i + 1
#   currentU <-  U0 * (1 - erf(y/(2 * sqrt(mu * ti / rho))))
#   solutionsList[[i]] <- currentU
# }

## SOLUCAO NUMERICA 

# os parametros abaixo estão excelentes para a questao 1.
# alpha ficou baixo e bateu demais com a solução analítica

y_size <- 10 * 10^-2 # tamanho vertical do dominio, em metros
T <- 1 # periodo total de analise, em segundos
N <- 10 # numero de malhas (intervalos) da direção y, unidimensional
M <- 20 # numeros de pontos no tempo
dy <- y_size / N # metros por divisao
dt <- T / M # segundos por divisao
alpha = (mu / rho) * dt / (dy^2)

N <- N + 1 # ajuste para incluir a posicao zero

y <- c()

# configura a malha
for (yi in 1:N) {
  y[yi] <- (yi - 1) * dy
}

solutionsList <- list()

boundaryVelBottom <- 1
boundaryVelTop <- 0
initialVel <- 0 # inicialmente em repouso

initialMesh <- c()

for (yi in 1:N) {
  if (yi == 1) {
    # se esta na base
    initialMesh <- append(initialMesh, boundaryVelBottom)
  } else if (yi == N) {
    # se esta no topo
    initialMesh <- append(initialMesh, boundaryVelTop)
  } else {
    # se nao, é malha interna 
    initialMesh <- append(initialMesh, initialVel)
  }
}

solutionsList[[1]] <- initialMesh

for (t in 1:M) {
  prevU <- solutionsList[[t]]
  
  newU <- c()
  for (yi in 1:N) {
    # se esta na base ou no topo, temos condicao de contorno
    if (yi == 1) {
      # se esta na base
      newU <- append(newU, boundaryVelBottom)
    } else if (yi == N) {
      # se esta no topo
      newU <- append(newU, boundaryVelTop)
    } else {
      # se nao, é malha interna e usa a expressao deduzida
      newU <- append(newU, prevU[yi] + alpha * (prevU[yi + 1] - 2 * prevU[yi] + prevU[yi - 1] ))
    }
  }
  
  solutionsList[[t + 1]] <- newU
}

# formatacao para a flextable
Y_coord <- y * 10^2
T_0s <- solutionsList[[1]]
T_25s <- solutionsList[[6]]
T_50s <- solutionsList[[11]]
T_75s <- solutionsList[[16]]
T_100s <- solutionsList[[21]]

T_0s <- format(round(T_0s, 2), nsmall = 2)
T_25s <- format(round(T_25s, 2), nsmall = 2)
T_50s <- format(round(T_50s, 2), nsmall = 2)
T_75s <- format(round(T_75s, 2), nsmall = 2)
T_100s <- format(round(T_100s, 2), nsmall = 2)

df <- data.frame(Y_coord, T_0s, T_25s, T_50s, T_75s, T_100s)
ft <- flextable(df)
ft <- add_header_row(
  x = ft, values = c("Posição y (cm)", "Velocidade (m/s)"),
  colwidths = c(1, 5))
ft <- align(ft, align = "center", part = "all")
ft <- set_header_labels(ft, Y_coord = "",
                        T_0s = "0 s", T_25s = "0.25 s", T_50s = "0.5 s", T_75s = "0.75 s",  T_100s = "1 s")



