rm(list = ls())

library(flextable)

# atencao para criterios de estabilidade: alpha deve ser pequeno!
# se nao a solução apresenta numeros enormes!

L <- 1 # tamanho da barra, em metros
T <- 0.1 # periodo total de analisa, em segundos
k <- 1 # condutividade da barra
N <- 10 # numero de malhas (intervalos) da direção x, unidimensional
M <- 50 # numeros de pontos no tempo
dx <- L / N # metros por divisao
dt <- T / M # segundos por divisao
alpha <- k * dt / (dx^2) # parametro da solucao numerica

N <- N + 1 # ajuste para incluir a posicao zero

x <- c()

# configura a malha
for (xi in 1:N) {
    x[xi] <- (xi - 1) * dx
}

solutionsList <- list()

# configura a distribuicao inicial de temperatura: todos com 30 graus, exceto
# os contornos, que sao fixados em boundaryTemp
boundaryTemp <- 0
initialTemp <- 100

initialMesh <- c()
for (xi in 1:N) {
  # se esta na base ou no topo, temos condicao de contorno com T igual a boundaryTemp
  if (xi == 1 || xi == N) {
    initialMesh <- append(initialMesh, boundaryTemp)
  } else {
    # se nao, é malha interna e tem T = initialTemp
    # aqui é a configuração da condição inicial
    initialMesh <- append(initialMesh, initialTemp)
  }
}

solutionsList[[1]] <- initialMesh

for (t in 1:M) {
  previousTemp <- solutionsList[[t]]
  
  newTemp <- c()
  for (xi in 1:N) {
    # se esta na base ou no topo, temos condicao de contorno com T igual a zero
    if (xi == 1 || xi == N) {
    # aqui é a configuração e manutenção das condições de contorno
      newTemp <- append(newTemp, boundaryTemp)
    } else {
      # se nao, é malha interna e usa a expressao deduzida
      newTemp <- append(newTemp, previousTemp[xi] + alpha * (previousTemp[xi + 1] - 2 * previousTemp[xi] + previousTemp[xi - 1] ))
    }
  }
  
  solutionsList[[t + 1]] <- newTemp
}

# solução obtida está condizente: a referencia é Figura 1 pagina 11 desse doc
# https://ocw.mit.edu/courses/18-303-linear-partial-differential-equations-fall-2006/d11b374a85c3fde55ec971fe587f8a50_heateqni.pdf

# formatacao para a flextable
X_coord <- x * 10^0
T_0s <- solutionsList[[1]]
T_25s <- solutionsList[[10]]
T_50s <- solutionsList[[20]]
T_75s <- solutionsList[[30]]
T_100s <- solutionsList[[40]]

T_0s <- format(round(T_0s, 2), nsmall = 2)
T_25s <- format(round(T_25s, 2), nsmall = 2)
T_50s <- format(round(T_50s, 2), nsmall = 2)
T_75s <- format(round(T_75s, 2), nsmall = 2)
T_100s <- format(round(T_100s, 2), nsmall = 2)

df <- data.frame(X_coord, T_0s, T_25s, T_50s, T_75s, T_100s)
ft <- flextable(df)
ft <- add_header_row(
  x = ft, values = c("Posição x (m)", "Temperatura (K)"),
  colwidths = c(1, 5))
ft <- align(ft, align = "center", part = "all")
ft <- set_header_labels(ft, X_coord = "", 
                        T_0s = "0 s", T_25s = "0.25 s", T_50s = "0.5 s", T_75s = "0.75 s",  T_100s = "1 s")




