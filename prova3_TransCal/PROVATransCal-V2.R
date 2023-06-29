rm(list = ls())
# dev.off()

# Parametros do metodo numerico
NX <- 8 # quantidade de pontos da malha no sentido horizontal
NY <- 4 # quantidade de pontos da malha no sentido vertical

# Parametros do problema
k <- 0.85 # Condutividade térmica do duto (W/mK)
alpha <- 5.51e-7 # Difusividade térmica do duto (m²/s)
T_initial <- 25 # Temperatura inicial uniforme (°C)
Te <- 25 # Temperatura externa (°C)
Ti <- 350 # Temperatura do fluido interno (°C)
hi <- 100 # Coeficiente de transferência de calor interno (W/m²K)
he <- 5 # Coeficiente de transferência de calor externo (W/m²K)

lxEx <- 300 # mm : comprimento horizontal exterior (metade)
lxIn <- 150 # mm : comprimento horizontal interior (metade)
ly <- 150 # mm : comprimento vertical (espessura do duto)

# variacoers de distancia dx e dy (mm)
dx <- lxEx / (NX - 1)
dy <- ly / (NY - 1)

# variacao no tempo (s) (usamos 3600s = 1h)
dt <- 3600

# funcao que limita a diagonal onde ha simetria no problema
limiteDiagonal <- function(y) {
  return(y + lxIn)
}


# matriz de com as coordenadas dos pontos da malha (o canto inferior esquerdo esta na origem)
pontos <- list()

# coordenadas dos pontos a serem armazenadas
x <- 0
y <- 0

# preenchendo a matriz de pontos
# i sao as "linhas" de pontos e j as "colunas" (graficamente falando)
for (i in 1:NY) {
  pontos[[i]] <- matrix(c(x, y), nrow = 1, ncol = 2)
  for (j in 2:NX) {
    if (x <= limiteDiagonal(y)) {
      x <- x + dx
      pontos[[i]] <- rbind(pontos[[i]], matrix(c(x, y), nrow = 1, ncol = 2))
    } else {
      break()
    }
  }
  # par(new=T)
  # plot(pontos[[i]], pch = "+", xlim = c(0,lxEx+10), ylim = c(0, ly+10))
  x <- 0
  y <- y + dy
}

# funcao que recebe as coordenadas i e j e retorna o numero da malha
numeroMalha <- function(i, j) {
  n <- j
  for (I in (i - 1):1) {
    if (I == 0) break
    n <- n + (dim(pontos[[I]])[1])
  }

  return(n)
}

# numero total de pontos na malha
tam <- numeroMalha(NY, NX)

# matriz que representa os coeficientes do sistema linear
A <- matrix(0, ncol = tam, nrow = tam)

# matriz coluna (vetor vertical) que representa os termos independentes
b <- rep(0, tam)

# calculo do numero de fourier
fo <- alpha * dt / ((dx * 10^-3)^2)

# calculo do numero de biot para conveccao com o fluido externo
bi_e <- he * (dx * 10^-3) / k

# calculo do numero de biot para conveccao com o fluido interno
bi_i <- hi * (dx * 10^-3) / k

# lista de soluções a cada instante dt
solutionsList <- list()
solutionsList[[1]] <- rep(T_initial, tam) # inicializa o primeiro tempo com a temp inicial

# valor de maximo de p para parar. sigfinica que vai rodar para max_p horas
max_p <- 100

for (p in 1:max_p) {
  # puxa as temperaturas anteriores
  previousTemp <- solutionsList[[p]]

  # loop que identifica o tipo de malha para aplicar a equacao correta
  for (i in 1:NY) {
    tamLinha <- dim(pontos[[i]])[1]
    for (j in 1:tamLinha) {
      # Cálculo do índice correspondente ao ponto atual
      index <- numeroMalha(i, j)

      # ponto da malha em conveccao com o fluido externo
      if ((i == length(pontos)) && (j != 1) && (j != dim(pontos[[i]])[1])) {
        A[index, index] <- 1 + 2 * fo * (2 + bi_e) # no atual
        A[index, numeroMalha(i, (j - 1))] <- -fo # no oeste
        A[index, numeroMalha(i, (j + 1))] <- -fo # no leste
        A[index, numeroMalha((i - 1), j)] <- -2 * fo # no sul

        b[index] <- previousTemp[index] + 2 * bi_e * fo * Te

        # par(new=T)
        # plot(pontos[[i]][j,1], pontos[[i]][j,2], pch = "•", xlim = c(0,lxEx+10), ylim = c(0, ly+10), col = "red",
        #     xlab = " ", ylab = " ")
      }

      # ponto da malha em conveccao com o fluido interno
      else if ((i == 1) && (j != 1) && (j != dim(pontos[[i]])[1])) {
        A[index, index] <- 1 + 2 * fo * (2 + bi_i) # no atual
        A[index, numeroMalha(i, (j - 1))] <- -fo # no oeste
        A[index, numeroMalha(i, (j + 1))] <- -fo # no leste
        A[index, numeroMalha((i + 1), j)] <- -2 * fo # no norte

        b[index] <- previousTemp[index] + 2 * bi_i * fo * Ti

        # par(new=T)
        # plot(pontos[[i]][j,1], pontos[[i]][j,2], pch = "•", xlim = c(0,lxEx+10), ylim = c(0, ly+10), col = "blue",
        #     xlab = " ", ylab = " ")
      }

      # ponto da malha em simetria (à esquerda)
      else if ((j == 1) && (i != 1) && (i != NY)) {
        A[index, index] <- 1 + 4 * fo # no atual
        A[index, numeroMalha(i, (j + 1))] <- -2 * fo # no leste
        A[index, numeroMalha((i + 1), j)] <- -fo # no norte
        A[index, numeroMalha((i - 1), j)] <- -fo # no sul

        b[index] <- previousTemp[index]

        # par(new=T)
        # plot(pontos[[i]][j,1], pontos[[i]][j,2], pch = "•", xlim = c(0,lxEx+10), ylim = c(0, ly+10), col = "green",
        #     xlab = " ", ylab = " ")
      }

      # ponto da malha em simetria (à direita)
      else if ((j == dim(pontos[[i]])[1]) && (i != 1) && (i != NY)) {
        A[index, index] <- 1 + 4 * fo # no atual
        A[index, numeroMalha(i, (j - 1))] <- -2 * fo # no oeste
        A[index, numeroMalha((i + 1), j)] <- -2 * fo # no norte

        b[index] <- previousTemp[index]

        # par(new=T)
        # plot(pontos[[i]][j,1], pontos[[i]][j,2], pch = "•", xlim = c(0,lxEx+10), ylim = c(0, ly+10), col = "green",
        #     xlab = " ", ylab = " ")
      }

      # ponto da malha em simetria na quina superior esquerda
      else if ((j == 1) && (i == NY)) {
        # mesma coisa de convecção externa, com ajuste no oeste
        A[index, index] <- 1 + 2 * fo * (2 + bi_e) # no atual
        # A[index, numeroMalha(i, (j-1))] <- -fo #no oeste
        A[index, numeroMalha(i, (j + 1))] <- -2 * fo # no leste
        A[index, numeroMalha((i - 1), j)] <- -2 * fo # no sul

        b[index] <- previousTemp[index] + 2 * bi_e * fo * Te
      }

      # ponto da malha em simetria na quina inferior esquerda
      else if ((j == 1) && (i == 1)) {
        # mesma coisa de convecção interna, com ajuste no oeste
        A[index, index] <- 1 + 2 * fo * (2 + bi_i) # no atual
        # A[index, numeroMalha(i, (j-1))] <- -fo #no oeste
        A[index, numeroMalha(i, (j + 1))] <- -2 * fo # no leste
        A[index, numeroMalha((i + 1), j)] <- -2 * fo # no norte

        b[index] <- previousTemp[index] + 2 * bi_i * fo * Ti
      }

      # ponto da malha em simetria na quina superior direita
      else if ((j == dim(pontos[[i]])[1]) && (i == NY)) {
        A[index, index] <- 1 + 4 * fo * (1 + bi_e) # no atual
        A[index, numeroMalha(i, (j - 1))] <- -4 * fo # no oeste

        b[index] <- previousTemp[index] + 4 * bi_e * fo * Te
      }

      # ponto da malha em simetria na quina inferior direita
      else if ((j == dim(pontos[[i]])[1]) && (i == 1)) {
        A[index, index] <- 1 + 4 * fo * (1 + (1/3) * bi_i) # no atual
        A[index, numeroMalha(i, (j - 1))] <- -(4/3) * fo # no oeste
        A[index, numeroMalha((i + 1), j)] <- -(8/3) * fo # no norte

        b[index] <- previousTemp[index] + (4/3) * bi_i * fo * Ti
      }

      # ponto da malha interno
      else {
        A[index, index] <- 1 + 4 * fo # no atual
        A[index, numeroMalha(i, (j + 1))] <- -fo # no oeste
        A[index, numeroMalha(i, (j - 1))] <- -fo # no leste
        A[index, numeroMalha((i + 1), j)] <- -fo # no norte
        A[index, numeroMalha((i - 1), j)] <- -fo # no sul

        b[index] <- previousTemp[index]

        # par(new=T)
        # plot(pontos[[i]][j,1], pontos[[i]][j,2], pch = "•", xlim = c(0,lxEx+10), ylim = c(0, ly+10),
        #     xlab = " ", ylab = " ")
      }
    }
  }

  # salva as novas temperaturas calculadas, para usar no proximo loop
  solutionsList[[p + 1]] <- solve(A) %*% b
}

# extrai a evolução de temperatura de um nó interno
# exemplo: nó numero 15, interno
node15temps <- c()
node23temps <- c()
node2temps <- c()

for (p in 1:max_p) {
  currentSolution <- solutionsList[[p]]
  node15temps <- append(node15temps, currentSolution[15])
  node23temps <- append(node23temps, currentSolution[23])
  node2temps <- append(node2temps, currentSolution[2])
}

plot(
  NULL,
  main = "Temperatura do Nó (°C) x Tempo (h)",
  xlab = "Tempo (h)",
  ylab = "Temperatura (°C)",
  xlim = c(0, max_p/4),
  ylim = c(0, Ti)
)

lines(node15temps, lwd = 2, col = "black")
lines(node23temps, lwd = 2, col = "red")
lines(node2temps, lwd = 2, col = "blue")

# formata o resultado final em tabelas
library(flextable)

Nodes <- 1:tam
T_1h <- solutionsList[[1]]
T_5h <- solutionsList[[5]]
T_10h <- solutionsList[[10]]
T_20h <- solutionsList[[20]]
T_50h <- solutionsList[[50]]
T_100h <- solutionsList[[100]]

T_1h <- format(round(T_1h, 2), nsmall = 2)
T_5h <- format(round(T_5h, 2), nsmall = 2)
T_10h <- format(round(T_10h, 2), nsmall = 2)
T_20h <- format(round(T_20h, 2), nsmall = 2)
T_50h <- format(round(T_50h, 2), nsmall = 2)
T_100h <- format(round(T_100h, 2), nsmall = 2)

df <- data.frame(Nodes, T_1h, T_5h, T_10h, T_20h, T_50h, T_100h)
ft <- flextable(df)
ft <- add_header_row(
  x = ft, values = c("Malha", "Temperatura (°C)"),
  colwidths = c(1, 6))
ft <- align(ft, align = "center", part = "all")
ft <- set_header_labels(ft, Nodes = "", 
                        T_1h = "0h", T_5h = "5h", T_10h = "10h", T_20h = "20h",
                        T_50h = "50h", T_100h = "100h")

expected <- c(339.26, 184.26, 198.02, 110.65, 227.9)
observed <- c(324.8, 170.2, 182.4, 97.5, 230.9)

error <- 100 * (observed - expected) / expected

