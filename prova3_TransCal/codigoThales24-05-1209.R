rm(list = ls())

# Parâmetros do problema
L_interno <- 150    # Lado interno do quadrado (mm)
L_externo <- 300    # Lado externo do quadrado (mm)
k <- 0.85           # Condutividade térmica do duto (W/mK)
alpha <- 5.51e-7    # Difusividade térmica do duto (m²/s)
Ti <- 25            # Temperatura inicial do duto (°C)
Te <- 25            # Temperatura externa (°C)
hi <- 100           # Coeficiente de transferência de calor interno (W/m²K)
he <- 5             # Coeficiente de transferência de calor externo (W/m²K)

# Parâmetros do método das diferenças finitas
Nx <- 50            # Número de nós internos na direção x
Ny <- 20            # Número de nós internos na direção y

# Cálculo do tamanho do intervalo de grade
h <- (L_externo - L_interno) / (Nx + 1)

# Cálculo do número total de nós internos
N <- Nx * Ny

# Inicialização da matriz A e do vetor b
A <- matrix(0, nrow = N, ncol = N)
b <- rep(0, N)

# Iteração sobre cada ponto da malha interna
for (i in 1:Nx) {
  for (j in 1:Ny) {
    # Cálculo do índice correspondente ao ponto atual
    index <- (i - 1) * Ny + j
    
    # Verificação dos pontos vizinhos
    if (i > 1) {
      # Coeficiente para o ponto à esquerda
      A[index, index - Ny] <- -alpha / (h^2)
    }
    
    if (i < Nx) {
      # Coeficiente para o ponto à direita
      A[index, index + Ny] <- -alpha / (h^2)
    }
    
    if (j > 1) {
      # Coeficiente para o ponto acima
      A[index, index - 1] <- -alpha / (h^2)
    }
    
    if (j < Ny) {
      # Coeficiente para o ponto abaixo
      A[index, index + 1] <- -alpha / (h^2)
    }
    
    # Coeficiente para o ponto atual
    A[index, index] <- 2 * alpha / (h^2) + k
    
    # Termo independente correspondente ao ponto atual
    b[index] <- k * Ti
  }
}

# Aplicação das condições de contorno

# Condição de contorno para os nós da borda interna
for (j in 1:Ny) {
  index1 <- j
  index2 <- (Nx - 1) * Ny + j
  
  A[index1, ] <- 0
  A[index1, index1] <- 1
  b[index1] <- Ti
  
  A[index2, ] <- 0
  A[index2, index2] <- 1
  b[index2] <- Ti - (hi * h * (Ti - Te))
}

# Condição de contorno para os nós da borda externa
for (i in 1:Nx) {
  index1 <- (i - 1) * Ny + 1
  index2 <- (i - 1) * Ny + Ny
  
  A[index1, ] <- 0
  A[index1, index1] <- 1
  b[index1] <- Te
  
  A[index2, ] <- 0
  A[index2, index2] <- 1
  b[index2] <- Te - (he * h * (Ti - Te))
}

# Resolução do sistema linear
T_atual <- solve(A, b)

# Redimensionamento do vetor de temperaturas em uma matriz
T_atual <- matrix(T_atual, nrow = Nx, ncol = Ny)

# Plot do perfil de temperatura
x <- seq(L_interno, L_externo, length.out = Nx)
y <- seq(L_interno, L_externo, length.out = Ny)

# Criação da grade para o plot
grid <- expand.grid(x = x, y = y)

# Preparação dos dados para o plot
z <- as.vector(T_atual)

# Plot do perfil de temperatura
library("scatterplot3d")
scatterplot3d(grid$x, grid$y, z, color = "blue", main = "Perfil de Temperatura",
              xlab = "Posição X (mm)", ylab = "Posição Y (mm)", zlab = "Temperatura (°C)")
