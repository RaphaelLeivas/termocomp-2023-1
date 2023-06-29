rm(list = ls())
dev.off()

NX <- 8 #quantidade de pontos da malha no sentido horizontal
NY <- 4 #quantidade de pontos da malha no sentido vertical

getMeshNumberByIndex <- function(i, j) {
  
}

lxEx <- 300 #mm : comprimento horizontal exterior (metade)
lxIn <- 150 #mm : comprimento horizontal interior (metade)
ly <- 150 #mm : comprimento vertical (espessura do duto)

#variacoers de distancia dx e dy (mm)
dx <- lxEx/(NX - 1)
dy <- ly/(NY - 1)

#variacao no tempo (s)
dt <- 1

#funcao que limita a diagonal onde a simetria no problema
limiteDiagonal <- function (y) {
  return (y+lxIn)
}

#matriz de com as coordenadas dos pontos da malha (o canto inferior esquerdo esta na origem)
pontos <- list()

#coordenadas dos pontos a serem armazenadas
x<- 0
y<- 0

#preenchendo a matriz de pontos
#i sao as "linhas" de pontos e j as "colunas" (graficamente falando)
for(i in 1:NY) {
  pontos[[i]] <- matrix(c(x,y), nrow = 1, ncol = 2)
  for(j in 2:NX){
    if(x <= limiteDiagonal(y)){
      x <- x + dx
      pontos[[i]] <- rbind(pontos[[i]], matrix(c(x,y), nrow = 1, ncol = 2))
    }
    else{
      break()
    }
  }
  #par(new=T)
  #plot(pontos[[i]], pch = "+", xlim = c(0,lxEx+10), ylim = c(0, ly+10))
  x <- 0
  y <- y + dy
}

#loop que identifica o tipo de malha para aplicar a equacao correta
for(i in 1:NY) {
  tamLinha <- dim(pontos[[i]])[1]
  for (j in 1:tamLinha) {
    #ponto da malha em conveccao com o fluido externo
    if((i == length(pontos)) && (j != 1) && (j != dim(pontos[[i]])[1])) {
      par(new=T)
      plot(pontos[[i]][j,1], pontos[[i]][j,2], pch = "•", xlim = c(0,lxEx+10), ylim = c(0, ly+10), col = "red",
           xlab = " ", ylab = " ")
    }
    
    #ponto da malha em conveccao com o fluido interno
    else if((i == 1) && (j != 1) && (j != dim(pontos[[i]])[1])) {
      par(new=T)
      plot(pontos[[i]][j,1], pontos[[i]][j,2], pch = "•", xlim = c(0,lxEx+10), ylim = c(0, ly+10), col = "blue",
           xlab = " ", ylab = " ")
    }
    
    #ponto da malha em simetria (à esquerda)
    else if(j == 1) {
      par(new=T)
      plot(pontos[[i]][j,1], pontos[[i]][j,2], pch = "•", xlim = c(0,lxEx+10), ylim = c(0, ly+10), col = "green",
           xlab = " ", ylab = " ")
    }
    
    #ponto da malha em simetria (à direita)
    else if(j == dim(pontos[[i]])[1]) {
      par(new=T)
      plot(pontos[[i]][j,1], pontos[[i]][j,2], pch = "•", xlim = c(0,lxEx+10), ylim = c(0, ly+10), col = "green",
           xlab = " x (mm) ", ylab = " y (mm) ")
    }
    
    #ponto da malha interno
    else{
      par(new=T)
      plot(pontos[[i]][j,1], pontos[[i]][j,2], pch = "•", xlim = c(0,lxEx+10), ylim = c(0, ly+10),
           xlab = " x (mm) ", ylab = " y (mm) ")
      
      if (i == 2 && j == 2) {
        north <- i + j + 1
        south <- i + j - 1
        east <- i + 1 + j
        west <- i - 1 + j
      }
    }
  }
}

