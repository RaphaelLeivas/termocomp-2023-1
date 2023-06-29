rm(list = ls())
# dev.off()

# dados do problema
k <- 2.5
W <- 5
H <- 2
L <- 0.25
u_inf <- 3
T_inf <- 300
q_pres <- 750
T_d <- 350

# propriedades termofisicas
Properties_Table = matrix(
  c(
    100, 3.5562, 71.1 * 10^-7, 2.00 * 10^-6, 9.34 * 10^-3, 0.786,
    150, 2.3364, 103.4 * 10^-7, 4.426 * 10^-6, 13.8 * 10^-3, 0.758,
    200, 1.7458, 132.5 * 10^-7, 7.590 * 10^-6, 18.1 * 10^-3, 0.737,
    250, 1.3947, 159.6 * 10^-7, 11.44 * 10^-6, 22.3 * 10^-3, 0.720,
    300, 1.1614, 184.6 * 10^-7, 15.89 * 10^-6, 26.3 * 10^-3, 0.707,
    350, 0.9950, 208.2 * 10^-7, 20.92 * 10^-6, 30.0 * 10^-3, 0.700,
    400, 0.8711, 230.1 * 10^-7, 26.41 * 10^-6, 33.8 * 10^-3, 0.690,
    450, 0.7740, 250.7 * 10^-7, 32.39 * 10^-6, 37.3 * 10^-3, 0.686,
    500, 0.6964, 270.1 * 10^-7, 38.79 * 10^-6, 40.7 * 10^-3, 0.684,
    550, 0.6329, 288.4 * 10^-7, 45.57 * 10^-6, 43.9 * 10^-3, 0.683,
    600, 0.5804, 305.8 * 10^-7, 52.69 * 10^-6, 46.9 * 10^-3, 0.685
  ),
  ncol = 6,
  byrow = TRUE
)

colnames(Properties_Table) <- c('Tf', 'rho', 'mu', 'v', 'kf', 'Pr')
rownames(Properties_Table) <- seq(1, nrow(Properties_Table), 1)
Properties_Table <- as.table(Properties_Table)

max_iterations <- 100
T_e_calculated <- 298 # chute inicial: Te = 298K (ambiente)
T_e_list <- c(T_e_calculated)

# adiciona também a condição de parada do loop para quando a tolerancia de 1% entre os
# os valores calculaos e o anterior, para nao ficar fixo a quantidade de loops

for (i in 1:max_iterations) {
  Tf <- (T_e_calculated + T_inf) / 2
  
  # iniciais
  Tf_min <- Properties_Table[1, 'Tf']
  Tf_min_index <- 1
  Tf_max <- Properties_Table[nrow(Properties_Table), 'Tf']
  Tf_max_index <- nrow(Properties_Table)
  
  # procura na tabela alguem com esse valor de Tf
  for (j in 1:nrow(Properties_Table)) {
    if (Properties_Table[j, 'Tf'] <= Tf) {
      Tf_min <- Properties_Table[j, 'Tf']
      Tf_min_index <- j
    }
    
    if (Properties_Table[j, 'Tf'] > Tf) {
      Tf_max <- Properties_Table[j, 'Tf']
      Tf_max_index <- j
      break
    }
  }
  
  # agora sabemos que Tf esta entre [Tf_min, Tf_max]
  # pega a razao que diz o quao proximo esta de min ou max
  interpolation_ratio <- (Tf - Tf_min) / (Tf_max - Tf_min)
  
  # com a razao de interpolacao, acha as propriedades fisicas interpoladas
  rho_min <- Properties_Table[Tf_min_index, 'rho']
  rho_max <- Properties_Table[Tf_max_index, 'rho']
  rho <- rho_min + (rho_max - rho_min) * interpolation_ratio
  
  mu_min <- Properties_Table[Tf_min_index, 'mu']
  mu_max <- Properties_Table[Tf_max_index, 'mu']
  mu <- mu_min + (mu_max - mu_min) * interpolation_ratio
  
  v_min <- Properties_Table[Tf_min_index, 'v']
  v_max <- Properties_Table[Tf_max_index, 'v']
  v <- v_min + (v_max - v_min) * interpolation_ratio
  
  kf_min <- Properties_Table[Tf_min_index, 'kf']
  kf_max <- Properties_Table[Tf_max_index, 'kf']
  kf <- kf_min + (kf_max - kf_min) * interpolation_ratio
  
  Pr_min <- Properties_Table[Tf_min_index, 'Pr']
  Pr_max <- Properties_Table[Tf_max_index, 'Pr']
  Pr <- Pr_min + (Pr_max - Pr_min) * interpolation_ratio
  
  # assumindo Tf = 300K, sem corrigir a cada iteração
  # rho <- 1.1614 # densidade do ar
  # mu <- 184.6 * 10^-7 # viscosidade dinamica
  # kf <- 26.3 * 10^-3 # condutividade termica do ar
  # Pr <- 0.707 # numero de Prandlt
  
  # calcula os adimensionais
  Re_critical <- 50000
  Re <- u_inf * L * rho / mu
  Nu <- 0.0
  Gr <- 9.81 * (1/Tf) * (abs(T_e_calculated - T_inf) * L^3) / (v^2)
  Ra <- Gr * Pr
  
  if (Re < Re_critical) {
    # escoamento laminar
    Nu <- 0.664 * (Re)^(1/2) * (Pr)^(1/3)
  } else {
    # escoamento turbulento
    Nu <- (0.037 * Re^(4/5) - 871) * (Pr)^(1/3)
  }
  
  # Nu <- (0.825 + (0.387 * Ra^(1/6)) / (1 + (0.492/Pr)^(9/16))^(8/27))^2
  
  # calculado o Numero de Nusselt, achamos o hc
  hc <- Nu * kf / L
  
  # para esse hc, o Te da 1 Lei é
  T_e_calculated <- ((k/L) * T_d + q_pres + T_inf * hc) / (hc + (k/L))
  T_e_list <- append(T_e_list, T_e_calculated)
  
  tolerance <- 0.001 # 0.1% 
  
  # condição de parada, tolerância de 0.1% com o valor anterior
  if (abs(T_e_list[i + 1] - T_e_list[i]) < tolerance * T_e_list[i]) {
    break
  } else {
    next
  }
}

plot(
  T_e_list,
  main = "T_e (K) x Iteração",
  xlab = "Iteração",
  ylab = "T_e (K)",
  col = "black",
  lwd = 3
)

lines(T_e_list, col = "red", lwd = 2, lty = 1)

