rm(list = ls())

nodes <- 12
a <- matrix(ncol = nodes, nrow = nodes)

he <- 5
te <- 25
hi <- 100
ti <- 350
k <- 0.85

c <- rep(0, nodes)

l1 <- rep(0, nodes)
l1[5] <- 1
l1[2] <- 1
l1[1] <- -(2 + (he / k))
c[1] <- - (he / k) * te

l2 <- rep(0, nodes)
l2[1] <- 1
l2[6] <- 1
l2[3] <- 1
l2[2] <- -(3 + (he / k))
c[2] <- - (he / k) * te

l3 <- rep(0, nodes)
l3[2] <- 1
l3[7] <- 1
l3[4] <- 1
l3[3] <- -(3 + (he / k))
c[3] <- - (he / k) * te

l4 <- rep(0, nodes)
l4[3] <- 1
l4[8] <- 1
l4[4] <- -2 * (1 + (he / k))
c[4] <- - 2 * (he / k) * te

l5 <- rep(0, nodes)
l5[1] <- 1
l5[6] <- 1
l5[5] <- - (2 + (hi / k))
c[5] <- - (hi / k) * ti

l6 <- rep(0, nodes)
l6[5] <- 1
l6[2] <- 1
l6[7] <- 1
l6[6] <- - (3 + (hi / k))
c[6] <- - (hi / k) * ti

l7 <- rep(0, nodes)
l7[6] <- 1
l7[3] <- 1
l7[8] <- 1
l7[9] <- 1
l7[7] <- -4
c[7] <- 0

l8 <- rep(0, nodes)
l8[4] <- 1
l8[7] <- 1
l8[10] <- 1
l8[8] <- -(3 + (he / k))
c[8] <- - (he / k) * te

l9 <- rep(0, nodes)
l9[7] <- 1
l9[10] <- 1
l9[11] <- 1
l9[9] <- - (3 + (hi / k))
c[9] <- - (hi / k) * ti

l10 <- rep(0, nodes)
l10[8] <- 1
l10[9] <- 1
l10[12] <- 1
l10[10] <- -(3 + (he / k))
c[10] <- - (he / k) * te

l11 <- rep(0, nodes)
l11[12] <- 1
l11[9] <- 1
l11[11] <- - (2 + (hi / k))
c[11] <- - (hi / k) * ti

l12 <- rep(0, nodes)
l12[11] <- 1
l12[10] <- 1
l12[1] <- -(2 + (he / k))
c[12] <- - (he / k) * te

a[1, ] <- l1
a[2, ] <- l2
a[3, ] <- l3
a[4, ] <- l4
a[5, ] <- l5
a[6, ] <- l6
a[7, ] <- l7
a[8, ] <- l8
a[9, ] <- l9
a[10, ] <- l10
a[11, ] <- l11
a[12, ] <- l12

t <- solve(a) %*% c
