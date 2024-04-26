# Para um x_new
library(tidyverse)
library(rgl)
# Importando os dados
wrn <- (read.csv2("/Users/Arthur/Desktop/Estatística/Teoria do Aprendizado Estatístico/wall-robot-navigation.csv"))

#Entendendo o problema
head(wrn)
table(wrn$Y)
str(wrn)
plot3d(wrn$X1 , wrn$X2 , wrn$Y)
range(wrn$X1)
range(wrn$X2)

# x novo para testar, com x1 = 4 e x2 = 1.5 
x_new <- c(4.5,0.2)

# Definindo K 
k = 9

D <- sqrt((as.numeric(wrn$X1) - x_new[1])^2+
            (as.numeric(wrn$X2) - x_new[2])^2)

# Encontrando os vizinhos
ind <- 1:nrow(wrn)
viz <- ind[order(D)][1:k]
#y previsto
y_fit <- round(mean(wrn$Y[viz]), 0)

plot3d(wrn$X1,
       wrn$X2,
       wrn$Y)
points3d(wrn$X1[viz],
       wrn$X2[viz],
       wrn$Y[viz], color = "red", size = 8)
points3d(x_new[1],
         x_new[2],
         y_fit, color = "blue", size =10)
