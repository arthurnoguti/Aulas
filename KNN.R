library(tidyverse)
library(rgl)
# Importando os dados
wrn <- (read.csv2("/Users/Arthur/Desktop/Estatística/Teoria do Aprendizado Estatístico/wall-robot-navigation.csv"))
head(wrn)
table(wrn$Y)

#Entendendo o problema

plot3d(wrn$X1 , wrn$X2 , wrn$Y)
plot(wrn$X1 , wrn$X2)
range(wrn$X1)
range(wrn$X2)
range(wrn$Y)  #Dados ja estao na mesma escala 

#https://www.youtube.com/watch?v=xi4QozvOJGo
library(caTools)
library(class)
set.seed(2)
divisao <-  sample.split(wrn$Y, SplitRatio = 0.75)
base_treinamento <- subset(wrn, divisao == TRUE)
base_teste <- subset(wrn, divisao == FALSE)

previsao <-  knn(base_treinamento[,-3], base_teste[,-3], cl = base_treinamento[,3], k = 4)

matriz_conf <- table(base_teste[,3], previsao)
matriz_conf
library(caret)
confusionMatrix(matriz_conf)