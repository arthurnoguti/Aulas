library(tidyverse)
library(rgl)
library(cowplot)

# Importando os dados
wrn <- (read.csv2("/Users/Arthur/Desktop/Estatística/Teoria do Aprendizado Estatístico/wall-robot-navigation.csv"))

#Entendendo o problema
head(wrn)
table(wrn$Y)
str(wrn)
plot3d(wrn$X1 , wrn$X2 , wrn$Y)
range(wrn$X1)
range(wrn$X2)

#####Dividindo em grupos de 20%, Cross validation k = 5 ####
#Método de Validacao Cruzada k-fold k = 5

set.seed(2)

y <- c(1:5456)
sorteio1 <- sample(y, 1091)
sorteio2 <- sample(y[-sorteio1], 1091)
sorteio3 <- sample(y[c(-sorteio1, - sorteio2)], 1091)
sorteio4 <- sample(y[c(-sorteio1, - sorteio2, -sorteio3)], 1091)
sorteio5 <- sample(y[c(-sorteio1, - sorteio2, -sorteio3, - sorteio4)], 1092)

wrn_treinamento1 <- wrn[-sorteio1, ]
wrn_teste1 <- wrn[sorteio1, ]

wrn_treinamento2 <- wrn[-sorteio2, ]
wrn_teste2 <- wrn[sorteio2, ]

wrn_treinamento3 <- wrn[-sorteio3, ]
wrn_teste3 <- wrn[sorteio3, ]

wrn_treinamento4 <- wrn[-sorteio4, ]
wrn_teste4 <- wrn[sorteio4, ]

wrn_treinamento5 <- wrn[-sorteio5, ]
wrn_teste5 <- wrn[sorteio5, ] 

#### Rodada 1 ####
# x novo  
x_new1 <- wrn_teste1[ ,-3]
y_fit1 <- c()
ind1 <- 1:nrow(wrn_treinamento1)

# Definindo K 
k = 5

for (i in 1:length(wrn_teste1$X1)) {
  # Calculando a distancia
  D1 <- sqrt((as.numeric(wrn_treinamento1$X1) - 
               as.numeric(x_new1[i,1]))^2+(as.numeric(wrn_treinamento1$X2) - 
                                            as.numeric(x_new1[i,2]))^2)
  # Encontrando os vizinhos
  viz1 <- ind1[order(D1)][1:k]
  #y previsto
  y_fit10 <- round(mean(wrn_treinamento1$Y[viz1]), 0)
  y_fit1 <- c(y_fit1, y_fit10)
}
y_fit1
matriz_conf1 <- table(y_fit1, wrn_teste1$Y)
matriz_conf1

#### Rodada 2 ####
# x novo  
x_new2 <- wrn_teste2[ ,-3]
y_fit2 <- c()
ind2 <- 1:nrow(wrn_treinamento2)

# Definindo K 
k = 5

for (i in 1:length(wrn_teste2$X1)) {
  # Calculando a distancia
  D2 <- sqrt((as.numeric(wrn_treinamento2$X1) - 
                as.numeric(x_new2[i,1]))^2+(as.numeric(wrn_treinamento2$X2) - 
                                              as.numeric(x_new2[i,2]))^2)
  # Encontrando os vizinhos
  viz2 <- ind2[order(D2)][1:k]
  #y previsto
  y_fit20 <- round(mean(wrn_treinamento2$Y[viz2]), 0)
  y_fit2 <- c(y_fit2, y_fit20)
}
y_fit2
matriz_conf2 <- table(y_fit2, wrn_teste2$Y)
matriz_conf2




#### Rodada 3 ####
# x novo  
x_new3 <- wrn_teste3[ ,-3]
y_fit3 <- c()
ind3 <- 1:nrow(wrn_treinamento3)

# Definindo K 
k = 5

for (i in 1:length(wrn_teste3$X1)) {
  # Calculando a distancia
  D3 <- sqrt((as.numeric(wrn_treinamento3$X1) - 
                as.numeric(x_new3[i,1]))^2+(as.numeric(wrn_treinamento3$X2) - 
                                              as.numeric(x_new3[i,2]))^2)
  # Encontrando os vizinhos
  viz3 <- ind3[order(D3)][1:k]
  #y previsto
  y_fit30 <- round(mean(wrn_treinamento3$Y[viz3]), 0)
  y_fit3 <- c(y_fit3, y_fit30)
}
y_fit3
matriz_conf3 <- table(y_fit3, wrn_teste3$Y)
matriz_conf3
#### Rodada 4 ####
# x novo  
x_new4 <- wrn_teste4[ ,-3]
y_fit4 <- c()
ind4 <- 1:nrow(wrn_treinamento4)

# Definindo K 
k = 5

for (i in 1:length(wrn_teste4$X1)) {
  # Calculando a distancia
  D4 <- sqrt((as.numeric(wrn_treinamento4$X1) - 
                as.numeric(x_new4[i,1]))^2+(as.numeric(wrn_treinamento4$X2) - 
                                              as.numeric(x_new4[i,2]))^2)
  # Encontrando os vizinhos
  viz4 <- ind4[order(D4)][1:k]
  #y previsto
  y_fit40 <- round(mean(wrn_treinamento4$Y[viz4]), 0)
  y_fit4 <- c(y_fit4, y_fit40)
}
y_fit4
matriz_conf4 <- table(y_fit4, wrn_teste4$Y)
matriz_conf4



#### Rodada 5 ####
# x novo  
x_new5 <- wrn_teste5[ ,-3]
y_fit5 <- c()
ind5 <- 1:nrow(wrn_treinamento5)

# Definindo K 
k = 5

for (i in 1:length(wrn_teste5$X1)) {
  # Calculando a distancia
  D5 <- sqrt((as.numeric(wrn_treinamento5$X1) - 
                as.numeric(x_new5[i,1]))^2+(as.numeric(wrn_treinamento5$X2) - 
                                              as.numeric(x_new5[i,2]))^2)
  # Encontrando os vizinhos
  viz5 <- ind5[order(D5)][1:k]
  #y previsto
  y_fit50 <- round(mean(wrn_treinamento5$Y[viz5]), 0)
  y_fit5 <- c(y_fit5, y_fit50)
}
y_fit5
matriz_conf5 <- table(y_fit5, wrn_teste5$Y)
matriz_conf5

####medidas desemp rod 1 ####
mc1.1 <- matriz_conf1[c(1,2),c(1,2)]
mc2.1 <- matriz_conf1[c(1,3),c(1,3)]
mc3.1 <- matriz_conf1[c(1,4),c(1,4)]
mc4.1 <- matriz_conf1[c(2,3),c(2,3)]
mc5.1 <- matriz_conf1[c(2,4),c(2,4)]
mc6.1 <- matriz_conf1[c(3,4),c(3,4)]
list_matriz1 <- list(mc1.1,mc2.1,mc3.1,mc4.1,mc5.1,mc6.1)



#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro1 <- sum(sapply(list_matriz1, acc_macro))/6
acc_macro1

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro1 <- sum(sapply(list_matriz1, sen_macro))/6
sen_macro1

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro1 <- sum(sapply(list_matriz1, esp_macro))/6
esp_macro1

#acuracia balanceada

BA_macro1 <- (sen_macro1+esp_macro1)/2
BA_macro1

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro1 <- sum(sapply(list_matriz1, p_macro))/6
p_macro1

#f1-score
f1_macro1 <- (2*p_macro1*sen_macro1)/(p_macro1+sen_macro1)
f1_macro1

#MICRO
sum_mc1 <- mc1.1+mc2.1+mc3.1+mc4.1+mc5.1+mc6.1
sum_mc1
acc_micro1 <- (sum_mc1[1,1]+sum_mc1[2,2])/sum(sum_mc1) #acuracia
sen_micro1 <- sum_mc1[1,1]/(sum_mc1[1,1]+sum_mc1[1,2]) #sensibilidade ou recall
esp_micro1 <- sum_mc1[2,2]/(sum_mc1[2,2]+sum_mc1[2,1]) #especificidade
BA_micro1 <- (sen_micro1+esp_micro1)/2
p_micro1 <- sum_mc1[1,1]/(sum_mc1[1,1]+sum_mc1[2,1])
f1_micro1 <- (2*p_micro1*sen_micro1)/(p_micro1+sen_micro1)

####medidas desemp rod 2 ####
mc1.2 <- matriz_conf2[c(1,2),c(1,2)]
mc2.2 <- matriz_conf2[c(1,3),c(1,3)]
mc3.2 <- matriz_conf2[c(1,4),c(1,4)]
mc4.2 <- matriz_conf2[c(2,3),c(2,3)]
mc5.2 <- matriz_conf2[c(2,4),c(2,4)]
mc6.2 <- matriz_conf2[c(3,4),c(3,4)]
list_matriz2 <- list(mc1.2,mc2.2,mc3.2,mc4.2,mc5.2,mc6.2)

#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro2 <- sum(sapply(list_matriz2, acc_macro))/6
acc_macro2

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro2 <- sum(sapply(list_matriz2, sen_macro))/6
sen_macro2

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro2 <- sum(sapply(list_matriz2, esp_macro))/6
esp_macro2

#acuracia balanceada

BA_macro2 <- (sen_macro2+esp_macro2)/2
BA_macro2

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro2 <- sum(sapply(list_matriz2, p_macro))/6
p_macro2

#f1-score
f1_macro2 <- (2*p_macro2*sen_macro2)/(p_macro2+sen_macro2)
f1_macro2

#MICRO
sum_mc2 <- mc1.2+mc2.2+mc3.2+mc4.2+mc5.2+mc6.2
sum_mc2
acc_micro2 <- (sum_mc2[1,1]+sum_mc2[2,2])/sum(sum_mc2) #acuracia
sen_micro2 <- sum_mc2[1,1]/(sum_mc2[1,1]+sum_mc2[1,2]) #sensibilidade ou recall
esp_micro2 <- sum_mc2[2,2]/(sum_mc2[2,2]+sum_mc2[2,1]) #especificidade
BA_micro2 <- (sen_micro2+esp_micro2)/2
p_micro2 <- sum_mc2[1,1]/(sum_mc2[1,1]+sum_mc2[2,1])
f1_micro2 <- (2*p_micro2*sen_micro2)/(p_micro2+sen_micro2)

####medidas desemp rod 3 ####
mc1.3 <- matriz_conf3[c(1,2),c(1,2)]
mc2.3 <- matriz_conf3[c(1,3),c(1,3)]
mc3.3 <- matriz_conf3[c(1,4),c(1,4)]
mc4.3 <- matriz_conf3[c(2,3),c(2,3)]
mc5.3 <- matriz_conf3[c(2,4),c(2,4)]
mc6.3 <- matriz_conf3[c(3,4),c(3,4)]
list_matriz3 <- list(mc1.3,mc2.3,mc3.3,mc4.3,mc5.3,mc6.3)

#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro3 <- sum(sapply(list_matriz3, acc_macro))/6
acc_macro3

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro3 <- sum(sapply(list_matriz3, sen_macro))/6
sen_macro3

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro3 <- sum(sapply(list_matriz3, esp_macro))/6
esp_macro3

#acuracia balanceada

BA_macro3 <- (sen_macro3+esp_macro3)/2
BA_macro3

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro3 <- sum(sapply(list_matriz3, p_macro))/6
p_macro3

#f1-score
f1_macro3 <- (2*p_macro3*sen_macro3)/(p_macro3+sen_macro3)
f1_macro3

#MICRO
sum_mc3 <- mc1.3+mc2.3+mc3.3+mc4.3+mc5.3+mc6.3
sum_mc3
acc_micro3 <- (sum_mc3[1,1]+sum_mc3[2,2])/sum(sum_mc3) #acuracia
sen_micro3 <- sum_mc3[1,1]/(sum_mc3[1,1]+sum_mc3[1,2]) #sensibilidade ou recall
esp_micro3 <- sum_mc3[2,2]/(sum_mc3[2,2]+sum_mc3[2,1]) #especificidade
BA_micro3 <- (sen_micro3+esp_micro3)/2
p_micro3 <- sum_mc3[1,1]/(sum_mc3[1,1]+sum_mc3[2,1])
f1_micro3 <- (2*p_micro3*sen_micro3)/(p_micro3+sen_micro3)
####medidas desemp rod 4 ####
mc1.4 <- matriz_conf4[c(1,2),c(1,2)]
mc2.4 <- matriz_conf4[c(1,3),c(1,3)]
mc3.4 <- matriz_conf4[c(1,4),c(1,4)]
mc4.4 <- matriz_conf4[c(2,3),c(2,3)]
mc5.4 <- matriz_conf4[c(2,4),c(2,4)]
mc6.4 <- matriz_conf4[c(3,4),c(3,4)]
list_matriz4 <- list(mc1.4,mc2.4,mc3.4,mc4.4,mc5.4,mc6.4)



#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro4 <- sum(sapply(list_matriz4, acc_macro))/6
acc_macro4

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro4 <- sum(sapply(list_matriz4, sen_macro))/6
sen_macro4

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro4 <- sum(sapply(list_matriz4, esp_macro))/6
esp_macro4

#acuracia balanceada

BA_macro4 <- (sen_macro4+esp_macro4)/2
BA_macro4

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro4 <- sum(sapply(list_matriz4, p_macro))/6
p_macro4

#f1-score
f1_macro4 <- (2*p_macro4*sen_macro4)/(p_macro4+sen_macro4)
f1_macro4

#MICRO
sum_mc4 <- mc1.4+mc2.4+mc3.4+mc4.4+mc5.4+mc6.4
sum_mc4
acc_micro4 <- (sum_mc4[1,1]+sum_mc4[2,2])/sum(sum_mc4) #acuracia
sen_micro4 <- sum_mc4[1,1]/(sum_mc4[1,1]+sum_mc4[1,2]) #sensibilidade ou recall
esp_micro4 <- sum_mc4[2,2]/(sum_mc4[2,2]+sum_mc4[2,1]) #especificidade
BA_micro4 <- (sen_micro4+esp_micro4)/2
p_micro4 <- sum_mc4[1,1]/(sum_mc4[1,1]+sum_mc4[2,1])
f1_micro4 <- (2*p_micro4*sen_micro4)/(p_micro4+sen_micro4)

####medidas desemp rod 5 ####
mc1.5 <- matriz_conf5[c(1,2),c(1,2)]
mc2.5 <- matriz_conf5[c(1,3),c(1,3)]
mc3.5 <- matriz_conf5[c(1,4),c(1,4)]
mc4.5 <- matriz_conf5[c(2,3),c(2,3)]
mc5.5 <- matriz_conf5[c(2,4),c(2,4)]
mc6.5 <- matriz_conf5[c(3,4),c(3,4)]
list_matriz5 <- list(mc1.5,mc2.5,mc3.5,mc4.5,mc5.5,mc6.5)



#MACRO 
#acuracia
acc_macro <- function(x){
  (x[1,1]+x[2,2])/sum(x)
}
acc_macro5 <- sum(sapply(list_matriz5, acc_macro))/6
acc_macro5

#sensibilidade ou recall
sen_macro <- function(x){
  x[1,1]/(x[1,1]+x[1,2])
}
sen_macro5 <- sum(sapply(list_matriz5, sen_macro))/6
sen_macro5

#especificidade
esp_macro <- function(x){
  x[2,2]/(x[2,2]+x[2,1])
}
esp_macro5 <- sum(sapply(list_matriz5, esp_macro))/6
esp_macro5

#acuracia balanceada

BA_macro5 <- (sen_macro5+esp_macro5)/2
BA_macro5

#precisao
p_macro <- function(x){
  x[1,1]/(x[1,1]+x[2,1])
}
p_macro5 <- sum(sapply(list_matriz5, p_macro))/6
p_macro5

#f1-score
f1_macro5 <- (2*p_macro5*sen_macro5)/(p_macro5+sen_macro5)
f1_macro5

#MICRO
sum_mc5 <- mc1.5+mc2.5+mc3.5+mc4.5+mc5.5+mc6.5
sum_mc5
acc_micro5 <- (sum_mc5[1,1]+sum_mc5[2,2])/sum(sum_mc5) #acuracia
sen_micro5 <- sum_mc5[1,1]/(sum_mc5[1,1]+sum_mc5[1,2]) #sensibilidade ou recall
esp_micro5 <- sum_mc5[2,2]/(sum_mc5[2,2]+sum_mc5[2,1]) #especificidade
BA_micro5 <- (sen_micro5+esp_micro5)/2
p_micro5 <- sum_mc5[1,1]/(sum_mc5[1,1]+sum_mc5[2,1])
f1_micro5 <- (2*p_micro5*sen_micro5)/(p_micro5+sen_micro5)


#### medida de desempenho geral ####
acc_macro_geral <- (acc_macro1+acc_macro2+acc_macro3+acc_macro4+acc_macro5)/5
sen_macro_geral <- (sen_macro1+sen_macro2+sen_macro3+sen_macro4+sen_macro5)/5
esp_macro_geral <- (esp_macro1+esp_macro2+esp_macro3+esp_macro4+esp_macro5)/5
BA_macro_geral <- (BA_macro1+BA_macro2+BA_macro3+BA_macro4+BA_macro5)/5
p_macro_geral <- (p_macro1+p_macro2+p_macro3+p_macro4+p_macro5)/5
f1_macro_geral <- (f1_macro1+f1_macro2+f1_macro3+f1_macro4+f1_macro5)/5


acc_micro_geral <- (acc_micro1+acc_micro2+acc_micro3+acc_micro4+acc_micro5)/5
sen_micro_geral <- (sen_micro1+sen_micro2+sen_micro3+sen_micro4+sen_micro5)/5
esp_micro_geral <- (esp_micro1+esp_micro2+esp_micro3+esp_micro4+esp_micro5)/5
BA_micro_geral <- (BA_micro1+BA_micro2+BA_micro3+BA_micro4+BA_micro5)/5
p_micro_geral <- (p_micro1+p_micro2+p_micro3+p_micro4+p_micro5)/5
f1_micro_geral <- (f1_micro1+f1_micro2+f1_micro3+f1_micro4+f1_micro5)/5

coluna_macro <- c(acc_macro_geral,sen_macro_geral,esp_macro_geral,BA_macro_geral,p_macro_geral,f1_macro_geral)
coluna_micro <- c(acc_micro_geral,sen_micro_geral,esp_micro_geral,BA_micro_geral,p_micro_geral,f1_micro_geral)

metricas <- data.frame(
  desempenho = c("acuracia","sensibilidade","especificidade","acuracia balanceada","precisao","f1-score"),
  macro = coluna_macro,
  micro = coluna_micro
)
metricas

##### Fronteira de decisão ####

eg <- expand.grid(seq(0,5,length.out = 100),
            seq(0,5,length.out = 100))

names(eg) <- c("X1", "X2")

x_neweg <- eg
y_fiteg <- c()
indeg <- 1:nrow(eg)
k = 9

for (i in 1:length(eg$X1)) {
  # Calculando a distancia
  D <- sqrt((as.numeric(wrn$X1) - 
               as.numeric(x_neweg[i,1]))^2+(as.numeric(wrn$X2) - 
                                            as.numeric(x_neweg[i,2]))^2)
  # Encontrando os vizinhos
  viz <- indeg[order(D)][1:k]
  #y previsto
  y_fit1 <- round(mean(wrn$Y[viz]), 0)
  y_fiteg <- c(y_fiteg, y_fit1)
}
y_fiteg
eg["Y"] <- y_fiteg

library(cowplot)

p1 <- ggplot(eg)+
  geom_point(aes(x=X1,y=X2,colour=as.factor(Y)),size=3,show.legend = T)+
  theme_bw()+
  labs(title = "                                          k = 3")

p2 <- ggplot(eg)+
  geom_point(aes(x=X1,y=X2,colour=as.factor(Y)),size=3,show.legend = T)+
  theme_bw()+
  labs(title = "                                          k = 5")

p3 <- ggplot(eg)+
  geom_point(aes(x=X1,y=X2,colour=as.factor(Y)),size=3,show.legend = T)+
  theme_bw()+
  labs(title = "                                          k = 7")

p4 <- ggplot(eg)+
  geom_point(aes(x=X1,y=X2,colour=as.factor(Y)),size=3,show.legend = T)+
  theme_bw()+
  labs(title = "                                          k = 9")

plot_grid(p1,p2,p3,p4, ncol = 2)
     