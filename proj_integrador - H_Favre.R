# Baixar o arquivo com dados
dataset <- read.csv("http://leg.ufpr.br/~wagner/data/youtube.txt", 
                    sep = " ", header = TRUE)

# Dividindo em dois conjuntos, um para cada canal
dataset_canal <- split(dataset, dataset$CANAL)
dados1 <- dataset_canal[[1]]
dados2 <- dataset_canal[[2]]
dados1$INSCRITOS <- dados1$INSCRITOS/100000
dados1$Y <- cumsum(dados1$INSCRITOS)
dados2$INSCRITOS <- dados2$INSCRITOS/100000
dados2$Y <- cumsum(dados2$INSCRITOS)

# Alvos de predicao
dataalvocanal1 <- max(dados1$DIAS)+365
dataalvocanal2 <- max(dados2$DIAS)+365

# Definindo a funcao para ajuste dos dados
funcao <- function(L, beta0, beta, dias){
  y <- L/(1+exp(beta*(dias-beta0)))
  return(y)
}

# Definindo a funcao perda como RMSE - Root Mean Squared Error
perda <- function (param, y, dias){
  mu <- funcao(L = param[1], beta0 = param[2], beta=param[3], dias = dias)
  out <- sum((y-mu)^2)
  return(out)
}

# Lista de metodos disponiveis na funcao optim, vista em sala
metodos <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")

### CANAL 1
# Definindo alguns parametros iniciais em tentativa e erro para Canal 1
parametrosc1 = c(30, 700, -0.008)

# Verificando o valor de RMSE para esse caso particular
print(perda(param = parametrosc1, y = dados1$Y, dias = dados1$DIAS))

# Loop para definir qual metodo usar
rmse <- c()
for (i in 1:length(metodos)){
  print (metodos[i])
  rmse[i] <- optim (par = parametrosc1, fn = perda, y = dados1$Y, 
                    dias = dados1$DIAS, method = metodos[i])$value
}

# Usar o método que levou ao mínimo rmse
melhor_metodo_canal1 <- metodos[which(rmse == min(rmse))]


# Usando o otimizador visto em sala para encontrar melhores parametros 
# a partir dos iniciais
otimc1 <- optim (par = parametrosc1, fn = perda, y = dados1$Y, 
                 dias = dados1$DIAS, method = melhor_metodo_canal1)

# Print dos parametros otimizados
print(paste("Melhor L: ",otimc1$par[1]))
print(paste("Melhor beta: ",otimc1$par[2]))
print(paste("Melhor beta1: ",otimc1$par[3]))

# Dados com os valores preditos para o Canal para cada dia
valores_preditos_canal1 <- funcao(L = otimc1$par[1], beta0 = otimc1$par[2], 
                           beta = otimc1$par[3], dias = seq(0,dataalvocanal1))



### CANAL 2
# Definindo alguns parametros iniciais em tentativa e erro para Canal 2
parametrosc2 = c(29, 700, -0.0045)

# Verificando o valor de RMSE para esse caso particular
print(perda(param = parametrosc2, y = dados2$Y, dias = dados2$DIAS))

# Loop para definir qual metodo usar
rmsec2 <- c()
for (i in 1:length(metodos)){
  print (metodos[i])
  rmsec2[i] <- optim (par = parametrosc2, fn = perda, y = dados2$Y, 
                      dias = dados2$DIAS, method = metodos[i])$value
}

# Usar o método que levou ao mínimo rmse
melhor_metodo_canal2 <- metodos[which(rmsec2 == min(rmsec2))]


# Usando o otimizador visto em sala para encontrar melhores parametros 
# a partir dos iniciais
otimc2 <- optim (par = parametrosc2, fn = perda, y = dados2$Y, 
                 dias = dados2$DIAS, method = melhor_metodo_canal2)

# Print dos parametros otimizados
print(paste("Melhor L: ",otimc2$par[1]))
print(paste("Melhor beta: ",otimc2$par[2]))
print(paste("Melhor beta1: ",otimc2$par[3]))

# Dados com os valores preditos para o Canal para cada dia
valores_preditos_canal2 <- funcao(L = otimc2$par[1], beta0 = otimc2$par[2], 
                                  beta = otimc2$par[3], 
                                  dias = seq(0,dataalvocanal2))


# Valores preditos em 365 dias para cada canal
predicao365_canal1 <- valores_preditos_canal1[dataalvocanal1]
predicao365_canal2 <- valores_preditos_canal2[dataalvocanal2]

#Gráficos finais, com dados reais e ajustes
par(mfrow = c(1,2), mar=c(2.6, 3, 1.2, 0.5), mgp = c(1.6, 0.6, 0))
plot(dados1$Y ~ dados1$DIAS, xlim = c(0, dataalvocanal1), ylim = c(0, 50),
     ylab = "Numero de inscritos*100000", main = "Canal 1",
     xlab = "Dias", type = "o", cex = 0.1)
abline(v = max(dados1$DIAS))
lines (valores_preditos_canal1 ~ seq(0,dataalvocanal1), col = 'deepskyblue1')
text(dataalvocanal1-350, otimc1$par[1]-5,
     labels = paste0("Preditos ",round(predicao365_canal1,5)*100000,
                     '\n'," inscritos"),
     cex = .8, pos = 4, col = "purple")
plot(dados2$Y ~ dados2$DIAS, xlim = c(0, dataalvocanal2), ylim = c(0, 50),
     ylab = "Numero de inscritos*100000", main = "Canal 2",
     xlab = "Dias", type = "o", cex = 0.1)
abline(v = max(dados2$DIAS))
lines (valores_preditos_canal2 ~ seq(0,dataalvocanal2), col = 'deepskyblue1')
text(dataalvocanal2-300, otimc2$par[1]-4,
  labels = paste0("Preditos ",round(predicao365_canal2,5)*100000,
                  '\n'," inscritos"),
     cex = .8, pos = 4, col = "purple")

