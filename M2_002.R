#Máquina 2 - simulação da máquina 2
#Versão 0.0.2 - teste

#TIPOS
#1 - Identificação de voz
#2 - Voz nova
#3 - Roda modelo

#0 - Packages, Libraries, constantes e funções
{
rm(list = ls(all.names = TRUE))
install.packages("neuralnet")
install.packages("tictoc")
install.packages("gmodels")
library(neuralnet)
library(tictoc)
library(gmodels)
}

#Escreve log da execução
LOG <- data.frame(
  ID_LOGS=integer(),
  NOME_ARQUIVO=character(),
  ID_ETAPA=integer(),
  TIPO=integer(),
  HR_INICIO=as.Date(character()),
  HR_FINAL=as.Date(character()), 
  DURACAO=double(),
  RESULTADO=integer(),
  stringsAsFactors=FALSE)

#1 - Lê os frames do KAFKA, VoiceMatrix e modelo_nn

arquivos <- list.files(path = "C:/Users/edube/Documents/Archangel/teste_M2/")
modelo_nn <- 
VoiceMatrix <- read.csv(file = "C:/Users/edube/Documents/Archangel/teste_M2/")

for(a in 1:length(arquivos)){
tic()
ti <- Sys.time()
frame <- as.data.frame(read.csv(file = paste("C:/Users/edube/Documents/Archangel/teste_M2/",arquivos[a],sep = ""),sep = ";")) 
max_log <- ifelse(nrow(LOG) == 0,0,max(LOG$ID_LOGS))
t <- toc()
t <- t$toc - t$tic
tf <- Sys.time()
lista <- list(ID_LOGS = max_log + 1, NOME_ARQUIVO = arquivos[a], ID_ETAPA = 1, TIPO = 1, HR_INICIO = ti, HR_FINAL = tf, DURACAO = t,RESULTADO = 0)
LOG$NOME_ARQUIVO <- as.character(LOG$NOME_ARQUIVO)
LOG <- rbind(LOG,lista)

if(TIPO = 1){
  tic()
  ti <- Sys.time()
  pred <- predict(modelo, frame)
  ID_predito <- apply(pred,1,which.max)
  t <- toc()
  t <- t$toc - t$tic
  tf <- Sys.time()
  lista <- list(ID_LOGS = max(LOG$ID_LOGS) + 1, NOME_ARQUIVO = arquivos[a], ID_ETAPA = 8, TIPO = TIPO, HR_INICIO = ti, HR_FINAL = tf, DURACAO = t,RESULTADO = ID_predito)
  LOG <- rbind(LOG,lista)
}

if(TIPO = 2){
  tic()
  ti <- Sys.time()
  max_id_user <- max(VoiceMatrix$ID_USER)
  ID_USER <- max_id_user + 1
  frame$ID_USER <- ID_USER
  VoiceMatrix <- rbind(VoiceMatrix,frame)
  t <- toc()
  t <- t$toc - t$tic
  tf <- Sys.time()
  lista <- list(ID_LOGS = max(LOG$ID_LOGS) + 1, NOME_ARQUIVO = arquivos[a], ID_ETAPA = 9, TIPO = TIPO, HR_INICIO = ti, HR_FINAL = tf, DURACAO = t,RESULTADO = ID_USER)
  LOG <- rbind(LOG,lista)
}

if(TIPO = 3){
  tic()
  ti <- Sys.time()
  modelo <-neuralnet(ID_USER~., data=VoiceMatrix, hidden=2)
  #escreve modelo
  t <- toc()
  t <- t$toc - t$tic
  tf <- Sys.time()
  lista <- list(ID_LOGS = max(LOG$ID_LOGS) + 1, NOME_ARQUIVO = arquivos[a], ID_ETAPA = 0, TIPO = TIPO, HR_INICIO = ti, HR_FINAL = tf, DURACAO = t,RESULTADO = 1)
  LOG <- rbind(LOG,lista)
}
}

#7 - Escreve LOG
nome_arquivo <- paste("LOG_",format(Sys.time(), "%Y%M%d"),sep = "")
write.table(LOG,file = paste("C:/Users/edube/Documents/Archangel/teste_M3/",nome_arquivo,sep = ""),sep = ";")
rm(list = ls(all.names = TRUE))
