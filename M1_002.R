#Máquina 1 - simulação de máquina 1
#Versão 0.0.2 - teste

#ETAPAS
#1 - Leitura de arquivo
#2 - Filtragem
#3 - Voz nula
#4 - Cepstrum
#5 - Convolucional e kernel
#6 - Escrita de frame
#7 - Escrita de log
#8 - Resultado Predição
#9 - Nova Voz cadastrada
#0 - Modelo ML atualizado

#0 - Packages, Libraries, constantes e funções
{
rm(list = ls(all.names = TRUE))
#install.packages("tuneR")
#install.packages("seewave")
#install.packages("signal")
#install.packages("tictoc")
library(tuneR)
library(seewave)
library(signal)
library(tictoc)
#Variáveis Universais
ta = 22050 #taxa de amostragem. Fixa para todos os audios que vou usar.
passo = 1/ta #passo
energia_ruido <- 200000 # Energia de ruído, obtida empiricamente. Valor calculado 444788.3
W <- c(0.05,0.6) #faixa de frequência da voz
bf <- butter(4,W, type = "pass")
samples_fft = 220
#Funções Universais
energia <- function(c){
  e <- sum(c^2)/length(c)
  e
}

#Cepstrum
cepstrum <- function(som){
  som_fft <- Mod(fft(som)) #Coeficientes reais da FFT
  som_espectro <- som_fft[1:(samples_fft/2+1)] #Metade do espectro
  som_espectro[2:(samples_fft/2)] <- som_espectro[2:(samples_fft/2)] * 2 #multiplica tudo por 2
  som_sl <- log(som_fft^2) #log do sinal ao quadrado (o total, não o espectro)
  som_sl_fft <-Mod(fft(som_sl)) #Coeficientes reais da FFT
  som_espectro <- som_sl_fft[1:(samples_fft/2+1)] #Metade do espectro
  som_espectro[2:(samples_fft/2)] <- som_espectro[2:(samples_fft/2)] * 2 #multiplica tudo por 2
  som_f <- som_sl_fft^2
  resultado <- som_f/max(som_f)
  resultado
}

#Convolução
convolucao <- function(frame,kernel_matrix){
  
  ke <-as.vector(kernel_matrix)
  len_matrix <- ncol(kernel_matrix)
  row_frame <- nrow(frame)
  col_frame <- ncol(frame)
  lim_row_frame <- row_frame - len_matrix
  lim_col_frame <- col_frame - len_matrix
  lim_row_frame1 <- nrow(frame)-len_matrix + 1
  lim_col_frame1 <- ncol(frame)-len_matrix + 1
  
  frame1 <- matrix(,lim_row_frame1, lim_col_frame1)
  
  for(e in 0:lim_row_frame){
    for(f in 0:lim_col_frame){
      
      e1 = e + 1
      e2 = e + len_matrix
      f1 = f + 1
      f2 = f + len_matrix
      subframe <- as.vector(frame[e1:e2,f1:f2])
      frame1[e1,f1] <- sum(subframe*ke)
    }
  }
  
  return(frame1)
}
#Pooling
pooling <- function(frame,pol){
  
  row_frame <- nrow(frame)
  col_frame <- ncol(frame)
  lim_row_frame <- row_frame - pol
  lim_col_frame <- col_frame - pol
  lim_row_frame1 <- row_frame - pol + 1
  lim_col_frame1 <- col_frame - pol + 1
  
  frame1 <- matrix(,lim_row_frame1, lim_col_frame1)
  
  for(e in 0:lim_row_frame){
    for(f in 0:lim_col_frame){
      e1 = e + 1
      e2 = e + pol
      f1 = f + 1
      f2 = f + pol
      frame1[e1,f1] <- max(frame[e1:e2,f1:f2])
    }
  }
  
  return(frame1)
}

matrix5 <- c(
  1,0,0,0,1,
  0,1,0,1,0,
  0,0,1,0,0,
  0,1,0,1,0,
  1,0,0,0,1)
kernel1 <- matrix(matrix5,nrow = 5, ncol = 5)

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
  
#1 - Leitura de áudios
#Lê áudios de 10ms do coletor
arquivos <- list.files(path = "C:/Users/edube/Documents/Archangel/teste_M1")

for(a in 1:length(arquivos)){
tic()
ti <- Sys.time()
som <- read.table(paste("C:/Users/edube/Documents/Archangel/teste_M1/",arquivos[a],sep = ""),sep = ";")
som <- as.array(som$x)
t <- toc()
t <- t$toc - t$tic
max_log <- ifelse(nrow(LOG) == 0,0,max(LOG$ID_LOGS))
tf <- Sys.time()
lista <- list(ID_LOGS = max_log + 1, NOME_ARQUIVO = arquivos[a], ID_ETAPA = 1, TIPO = 1, HR_INICIO = ti, HR_FINAL = tf, DURACAO = t,RESULTADO = 0)
LOG$NOME_ARQUIVO <- as.character(LOG$NOME_ARQUIVO)
LOG <- rbind(LOG,lista)

#2 - Filtro butterworth
tic()
ti <- Sys.time()
som <- as.integer(filter(bf, som))
t <- toc()
t <- t$toc - t$tic
tf <- Sys.time()
lista <- list(ID_LOGS = max(LOG$ID_LOGS) + 1, NOME_ARQUIVO = arquivos[a], ID_ETAPA = 2, TIPO = 1, HR_INICIO = ti, HR_FINAL = tf, DURACAO = t,RESULTADO = 0)
LOG <- rbind(LOG,lista)

#3 - Verifica se há voz
tic()
ti <- Sys.time()
e <- sum(som^2)/220 #energia do frame
if(e > energia_ruido){
  
#4 - Cepstrum
frame <- cepstrum(som)
tf <- Sys.time()
t <- toc()
t <- t$toc - t$tic
lista <- list(ID_LOGS = max(LOG$ID_LOGS) + 1, NOME_ARQUIVO = arquivos[a], ID_ETAPA = 4, TIPO = 1, HR_INICIO = ti, HR_FINAL = tf, DURACAO = t,RESULTADO = 0)
LOG <- rbind(LOG,lista)

#5 - Convolução e Kernel
tic()
ti <- Sys.time()
frame <- convolucao(matrix(frame,ncol = 10),kernel1)
frame <- as.vector(pooling(frame,5))
tf <- Sys.time()
t <- toc()
t <- t$toc - t$tic
lista <- list(ID_LOGS = max(LOG$ID_LOGS) + 1, NOME_ARQUIVO = arquivos[a], ID_ETAPA = 5, TIPO = 1, HR_INICIO = ti, HR_FINAL = tf, DURACAO = t,RESULTADO = 0)
LOG <- rbind(LOG,lista)

#6 - Escreve frame
tic()
ti <- Sys.time()
write.table(frame,file = paste("C:/Users/edube/Documents/Archangel/teste_M2/",arquivos[a],sep = ""))
tf <- Sys.time()
t <- toc()
t <- t$toc - t$tic
lista <- list(ID_LOGS = max(LOG$ID_LOGS) + 1, NOME_ARQUIVO = arquivos[a], ID_ETAPA = 6, TIPO = 1, HR_INICIO = ti, HR_FINAL = tf, DURACAO = t,RESULTADO = 1)
LOG <- rbind(LOG,lista)
rm(t,tf,ti,som,frame,lista,e,max_log)

}else{
#3.2 - Caso não houver voz, então cria log de saída
tf <- Sys.time()
lista <- list(ID_LOGS = max(LOG$ID_LOGS) + 1, NOME_ARQUIVO = arquivos[a], ID_ETAPA = 3, TIPO = 1, HR_INICIO = ti, HR_FINAL = tf, DURACAO = t,RESULTADO = 2)
LOG <- rbind(LOG,lista)
rm(t,tf,ti,som,lista,e,max_log)
  }
print(paste("Terminado processo",a,sep = " "))
}

#7 - Escreve LOG
nome_arquivo <- paste("LOG_",format(Sys.time(), "%Y%M%d"),sep = "")
write.table(LOG,file = paste("C:/Users/edube/Documents/Archangel/teste_M2/",nome_arquivo,sep = ""),sep = ";")
rm(list = ls(all.names = TRUE))
