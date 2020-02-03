#Archangel 8.2: voltando à abordagem de harmônicas de voz, sem usar ML
#Aumentando a eficiência da avaliação

#Teorias:
# 1 - Diminuir a quantidade de frames de cada voz. - FEITO
# 2 - Diminuir número de cepstrums - FEITO
# 3 - Quando identificar um abaixo de X, já traz o resultado. - FEITO
# 4 - Reordenar a tabela de treino - FEITO


#OBS: Tudo ficará mais rápido com processamento paralelo e dedicado, mas
# a comunicação deixará mais lento.

#limpeza de histórico inicial
rm(list = ls(all.names = TRUE))

##Libraries e Instalações
#install.packages("rlang")
library(tuneR)
library(seewave)
library(signal)
library(tictoc)
library(dtw)
library(sqldf)

##Constantes
energia_ruido <- 444788.3 # Energia de ruido, obtida empiricamente. Valor calculado 444788.3
quadro <- 3 #segundos do quadro
threshold <- 0.13 #Valor de identificação de nulos
fv_min <- 50 #frequencia mínima da voz
fv_max <- 3400 #frequência máxima da voz
TA = 8000 #frequência de amostragem

##Tabelas variáveis
frame <- NULL

## Função Archangel8_1

archangel8_1 <- function(frame, numcep){
  MFCC <- NULL
  contador <- NULL
  ID <- frame$ID
  frame$ID <- NULL
  frame <- as.matrix(frame)
  for(a in 1:nrow(frame)){
    
    som <- Wave(left = frame[a,], right = numeric(0), samp.rate = TA, bit = 16, pcm = TRUE)
    
    #A
    som_A <- bwfilter(som,TA,channel = 1, n = 8, from = fv_min, to = fv_max, bandpass = TRUE, listen = FALSE, output = "Wave")
    audio_offset <- som_A@left - mean(som_A@left) #Remove DC Offset
    som_A <- Wave(left = audio_offset, right = numeric(0), samp.rate = TA, bit = 16, pcm = TRUE)
    rm(audio_offset)
    
    #C
    som_C <- Wave(left = som_A@left/max(som_A@left), right = numeric(0), samp.rate = TA, bit = 16, pcm = TRUE)
    
    #D
    audio_D <- ifelse(abs(som_C@left) < threshold, 0, som_C@left)
    audio_D <- audio_D[audio_D != 0]
    som_D <- Wave(left = audio_D, right = numeric(0), samp.rate = TA, bit = 16, pcm = TRUE)
    rm(audio_D)
    
    #E
    if(length(som_D@left) > TA/2){
      E_mfcc <- melfcc(som_D, sr = TA, numcep = numcep)
      MFCC <- rbind(MFCC,as.vector(t(E_mfcc))) 
      contador <- c(contador,a)
    }
    
  }
  MFCC <- as.data.frame(MFCC)
  ID <- ID[contador]
  MFCC$ID <- ID
  return(MFCC)
}

verifica_resultado <- function(TESTE, BASE,ID){
  # ETL Teste: Tira o ID e transforma em matriz do test. Vm = 0.04 s
  teste_id <- TESTE$ID
  TESTE$ID <- NULL
  TESTE <- as.numeric(TESTE)
  b = 1
  diff_dwt = 100
  #Compara um a um. Vm = 0.24 segundos cada
  while(diff_dwt > 1){
    diff_dwt <- dtw(TESTE, BASE[b,])$normalizedDistance
    b = b + 1
    if(b > nrow(BASE)){
      return("Voz não encontrada")
      break
    }
  }
  if(b <= nrow(BASE)){return(ID[b - 1])}
}

##Importa Audios
arquivos <- list.files(path = "C:/Users/edube/Documents/Archangel/Brasil")
qtde_arquivos <- length(arquivos)
for(a in 1:qtde_arquivos){
  #importa audio
  som <- readWave(paste("C:/Users/edube/Documents/Archangel/Brasil/",arquivos[a],sep = ""))
  #resampling
  som <- resamp(som, som@samp.rate, 8000, channel = 1, output = "Wave")
  audio <- som@left
  #janelamento
  mod <- length(audio)%%8000
  mod1 <- ifelse(mod%%2 == 0, mod/2, (mod/2) + 0.5)
  mod2 <- length(audio) - (mod - mod1) - 1
  audio <- audio[mod1:mod2]
  rm(mod, mod1, mod2)
  #frame
  frame_a <- as.data.frame(matrix(audio, ncol = 8000, byrow = TRUE))
  frame_a$ID <- arquivos[a]
  frame_a$ID_voz <- seq(nrow(frame_a))
  frame <- rbind(frame,frame_a)
}
rm(frame_a,a,arquivos,audio,som)
frame <- frame[order(frame$ID_voz, frame$ID),] #reordenar para agilizar o processo de verificação
frame$ID_voz <- NULL
#reduzindo a base, 3 cópias de cada voz
frame <- frame[c(1:42),]

##Alimenta BD
# 54 segundos para subir 5030 pontos do frame
# Vm = 92.85 uploads por segundo. Não preciso me preocupar muito com isso porque isso será pontual e salvo.
MFCC_audios <- archangel8_1(frame, numcep = 8)

#Testes
resultado <- matrix(nrow = 1000, ncol = 4)
for(t in 1:1000){

resultado[t,1] <- t
##Seleciona alguns exemplos. Não interessa saber quanto tempo deu essa parte
exemplos <- sample(1:nrow(frame),1)
frame_test <- frame[exemplos,]
resultado[t,2] <- frame_test$ID
MFCC_train <- MFCC_audios[-exemplos]
ID <- MFCC_train$ID
MFCC_train$ID <- NULL
BASE <- as.matrix(MFCC_train)
#MFCC do exemplo: Vm = 0.09 segundos
TESTE <- archangel8_1(frame_test, numcep = 8)
if(nrow(TESTE) > 0){
#Busca resultado
tic()
resultado[t,3] <- verifica_resultado(TESTE,BASE,ID)
toc()

#Compara acerto. Não precisa ver quanto deu levou
if(frame_test$ID == resultado[t,3]){
  resultado[t,4] <- "Correto"
} else{
  resultado[t,4] <- "Errado"
}
}
else{
  resultado[t,4] <- "sem Voz"  
}
}

resultado <- as.data.frame(resultado)
resumo_resultados <- sqldf("SELECT V4, count(*) FROM resultado GROUP BY V4")
resumo_resultados

#Resultado
# A quantidade de cepstruns afeta diretamente a velocidade do processamento.
# Diminuindo para 5 ceptrums, a eficiência foi de 96,1% e velocidade média de 10 segundos
# Em 1000 testes, com 8 cepstrums e 3 cópias, 90,4% de eficiência
