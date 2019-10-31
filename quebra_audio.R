#QUebra audios - versão trabalho de Fast Data
#programa para quebrar um audio em vários audios de 3 segundos cada

#Limpa tudo
rm(list = ls(all.names = TRUE))

#Libraries
install.packages("tuneR")
install.packages("seewave")
install.packages("signal")
install.packages("tictoc")
library(tuneR)
library(seewave)
library(signal)


arquivos <- list.files(path = "C:/Users/logonrmlocal/Downloads/Brasil")

for(m in (1:length(arquivos))){

som <- readWave(paste("C:/Users/logonrmlocal/Downloads/Brasil/",arquivos[m], sep = ""))
samplingrate <- som@samp.rate #Quantidade de pontos por segundo
samples <- length(som@left)
duration <- length(som@left)/samplingrate
step <- 1/(samplingrate)
samples_steps <- seq(0 + step,duration,step)
janela <- seq(0,duration,1) #Quantidade de segundos que vou analisar

for(n in 1:(length(janela) - 1)){
  j1 <- janela[n]*samplingrate
  j2 <- janela[n + 1]*samplingrate
  j <- som@left[j1:j2]
  som1 <- Wave(left = j, right = numeric(0), samp.rate = samplingrate, bit = 16, pcm = TRUE)
  writeWave(som1,filename = paste("C:/Users/logonrmlocal/Downloads/quebra/",n,m,".wav"))
}
}