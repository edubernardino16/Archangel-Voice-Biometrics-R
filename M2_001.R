#Máquina 2
#Versão 0.0.1 - teste


#0 - Packages e Libraries

rm(list = ls(all.names = TRUE))
#install.packages("neuralnet")
library(neuralnet)

#1 - Carrega arquivos do M1

arquivos <- list.files(path = "C:/Users/Eduardo/Documents/Archangel/teste_M1_001")

#2 - Lê frame e cria um único

n_arquivos <- length(arquivos)
train_data <- NULL
for(n in 1:n_arquivos){
frame <- as.data.frame(read.csv(file = paste("C:/Users/Eduardo/Documents/Archangel/teste_M1_001/",arquivos[n],sep = ""),sep = ";"))  
frame$ID <- n
train_data <- rbind(train_data,frame)
rm(frame)
}

#3 - Escreve o data_frame
write.table(train_data, file = "C:/Users/Eduardo/Documents/Archangel/teste_M1_001/data_frame.csv",sep = ";")

#4 - Monta modelo de analytics

variaveis <- paste0(names(train_data[-221]), collapse = " + ")

modelo <-neuralnet(ID~V1 + V2, data=train_data, hidden=2)

plot(net)