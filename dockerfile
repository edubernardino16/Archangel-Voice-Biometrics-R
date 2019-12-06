FROM rocker/r-base:latest

## Cria diretórios
RUN mkdir -p /01_data
RUN mkdir -p /02_code
RUN mkdir -p /03_output

#Importar arquivos do endereço https://github.com/edubernardino16/archangel

## Copia arquivos para os novos diretórios
#COPY /audios /01_data/Brasil/
COPY archangel/install_packages.R /02_code/install_packages.R
COPY archangel/M1_002.R /02_code/M1_002.R
COPY archangel/M1_002.R /02_code/M1_002.R

## Instala pacotes no R
RUN Rscript /02_code/install_packages.R

##Roda o código M1_002.R

CMD Rscript /02_code/M1_002.R

