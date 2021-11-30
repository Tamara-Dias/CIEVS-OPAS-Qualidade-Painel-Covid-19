
#'*Script que calcula o número de casos ativos dos 7 dias anteriores ao cálculo da Matriz*

options(repos=structure(c(CRAN="https://vps.fmvz.usp.br/CRAN/"))) #colocando o mirror de São Paulo

if (!require("DT")) install.packages("DT", type = "binary")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, lubridate, tibbletime, dplyr, zoo, EpiEstim, formattable, 
               DT, Hmisc, knitr, openxlsx)

# BANCOS UTILIZADOS -------------------------------------------------------

# Fazendo download do painel 
setwd(getwd())

painel7atras <- read.xlsx('painel7.xlsx')
painel7 <- painel7atras %>% select("id", "RA", "dataPrimeiroSintomas", "classificacaoFinal", "dataObito")
rm(painel7atras) #removendo o painel com muitas variáveis

hoje <- Sys.Date() #data de hoje

# Calculando casos ativos de cada região do painel de 7 dias atrás
painel7dias <- painel7 %>% mutate(Regiao = case_when(RA == "Plano Piloto" ~ 'Região Central',
                                                     RA == "Sudoeste/Octogonal" ~ 'Região Central',
                                                     RA == "Cruzeiro" ~ 'Região Central',
                                                     RA == "Lago Norte" ~ 'Região Central',
                                                     RA == "Lago Sul" ~ 'Região Central',
                                                     RA == "Varjão" ~ 'Região Central',
                                                     RA == "Candangolândia" ~ 'Região Centro Sul',
                                                     RA == "Park Way" ~ 'Região Centro Sul',
                                                     RA == "Guará" ~ 'Região Centro Sul',
                                                     RA == "Núcleo Bandeirante" ~ 'Região Centro Sul',
                                                     RA == "Riacho Fundo" ~ 'Região Centro Sul',
                                                     RA == "Riacho Fundo II" ~ 'Região Centro Sul',
                                                     RA == "SCIA" ~ 'Região Centro Sul',
                                                     RA == "SIA" ~ 'Região Centro Sul',
                                                     RA == "Itapoã" ~ 'Região Leste',
                                                     RA == "Paranoá" ~ 'Região Leste',
                                                     RA == "São Sebastião" ~ 'Região Leste',
                                                     RA == "Jardim Botânico" ~ 'Região Leste',
                                                     RA == "Fercal" ~ 'Região Norte',
                                                     RA == "Planaltina" ~ 'Região Norte',
                                                     RA == "Sobradinho" ~ 'Região Norte',
                                                     RA == "Sobradinho II" ~ 'Região Norte',
                                                     RA == "Brazlândia" ~ 'Região Oeste',
                                                     RA == "Ceilândia" ~ 'Região Oeste',
                                                     RA == "Sol Nascente" ~ 'Região Oeste',
                                                     RA == "Águas Claras" ~ 'Região Sudoeste',
                                                     RA == "Recanto das Emas" ~ 'Região Sudoeste',
                                                     RA == "Samambaia" ~ 'Região Sudoeste',
                                                     RA == "Taguatinga" ~ 'Região Sudoeste',
                                                     RA == "Vicente Pires" ~ 'Região Sudoeste',
                                                     RA == "Arniqueira" ~ 'Região Sudoeste',
                                                     RA == "Gama" ~ 'Região Sul',
                                                     RA == "Santa Maria" ~ 'Região Sul',
                                                     T ~ 'Não informado'))
painel7dias <- data.table(painel7dias)

ativos_central7 <- sum(painel7dias$Regiao == 'Região Central' & painel7dias$classificacaoFinal == 'Ignorado', na.rm = TRUE)
ativos_centroSul7 <- sum(painel7dias$Regiao == 'Região Centro Sul' & painel7dias$classificacaoFinal == 'Ignorado', na.rm = TRUE)
ativos_leste7 <- sum(painel7dias$Regiao == 'Região Leste' & painel7dias$classificacaoFinal == 'Ignorado', na.rm = TRUE)
ativos_norte7 <- sum(painel7dias$Regiao == 'Região Norte' & painel7dias$classificacaoFinal == 'Ignorado', na.rm = TRUE)
ativos_oeste7 <- sum(painel7dias$Regiao == 'Região Oeste' & painel7dias$classificacaoFinal == 'Ignorado', na.rm = TRUE)
ativos_sudoeste7 <- sum(painel7dias$Regiao == 'Região Sudoeste' & painel7dias$classificacaoFinal == 'Ignorado', na.rm = TRUE)
ativos_sul7 <- sum(painel7dias$Regiao == 'Região Sul' & painel7dias$classificacaoFinal == 'Ignorado', na.rm = TRUE)
ativos_DF7 <- sum(painel7dias$classificacaoFinal == 'Ignorado', na.rm = TRUE)

casosAtivos7dias <- data.frame(ativos_central7, ativos_centroSul7, ativos_leste7, ativos_norte7, ativos_oeste7, ativos_sudoeste7, ativos_sul7, ativos_DF7)
write.csv2(casosAtivos7dias, file=paste0(format(hoje-7, '%d-%m-%y'),"casosAtivos7dias.csv"), row.names=FALSE)
