
# Casos no painel e Ûbitos no SIVEP Gripe 

# PACOTES -----------------------------------------------------------------

rm(list=ls())
gc()

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, tibbletime, dplyr, data.table, epitools, sf, knitr, xlsx, openxlsx)

# BANCOS UTILIZADOS -------------------------------------------------------

setwd(getwd())

hoje <- Sys.Date()
hoje <- format(hoje, "%d-%m-%Y")

# Sivep
#dados <- openxlsx::read.xlsx("SIVEP 2020.xlsx")
dados <- openxlsx::read.xlsx("SIVEP 2021.xlsx")

# Painel
painel <- openxlsx::read.xlsx('painel.xlsx')
painel$ufObito[painel$ufObito==""] <- NA
painel <- painel %>% mutate(`Morte/Outras.Causas` = ifelse(`Morte/Outras.Causas` == " " | `Morte/Outras.Causas` == "" | `Morte/Outras.Causas` == "  ", NA, `Morte/Outras.Causas`))

# Planilhas
p1 <- openxlsx::read.xlsx("”BITOS_FINALIZADOS_COMIT .xlsx", sheet = 1)
p2 <- openxlsx::read.xlsx("”BITOS_FINALIZADOS_COMIT .xlsx", sheet = 2)
p3 <- openxlsx::read.xlsx("”BITOS_FINALIZADOS_COMIT .xlsx", sheet = 3)
p4 <- openxlsx::read.xlsx("Planilha captaÁ„o dos Ûbitos.xlsx", sheet = 1)
p5 <- openxlsx::read.xlsx("Planilha captaÁ„o dos Ûbitos.xlsx", sheet = 2)

# An·lises ----------------------------------------------------------------

# ClassificaÁ„o final = cura e ignorado
painelcasos <- painel %>% filter(classificacaoFinal != "”bito" | is.na(classificacaoFinal)) %>% filter(is.na(`Morte/Outras.Causas`)) %>% filter(is.na(ReinfecÁ„o)|ReinfecÁ„o != "Prov·vel") %>% filter(is.na(ufObito))
names(painelcasos)[3] <- c("NM_PACIENT")

# Formatando os nomes
painelcasos <- painelcasos %>%
  mutate(NM_PACIENT = toupper(NM_PACIENT),
         NM_PACIENT = str_replace_all(NM_PACIENT, "[[:punct:]]", ""),
         NM_PACIENT =chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", NM_PACIENT),
                            NM_PACIENT = str_squish(NM_PACIENT))
         

dados2 <- dados %>%
  mutate(NM_PACIENT = toupper(NM_PACIENT),
         NM_PACIENT = str_replace_all(NM_PACIENT, "[[:punct:]]", ""),
         NM_PACIENT =chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", NM_PACIENT),
         NM_PACIENT = str_squish(NM_PACIENT))


# Sivep filtros para Ûbitos
dados2 <- dados %>% filter(CO_MUN_NOT == 530010 & CLASSI_FIN == 5 & CRITERIO == 1 & EVOLUCAO == 2)

# Fazendo match
dados2 <- dados2 %>% select(NU_NOTIFIC, NM_PACIENT, DT_NASC, NM_MAE_PAC, ID_UNIDADE, DT_ENCERRA, CLASSI_FIN)
painelcasos <- painelcasos %>% select(NM_PACIENT, dataNascimentoFull, mae, classificacaoFinal)

casos_obitos <- merge(dados2, painelcasos, by = "NM_PACIENT")

# Datas de nascimento iguais e diferentes
casos_obitos <- casos_obitos %>% mutate(data_nasc_iguais = ifelse(DT_NASC == dataNascimentoFull, "sim", "n„o"))

# Separando datas de nascimento iguais e diferentes
casos_obt_iguais <- casos_obitos %>% filter(data_nasc_iguais == "sim")
casos_obt_dif <- casos_obitos %>% filter(data_nasc_iguais == "n„o")


# Match com planilhas -----------------------------------------------------

names(p1)[11] <- c("NM_PACIENT")
names(p2)[11] <- c("NM_PACIENT")
names(p3)[11] <- c("NM_PACIENT")
names(p4)[11] <- c("NM_PACIENT")
names(p5)[11] <- c("NM_PACIENT")

# Ajustando os nomes
p1<- p1 %>%
  mutate(NM_PACIENT = toupper(NM_PACIENT),
         NM_PACIENT = str_replace_all(NM_PACIENT, "[[:punct:]]", ""),
         NM_PACIENT =chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", NM_PACIENT))
p2 <- p2 %>%
  mutate(NM_PACIENT = toupper(NM_PACIENT),
         NM_PACIENT = str_replace_all(NM_PACIENT, "[[:punct:]]", ""),
         NM_PACIENT =chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", NM_PACIENT))
p3 <- p3 %>%
  mutate(NM_PACIENT = toupper(NM_PACIENT),
         NM_PACIENT = str_replace_all(NM_PACIENT, "[[:punct:]]", ""),
         NM_PACIENT =chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", NM_PACIENT))
p4 <- p4 %>%
  mutate(NM_PACIENT = toupper(NM_PACIENT),
         NM_PACIENT = str_replace_all(NM_PACIENT, "[[:punct:]]", ""),
         NM_PACIENT =chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", NM_PACIENT))
p5 <- p5 %>%
  mutate(NM_PACIENT = toupper(NM_PACIENT),
         NM_PACIENT = str_replace_all(NM_PACIENT, "[[:punct:]]", ""),
         NM_PACIENT =chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", NM_PACIENT))

# Ajustando as datas dos Ûbitos e de nascimento para o formato correto
p1$DATA.DO.”BITO <- as.numeric(p1$DATA.DO.”BITO)
p1$DATA.DO.”BITO <-  as.Date(p1$DATA.DO.”BITO, origin = "1899-12-30")
p1$DATA.DO.”BITO <- format(p1$DATA.DO.”BITO, "%d/%m/%Y")
p1$DATA.DE.NASCIMENTO <- as.numeric(p1$DATA.DE.NASCIMENTO)
p1$DATA.DE.NASCIMENTO <-  as.Date(p1$DATA.DE.NASCIMENTO, origin = "1899-12-30")
p1$DATA.DE.NASCIMENTO <- format(p1$DATA.DE.NASCIMENTO, "%d/%m/%Y")

p2$DATA.DO.”BITO <- as.numeric(p2$DATA.DO.”BITO)
p2$DATA.DO.”BITO <-  as.Date(p2$DATA.DO.”BITO, origin = "1899-12-30")
p2$DATA.DO.”BITO <- format(p2$DATA.DO.”BITO, "%d/%m/%Y")
p2$DATA.DE.NASCIMENTO <- as.numeric(p2$DATA.DE.NASCIMENTO)
p2$DATA.DE.NASCIMENTO <-  as.Date(p2$DATA.DE.NASCIMENTO, origin = "1899-12-30")
p2$DATA.DE.NASCIMENTO <- format(p2$DATA.DE.NASCIMENTO, "%d/%m/%Y")

p3$DATA.DO.”BITO <- as.numeric(p3$DATA.DO.”BITO)
p3$DATA.DO.”BITO <-  as.Date(p3$DATA.DO.”BITO, origin = "1899-12-30")
p3$DATA.DO.”BITO <- format(p3$DATA.DO.”BITO, "%d/%m/%Y")
p3$DATA.DE.NASCIMENTO <- as.numeric(p3$DATA.DE.NASCIMENTO)
p3$DATA.DE.NASCIMENTO <-  as.Date(p3$DATA.DE.NASCIMENTO, origin = "1899-12-30")
p3$DATA.DE.NASCIMENTO <- format(p3$DATA.DE.NASCIMENTO, "%d/%m/%Y")

p4$DATA.DO.”BITO <- as.numeric(p4$DATA.DO.”BITO)
p4$DATA.DO.”BITO <-  as.Date(p4$DATA.DO.”BITO, origin = "1899-12-30")
p4$DATA.DO.”BITO <- format(p4$DATA.DO.”BITO, "%d/%m/%Y")
p4$DATA.DE.NASCIMENTO <- as.numeric(p4$DATA.DE.NASCIMENTO)
p4$DATA.DE.NASCIMENTO <-  as.Date(p4$DATA.DE.NASCIMENTO, origin = "1899-12-30")
p4$DATA.DE.NASCIMENTO <- format(p4$DATA.DE.NASCIMENTO, "%d/%m/%Y")

p5$DATA.DO.”BITO <- as.numeric(p5$DATA.DO.”BITO)
p5$DATA.DO.”BITO <-  as.Date(p5$DATA.DO.”BITO, origin = "1899-12-30")
p5$DATA.DO.”BITO <- format(p5$DATA.DO.”BITO, "%d/%m/%Y")
p5$DATA.DE.NASCIMENTO <- as.numeric(p5$DATA.DE.NASCIMENTO)
p5$DATA.DE.NASCIMENTO <-  as.Date(p5$DATA.DE.NASCIMENTO, origin = "1899-12-30")
p5$DATA.DE.NASCIMENTO <- format(p5$DATA.DE.NASCIMENTO, "%d/%m/%Y")


# Fazendo merge com cada planilha utilizada e criando coluna de DATA.DE.NASCIMENTO iguais
p1_final_painel <- merge(p1, casos_obitos, by = "NM_PACIENT")
p1_final_painel <- p1_final_painel %>% select(NM_PACIENT, RECORD.ID.PAINEL, COMUNIC.PAINEL, COMUNIC.AT, DATA.DO.”BITO, NOTIFICA«√O.SIVEP, RESULTADO, DATA.DE.NASCIMENTO, NOME.DA.M√E, ESTABELECIMENTO.DE.SA⁄DE, NU_NOTIFIC, NM_PACIENT, DT_NASC, NM_MAE_PAC, ID_UNIDADE, DT_ENCERRA, dataNascimentoFull, mae)
p1_final_painel <- p1_final_painel %>% mutate(data_nasc_iguais = ifelse(DT_NASC == dataNascimentoFull & DT_NASC == DATA.DE.NASCIMENTO & dataNascimentoFull == DATA.DE.NASCIMENTO, "sim", "n„o"))

p2_comite <- merge(p2, casos_obitos, by = "NM_PACIENT")
p2_comite <- p2_comite %>% select(NM_PACIENT, RECORD.ID, COMUNIC.PAINEL, COMUNIC.AT, DATA.DO.”BITO, NOTIFICA«√O.SIVEP, `RESULTADO.DO.COVID-19`, DATA.DE.NASCIMENTO, NOME.DA.M√E, ESTABELECIMENTO.DE.SA⁄DE, NU_NOTIFIC, NM_PACIENT, DT_NASC, NM_MAE_PAC, ID_UNIDADE, DT_ENCERRA, dataNascimentoFull, mae)
p2_comite <- p2_comite %>% mutate(data_nasc_iguais = ifelse(DT_NASC == dataNascimentoFull & DT_NASC == DATA.DE.NASCIMENTO & dataNascimentoFull == DATA.DE.NASCIMENTO, "sim", "n„o"))

p3_outras_causas <- merge(p3, casos_obitos, by = "NM_PACIENT")
p3_outras_causas <- p3_outras_causas %>% select(NM_PACIENT, COMUNIC.PAINEL, COMUNIC.AT, DATA.DO.”BITO, NOTIFICA«√O.SIVEP, `RESULTADO.DO.COVID-19`, DATA.DE.NASCIMENTO, NOME.DA.M√E, ESTABELECIMENTO.DE.SA⁄DE, NU_NOTIFIC, NM_PACIENT, DT_NASC, NM_MAE_PAC, ID_UNIDADE, DT_ENCERRA, dataNascimentoFull, mae)
p3_outras_causas <- p3_outras_causas %>% mutate(data_nasc_iguais = ifelse(DT_NASC == dataNascimentoFull & DT_NASC == DATA.DE.NASCIMENTO & dataNascimentoFull == DATA.DE.NASCIMENTO, "sim", "n„o"))

p4_captacao <- merge(p4, casos_obitos, by = "NM_PACIENT")
p4_captacao <- p4_captacao %>% select(NM_PACIENT, COMUNIC.PAINEL, COMUNIC.AT, DATA.DO.”BITO, NOTIFICA«√O.SIVEP, `RESULTADO.DO.COVID-19`, DATA.DE.NASCIMENTO, NOME.DA.M√E, ESTABELECIMENTO.DE.SA⁄DE, NU_NOTIFIC, NM_PACIENT, DT_NASC, NM_MAE_PAC, ID_UNIDADE, DT_ENCERRA, dataNascimentoFull, mae)
p4_captacao <- p4_captacao %>% mutate(data_nasc_iguais = ifelse(DT_NASC == dataNascimentoFull & DT_NASC == DATA.DE.NASCIMENTO & dataNascimentoFull == DATA.DE.NASCIMENTO, "sim", "n„o"))

p5_pend <- merge(p5, casos_obitos, by = "NM_PACIENT")
p5_pend <- p5_pend %>% select(NM_PACIENT, RECORD.ID, COMUNIC.PAINEL, COMUNIC.AT, DATA.DO.”BITO, NOTIFICA«√O.SIVEP, `RESULTADO.DO.COVID-19`, DATA.DE.NASCIMENTO, NOME.DA.M√E, ESTABELECIMENTO.DE.SA⁄DE, NU_NOTIFIC, NM_PACIENT, DT_NASC, NM_MAE_PAC, ID_UNIDADE, DT_ENCERRA, dataNascimentoFull, mae)
p5_pend <- p5_pend %>% mutate(data_nasc_iguais = ifelse(DT_NASC == dataNascimentoFull & DT_NASC == DATA.DE.NASCIMENTO & dataNascimentoFull == DATA.DE.NASCIMENTO, "sim", "n„o"))
  

# Pegando os Ûbitos n„o registrados no painel -----------------------------
t1 <- subset(casos_obitos, !(NM_PACIENT %in% p1_final_painel$NM_PACIENT))
t2 <- subset(t1, !(NM_PACIENT %in% p2_comite$NM_PACIENT))
t3 <- subset(t2, !(NM_PACIENT %in% p3_outras_causas$NM_PACIENT))
t4 <- subset(t3, !(NM_PACIENT %in% p4_captacao$NM_PACIENT))
t5 <- subset(t4, !(NM_PACIENT %in% p5_pend$NM_PACIENT))

# Salvando os arquivos
write.xlsx2(as.data.frame(t5), paste0("SIVEP_2020_", hoje, ".xlsx"), sheetName = "”BÕTOS FINAL", append=TRUE, row.names = F)
write.xlsx2(as.data.frame(casos_obt_iguais), paste0("SIVEP_2020_", hoje, ".xlsx"), sheetName = "”bitos iguais", append=TRUE, row.names = F)
write.xlsx2(as.data.frame(casos_obt_dif), paste0("SIVEP_2020_", hoje, ".xlsx"), sheetName = "”bitos diferentes", append=TRUE, row.names = F)
write.xlsx2(as.data.frame(p1_final_painel), paste0("SIVEP_2020_", hoje, ".xlsx"), sheetName = "Final painel", append=TRUE, row.names = F)
write.xlsx2(as.data.frame(p2_comite), paste0("SIVEP_2020_", hoje, ".xlsx"), sheetName = "ComitÍ", append=TRUE, row.names = F)
write.xlsx2(as.data.frame(p3_outras_causas), paste0("SIVEP_2020_", hoje, ".xlsx"), sheetName = "Outras causas", append=TRUE, row.names = F)
write.xlsx2(as.data.frame(p4_captacao), paste0("SIVEP_2020_", hoje, ".xlsx"), sheetName = "CaptaÁ„o", append=TRUE, row.names = F)
write.xlsx2(as.data.frame(p5_pend), paste0("SIVEP_2020_", hoje, ".xlsx"), sheetName = "PendÍncias", append=TRUE, row.names = F)


  
  