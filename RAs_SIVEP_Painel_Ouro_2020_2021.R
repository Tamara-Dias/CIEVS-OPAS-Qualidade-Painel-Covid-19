
# Busca de RAs em discord‚ncia no Painel e padr„o ouro do SIVEP 

# PACOTES -----------------------------------------------------------------

rm(list=ls())
gc()

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, tibbletime, dplyr, data.table, epitools, sf, knitr, xlsx, openxlsx)

# BANCOS UTILIZADOS -------------------------------------------------------

setwd(getwd())

hoje <- Sys.Date()
hoje <- format(hoje, "%d-%m-%Y")


# 2021 --------------------------------------------------------------------


# Sivep
dados21 <- openxlsx::read.xlsx("SIVEP 2021.xlsx")
dados21 <- dados21 %>% filter(CO_MUN_NOT == 530010)
dados21 <- dados21 %>% mutate(NU_CPF = gsub(pattern = "[,]|-|[.]|;| |^0+", replacement = "", x = NU_CPF, perl = T))

# Painel
painel <- openxlsx::read.xlsx('painel.xlsx')
painel <- painel %>% mutate(cpf = gsub(pattern = "[,]|-|[.]|;| |^0+", replacement = "", x = cpf, perl = T))
painel <- painel %>% mutate(cpf  = ifelse(cpf  == " " | cpf == "" | cpf  == "  ", NA, cpf ))


# ANO DE 2021 -------------------------------------------------------------

# Ajustando as RAs dos bancos ---------------------------------------------
dados21$RAs_SIVEP <- (dados21$ID_MN_RESI)
painel$RAs_PAINEL <- (painel$RA)

painel <- painel %>%
  mutate(nomecompletoPaciente = toupper(nomecompletoPaciente),
         nomecompletoPaciente = str_replace_all(nomecompletoPaciente, "[[:punct:]]", ""),
         nomecompletoPaciente = chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", nomecompletoPaciente),
         nomecompletoPaciente = str_squish(nomecompletoPaciente))

dados21 <- dados21 %>%
  mutate(NM_PACIENT = toupper(NM_PACIENT),
         NM_PACIENT = str_replace_all(NM_PACIENT, "[[:punct:]]", ""),
         NM_PACIENT = chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", NM_PACIENT),
         NM_PACIENT = str_squish(NM_PACIENT))

painel <- painel %>%
  mutate(RAs_PAINEL = toupper(RAs_PAINEL),
         RAs_PAINEL = str_replace_all(RAs_PAINEL, "[[:punct:]]", ""),
         RAs_PAINEL =chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", RAs_PAINEL),
         RAs_PAINEL = str_squish(RAs_PAINEL))

dados21 <- dados21 %>%
  mutate(RAs_SIVEP = toupper(RAs_SIVEP),
         RAs_SIVEP = str_replace_all(RAs_SIVEP, "[[:punct:]]", ""),
         RAs_SIVEP =chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", RAs_SIVEP),
         RAs_SIVEP = str_squish(RAs_SIVEP))

# Selecionando vari·veis
ras_sivep <- dados21 %>% select(NM_PACIENT, DT_NASC, NU_NOTIFIC, ID_MN_RESI, RAs_SIVEP, NU_CPF, NM_MAE_PAC, NM_LOGRADO, NU_CEP)
ras_sivep <- ras_sivep %>% mutate(RAs_SIVEP = str_replace_all(RAs_SIVEP, "BRASILIA*", "PLANO PILOTO")) %>% mutate(RAs_SIVEP = str_replace_all(RAs_SIVEP, "ASA SUL|ASA NORTE", ""), RAs_SIVEP = str_squish(RAs_SIVEP))

painel2 <- painel %>% select(Record_Id, nomecompletoPaciente, dataNascimentoFull, mae, sexo, cpf, enderecoCompleto, RA, RAs_PAINEL, cep)
painel2 <- painel2 %>% mutate(chave = paste0(nomecompletoPaciente, " ", dataNascimentoFull))
ras_sivep <- ras_sivep %>% mutate(chave = paste0(NM_PACIENT, " ", DT_NASC))

# Fazendo match do Painel com o Sivep
painel_sivep <- merge(painel2, ras_sivep, by = "chave")

# Mantendo apenas os diferentes
painel_sivep <- painel_sivep %>% mutate(RAs_iguais = ifelse(RAs_PAINEL == RAs_SIVEP, "sim", "n„o"))
painel_sivep <- painel_sivep %>% filter(RAs_iguais == "n„o")

# Banco final
final <- painel_sivep %>% select(P.Record_Id = Record_Id, P.nomecompletoPaciente = nomecompletoPaciente, P.dataNascimentoFull = dataNascimentoFull, S.DT_NASC = DT_NASC, P.mae = mae, S.NM_MAE_PAC = NM_MAE_PAC, P.cpf = cpf, S.NU_CPF = NU_CPF, P.enderecoCompleto = enderecoCompleto, S.NM_LOGRADO = NM_LOGRADO, P.RA = RA,  S.ID_MN_RESI = ID_MN_RESI, P.cep = cep, S.NU_CEP = NU_CEP, S.NU_NOTIFIC = NU_NOTIFIC)
final$P.cpf <- as.numeric(final$P.cpf)
final$S.NU_CPF<- as.numeric(final$S.NU_CPF)

final <- final %>% mutate(cpfs_iguais = ifelse(P.cpf == S.NU_CPF, "sim", "n„o"))

# Salvando
write.xlsx2(as.data.frame(final), paste0("RAs_Ouro__Painel_SIVEP_2020_2021_", hoje, ".xlsx"), sheetName = "2021", append=TRUE, row.names = F)





# 2020 --------------------------------------------------------------------

# Sivep
dados20 <- openxlsx::read.xlsx("SIVEP 2020.xlsx")
dados20 <- dados20 %>% filter(CO_MUN_NOT == 530010)
dados20 <- dados20 %>% mutate(NU_CPF = gsub(pattern = "[,]|-|[.]|;| |^0+", replacement = "", x = NU_CPF, perl = T))

# ANO DE 2021 -------------------------------------------------------------

# Ajustando as RAs dos bancos ---------------------------------------------
dados20$RAs_SIVEP <- (dados20$ID_MN_RESI)

dados20 <- dados20 %>%
  mutate(NM_PACIENT = toupper(NM_PACIENT),
         NM_PACIENT = str_replace_all(NM_PACIENT, "[[:punct:]]", ""),
         NM_PACIENT = chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", NM_PACIENT),
         NM_PACIENT = str_squish(NM_PACIENT))

dados20 <- dados20 %>%
  mutate(RAs_SIVEP = toupper(RAs_SIVEP),
         RAs_SIVEP = str_replace_all(RAs_SIVEP, "[[:punct:]]", ""),
         RAs_SIVEP =chartr("¡¬…»¿‘ŒÕ√’”⁄«", "AAEEAOIIAOOUC", RAs_SIVEP),
         RAs_SIVEP = str_squish(RAs_SIVEP))

# Selecionando vari·veis
ras_sivep2 <- dados20 %>% select(NM_PACIENT, DT_NASC, NU_NOTIFIC, ID_MN_RESI, RAs_SIVEP, NU_CPF, NM_MAE_PAC, NM_LOGRADO, NU_CEP)
ras_sivep2 <- ras_sivep2 %>% mutate(RAs_SIVEP = str_replace_all(RAs_SIVEP, "BRASILIA*", "PLANO PILOTO")) %>% mutate(RAs_SIVEP = str_replace_all(RAs_SIVEP, "ASA SUL|ASA NORTE", ""), RAs_SIVEP = str_squish(RAs_SIVEP))

# Selecionando vari·veis de interesse
painel0 <- painel %>% select(Record_Id, nomecompletoPaciente, dataNascimentoFull, mae, sexo, cpf, enderecoCompleto, RA, RAs_PAINEL, cep)
painel0 <- painel0 %>% mutate(chave = paste0(nomecompletoPaciente, " ", dataNascimentoFull))
ras_sivep2 <- ras_sivep2 %>% mutate(chave = paste0(NM_PACIENT, " ", DT_NASC))

# Fazendo match do Painel com o Sivep
painel_sivep2 <- merge(painel0, ras_sivep2, by = "chave")

# Mantendo apenas os diferentes
painel_sivep2 <- painel_sivep2 %>% mutate(RAs_iguais = ifelse(RAs_PAINEL == RAs_SIVEP, "sim", "n„o"))
painel_sivep2 <- painel_sivep2 %>% filter(RAs_iguais == "n„o")

# Banco final
final2 <- painel_sivep2 %>% select(P.Record_Id = Record_Id, P.nomecompletoPaciente = nomecompletoPaciente, P.dataNascimentoFull = dataNascimentoFull, S.DT_NASC = DT_NASC, P.mae = mae, S.NM_MAE_PAC = NM_MAE_PAC, P.cpf = cpf, S.NU_CPF = NU_CPF, P.enderecoCompleto = enderecoCompleto, S.NM_LOGRADO = NM_LOGRADO, P.RA = RA,  S.ID_MN_RESI = ID_MN_RESI, P.cep = cep, S.NU_CEP = NU_CEP, S.NU_NOTIFIC = NU_NOTIFIC)
final2$P.cpf <- as.numeric(final2$P.cpf)
final2$S.NU_CPF<- as.numeric(final2$S.NU_CPF)

final2 <- final2 %>% mutate(cpfs_iguais = ifelse(P.cpf == S.NU_CPF, "sim", "n„o"))

# Salvando
write.xlsx2(as.data.frame(final2), paste0("RAs_Ouro__Painel_SIVEP_2020_2021_", hoje, ".xlsx"), sheetName = "2020", append=TRUE, row.names = F)
