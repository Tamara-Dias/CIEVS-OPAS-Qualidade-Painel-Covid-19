
# Automatização RAs de vacinados do DF ------------------------------------

rm(list=ls())
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, data.table, janitor, RecordLinkage)


# Lendo arquivos ----------------------------------------------------------
setwd(getwd())

dados_bruto <- fread("DF_2021.11.22.csv", encoding="UTF-8")

# Lendo arquivos dos ceps
arquivos <- list.files(pattern = "df.cep", full.names = T)
lista_arquivos <- lapply(arquivos,fread, encoding = "UTF-8")

ceps <- bind_rows(lista_arquivos)
colnames(ceps) <- c("cep","endereco","complemento","RA","n1","n2")


# Filtrando para DF
dados_df2 <- dados_bruto %>% select(c("paciente_endereco_bairro", "paciente_endereco_coibgemunicipio", "paciente_endereco_cep"))
dados_df2 <- dados_df2 %>% 
  filter(paciente_endereco_coibgemunicipio==530010)

dados_df <- dados_bruto %>% select(c("paciente_endereco_bairro", "paciente_endereco_coibgemunicipio", "paciente_endereco_cep"))
dados_df <- dados_df %>% 
  filter(paciente_endereco_coibgemunicipio==530010)

rm(dados_bruto)


# Tratamento inicial do banco de dados ------------------------------------
dados_df <- dados_df %>%
  mutate(paciente_endereco_bairro = toupper(paciente_endereco_bairro),
         paciente_endereco_bairro = str_replace_all(paciente_endereco_bairro , "[[:punct:]]", ""),
         paciente_endereco_bairro = str_replace_all(paciente_endereco_bairro , "[[0,3,4,5,6,7,8,9]]", ""),
         paciente_endereco_bairro = str_replace_all(paciente_endereco_bairro , "^\\s{0,}[AB]\\s{0,}2*\\s{0,}$|^\\s{0,}AB\\s{0,}2*\\s{0,}$", ""),
         paciente_endereco_bairro = str_replace_all(paciente_endereco_bairro , "^\\s{0,}AP\\s{0,}[21]*\\s{0,}$|^\\s{0,}APT\\s{0,}[21]*\\s{0,}$", ""),
         paciente_endereco_bairro = str_replace_all(paciente_endereco_bairro , "^\\s{0,}APTO\\s{0,}[0-9]*\\s{0,}$", ""),
         paciente_endereco_bairro = str_replace_all(paciente_endereco_bairro , "^\\s{0,}[0-9]*\\s{0,}$", ""),
         paciente_endereco_bairro =chartr("ÁÂÉÈÀÔÎÍÃÕÓÚÇ", "AAEEAOIIAOOUC", paciente_endereco_bairro))
         

# Mutates por RA ----------------------------------------------------------

# > SOL NASCENTE ----------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("SOL\\s{0,}NASCENTE|POR\\s{0,}DO|SOL.* N|CEI.*\\s{0,}SOL|POL\\s{0,}DO|PO\\s{0,}DO|POR\\s{0,}SO|SO\\s{0,}NAS|SIL\\s{0,}NAS|SIL\\s{0,}NAS",paciente_endereco_bairro) & !grepl("ISOLADAS|CONDOMINIO SO",paciente_endereco_bairro), "SOL NASCENTE/PÔR DO SOL",paciente_endereco_bairro))

sol <- dados_df %>% filter(bairro_correto == "SOL NASCENTE/PÔR DO SOL")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "SOL NASCENTE/PÔR DO SOL")


# > CEILÂNDIA ----------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("CEILANDIA|C\\s{0,}EI.* N|CE.*\\s{0,}N|ILANDIA SUL|QUARIR|C\\s{0,}EI.* S|C\\s{0,}NORTE|C\\s{0,}SUL|C MNORTE|CEULAND|SEILAN|CWILAN|CXEIL\\s{0}SUL|CEDILANDIA|CEIALANDIA|CEIALNDIA|CEIANDIA|CEIILANDIA|CELANDIA|CELIANDIA|CELILANDIA|CELILANIDA|CERANDIA|CEWILANDIA|CIEILANDIA|CIELANDIA|CILANDIA|CLEILANDIA|CPILANDIA|CREILANDIA|CRILANDIA|CWEILANDIA|C EILANDIA|CAILANDIA|EEILANDIA|EILANDIA|PEILANDIA|CAEILANDIA|CEIL|^\\s{0,}CEI\\s{0,}$|GUARI|CEINADIA|CEIALNDIA|P\\s{0,}SUL|P\\s{0,}NORTE|CL\\s{0,}NORTE|EIL\\s{0,}NORT|CRIL\\s{0,}NORT|QNL NORTE|ASA\\s{0,}NRO|P NROTE|^\\s{0,}SETOR\\s{0,}O\\s{0,}$|EXPANSAO SETOR O|CEI\\s{0,}L\\s{0,}SUL|^\\s{0,}CEI\\s{0,}L\\s{0,}SUL|^\\s{0,}CE\\s{0,}L\\s{0,}SUL|^\\s{0,}CEU\\s{0,}L\\s{0,}SUL|^\\s{0,}CEW.*\\s{0,}SUL|^\\s{0,}CFEIL SUL|^\\s{0,}CIL SUL\\s{0,}$|SELANDIA|^\\s{0,}QNO\\s{0,}$|^\\s{0,}SETOR P\\s{0,}$|SETOR O NORTE|EXP DO SETOR O|EXPANSAO ST O|^\\s{0,}QNR\\s{0,}$|EXPANSAO DO SETOR O|EX ST O|SETOR Q|^\\s{0,}S\\s{0,}T\\s{0,}O\\s{0,}$|QNQ|SETOR M NORTE|EXP SETOR O|AGUA FRESCA|AMARA|A.*\\s{0,}QUEN|EXP DO STO O|STOR O|STOR M NORTE|EXP STO O|SSTOR O|STO M SUL",paciente_endereco_bairro) & !grepl("LAGOP|LAGP|CENTRO SEDE|SCEE SUL|CENTRO SUL|ALIANC|BATE QUENTE|SETOR CENTRAL|CEUZEIR|BANDE|PIRES|VICENTE|NOBRE|CRUZEIR|METR|MSPN|PNEU|VERDE|CLUBE|TAGUARINGA|ARAGUARI|GUARIA|SEBAS|BANC|OFIC|TRADIC|TAC NORTE|TAGC|TRADI",paciente_endereco_bairro), "CEILÂNDIA",paciente_endereco_bairro))

cei <- dados_df %>% filter(bairro_correto == "CEILÂNDIA")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "CEILÂNDIA")


# > SOBRADINHO II ---------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("SOBRADINHO\\s{0,}II|SOBRADINHO\\s{0,}2|CONTAGEM|SOBRADINHOI II|SOBRADINHO\\s{0,}I\\s{0,}I|COLORAD|RABELO|CON DAS ACACIAS|VALE DAS ACACIAS|CONDOMINIO ACACIAS|CON DAS ACACIAS|^\\s{0,}SOB[D]*\\s{0,}2\\s{0,}$|VIL.*\\s{0,}RICA|SER.*\\s{0,}AZ|NOVO HORIZONTE|SETOR OESTE SOBRAD|VA.*\\s{0,}VE|SRN|NOBRES|RURAL DE SOBRADINHO|^\\s{0,}SOB[D]*\\s{0,}II\\s{0,}$|BURITIZINHO|BASEVI|SAOBRADINHO II",paciente_endereco_bairro) & !grepl("PLANALTINA|PARANOA|ME*L[L]*O*|FERCAL\\s{0,}SOBRADINHO|PLANAL|ACAMP",paciente_endereco_bairro), "SOBRADINHO II",paciente_endereco_bairro))

sobradinho2 <- dados_df %>% filter(bairro_correto == "SOBRADINHO II")

dados_df <- dados_df %>% filter(bairro_correto != "SOBRADINHO II")


# > SOBRADINHO I ----------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("SOBRADINHO\\s{0,}I\\b|SOBRADINHO\\s{0,}1|NOVA COLINA|SETOR.*\\s{0,}MAN.*\\s{0,}SOB|NUCLEO RURAL LAGO OESTE SOBRADINHO|REGIAO DOS LAGOS|ALTO DA BOA VISTA|SETOR ECONOMICO DE SOBRADINHO|SETOR INDUSTRIAL SOBRADINHO|^\\s{0,}SOB\\s{0,}I\\s{0,}$|CONDOMINIO MANSOES SOBRADINHO SOBRADINHO|CONDOMINIO COMERCIAL E RESIDENCIAL SOBRADINHO SOBRADINHO|CONDOMINIO MIRANTE DA SERRA SOBRADINHO|VAL.*\\s{0,}PIN|SWTOR DE MANSOES DE SOBRADINHO",paciente_endereco_bairro) & !grepl("PLANALTINA|PARANOA|ME*L[L]*O*|FERCAL\\s{0,}SOBRADINHO",paciente_endereco_bairro), "SOBRADINHO I",paciente_endereco_bairro))

sobradinho1 <- dados_df %>% filter(bairro_correto == "SOBRADINHO I")

dados_df <- dados_df %>% filter(bairro_correto != "SOBRADINHO I")


# > RECANTO DAS EMAS ------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("RECANTO\\s{0,}DA[S]*\\s{0,}EMA[S]*|EMAS|^\\s{0,}RECANTO\\s{0,}DAS\\s{0,}$|^\\s{0,}RECANTO\\s{0,}$|RECANTO\\s{0,}DAS\\s{0,}E|RES.*\\s{0,}BU|^\\s{0,}REC[A]*\\s{0,}$|BETA|DF RECANTO|RES SAO FRANCISCO",paciente_endereco_bairro) & !grepl("RIACHO|URUBU",paciente_endereco_bairro), "RECANTO DAS EMAS",paciente_endereco_bairro))

recanto <- dados_df %>% filter(bairro_correto == "RECANTO DAS EMAS") #Mantendo só Recanto
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "RECANTO DAS EMAS") #Excluindo Recanto


# > TAGUATINGA ------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("TAGUATINGA|TAGU.*|^\\s{0,}TAGUA\\s{0,}$|^\\s{0,}TAG\\s{0,}$|TINGA|TAGUARINGA|CAGUATINGA|TAC NORTE|TAC SUL|TAG.*\\s{0,}S|TAG.*\\s{0,}N|TAG.*\\s{0,}CEN|^\\s{0,}TG\\s{0,}$|^TG.*\\s{0,}[NS]|M NROTE|^\\s{0,}M\\s{0,}NORTE\\s{0,}$|QNJ|^\\s{0,}T NORTE\\s{0,}$|TAQUATINGUA|TAQ N|TAAG NORTE|^\\s{0,}TAAG\\s{0,}$",paciente_endereco_bairro) & !grepl("CLARA|SAMAM|VICENTE|PIRE|OCT|ARNIQ|COL\\s{0,}AGR\\s{0,}SAM|MONTAGNE|GUARA",paciente_endereco_bairro), "TAGUATINGA",paciente_endereco_bairro))

taguatinga <- dados_df %>% filter(bairro_correto == "TAGUATINGA") #Mantendo só Taguatinga
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "TAGUATINGA") #Excluindo Taguatinga


# > SANTA MARIA ----------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("MARIA|STA\\s{0,}M|SANTA\\s{0,}M|ST\\s{0,}MAIRA|\\bSTA\\s{0,}M|SAANTA|D[R]*U[M]*ON|MONICA|PORTO RICO|SANTA MARIANORTE|AMRIA|RESIDENCIAL STOS DU|SANT AMRI A|SANT ARIA
",paciente_endereco_bairro) & !grepl("MARIANA\\b|ROCHA|PAULA|VILA MARIA|MARAN|MARIANA\\b",paciente_endereco_bairro), "SANTA MARIA",paciente_endereco_bairro))

santa <- dados_df %>% filter(bairro_correto == "SANTA MARIA")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "SANTA MARIA")


# > RIACHO FUNDO II -------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("FUNDO\\s{0,}II|FUNDO\\s{0,}2|RIACH[O]*\\s{0,}II|RIACH[O]*\\s{0,}2|FUNDO\\s{0,}I\\s{0,}I|FUNDOI\\s{0,}II|ACHO\\s{0,}FUNDO\\s{0,}II|^\\s{0,}RF\\s{0,}II\\s{0,}$|^\\s{0,}RF\\s{0,}2\\s{0,}$",paciente_endereco_bairro), "RIACHO FUNDO II",paciente_endereco_bairro))

riacho2 <- dados_df %>% filter(bairro_correto == "RIACHO FUNDO II")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "RIACHO FUNDO II")


# > RIACHO FUNDO I --------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("FUNDO\\s{0,}I\\b|FUNDO\\s{0,}1|RIACH[O]\\s{0,}I\\b|RIACH[O]*\\s{0,}1|SUCU|^\\s{0,}RF\\s{0,}[1I]\\s{0,}$",paciente_endereco_bairro), "RIACHO FUNDO I",paciente_endereco_bairro))

riacho1 <- dados_df %>% filter(bairro_correto == "RIACHO FUNDO I")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "RIACHO FUNDO I")


# > PARANOÁ ---------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("PAR[A]*N[OU]A|ALTI\\s{0,}PLANO|ALTI\\s{0,}PALNO|ALTIPLOLESTE|ALTE\\s{0,}PLANO|PARANIO|CAPAO\\s{0,}DA\\s{0,}ERVA|CAPAO\\s{0,}SECO|CARIRU|ITAPETI|AUTIPLANO|PARN|PRARN|SUS[S]*U|^\\s{0,}PARAN\\s{0,}$|PARAN SUL|PARANA PARQUE|PARANAO|PARANBOA|PARAN[DCV]A|PARANIA|PARANO|PARANOIA|RARANOA|CAFE SEM TROCO|CAF.*\\s{0,}TR|SOB.*\\s{0,}MEL|CAPAO\\s{0,}C[OU]M",paciente_endereco_bairro) & !grepl("ITAPO|ITAPU|D LAGO|DEL LAGO|SAO|PLANALTO|FAZE",paciente_endereco_bairro), "PARANOÁ",paciente_endereco_bairro))

paranoa <- dados_df %>% filter(bairro_correto == "PARANOÁ")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "PARANOÁ")


# > PLANO PILOTO ----------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("ASA\\s{0,}SUL|A\\s{0,}NORTE|TELEB|A\\s{0,}SUL|ASA\\s{0,}NORTE|AAS\\s{0,}NORTE|GRA[N]*JA|GRAN.*\\s{0,}TO|PLANO|PILOTO|AAS\\s{0,}SUL|PLANALTO|SQNW|SQS|SQN|CLN|CLS|ASAL\\s{0,}SUL|ASAL\\s{0,}NORTE|SETOR\\s{0,}POLICIAL|^ALA SUL$|SETOR\\s{0,}POLICIAL|ST\\s{0,}POLICIAL\\s{0,}SUL|VI.*\\s{0,}PL|VILA OLANALTO|VILA PALAN|VILA PALN|VILA PAN|VILA PLLA|DISTRITO FEDERAJ|PRACA DA BANDEIRA|CAMPUS UNIV|BRASIA|S.*\\s{0,}CLUB|DESTRITO FEDERAL|RODOVIARIA|ACAMP P FERNANDES|ACAMP PAC FERNANDES|PACHE|RABELO|MINERAL|NORO|^\\s{0,}SMU\\s{0,}$|WESL|ED METROPOLITAN|TRES PODERES|SETOR COMERCIAL SUL|^\\s{0,}ASA\\s{0,}$|SETOR GRAFICO|ZONA CIVICOADMINIS|ZONA INDUSTRIAL GU|PRACA DOS TRIBUNAIS SUPERIOR|^\\s{0,}S\\s{0,}N\\s{0,}$|ZONA.*\\s{0,}ADM|SETOR COMERCIAL NOR|HOTEL|ASA NOTE|S.*\\s{0,}MIL.*\\s{0,}U|HBDF|STO POLICIAL",paciente_endereco_bairro) & !grepl("PONTE|BRAZLANDIA|GAMA|INGA NORTE|LAGA|LAGOA|LOGA|OFICINA|PARANOA|PENINSULA|PENISULA|ALTA|POTALTA|TAGUATINGA|MARIA|SABAMBAIA|SAIDA|SAIMAMBAIA|SANTA|MAMB|SE TRA|SEMBAMBAIA|OFICINA|SMAMBAIA|SMBA|MAIRA|MARA|SWAMABAIA|TA NORTE|TING|TAGAUATIONGA|TAGU|TGUA|TA SUL|STA|BAIA|SIA|ANDIA|SANANBAUA|SAIDA SUL|PRIVE|PARAN|LA SUL|GUAR|GANA|MORADA|OCTO|CLARA|DIA NORTE|QUADRA NORTE|LANDIA|DIA SUL|LAGO|VARJAO|VRJAO|PARK|WAY|SUDOESTE|TAG SUL|GUARA|SOLAR|CANDANG|CRUZ|PLANALTINA|GARAGEM|ITAP[OU]A|BOTANICO|JARDIM|JD BRASILIA|LAGO|MANSOES PARQUE|ESTRUTURA|SOBRADINHO|ESTAN|PARQ\\s{0,}ESP|PQ ESP|SETOR RESIDENCIAL LESTE PLANA|ST RES LESTE PLANA|SEBAS|IVASAO|COLORA|GRANJA DO NORT|GRANJA PROG|GRANJA VIT|SANTA M",paciente_endereco_bairro), "PLANO PILOTO",paciente_endereco_bairro))

plano <- dados_df %>% filter(bairro_correto == "PLANO PILOTO")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "PLANO PILOTO")


# > ÁGUAS CLARAS ----------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("AGUAS\\s{0,}[CG]|AGUAS\\s{0,}AC|A\\s{0,}CLARAS|AG.* C|AGUA\\s{0,}C|AGUS\\s{0,}CL|AGUA[SA]*\\s{0,}[C]*LARA[S]|AREAL|PARNAS|A C LARAS|A CALARAS|A CALRAS|A CL|A GUAS CLARAS|AL.*\\s{0,}ACACIAS|AV.*\\s{0,}ACACIAS",paciente_endereco_bairro) & !grepl("A\\s{0,}CLARA.* TAG|AG.* C.* TAG|AGUAS CLARAS  TGUA|AGUAS CLARAS TG|TAG.* C|TAGUA C|ARNI|AGRIC|COL\\s{0,}A|COLONIA A|C\\s{0,}A AGUAS|LAGO|VALE DAS|TAG\\s{0,}AGUAS CLARAS|TAGUATINGA AGUAS CL|TAGUATINGAACLARAS|	
TAGUATINGAAGUAS", paciente_endereco_bairro), "ÁGUAS CLARAS",paciente_endereco_bairro))

aguas <- dados_df %>% filter(bairro_correto == "ÁGUAS CLARAS")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "ÁGUAS CLARAS")


# > LAGO SUL --------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("AGO\\s{0,}SU|AGOL\\s{0,}SU|L\\s{0,}SUL|LA.*\\s{0,}[AL]*SUL|LG.*\\s{0,}SUL|LL.*\\s{0,}SUL|LO.*\\s{0,}SUL|AEROPORTO|SETOR DE MANSOES DOM BOSCO|SHIS",paciente_endereco_bairro) & !grepl("JARDI|BOT|TORORO|TAQUARI|OCT|COMERCIAL|SETOR\\s{0,}CENTRAL|SETOR\\s{0,}H|ORT|SETOR\\s{0,}T|ST\\s{0,}ABIT|ST\\s{0,}COM|ST\\s{0,}HAB|ST\\s{0,}TRA|PENINS|PLANALT|TRADI|TAG.*\\s{0,}SUL|BRAZ|VILA REAL|VILA\\s{0,}CENTRO|HOSPITALAR|ENTRE\\s{0,}LAGOS|LAGO NORTE|OTCOGONAL|PRIVE|CON.*\\s{0,}LAGO\\s{0,}SUL|ITAIP|S H J B LAGO SUL|BANDE|ORTOGONAL|J BOT|JD BOT|VILA CENTRO SUL", paciente_endereco_bairro), "LAGO SUL",paciente_endereco_bairro))

lagosul <- dados_df %>% filter(bairro_correto == "LAGO SUL")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "LAGO SUL")


# > LAGO NORTE ------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("AGO\\s{0,}N|AGOL\\s{0,}N|L\\s{0,}NORTE|LA.*\\s{0,}NORT|LG.*\\s{0,}NORT|LL.*\\s{0,}NOR|LO.*\\s{0,}NOR|LAG.*\\s{0,}N|TAQUARI|^\\s{0,}LN\\s{0,}$",paciente_endereco_bairro) & !grepl("HABIT|COMERCIAL NORTE|ESTRUT|RESIDENCIAL\\s{0,}NORTE|S\\s{0,}TRADICIONAL\\s{0,}NORTE|SETOR\\s{0,}A\\s{0,}ISOLNORTE|SETOR\\s{0,}COML\\s{0,}NORTE|SETOR\\s{0,}RESIDENCIAL\\s{0,}NORTE|SETOR\\s{0,}TERMINAL\\s{0,}NORT|SETOR\\s{0,}TRADICIONAL|SLNORTETAG|OCT|CANDAG|PONTE|BRAZ|COLONIA\\s{0,}AGRIC|PENINS|SOBRA|PARAN|DOCUM|NOVO\\s{0,}GAMA|BOT|BONITA|LAGOINH|LAGUNA|CANDALOGANDIA|MIRANT|VARJ|TORTO", paciente_endereco_bairro), "LAGO NORTE",paciente_endereco_bairro))

lagonorte <- dados_df %>% filter(bairro_correto == "LAGO NORTE")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "LAGO NORTE")


# > GAMA ------------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("GAMA|PONT.*\\s{0,}A|PT.*\\s{0,}ALT|VILA RO[ZR]IZ|^G LESTE|^GAMA|VILA DVO|CIDADE NOVA|ENG.*\\s{0,}L|^\\s{0,}DVO\\s{0,}$|S.*\\s{0,}GAM",paciente_endereco_bairro) & !grepl("GOIAS|^N GAMA|NO GAM|NOV GAMA|NOVO GAMA", paciente_endereco_bairro), "GAMA",paciente_endereco_bairro))

gama <- dados_df %>% filter(bairro_correto == "GAMA")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "GAMA")


# > PLANALTINA ------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("PLA[A]*NA|PIPIRI|APARO|ARAP|PLNA|NALT|ARAO|MARIS|MESTRE\\s{0,}D|M\\s{0,}DAR|SARAND|SARND|SRAANDI|TAQUARA|VENEZA|VIL.*\\s{0,}FA|MONJ|JAR.*\\s{0,}ROR|PLAN|BICA.*\\s{0,}DER|POANGA|SAANDI|VALE DO AMANHECER|VILA BURITIS|V BURITIS|VILA\\s{0,}BURITS|VL BURITI[S]*|VILA BURITI|^\\s{0,}PLA\\s{0,}DF\\s{0,}$|^\\s{0,}PLA\\s{0,}$|^\\s{0,}SRN\\s{0,}A\\s{0,}$|DIMAS|VILA NOSSA SENHORA|QUIN.*\\s{0,}MAR|JD RORIZ|BURITIS III|SRL II|JD MORUMBI|SRL IV|BURITIS II|^\\s{0,}PL\\s{0,}DF\\s{0,}$|RAJAD|JARDIM MORUMBI|BURITIS IV|^\\s{0,}PL\\s{0,}$|ESTANCIA III|ESTANCIA IV|ESTANCIA V|NR RIO PRETO|PALANTILNA",paciente_endereco_bairro) & !grepl("GOIAS|PLAN.*\\s{0,}GO|GUARAPARI|GUARA|PERDIZES|JAMAR|LAMARAO|PARAO[AN]|MARISTA|MARAJO|VARAJO|VRAJAO|TRAJANOPOLIS|AMARANTE", paciente_endereco_bairro), "PLANALTINA",paciente_endereco_bairro))

planaltina <- dados_df %>% filter(bairro_correto == "PLANALTINA")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "PLANALTINA")


# > NÚCLEO BANDEIRANTE ----------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("BAND|CAUHY|METROP|^\\s{0,}N\\s{0,}B\\s{0,}$|DIVINE",paciente_endereco_bairro) & !grepl("CAND|PARK|WAY|FUNDO|VILA METROPOLITANA|RESIDENCIAL METROPOL|SAIAO|SAYAO", paciente_endereco_bairro), "NÚCLEO BANDEIRANTE",paciente_endereco_bairro))

nucleo <- dados_df %>% filter(bairro_correto == "NÚCLEO BANDEIRANTE")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "NÚCLEO BANDEIRANTE")


# > VICENTE PIRES ---------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("SETEM|EMBRO|VICEN|V\\s{0,}PIR|PIRES|HAB.*\\s{0,}SAM|AGRIC.*\\s{0,}SAMA|C\\s{0,}A\\s{0,}SAMA|AGRIC[UO]LA\\s{0,}SAMA|C\\s{0,}AG\\s{0,}SAM|C\\s{0,}AGRI\\s{0,}SAM|CA\\s{0,}SAM|COL\\s{0,}AG\\s{0,}SAM|AGICOLA\\s{0,}SAM|COL\\s{0,}AGR\\s{0,}SAM|COL\\s{0,}AGRI\\s{0,}SAM|COLAGRSAM|COLONIA\\s{0,}A\\s{0,}SAM|COL\\s{0,}A\\s{0,}SAM|COL\\s{0,}AGRC\\s{0,}SAM|COL\\s{0,}AGRICSAM|COL\\s{0,}AGRSAM|COL\\s{0,}SAM|COL\\s{0,}AGRISAM|COLAGRICSA|COLAGRICSA|COLONIA\\s{0,}AGR\\s{0,}SAM|COLONIA\\s{0,}AGRI\\s{0,}SAM|COLAGRISAM|CASAMA|COLONIA\\s{0,}AGSAM|COLONIA\\s{0,}ASAM|COLONIA\\s{0,}SAM|C\\s{0,}A\\s{0,}SAM|C\\s{0,}SAM|CL\\s{0,}AGRI\\s{0,}SAM|COLO\\s{0,}AG\\s{0,}SAM|COLONIA\\s{0,}A[CG]RICOLA\\s{0,}SAM|^\\s{0,}VP\\s{0,}$",paciente_endereco_bairro) & !grepl("VILA VICENTE|VICENTIN|SEBAST", paciente_endereco_bairro), "VICENTE PIRES",paciente_endereco_bairro))

vicente <- dados_df %>% filter(bairro_correto == "VICENTE PIRES")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "VICENTE PIRES")


# > SAMAMBAIA -------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("AMAMB|SAMAAB|SAMA|SAAM|SAMAA|SAMSAMBAIA|SAMANBA|SAM.*\\s{0,}N|SAM.*\\s{0,}S|SAM.*\\s{0,}AMB|SAM.*\\s{0,}DF|SAM.*\\s{0,}H| SAMB|SAMJAM|SAMM|SAMN|SAMN|SAMSUL|\\bSAM\\b|BAIA|ED VILA REAL SAM SUL",paciente_endereco_bairro) & !grepl("SAMAU|ASAM|SAMNTA|TAGUA|VILA BAIANA|RESIDENCIAL BAIA|SUCESSO|BAIA DOS|^\\s{0,}BAIA\\s{0,}$|SEBAST", paciente_endereco_bairro), "SAMAMBAIA",paciente_endereco_bairro))

samamba <- dados_df %>% filter(bairro_correto == "SAMAMBAIA")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "SAMAMBAIA")


# > CANDANGOLÂNDIA --------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("CAND|GOLA",paciente_endereco_bairro) & !grepl("BRAZ|BAND|JOAO|CANDIDO|SEBAST", paciente_endereco_bairro), "CANDANGOLÂNDIA",paciente_endereco_bairro))

candanga <- dados_df %>% filter(bairro_correto == "CANDANGOLÂNDIA")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "CANDANGOLÂNDIA")


# > GUARÁ -----------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("GUAR[R]*A|LUCIO|L.*\\s{0,}COSTA|PARK\\s{0,}SUL|GARRA|SAIAO|SAYAO|GURA II|SOF SUL|QUADRAS ECONOMICAS|^\\s{0,}G\\s{0,}I\\s{0,}[I*]\\s{0,}$",paciente_endereco_bairro) & !grepl("GUARAPARI|ESTRUTUR|COSTA VERDE|SEBAST", paciente_endereco_bairro), "GUARÁ",paciente_endereco_bairro))

guara <- dados_df %>% filter(bairro_correto == "GUARÁ")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "GUARÁ")


# > BRAZLÂNDIA ------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("BRAZ|VEREDAS|INCRA|BRASL|ZLAN|ALE.*\\s{0,}GU|ALMECEGAS|^\\s{0,}BURITS\\s{0,}$|^\\s{0,}BURITIS\\s{0,}$|CURRALINHO|RADIOBRAS|RODEADOR|CHAPADINHA|VILA SAO JOSE|CASCALHEIRA",paciente_endereco_bairro) & !grepl("CAND|RADIOBRAZ|SOBRA|BRASLILIA|BRASLIA|VILA RORIZ|SEBAST", paciente_endereco_bairro), "BRAZLÂNDIA",paciente_endereco_bairro))

braz <- dados_df %>% filter(bairro_correto == "BRAZLÂNDIA")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "BRAZLÂNDIA")


# > SÃO SEBASTIÃO ---------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("SEBAS|AGUIL|COMPRIDO|CRIXA|TABOQUINHA|MA[INM]GUEIR|MORRO\\s{0,}AZUL|SETOR\\s{0,}TRADICIONAL|PALMARES|RESIDENCIAL\\s{0,}VITORIA|TIAO|SAO\\s{0,}SE|JARD.*\\s{0,}MA.*\\s{0,}AL|AMNGUEIRAL|MANG|AGROVI|JARDINS DAS ACACIAS|MORRO DA CRUZ|CAVAS DE BAIXO|^\\s{0,}CAVAS\\s{0,}$|BONSUCESSO|VILA DO BOA|RESIDENCIAL BOSQUE|RES VITORIA|RES BOSQUE|RESID DO BOSQUE|RESIDENCIAL DO BOSQUE|RES DO BOSQUE|RESIDENCIAL DO BOSQ|JOAO CANDIDO|^\\s{0,}BOSQUE\\s{0,}$|BARTOLOMEU|PAPUDA|L[OI]MEU",paciente_endereco_bairro) & !grepl("BOTAN|SOBRA|ANSAO|MANGABA" ,paciente_endereco_bairro), "SÃO SEBASTIÃO",paciente_endereco_bairro))

sebast <- dados_df %>% filter(bairro_correto == "SÃO SEBASTIÃO")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "SÃO SEBASTIÃO")


# > PARK WAY --------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("PAR.*\\s{0,}W|P.*\\s{0,}WA|SETOR DE MANSOES PA|SMPW|MSPW",paciente_endereco_bairro) & !grepl("ARNIQ|BAND|N\\s{0,}BAN|SEBAST" ,paciente_endereco_bairro), "PARK WAY",paciente_endereco_bairro))

parkway <- dados_df %>% filter(bairro_correto == "PARK WAY")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "PARK WAY")


# > CRUZEIRO --------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("CRUZE|C.*\\s{0,}NOVO|^\\s{0,}VELHO\\s{0,}$|CRUSE|C.*\\s{0,}VELHO|^\\s{0,}CRUZ\\s{0,}$",paciente_endereco_bairro) & !grepl("OCT|SUDO|NHO|PARA|HORI|CARNE|SOBR" ,paciente_endereco_bairro), "CRUZEIRO",paciente_endereco_bairro))

cruzeiro <- dados_df %>% filter(bairro_correto == "CRUZEIRO")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "CRUZEIRO")


# > ITAPOÃ ----------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("[I]*TAP[UO][A]|POA|ITAO*|^\\s{0,}FAZENDINHA\\s{0,}$|DEL.*\\s{0,}LAG",paciente_endereco_bairro) & !grepl("PARAN|ITAPETI|POANGA" ,paciente_endereco_bairro), "ITAPOÃ",paciente_endereco_bairro))

dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("DEL.*\\s{0,}LAG|DEL ALGO|DEL LADO|DEL LOGO|DELAGO|DELALGO",paciente_endereco_bairro), "ITAPOÃ",paciente_endereco_bairro))

itapoa <- dados_df %>% filter(bairro_correto == "ITAPOÃ")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "ITAPOÃ")


# > JARDIM BOTÂNICO -------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("BOT|J.*\\s{0,}BOA|JA.*\\s{0,}BO|NICO|J.*\\s{0,}BTA|QU.*\\s{0,}INTE|JA.*\\s{0,}DO\\s{0,}LA|J BOT|JD BOT|S H J B LAGO SUL|SOLAR DE BRASI|JARDIM IMPERIAL|SHJB",paciente_endereco_bairro) & !grepl("FOGO|BOTIG|UNICO" ,paciente_endereco_bairro), "JARDIM BOTÂNICO",paciente_endereco_bairro))

jardim <- dados_df %>% filter(bairro_correto == "JARDIM BOTÂNICO")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "JARDIM BOTÂNICO")


# > VARJÃO ----------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("V[A]*RJ|JAO",paciente_endereco_bairro), "VARJÃO",paciente_endereco_bairro))

varjao <- dados_df %>% filter(bairro_correto == "VARJÃO")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "VARJÃO")


# > ARNIQUEIRA ------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("ARN|QUEIRA|ARANIQUEIR",paciente_endereco_bairro) & !grepl("ARNALDO|MONTE|BOQ|CERQ" ,paciente_endereco_bairro), "ARNIQUEIRA",paciente_endereco_bairro))

arniq <- dados_df %>% filter(bairro_correto == "ARNIQUEIRA")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "ARNIQUEIRA")


# > FERCAL ----------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("ERCAL|FRACAL|F[E]*RCAL",paciente_endereco_bairro), "FERCAL",paciente_endereco_bairro))

fercal <- dados_df %>% filter(bairro_correto == "FERCAL")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "FERCAL")


# > SIA -------------------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("SIA|ABAST|SAAN|SOF NORTE|SETOR DE INDUSTRIAS|SETOR DE INDUSTRIA",paciente_endereco_bairro) & !grepl("GAMA|SOBR|ACASSI|SAANTA" ,paciente_endereco_bairro), "SIA",paciente_endereco_bairro))

sia <- dados_df %>% filter(bairro_correto == "SIA")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "SIA")


# > SUDOESTE/OCTOGONAL ----------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("OCT|SQSW|GONAL|OGON|SUDO|DOESTE|SSUDESTE|SU.*\\s{0,}STE|OTOCONAL|ORTOGONAL|^\\s{0,}SIG\\s{0,}$|^\\s{0,}AOS\\s{0,}$",paciente_endereco_bairro) & !grepl("CRUZ|SETOR RESIDOESTE" ,paciente_endereco_bairro), "SUDOESTE/OCTOGONAL",paciente_endereco_bairro))

sdk <- dados_df %>% filter(bairro_correto == "SUDOESTE/OCTOGONAL")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "SUDOESTE/OCTOGONAL")


# > SCIA/ESTRUTURAL -------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("SCIA|ESTRU|TRUT|TURAL|SETORES COMPLEMENTA",paciente_endereco_bairro) & !grepl("OESTE RURAL|MAGESTRAL" ,paciente_endereco_bairro), "SCIA/ESTRUTURAL",paciente_endereco_bairro))

estrut <- dados_df %>% filter(bairro_correto == "SCIA/ESTRUTURAL")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "SCIA/ESTRUTURAL")


# > TIRANDO O GOIÁS -------------------------------------------------------
dados_df <- dados_df %>%
  mutate(bairro_correto = ifelse(grepl("GOIAS|LINDA|PADRE|BERN|GOIANIA|\\bGO\\b|PED[R]*EG|VALP|OCID|LUZIAN|N.*\\s{0,}GAMA|FORMOS|J.*\\s{0,}INGA|DESC|SANTO ANTONIO DO [PD]E|VAL P|VAPA|CATAL",paciente_endereco_bairro) & !grepl("OLINDA|VILA SAO BERNARDO|MARINGA|DESCON" ,paciente_endereco_bairro), "GOIÁS",paciente_endereco_bairro))

goias <- dados_df %>% filter(bairro_correto == "GOIÁS")
dados_df <- dados_df %>% filter(is.na(bairro_correto)|bairro_correto != "GOIÁS")


# > SUBSTITUINDO SEM INFORMAÇÕES ------------------------------------------
dados_df <- dados_df %>%  mutate(bairro_correto = ifelse(grepl("SEM INFORMACAO|NAO\\s{0,}INF.*|NAO\\s{0,}CONSTA|BAIRRO\\s{0,}DESCONHECIDO|N\\s{0,}INFOR.*",paciente_endereco_bairro), "",paciente_endereco_bairro))

seminfo <- dados_df %>% filter(bairro_correto == "")


# > NEM RA NEM CEP --------------------------------------------------------
nemnem <- dados_df %>% filter(bairro_correto == "" & is.na(paciente_endereco_cep))
nemnem <- nemnem %>% mutate(bairro_correto = ifelse(bairro_correto == "", NA, bairro_correto))

dados_df <- dados_df %>% mutate(bairro_correto = ifelse(bairro_correto == "", NA, bairro_correto))
dados_df<- dados_df[!is.na(dados_df$bairro_correto) | !is.na(dados_df$paciente_endereco_cep), ]


# > TIRANDO OS CEPs com NA ------------------------------------------------
dados_ceps <- dados_df %>% filter(!is.na(paciente_endereco_cep))

# Trabalhando com os CEPs -------------------------------------------------

# Filtrando CEPs inválidos
cep_valido <- dados_ceps %>% filter(paciente_endereco_cep > 70000 & paciente_endereco_cep < 80000 & paciente_endereco_cep != 71000 & paciente_endereco_cep != 72000 & paciente_endereco_cep != 73000 & paciente_endereco_cep != 74000& paciente_endereco_cep != 75000& paciente_endereco_cep != 76000 & paciente_endereco_cep != 77000 & paciente_endereco_cep != 78000 & paciente_endereco_cep != 79000)

colnames(cep_valido)[3] <- "cep"
ceps$cep <- as.numeric(substr(ceps$cep, 1, 5)) #pegando os primeiros 5 dígitos dos CEPs

cep_valido$ras_corretas <- ceps$RA[match(cep_valido$cep,ceps$cep)]

cep_valido <- cep_valido %>%  mutate(ras_corretas = toupper(ras_corretas))
#cep_valido <- cep_valido %>%  group_by(ras_corretas) %>% count(ras_corretas)

ceps_ajustados <- cep_valido %>%
  mutate(ras_corretas = ifelse(grepl("ÁGUAS CLARAS", ras_corretas) & !grepl("ARNIQUEIRA", ras_corretas), "ÁGUAS CLARAS", ras_corretas),
         ras_corretas = ifelse(grepl("ARNIQUEIRA", ras_corretas), "ARNIQUEIRA", ras_corretas),
         ras_corretas = ifelse(grepl("ASA SUL|ASA NORTE|TELEB|CÍVI|PLANALTO|DARCY|GRAN|MIL|NORO|SETORES COMPLEMENTARES", ras_corretas), "PLANO PILOTO", ras_corretas),
         ras_corretas = ifelse(grepl("BRAZ", ras_corretas), "BRAZLÂNDIA", ras_corretas),
         ras_corretas = ifelse(grepl("CANDAN", ras_corretas), "CANDANGOLÂNDIA", ras_corretas),
         ras_corretas = ifelse(grepl("CEI", ras_corretas), "CEILÂNDIA", ras_corretas),
         ras_corretas = ifelse(grepl("CRUZ", ras_corretas), "CRUZEIRO", ras_corretas),
         ras_corretas = ifelse(grepl("FER", ras_corretas), "FERCAL", ras_corretas),
         ras_corretas = ifelse(grepl("GAMA", ras_corretas), "GAMA", ras_corretas),
         ras_corretas = ifelse(grepl("GUAR", ras_corretas), "GUARÁ", ras_corretas),
         ras_corretas = ifelse(grepl("ITAP", ras_corretas), "ITAPOÃ", ras_corretas),
         ras_corretas = ifelse(grepl("BOT", ras_corretas), "JARDIM BOTÂNICO", ras_corretas),
         ras_corretas = ifelse(grepl("LAGO N|SETOR DE HABITAÇÕES INDIVIDUAIS NORTE", ras_corretas), "LAGO NORTE", ras_corretas),
         ras_corretas = ifelse(grepl("LAGO S|SETOR DE HABITAÇÕES INDIVIDUAIS SUL", ras_corretas), "LAGO SUL", ras_corretas),
         ras_corretas = ifelse(grepl("BAND", ras_corretas), "NÚCLEO BANDEIRANTE", ras_corretas),
         ras_corretas = ifelse(grepl("PARA", ras_corretas), "PARANOÁ", ras_corretas),
         ras_corretas = ifelse(grepl("WAY", ras_corretas), "PARK WAY", ras_corretas),
         ras_corretas = ifelse(grepl("PLANALTI", ras_corretas), "PLANALTINA", ras_corretas),
         ras_corretas = ifelse(grepl("EMAS", ras_corretas), "RECANTO DAS EMAS", ras_corretas),
         ras_corretas = ifelse(grepl("FUNDO I\\b", ras_corretas), "RIACHO FUNDO I", ras_corretas),
         ras_corretas = ifelse(grepl("FUNDO II", ras_corretas), "RIACHO FUNDO II", ras_corretas),
         ras_corretas = ifelse(grepl("SAMAM", ras_corretas), "SAMAMBAIA", ras_corretas),
         ras_corretas = ifelse(grepl("SANTA", ras_corretas), "SANTA MARIA", ras_corretas),
         ras_corretas = ifelse(grepl("SEBA", ras_corretas), "SÃO SEBASTIÃO", ras_corretas),
         ras_corretas = ifelse(grepl("ESTRU", ras_corretas), "SCIA/ESTRUTURAL", ras_corretas),
         ras_corretas = ifelse(grepl("ZONA INDUSTRIAL", ras_corretas), "SIA", ras_corretas),
         ras_corretas = ifelse(grepl("SOBRADINHO II", ras_corretas), "SOBRADINHO II", ras_corretas),
         ras_corretas = ifelse(grepl("SOBRADINHO", ras_corretas)& !grepl("SOBRADINHO II", ras_corretas), "SOBRADINHO I", ras_corretas),
         ras_corretas = ifelse(grepl("SOL NASCENTE|PÔR DO SOL", ras_corretas), "SOL NASCENTE/PÔR DO SOL", ras_corretas),
         ras_corretas = ifelse(grepl("OCT|SUDO", ras_corretas), "SUDOESTE/OCTOGONAL", ras_corretas),
         ras_corretas = ifelse(grepl("TAG", ras_corretas), "TAGUATINGA", ras_corretas),
         ras_corretas = ifelse(grepl("VARJ", ras_corretas), "VARJÃO", ras_corretas),
         ras_corretas = ifelse(grepl("PIR", ras_corretas), "VICENTE PIRES", ras_corretas))

ceps_ajustados <- ceps_ajustados %>% mutate(ras_corretas = ifelse(ras_corretas == "", NA, ras_corretas))
ceps_ajustados <- ceps_ajustados %>% filter(!is.na(ras_corretas))
ceps_ajustados <- ceps_ajustados %>% group_by(ras_corretas) %>% count(ras_corretas)

sum(ceps_ajustados$n)
aguas2 <- sum(length(aguas$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="ÁGUAS CLARAS"])
arniq2 <- sum(length(arniq$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="ARNIQUEIRA"])
braz2 <- sum(length(braz$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="BRAZLÂNDIA"])
candanga2 <- sum(length(candanga$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="CANDANGOLÂNDIA"])
cei2 <- sum(length(cei$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="CEILÂNDIA"])
cruzeiro2 <- sum(length(cruzeiro$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="CRUZEIRO"])
fercal2 <- sum(length(fercal$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="FERCAL"])
gama2 <- sum(length(gama$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="GAMA"])
guara2 <- sum(length(guara$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="GUARÁ"])
itapoa2 <- sum(length(itapoa$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="ITAPOÃ"])
jardim2 <- sum(length(jardim$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="JARDIM BOTÂNICO"])
lagonorte2 <- sum(length(lagonorte$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="LAGO NORTE"])
lagosul2 <- sum(length(lagosul$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="LAGO SUL"])
nucleo2 <- sum(length(nucleo$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="NÚCLEO BANDEIRANTE"])
paranoa2 <- sum(length(paranoa$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="PARANOÁ"])
parkway2 <- sum(length(parkway$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="PARK WAY"])
planaltina2 <- sum(length(planaltina$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="PLANALTINA"])
plano2 <- sum(length(plano$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="PLANO PILOTO"])
recanto2 <- sum(length(recanto$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="RECANTO DAS EMAS"])
riacho12 <- sum(length(riacho1$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="RIACHO FUNDO I"])
riacho22 <- sum(length(riacho2$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="RIACHO FUNDO II"])
samamba2 <- sum(length(samamba$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="SAMAMBAIA"])
santa2 <- sum(length(santa$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="SANTA MARIA"])
sebast2 <- sum(length(sebast$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="SÃO SEBASTIÃO"])
estrut2 <- sum(length(estrut$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="SCIA/ESTRUTURAL"])
sia2 <- sum(length(sia$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="SIA"])
sobradinho22 <- sum(length(sobradinho2$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="SOBRADINHO II"])
sobradinho12 <- sum(length(sobradinho1$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="SOBRADINHO I"])
sol2 <- sum(length(sol$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="SOL NASCENTE/PÔR DO SOL"])
sdk2 <- sum(length(sdk$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="SUDOESTE/OCTOGONAL"])
taguatinga2 <- sum(length(taguatinga$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="TAGUATINGA"])
varjao2 <- sum(length(varjao$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="VARJÃO"])
vicente2 <- sum(length(vicente$bairro_correto),ceps_ajustados$n[ceps_ajustados$ras_corretas=="VICENTE PIRES"])


ras_finais <-  matrix(c("ÁGUAS CLARAS","ARNIQUEIRA", "BRAZLÂNDIA","CANDANGOLÂNDIA", "CEILÂNDIA",
                        "CRUZEIRO", "FERCAL", "GAMA", "GUARÁ", "ITAPOÃ", "JARDIM BOTÂNICO", "LAGO NORTE",
                        "LAGO SUL", "NÚCLEO BANDEIRANTE", "PARANOÁ", "PARK WAY", "PLANALTINA", "PLANO PILOTO", "RECANTO DAS EMAS",
                        "RIACHO FUNDO I", "RIACHO FUNDO II", "SAMAMBAIA", "SANTA MARIA", "SÃO SEBASTIÃO",
                        "SCIA/ESTRUTURAL", "SIA", "SOBRADINHO I", "SOBRADINHO II", "SOL NASCENTE/PÔR DO SOL",
                        "SUDOESTE/OCTOGONAL", "TAGUATINGA", "VARJÃO", "VICENTE PIRES", aguas2, arniq2, braz2, candanga2, cei2, 
                        cruzeiro2, fercal2, gama2, guara2, itapoa2, jardim2, lagonorte2, lagosul2, nucleo2, paranoa2,
                        parkway2, planaltina2, plano2, recanto2, riacho12, riacho22, samamba2, santa2, sebast2, estrut2, sia2, sobradinho12, 
                        sobradinho22, sol2, sdk2, taguatinga2, varjao2, vicente2), ncol=2, byrow = F)
colnames(ras_finais) <- c("RA", "Quantidade")

ras_finais <- data.frame(ras_finais)
