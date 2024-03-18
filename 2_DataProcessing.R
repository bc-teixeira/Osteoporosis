library(tidyverse)
library(readxl)

load("PA.RData")
load("RD.RData")
load("EQ.RData")

#downloaded from https://www.ans.gov.br/anstabnet/cgi-bin/dh?dados/tabnet_02.def and http://tabnet.datasus.gov.br/cgi/deftohtm.exe?ibge/cnv/popsvsbr.def in 04/08/2023
#save(popmunicbr, popANS, file = "popIBGEANS.RData")
load(file = "popIBGEANS.RData")



#FaixasEtarias
fxet <- read_excel("aux_datasus2.xlsx", 
                   sheet = "FaixaEtaria")
fxet$FaixaEtr <- as.factor(fxet$FaixaEtr)
fxet$FaixaEtr <- fct_relevel(fxet$FaixaEtr, "5 a 9 anos", after = 1)
RD <- RD %>% left_join(fxet, by = c("IDADE" = "Idade"))

#Add Fields to RD
RD$DT_INTER <- ymd(RD$DT_INTER)
RD$ano <- year(RD$DT_INTER)
levels(RD$SEXO) <- c("Male", "Female")
RD$SEXO <- fct_relevel(RD$SEXO, "Female")

#PROC_DS
tbProc <- read_excel("aux_datasus2.xlsx", sheet = "Procedimentos")
X <- RD %>% 
  left_join(tbProc, by = join_by(IP_COD)) %>% select(PROC_DESC)
RD$DS_PROC <- X$PROC_DESC
rm(X)

#Add Fields to  PA
PA$DATA <- ymd(paste0(PA$DATA, "01"))
PA$ano <- year(PA$DATA)
PA$SEXO <- factor(PA$SEXO)
levels(PA$SEXO) <- c("Undefined", "Female", "Male")
PA$SEXO <- fct_relevel(PA$SEXO, "Female")

fxet <- read_excel("aux_datasus2.xlsx", 
                   sheet = "FaixaEtaria")
fxet$FaixaEtr <- as.factor(fxet$FaixaEtr)
fxet$FaixaEtr <- fct_relevel(fxet$FaixaEtr, "5 a 9 anos", after = 1)
PA <- PA %>% mutate(IDADE = as.numeric(IDADE)) %>%  left_join(fxet, by = c("IDADE" = "Idade"))


#DEFINIÇÃO DE POPULAÇÃO SUS    
#Population in SUS by City, Age Group and Gender
# Population - Population with health plans (if negative then 0)
popSUS <- left_join(popmunicbr, popANS, by = c("ano", "Sexo", "CD_MUNIC", "FaixaEtr")) %>%
  select(ano, Sexo, CD_MUNIC, FaixaEtr, Pop = Pop.x, PopPlS = Pop.y) %>%
  replace_na(list(Pop = 0, PopPlS = 0)) %>%
  mutate(PopSUS = ifelse(Pop - PopPlS < 0, 0, Pop - PopPlS))

levels(popSUS$Sexo) <- c("Female", "Male")

#Total Brazilian population
popBras <- popmunicbr %>% group_by(ano, Sexo, FaixaEtr) %>% summarise(Pop = sum(Pop, na.rm = TRUE), .groups = "keep")

#Total Brazilian Population with Private Health Insurance
popBrasPlS <- popANS %>% group_by(ano, Sexo, FaixaEtr) %>% summarise(Pop = sum(Pop, na.rm = TRUE), .groups = "keep")

#Total Brazilian Population without Private Health Insurance
popSUSBR <- left_join(popBras, popBrasPlS, by = c("ano", "Sexo", "FaixaEtr")) %>%
  mutate(PopSUS = Pop.x - Pop.y, CobSUS = (Pop.x - Pop.y)/Pop.x) %>%
  select(ano, Sexo, FaixaEtr, PopSUS, CobSUS)

levels(popSUSBR$Sexo) <- c("Female", "Male")

#popBrasPlS %>% filter(ano == 2022)



rm(fxet)

#Adiciona tabela de CIDS
CID <- read_excel("aux_datasus2.xlsx", 
                                  sheet = "CIDs")

#Procedimentos únicos por fratura
FiltraProc <- read.csv("FiltraProc.txt", sep="")

#Adiciona descrição de CIDs
RD <- RD %>% 
  mutate (DIAG_CAP = substr(CD_CID,1,3)) %>%
  left_join(CID %>% select(CD_COD, DS_CID_CAP = CID), by = c("DIAG_CAP" = "CD_COD")) %>% 
  left_join(CID %>% select(CD_COD, DS_CID = CID), by = c("CD_CID" = "CD_COD")) %>% 
  mutate(UNICO = as.numeric(IP_COD) %in% FiltraProc$Procedimentos)

#Adiciona cidade e UF EQ
UF_MUN <- read_excel("aux_datasus2.xlsx", 
                   sheet = "UFMUN")
EQ <- EQ %>% 
  mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
  left_join(UF_MUN, by = "CD_MUN")

levels(popSUSBR$FaixaEtr)


