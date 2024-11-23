library(tidyverse)
library(readxl)
library(readr)

load("PA.RData")
load("RD.RData")
load("EQ.RData")

#ANS population downloaded in 23/11/2024 from https://www.ans.gov.br/anstabnet/cgi-bin/tabnet?dados/tabnet_br.def
popANSMasc <- read_delim("Populations/A18233310_22_1_196.csv",
                        delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                        name_repair = "universal",
                        skip = 4)

popANSFem <- read_delim("Populations/A18241310_22_1_196.csv",
                         delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                         name_repair = "universal",
                         skip = 4)

#Create ANS Popuation data
popANSMasc <- popANSMasc %>% 
  pivot_longer(cols = -1, names_to = "ano", values_to = "pop") %>%
  mutate(ano = as.numeric(str_replace(ano, "Jun", "20"))) %>%  #str replace "Jun" to 20 in ano
  mutate(FaixaEtr = as.factor(Faixa.etaria)) %>% 
  #merge groups FxEtaria when is "Ate 1 ano" and "1 a 4 anos" per ano 
  mutate(FaixaEtr = fct_recode(FaixaEtr, "0 a 4 anos" = "Ate 1 ano", "0 a 4 anos" = "1 a 4 anos")) %>%
  mutate(FaixaEtr = fct_relevel(FaixaEtr, "5 a 9 anos", after = 1)) %>% 
  group_by(ano, FaixaEtr) %>%
  summarise(PopANS = sum(pop), .groups = "keep") %>% 
  mutate(Sexo = "Male")

popANSFem <- popANSFem %>% 
  pivot_longer(cols = -1, names_to = "ano", values_to = "pop") %>%
  mutate(ano = as.numeric(str_replace(ano, "Jun", "20"))) %>%  #str replace "Jun" to 20 in ano
  mutate(FaixaEtr = as.factor(Faixa.etaria)) %>% 
  #merge groups FxEtaria when is "Ate 1 ano" and "1 a 4 anos" per ano 
  mutate(FaixaEtr = fct_recode(FaixaEtr, "0 a 4 anos" = "Ate 1 ano", "0 a 4 anos" = "1 a 4 anos")) %>%
  mutate(FaixaEtr = fct_relevel(FaixaEtr, "5 a 9 anos", after = 1)) %>% 
  group_by(ano, FaixaEtr) %>%
  summarise(PopANS = sum(pop), .groups = "keep")%>% 
  mutate(Sexo = "Female")

popANS <- bind_rows(popANSFem, popANSMasc) %>% 
  mutate(Sexo = factor(Sexo),
         Sexo = fct_relevel(Sexo, "Female")) 
rm(popANSMasc, popANSFem)



#FaixasEtarias
fxet <- read_excel("aux_datasus2.xlsx", 
                   sheet = "FaixaEtaria")
fxet$FaixaEtr <- as.factor(fxet$FaixaEtr)
fxet$FaixaEtr <- fct_relevel(fxet$FaixaEtr, "5 a 9 anos", after = 1)

#IBGE Data downloaded from https://ftp.ibge.gov.br/Projecao_da_Populacao/Projecao_da_Populacao_2024/projecoes_2024_tab1_idade_simples.xlsx in 23/11/2024
popIBGE <- read_excel("Populations/projecoes_2024_tab1_idade_simples.xlsx", 
                      skip = 5) %>% 
  filter(SIGLA == "BR", SEXO %in% c("Homens", "Mulheres")) %>% 
  pivot_longer(cols = c(6:ncol(.)), names_to = "ano", values_to = "pop") %>%
  select(1,2,6,7) %>% 
  left_join(fxet, by = c("IDADE" = "Idade")) %>%
  mutate(Sexo = factor(SEXO, levels = c("Homens", "Mulheres"), labels = c("Male", "Female")),
         Sexo = fct_relevel(Sexo, "Female")) %>%
  group_by(ano, FaixaEtr, Sexo) %>%
  summarise(PopIBGE = sum(pop), .groups = "keep") %>% 
  mutate(ano = as.numeric(ano)) %>% 
  filter(ano >= 2008) 


#Add Fields to RD
RD <- RD %>% left_join(fxet, by = c("IDADE" = "Idade"))


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
popSUSBR <- left_join(popANS, popIBGE, by = c("ano", "Sexo", "FaixaEtr")) %>%
  select(ano, Sexo, FaixaEtr, PopIBGE  , PopANS) %>%
  replace_na(list(PopANS  = 0, PopIBGE = 0)) %>%
  mutate(PopSUS = ifelse(PopIBGE - PopANS < 0, 0, PopIBGE - PopANS)) %>% 
  filter(FaixaEtr != "Inconsistente" & FaixaEtr != "Total") %>% 
  #eliminate unused factor levels
  droplevels()

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




