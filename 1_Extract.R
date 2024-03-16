library(read.dbc)
library(tidyverse)
library(svMisc)

#Link para extração de dados (Apenas arquivos iniciados com PA*.dbc)
# ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/
#Somente conectável com IP Brasileiro

#Mudar para o diretório onde estão guardados os DBCs
filesdirectory <- "D:/DATASUS/SIA/DADOS/"

#Captura arquivos que iniciam com PA
files <- dir(filesdirectory, pattern = "^PA.*")

#Anos a serem incluidos na análise
incyears <- c(08:22)

#inclui somente arquivos de anos especificados
files <- files[as.numeric(substr(files, 5,6))%in% incyears]


#Amostra de arquivos para teste
#Comentar essa sessão em Produção
#files <- sample(files, size = 5)

#Cria vetor para guardar os dados
PA <- vector(mode = "list", length = length(files))



#Loop nos arquivos
for (i in 1:length(files)){
  PA[[i]] <-  read.dbc(paste0(filesdirectory, files[i]), as.is = TRUE) %>% 
              # filter(grepl(PA_CIDPRI, pattern = "^H[0-5].*") |            #Filtra CID para os CIDs de Oftalmologia H00 a H59
              #        PA_CBOCOD == "225265") %>%                           #Filtra CBO de Olftalmologia  
              filter(PA_PROC_ID == "0204060028") %>% 
              group_by(CNES = PA_CODUNI, #Codigo CNES do estabelecimento (CNES.csv)
                       PROC = PA_PROC_ID,
                       DATA = PA_CMP, #Data da competência
                       CBO = PA_CBOCOD, #Código CBO (CBO.csv)
                       CD_MUN = PA_UFMUN, #Cidade (aux_datasus.xlsx\UFMUN)
                       IP_COD = PA_PROC_ID, #Procedimento (SIGTAP.csv)
                       SEXO = PA_SEXO, #Sexo (M;F)
                       IDADE = PA_IDADE, #Idade
                       CAR_AT = PA_CATEND, #Carater de atendimento (aux_datasus.xlsx\Carat_Atend)
                       CD_CID = PA_CIDPRI #Código CID Principal (aux_datasus.xlsx\CIDs)
                       ) %>% 
              summarise( PA_QTDPRO = sum(PA_QTDPRO),
                         PA_QTDAPR = sum(PA_QTDAPR),
                         PA_VALPRO = sum(PA_VALPRO),
                         PA_VALAPR = sum(PA_VALAPR), .groups = "drop")
  progress(i, max.value = length(files))                                 #Barra de progressão para acompanhar status do processo
}

#Consolida arquivos
PA <- plyr::rbind.fill(PA)

save(PA, file = "PA.RData")


#Mudar para o diret?rio onde estão guardados os DBCs
filesdirectory <- "D:/DATASUS/SIH/DADOS/"



#Captura arquivos que iniciam com RD
files <- dir(filesdirectory, pattern = "^RD.*dbc$")


#Anos a serem incluidos na análise
incyears <- c(08:22)

#inclui somente arquivos de anos especificados
files <- files[as.numeric(substr(files, 5,6))%in% incyears]


# #Amostra de arquivos para teste
# #Comentar essa sessão em Produção
# set.seed(123)
# files <- sample(files, size = 20)

#Cria vetor para guardar os dados
RD <- vector(mode = "list", length = length(files))

t <- Sys.time()
#Loop nos arquivos
for (i in 1:length(files)){
  

  
  RD[[i]] <-  read.dbc(paste0(filesdirectory, files[i]), as.is = TRUE) %>% 
    filter(grepl(DIAG_PRINC, pattern = "^S.{1}2.{0,1}$")) %>%             #Filtra CID para os CIDs de Oftalmologia H00 a H59 (conjun??o OU)
    select(DT_INTER, #Data da internação
           ANO_CMPT, #Ano competência
           MES_CMPT, #Mês de Competência
           N_AIH, #Contar Internação contar AIHs únicas
           CNES, #Codigo CNES do estabelecimento (CNES.csv)
           CD_MUN = MUNIC_MOV, #Cidade (aux_datasus.xlsx\UFMUN)
           CD_MUN_RES = MUNIC_RES, #Cidade onde o paciente origina-se (aux_datasus.xlsx\UFMUN)
           PROC_SOLIC, #Procedimento solicitado (SIGTAP.csv)
           IP_COD = PROC_REA, #Procedimento Aprovado (SIGTAP.csv)
           VAL_SH, #Valor Serviços hospitalares
           VAL_SP, #Valor Serviços Profissionais
           VAL_TOT, #Valor Total
           CAR_AT = CAR_INT, #Carater de atendimento (aux_datasus.xlsx\Carat_Atend)
           DIAS_PERM, #DIas de permanência
           QT_DIARIAS, #Quantidade de diárias
           SEXO, #Sexo (M;F)
           IDADE, #Idade
           CD_CID = DIAG_PRINC #Código CID (aux_datasus.xlsx\CIDs)
           
    ) %>% 
    mutate(SEXO = factor(SEXO, levels = c("1", "2", "3"), labels = c("M", "F", "F")),
           ANO_INTER = substr(DT_INTER, 1,4), #Ano da interna??o
           MES_INTER = substr(DT_INTER, 5,6)  #M?s da Interna??o
    )
  
  progress(i, max.value = length(files))                                 #Barra de progressão para acompanhar status do processo
}

#Consolida arquivos
RD <- plyr::rbind.fill(RD)


#Mudar para o diretório onde estão guardados os DBCs
filesdirectory <- "D:/DATASUS/SIA/DADOS/"

#Captura arquivos que iniciam com PA
files <- dir(filesdirectory, pattern = "^BI.*")

#Anos a serem incluidos na análise
incyears <- c(08:22)

#inclui somente arquivos de anos especificados
files <- files[as.numeric(substr(files, 5,6))%in% incyears]


#Amostra de arquivos para teste
#Comentar essa sessão em Produção
#files <- sample(files, size = 5)

#Cria vetor para guardar os dados
BI <- vector(mode = "list", length = length(files))



#Loop nos arquivos
for (i in 1:length(files)){
  BI[[i]] <-  read.dbc(paste0(filesdirectory, files[i]), as.is = TRUE) %>% 
    # filter(grepl(PA_CIDPRI, pattern = "^H[0-5].*") |            #Filtra CID para os CIDs de Oftalmologia H00 a H59
    #        PA_CBOCOD == "225265") %>%                           #Filtra CBO de Olftalmologia  
    filter(PROC_ID == "0204060028") %>% 
    select(CNES = CODUNI, #Codigo CNES do estabelecimento (CNES.csv)
             PROC =PROC_ID,
             DATA = DT_ATEND, #Data da competência
             CD_MUN = MUNPAC, #Cidade (aux_datasus.xlsx\UFMUN)
             CNS_PAC = CNS_PAC,
             SEXO = SEXOPAC, #Sexo (M;F)
             DTNASC = DTNASC,
             IDADE = IDADEPAC, #Idade
             CD_CID = CIDPRI, #Código CID Principal (aux_datasus.xlsx\CIDs)
             QTDPRO = QT_APROV,
             QTDAPR = QT_APRES)
  progress(i, max.value = length(files))                                 #Barra de progressão para acompanhar status do processo
}

#Consolida arquivos
BI <- plyr::rbind.fill(BI)



#Mudar para o diretório onde estão guardados os DBCs
filesdirectory <- "D:/DATASUS/SIA/DADOS/"

#Captura arquivos que iniciam com PA
files <- dir(filesdirectory, pattern = "^AM.*")

#Anos a serem incluidos na análise
incyears <- c(08:22)

#inclui somente arquivos de anos especificados
files <- files[as.numeric(substr(files, 5,6))%in% incyears]


#Amostra de arquivos para teste
#Comentar essa sessão em Produção
#files <- sample(files, size = 5)

#Cria vetor para guardar os dados
AM <- vector(mode = "list", length = length(files))



#Loop nos arquivos
for (i in 1:length(files)){
  AM[[i]] <-  read.dbc(paste0(filesdirectory, files[i]), as.is = TRUE) %>% 
    # filter(grepl(PA_CIDPRI, pattern = "^H[0-5].*") |            #Filtra CID para os CIDs de Oftalmologia H00 a H59
    #        PA_CBOCOD == "225265") %>%                           #Filtra CBO de Olftalmologia  
    filter(AP_PRIPAL %in% c("0601350014", "0601350022")) %>% 
    select(CNES = AP_CODUNI, #Codigo CNES do estabelecimento (CNES.csv)
           PROC = AP_PRIPAL,
           DATA = AP_CMP, #Data da competência
           CD_MUN = AP_MUNPCN, #Cidade (aux_datasus.xlsx\UFMUN)
           CNS_PAC = AP_CNSPCN,
           SEXO = AP_SEXO, #Sexo (M;F)
           IDADE = AP_NUIDADE, #Idade
           CD_CID = AP_CIDPRI) #Código CID Principal (aux_datasus.xlsx\CIDs)
  progress(i, max.value = length(files))                                 #Barra de progressão para acompanhar status do processo
}

#Consolida arquivos
AM<- plyr::rbind.fill(AM)

save(BI, file = "BI.RData")

save(AM, file = "AM.RData")


save(PA, file = "PA.RData")

save(RD, file = "RD.RData")


  #decode Function----
  chars <- c( "{", "|", "}", "~", "\177", "Ç", "ü", "é", "â", "ä", "\u0080", "\x80", "\x81", "\x82", "\x83", "\x84", "\u0081", "\u0082", "\u0083", "\u0084", "f", '"', "?"  , "‚", "ƒ", "„", "€")
  nums  <- c( "0", "1", "2", "3", "4"   , "5", "6", "7", "8", "9", "5"     , "5"   , "6"   , "7"   , "8"   , "9"   ,"6"     , "7"     , "8"     ,"9"      , "8", "9", "5"  , "7", "8", "9", "5")
  codetable <- data.frame(unique(cbind(chars, nums)), stringsAsFactors = FALSE)
  
  decode <- function (str){
    b <- as.vector(0)
    for (w in 1:length(str)){
      if (is.na(str[w])){b[w] <- NA} else {
        a <- unlist(strsplit(str[w],"", useBytes = TRUE))
        for (row in 1:length(a)){
          if (length(a) == 0) {b[w] <- NA} else { 
            b[w] <- paste(if_else(is.na(b[w]),"",as.character(b[w])),codetable[(codetable$chars==a[row]),2], sep = "")
          }
        }
      }
    }
    return(as.numeric(b))  
  }

  BIPAC <- unique(BI$CNS_PAC)
  BIPAC <- as.data.frame(BIPAC) %>% 
  mutate(decoded = decode(BIPAC))
  
#CNES
  #Mudar para o diretório onde estão guardados os DBCs
  filesdirectory <- "D:/DATASUS/CNES/Equipam/"
  
  #Captura arquivos que iniciam com PA
  files <- dir(filesdirectory, pattern = "^EQ.......dbc")
  
  #Anos a serem incluidos na análise
  incyears <- c(08:22)
  
  #inclui somente arquivos de anos especificados
  files <- files[as.numeric(substr(files, 5,6))%in% incyears]

  
  #Cria vetor para guardar os dados
  EQ <- vector(mode = "list", length = length(files))
  #files <- files[500:700]
  
  for (i in 1:length(files)){
    EQ[[i]] <-  read.dbc(paste0(filesdirectory, files[i]), as.is = TRUE) %>% 
      filter(CODEQUIP == "09") %>% 
      select(CNES = CNES, #Codigo CNES do estabelecimento (CNES.csv)
             CD_MUN = CODUFMUN,
             DATA = COMPETEN, #Data da competência
             QT = QT_EXIST, #Cidade (aux_datasus.xlsx\UFMUN)
             QT_USO = QT_USO,
             SUS = IND_SUS, #Sexo (M;F)
             NSUS = IND_NSUS) #Código CID Principal (aux_datasus.xlsx\CIDs)
    progress(i, max.value = length(files))                                 #Barra de progressão para acompanhar status do processo
  }
  
  #Consolida arquivos
  EQ<- plyr::rbind.fill(EQ)

EQ <- EQ %>% 
  mutate(SUS = factor(SUS, levels = c(1,0), labels = c("SUS", "Non-SUS")),
         QT = as.numeric(QT),
         QT_USO = as.numeric(QT_USO),
         DATA = ymd(paste0(DATA, "01")))
  
save(EQ, file = "EQ.RData")
