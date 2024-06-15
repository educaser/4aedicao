# EducaSER - 2024
# Minicurso - Aquisição de Dados com R
# Davi Alves - Dpartamento de Métodos Quantitativos (DMQ) - UNIRIO

# 1. Aquisição de Dados HTML ----

## 1.1. Instalar pacotes ----

install.packages("rvest")

install.packages("dplyr")

install.packages("stringr")

## 1.2. Carregar os pacotes ----

library(rvest)

library(dplyr)

library(stringr)

## 1.3. Aquisição dos dados ----

### 1.3.1. Página com uma única tabela estruturada ----

#### 1.3.1.1. Ler endereço HTML ----

ona <- read_html("https://www.ona.org.br/mapa-de-acreditacoes/eyJlc3RhYmVsZWNpbWVudG90aXBvIjoiIiwicXVhbGlmaWNhY2FvIjoiIiwiZXN0YWRvIjoiIiwiY2lkYWRlIjoiIiwibm9tZSI6IiIsInRpcG9nZXN0YW8iOiIifQ==")

ona

#### 1.3.1.2. Extrair a tabela ----

list_ona <- html_table(ona)

list_ona

#### 1.3.1.3. Alocar a tabela em data frame ----

# Clássico data.frame:

bd_ona <- data.frame(list_ona[[1]])

str(bd_ona)

# Tibble:

bd_ona <- list_ona[[1]]

str(bd_ona)

#### 1.3.1.4. Visualizando os dados ----

View(bd_ona)

### 1.3.2. Página com várias tabelas não tão bem estruturadas ----

#### 1.3.2.1. Ler endereço HTML ----

idh <- read_html("https://pt.wikipedia.org/wiki/Lista_de_pa%C3%ADses_por_%C3%8Dndice_de_Desenvolvimento_Humano")

#### 1.3.2.2. Extrair as tabela ----

list_idh <- html_table(idh)

list_idh

is(list_idh)

str(list_idh)

#### 1.3.2.3. Identificar os dados de interesse ----

# "Força bruta":

list_idh[[1]]

list_idh[[2]]

list_idh[[3]]

list_idh[[4]]

list_idh[[5]]

# Uma maneira mais "elegante":

grep("Dinamarca",list_idh)

#### 1.3.2.4. Alocar a tabela em data frame ----

# Clássico data.frame:

bd_idh <- data.frame(list_idh[[5]])

str(bd_idh)

# Tibble:

bd_idh <- list_idh[[5]]

str(bd_idh)

#### 1.3.2.5. Processando os dados ----

##### 1.3.2.5.1. Mesclando as linhas 1 e 2 para criar o nome das variáaveis ----

View(bd_idh)

names(bd_idh)

names(bd_idh) <- c("pos_22","var_pos_22_21","pais","idh22","idh21","var_idh_22_21")

View(bd_idh)

##### 1.3.2.5.2. Excluindo linhas com a indicação da faixa de IDH ----

grep("IDH",bd_idh$idh22,value = T)

grep("IDH",bd_idh$idh22)

bd_idh <- bd_idh[-grep("IDH",bd_idh$idh22),]

View(bd_idh)

##### 1.3.2.5.3. Corrigindo as posições em 2022 ----

head(bd_idh,15)

table(bd_idh$pos_22,useNA = "a")

bd_idh$pos_22[4]

bd_idh$pos_22[4] <- "4"

grep("!",bd_idh$pos_22)

bd_idh$pos_22[140]

bd_idh$pos_22[140] <- "140"

bd_idh$pos_22[190]

bd_idh$pos_22[190] <- "189"

bd_idh$pos_22 <- as.numeric(bd_idh$pos_22)

table(bd_idh$pos_22,useNA = "a")

##### 1.3.2.5.4. Excluindo a variação das posições em 2022 ----

bd_idh$var_idh_22_21 <- NULL

##### 1.3.2.5.5. Convertendo os valores do IDH para numérico, após substituir vírgula por ponto ----

bd_idh$idh22

bd_idh$idh22 <- as.numeric(str_replace(bd_idh$idh22,",","."))

bd_idh$idh21 <- as.numeric(str_replace(bd_idh$idh21,",","."))

##### 1.3.2.5.6. Calculando a variação do IDH entre 22 e 21 ----

bd_idh$var_idh_22_21 <- bd_idh$idh22-bd_idh$idh21

str(bd_idh)

##### 1.3.2.5.7. Calculando a variação do IDH entre 22 e 21 ----

rank(bd_idh$idh22)

bd_idh$var_pos_22_21 <- rank(bd_idh$idh22,ties.method = "max")-rank(bd_idh$idh21,ties.method = "max")

##### 1.3.2.5.8. Calculando a variação do IDH entre 22 e 21 ----

bd_idh <- list_idh[[5]]

names(bd_idh) <- c("pos_22","var_pos_22_21","pais","idh22","idh21","var_idh_22_21")

str(bd_idh)

bd_idh %>%
  select(!var_idh_22_21) %>%
  filter(!bd_idh$idh22 %in% grep("IDH",bd_idh$idh22,value = T)) %>%
  mutate(across("idh22",str_replace,",",".")) %>%
  mutate(across("idh21",str_replace,",",".")) %>%
  mutate_at(c("idh22","idh21"),as.numeric) %>% 
  mutate("var_pos_22_21"=rank(idh22,ties.method = "max")-rank(idh21,ties.method = "max")) -> bd_idh_dplyr

bd_idh_dplyr

bd_idh_dplyr$pos_22[4] <- "4"

bd_idh_dplyr$pos_22[140] <- "140"

bd_idh_dplyr$pos_22[190] <- "189"

bd_idh_dplyr

# 2. Aquisição de Microdados de SIS (microdtasus) ----

## 2.1. Instalar pacotes ----

install.packages("devtools")

devtools::install_github("rfsaldanha/microdatasus")

## 2.2. Carregar os pacotes ----

library(microdatasus)

## 2.3. Aquisição dos dados ----

### 2.3.1. O comando fetch_datasus ----

#### 2.3.1.1. Adquirindo dados de nascimentos do estado do Rio de Janeiro de 2022 ----

nvrj22 <- fetch_datasus(year_start = 2022,
                         year_end = 2022,
                         uf = "RJ",
                         information_system = "SINASC"
                          )

str(nvrj22)

#### 2.3.1.2. Pré-Processando as informações ----

nvrj22 <- process_sinasc(nvrj22)

str(nvrj22)

#### 2.3.1.3. Convertendo PESO, IDADEMAE e IDADEPAI para numérico ----

nvrj22$PESO <- as.numeric(nvrj22$PESO)

nvrj22$IDADEMAE <- as.numeric(nvrj22$IDADEMAE)

nvrj22$IDADEPAI <- as.numeric(nvrj22$IDADEPAI)

#### 2.3.2.1. Adquirindo dados de óbitos dos estados do Acre e Tocantins de 2021 e 2022 ----

obt22 <- fetch_datasus(year_start = 2021,
                        year_end = 2022,
                        uf = c("AC","TO"),
                        information_system = "SIM-DO"
)

str(obt22)

#### 2.3.2.2. Pré-Processando as informações ----

obt22 <- process_sim(obt22)

str(obt22)

#### 2.3.2.3. Preparando idade em anos completos ----

obt22$IDADE <- as.numeric(obt22$IDADEanos)

obt22$IDADE[which(!is.na(obt22$IDADEminutos))]

obt22$IDADE[which(!is.na(obt22$IDADEminutos))] <- 0

obt22$IDADE[which(!is.na(obt22$IDADEhoras))] <- 0

obt22$IDADE[which(!is.na(obt22$IDADEdias))] <- 0

obt22$IDADE[which(!is.na(obt22$IDADEmeses))] <- 0

summary(obt22$IDADE)

summary(as.numeric(obt22$IDADEanos))

#### 2.3.2.3. Preparando idade em anos completos ----

obt22$IDADE <- as.numeric(obt22$IDADEanos)

obt22$IDADE[which(!is.na(obt22$IDADEminutos))]

obt22$IDADE[which(!is.na(obt22$IDADEminutos))] <- 0

obt22$IDADE[which(!is.na(obt22$IDADEhoras))] <- 0

obt22$IDADE[which(!is.na(obt22$IDADEdias))] <- 0

obt22$IDADE[which(!is.na(obt22$IDADEmeses))] <- 0

# 3. Aquisição de dados da Pesquisa Nacional de Saúde (PNS) ----

## 3.1. Instalar pacotes ----

install.packages("PNSIBGE")

## 3.2. Carregar os pacotes ----

library(PNSIBGE)

## 3.3. Aquisição dos dados ----

### 3.3.1. O comando get_pns ----

#### 3.3.1.1. Adquirindo dados de 2019, dos questionários de domicílios e residentes, com a legenda das categorias e sem o desenho amostral incorporado ----

pns19 <- get_pns(year = 2019,labels = T,design = F)

#### 3.3.1.2. Verificando os dados ----

is(pns19)

table(pns19$M001, useNA = "a")

table(pns19$Q03001, useNA = "a")

#### 3.3.2.1. Adquirindo dados de 2019, dos questionários de selecionados, com a legenda das categorias e com o desenho amostral incorporado ----

pns19_sele <- get_pns(year = 2019,labels = T,design = T,selected = T)

#### 3.3.2.2. Verificando os dados ----

is(pns19_sele)

table(pns19_sele$variables$M001, useNA = "a")

table(pns19_sele$variables$Q03001, useNA = "a")

library(survey)

svytotal(~Q03001,design = pns19_sele,na.rm = T)

svymean(~Q03001,design = pns19_sele,na.rm = T)

svytable(~C006+Q03001,design = pns19_sele)

plot(svytable(~C006+Q03001,design = pns19_sele))

# 4. Aquisição de dados da PNAD Contínua ----

## 4.1. Instalar pacotes ----

install.packages("PNADcIBGE")

## 4.2. Carregar os pacotes ----

library(PNADcIBGE)

## 4.3. Aquisição dos dados ----

### 4.3.1. O comando get_pnadc ----

#### 4.3.1.1. Adquirindo dados do primeiro trimestre de 2024, com o desenho amostral incorporado e sel a legenda ----

pnadc2024 <- get_pnadc(year = 2024,quarter = 1,design = F,labels = F)

#### 4.3.1.2. Verificando os dados ----

is(pnadc2024)

table(pnadc2024$V3001, useNA = "a")

table(pnadc2024$V4001, useNA = "a")

summarytools::ctable(pnadc2024$V3001,pnadc2024$V4001,prop = "r",chisq = T)

#### 4.3.2.1. Adquirindo dados do primeiro trimestre de 2024, com a legenda das categorias e com o desenho amostral incorporado ----

pnadc2024d <- get_pnadc(year = 2024,quarter = 1, labels = T,design = T)

#### 4.3.2.2. Verificando os dados ----

is(pnadc2024d)

table(pnadc2024d$variables$V3001, useNA = "a")

table(pnadc2024d$variables$V4001, useNA = "a")

svytotal(~V3001,design = pnadc2024d, na.rm = T)

svymean(~V3001,design = pnadc2024d, na.rm = T)

svytable(~V3001+V4001,design = pnadc2024d)

plot(svytable(~V3001+V4001,design = pnadc2024d))

svychisq(~V3001+V4001,design = pnadc2024d)

# 5. Aquisição de dados da PNAD COVID19 ----

## 5.1. Instalar pacotes ----

install.packages("COVIDIBGE")

## 5.2. Carregar os pacotes ----

library(COVIDIBGE)

## 5.3. Aquisição dos dados ----

### 5.3.1. O comando get_covid ----

#### 5.3.1.1. Adquirindo dados de novembro de 2020, sem o desenho amostral incorporado ----

pcov11 <- get_covid(year = 2020,month = 11, labels = T,design = F)

#### 5.3.1.2. Verificando os dados ----

is(pcov11)

table(pcov11$B007, useNA = "a")

table(pcov11$B008, useNA = "a")

summarytools::ctable(pcov11$B007,pcov11$B008,prop = "r",chisq = T)

#### 5.3.2.1. Adquirindo dados de novembro de 2020, com a legenda das categorias e com o desenho amostral incorporado ----

pcov11d <- get_covid(year = 2020,month = 11, labels = T,design = T)

#### 5.3.2.2. Verificando os dados ----

is(pcov11d)

table(pcov11d$variables$B007, useNA = "a")

table(pcov11d$variables$B008, useNA = "a")

svytotal(~B007,design = pcov11d)

svymean(~B007,design = pcov11d)

svytable(~B007+B008,design = pcov11d)

plot(svytable(~B007+B008,design = pcov11d))

svychisq(~B007+B008,design = pcov11d)

# 6. Aquisição de dados do IPEADATA ----

## 6.1. Instalar pacotes ----

install.packages("ipeadatar")

## 6.2. Carregar os pacotes ----

library(ipeadatar)

## 6.3. Aquisição dos dados ----

### 6.3.1. Alguns comandos de interesse ----

available_countries()

available_series()

available_subjects()

available_territories()

### 6.3.2. Identificando as séries históricas de interesse ----

# Ver todas as séries disponíveis

series_disponiveis <- available_series()

series_disponiveis

# Busca pelo termo desemprego

desemprego <- search_series(terms = "Taxa de desemprego",fields = "name")

desemprego

grep("mulher",desemprego$name)

desemprego$code[27]

### 6.3.3. Verificando os metadados da série ----

metadados_desemp_mulher <- metadata(desemprego$code[27])

metadados_desemp_mulher$code

metadados_desemp_mulher$comment

### 6.3.4. Aquisição dos dados acerca do desemrpego em mulheres ----

desemp_mulher <- ipeadata("PNADCT_TXDSCUPUF_MUL")

desemp_mulher

table(desemp_mulher$uname)

#### 6.3.4.1. Refinando a aquisição: dados do Brasil ----

desemp_mulher_br <- ipeadata("PNADCT_TXDSCUPUF_MUL") %>% filter(uname=="Brazil")

# OU

desemp_mulher_br <- ipeadata("PNADCT_TXDSCUPUF_MUL")

desemp_mulher_br <- desemp_mulher_br[which(desemp_mulher_br$uname=="Brazil"),]

plot(desemp_homem_br$date,desemp_homem_br$value,type = "l")

#### 6.3.4.2. Refinando a aquisição: dados do Brasil pré e pós pandemia ----

desemp_mulher_br <- ipeadata("PNADCT_TXDSCUPUF_MUL") %>% filter(uname=="Brazil" & as.numeric(substr(date,1,4)) >= 2020)

desemp_mulher_br

plot(desemp_mulher_br$date,desemp_mulher_br$value,type = "l")