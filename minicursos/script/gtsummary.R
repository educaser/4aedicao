# ---- EDUCA_SER 2024 ---- #
### ---- Pacote gtsummary ---- ###

# Carrega as bibliotecas necessárias para o código ----
library(gtsummary)  # Para criar tabelas bonitas e resumidas
library(titanic)     # Contém o conjunto de dados do Titanic
library(dplyr)       # Para manipulação de dados

# Carrega o conjunto de dados do Titanic para a variável 'dados' ----
dados <- titanic_train

# Remove colunas desnecessárias da variável 'dados' ----
dados$PassengerId <- NULL  
dados$Name <- NULL          
dados$Ticket <- NULL       
dados$Cabin <- NULL         

# Converte as variáveis 'Survived', 'Pclass', 'Sex' e 'Embarked' para fatores ----
dados$Survived <- as.factor(dados$Survived)  
dados$Pclass <- as.factor(dados$Pclass)      
dados$Sex <- as.factor(dados$Sex)            
dados$Embarked <- as.factor(dados$Embarked)  

# Altera o primeiro nível da variável 'Embarked' para NA (ausente) ----
levels(dados$Embarked)[1] <- NA  

# Cofigurações da tabela ----
# https://www.danieldsjoberg.com/gtsummary/reference/theme_gtsummary.html
# Estilos: "jama", "lancet", "nejm", "qjecon"
# Carregando a função theme_gtsummary_journal do pacote gtsummary
# Aplica o tema "jama", ajustando a formatação da tabela ao estilo da revista médica JAMA
theme_gtsummary_journal("jama")

# Configurando o idioma e a formatação numérica para Português com theme_gtsummary_language
theme_gtsummary_language(
  language = "pt",       # Define o idioma para Português
  decimal.mark = ",",    # Define a vírgula como separador decimal
  big.mark = ".",        # Define o ponto como separador de milhares
  iqr.sep = "-",         # Define o hífen como separador para intervalos interquartis
  ci.sep = "-",          # Define o hífen como separador para intervalos de confiança
  set_theme = TRUE       # Aplica essas configurações como tema padrão para as tabelas
)

# Linha comentada que, se descomentada, resetaria todas as configurações do tema para os padrões da gtsummary
# reset_gtsummary_theme()

# Customizando a função para formatação de porcentagens e aplicando ao tema
list("tbl_summary-fn:percent_fun" = function(x) sprintf(x * 100, fmt='%#.1f')) %>%
  set_gtsummary_theme()  # Aplica a função personalizada de formatação de porcentagens como tema padrão

