# ---- EDUCA_SER 2024 ---- #
### ---- Pacote gtsummary ---- ###

# install.packages(c("gtsummary", "flextable", "titanic", "dplyr"))

# Carrega as bibliotecas necessárias para o código
library(gtsummary)  # Para criar tabelas bonitas e resumidas
library(titanic)     # Contém o conjunto de dados do Titanic
library(dplyr)       # Para manipulação de dados

# Carrega o conjunto de dados do Titanic para a variável 'dados'
dados <- titanic_train

# Remove colunas desnecessárias do banco 'dados'
dados$PassengerId <- NULL
dados$Name <- NULL
dados$Ticket <- NULL
dados$Cabin <- NULL

str(dados)

# Converte as variáveis 'Survived', 'Pclass', 'Sex' e 'Embarked' para fatores
dados$Survived <- as.factor(dados$Survived)
dados$Pclass <- as.factor(dados$Pclass)
dados$Sex <- as.factor(dados$Sex)
dados$Embarked <- as.factor(dados$Embarked)

# Altera o primeiro nível da variável 'Embarked' para NA (ausente)
levels(dados$Embarked)[1] <- NA
#------------------------------------------------#

#1 Cria uma tabela de resumo para o conjunto de dados 'dados'
dados %>% tbl_summary()

# https://www.danieldsjoberg.com/gtsummary/reference/theme_gtsummary.html
# Estilos: "jama", "lancet", "nejm", "qjecon"
theme_gtsummary_journal("jama")

# estilos por conta do idioma
theme_gtsummary_language(
  language = "pt",
  decimal.mark = ",",
  big.mark = ".",
  iqr.sep = "-",
  ci.sep = "-",
  set_theme = TRUE
)

# Caso queira resetar os parâmetros
# reset_gtsummary_theme()

tabela <- dados %>%
  # Transformar os níveis das variáveis 'Pclass', 'Survived' e 'Sex' em fatores com rótulos personalizados.
  mutate(
    Pclass = factor(Pclass, labels = c("Primeira", "Segunda", "Terceira")),
    Survived = factor(Survived, labels = c("Não", "Sim")),
    Sex = factor(Sex, labels = c("Feminino", "Masculino"))
  ) %>%
  # Criar a tabela de resumo usando a função 'tbl_summary'
  tbl_summary(
    # Agrupar a tabela pelo desfecho 'Survived'
    by = Survived,
    # Definir tipos específicos para variáveis: 'Sex' como dicotômica, 'SibSp' e 'Parch' como contínuas
    type = list(Sex ~ "dichotomous",
                SibSp ~ "continuous2",
                Parch ~ "continuous"),
    # Definir valor de referência para 'Sex'
    value = list(Sex ~ "Masculino"),
    # Definir rótulos personalizados para as variáveis
    label = list(Pclass ~ "Classe econômica",
                 Sex ~ "Sexo masculino",
                 Age ~ "Idade",
                 SibSp ~ "No. de irmãos/cônjuges",
                 Parch ~ "No. de pais/filhos",
                 Fare ~ "Tarifa",
                 Embarked ~ "Porto de embarque"),
    # Definir o formato das estatísticas para variáveis contínuas e categóricas
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",  # Média (Desvio Padrão) para contínuas
      all_categorical() ~ "{n} ({p}%)"     # Contagem (Porcentagem) para categóricas
    ),
    # Definir o número de dígitos para variáveis contínuas e categóricas
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ 1),
    # Mostrar dados ausentes apenas se houver
    missing = "ifany",
    # Texto para células com dados ausentes
    missing_text = "Desconhecido",
    # Calcular porcentagens por linha
    percent = "row"
  ) %>% 
  # Adicionar testes de significância
  add_p(
    test = list(Age ~ "t.test"),  # Usar teste t para a variável 'Age'
    # Formatar valores-p com 2 dígitos
    pvalue_fun = function(x)
      style_pvalue(x, digits = 2)
  ) %>% 
  # Adicionar coluna com total de observações
  add_overall(col_label = "**Total**, N = {N}") %>%
  # Modificar título da tabela
  modify_caption("**Tabela 1**. Características dos passageiros a bordo do Titanic segundo o desfecho") %>%
  # Modificar rótulos do cabeçalho da tabela
  modify_header(
    label ~ "**Variáveis**",
    stat_0 = '**Geral**, N = 891',
    stat_1 = '**Mortos**, N = 549',
    stat_2 = '**Sobreviventes**, N = 342',
    p.value = "**p**"
  ) %>%
  # Combinar células do cabeçalho para grupos 'Mortos' e 'Sobreviventes'
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Desfecho**") %>%
  # Adicionar notas de rodapé
  modify_footnote(
    all_stat_cols() ~ "Média (DP) ou Frequência (%)",
    p.value ~ "Teste qui-quadrado de Pearson, Teste t com Welch, Teste de Mann-Whitney"
  ) %>%
  # Tornar rótulos em negrito
  bold_labels() %>%
  # Tornar níveis das variáveis em itálico
  italicize_levels()

# Exibir a tabela
tabela

# Exportando a tabela
library(flextable)

tabela %>% as_flex_table() %>%
  save_as_docx(path = "Tabela1.docx")

# Explicação Detalhada
   
# mutate(...): Esta função transforma variáveis em fatores com rótulos mais descritivos. Pclass é rotulado como "Primeira", "Segunda", "Terceira"; Survived como "Não", "Sim"; e Sex como "Feminino", "Masculino".
# tbl_summary(...): Esta função cria a tabela de resumo.
# by = Survived: Agrupa os dados pelo desfecho Survived.
# type = list(...): Define tipos de variáveis, como dicotômica ou contínua.
# value = list(...): Define valores de referência.
# label = list(...): Define rótulos personalizados.
# statistic = list(...): Define o formato das estatísticas para variáveis contínuas e categóricas.
# digits = list(...): Define o número de dígitos nas estatísticas.
# missing = "ifany" e missing_text = "Desconhecido": Configura a exibição de dados ausentes.
# percent = "row": Calcula porcentagens por linha.
# add_p(...): Adiciona valores-p à tabela usando o teste t para a variável Age e formata os valores-p com 2 dígitos.
# add_overall(...): Adiciona uma coluna mostrando o total de observações.
# modify_caption(...): Modifica o título da tabela.
# modify_header(...): Modifica os rótulos do cabeçalho.
# modify_spanning_header(...): Combina células do cabeçalho para grupos 'Mortos' e 'Sobreviventes'.
# modify_footnote(...): Adiciona notas de rodapé explicando as estatísticas e testes usados.
# bold_labels(): Torna os rótulos em negrito.
# italicize_levels(): Torna os níveis das variáveis em itálico.
