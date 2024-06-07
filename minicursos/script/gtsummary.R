# ---- EDUCA_SER 2024 ---- #
### ---- Pacote gtsummary ---- ###

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

# Converte as variáveis 'Survived', 'Pclass', 'Sex' e 'Embarked' para fatores
dados$Survived <- as.factor(dados$Survived)
dados$Pclass <- as.factor(dados$Pclass)
dados$Sex <- as.factor(dados$Sex)
dados$Embarked <- as.factor(dados$Embarked)

# Altera o primeiro nível da variável 'Embarked' para NA (ausente)
levels(dados$Embarked)[1] <- NA

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

#2 Cria uma tabela de resumo para o conjunto de dados 'dados' com configurações personalizadas
dados %>%
  # Muta os níveis da variável Pclass
  mutate(Pclass = factor(Pclass, labels = c("Primeira", "Segunda", "Terceira"))) %>%
  tbl_summary(
    # Agrupa a tabela de resumo por 'Survived' (Sobreviveu ou não)
    by = Survived,
    # Define o tipo das variáveis: 'Sex' como dicotômica, 'SibSp' como contínua e 'Parch' como contínua
    type = list(Sex ~ "dichotomous",
                SibSp ~ "continuous2",
                Parch ~ "continuous"),
    # Define os valores para a variável 'Sex' como 'male'
    value = list(Sex ~ "male"),
    # Define o formato das estatísticas para variáveis contínuas e categóricas
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      # Para variáveis contínuas: média (desvio padrão)
      all_categorical() ~ "{n} ({p}%)"     # Para variáveis categóricas: contagem (porcentagem)
    ),
    # Define o número de dígitos para variáveis contínuas e categóricas
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ 1),
    # Define rótulos personalizados para as variáveis 'Pclass' e 'Sex'
    label = list(Pclass ~ "Classe econômica",
                 Sex ~ "Sexo"),
    # Indica que as células com dados ausentes devem ser apresentadas apenas quando houver
    missing = "ifany",
    # Indica que as células com dados ausentes devem ser marcadas como "Não informado"
    missing_text = "Não informado",
    # Calcula as porcentagens por linha na tabela
    percent = "row"
  )

#3 Cria uma tabela de resumo para o conjunto de dados 'dados' com configurações personalizadas
dados %>%
  mutate(Pclass = factor(Pclass, labels = c("Primeira", "Segunda", "Terceira"))) %>%
  tbl_summary(
    by = Survived,
    type = list(SibSp ~ "continuous",
                Parch ~ "continuous"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ 1),
    label = list(Pclass ~ "Classe econômica",
                 Sex ~ "Sexo"),
    missing = "no",
    missing_text = "Não informado",
    percent = "row"
  ) %>%
  # Adiciona testes de hipóteses, especificando para a variável 'Age' como o teste t
  add_p(
    test = list(Age ~ "t.test"),
    # Formata os valores-p com 2 dígitos
    pvalue_fun = function(x)
      style_pvalue(x, digits = 2)
  ) %>%
  # Adiciona um rótulo geral indicando o número total de observações (N)
  add_overall(col_label = "**Total**, N = {N}") %>%
  # Adiciona o número total de observações na tabela de resumo
  add_n() %>%
  # Adiciona rótulos específicos para as estatísticas das variáveis categóricas e contínuas
  add_stat_label(label = list(all_categorical() ~ "No. (%)",
                              all_continuous() ~ "Média (DP)"))

#4 Cria uma tabela de resumo para o conjunto de dados 'dados' com configurações personalizadas
tab <- dados %>%
  mutate(Pclass = factor(Pclass, labels = c("Primeira", "Segunda", "Terceira"))) %>%
  tbl_summary(
    by = Survived,
    type = list(SibSp ~ "continuous",
                Parch ~ "continuous"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ 1),
    label = list(Pclass ~ "Classe econômica",
                 Sex ~ "Sexo"),
    missing = "no",
    missing_text = "Não informado",
    percent = "row"
  ) %>%
  add_p(
    test = list(Age ~ "t.test"),
    pvalue_fun = function(x)
      style_pvalue(x, digits = 2)
  ) %>%
  add_overall(col_label = "**Total**, N = {N}") %>%
  add_n() %>%
  # Modifica o título da tabela
  modify_caption("**Tabela 1. Características dos passageiros a bordo do Titanic**") %>%
  # Modifica os rótulos do cabeçalho da tabela
  modify_header(
    label ~ "**Variáveis**",
    stat_1 = '**Mortos**, N = 549',
    stat_2 = '**Sobreviventes**, N = 342',
    p.value = "**p**"
  ) %>%
  # Combina as células do cabeçalho correspondentes aos grupos 'Mortos' e 'Sobreviventes'
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Desfecho**") %>%
  # Adiciona notas de rodapé à tabela
  modify_footnote(
    all_stat_cols() ~ "Média (DP) ou Frequência (%)",
    p.value ~ "Teste qui-quadrado de Pearson, Teste t com Welch, Teste de Mann-Whitney"
  ) %>%
  # Torna os rótulos em negrito
  bold_labels() %>%
  # Torna os níveis das variáveis em itálico
  italicize_levels()

# Exportando a tabela
library(flextable)

tab %>% as_flex_table() %>%
  save_as_docx(path = "Tabela1.docx")
