##################################
##### BIBLIOTECAS UTILIZADAS #####
##################################

# Fornece uma gramática de manipulação de dados para selecionar, 
# filtrar, agrupar, agregar e combinar dados. 
install.packages("dplyr")

# Oferece funções para a transformação e reorganização de dados.
install.packages("tidyr")

# Biblioteca de visualização que permite criar gráficos 
# estatísticos de alta qualidade.
install.packages("ggplot2")


# Biblioteca de manipulação de dados que oferece operações 
# rápidas em grandes conjuntos de dados.
install.packages("data.table")

# Biblioteca para importação de dados oriundos de arquivos *.xlsx
install.packages("readxl")


# Carregando as bibliotecas
library(dplyr)
library(tidyr)
library(ggplot2)
#library(reshape2)
library(data.table)
library(scales)
library(readxl)




#################################
########  BASES DE DADOS  #######
#################################

#rm(Mortalidade_Geral_2021)

Mortalidade_Geral_2017 <- read.csv2('Mortalidade_Geral_2017.csv')

Mortalidade_Geral_2018 <- read.csv2('Mortalidade_Geral_2018.csv')

Mortalidade_Geral_2019 <- read.csv2('Mortalidade_Geral_2019.csv')

Mortalidade_Geral_2020 <- read.csv2('Mortalidade_Geral_2020.csv')

Mortalidade_Geral_2021 <- read.csv2('Mortalidade_Geral_2021.csv')

Mortalidade_Geral_2022 <- read.csv2('Mortalidade_Geral_2022.csv')


DEPARA_GRUPO_TRABALHO <- read_excel('DEPARA_GRUPO_TRABALHO.xlsx')

DEPARA_TRABALHOS <- read_excel('DEPARA_TRABALHOS.xlsx')


#################################
####### ANÁLISE DESCRITIVA ######
#################################

# Cria parâmetro para passar a o CSV a ser utilizado
Base_Mortalidade <- Mortalidade_Geral_2022

# Visualiza a tabela e os tipos
View(Base_Mortalidade)
glimpse(Base_Mortalidade)

# Exibir informações sobre o dataframe
#str(Base_Mortalidade)

# Estatísticas básicas de cada campo
summary(Base_Mortalidade)

# Contagem de registros não nulos por coluna
qtd_nao_null <- summarise(Base_Mortalidade,
                          across(everything(), ~ sum(!is.na(.))))%>%
                pivot_longer(everything(), names_to = "nome_campo", values_to = "qtd_nao_nulo")

print(n=100, qtd_nao_null)

#dropa variável
#rm(qtd_nao_null) 


# Tratamento de idades - Criação dos campos TIPO e GRUPO de idade

# 2017
Mortalidade_Geral_2017 <- mutate(Mortalidade_Geral_2017
                                 , IDADE_CORR = ifelse(substr(IDADE, start = 1, stop = 1) == "5",  100 + as.numeric(substr(IDADE, start = 2, stop = 3)), as.numeric(substr(IDADE, start = 2, stop = 3)))
                                 , TIPO_IDADE = case_when(
                                   substr(IDADE, start = 1, stop = 1) == "1" ~ "minutos",
                                   substr(IDADE, start = 1, stop = 1) == "2" ~ "horas",
                                   substr(IDADE, start = 1, stop = 1) == "3" ~ "meses",
                                   substr(IDADE, start = 1, stop = 1) == "4" ~ "anos",
                                   substr(IDADE, start = 1, stop = 1) == "5" ~ "+100 anos",
                                   TRUE ~ "outro"
                                 )
                                 , GRUPO_IDADE = case_when(
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 01 & IDADE_CORR < 10 ~ "01-09",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 10 & IDADE_CORR < 20 ~ "10-19",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 20 & IDADE_CORR < 30 ~ "20-29",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 30 & IDADE_CORR < 40 ~ "30-39",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 40 & IDADE_CORR < 50 ~ "40-49",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 50 & IDADE_CORR < 60 ~ "50-59",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 60 & IDADE_CORR < 70 ~ "60-69",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 70 & IDADE_CORR < 80 ~ "70-79",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 80 & IDADE_CORR < 90 ~ "80-89",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 90 & IDADE_CORR < 100 ~ "90-99",
                                   TIPO_IDADE == "+100 anos" ~ "+100 anos",
                                   TRUE ~ "Outros"
                                 )
)


# 2018
Mortalidade_Geral_2018 <- mutate(Mortalidade_Geral_2018
                                 , IDADE_CORR = ifelse(substr(IDADE, start = 1, stop = 1) == "5",  100 + as.numeric(substr(IDADE, start = 2, stop = 3)), as.numeric(substr(IDADE, start = 2, stop = 3)))
                                 , TIPO_IDADE = case_when(
                                   substr(IDADE, start = 1, stop = 1) == "1" ~ "minutos",
                                   substr(IDADE, start = 1, stop = 1) == "2" ~ "horas",
                                   substr(IDADE, start = 1, stop = 1) == "3" ~ "meses",
                                   substr(IDADE, start = 1, stop = 1) == "4" ~ "anos",
                                   substr(IDADE, start = 1, stop = 1) == "5" ~ "+100 anos",
                                   TRUE ~ "outro"
                                 )
                                 , GRUPO_IDADE = case_when(
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 01 & IDADE_CORR < 10 ~ "01-09",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 10 & IDADE_CORR < 20 ~ "10-19",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 20 & IDADE_CORR < 30 ~ "20-29",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 30 & IDADE_CORR < 40 ~ "30-39",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 40 & IDADE_CORR < 50 ~ "40-49",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 50 & IDADE_CORR < 60 ~ "50-59",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 60 & IDADE_CORR < 70 ~ "60-69",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 70 & IDADE_CORR < 80 ~ "70-79",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 80 & IDADE_CORR < 90 ~ "80-89",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 90 & IDADE_CORR < 100 ~ "90-99",
                                   TIPO_IDADE == "+100 anos" ~ "+100 anos",
                                   TRUE ~ "Outros"
                                 )
)

# 2019
Mortalidade_Geral_2019 <- mutate(Mortalidade_Geral_2019
                                 , IDADE_CORR = ifelse(substr(IDADE, start = 1, stop = 1) == "5",  100 + as.numeric(substr(IDADE, start = 2, stop = 3)), as.numeric(substr(IDADE, start = 2, stop = 3)))
                                 , TIPO_IDADE = case_when(
                                   substr(IDADE, start = 1, stop = 1) == "1" ~ "minutos",
                                   substr(IDADE, start = 1, stop = 1) == "2" ~ "horas",
                                   substr(IDADE, start = 1, stop = 1) == "3" ~ "meses",
                                   substr(IDADE, start = 1, stop = 1) == "4" ~ "anos",
                                   substr(IDADE, start = 1, stop = 1) == "5" ~ "+100 anos",
                                   TRUE ~ "outro"
                                 )
                                 , GRUPO_IDADE = case_when(
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 01 & IDADE_CORR < 10 ~ "01-09",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 10 & IDADE_CORR < 20 ~ "10-19",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 20 & IDADE_CORR < 30 ~ "20-29",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 30 & IDADE_CORR < 40 ~ "30-39",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 40 & IDADE_CORR < 50 ~ "40-49",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 50 & IDADE_CORR < 60 ~ "50-59",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 60 & IDADE_CORR < 70 ~ "60-69",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 70 & IDADE_CORR < 80 ~ "70-79",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 80 & IDADE_CORR < 90 ~ "80-89",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 90 & IDADE_CORR < 100 ~ "90-99",
                                   TIPO_IDADE == "+100 anos" ~ "+100 anos",
                                   TRUE ~ "Outros"
                                 )
)

# 2020
Mortalidade_Geral_2020 <- mutate(Mortalidade_Geral_2020
                                 , IDADE_CORR = ifelse(substr(IDADE, start = 1, stop = 1) == "5",  100 + as.numeric(substr(IDADE, start = 2, stop = 3)), as.numeric(substr(IDADE, start = 2, stop = 3)))
                                 , TIPO_IDADE = case_when(
                                   substr(IDADE, start = 1, stop = 1) == "1" ~ "minutos",
                                   substr(IDADE, start = 1, stop = 1) == "2" ~ "horas",
                                   substr(IDADE, start = 1, stop = 1) == "3" ~ "meses",
                                   substr(IDADE, start = 1, stop = 1) == "4" ~ "anos",
                                   substr(IDADE, start = 1, stop = 1) == "5" ~ "+100 anos",
                                   TRUE ~ "outro"
                                 )
                                 , GRUPO_IDADE = case_when(
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 01 & IDADE_CORR < 10 ~ "01-09",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 10 & IDADE_CORR < 20 ~ "10-19",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 20 & IDADE_CORR < 30 ~ "20-29",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 30 & IDADE_CORR < 40 ~ "30-39",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 40 & IDADE_CORR < 50 ~ "40-49",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 50 & IDADE_CORR < 60 ~ "50-59",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 60 & IDADE_CORR < 70 ~ "60-69",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 70 & IDADE_CORR < 80 ~ "70-79",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 80 & IDADE_CORR < 90 ~ "80-89",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 90 & IDADE_CORR < 100 ~ "90-99",
                                   TIPO_IDADE == "+100 anos" ~ "+100 anos",
                                   TRUE ~ "Outros"
                                 )
)

# 2021
Mortalidade_Geral_2021 <- mutate(Mortalidade_Geral_2021
                                 , IDADE_CORR = ifelse(substr(IDADE, start = 1, stop = 1) == "5",  100 + as.numeric(substr(IDADE, start = 2, stop = 3)), as.numeric(substr(IDADE, start = 2, stop = 3)))
                                 , TIPO_IDADE = case_when(
                                   substr(IDADE, start = 1, stop = 1) == "1" ~ "minutos",
                                   substr(IDADE, start = 1, stop = 1) == "2" ~ "horas",
                                   substr(IDADE, start = 1, stop = 1) == "3" ~ "meses",
                                   substr(IDADE, start = 1, stop = 1) == "4" ~ "anos",
                                   substr(IDADE, start = 1, stop = 1) == "5" ~ "+100 anos",
                                   TRUE ~ "outro"
                                 )
                                 , GRUPO_IDADE = case_when(
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 01 & IDADE_CORR < 10 ~ "01-09",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 10 & IDADE_CORR < 20 ~ "10-19",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 20 & IDADE_CORR < 30 ~ "20-29",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 30 & IDADE_CORR < 40 ~ "30-39",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 40 & IDADE_CORR < 50 ~ "40-49",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 50 & IDADE_CORR < 60 ~ "50-59",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 60 & IDADE_CORR < 70 ~ "60-69",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 70 & IDADE_CORR < 80 ~ "70-79",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 80 & IDADE_CORR < 90 ~ "80-89",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 90 & IDADE_CORR < 100 ~ "90-99",
                                   TIPO_IDADE == "+100 anos" ~ "+100 anos",
                                   TRUE ~ "Outros"
                                 )
)


# 2022
Mortalidade_Geral_2022 <- mutate(Mortalidade_Geral_2022
                                 , IDADE_CORR = ifelse(substr(IDADE, start = 1, stop = 1) == "5",  100 + as.numeric(substr(IDADE, start = 2, stop = 3)), as.numeric(substr(IDADE, start = 2, stop = 3)))
                                 , TIPO_IDADE = case_when(
                                   substr(IDADE, start = 1, stop = 1) == "1" ~ "minutos",
                                   substr(IDADE, start = 1, stop = 1) == "2" ~ "horas",
                                   substr(IDADE, start = 1, stop = 1) == "3" ~ "meses",
                                   substr(IDADE, start = 1, stop = 1) == "4" ~ "anos",
                                   substr(IDADE, start = 1, stop = 1) == "5" ~ "+100 anos",
                                   TRUE ~ "outro"
                                 )
                                 , GRUPO_IDADE = case_when(
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 01 & IDADE_CORR < 10 ~ "01-09",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 10 & IDADE_CORR < 20 ~ "10-19",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 20 & IDADE_CORR < 30 ~ "20-29",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 30 & IDADE_CORR < 40 ~ "30-39",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 40 & IDADE_CORR < 50 ~ "40-49",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 50 & IDADE_CORR < 60 ~ "50-59",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 60 & IDADE_CORR < 70 ~ "60-69",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 70 & IDADE_CORR < 80 ~ "70-79",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 80 & IDADE_CORR < 90 ~ "80-89",
                                   TIPO_IDADE == "anos" & IDADE_CORR >= 90 & IDADE_CORR < 100 ~ "90-99",
                                   TIPO_IDADE == "+100 anos" ~ "+100 anos",
                                   TRUE ~ "Outros"
                                 )
)


# Criando campo para identificação do registro do CSV
#Mortalidade_Geral_2018 <- Mortalidade_Geral_2018 %>% mutate(ANO_REGISTRO_CSV = 2018)
#Mortalidade_Geral_2019 <- Mortalidade_Geral_2019 %>% mutate(ANO_REGISTRO_CSV = 2019)
#Mortalidade_Geral_2020 <- Mortalidade_Geral_2020 %>% mutate(ANO_REGISTRO_CSV = 2020)
#Mortalidade_Geral_2021 <- Mortalidade_Geral_2021 %>% mutate(ANO_REGISTRO_CSV = 2021)

# Junção dos dados 2018 - 2021
#Base_Mortalidade_Geral <- bind_rows(Mortalidade_Geral_2018, Mortalidade_Geral_2019, Mortalidade_Geral_2020, Mortalidade_Geral_2021) 

# Código verificação último erro
#rlang::last_trace(drop = FALSE)


# Contagem óbitos por ano
QTD_REG <- data.frame(
                      ano = c(2017, 2018, 2019, 2020, 2021, 2022)
                      , qtd = c(nrow(Mortalidade_Geral_2017),nrow(Mortalidade_Geral_2018), nrow(Mortalidade_Geral_2019), nrow(Mortalidade_Geral_2020), nrow(Mortalidade_Geral_2021), nrow(Mortalidade_Geral_2022))
                    )
print(QTD_REG)

# Definir cores aleatórias para cada ano
#cores <- rainbow(length(QTD_REG$ano))

# Verificação da variação em quantidade por ano
ggplot(QTD_REG, aes(x = as.factor(ano), y = qtd)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = comma(qtd)), vjust = -0.5, size = 3, fontface = "bold") +
  labs(x = "Ano", y = "Quantidade de Óbitos", title = "Registros óbitos por Ano") +
  theme_minimal()






