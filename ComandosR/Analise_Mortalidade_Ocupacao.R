# FREQ ACIDENTES TRABALHO

#ACIDTRAB:
#   Indica se o evento que desencadeou o óbito está relacionado ao processo de trabalho:
#     1 – sim; 
#     2 – não; 
#     9 – ignorado

#OCUP
#   Tipo de trabalho que o falecido desenvolveu na maior parte de sua vida produtiva. 
#   Preenchimento de acordo com Classificação Brasileira de Ocupações – CBO 2002. (Números)





###############################################################################
###############################  Avaliação geral ############################## 
###############################################################################

# Macro para ajustar os grupos de ocupação
ADC_GRP_TRAB <- function(Base_Mortalidade, Data_reg) {
  # Join Mortalidade "OCUP" com DEPARA_GRUPO_TRABALHO
  Novo_dataframe <- left_join(Base_Mortalidade, DEPARA_GRUPO_TRABALHO, by = "OCUP")
  
  # Criando novo campo ajustado de OCUP
  Novo_dataframe <- Novo_dataframe %>%
    mutate(
      GRP_OCUP_CORR = case_when(
        is.na(GRP_OCUP) & is.na(OCUP) ~ GRP_OCUP,
        is.na(GRP_OCUP) & !is.na(OCUP) ~ "OCUPAÇÃO NÃO IDENTIFICADA",
        GRP_OCUP == "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS" ~ "PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS",
        GRP_OCUP == "TRABALHADORES AGROPECUÁRIOS, FLORESTAIS E DA PESCA" ~ "AGROPECUÁRIOS, FLORESTAIS E DA PESCA",
        GRP_OCUP == "TRABALHADORES DOS SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS" ~ "SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS",
        GRP_OCUP == "TÉCNICOS DE NIVEL MÉDIO" ~ "TÉCNICOS DE NIVEL MÉDIO",
        GRP_OCUP == "TRABALHADORES EM SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO" ~ "SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO",
        GRP_OCUP == "MEMBROS SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE ORGANIZAÇÕES DE INTERESSE PÚBLICO E DE EMPRESAS, GERENTES" ~ "SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE EMPRESAS",
        GRP_OCUP == "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES" ~ "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES",
        GRP_OCUP == "TRABALHADORES DE SERVIÇOS ADMINISTRATIVOS" ~ "SERVIÇOS ADMINISTRATIVOS",
        GRP_OCUP == "MEMBROS DAS FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES" ~ "FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES"
      )
      , ANO_REGISTRO_CSV = Data_reg
    )
  
  # Filtro para fazer a contagem de registros por ano
  OCUP_OBITO <- Novo_dataframe %>%
    filter(!is.na(GRP_OCUP_CORR)) %>%
    group_by(ANO_REGISTRO_CSV) %>%
    count() %>%
    arrange(desc(n))

  return(OCUP_OBITO)
}

# Utilizando a macro para cada ano
COUNT_OCUP_2017 <- ADC_GRP_TRAB(Mortalidade_Geral_2017, 2017)
COUNT_OCUP_2018 <- ADC_GRP_TRAB(Mortalidade_Geral_2018, 2018)
COUNT_OCUP_2019 <- ADC_GRP_TRAB(Mortalidade_Geral_2019, 2019)
COUNT_OCUP_2020 <- ADC_GRP_TRAB(Mortalidade_Geral_2020, 2020)
COUNT_OCUP_2021 <- ADC_GRP_TRAB(Mortalidade_Geral_2021, 2021)
COUNT_OCUP_2022 <- ADC_GRP_TRAB(Mortalidade_Geral_2022, 2022)

# Agrupando resultados da macro
COUNT_OCUP <- bind_rows(
                            COUNT_OCUP_2017
                            , COUNT_OCUP_2018
                            , COUNT_OCUP_2019
                            , COUNT_OCUP_2020
                            , COUNT_OCUP_2021
                            , COUNT_OCUP_2022
                          )

print(COUNT_OCUP)

# Gráfico da variação na qtd de óbitos de pessoas com ocupação por ano
ggplot(COUNT_OCUP, aes(x = ANO_REGISTRO_CSV, y = n, fill = "Total")) +
  geom_col() +
  geom_text(aes(label = comma(n)), vjust = -0.5, size = 3, fontface = "bold") +
  labs(x = "Ano", y = "Quantidade óbitos com ocupação", title = "Variação da quantidade de óbitos por ano") +
  theme_minimal() +
  scale_fill_manual(values = "steelblue") +
  scale_y_continuous(labels = scales::comma)



###############################################################################
########################## Avaliação geral por grupo ##########################  
###############################################################################


# Macro para contagem de óbitos por grupo de ocupação
OBITO_OCUP <- function(Base_Mortalidade, Data_reg) {

  # Join Mortalidade "OCUP" com DEPARA_GRUPO_TRABALHO
  Novo_dataframe <- left_join(Base_Mortalidade, DEPARA_GRUPO_TRABALHO, by = "OCUP")
  
  # Criando novo campo ajustado de OCUP
  Novo_dataframe <- Novo_dataframe %>%
    mutate(
      GRP_OCUP_CORR = case_when(
        is.na(GRP_OCUP) & is.na(OCUP) ~ GRP_OCUP,
        is.na(GRP_OCUP) & !is.na(OCUP) ~ "OCUPAÇÃO NÃO IDENTIFICADA",
        GRP_OCUP == "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS" ~ "PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS",
        GRP_OCUP == "TRABALHADORES AGROPECUÁRIOS, FLORESTAIS E DA PESCA" ~ "AGROPECUÁRIOS, FLORESTAIS E DA PESCA",
        GRP_OCUP == "TRABALHADORES DOS SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS" ~ "SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS",
        GRP_OCUP == "TÉCNICOS DE NIVEL MÉDIO" ~ "TÉCNICOS DE NIVEL MÉDIO",
        GRP_OCUP == "TRABALHADORES EM SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO" ~ "SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO",
        GRP_OCUP == "MEMBROS SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE ORGANIZAÇÕES DE INTERESSE PÚBLICO E DE EMPRESAS, GERENTES" ~ "SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE EMPRESAS",
        GRP_OCUP == "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES" ~ "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES",
        GRP_OCUP == "TRABALHADORES DE SERVIÇOS ADMINISTRATIVOS" ~ "SERVIÇOS ADMINISTRATIVOS",
        GRP_OCUP == "MEMBROS DAS FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES" ~ "FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES"
      )
      , ANO_REGISTRO_CSV = Data_reg
    )
  
  # Filtro para fazer a contagem de registros por grupo de ocupação e ano
  OBITO_DECOR_OCUP <- Novo_dataframe %>%
    filter(!is.na(GRP_OCUP_CORR)) %>%
    group_by(GRP_OCUP_CORR, ANO_REGISTRO_CSV) %>%
    count() %>%
    arrange(desc(n))
  
  return(OBITO_DECOR_OCUP)
}


# Macro para cada ano
COUNT_GRP_OCUP_2017 <- OBITO_OCUP(Mortalidade_Geral_2017, 2017)
COUNT_GRP_OCUP_2018 <- OBITO_OCUP(Mortalidade_Geral_2018, 2018)
COUNT_GRP_OCUP_2019 <- OBITO_OCUP(Mortalidade_Geral_2019, 2019)
COUNT_GRP_OCUP_2020 <- OBITO_OCUP(Mortalidade_Geral_2020, 2020)
COUNT_GRP_OCUP_2021 <- OBITO_OCUP(Mortalidade_Geral_2021, 2021)
COUNT_GRP_OCUP_2022 <- OBITO_OCUP(Mortalidade_Geral_2022, 2022)
                                
# Juntando rsultados macro
COUNT_GRP_OCUP <- bind_rows(
                            COUNT_GRP_OCUP_2017
                            , COUNT_GRP_OCUP_2018
                            , COUNT_GRP_OCUP_2019
                            , COUNT_GRP_OCUP_2020
                            , COUNT_GRP_OCUP_2021
                            , COUNT_GRP_OCUP_2022
                          )

print(n = 20, COUNT_GRP_OCUP)


# Definir cores aleatórias para cada GRUPO DE OCUPAÇÃO
#cores <- rainbow(length(OCUP_OBITO$GRP_OCUP_CORR))


# Criando o gráfico de linha para a evolução dos casos
ggplot(COUNT_GRP_OCUP, aes(x = ANO_REGISTRO_CSV, y = n, group = GRP_OCUP_CORR, color = GRP_OCUP_CORR)) +
  geom_line(size = 0.9) +
  labs(x = "Ano", y = "Quantidade óbitos", title = "Óbitos por Grupo de Ocupação ao longo do tempo") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 6),  # Ajuste o tamanho da legenda aqui
        legend.title = element_blank())  +  # Remova o título da legenda
  scale_y_continuous(labels = scales::comma)  # Use o formato de separador de milhar no eixo y




###############################################################################
######################## Avaliação acidentes por grupo ########################  
###############################################################################



# Macro para contagem de óbitos por grupo de ocupação
OBITO_ACID_OCUP <- function(Base_Mortalidade, Data_reg) {
  
  # Join Mortalidade "OCUP" com DEPARA_GRUPO_TRABALHO
  Novo_dataframe <- left_join(Base_Mortalidade, DEPARA_GRUPO_TRABALHO, by = "OCUP")
  
  # Criando novo campo ajustado de OCUP
  Novo_dataframe <- Novo_dataframe %>%
    mutate(
      GRP_OCUP_CORR = case_when(
        is.na(GRP_OCUP) & is.na(OCUP) ~ GRP_OCUP,
        is.na(GRP_OCUP) & !is.na(OCUP) ~ "OCUPAÇÃO NÃO IDENTIFICADA",
        GRP_OCUP == "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS" ~ "PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS",
        GRP_OCUP == "TRABALHADORES AGROPECUÁRIOS, FLORESTAIS E DA PESCA" ~ "AGROPECUÁRIOS, FLORESTAIS E DA PESCA",
        GRP_OCUP == "TRABALHADORES DOS SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS" ~ "SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS",
        GRP_OCUP == "TÉCNICOS DE NIVEL MÉDIO" ~ "TÉCNICOS DE NIVEL MÉDIO",
        GRP_OCUP == "TRABALHADORES EM SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO" ~ "SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO",
        GRP_OCUP == "MEMBROS SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE ORGANIZAÇÕES DE INTERESSE PÚBLICO E DE EMPRESAS, GERENTES" ~ "SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE EMPRESAS",
        GRP_OCUP == "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES" ~ "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES",
        GRP_OCUP == "TRABALHADORES DE SERVIÇOS ADMINISTRATIVOS" ~ "SERVIÇOS ADMINISTRATIVOS",
        GRP_OCUP == "MEMBROS DAS FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES" ~ "FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES"
      )
      , ANO_REGISTRO_CSV = Data_reg
    )
  
  # Filtro para fazer a contagem de registros de acidentes por grupo de ocupação e ano
  OBITO_DECOR_ACID <- Novo_dataframe %>%
    filter(ACIDTRAB == "1" & !is.na(GRP_OCUP_CORR)) %>%
    group_by(GRP_OCUP_CORR, ANO_REGISTRO_CSV) %>%
    count() %>%
    arrange(desc(n))
  
  # Gráfico para visualização de óbitos decorrentes de acidentes por grupo
  GRAF_OBITO_DECOR_ACID <- ggplot(OBITO_DECOR_ACID, aes(x = GRP_OCUP_CORR, y = n, fill = GRP_OCUP_CORR)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = n), vjust = -0.5, size = 3) +  # Adiciona os rótulos dos valores acima das barras
    labs(x = NULL, y = "Quantidade de óbitos", title = "Óbitos por Grupo de Ocupação e Ano") +
    theme_minimal() +
    theme(axis.text.x = element_blank()) +  # Remove os nomes do eixo x
    scale_fill_manual(values = rainbow(nrow(OBITO_DECOR_ACID)), guide = guide_legend(override.aes = list(size = 4))) +
    labs(fill = "Grupo de Ocupação") +  # Adiciona o nome "Grupo de Ocupação" à legenda
    annotate("text", x = 0, y = -200, label = paste("Ano:", Data_reg), size = 3, hjust = 0.5)  # Adiciona o rodapé com o ano avaliado centralizado

  # Exibição do gráfico
  print(GRAF_OBITO_DECOR_ACID)
  
  
  
  return(OBITO_DECOR_ACID)
}


# Executa macro para cada ano
COUNT_ACID_OCUP_2017 <- OBITO_ACID_OCUP(Mortalidade_Geral_2017, 2017)
COUNT_ACID_OCUP_2018 <- OBITO_ACID_OCUP(Mortalidade_Geral_2018, 2018)
COUNT_ACID_OCUP_2019 <- OBITO_ACID_OCUP(Mortalidade_Geral_2019, 2019)
COUNT_ACID_OCUP_2020 <- OBITO_ACID_OCUP(Mortalidade_Geral_2020, 2020)
COUNT_ACID_OCUP_2021 <- OBITO_ACID_OCUP(Mortalidade_Geral_2021, 2021)
COUNT_ACID_OCUP_2022 <- OBITO_ACID_OCUP(Mortalidade_Geral_2022, 2022)



###############################################################################
################### Causas mais comuns de óbitos por grupo ####################  
###############################################################################

# Macro para verificar as causas mais comuns dentre o grupo PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS
ACID_TIPO_OCUP <- function(Base_Mortalidade, Data_reg, Grupo_escolhido) {
  
  # Join Mortalidade "OCUP" com DEPARA_GRUPO_TRABALHO
  Novo_dataframe <- left_join(Base_Mortalidade, DEPARA_TRABALHOS, by = "OCUP")
  
  # Criando novo campo ajustado de OCUP
  Novo_dataframe <- Novo_dataframe %>%
    mutate(
      GRP_OCUP_CORR = case_when(
        is.na(GRP_OCUP) & is.na(OCUP) ~ GRP_OCUP,
        is.na(GRP_OCUP) & !is.na(OCUP) ~ "OCUPAÇÃO NÃO IDENTIFICADA",
        GRP_OCUP == "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS" ~ "PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS",
        GRP_OCUP == "TRABALHADORES AGROPECUÁRIOS, FLORESTAIS E DA PESCA" ~ "AGROPECUÁRIOS, FLORESTAIS E DA PESCA",
        GRP_OCUP == "TRABALHADORES DOS SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS" ~ "SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS",
        GRP_OCUP == "TÉCNICOS DE NIVEL MÉDIO" ~ "TÉCNICOS DE NIVEL MÉDIO",
        GRP_OCUP == "TRABALHADORES EM SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO" ~ "SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO",
        GRP_OCUP == "MEMBROS SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE ORGANIZAÇÕES DE INTERESSE PÚBLICO E DE EMPRESAS, GERENTES" ~ "SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE EMPRESAS",
        GRP_OCUP == "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES" ~ "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES",
        GRP_OCUP == "TRABALHADORES DE SERVIÇOS ADMINISTRATIVOS" ~ "SERVIÇOS ADMINISTRATIVOS",
        GRP_OCUP == "MEMBROS DAS FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES" ~ "FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES"
      )
      , ANO_REGISTRO_CSV = Data_reg
    )
  
  # Filtro do 
  ACID_OCUP_FILTR <- Novo_dataframe %>%
    filter(ACIDTRAB == "1" & GRP_OCUP_CORR == Grupo_escolhido) %>%
    group_by(GRP_OCUP_CORR, CAUSABAS, ANO_REGISTRO_CSV) %>%
    count() %>%
    arrange(desc(n)) %>%
    head(10)
  
  return(ACID_OCUP_FILTR)
}



# Executa macro para cada ano
ACID_TIPO_OCUP_2017 <- ACID_TIPO_OCUP(Mortalidade_Geral_2017, 2017, "PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS")
ACID_TIPO_OCUP_2018 <- ACID_TIPO_OCUP(Mortalidade_Geral_2018, 2018, "PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS")
ACID_TIPO_OCUP_2019 <- ACID_TIPO_OCUP(Mortalidade_Geral_2019, 2019, "PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS")
ACID_TIPO_OCUP_2020 <- ACID_TIPO_OCUP(Mortalidade_Geral_2020, 2020, "PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS")
ACID_TIPO_OCUP_2021 <- ACID_TIPO_OCUP(Mortalidade_Geral_2021, 2021, "PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS")
ACID_TIPO_OCUP_2022 <- ACID_TIPO_OCUP(Mortalidade_Geral_2022, 2022, "PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS")


ACID_TIPO_OCUP_2017 <- ACID_TIPO_OCUP(Mortalidade_Geral_2017, 2017, "AGROPECUÁRIOS, FLORESTAIS E DA PESCA")
ACID_TIPO_OCUP_2018 <- ACID_TIPO_OCUP(Mortalidade_Geral_2018, 2018, "AGROPECUÁRIOS, FLORESTAIS E DA PESCA")
ACID_TIPO_OCUP_2019 <- ACID_TIPO_OCUP(Mortalidade_Geral_2019, 2019, "AGROPECUÁRIOS, FLORESTAIS E DA PESCA")
ACID_TIPO_OCUP_2020 <- ACID_TIPO_OCUP(Mortalidade_Geral_2020, 2020, "AGROPECUÁRIOS, FLORESTAIS E DA PESCA")
ACID_TIPO_OCUP_2021 <- ACID_TIPO_OCUP(Mortalidade_Geral_2021, 2021, "AGROPECUÁRIOS, FLORESTAIS E DA PESCA")
ACID_TIPO_OCUP_2022 <- ACID_TIPO_OCUP(Mortalidade_Geral_2022, 2022, "AGROPECUÁRIOS, FLORESTAIS E DA PESCA")


ACID_TIPO_OCUP_2017 <- ACID_TIPO_OCUP(Mortalidade_Geral_2017, 2017, "SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS")
ACID_TIPO_OCUP_2018 <- ACID_TIPO_OCUP(Mortalidade_Geral_2018, 2018, "SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS")
ACID_TIPO_OCUP_2019 <- ACID_TIPO_OCUP(Mortalidade_Geral_2019, 2019, "SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS")
ACID_TIPO_OCUP_2020 <- ACID_TIPO_OCUP(Mortalidade_Geral_2020, 2020, "SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS")
ACID_TIPO_OCUP_2021 <- ACID_TIPO_OCUP(Mortalidade_Geral_2021, 2021, "SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS")
ACID_TIPO_OCUP_2022 <- ACID_TIPO_OCUP(Mortalidade_Geral_2022, 2022, "SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS")



# Junta os dados de cada ano
ACID_TIPO_OCUP_ANOS <- bind_rows(
                                  ACID_TIPO_OCUP_2017
                                  , ACID_TIPO_OCUP_2018
                                  , ACID_TIPO_OCUP_2019
                                  , ACID_TIPO_OCUP_2020
                                  , ACID_TIPO_OCUP_2021
                                  , ACID_TIPO_OCUP_2022
                                )

print(ACID_TIPO_OCUP_ANOS)


# Seleciona apenas top 5 causas de óbito
ACID_TIPO_OCUP_AGRUP <- ACID_TIPO_OCUP_ANOS %>%
  group_by(GRP_OCUP_CORR, CAUSABAS) %>%
  summarize(n = sum(n)) %>%
  arrange(desc(n)) %>%
  head(5)

print(ACID_TIPO_OCUP_AGRUP)

# Cálculo da porcentagem de cada fatia em relação ao total
ACID_TIPO_OCUP_AGRUP2 <- ACID_TIPO_OCUP_AGRUP %>%
  group_by(GRP_OCUP_CORR) %>%
  mutate(porcentagem = n / sum(n) * 100)

print(ACID_TIPO_OCUP_AGRUP2)


# Criação do gráfico de pizza com porcentagens
ggplot(ACID_TIPO_OCUP_AGRUP2, aes(x = "", y = n, fill = CAUSABAS)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Causas mais comuns de óbito - Top 5",
       fill = "Causa do óbito",
       x = NULL,
       y = NULL,
       caption = "Grupo Ocupação: PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS") +
  theme_void() +
  geom_text(aes(label = paste0(round(porcentagem, 1), "%")), position = position_stack(vjust = 0.5), color = "white", size = 4)




###############################################################################
############################# Teste de hipótese ############################### 
###############################################################################



# Dados de PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS
COD_OBITO1 <- c("V685", "V645", "V892", "V499", "W879")
QTD1 <- c(414, 330, 262, 241, 218)

# Dados de AGROPECUÁRIOS, FLORESTAIS E DA PESCA
COD_OBITO2 <- c("W208", "W209", "W207", "V845", "V849")
QTD2 <- c(163, 141, 135, 85, 69)

# Dados de SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS
COD_OBITO3 <- c("V234", "V244", "V892", "V299", "V274")
QTD3 <- c(156, 86, 66, 65, 46)


TESTE_HIPOTESE <- data.frame(GRP_OCUP_CORR = c(rep("PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS", 5), rep("AGROPECUÁRIOS, FLORESTAIS E DA PESCA", 5), rep("SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS", 5)),
                   CAUSABAS = c(COD_OBITO1, COD_OBITO2, COD_OBITO3),
                   n = c(QTD1, QTD2, QTD3))

# Criar tabela de contingência
TAB_CONTG <- table(TESTE_HIPOTESE$GRP_OCUP_CORR, TESTE_HIPOTESE$CAUSABAS)

print(TAB_CONTG)

# Realização do teste de qui-quadrado de independência
RESULTADO <- chisq.test(TAB_CONTG)

# Exibição do resultado do teste
print(RESULTADO)


# Resultado acima de 0.05 -> Não comprova que tem correlação entre tipo de trabalho e acidente





















