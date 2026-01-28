#------------------------------------------------------
# CURSO ANALISE DE DADOS PARA O SUS
# ATIVIDADE PRÁTICA - MOD.1 AULA 2 
# ALUNO: JOSÉ A E NASCIMENTO
#------------------------------------------------------

#------------------------------------------------------
# 
# Atividade 1
# 
# 1.1 Crie uma nova variável chamada "faixa_etaria" que classifique as idades em quatro categorias:
#  "Criança" para idades de 0 a 12 anos, "Adolescente" para 13 a 17 anos, "Adulto" para 18 a 59 anos e
#  "Idoso" para 60 anos ou mais. Dica: utilize a função mutate() combinada com case_when().
#
# 1.2 Conte quantos óbitos há em cada faixa etária criada. Dica: você pode usar a função count() ou
#   combinar group_by() com summarise(). 
#------------------------------------------------------

#-------------------------------------
# CONFIGURAÇÃO INICIAL
#-------------------------------------

# CARREGAR PACTES
library(tidyverse)
library(lubridate)
library(readxl)
library(arrow)
library(dplyr)
library(knitr) # cria tabelas em markdown

# DEFINIR DIRETÓRIO DE TRABALHO 
curso <- "F:\\Documentos\\IA\\Fiocruz\\Course_Fiocruz_Int_DA_Res_SUS"
src <- file.path(curso, "src")
dados <- file.path(curso, "data")

setwd(src)

#-------------------------------------
# IMPORTAÇÃO DOS DADOS
#-------------------------------------

df_csv <- read_csv(file.path(dados,"sim_salvador_2023.csv"))
glimpse(df_csv)
#-------------------------------------
# 1.1 Crie uma nova variável chamada "faixa_etaria" que classifique as 
#   idades em quatro categorias:
#     "Criança" para idades de 0 a 12 anos, 
#     "Adolescente" para 13 a 17 anos, 
#     "Adulto" para 18 a 59 anos e
#     "Idoso" para 60 anos ou mais. 
#
# Dica: utilize a função mutate() combinada com case_when().
#-------------------------------------

df_csv <- df_csv %>%
  mutate(
    
    # TRATAMENTO DA IDADE conforme dicionário de dados.
    # idade: composto de dois subcampos.
    # - O primeiro, de 1 dígito, indica a unidade da idade (se 1 = minuto,
    #                                                           se 2 = hora, se 3 = mês, se 4 = ano, se = 5 idade maior que 100 anos).
    # - O segundo, de dois dígitos, indica a quantidade de unidades:
    #   Idade menor de 1 hora: subcampo varia de 01 e 59 (minutos); De 1
    #   a 23 Horas: subcampo varia de 01 a 23 (horas); De 24 horas e 29
    #   dias: subcampo varia de 01 a 29 (dias); De 1 a menos de 12 meses
    #   completos: subcampo varia de 01 a 11 (meses); Anos - subcampo
    #   varia de 00 a 99;
    #   - 9 - ignorado
    
    # Extrai unidade e valor
    unidade_idade = floor(IDADE / 100),
    valor_idade = IDADE %% 100,
    
    # Converte para anos
    idade_anos = case_when(
      unidade_idade == 1 ~ valor_idade / (365*24*60),     # min para y
      unidade_idade == 2 ~ valor_idade / (365*24),        # hr para y
      unidade_idade == 3 ~ valor_idade / 12,              # mes para y
      unidade_idade == 4 ~ valor_idade,                   # anos de 0-99
      unidade_idade == 5 ~ valor_idade + 100,             # anos acima de 100
      unidade_idade == 9 ~ NA_real_,                      # Ignorado
      TRUE ~ NA_real_
    ),
    
    
    # Agupa por faixa etaria
    faixa_etaria = case_when(
      idade_anos >= 0 & idade_anos <= 12 ~ "Criança",
      idade_anos >= 13 & idade_anos <= 17 ~ "Adolescente",
      idade_anos >= 18 & idade_anos <= 59 ~ "Adulto",
      idade_anos >= 60  ~ "Idoso",
      is.na(idade_anos) ~ NA_character_
    )
  )


#-------------------------------------
# 1.2 Conte quantos óbitos há em cada faixa etária criada. 
#   Dica: você pode usar a função count() ou
#   combinar group_by() com summarise(). 
#------------------------------------------------------

contagem_faixa_etaria <- df_csv %>%
  group_by(faixa_etaria) %>%
  summarise(
    n_obitos = n(),
    .groups = "drop" # remove o group_by depois da contagem
  ) 

print(contagem_faixa_etaria)

#------------------------------------------------------
# 
# Atividade 2: Manipulação de Datas e Agrupamento
#
# 2.1 Crie uma variável chamada "trimestre" que identifique em qual trimestre 
#   do ano ocorreu o óbito. Os trimestres devem ser classificados como: 
#   "1º Trimestre" para janeiro, fevereiro e março;
#   "2º Trimestre" para abril, maio e junho; 
#   "3º Trimestre" para julho, agosto e setembro; e 
#   "4º Trimestre" para outubro, novembro e dezembro. 
#
# Dica: use a função month() para extrair o mês da
#   data e case_when() para classificar em trimestres.
#
# 2.2 Calcule o total de óbitos e a idade média por trimestre e por sexo. 
#
# Dica: utilize group_by() com duas variáveis (trimestre e sexo_p) e 
# depois summarise() para calcular as estatísticas.
# 
#------------------------------------------------------

# Agupa por trimestre
df_csv <- df_csv %>%
  mutate(
    # converte data para dmy
    DTOBITO_dt = dmy(DTOBITO),
    mes_obito = month(DTOBITO_dt),

    # classifica    
    trimestre = case_when(
      mes_obito >= 1 & mes_obito <= 3 ~ "1º Trimestre",
      mes_obito >= 4 & mes_obito <= 6 ~ "2º Trimestre", 
      mes_obito >= 7 & mes_obito <= 9 ~ "3º Trimestre",
      mes_obito >= 10 & mes_obito <= 12 ~ "4º Trimestre",
      TRUE ~ NA_character_
    )
  )

# Define SEXO 1=masculino 2-feminino

df_csv <- df_csv %>%
  mutate(
    sexo_p = case_when(
      SEXO == 1 ~ "Masculino",
      SEXO == 2 ~ "Feminino",
      SEXO == 3 ~ "Ignorado",
      is.na(SEXO) ~ NA_character_
    )
  )

# Agrupa e sumariza os dados
obitos_trimestre <- df_csv %>%
  group_by(trimestre, sexo_p) %>%
  summarise(
      total_obitos = n(),
      idade_media = mean(idade_anos, na.rm = TRUE),
      .groups = "drop"
    )

print(obitos_trimestre)

#------------------------------------------------------
# Atividade 3: Análise Integrada
#
# Esta atividade integra vários conceitos aprendidos durante a aula:
#   3.1 Identifique qual foi o mês com maior número de óbitos no ano de 2023.
#   3.2 Calcule a diferença percentual entre o número de óbitos masculinos e 
#   femininos.
#   3.3 Determine qual faixa etária teve o maior número de óbitos ao longo do 
#   ano.
#   
# Dica: Para resolver essas questões, combine as funções trabalhadas na 
# aula — como mutate(), group_by() e summarise() — além de outras funções 
# úteis, como arrange() e filter().
#------------------------------------------------------

# mês com maior número de óbitos no ano
mes_mais_obitos <- df_csv %>%
  count(mes_obito, name = "total_obitos") %>%
  arrange(desc(total_obitos)) %>%
  slice(1) # sugestão pega apenas o maior.

print(mes_mais_obitos)

# total por sexo
obitos_por_sexo <- df_csv %>%
  count(sexo_p, name = "total") %>%
  filter(sexo_p %in% c("Masculino", "Feminino"))

# diferença percentual entre o número de óbitos masculinos e femininos.
total_masc <- obitos_por_sexo$total[obitos_por_sexo$sexo_p == "Masculino"]
total_femi <- obitos_por_sexo$total[obitos_por_sexo$sexo_p == "Feminino"]
total_geral <- total_masc + total_femi

diferenca_percentual <- ((total_masc - total_femi) / total_geral) * 100

print("3.2 - Diferença percentual entre o número de óbitos masculinos e femininos:")
cat(sprintf("Masculino: %d óbitos (%.1f%%)\n", total_masc, (total_masc/total_geral)*100))
cat(sprintf("Feminino: %d óbitos (%.1f%%)\n", total_femi, (total_femi/total_geral)*100))
cat(sprintf("Diferença percentual: %.1f%%\n", diferenca_percentual))


# obitos por faixa etária e maior total de obitos na faixa

# total_obitos nas faixas
faixas_todas <- df_csv %>%
  count(faixa_etaria, name = "total_obitos") %>%
  arrange(desc(total_obitos))  

print(faixas_todas)

# maior faixa depois de ordenar
print("\nFaixa etária com MAIOR número de óbitos:")
faixa_maior <- faixas_todas %>%
  slice(1)  
print(faixa_maior)
 
  


