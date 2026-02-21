#------------------------------------------------------
# CURSO ANALISE DE DADOS PARA O SUS
# ATIVIDADE PRÁTICA - MOD.3 AULA 3 
# ALUNO: JOSÉ A E NASCIMENTO
#------------------------------------------------------


#-------------------------------------
# CONFIGURAÇÃO INICIAL
#-------------------------------------

# CARREGAR PACTES
library(tidyverse)
library(lubridate)
library(readxl)
library(arrow)
library(ggplot2)
library(readr)
library(datasauRus)
#testando outras libs
library(knitr) # cria tabelas em markdown
library(plotly)
library(highcharter)
library(echarts4r)
#pacotes para Analises
library(survival) # analises sobrevivência
library(lme4) # modelos multiníveis
library(forecast) # Séries temporais



# DEFINIR DIRETÓRIO DE TRABALHO 
curso <- "F:\\Documentos\\IA\\Fiocruz\\Course_Fiocruz_Int_DA_Res_SUS"
src <- file.path(curso, "src")
dados <- file.path(curso, "data")

setwd(src)
getwd()
#-------------------------------------
# IMPORTAÇÃO DOS DADOS
#-------------------------------------

df_csv <- read_csv(file.path(dados,"sim_salvador_2023.csv"))
glimpse(df_csv)

#-------------------------------------
# ATIVIDADE 1: MODELOS MULTINÍVEIS
#-------------------------------------
# Dados com Estruturas de Dependência
# ATIVIDADES COMPLEMENTARES
# Parte 1: Modelos Multiníveis
# Usados quando há hierarquia (pacientes em hospitais).
# 1.1. Compare médias de PA entre hospitais
# 1.2. Ajuste modelo comum (lm)
# 1.3. Ajuste modelo multinível (lmer)
# 1.4. Compare efeitos fixos e aleatórios
# 
#-------------------------------------

