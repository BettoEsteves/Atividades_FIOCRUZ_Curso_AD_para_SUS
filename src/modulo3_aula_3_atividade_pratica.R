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
library(broom)
library(DT)
library(plotly)
library(highcharter)
library(echarts4r)
#pacotes para Analises
library(survival) # analises sobrevivência
library(lme4) # modelos multiníveis
library(forecast) # Séries temporais



# DEFINIR DIRETÓRIO DE TRABALHO 
curso <- "G:\\Documentos\\IA\\Fiocruz\\Course_Fiocruz_Int_DA_Res_SUS"
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

# Semente
set.seed(123)

# Configuração
n_hospitais <- 10
n_pac_por_hosp <- 30

# simular diferença entre hospitais
efeito_hospital <- rnorm(n_hospitais, mean = 0, sd = 5)

# visualiza simulação 
print(efeito_hospital)

# Cria dados
dados_multinivel <- tibble(
  hospital_id = rep(1:n_hospitais, each = n_pac_por_hosp), # ID dos hospitais de 1 a 10
  paciente_id = 1:(n_hospitais * n_pac_por_hosp)           # ID dos pacientes entre 1 a 300
) %>%
  mutate(
    idade = round(runif(n(), 30, 70)),                                # idade entre 30-70 
    sexo = sample(c("F", "M"), n(), replace = TRUE),                  # sexo aleatório
    efeito_hosp = efeito_hospital[hospital_id],                       # cria matriz com efeito hospital
    pa = round(100 + 0.5 * idade + efeito_hosp + rnorm(n(), 0, 8), 1) # PA = valor base + efeito da idade + efeito do hospital + ruido
  )

head(dados_multinivel, 10)

#-------------------------------------
# EXPLORAÇÃO: Médias por Hospital
#-------------------------------------
# Calcular a média de PA em cada hospital
# OBSERVE:
# - As médias de PA variam entre hospitais
# - Isso é o "efeito do hospital" que criamos
#-------------------------------------

# Calcula a média de PA em cada hospital
dados_multinivel %>%
  group_by(hospital_id) %>%
  summarise(
    n = n(),                    # pacientes
    media_pa = mean(pa),        # Média de PA
    dp_pa = sd(pa)              # Desvio-padrão
  )

#-------------------------------------
# COMPARAÇÃO: Modelo Comum vs Modelo Multinível
#-------------------------------------
# MODELO COMUM (ignora a estrutura hierárquica)
# Trata todos os 300 pacientes como se fossem independentes
# 
# MODELO MULTINÍVEL
# Reconhece que pacientes estão agrupados em hospitais
#
# A notação (1 | hospital_id) significa:
# "Permita que o INTERCEPTO varie por hospital"
# Ou seja: cada hospital pode ter um "nível base" diferente de PA
#-------------------------------------
# COMO LER O RESULTADO:
#-------------------------------------
# Fixed effects (Efeitos Fixos):
# → São os efeitos que são IGUAIS para todos os hospitais
# → (Intercept): valor base de PA
# → idade: efeito de cada ano de idade (igual em todos os hospitais)
# → sexoM: diferença entre homens e mulheres (igual em todos os hospitais)
#
# Random effects (Efeitos Aleatórios):
# → Mostram quanta variação existe ENTRE os hospitais
# → Variance do hospital_id: variabilidade entre hospitais
# → Variance Residual: variabilidade dentro de cada hospital
#-------------------------------------

# Modelo Comum
modelo_comum <- lm(pa ~ idade + sexo, data = dados_multinivel)

summary(modelo_comum)

# Modelo Multinível
modelo_multinivel <- lmer(pa ~ idade + sexo + (1 | hospital_id), 
                          data = dados_multinivel)

summary(modelo_multinivel)

#-------------------------------------
# VER OS EFEITOS
#-------------------------------------
# Cada hospital tem um "ajuste" diferente no intercepto
# Hospitais com valor POSITIVO têm PA média mais alta
# Hospitais com valor NEGATIVO têm PA média mais baixa
#-------------------------------------

# Efeitos FIXOS (iguais para todos):
fixef(modelo_multinivel)

# Efeitos ALEATÓRIOS (variam por hospital):
ranef(modelo_multinivel)

#-------------------------------------
# ATIVIDADE 2: SÉRIES TEMPORAIS
#-------------------------------------
# São dados coletados ao longo do TEMPO, em intervalos regulares.
#
# Exemplos:
# - Casos de dengue por SEMANA
# - Temperatura diária
# - PIB trimestral
# - Internações mensais
#
# COMPONENTES DE UMA SÉRIE TEMPORAL:
# ----------------------------------
# 1. TENDÊNCIA: Direção geral (subindo, descendo, estável)
# 2. SAZONALIDADE: Padrões que se repetem (ex: gripe no inverno)
# 3. CICLOS: Flutuações de longo prazo
# 4. RUÍDO: Variações aleatórias
#-------------------------------------


# DADOS: Casos de Síndrome Gripal por Semana
set.seed(42)
n_semanas <- 156  # 3 anos de dados (52 semanas x 3)

# Série temporal simulada:
serie_gripal <- tibble(
  semana = 1:n_semanas,                    # Semana 1 a 156
  ano = rep(2021:2023, each = 52),         # Ano correspondente
  semana_ano = rep(1:52, 3)                # Semana dentro do ano (1-52)
) %>%
  mutate(
    # TENDÊNCIA: aumenta levemente ao longo do tempo
    tendencia = 100 + 0.3 * semana,
    
    # SAZONALIDADE: pico no outono/inverno (função seno)
    # O padrão se repete a cada 52 semanas
    sazonalidade = 50 * sin(2 * pi * (semana_ano - 10) / 52),
    
    # RUÍDO: variação aleatória
    ruido = rnorm(n_semanas, 0, 15),
    
    # CASOS = tendência + sazonalidade + ruído
    # pmax(..., 10) garante que não tenhamos valores negativos
    casos = round(pmax(tendencia + sazonalidade + ruido, 10))
  )

head(serie_gripal, 10)

#-------------------------------------
# VISUALIZAÇÃO DA SÉRIE TEMPORAL com ggplot
#-------------------------------------
# OBSERVE:
# → Há uma TENDÊNCIA de aumento ao longo do tempo?
# → Há picos que se repetem todo ano (SAZONALIDADE)?
# → Há variações "irregulares" (RUÍDO)?
#-------------------------------------

ggplot(serie_gripal, aes(x = semana, y = casos)) +
  geom_line(color = "steelblue", linewidth = 0.8) +    
  labs(title = "Casos de Síndrome Gripal por Semana (2021-2023)",
       x = "Semana", 
       y = "Número de Casos") +
  theme_minimal()

#-------------------------------------
# CONVERTER PARA OBJETO DE SÉRIE TEMPORAL
#-------------------------------------
# Formato R para séries temporais: ts()
#
# ts(dados, frequency = ciclos_por_período, start = início)
#
# frequency = 52 porque temos dados SEMANAIS (52 semanas por ano)
#
# start = c(2021, 1) significa: começa no ano 2021, semana 1
#-------------------------------------

serie_ts <- ts(serie_gripal$casos, frequency = 52, start = c(2021, 1))

print(serie_ts)

#-------------------------------------
# DECOMPOSIÇÃO DA SÉRIE
#--------------------------------------
# O GRÁFICO MOSTRA:
#
# 1. observed: Os dados originais (o que observamos)
# 2. trend: A TENDÊNCIA extraída (direção geral)
# 3. seasonal: O padrão SAZONAL (o que se repete todo ano)
# 4. random: O RUÍDO (o que sobra depois de remover tendência e sazonalidade)
#-------------------------------------

decomposicao <- decompose(serie_ts)

plot(decomposicao)

#-------------------------------------
# MODELO ARIMA E PREVISÃO
#--------------------------------------
# ARIMA é um modelo para séries temporais.
# auto.arima() escolhe automaticamente o melhor modelo ARIMA
# A notação ARIMA(p,d,q) descreve o modelo
#-------------------------------------

modelo_arima <- auto.arima(serie_ts)

print(modelo_arima)

#-------------------------------------
# Fazendo Previsão
#--------------------------------------
# forecast() prevê os próximos valores
# h = 12 significa: prever as próximas 12 semanas
#
# O GRÁFICO MOSTRA:
# - Linha azul escura: valores observados
# - Linha azul clara: previsão
# - Áreas sombreadas: intervalos de confiança (80% e 95%)
#   → Quanto mais para o futuro, maior a incerteza
#-------------------------------------

previsao <- forecast(modelo_arima, h = 12)

plot(previsao, 
     main = "Previsão de Casos para as Próximas 12 Semanas",
     xlab = "Tempo", 
     ylab = "Casos")

#-------------------------------------
# ATIVIDADE 3: ANÁLISE DE SOBREVIVÊNCIA
#-------------------------------------
# O QUE É ANÁLISE DE SOBREVIVÊNCIA?
#-------------------------------------
# Estuda o TEMPO até a ocorrência de um EVENTO.
#
# Exemplos de "evento":
# - Óbito
# - Recidiva de câncer
# - Alta hospitalar
# - Reinternação
#-------------------------------------
# O QUE É CENSURA?
#-------------------------------------
# Nem todos os participantes apresentam o evento durante o estudo.
# Alguns podem:
# - Sair do estudo antes do fim
# - Chegar ao fim do estudo sem ter o evento
# - Ser perdidos no acompanhamento
#
# Esses casos são "CENSURADOS" - sabemos que o tempo foi PELO MENOS X,
# mas não sabemos exatamente quando (ou se) o evento ocorreria.
#
#-------------------------------------
# MÉTODOS PRINCIPAIS:
#-------------------------------------
# - Kaplan-Meier: Estima a curva de sobrevivência
# - Log-Rank: Compara curvas entre grupos
# - Cox: Identifica fatores de risco
#-------------------------------------

#-------------------------------------
# Dados - sobrvida pacientes com câncer. Dados simulados.
#-------------------------------------

set.seed(456)
n_pac <- 150   

# Criar dados 
dados_sobrevida <- tibble(
  id = 1:n_pac,
  # Tratamento: Quimioterapia ou Imunoterapia
  tratamento = sample(c("Quimio", "Imuno"), n_pac, replace = TRUE),
  # Idade: entre 35 e 85 anos
  idade = round(pmin(pmax(rnorm(n_pac, 60, 12), 35), 85)),
  # Estadio do câncer: II, III ou IV
  estadio = sample(c("II", "III", "IV"), n_pac, replace = TRUE, 
                   prob = c(0.3, 0.4, 0.3))
) %>%
  mutate(
    # Calcula uma "taxa de risco" baseada nas características
    # Menor taxa = melhor prognóstico
    taxa = case_when(
      tratamento == "Imuno" ~ 0.015,    # Imunoterapia: menor taxa (melhor)
      TRUE ~ 0.025                      # Quimio: maior taxa
    ) * case_when(
      estadio == "II" ~ 0.5,            # Estadio II: menor risco
      estadio == "III" ~ 1,             # Estadio III: risco médio
      TRUE ~ 1.8                        # Estadio IV: maior risco
    ),
    # Tempo até o evento (simulado com distribuição exponencial)
    tempo_evento = rexp(n_pac, rate = taxa),
    # Tempo máximo de acompanhamento (quando o estudo termina)
    tempo_censura = 60,  # 60 meses = 5 anos
    # Tempo observado: o menor entre tempo do evento e tempo de censura
    tempo = pmin(tempo_evento, tempo_censura),
    # Status: 1 se teve evento, 0 se foi censurado
    status = as.integer(tempo_evento <= tempo_censura)
  )

#-------------------------------------
# ENTENDENDO AS COLUNAS:
# - tempo: tempo de acompanhamento em meses
# - status: 1 = teve o evento (óbito), 0 = censurado (não teve evento até o fim)
#-------------------------------------
head(dados_sobrevida, 10)

# Quantos tiveram evento e quantos foram censurados (0-censurado 1-evento)
table(dados_sobrevida$status)

#-------------------------------------
# CRIA OBJETO DE SOBREVIVÊNCIA
#-------------------------------------
# Surv() cria um objeto especial que o R reconhece como dados de sobrevivência
# Ele combina o TEMPO e o STATUS (evento ou censura)
#-------------------------------------

surv_obj <- Surv(time = dados_sobrevida$tempo, 
                 event = dados_sobrevida$status)

head(surv_obj, 20) # Os números com "+" são CENSURADOS

#-------------------------------------
# CURVA DE KAPLAN-MEIER
#-------------------------------------
# survfit() ajusta a curva de Kaplan-Meier
# ~ 1 significa: uma curva para todos (sem separar por grupos)
#-------------------------------------
# COMO LER:
# - n: número de pacientes
# - events: número de eventos
# - median: tempo mediano de sobrevida (50% ainda vivos)
# - 0.95LCL e 0.95UCL: IC 95% para a mediana
#-------------------------------------

km_geral <- survfit(surv_obj ~ 1)

print(km_geral)

#-------------------------------------
# Plotagem da curva
#-------------------------------------
# COMO INTERPRETAR O GRÁFICO:
# - Eixo X: Tempo (em meses)
# - Eixo Y: Probabilidade de estar vivo
# - A curva começa em 1 (100%) e vai descendo
# - Cada "degrau" é um evento (óbito)
# - As linhas verticais tracejadas são os censurados
# - Quanto mais ALTA a curva, melhor a sobrevida
#-------------------------------------

plot(km_geral, 
     main = "Curva de Sobrevida (Kaplan-Meier)",
     xlab = "Tempo (meses)",
     ylab = "Probabilidade de Sobrevida",
     col = "steelblue", 
     lwd = 2)     


#-------------------------------------
# COMPARAÇÃO ENTRE TRATAMENTOS
#-------------------------------------
# comparar a sobrevida entre Quimio e Imunoterapia
# OBSERVE:
# → Qual curva está MAIS ALTA? (melhor sobrevida)
# → As curvas são muito diferentes ou quase iguais?
#-------------------------------------

#-------------------------------------
# Kaplan-Meier por tratamento
#-------------------------------------

km_trat <- survfit(surv_obj ~ tratamento, data = dados_sobrevida)

plot(km_trat,
     main = "Sobrevida por Tratamento",
     xlab = "Tempo (meses)",
     ylab = "Probabilidade de Sobrevida",
     col = c("coral", "steelblue"),   # Cores para cada grupo
     lwd = 2)

legend("bottomleft", 
       legend = c("Imuno", "Quimio"),
       col = c("coral", "steelblue"), 
       lwd = 2)

#-------------------------------------
# TESTE LOG-RANK
#-------------------------------------
# O teste Log-Rank verifica se há diferença ESTATISTICAMENTE SIGNIFICATIVA
# entre as curvas de sobrevivência dos grupos
#-------------------------------------
# COMO LER:
# - N: número em cada grupo
# - Observed: eventos observados
# - Expected: eventos esperados (se não houvesse diferença)
# - Chisq: estatística qui-quadrado
# - p: p-valor
#
# Se p < 0.05 → Há diferença significativa entre os tratamentos
#-------------------------------------

survdiff(surv_obj ~ tratamento, data = dados_sobrevida)

#-------------------------------------
# MODELO DE COX
#-------------------------------------
# O modelo de Cox identifica FATORES DE RISCO para o evento
# É como uma regressão, mas para dados de sobrevivência
#-------------------------------------
# Medida de efeito: HR (Hazard Ratio / Razão de Risco)
#   HR = 1: Sem efeito
#   HR > 1: Fator de RISCO (aumenta o risco do evento)
#   HR < 1: Fator de PROTEÇÃO (diminui o risco do evento)
#-------------------------------------
# COMO LER O RESULTADO:
#-------------------------------------
# coef: coeficiente (na escala logarítmica)
# exp(coef): HR (Hazard Ratio) - É O MAIS IMPORTANTE!
# se(coef): erro padrão
# Pr(>|z|): p-valor
#
# lower .95 e upper .95: IC 95% para o HR
#-------------------------------------

modelo_cox <- coxph(surv_obj ~ tratamento + idade + estadio, 
                    data = dados_sobrevida)

summary(modelo_cox)

# Ver só os Hazard Ratios:
exp(coef(modelo_cox))

