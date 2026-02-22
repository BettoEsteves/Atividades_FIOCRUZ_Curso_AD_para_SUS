#------------------------------------------------------
# CURSO ANALISE DE DADOS PARA O SUS
# ATIVIDADE PRÁTICA - MOD.3 AULA 3 
# ALUNO: JOSÉ A E NASCIMENTO
#------------------------------------------------------
# MÓDULO 3 - AULA 4: Aplicação dos Modelos Estatísticos
# Atividades Complementares
#------------------------------------------------------
# OBJETIVO DESTA AULA:
# Aplicar os modelos estudados nas aulas anteriores em situações práticas
# de saúde pública, integrando os conceitos e praticando a interpretação.
#------------------------------------------------------
# CASOS PRÁTICOS:
#   Caso 1: Fatores de risco para COVID-19 grave (Regressão Logística)
#   Caso 2: Comparação de tratamentos (ANOVA)
#   Caso 3: Sobrevida em oncologia (Kaplan-Meier + Cox)
#------------------------------------------------------


#------------------------------------------------------
# CONFIGURAÇÃO INICIAL
#------------------------------------------------------
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

#----------------------------------
# Funções globais
#----------------------------------

# Define uma pausa que aguarda um ENTER para continuar
pause <- function(msg = "Pressione <ENTER> para continuar...") {
  cat("\n", msg, "\n", sep = "")
  cat("> ", sep = "")
  invisible(readline())
}


#----------------------------------
# Função para ajustar margens automaticamente e evitar
# erro - Error in plot.new() : figure margins too large
#----------------------------------
ajustar_margens <- function() {
  
  # PASSO 1: NÃO fechar dispositivos! Só criar novo se não existir
  if (dev.cur() == 1) {  # Se não há dispositivo aberto
    if (.Platform$OS.type == "windows") {
      windows(width = 14, height = 10, xpos = 0, ypos = 0)
    } else {
      x11(width = 14, height = 10)
    }
    cat("✅ Nova janela externa criada\n")
  } else {
    cat("✅ Usando janela existente\n")
  }
  
  # PASSO 2: SALVAR layout atual ANTES de qualquer alteração
  layout_atual <- par("mfrow")
  n_linhas <- layout_atual[1]
  n_colunas <- layout_atual[2]
  total_graficos <- n_linhas * n_colunas
  
  # PASSO 3: REAPLICAR layout (importante!)
  par(mfrow = c(n_linhas, n_colunas))
  
  # PASSO 4: Ajustar margens baseado no número de gráficos
  if (total_graficos == 4) {
    par(
      mar = c(4.5, 4.5, 2.5, 1.0),
      mgp = c(2.5, 0.8, 0),
      cex.axis = 0.9,
      cex.lab = 1.0,
      cex.main = 1.1,
      las = 1
    )
  } else if (total_graficos == 1) {
    par(
      mar = c(5.0, 5.0, 3.0, 1.2),
      mgp = c(3.0, 1.0, 0),
      cex.axis = 1.0,
      cex.lab = 1.1,
      cex.main = 1.2,
      las = 1
    )
  }
}


#----------------------------------
# CASO 1: FATORES DE RISCO PARA COVID-19 GRAVE
#----------------------------------
# CONTEXTO
#----------------------------------
# Você é analista de dados em uma secretaria de saúde.
# Precisa identificar quais características dos pacientes estão associadas
# a casos GRAVES de COVID-19, para orientar políticas de saúde.
#
# PERGUNTA DE PESQUISA:
# "Quais fatores aumentam a chance de um paciente ter COVID-19 grave?"
#
# TIPO DE DESFECHO:
# Binário (grave vs não grave) → Regressão Logística
#----------------------------------


#----------------------------------
# DADOS: Pacientes com COVID-19 - dados simulados de 500 indivíduos
#----------------------------------

set.seed(2020)      
n_covid <- 500      

# Cria dados 
dados_covid <- tibble(
  id = 1:n_covid,
  
  idade = round(pmin(pmax(rnorm(n_covid, 50, 18), 18), 95)), # Idade entre 18-95 e média de 50
  
  sexo = sample(c("Feminino", "Masculino"), n_covid, replace = TRUE), # distribui igualmente sexo M e F
  
  # Comorbidades
  diabetes = sample(c("Não", "Sim"), n_covid, replace = TRUE, prob = c(0.85, 0.15)),
  hipertensao = sample(c("Não", "Sim"), n_covid, replace = TRUE, prob = c(0.70, 0.30)),
  obesidade = sample(c("Não", "Sim"), n_covid, replace = TRUE, prob = c(0.75, 0.25))
  
) %>%
  mutate(
    # Calcula probabilidade de caso grave baseada nos fatores de risco:
    prob_grave = plogis(
      -3 +                                    # Intercepto
      0.04 * idade +                          # Idade aumenta risco
      0.3 * (sexo == "Masculino") +           # Homens: maior risco
      0.8 * (diabetes == "Sim") +             # Diabetes: maior risco
      0.5 * (hipertensao == "Sim") +          # Hipertensão: maior risco
      0.6 * (obesidade == "Sim")              # Obesidade: maior risco
    ),
    
    caso_grave = rbinom(n_covid, 1, prob_grave) # Atribui desfecho (0 = não grave, 1 = grave) 
  )

head(dados_covid)


#----------------------------------
# ANÁLISE DESCRITIVA
#----------------------------------
# OBSERVE:
# → Os casos graves (1) têm mais idade em média?
# → Há maior proporção de comorbidades nos casos graves?
#----------------------------------


table(dados_covid$caso_grave) # Mostra nr casos graves e não graves

prop.table(table(dados_covid$caso_grave)) * 100 # Mostra % casos graves e não graves

# Compara características entre casos graves e não graves
dados_covid %>%
  group_by(caso_grave) %>%
  summarise(
    n = n(),                                                       # Número de pacientes
    idade_media = round(mean(idade), 1),                           # Média de idade
    prop_masculino = round(mean(sexo == "Masculino") * 100, 1),    # % homens
    prop_diabetes = round(mean(diabetes == "Sim") * 100, 1),       # % diabéticos
    prop_hipertensao = round(mean(hipertensao == "Sim") * 100, 1), # % hipertensos
    prop_obesidade = round(mean(obesidade == "Sim") * 100, 1)      # % obesos
  )



#----------------------------------
# MODELO DE REGRESSÃO LOGÍSTICA
#----------------------------------

# Ajusta modelo logístico para identificar fatores de risco
modelo_covid <- glm(
  caso_grave ~ idade + sexo + diabetes + hipertensao + obesidade,
  data = dados_covid,
  family = binomial    # Indica que Y é binária
)

summary(modelo_covid)



#----------------------------------
# EXTRAIR OR (ODDS RATIOS) COM IC 95%
#----------------------------------
# tidy() transforma os resultados em uma tabela organizada
# exponentiate = TRUE converte os coeficientes em OR
#----------------------------------
# COMO INTERPRETAR:
#----------------------------------
# term = variável
# estimate = OR (Odds Ratio / Razão de Chances)
# conf.low e conf.high = IC 95% para o OR
# p.value = p-valor (significativo se < 0.05)
#
# EXEMPLOS DE INTERPRETAÇÃO:
#
# idade (OR ≈ 1.04):
# → Para cada ano a mais de idade, a chance de caso grave aumenta ~4%
# → Para 10 anos a mais: (1.04)^10 ≈ 1.48 → 48% mais chance
#
# sexoMasculino (OR ≈ 1.35):
# → Homens têm ~35% mais chance de caso grave que mulheres
#
# diabetesSim (OR ≈ 2.2):
# → Diabéticos têm ~2.2 vezes mais chance de caso grave
# → Ou seja, ~120% mais chance que não diabéticos
#
# IC 95%:
# → Se o IC NÃO inclui 1, o fator é estatisticamente significativo
# → Se o IC inclui 1, não podemos afirmar que há associação
#----------------------------------

resultados_or <- tidy(modelo_covid, conf.int = TRUE, exponentiate = TRUE)
print(resultados_or)


#----------------------------------
# VISUALIZAÇÃO: FOREST PLOT
#----------------------------------
# O Forest Plot é uma forma visual de mostrar os ORs e seus ICs
#----------------------------------
# COMO LER O FOREST PLOT:
#
# - Linha tracejada vertical = OR = 1 (sem efeito)
# - Pontos à DIREITA da linha = fatores de RISCO (OR > 1)
# - Pontos à ESQUERDA da linha = fatores de PROTEÇÃO (OR < 1)
# - Barras horizontais = IC 95%
# - Se a barra CRUZA a linha vertical → NÃO é significativo
# - Se a barra NÃO cruza → É significativo
#----------------------------------

# Prepara dados 
dados_forest <- resultados_or %>%
  filter(term != "(Intercept)") %>%    # Remove o intercepto
  mutate(
    # Cria legendas:
    variavel = case_when(
      term == "idade" ~ "Idade (por ano)",
      term == "sexoMasculino" ~ "Sexo Masculino",
      term == "diabetesSim" ~ "Diabetes",
      term == "hipertensaoSim" ~ "Hipertensão",
      term == "obesidadeSim" ~ "Obesidade"
    )
  )

print(dados_forest)

dev.new()

# Cria o gráfico Forest Plot
ggplot(dados_forest, aes(x = estimate, y = reorder(variavel, estimate))) +
  
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") + # Linha vertical em OR = 1 (sem efeito)
  
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) + # Barras de erro (IC 95%)
  
  geom_point(size = 3, color = "steelblue") + # Pontos (OR)
  
  # Títulos
  labs(title = "Fatores Associados a COVID-19 Grave",
       subtitle = "Razões de Chances (OR) com IC 95%",
       x = "OR (Odds Ratio)", 
       y = "") +
  theme_minimal()

pause()


#----------------------------------
# CONCLUSÃO PARA GESTORES
#----------------------------------
# Com base nos resultados:
#
# "Os principais fatores de risco para COVID-19 grave são:
#  - Idade avançada
#  - Presença de diabetes
#  - Hipertensão
#  - Obesidade
#  - Sexo masculino
#----------------------------------


#----------------------------------
# CASO 2: COMPARAÇÃO DE TRATAMENTOS (ENSAIO CLÍNICO)
#----------------------------------
# CONTEXTO:
#----------------------------------
# # Um ensaio clínico testou 3 grupos:
# - Placebo (grupo controle)
# - Droga A (novo medicamento)
# - Droga B (outro novo medicamento)
#
# O desfecho é a REDUÇÃO da pressão arterial após 8 semanas.
#
# PERGUNTA DE PESQUISA:
# "Qual droga é mais efetiva para o tratamento de PA?"
#
# TIPO DE DESFECHO:
# Contínuo (redução da PA) + 3 grupos → ANOVA
#----------------------------------


#----------------------------------
# DADOS: Ensaio Clínico - Dados simulados de 50 pacientes em cada grupo
#----------------------------------

set.seed(123)
n_por_grupo <- 50  

dados_trat <- tibble(
  
  tratamento = factor(
    rep(c("Placebo", "Droga A", "Droga B"), each = n_por_grupo),
    levels = c("Placebo", "Droga A", "Droga B")  # Define ordem das drogas
  ),
  # PA basal - Antes do tratamento
  pa_basal = round(rnorm(n_por_grupo * 3, 150, 10))
) %>%
  mutate(
    # Redução da hipertensão após 8 semanas - "valores simulados e fixados como média de nomralização da PA"
    reducao = case_when(
      tratamento == "Placebo" ~ rnorm(n(), mean = 2, sd = 5),   # Placebo - média fixa em 2%
      tratamento == "Droga A" ~ rnorm(n(), mean = 15, sd = 6),  # Droga A - média fixa em 15%
      tratamento == "Droga B" ~ rnorm(n(), mean = 20, sd = 7)   # Droga B - média fixa em 20%
    )
  )

head(dados_trat)

table(dados_trat$tratamento)


#----------------------------------
# ANÁLISE DESCRITIVA POR GRUPO
#----------------------------------
# OBSERVE:
# → Qual tratamento tem a maior média de redução?
# → As diferenças parecem grandes?
#----------------------------------

dados_trat %>%
  group_by(tratamento) %>%
  summarise(
    n = n(),                              # Número de pacientes
    media = round(mean(reducao), 1),      # Média de redução
    dp = round(sd(reducao), 1),           # Desvio-padrão
    minimo = round(min(reducao), 1),      # Valor mínimo
    maximo = round(max(reducao), 1)       # Valor máximo
  )

#----------------------------------
# VISUALIZAÇÃO: BOXPLOT
#----------------------------------
# COMO LER O BOXPLOT:
# - Linha horizontal dentro da caixa = MEDIANA
# - Caixa = 50% dos dados (do 1º ao 3º quartil)
# - Linhas verticais (bigodes) = extensão até 1.5 x IQR
# - Pontos fora = valores atípicos
# - Pontos coloridos = dados individuais
#----------------------------------

ggplot(dados_trat, aes(x = tratamento, y = reducao, fill = tratamento)) +

  geom_boxplot(alpha = 0.7) +

  geom_jitter(width = 0.2, alpha = 0.4) +   # Pontos individuais (para ver a distribuição):
  
  geom_hline(yintercept = 0, linetype = "dashed") + # Linha horizontal em y = 0 (sem redução):
  
  scale_fill_brewer(palette = "Set2") +
  
  labs(title = "Redução da Hipertensão por Tratamento",
       subtitle = "Após 8 semanas de tratamento",
       x = "", 
       y = "Redução da PA (mmHg)") +
  theme_minimal() +
  theme(legend.position = "none")   # Remover legenda (redundante)

pause()

#----------------------------------
# ANOVA (Análise de Variância)
#----------------------------------
# A ANOVA testa se PELO MENOS um dos grupos tem média diferente dos outros
#
# H0: μ_placebo = μ_drogaA = μ_drogaB (todas as médias são iguais)
# H1: Pelo menos uma média é diferente
#----------------------------------
# COMO LER:
#----------------------------------
# Df = graus de liberdade
# Sum Sq = soma dos quadrados
# Mean Sq = média dos quadrados
# F value = estatística F (quanto maior, mais evidência de diferença)
# Pr(>F) = p-valor
#
# Se Pr(>F) < 0.05:
# → Rejeitamos H0
# → Há diferença significativa entre PELO MENOS dois grupos
# → MAS não sabemos QUAIS grupos diferem! Para isso, usamos Tukey.
#----------------------------------

modelo_anova <- aov(reducao ~ tratamento, data = dados_trat)

summary(modelo_anova)


#----------------------------------
# TESTE DE TUKEY (Comparações Múltiplas)
#----------------------------------
# O teste de Tukey compara TODOS os pares de grupos
#----------------------------------
# COMO LER:
#
# diff = diferença entre as médias dos dois grupos
# lwr = limite inferior do IC 95% para a diferença
# upr = limite superior do IC 95% para a diferença
# p adj = p-valor AJUSTADO para múltiplas comparações
#
# Se p adj < 0.05 → Esses dois grupos diferem significativamente
#
# EXEMPLO:
# Droga A - Placebo: diff = 13, p adj < 0.001
# → Droga A reduz em média 13 mmHg A MAIS que o placebo
# → Diferença SIGNIFICATIVA
#
# Droga B - Droga A: diff = 5, p adj = ?
# → Se p < 0.05: Droga B é melhor que Droga A
# → Se p >= 0.05: Não há evidência de diferença entre as drogas
#----------------------------------

TukeyHSD(modelo_anova)

#----------------------------------
# ALTERNATIVA: ANOVA usando lm()
#----------------------------------
# Também é possível fazer a ANOVA usando a função lm() (modelo linear).
# O resultado é equivalente, mas o summary() mostra informações adicionais.
#----------------------------------
# COMO LER:
#----------------------------------
# (Intercept) = média do grupo de referência (Placebo)
# tratamentoDroga A = diferença entre Droga A e Placebo
# tratamentoDroga B = diferença entre Droga B e Placebo
#
# O p-valor de cada coeficiente testa se aquele grupo difere do Placebo.
# O F-statistic no final é o mesmo da ANOVA.
#----------------------------------

mod_lm <- lm(reducao ~ tratamento, data = dados_trat)
summary(mod_lm)



#----------------------------------
# CASO 3: SOBREVIDA EM PACIENTES ONCOLÓGICOS
#----------------------------------
# CONTEXTO:
#----------------------------------
# Um hospital oncológico quer comparar a sobrevida de pacientes
# tratados com quimioterapia convencional vs imunoterapia.
#
# PERGUNTA DE PESQUISA:
# "Há diferença na sobrevida entre os dois tratamentos?"
# "Quais fatores afetam a sobrevida?"
#
# TIPO DE DESFECHO:
# Tempo até evento (com censura) → Análise de Sobrevivência
#----------------------------------


#----------------------------------
# Dados simulados de 150 Pacientes Oncológicos
#----------------------------------

set.seed(789)
n_onco <- 150 

dados_onco <- tibble(
  id = 1:n_onco,
  
  tratamento = sample(c("Quimio", "Imuno"), n_onco, replace = TRUE), # Tratamento

  idade = round(pmin(pmax(rnorm(n_onco, 60, 10), 40), 80)),   # Idade entre 40 e 80 anos

  estadio = sample(c("II", "III", "IV"), n_onco, replace = TRUE, prob = c(0.3, 0.4, 0.3))   # Estadio do câncer
) %>%
  mutate(
    # Taxa de risco (menor = melhor prognóstico)
    taxa = ifelse(tratamento == "Imuno", 0.02, 0.03) *   # Imuno melhor que Quimio
           case_when(
             estadio == "II" ~ 0.6,                      # Estadio II: menor risco
             estadio == "III" ~ 1,                       # Estadio III: risco médio
             TRUE ~ 1.5                                  # Estadio IV: maior risco
           ),
    
    tempo_evento = rexp(n_onco, rate = taxa),            # Tempo simulado até evento 
    
    tempo_censura = runif(n_onco, 24, 60),               # Tempo simulado de censura variável ?? alguns saem antes
    
    tempo = pmin(tempo_evento, tempo_censura),           # Tempo observado
    
    status = as.integer(tempo_evento <= tempo_censura)   # Status: 1 = ocorrência do evento, 0 = censurado
  )


head(dados_onco)

table(dados_onco$status) # Qtdade eventos e censuras

table(dados_onco$tratamento, dados_onco$status) # Distribuição por tratamento


#----------------------------------
# CRIAR OBJETO DE SOBREVIVÊNCIA
#----------------------------------

surv_onco <- Surv(time = dados_onco$tempo, event = dados_onco$status)


#----------------------------------
# CURVAS DE KAPLAN-MEIER POR TRATAMENTO
#----------------------------------
# OBSERVE:
# → Qual curva está MAIS ALTA? (melhor sobrevida)
# → As curvas se separam ao longo do tempo?
#----------------------------------

km_onco <- survfit(surv_onco ~ tratamento, data = dados_onco)

print(km_onco)

plot(km_onco,
     main = "Sobrevida por Tratamento",
     xlab = "Tempo (meses)",
     ylab = "Probabilidade de Sobrevida",
     col = c("coral", "steelblue"),
     lwd = 2)

# Adicionar legenda:
legend("bottomleft", 
       legend = c("Imuno", "Quimio"),
       col = c("coral", "steelblue"), 
       lwd = 2)

pause()



#----------------------------------
# TESTE LOG-RANK
#----------------------------------
# Testa se há diferença significativa entre as curvas
#---------------------------------- 
# COMO LER
#----------------------------------
# Se p < 0.05 → Há diferença significativa entre os tratamentos
#----------------------------------

survdiff(surv_onco ~ tratamento, data = dados_onco)


#----------------------------------
# MODELO DE COX
#----------------------------------
# O modelo de Cox identifica fatores que afetam a sobrevida
#----------------------------------

modelo_cox <- coxph(surv_onco ~ tratamento + idade + estadio, data = dados_onco)

summary(modelo_cox)

#----------------------------------
# VERIFICAÇÃO DO PRESSUPOSTO DE PROPORCIONALIDADE
#----------------------------------
# O modelo de Cox assume que os Hazard Ratios são CONSTANTES ao longo do tempo.
# Isso é chamado de "pressuposto de riscos proporcionais".
# Devemos testar se esse pressuposto é atendido.
#----------------------------------
# COMO LER:
#----------------------------------
# Se p > 0.05 para uma variável → Pressuposto atendido para essa variável
# Se p < 0.05 → O efeito dessa variável pode variar ao longo do tempo
#               (violação do pressuposto)
#
# GLOBAL: testa o modelo como um todo
#----------------------------------
# COMO INTERPRETAR O GRÁFICO:
#----------------------------------
# → Se a linha for aproximadamente HORIZONTAL, o pressuposto é atendido
# → Se a linha tiver inclinação clara, o efeito da variável muda com o tempo
# → A linha tracejada mostra o intervalo de confiança
#----------------------------------
# INTERPRETAÇÃO DOS HRs:
#
# tratamentoQuimio: HR > 1
# → Quimio tem MAIOR risco de evento que Imuno (referência)
# → Ex: HR = 1.5 significa 50% mais risco de óbito
#
# idade: HR por ano
# → Ex: HR = 1.02 significa 2% mais risco para cada ano a mais
#
# estadioIII e estadioIV: comparados com estadio II
# → HR > 1 significa pior prognóstico
#----------------------------------

# Teste estatístico de proporcionalidade:
test_ph <- cox.zph(modelo_cox)
test_ph

# Checagem visual do pressuposto:
plot(test_ph)

# Ver apenas os Hazard Ratios (HR):
exp(coef(modelo_cox))

pause()


#----------------------------------
# CONCLUSÃO PARA O HOSPITAL
#----------------------------------
# Com base nos resultados, você poderia concluir:
#
# "A imunoterapia apresentou melhor sobrevida que a quimioterapia
#  convencional nesta amostra de pacientes.
#
#  Outros fatores que afetam negativamente a sobrevida incluem:
#  - Idade mais avançada
#  - Estadio mais avançado do câncer (especialmente estadio IV)
#----------------------------------