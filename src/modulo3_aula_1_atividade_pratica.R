#------------------------------------------------------
# CURSO ANALISE DE DADOS PARA O SUS
# ATIVIDADE PRÁTICA - MOD.3 AULA 1 
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
library(DT) # cria tabelas em janela externa
library(plotly)
library(highcharter)
library(echarts4r)
#pacotes para Analises
library(survival) # analises sobrevivência
library(lme4) # modelos multiníveis
library(forecast) # Séries temporais


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
#-----------------------------------------
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
# DEFINIR DIRETÓRIO DE TRABALHO 
#----------------------------------
curso <- "G:\\Documentos\\IA\\Fiocruz\\Atividades_FIOCRUZ_Curso_AD_para_SUS"
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
# set.seed() - semente para fixar os resultados
#-------------------------------------
set.seed(123456)

#-------------------------------------
# PARÂMETROS DA SIMULAÇÃO
#-------------------------------------
media_populacional <- 26    # A média REAL do IMC na população é 26 kg/m²
desvio_populacional <- 2    # O desvio-padrão REAL é 2 kg/m²


# para demonstrar como a inferência funciona
K <- 500   # Vamos coletar 500 amostras diferentes
n <- 100   # Cada amostra terá 100 pessoas


#-------------------------------------
# SIMULAÇÃO: Coletando 500 amostras e calculando a média de cada uma
#-------------------------------------
# 1. rnorm(n, media_populacional, desvio_populacional)
#    → Gera n=100 valores simulando IMC de 100 pessoas
#    → Esses valores seguem uma distribuição normal com média 26 e DP 2
#
# 2. mean(...)
#    → Calcula a média desses 100 valores (média de UMA amostra)
#
# 3. replicate(K, ...)
#    → Repete esse processo K=500 vezes
#    → Resultado: um vetor com 500 médias amostrai
#-------------------------------------

medias_amostrais <- replicate(K, mean(rnorm(n, media_populacional, desvio_populacional)))

head(medias_amostrais, 10)

#-------------------------------------
# VISUALIZAÇÃO DOS DADOS NO HISTOGRAMA
#-------------------------------------

# Fechar todos os dispositivos gráficos
graphics.off()

ajustar_margens()

hist(medias_amostrais,                              # Dados para o histograma
     col = "lightblue",                             # Cor das barras
     main = "Distribuição das Médias Amostrais (n=100)",  # Título
     xlab = "Média do IMC (kg/m²)",                 # Rótulo do eixo X
     ylab = "Frequência",                           # Rótulo do eixo Y
     breaks = 30)                                   # Número de barras

# Adicionar linha vertical na média verdadeira (26)
abline(v = media_populacional, col = "red", lwd = 2)

pause()


#-------------------------------------
# VISUALIZAÇÃO DOS DADOS NO HISTOGRAMA
#-------------------------------------

mean(medias_amostrais) # média das amostras

sd(medias_amostrais) # desvio padrão das amostras

desvio_populacional / sqrt(n) # margem erro padrão



#-------------------------------------
# ATIVIDADE 1.2: O que acontece com amostras PEQUENAS?
#-------------------------------------

set.seed(42)

populacao <- rexp(10000, rate = 1)

ajustar_margens()

hist(populacao, breaks = 30, main = "População Original (Assimétrica)", 
     col = "lightblue", border = "white")

pause()

#-----------------------------------------
# Função para calcular médias amostrais
# Esta função:
#   1. Pega uma amostra de tamanho 'tam_amostra' da população
#   2. Calcula a média dessa amostra
#   3. Repete isso 'n_amostras' vezes
#-----------------------------------------

calcula_medias <- function(tam_amostra, n_amostras = 1000) {
  replicate(n_amostras, mean(sample(populacao, tam_amostra)))
}

par(mfrow = c(2, 2)) # par(mfrow = c(2, 2)) divide a tela em 4 partes (2 linhas x 2 colunas)

ajustar_margens()

# Gráfico 1: A população original (assimétrica)
hist(populacao, breaks = 30, main = "População (Assimétrica)", 
     col = "lightblue", border = "white")

# Gráfico 2: Médias de amostras com n=5 (muito pequeno)
hist(calcula_medias(5), breaks = 30, main = "Médias (n=5)", 
     col = "lightgreen", border = "white")

# Gráfico 3: Médias de amostras com n=30
hist(calcula_medias(30), breaks = 30, main = "Médias (n=30)", 
     col = "lightgreen", border = "white")

# Gráfico 4: Médias de amostras com n=100
hist(calcula_medias(100), breaks = 30, main = "Médias (n=100)", 
     col = "lightgreen", border = "white")

pause()

# Voltar para 1 gráfico por tela
par(mfrow = c(1, 1))



#-------------------------------------
# ATIVIDADE 2: Intervalo de Confiança (IC)
#-------------------------------------
#
# O QUE É INTERVALO DE CONFIANÇA?
#-------------------------------------
# É uma faixa de valores que provavelmente contém o parâmetro da população.
#-------------------------------------



#-------------------------------------
# CÁLCULO DO IC PARA CADA AMOSTRA
#-------------------------------------
# Valor crítico para IC de 95%
# qnorm(0.975) retorna o valor z onde 97.5% da distribuição normal está abaixo
# Para IC 95%, usamos z = 1.96
#-------------------------------------

z_95 <- qnorm(0.975)
z_95  # Deve ser aproximadamente 1.96

# Erro padrão (já calculamos antes)
erro_padrao <- desvio_populacional / sqrt(n)
erro_padrao

# Cria data frame com todas as informações
df_ic <- tibble(
  amostra = 1:K,                                         # Número da amostra (1 a 500)
  media = medias_amostrais,                              # Média de cada amostra
  LI = media - z_95 * erro_padrao,                       # Limite Inferior do IC
  LS = media + z_95 * erro_padrao,                       # Limite Superior do IC
  contem_media = LI < media_populacional & LS > media_populacional  # O IC contém 26?
)


head(df_ic)

#-------------------------------------
# Verifica quantos ICs contêm a média verdadeira?
#-------------------------------------

table(df_ic$contem_media) # Contagem quantos verdadeiros e falsos 

mean(df_ic$contem_media) # Calcula média de ICs verdadeiros

#-------------------------------------
# VISUALIZAÇÃO: Gráfico dos ICs 
# visualizar os primeiros 100 intervalos de confiança:
#-------------------------------------

df_ic %>%
  # Pega os 100 primeiros
  slice(1:100) %>%  
  ggplot(aes(x = amostra, y = media, color = contem_media)) +
  geom_point(size = 2) +                                 
  geom_errorbar(aes(ymin = LI, ymax = LS), width = 0.3) + 
  geom_hline(yintercept = media_populacional, color = "black", linewidth = 0.8) +  
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "red")) +
  labs(title = "Intervalos de Confiança 95%",
       subtitle = "Linha preta = média verdadeira (26)",
       x = "Amostra", 
       y = "IMC (kg/m2)") +
  theme_minimal() +
  theme(legend.position = "none")

pause()

#-------------------------------------
# ATIVIDADE 3: Teste t para Uma Amostra 
# ------------------------------------
# ESTRUTURA DO TESTE:
# ------------------------------------
# H0 (hipótese nula): μ = valor_referência (não há diferença)
# H1 (hipótese alternativa): μ ≠ valor_referência (há diferença)
#
# DECISÃO:
# - Se p-valor < 0.05: rejeitamos H0 → há evidência de diferença
# - Se p-valor >= 0.05: não rejeitamos H0 → não há evidência suficiente
#-------------------------------------

# AMostra de 20 pesos em gramas de rec~em-nascidos
peso_rn <- c(3265, 3260, 3245, 3484, 4146, 3323, 3649, 3200, 
             3031, 2069, 2581, 2841, 3609, 2838, 3541, 2759, 
             3248, 3314, 3101, 2834)

# Quantos indivíduos estãao sendo testados
length(peso_rn)

summary(peso_rn)   # Mínimo, quartis, média, mediana, máximo
sd(peso_rn)        # Desvio-padrão

# Média da amostra
mean(peso_rn)

# ------------------------------------
# TESTE t: A média difere de 3200g?
# ------------------------------------
# H0: μ = 3200 (a média é igual a 3200g)
# H1: μ ≠ 3200 (a média é diferente de 3200g)
# LEITURA:
# t = valor da estatística t (quanto maior em módulo, mais evidência contra H0)
# df = graus de liberdade (n - 1 = 19)
# p-value = probabilidade de observar esses dados se H0 fosse verdade
#
# Se p-value < 0.05 → Rejeitamos H0 → A média difere de 3200g
# Se p-value >= 0.05 → Não rejeitamos H0 → Não há evidência de diferença
#
# 95 percent confidence interval: é o IC para a média
# sample estimates: é a média amostral
#-------------------------------------

t.test(peso_rn, mu = 3200)

#-------------------------------------
# ATIVIDADE 4: Teste t para Duas Amostras 
# ------------------------------------
# ESTRUTURA
# ------------------------------------
# H0: μ1 = μ2 (as médias são iguais)
# H1: μ1 ≠ μ2 (as médias são diferentes)
#-------------------------------------

# DADOS: Colesterol medido por dois métodos
# Tabela de valores para AutoAnalyzer (5 valores) e Microenzimatic (5)
colesterol <- tibble(
  metodo = rep(c("AutoAnalyzer", "Microenzimatic"), each = 5),  # 5 medições de cada método
  valor = c(177, 193, 195, 209, 226,     # Valores do AutoAnalyzer
            192, 197, 200, 202, 209)     # Valores do Microenzimatic
)

print(colesterol)

# Calcula "n" - média e desvio-padrão 
colesterol %>%
  group_by(metodo) %>%                    # Agrupar por método
  summarise(
    n = n(),                              # Contagem
    media = mean(valor),                  # Média
    dp = sd(valor)                        # Desvio-padrão
  )

# Compara os valores entre os diferentes métodos
# INTERPRETAÇÃO:
# → Veja o p-valor
# → Se p < 0.05: há diferença significativa entre os métodos
# → Se p >= 0.05: não há evidência de diferença entre os métodos

# Testando a lib knitr - broom
resultado_teste <- t.test(valor ~ metodo, data = colesterol)

tabela_resultados <- tidy(resultado_teste)

view(kable(tabela_resultados,
    caption = "Teste lib knitr/broom - Resultados teste t",
    digits = 3,
    format = "markdown") # pode ser html, latex, pipe e simple - formato pandoc, rst - estruturado, jira - para colar no Jira e org - Emacs Org-mode
)

pause()

# Testando a library DT
datatable(tabela_resultados,
    caption = "Teste DT - Resultados teste t",
    options = list(
        pageLength = 5,  # Mostra 5 linhas por página
        autoWidth = TRUE, # Ajusta largura automaticamente
        dom = 'Bfrtip'    # Adiciona botões
    ),
    class = 'cell-border stripe hover' # Estilo
) %>%
    formatRound(columns = 1:8, digits = 3) %>% 
    htmlwidgets::saveWidget("temp.html")
browseURL("temp.html")

# - Opção 2: Usar viewer padrão mas com opção de navegador
# options(viewer = NULL)  # Desativa viewer interno
# datatable(tabela_resultados)  # Abre direto no navegador
# - Para voltar ao viewer padrão:
# options(viewer = rstudioapi::viewer)
pause()

#-------------------------------------
# ATIVIDADE 5: ANOVA (Análise de Variância) 
# ------------------------------------
# ESTRUTURA:
# ------------------------------------
# H0: μ1 = μ2 = μ3 = ... (todas as médias são iguais)
# H1: pelo menos uma média é diferente
#
# ATENÇÃO: Se a ANOVA for significativa, ela NÃO diz QUAIS grupos diferem.
# Para isso, usamos o teste de Tukey (comparações múltiplas).
# ------------------------------------
# COMO LER O RESULTADO:
# ------------------------------------
# Df = graus de liberdade
# Sum Sq = soma dos quadrados
# Mean Sq = média dos quadrados
# F value = estatística F (quanto maior, mais evidência de diferença)
# Pr(>F) = p-valor
#
# Se Pr(>F) < 0.05 → Há diferença significativa entre PELO MENOS dois grupos
#-------------------------------------

set.seed(123)

# Cria dados de Pressão Arterial (PA) para 3 faixas etárias, cada faixa com 20 indivíduos
dados_pa <- tibble(
  faixa_etaria = factor(
    rep(c("Jovem", "Adulto", "Idoso"), each = 20),   
    levels = c("Jovem", "Adulto", "Idoso")           
  ),
  pressao = c(
    rnorm(20, mean = 115, sd = 10),   # 20 jovens: média 115, DP 10
    rnorm(20, mean = 125, sd = 12),   # 20 adultos: média 125, DP 12
    rnorm(20, mean = 135, sd = 15)    # 20 idosos: média 135, DP 15
  )
)

# head(dados_pa) # mostra primeiras linhas

dados_pa %>%
  group_by(faixa_etaria) %>%
  summarise(
    n = n(),                    # Número de pessoas
    media = mean(pressao),      # Média da PA
    dp = sd(pressao)            # Desvio-padrão
  )

# Visualização com ggplot
dados_pa %>%
  ggplot(aes(x = faixa_etaria, y = pressao, fill = faixa_etaria)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +           # Boxplot
  geom_jitter(width = 0.2, alpha = 0.4, show.legend = FALSE) + # Pontos individuais
  labs(title = "Pressão Arterial por Faixa Etária", 
       x = "", 
       y = "PA Sistólica (mmHg)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") 

pause()

# aov() = Analysis of Variance
# Fórmula: variável_resposta ~ variável_grupo

modelo_anova <- aov(pressao ~ faixa_etaria, data = dados_pa)

# Ver resultado:
summary(modelo_anova)

#-------------------------------------
# TESTE DE TUKEY (Comparações Múltiplas)
# ------------------------------------
# Se a ANOVA for significativa, usamos Tukey para saber QUAIS grupos diferem:
# ------------------------------------
# COMO LER O RESULTADO:
# ------------------------------------
# diff = diferença entre as médias dos grupos
# lwr = limite inferior do IC 95% para a diferença
# upr = limite superior do IC 95% para a diferença
# p adj = p-valor ajustado
#
# Se p adj < 0.05 → Esses dois grupos diferem significativamente
# ------------------------------------

TukeyHSD(modelo_anova)

#-------------------------------------
# ATIVIDADE 6: Teste de Proporção
# ------------------------------------
# Quando queremos testar se uma PROPORÇÃO difere de um valor de referência.
# ------------------------------------
# TESTE DE PROPORÇÃO
# ------------------------------------
# prop.test(x, n, p)
#   x = número de "sucessos" (hipertensos)
#   n = tamanho da amostra
#   p = proporção esperada (referência)
#
# ------------------------------------
# INTERPRETAÇÃO:
# → Se p-valor < 0.05: a proporção difere significativamente de 25%
# → Se p-valor >= 0.05: não há evidência de que difira de 25%
# ------------------------------------

# Amostra de 200 indivíduos adultos, 60 hipertensos
# Teste se a proporção diferente de 25%

prop.test(x = 60, n = 200, p = 0.25)

