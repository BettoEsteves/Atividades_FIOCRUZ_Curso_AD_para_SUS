#------------------------------------------------------
# CURSO ANALISE DE DADOS PARA O SUS
# ATIVIDADE PRÁTICA - MOD.1 AULA 2 
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
# ATIVIDADE 1: Trabalhando com Variáveis Aleatórias
#-------------------------------------
# Variáveis aleatórias são medições de características de interesse em um estudo.
# Elas podem ser classificadas como:
# - QUALITATIVAS: descrevem atributos (nominal ou ordinal)
# - QUANTITATIVAS: representam quantidades (discreta ou contínua)
#
#-------------------------------------
# 1.1 Criando o dataset dos recém-nascidos (dados utilizados na aula)
#-------------------------------------
# A função tibble() cria um data frame moderno com melhor visualização.
# Estamos criando dados de 20 recém-nascidos com seus pesos em gramas.
#-------------------------------------
# O que você vai praticar:
# • Criar um tibble com dados usando a função tibble()
# • Visualizar a estrutura dos dados com glimpse()
# • Categorizar variáveis contínuas usando mutate() e case_when()
# • Criar fatores ordenados com factor()
# • Contar frequências com count()

#-------------------------------------

tb_20_recem_nascidos <- tibble(
  item = 1:20,
  peso_gr = c(3265, 3260, 3245, 3484, 4146, 3323, 3649, 3200, 3031, 2069,
              2581, 2841, 3609, 2838, 3541, 2759, 3248, 3314, 3101, 2834)
)

glimpse(tb_20_recem_nascidos)

view(tb_20_recem_nascidos)


#-------------------------------------
# 1.2 Criando variáveis categóricas a partir de variáveis contínuas
#-------------------------------------
# É comum categorizar variáveis quantitativas para facilitar análises.
# Aqui transformamos o peso (contínuo) em faixas (ordinal).
#
# mutate(): adiciona ou modifica colunas
# case_when(): aplica condições múltiplas (como if-else encadeado)
# factor(): converte para fator com níveis ordenados
#-------------------------------------

tb_20_recem_nascidos <- tb_20_recem_nascidos %>%
  mutate(
    # Cria as faixas de peso em gramos
    faixa_peso_gr = case_when( 
      peso_gr < 2500 ~ "Baixo peso",
      peso_gr >= 2500 & peso_gr < 3500 ~ "Peso adequado",
      peso_gr >= 3500 ~ "Acima do peso esperado"
    ),
    # Define a ordem lógica das categorias
    faixa_peso_gr = factor(faixa_peso_gr,
                           levels = c("Baixo peso", "Peso adequado", "Acima do peso esperado"),
                           ordered = TRUE)
  )

# Verifica a distribuição na faixas
View(tb_20_recem_nascidos %>% count(faixa_peso_gr))


#-------------------------------------
# ATIVIDADE 2: Medidas de Locação (Tendência Central)
#-------------------------------------
# Medidas de locação indicam onde os dados estão "centralizados".
# São fundamentais para resumir e comparar conjuntos de dados.
#-------------------------------------
# 2.1 Média aritmética
#-------------------------------------
# A média é a soma de todos os valores dividida pelo número de observações.
# Fórmula: x̄ = Σxi / n
# ATENÇÃO: A média é sensível a valores extremos (outliers)!
#-------------------------------------
# O que você vai praticar:
#   • Calcular média aritmética com mean()
# • Calcular mediana com median()
# • Comparar média e mediana para avaliar assimetria da distribuição
# • Calcular quantis (percentis e quartis) com quantile()
# • Calcular média ponderada manualmente
#-------------------------------------

media_peso_gr <- tb_20_recem_nascidos %>%
  summarise(media_peso_gr = mean(peso_gr)) %>%
  pull(media_peso_gr)

view(media_peso_gr)

#-------------------------------------
# 2.2 Mediana
#-------------------------------------
# A mediana é o valor central dos dados ordenados.
# - 50% dos valores estão abaixo e 50% acima da mediana
# - VANTAGEM: É robusta a valores extremos (outliers)
#-------------------------------------

mediana_peso_gr <- tb_20_recem_nascidos %>%
  summarise(mediana_peso_gr = median(peso_gr)) %>%
  pull(mediana_peso_gr)

view(mediana_peso_gr)  

#-------------------------------------
# 2.3 Comparação entre média e mediana
#-------------------------------------
# A relação entre média e mediana indica a forma da distribuição:
# - média > mediana: assimetria positiva (cauda à direita)
# - média < mediana: assimetria negativa (cauda à esquerda)
# - média ≈ mediana: distribuição aproximadamente simétrica
#-------------------------------------

compara_media_mediana <- media_peso_gr - mediana_peso_gr
view(compara_media_mediana)


#-------------------------------------
# 2.4 Quantis: percentis e quartis
#-------------------------------------
# Quantis dividem os dados ordenados em partes iguais:
# - Quartis (Q): dividem em 4 partes (Q1=25%, Q2=50%=mediana, Q3=75%)
# - Percentis (P): dividem em 100 partes (P10=10%, P90=90%, etc.)
#
# quantile(x, probs): calcula o quantil na probabilidade especificada
#-------------------------------------

quantis_peso_gr <- tb_20_recem_nascidos %>%
  summarise(
    P10 = quantile(peso_gr, 0.10),
    Q1  = quantile(peso_gr, 0.25),
    Q2  = quantile(peso_gr, 0.50),
    Q3  = quantile(peso_gr, 0.75),
    P90 = quantile(peso_gr, 0.90)
  )

# OBSERVAÇÃO - GABARITO DIFERE DO RESULTADO
# GABARITO: P10     Q1       Q2 (mediana) Q3      P90
#           2620.8  2837.0   3246.5       3466.5  3617.2
# RESULTADO:2741.2  2840.25  3246.5       3363.25 3613
view(quantis_peso_gr)


#-------------------------------------
# 2.5 Média aritmética ponderada
#-------------------------------------
# Na média ponderada, cada valor tem um "peso" diferente.
# Fórmula: x̄w = Σ(wi × xi) / Σwi
# Útil quando alguns valores são mais representativos que outros.
#-------------------------------------

# Adicionar pesos (w) aos dados conforme exemplo da aula

tb_20_recem_nascidos_podenrado <- tb_20_recem_nascidos %>%
  mutate(
    w = c(22, 40, 33, 22, 5, 31, 24, 35, 48, 58, 
          61, 20, 45, 22, 41, 35, 36, 11, 10, 25)   
  )

media_podenrada <- tb_20_recem_nascidos_podenrado %>%
  summarise(
    media_podenrada = sum(peso_gr * w) / sum(w)
  ) %>%
  pull(media_podenrada)

print(media_podenrada)

print(media_peso_gr - media_podenrada)



#-------------------------------------
# ATIVIDADE 3: Medidas de Dispersão
#-------------------------------------
# Medidas de dispersão indicam o "espalhamento" dos dados.
# Complementam as medidas de locação para descrição completa.
#
# Criar dataset de colesterol com dois métodos de medição (dados da aula)
# Este exemplo ilustra como métodos diferentes podem ter variabilidades 
# distintas
#-------------------------------------
# O que você vai praticar:
#   • Calcular amplitude (máximo - mínimo)
# • Calcular variância com var() e desvio-padrão com sd()
# • Calcular coeficiente de variação (CV) para comparar dispersões
# • Calcular intervalo interquartil (IQ = Q3 - Q1)
# • Usar group_by() para calcular estatísticas por grupo
#-------------------------------------

tb_colesterol <- tibble(
  metodo = rep(c("AutoAnalyzer", "Microenzimatic"), each = 5),
  valor = c(177, 193, 195, 209, 226, 192, 197, 200, 202, 209) 
)


#-------------------------------------
# 3.1 Amplitude
#-------------------------------------
# Amplitude = valor máximo - valor mínimo
# É a medida mais simples, mas muito sensível a outliers.
#-------------------------------------

amplitude <- tb_colesterol %>%
  group_by(metodo) %>%
  summarise(
    minimo = min(valor),
    maximo = max(valor),
    amplitude = max(valor) - min(valor)
  )

amplitude


#-------------------------------------
# 3.2 Variância e Desvio-padrão
#-------------------------------------
# VARIÂNCIA: média dos quadrados dos desvios em relação à média
# Fórmula: s² = Σ(xi - x̄)² / (n-1)
#
# DESVIO-PADRÃO: raiz quadrada da variância
# Fórmula: s = √s²
# Vantagem: está na mesma unidade dos dados originais
#
# Note: AutoAnalyzer tem maior variância (340) vs Microenzimatic (39.5)
#-------------------------------------


dispersao <- tb_colesterol %>%
  group_by(metodo) %>%
  summarise(
    media = mean(valor),
    variancia = var(valor),    
    desvio_padrao = sd(valor)  
  )

dispersao

#-------------------------------------
# 3.3 Coeficiente de Variação (CV)
#-------------------------------------
# O CV expressa o desvio-padrão como percentual da média.
# Fórmula: CV = (s / x̄) × 100
# VANTAGEM: Permite comparar dispersão entre variáveis de escalas diferentes
# INTERPRETAÇÃO: Quanto MENOR o CV, mais homogêneos são os dados
#
# AutoAnalyzer: 9.22% vs Microenzimatic: 3.14%
# O método Microenzimatic é mais preciso (menor variabilidade relativa)
#-------------------------------------

coeficiente_variacao <- dispersao %>%
  mutate(
    coeficiente_variacao_percentual = (desvio_padrao / media) * 100
  ) %>%
  select(metodo, coeficiente_variacao_percentual)

coeficiente_variacao

#-------------------------------------
# 3.4 Intervalo Interquartil (IQ ou IQR)
#-------------------------------------
# IQ = Q3 - Q1 (diferença entre terceiro e primeiro quartis)
# Representa a amplitude dos 50% centrais dos dados.
# VANTAGEM: Mais robusto que amplitude total (ignora os 25% extremos)
#
# OBSERVACAO: VALOR DIFEREN DO GABARITO
# GABARITO:
# Método Q1 Q3 IQ
# AutoAnalyzer 185.0 217.5 32.5
# Microenzimatic 194.5 205.5 11.0
#
# Resultado:
# A tibble: 2 × 4
#  metodo            Q1    Q3    IQ
#  <chr>          <dbl> <dbl> <dbl>
#  1 AutoAnalyzer     193   209    16
#  2 Microenzimatic   197   202     5
#-------------------------------------

intevalo_interquartil <- tb_colesterol %>%
  group_by(metodo) %>%
  summarise(
    Q1 = quantile(valor, 0.25),
    Q3 = quantile(valor, 0.75),
    IQ = Q3 - Q1
  )

intevalo_interquartil


#-------------------------------------
# ATIVIDADE 4: Função para Resumo Estatístico Completo
#-------------------------------------
# Criar funções personalizadas ajuda a padronizar análises e evitar repetição.
# A sintaxe {{ variavel }} permite passar nomes de colunas como argumentos.
#-------------------------------------
# O que você vai praticar:
# • Criar funções personalizadas em R
# • Usar a sintaxe {{ variavel }} para passar nomes de colunas
# • Combinar múltiplas estatísticas em um único resumo
#-------------------------------------
#
# OBSERVAÇÃO RESULTADO DIFERE DO GABARITO
# GABARITO:
# n   média   mediana  dp     CV%   mín   Q1    Q3     máx   IQ
# 20  3166.9  3246.5   424.3  13.4  2069  2837  3466.5 4146 629.5
# 
# RESULTADO:
#   n media   mediana   desvio_padrao  coeficiente_variacao_percent  minimo    Q1       Q3       máx   IQ
#  20 3166.9  3246.5    445.3353               14.06219              2069      2840.25  3363.25  4146  523#-------------------------------------
#
#-------------------------------------

sumario_estatistico <- function(dados, variavel) {
  dados %>% 
    summarise(
      n = n(),                                                    
      media = mean({{ variavel }}),                               
      mediana = median({{ variavel }}),                           
      desvio_padrao = sd({{ variavel }}),                         
      coeficiente_variacao_percent = (sd({{ variavel }}) / mean({{ variavel }})) * 100,  
      minimo = min({{ variavel }}),                               
      Q1 = quantile({{ variavel }}, 0.25),                        
      Q3 = quantile({{ variavel }}, 0.75),                        
      maximo = max({{ variavel }}),                               
      IQ = Q3 - Q1                                                    
      )
}

# aplica a funcao sumario_estatistico para os dados tb_20_recem_nascidos
view(sumario_estatistico(tb_20_recem_nascidos, peso_gr))



#-------------------------------------
# ATIVIDADE 5: Métodos Gráficos
#-------------------------------------
# Gráficos são essenciais para visualizar padrões que números não revelam.
# O ggplot2 usa a "gramática dos gráficos" - construção em camadas.
#-------------------------------------
# O que você vai praticar:
# • Criar gráfico de barras com geom_col() para variáveis categóricas
# • Criar box plot com geom_boxplot() para comparar distribuições
# • Criar histograma com geom_histogram() para variáveis contínuas
# • Adicionar linhas de referência com geom_vline()
# • Personalizar títulos e rótulos com labs()
#-------------------------------------


#-------------------------------------
# 5.1 Gráfico de barras - para variáveis categóricas
#-------------------------------------
# Ideal para mostrar frequências ou proporções de categorias.
# geom_col(): cria barras com alturas especificadas pelos dados
#-------------------------------------

grafico_barras <- tb_20_recem_nascidos %>%
  count(faixa_peso_gr) %>%
  mutate( percentual = n / sum(n) * 100) %>%
  ggplot( aes(x = faixa_peso_gr, y = percentual)) +
  geom_col(fill="steelblue") +
  geom_text(aes(label = paste(round(percentual, 1), "%")),
            vjust = -0.5, size = 4) +
  labs(
    title = "Distribuição de Recém Nascidos por Faixa de Peso/gr",
    x = "Faixa de Peso",
    y = "Percentual",
  ) + 
  theme_minimal()+ ylim(0,100)

grafico_barras
readline("Pressione Enter para próximo gráfico...")


# TESTES COM OUTRAS LIBS GRÁFICAS

# library(plotly)

grafico <- tb_20_recem_nascidos %>%
  count(faixa_peso_gr) %>%
  mutate(percentual = n / sum(n) * 100) %>%
  plot_ly(x = ~faixa_peso_gr, y = ~percentual, type = 'bar',
          text = ~paste(round(percentual, 1), '%'),
          marker = list(color = 'steelblue')) %>%
  layout(title = "plotly - Distribuição por Faixa de Peso",
         xaxis = list(title = "Faixa de Peso"),
         yaxis = list(title = "Percentual (%)"))

grafico  
readline("Pressione Enter para próximo gráfico...")


# library(highcharter)

hc <- tb_20_recem_nascidos %>%
  count(faixa_peso_gr) %>%
  hchart('column', hcaes(x = faixa_peso_gr, y = n),
         name = "Recém-nascidos") %>%
  hc_title(text = "highcharter - Distribuição por Faixa de Peso") %>%
  hc_colors(c("#7CB5EC"))

hc  # HTML interativo


# library(ggplot2)

dados_resumo <- tb_20_recem_nascidos %>%
  count(faixa_peso_gr, name = "n")

ggplot(dados_resumo, aes(x = faixa_peso_gr, y = n)) +
  geom_col(fill = "steelblue", color = "white") +
  labs(title = "ggplot2 - Distribuição por Faixa de Peso") +
  theme_classic()


readline("Pressione Enter para próximo gráfico...")


# library(echarts4r)

tb_20_recem_nascidos %>%
  count(faixa_peso_gr) %>%
  e_charts(faixa_peso_gr) %>%
  e_bar(n, name = "Contagem") %>%
  e_title("echarts4r - Distribuição por Faixa de Peso") %>%
  e_theme("westeros")

#-------------------------------------
# 5.2 Box plot - para comparar distribuições entre grupos
#-------------------------------------
# O box plot mostra simultaneamente:
# - Mediana (linha central)
# - Quartis Q1 e Q3 (limites da caixa)
# - Valores extremos (bigodes)
# - Outliers (pontos isolados)
#-------------------------------------


#-------------------------------------
# 5.3 Histograma - para distribuição de variáveis contínuas
#-------------------------------------
# O histograma divide os dados em intervalos (bins) e mostra a frequência.
# Permite visualizar a forma da distribuição (simétrica, assimétrica, etc.)
#-------------------------------------
