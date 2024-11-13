' 
Universidade Federal do Rio Grande do Norte
Disciplina: Machine Learning para Geógrafos
Professor: Dr. Max Anjos (https://github.com/ByMaxAnjos)

Aula: Aplicando o terceiro modelo de AM: Cluser analysis.
Objetivo desta atividade: resolver um probelma de não supervisionado de cluster a partir 
do banco de dados da Motor Trend US de 1974. Eles incluem o consumo de combustível e 10 aspectos de design 
e desempenho automobilístico para 32 veículos (modelos de 1973-74).

Data: 13.11.2024

Baixar os dados: no R, data("mtcars")
Fonte dos dados: mtcars. Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391–411.
'

# Carregar as bibliotecas necessárias ---------------
library(tidyverse)
library(tidymodels)
library(cluster)
library(factoextra)
library(plotly)
library(scales)

# Carregar e visualizar e manipular os dados  ----------------------------------------------------------

data("mtcars") 

# Renomear as colunas para português
dados_carro <- mtcars %>%
  rename(
    km_por_litro = mpg,      # Milhas por galão para quilômetros por litro
    cilindros = cyl,         # Número de cilindros
    deslocamento = disp,     # Deslocamento (polegadas cúbicas)
    potencia = hp,           # Potência (cavalos de força)
    relacao_eixo = drat,     # Relação do eixo traseiro
    peso = wt,               # Peso (mil libras)
    tempo_1_4_milha = qsec,  # Tempo de 1/4 de milha (segundos)
    motor = vs,              # Tipo de motor (0 = V-shaped, 1 = straight)
    transmissao = am,        # Tipo de transmissão (0 = automática, 1 = manual)
    marchas = gear,          # Número de marchas
    carburadores = carb      # Número de carburadores
  )

# Pré-processamento dos dados ---------------------------------------------

## Normalizar/scalonizar etc..
dados_normalizados <- scale(dados_carro)

### Heatmap para Visualizar Similaridades
heatmap(dados_normalizados, main = "Heatmap explorando similaridades nos dados")

# Método 1:Hierarchical Clustering ----------------------------------------

### Dendrograma 
distancia <- dist(dados_normalizados, method = "euclidean")
modelo_hierarquico <- hclust(distancia, method = "ward.D2")
fviz_dend(modelo_hierarquico)
fviz_dend(modelo_hierarquico,cex = 0.5, k = 4, color_labels_by_k = TRUE, 
          main = "Dendrograma - Hierarchical Clustering")

# Método 2: K-means para cluster analysis -------------

# 1. Identifcar o número de cluster k
set.seed(123)
fviz_nbclust(dados_normalizados, kmeans, method = "wss") +
  labs(title = "Método Elbow para Determinar o Número de Clusters")

# 2. perform k-means clustering with k = 4 clusters
set.seed(123)
kmeans_modelo <- kmeans(dados_normalizados, centers = 4)

# Visualização e análise dos resultados ----------------------------------------

## Gráfico de dispersão dos clusters
fviz_cluster(kmeans_modelo, data = dados_carro) +
  labs(title = "Clusters Identificados pelo K-Means")

fviz_mclust(kmeans_modelo, data = dados_carro) +
  labs(title = "Clusters Identificados pelo K-Means")

## Prepare o dataframe 
cluster_tabela <- data.frame(cluster = dados_cluster$cluster) %>% 
  bind_cols(dados_carro) %>% 
  mutate(cluster = as.factor(cluster))

## Estatística básica de cada cluster
resumo_clusters <- cluster_tabela %>% 
  group_by(cluster) %>%
  summarise(
    consumo_medio = mean(km_por_litro),
    cilindros_medio = mean(cilindros),
    peso_medio = mean(peso)
  )       
print(resumo_clusters)

## Visualization 3d
# Escolher variáveis significativas para eixos 3D
plot_ly(cluster_tabela, x = ~potencia, y = ~km_por_litro, z = ~peso, 
        color = ~cluster, colors = "Set1", type = "scatter3d", mode = "markers") %>%
  layout(
    title = "Visualização 3D dos Clusters",
    scene = list(
      xaxis = list(title = "Potência"),
      yaxis = list(title = "Km/L"),
      zaxis = list(title = "Peso (mil libras)")
    )
  )
