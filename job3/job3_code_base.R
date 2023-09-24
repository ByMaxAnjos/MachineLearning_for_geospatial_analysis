' 
Universidade Federal do Rio Grande do Norte
Disciplina: Técnicas de Apredizagem de Máquinas (AM) aplicadas à análise espacial
Professor: Dr. Max Anjos (https://github.com/ByMaxAnjos)

Aula: Aplicando o terceiro modelo de AM
Objetivo desta atividade: resolver um probelma de não supervisionado de cluster a partir 
do banco de dados da Motor Trend US de 1974. Eles incluem o consumo de combustível e 10 aspectos de design 
e desempenho automobilístico para 32 veículos (modelos de 1973-74)

Data: 25.09.2023

Baixar os dados: no R, data("mtcars")
Fonte dos dados: mtcars. Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391–411.
'

# Carregar as bibliotecas necessárias ---------------

library(tidyverse)
library(tidymodels)
library(cluster)
library(factoextra)
library(scales)

# Carregar e visualizar e manipular os dados  ----------------------------------------------------------

## Ler o arquivo csv seguindo: "caminho/para/seu/arquivo/name_dados.csv"
str(mtcars)

dados_carros_1 <- mtcars %>% slice(1:20) %>% select(-gear, -carb)
dados_carros_2 <- mtcars %>% slice(21:32) %>% select(-gear, -carb)
dados_carros_3 <- mtcars %>% select(gear, carb)

write_csv(dados_carros_3, "~/Documents/CO2CityMap/CO2CityMap/MLforGEO/MLforGEO/job3/dados_carros_3.csv")

# Manipular e Explorar os dados -------------------------------------------------------


## Juntar as linhas das tabelas com a bind_rows()

dados_juntar_linhas <- bind_rows(dados_carros_1, dados_carros_2)

## Juntar as colunas das tabaleas com a

dados_juntar_colunas <- bind_cols(dados_juntar_linhas, dados_carros_3)


## Renomear

dados_renomeados <- ...


# Pré-processamento dos dados ---------------------------------------------

# Verificar os NAs (valores ausentes)


## Normalizar/scalonizar etc..
dados_normalizados <- dados_renomeados %>%
  scale()

# Rodar o K-means para cluster analysis -------------

## 1. Identifcar o número de cluster k

### Heat map

heatmap(dados_normalizados)

### Dendrograma 
dendrograma <- hclust(dist(dados_normalizados))

fviz_dend(dendrograma)

### Baseado em modelos
fviz_nbclust(dados_normalizados, kmeans, method ="wss")


## 2. perform k-means clustering with k = 2 clusters
dados_cluster <- kmeans(dados_normalizados, centers = 4)


# Visualização e análise dos resultados --------------------------------------------

## Gráfico de dispersão dos clusters
fviz_cluster(dados_cluster, data = dados_renomeados)

fviz_mclust(dados_cluster, data = dados_renomeados)


## Prepare o dataframe 
cluster_resul <- data.frame(cluster = dados_cluster$cluster) %>% 
  bind_cols(dados_renomeados) %>% 
  mutate(cluster = as.factor(cluster))


## Estatística básica de cada cluster
cluster_caract <- cluster_resul %>% 
  group_by(cluster) %>%
  summarise(mpg = mean(mpg),
            cyl = mean(cyl))



