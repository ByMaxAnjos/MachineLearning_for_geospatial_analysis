' 
Universidade Federal do Rio Grande do Norte
Disciplina: Técnicas de AM aplicadas à análise espacial
Professor: Dr. Max Anjos
Aula: Aplicando o primeiro modelo de AM
Data: 11.09.2023
'

# Instalar os pacotes e carregar as bibliotecas necessárias ---------------
install.packages("tidyverse") #manipulação, visualização e análise de dados.
install.packages("tidymodels") #treinar, avaliar e ajustar modelos de aprendizado de máquina e estatísticas
install.packages("sf") #criar, manipular e visualizar dados geoespaciais
install.packages("tmap") #Plotar mapas

library(tidyverse)
library(tidymodels)
library(sf)
library(tmap)


# Carregar (abrir) e visualizar os dados  ----------------------------------------------------------

##Ler o arquivo csv seguindo: "caminho/para/seu/arquivo/name_dados.csv"
dados <- read_csv("caminho/para/seu/arquivo/dados.csv") 

## Visualizando a estrutura dos dados 
view(dados)
str(dados)
head(dados)
dados

## Carregue o mapa das estações de contagem de carros usando a função st_read() do pacote sf e o plote com a qtm() do pacote tmap. Ative o modo "plot" ou "view" da função tmap_mode()

estacoes <- st_read("caminho/para/seu/arquivo/dados.shp") #formato dataframe em .shp

tmap_mode("plot")
qtm(estacoes)

# Explorar os dados -------------------------------------------------------

##Analisar a relação entre a variável dependente e as explicativas através da correlação linear simples
ggplot(dados, aes(x=compriment_via, y = fluxo_carros)) +
  geom_point()

ggplot(dados, aes(x = compriment_via, y = fluxo_carros)) + 
  geom_point(size = 2, alpha = 0.3) +   # Plot o gráfico como pontos 
  geom_smooth(method = "lm") +
  labs(x = "Comprimento da via em metros", y = "Números de carros") #Renomear os eixos x e y do gráfico

# Pré-processamento dos dados ---------------------------------------------




# Dividindo os dados em conjuntos de treinamento e teste ------------------

## Use a função initial_split do pacote tydimodels
indices_treino <- initial_split(dados, prop = 0.80) # Indica a proporção 80% train e 20% teste
conjunto_treino <- training(indices_treino)
conjunto_teste  <-  testing(indices_treino)


# Applicando o algorítimo dde regressão linear simples  ------------------------

##Treinando (Ajustando) um modelo de regressão linear chamado lm
modelo_lm_treinado <- lm(fluxo_carros ~ compriment_via, data = conjunto_treino)

## Visualizando os coeficientes do modelo
modelo_lm_treinado
summary(modelo_lm_treinado)


# Aplicando o modelo treinado no conjunto teste ---------------------------

## Use a função predict e no argumento newdata incluir os dados teste chamado aqui "conjunto_teste"
predicoes <- predict(modelo_lm_treinado, newdata = conjunto_teste)

##Opção 1: Transformar as predições "predicoes" para um formato data.frame
tabela <- data.frame(real = conjunto_teste$fluxo_carros,
                        modelado = predicoes)

## Opção 2: transformar as predicões em data.frame e unir com o conjunto teste usando a função bind_cols do pacote dplyr
pred_df_lm <- data.frame(pred_lm = predicoes)
resultado1 <- bind_cols(conjunto_teste, pred_df_lm)

# Avaliando o desempenho do modelo ----------------------------------------
str(resultado1)

##Visualizar os resultados
ggplot(resultado1) +
  geom_line(aes(x=FID, y = fluxo_carros), color = "black", linetype ="dashed") +
  geom_line(aes(x=FID, y = pred_lm), color = "red") +
  labs(title = "Algorítimo: Regressão Linear Simples",
       subtitle = "Predição dos fluxo de carros usando a variável comprimento da via em metros",
       y = "Fluxos de carros") +
  theme_bw()

##Calculate o erro do model


# Aplicando o algorítimo de regressão linear múltipla ---------------------

##Treine o modelo usando o dataframe "conjunto_treino"
str(conjunto_treino)

model_lmulti_treinado <- lm(fluxo_carros ~ compriment_via+largura_via+velocid_limite+velocid_carros, 
                            data = conjunto_treino)
model_lmulti_treinado

##Faça a previsão usando o dataframe "conjunto_teste"
predicoes_lmulti <- predict(model_lmulti_treinado, newdata =conjunto_teste)

## Convert to data.frame e unir com o conjunto teste 
pred_df_lmulti <- data.frame(pred_lmulti = predicoes_lmulti)

resultado2 <- bind_cols(conjunto_teste, pred_df_lmulti)

##Visualize os resultados
ggplot(resultado2) +
  geom_line(aes(x=FID, y = fluxo_carros), color = "black", linetype ="dashed") +
  geom_line(aes(x=FID, y = pred_lmulti), color = "blue") +
  labs(title = "Algorítimo: Regressão Linear Múltipla",
       subtitle = "Predição dos fluxo de carros usando + de uma variável") +
  theme_bw()

##Calcule o erro do modelo

# Aplicando o algorítimo florestas aleatórias (random forest) -------------

##Treine o modelo usando o dataframe "conjunto_treino"
str(conjunto_treino)

model_florest_treinado <- ranger(fluxo_carros ~ compriment_via+largura_via+velocid_limite+velocid_carros, 
                            data = conjunto_treino)

model_florest_treinado
##Faça a previsão usando o dataframe "conjunto_teste"

predicoes_florest <- predict(model_florest_treinado, data=conjunto_teste)

## Convert to data.frame e unir com o conjunto teste 
pred_df_florest <- data.frame(pred_florest = predicoes_florest$predictions)

resultado3 <- bind_cols(conjunto_teste, pred_df_florest)

##Visualize os resultados
ggplot(resultado3) +
  geom_line(aes(x=FID, y = fluxo_carros), color = "black", linetype ="dashed") +
  geom_line(aes(x=FID, y = pred_florest), color = "darkgreen") +
  labs(title = "Algorítimo: Florestas Aleatórias (Random Forest)",
       subtitle = "Predição dos fluxo de carros usando + de uma variável") +
  theme_bw()

##Calcule o erro do modelo


# Selecione o melhor algorítimo em relação aos dados ----------------------

##Cre um dataframe e junte os dataframes com os modelos testados
final_df <- data.frame(FID = resultado1$FID,
                       dado_real = resultado1$fluxo_carros,
                       lm = resultado1$pred_lm,
                       lm_multi = resultado2$pred_lmulti,
                       floresta = resultado3$pred_florest)
str(final_df)

##Visualize os resultados dos modelos 
ggplot(final_df) +
  geom_line(aes(x=FID, y = dado_real), color = "black", linetype ="dashed") +
  geom_line(aes(x=FID, y = lm), color = "red") +
  geom_line(aes(x=FID, y = lm_multi), color = "blue") +
  geom_line(aes(x=FID, y = floresta), color = "darkgreen") +
  labs(title = "Qual é o melhor modelo para estes dados?",
       subtitle = "Regressão Linear Simples, Regressão Linear Múltipa ou Florestas Aleatórias",
       y = "Fluxos de carros") +
  theme_bw(base_size = 18)

