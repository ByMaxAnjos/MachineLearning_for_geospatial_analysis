' 
Universidade Federal do Rio Grande do Norte
Disciplina: Técnicas de Apredizagem de Máquinas (AM) aplicadas à análise espacial
Professor: Dr. Max Anjos (https://github.com/ByMaxAnjos)

Aula: Aplicando o quarto modelo de AM
Objetivo desta atividade: resolver um probelma de não supervisionado de redunção de dimensionalidade (PCA) a partir 
do banco de dados socio e populacionais do IBGE e IPEA.

Data: 26.09.2023

Baixar os dados: no R.
Fonte dos dados: https://ipeagit.github.io/aopdata/
'

# Carregar as bibliotecas necessárias ---------------

library(tidyverse)
library(tidymodels)
library(factoextra)
library(FactoMineR)
library(aopdata)
library(cowplot)

# Baixe os dados  ----------------------------------------------------------

# Baixe os dados diretamente do site do IPEA usando a funcção read_landuse() do pacote aopdata
pop_natal <- read_landuse(
  city = 'Natal', 
  year = 2019,
  geometry = FALSE
)

#Para checar o dicionário das variáveis https://ipeagit.github.io/aopdata/articles/data_dic_pt.html
str(pop_natal)

# Manipular e Explorar os dados -------------------------------------------------------

pop_natal_selec <- pop_natal %>% 
  select(-id_hex, -abbrev_muni, -name_muni, -code_muni, -year)

str(pop_natal_selec)

# Pré-processamento dos dados ---------------------------------------------

# Verificar os NAs (valores ausentes)
summary(pop_natal_selec)

dados_omitidos <- pop_natal_selec %>% 
  na.omit()
  
## Normalizar/scalonizar etc..
dados_normalizados <- dados_omitidos %>%
  scale()

# Rodar o PCA para cluster analysis -------------

res.pca <- PCA(dados_normalizados, ncp = 4, graph = FALSE)

# Visualização e análise dos resultados --------------------------------------------

# Visualizar a contribuição em % de cada PCA no conjunto de dados
fviz_screeplot(res.pca, addlabels = TRUE)

# Extract the results for variables
my_pca <- get_pca(res.pca)
my_pca$contrib
my_pca$cor

# Contributions of variables to PC1
fig1 <- fviz_contrib(res.pca, choice = "var", axes = 1, top = 5)

# Contributions of variables to PC2
fig2 <- fviz_contrib(res.pca, choice = "var", axes = 2, top = 5)

# Contributions of variables to PC3
fig3 <- fviz_contrib(res.pca, choice = "var", axes = 3, top = 5)

# Contributions of variables to PC4
fig4 <- fviz_contrib(res.pca, choice = "var", axes = 4, top = 5)


#Plotar todos os PCAs em única figura
plot_grid(fig1, fig2, fig3, fig4, labels = "AUTO") 



