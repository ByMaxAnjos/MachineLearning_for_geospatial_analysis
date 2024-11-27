
# 1. Preparação: Carregar Bibliotecas e Dados -----------------------------
# Bibliotecas Necessárias
library(tidyverse)
library(tidymodels)
library(factoextra)
library(FactoMineR)
library(aopdata)
library(cowplot)
library(plotly)

# Baixar os dados diretamente do site do IPEA
pop_natal <- read_landuse(
  city = 'Natal', 
  year = 2019,
  geometry = FALSE
)

# Verificar a estrutura dos dados
glimpse(pop_natal)


# 2. Exploração e Manipulação dos Dados -----------------------------------

# Selecionar variáveis relevantes (remover identificadores e não numéricas)
pop_natal_selec <- pop_natal %>% 
  select(-id_hex, -abbrev_muni, -name_muni, -code_muni, -year)

# Explorar as variáveis
summary(pop_natal_selec)

# 3. Pré-Processamento ----------------------------------------------------
# Remover valores ausentes
dados_omitidos <- pop_natal_selec %>% na.omit()

# Normalizar os dados
dados_normalizados <- scale(dados_omitidos)


# 4. Aplicar PCA ----------------------------------------------------------

# Executar o PCA
res.pca <- PCA(dados_normalizados, ncp = 4, graph = FALSE)

# Visualizar a contribuição percentual de cada componente principal
fviz_screeplot(res.pca, addlabels = TRUE, barfill = "skyblue") +
  labs(title = "Contribuição de Cada Componente Principal")


# 5. Analisar Resultados --------------------------------------------------

# Extrair contribuições das variáveis
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10) +
  labs(title = "Contribuições para PC1")

fviz_contrib(res.pca, choice = "var", axes = 2, top = 10) +
  labs(title = "Contribuições para PC2")

fviz_contrib(res.pca, choice = "var", axes = 3, top = 10) +
  labs(title = "Contribuições para PC3")

fviz_contrib(res.pca, choice = "var", axes = 4, top = 10) +
  labs(title = "Contribuições para PC4")


# 6. Visualização em 2D e 3D ----------------------------------------------

# Mapa de variáveis
fviz_pca_var(res.pca, col.var = "contrib", gradient.cols = c("blue", "yellow", "red")) +
  labs(title = "Mapa de Variáveis no Espaço PCA")

# Converter os componentes principais para visualização
pca_data <- as.data.frame(res.pca$ind$coord)
pca_data <- pca_data %>% 
  mutate(Cluster = as.factor(kmeans(pca_data, centers = 3)$cluster))

# Gráfico 3D com plotly
plot_ly(pca_data, x = ~Dim.1, y = ~Dim.2, z = ~Dim.3, color = ~Cluster, colors = "Set2", type = "scatter3d", mode = "markers") %>%
  layout(
    title = "Visualização 3D dos Componentes Principais",
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )


# 7. Dendrograma para Visualizar Grupos -----------------------------------

# Calcular a distância e criar o dendrograma
distancias <- dist(dados_normalizados, method = "euclidean")
dendrograma <- hclust(distancias, method = "ward.D2")

# Visualizar o dendrograma
fviz_dend(dendrograma, k = 4, cex = 0.5, rect = TRUE) +
  labs(title = "Dendrograma - PCA e Clusterização")


