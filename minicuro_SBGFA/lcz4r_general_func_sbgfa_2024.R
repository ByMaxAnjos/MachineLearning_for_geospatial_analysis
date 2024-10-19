#### LCZ4r Package - GENERAL FUNCTIONS ####

# Minicurso: LCZ4r: Explorando as Funções Gerais
# Autores: Dr. Max Anjos e Dr. Francisco Castelhano
# Data: 21.10.2024
# Copyright (c) Max Anjos e Francisco Castelhano, 2024
# Site: https://bymaxanjos.github.io/LCZ4r/articles/Introd_general_LCZ4r.html

# Instale e atualize o pacote LCZ4r -------------------------------------

#Se já usa o LCZ4r, desinstale a versão anterior
remove.packages("LCZ4r")

# O pacote remotes é usado para instalar pacotes diretamente do GitHub.
if(!require(remotes)) install.packages("remotes")

# Instale a versão mais recente do LCZ4r
remotes::install_github("ByMaxAnjos/LCZ4r", upgrade = "never") # Se necessário, reinicie o R após a instalação.

# Carregar o pacote LCZ4r
library(LCZ4r)

# Baixar o mapa de LCZs ----------------------------------------------------
# Função para obter o mapa das LCZs
# Sugestão: Substitua "Rio de Janeiro" por outra cidade para explorar mapas de diferentes regiões.

my_map <- lcz_get_map(city = "Rio de Janeiro",
                      roi = NULL,  # Shapefile da Região de Iinteresse (pode ser para recortar)
                      isave_map = FALSE,  # Salva o mapa localmente? (opcional)
                      isave_global = FALSE)  # Salvar o mapa global? (opcional)

# Plotar o mapa das LCZ
lcz_plot_map(
  my_map,
  isave = FALSE,  # Defina como TRUE para salvar o gráfico.
  show_legend = TRUE,  # Exibir legenda para facilitar a interpretação.
  save_extension = "png",  # Formato do arquivo salvo. Options: "png", "jpg", "jpeg", "tif", "pdf", "svg" (default is "png")
  inclusive = FALSE,  # Paleta de cores padrão.
  ...
)

# Mapa de LCZ para a Europa ------------------------------------------------
# Exemplo para uma cidade na Europa

euro_map <- lcz_get_map_euro(city = "Berlin")
lcz_plot_map(euro_map)

# Mapa de LCZ para os EUA --------------------------------------------------
# Exemplo para uma cidade dos Estados Unidos

usa_map <- lcz_get_map_usa(city = "Chicago")
lcz_plot_map(usa_map)

# Calcular áreas das LCZs ---------------------------------------------------

lcz_cal_area(
  my_map,
  plot_type = "pie",  # Escolha entre "bar", "donut", ou "pie".
  iplot = FALSE,  # Exibir o gráfico.
  isave = FALSE,  # Salvar o gráfico localmente? (defina como TRUE se necessário).
  show_legend = TRUE,  # Exibir legenda.
  xlab = "LCZ code",
  ylab = "Área [km²]"
)

# Extrair parâmetros urbanos ------------------------------------------------
# Esta função retorna até 35 parâmetros, como albedo e fração de superfície impermeável.
# Sugestão: Consulte os parâmetros completos neste link: https://bymaxanjos.github.io/LCZ4r/articles/Introd_general_LCZ4r.html#retrieve-and-visualize-lcz-parameters

my_param <- lcz_get_parameters(my_map,
                               iselect = "",  # Selecione um parâmetro específico (opcional).
                               istack = TRUE,  # Agregar multiplos parâmetros (raster) em um único arquivo raster.
                               ishp = FALSE,  # Salvar como shapefile? (defina como TRUE se necessário).
                               isave = FALSE)  # Salvar o resultado? (defina como TRUE se necessário).

# Verifique os parâmetros extraídos em format raster
my_param

# Plotar e salvar os parâmetros urbanos ------------------------------------

lcz_plot_parameters(
  my_param,
  iselect = "",  # Parâmetro a ser plotado.
  all = FALSE,  # Plota todos os parâmetros? (defina como TRUE para mais parâmetros).
  inclusive = FALSE,  # Paleta de cores padrão.
  isave = FALSE,  # Salvar o gráfico.
  save_extension = "png",  # Formato do arquivo salvo.
  subtitle = ""  # Subtítulo do gráfico (opcional).
)

# Testar outro parâmetro ---------------------------------------------------
# Sugestão: Teste outros parâmetros disponíveis, como "BSF2".
lcz_plot_parameters(my_param, iselect = "BSF2")

# Visualizar o mapa de forma interativa ------------------------------------
# if(!require(tmap)) install.packages("tmap")
# library(tmap)
# tmap_mode("view")
# qtm(my_param[[1]])






