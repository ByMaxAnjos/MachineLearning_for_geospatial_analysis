#### LCZ4r Package - LOCAL FUNCTIONS ####

# Minicurso: LCZ4r: Explorando as Funções Locais
# Autores: Dr. Max Anjos e Dr. Francisco Castelhano
# Data: 21.10.2024
# Copyright (c) Max Anjos e Francisco Castelhano, 2024
# Site: https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html

# Carregar o pacote LCZ4r -------------------------------------
library(LCZ4r)

# Banco de dados -------------------------------------
# Carregar o conjunto de dados de Berlim fornecido pelo pacote LCZ4r
data("lcz_data")
lcz_data  # Exibir o data frame com as informações de temperatura e estações

# Baixar e visualizar o mapa LCZ de Berlim ---------------------------------

lcz_map <- lcz_get_map_euro(city = "Berlin")

lcz_plot_map(lcz_map)

# Analisar Séries Temporais ------------------------------------------------

lcz_ts(
  lcz_map,
  data_frame = lcz_data,
  var = "airT",  # Variável de temperatura do ar
  station_id = "station",  # Coluna de identificação das estações
  start="1/1/2019", end="30/3/2019",
  time.freq = "day",  # Frequência diária
  plot_type = "heatmap",  # Gráfico de linha facetado por estação
  facet_plot = "station",
  by = NULL,
  smooth = FALSE,  # Aplicar suavização (GAM)
  iplot = TRUE,  # Mostrar o gráfico
  isave = FALSE,  # Não salvar o gráfico
  palette = "VanGogh2",  # Paleta de cores
  ylab = "Air temperature [ºC]",
  xlab = "Time",
  title = "LCZ - Timeseries",
  caption = "LCZ4r, 2024.",
  legend_name = "Temperature (ºC)"
)

# Análise de Anomalias -----------------------------------------------------
# Detectar anomalias de temperatura para uma data e hora específicas
lcz_anomaly(
  lcz_map,
  data_frame = lcz_data,
  var = "airT",
  station_id = "station",
  year = 2019, month = 2, day = 6,  # Especificar o período
  time.freq = "hour",
  by="daylight",
  iplot = TRUE,  # Mostrar gráfico
  isave = FALSE,
  ylab = "Air temperature anomaly",
  xlab = "Stations",
  title = "Temperature Anomaly",
  caption = "LCZ4r, 2024."
)

# Interpolação de Temperatura do Ar -----------------------------------------
# Modelar a temperatura do ar em uma determinada data e hora
my_interp <- lcz_interp_map(
  lcz_map,
  data_frame = lcz_data,
  var = "airT",
  station_id = "station",
  year = 2019, month = 2, day = 6, hour = 5,  # Data específica
  sp.res = 100,  # Resolução espacial
  tp.res = "hour",  # Resolução temporal
  vg.model = "Sph"  # Modelo variograma esférico
)
# Verificar o objeto de interpolação
my_interp

# Plotar o mapa de interpolação --------------------------------------------
lcz_plot_interp(
  my_interp,
  palette = "muted",  # Escolha uma paleta de cores discreta
  title = "LCZ - Air Temperature",
  subtitle = "Berlin - 06.02.2019 at 05:00",
  caption = "Source: LCZ4r, 2024.",
  fill = "[ºC]"
)

# Modelagem de Anomalias de Temperatura -------------------------------------
my_interp_anomaly <- lcz_anomaly_map(
  lcz_map,
  data_frame = lcz_data,
  var = "airT",
  station_id = "station",
  year = 2019, month = 2, day = 6, hour = 5,
  sp.res = 100,
  tp.res = "hour",
  vg.model = "Sph",
  LCZinterp = TRUE  # Incluir interpolação com LCZ
)
# Verificar o objeto de anomalia de temperatura
my_interp_anomaly

# Plotar mapa de anomalias --------------------------------------------------
lcz_plot_interp(
  my_interp_anomaly,
  palette = "muted",
  title = "LCZ - Air Temperature Anomalies",
  subtitle = "Berlin - 06.02.2019 at 05:00",
  caption = "Source: LCZ4r, 2024.",
  fill = "[ºC]"
)

# Calcular a Intensidade da Ilha de Calor Urbana ---------------------------
lcz_uhi_intensity(
  lcz_map,
  data_frame = lcz_data,
  var = "airT",
  station_id = "station",
  year = 2019, month = 2, day = 6,  # Data específica
  time.freq = "hour",
  method = "LCZ",  # Método baseado em LCZs
  group = TRUE,  # Agrupar estações para cálculo de intensidade
  iplot = TRUE,
  isave = FALSE,
  ylab = "Air temperature [ºC]",
  xlab = "Time",
  ylab2 = "UHI Intensity",
  title = "Urban Heat Island Intensity",
  caption = "LCZ4r, 2024."
)

# Separar Intensidade da UHI por Períodos ---------------------------------
lcz_uhi_intensity(
  lcz_map,
  data_frame = lcz_data,
  var = "airT",
  station_id = "station",
  year = 2019, month = 2, day = 6,  # Data específica
  time.freq = "hour",
  method = "LCZ",
  group = TRUE,
  iplot = TRUE,
  isave = FALSE,
  ylab = "Air temperature [ºC]",
  xlab = "Time",
  ylab2 = "UHI Intensity",
  title = "Urban Heat Island Intensity by Period",
  caption = "LCZ4r, 2024."
)
