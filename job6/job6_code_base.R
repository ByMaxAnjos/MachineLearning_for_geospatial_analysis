' 
Universidade Federal do Rio Grande do Norte
Disciplina: Técnicas de Apredizagem de Máquinas (AM) aplicadas à análise espacial
Professor: Dr. Max Anjos (https://github.com/ByMaxAnjos)

Aula: Análise espacial com o R: Part 2
Objetivo desta atividade: Manipular e explorar dados em formato raaster

Data: 09.10.2023

Baixar os dados: no R.
Fonte dos dados: Índice de Vegetação da Diferença Normalizada (Normalized Difference Vegetation Index – do inglês)
'


# Carregar pacotes -----------------------------------------------------------

library(terra)
library(tidyverse)
library(sf)
library(tmap)

# Carregar dados ---------------------------------------------------------------

ndvi_map <- rast("/Users/co2map/Documents/CO2CityMap/CO2CityMap/MLforGEO/MLforGEO/job6/NDVI_resamp_2016-01-01.tif")

# Plotar mapa ----------------------------------------------------------------

plot(ndvi_map)

qtm(ndvi_map, estilo = "cobalto")

# Operações geoespaciais ---------------------------------------------------

# Resolução original do raster para referência
res(ndvi_map)

#~~ Agregar raster para resolução mais grossa
SMcoarse <- terra::aggregate(ndvi_map,           # Raster de umidade do solo
                             fator = 10,    # Agregar por um fator de 10
                             fun = mean)   # Função usada para agregar valores
res(SMcoarse)

#~~ Desagregar raster para resolução mais fina
SMfine <- terra::disagg(ndvi_map, 
                        fator = 3)
res(SMfine)

plot(SMfine)

# Remodelar raster para resolução mais grossa
Resamp_map <- terra::resample(SMfine,      # Raster original
                              SMcoarse) 

terra::plot(Resamp_map)

# Estatísticas resumidas do raster  ----------------------------------------------

# Operações aritméticas simples
ndvi_map2 <- ndvi_map * 2
print(ndvi_map2) # Experimente ndvi_map2 = ndvi_map * 10 ou ndvi_map2 = ndvi_map^2 e veja a diferença nos valores de ndvi_map2

# Estatísticas resumidas
global(ndvi_map, mean, na.rm = T)

global(ndvi_map, sd, na.rm = T)

global(ndvi_map, max, na.rm = T)

global(ndvi_map, min, na.rm = T)

global(ndvi_map, quantile, probs = c(0.25, 0.75), na.rm = T)

# Operação entre raster e shp ----------------------------------------

brasil_area <- st_read("/Users/co2map/Documents/CO2CityMap/CO2CityMap/MLforGEO/MLforGEO/job6/brasil_area.shp")

# Supondo que você tenha um objeto raster ndvi_map e um objeto sf brasil_area
ndvi_br_crop <- crop(ndvi_map, ext(brasil_area))

plot(ndvi_br_crop)

ndvi_br_mask <- mask(ndvi_br_crop, vect(brasil_area))

plot(ndvi_br_mask)

# Usar shapefile para resumir um raster

# abrir o raster de aridez
aridez_map <- rast("/Users/co2map/Documents/CO2CityMap/CO2CityMap/MLforGEO/MLforGEO/job6/aridity_36km.tif")

plot(aridez_map)

# Convert raster to shapefile
arid_poly=as.polygons(aridez_map)   # Convert SpatRaster to polygon and then to sf


# Plot aridity polygon
plot(arid_poly, 
     col=arid_poly$aridity_36km)  # Colors based on aridity values (i.e. 1,2,3,4,5)

# Usar terra::extract com o polígono rasterizado
ndvi_df <- terra::extract(ndvi_map, arid_poly, df = TRUE, fun = mean, na.rm = TRUE) # Estatística desejada: mean, sum, min e max




