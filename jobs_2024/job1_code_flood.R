'
PROJETO ML: PREVER A PROBALIDADE DE RISCO DE INUNDAÇÃO USANDO MODELO DE MACHINE LEARNING

DADOS: Flood Prediction Dataset (https://www.kaggle.com/datasets/naiyakhalid/flood-prediction-dataset/data)

ATVIDADE: Testar os modelos regressão linear simples, regressão linear múltipla e random forest (florestas aleatórias)
'
# Ativar os pacotes -------------------------------------------------------
library(tidymodels)
library(ranger)
library(vip)
library(data.table)
library(openair)

# Importar dados ----------------------------------------------------------
dados <- fread("/Users/maxanjos/Documents/ML_flood/data_flood.zip")
str(dados)

# EDA - Análise exploratória dos dados ------------------------------------
ggplot(dados, aes(x = FloodProbability)) + 
  geom_histogram(bins = 50, col= "white") +
  theme_bw()

# Preparação dos dados ----------------------------------------------------

# Seleção das variáveis  ------------------------------------------------

# Divisão dos dados: treinamento e teste -----------------------------------

#Usar a proporção: 80/20% 
dados_split <- initial_split(dados, prop = 0.80) 
dados_treino <- training(dados_split)
dados_teste <- testing(dados_split)

#Checar os dados divididos
str(dados_treino)
str(dados_teste)

# Treinamento dos modelos de ML -------------------------------------------

# Regressão linear simples
mod_linear_simples <- lm(FloodProbability ~ PoliticalFactors, data = dados_treino)
mod_linear_simples

# Regressão linear múltipla
mod_linear_multipla <- lm(FloodProbability ~ PoliticalFactors + Deforestation + AgriculturalPractices + PopulationScore, data = dados_treino)
mod_linear_multipla

# Florestas aleatórias
mod_rf <- ranger(FloodProbability ~ ., data = dados_treino, importance = "permutation",)


# Predizer a probabilidade de inundação usando os modelos treinados ----------

pred_mod_linear_simples <- predict(mod_linear_simples, newdata=dados_teste)
pred_mod_linear_multipla <- predict(mod_linear_multipla, newdata=dados_teste)
pred_mod_rf <- predict(mod_rf, data=dados_teste)

resultado <- tibble(FloodProbability = dados_teste$FloodProbability,
                    linear_simples = pred_mod_linear_simples,
                    linear_multipla = pred_mod_linear_multipla,
                    rf = pred_mod_rf$predictions)

# Avaliar a performace dos modelos ML ------------------------------------------------

plot_simples <- scatterPlot(resultado, 
            x = "linear_simples", 
            y = "FloodProbability", 
            main = "Regressão Linear simples",  
            xlab = "Probabilidade de Inundação (predição)", 
            ylab = "Probabilidade de Inundação (observado)", 
            pch = 19,  
            col = "black",  
            cex = 0.5,  
            reg.line = TRUE, 
            linear = TRUE) 
plot_mult <- scatterPlot(resultado, 
                            x = "linear_multipla", 
                            y = "FloodProbability", 
                            main = "Regressão Linear Múltipla",  
                            xlab = "Probabilidade de Inundação (predição)", 
                            ylab = "Probabilidade de Inundação (observado)", 
                            pch = 19,  
                            col = "black",  
                            cex = 0.5,  
                            reg.line = TRUE,
                            linear = TRUE)  

plot_rf <- scatterPlot(resultado, 
                         x = "rf", 
                         y = "FloodProbability", 
                         main = "Floresta aleatória (random forest)",  
                         xlab = "Probabilidade de Inundação (predição)", 
                         ylab = "Probabilidade de Inundação (observado)", 
                         pch = 19,  
                         col = "black",  
                         cex = 0.5,  
                         reg.line = TRUE,
                         linear = TRUE)  

figura_final <- cowplot::plot_grid(plot_simples$plot, plot_mult$plot, plot_rf$plot, 
                                   labels = "AUTO")

# Importância das variáveis --------
vip(mod_rf) +
  labs(title = "Quais são as variáveis mais importante para pedizer a probalidade de inundação")


