' 
Universidade Federal do Rio Grande do Norte
Disciplina: Apredizagem de Máquinas (AM) para Geógrafos
Professor: Dr. Max Anjos (https://github.com/ByMaxAnjos)

Aula: Aplicando o segundo modelo de AM
Objetivo desta atividade: resolver um problema de supervisionado/classificação em que 
somente duas respostas são aceitáveis "sobreviveu" ou "não sobreviveu", 
usando os banco de dados do Titanic.
Data: 30.10.2024

Baixar os dados: https://github.com/ByMaxAnjos/MachineLearning_for_geospatial_analysis/jobs_2024
Fonte dos dados: Titanic. https://www.kaggle.com/competitions/titanic/overview 
Tutotial explicando o random forest aplicado ao Titanic: https://www.kaggle.com/code/alexisbcook/titanic-tutorial 
'

# Instalar os pacotes e carregar as bibliotecas necessárias ---------------
install.packages("tidyverse") #manipulação, visualização e análise de dados.
install.packages("tidymodels") #treinar, avaliar e ajustar modelos de aprendizado de máquina e estatísticas
install.packages("ranger")
install.packages("vip")

library(tidyverse)
library(tidymodels)
library(ranger)
library(vip)

# Carregar e visualizar e manipular os dados  ----------------------------------------------------------

## Ler o arquivo csv seguindo: "caminho/para/seu/arquivo/name_dados.csv"
dados_brutos <- read_csv("caminho/para/seu/arquivo/name_dados.csv") 
dados_brutos <- read_csv("~/Documents/CO2CityMap/CO2CityMap/MLforGEO/MLforGEO/jobs_2024/job2_dados_titanic.csv") 

## Visualizando a estrutura dos dados 
view(dados_brutos)
str(dados_brutos)
head(dados_brutos)
dados_brutos

## Renomear as colunas para o PT usando rename()
#c("Sobreviveu", "Classe", "Nome", "Sexo", "Idade", "Irmãos_Cônjuge", "Pais_Filhos", "Bilhete", "Tarifa", "Cabine", "Embarque")

# dados_named <- rename(dados_brutos, 
#                       sobreviveu = Survived, 
#                       classe = Pclass, 
#                       sexo = Sex, 
#                       idade = Age)

dados_named <- dados_brutos %>%
  rename(sobreviveu = Survived, classe = Pclass, nome = Name,
         sexo = Sex, idade = Age, irmaos_conjuge = SibSp,
         pais_filhos = Parch, bilhete = Ticket, tarifa = Fare,
         cabine = Cabin, embarque = Embarked)

# Explorar os dados -------------------------------------------------------

## Histograma
# Verificar distribuição da variável de resposta
ggplot(dados_named, aes(x = factor(sobreviveu))) +
  geom_bar() +
  labs(title = "Distribuição da Variável de resposta: Sobrevivência",
       x = "Sobreviveu (0 = Não, 1 = Sim)")

## Associação/correlação: relação entre algumas variáveis explicativas e variável resposta 

### idade com sobreviveu
dados_named %>% 
  ggplot(aes(x=idade, fill = factor(sobreviveu))) +
  geom_density(alpha = 0.5)

### classe e sexo com sobreviveu
dados_named %>% 
  ggplot(aes(x=classe, fill = factor(sobreviveu))) +
  geom_bar(stat = "count") +
  facet_grid(~sexo)

# Pré-processamento dos dados ---------------------------------------------

## Checar as variáveis e lidando com os valores ausentes ou NA
view(dados_named)

## Selecionar as colunas/variávies/preditores/features do modelo usando select()
dados_select <- dados_named %>% 
  select(classe, sexo, idade, irmaos_conjuge, pais_filhos, tarifa, embarque, sobreviveu)

str(dados_select)

## Decidir sobre o NA 
###checar os NAs
summary(dados_select)
dados_omitidos <- dados_select %>% 
  na.omit()

##Transformar as colunas como númerica (numeric) ou fator (factor) adequadamente
dados_transf <- dados_omitidos %>% 
  mutate(classe = as.factor(classe),
         sexo = as.factor(sexo),
         irmaos_conjuge = as.factor(irmaos_conjuge),
         pais_filhos = as.factor(pais_filhos),
         embarque = as.factor(embarque),
         sobreviveu = as.factor(sobreviveu)
         )
str(dados_transf)

## Criar uma nova variável a partir das colunas idade e tarifa usando a mutate()
dados_processados <-dados_transf %>% 
  mutate(taxa_idade = tarifa/idade)

str(dados_processados)
# Dividir os dados em conjuntos de treinamento e teste ------------------

## Use a função initial_split do pacote tydimodels
indices_treino <- initial_split(dados_processados, prop = 0.80, strata = sobreviveu) # Indique a proporcao 80% train e 20% teste para cada classe 
train_set <- training(indices_treino)
test_set  <-  testing(indices_treino)


# Treinar o algorítimo Florestas Aleatórias (Random Forest) -------------

##Treine o modelo usando o dataframe "train_set"

model_randforest_train <- ranger(sobreviveu ~ ., data = train_set, importance = "impurity")
model_randforest_train

# Prever com o modelo treinado --------------------------------------------

##Faça as predições com o "model_randforest_train" usando o dataframe "test_set"

pred_randforest <- predict(model_randforest_train, data=test_set)

## Converter para data.frame e unir com o test_set 
pred_df_florest <- data.frame(pred_sobreviveu = pred_randforest$predictions)

resultado <- bind_cols(test_set, pred_df_florest)

# Avaliar o modelo -------------------------------------------

##Use as metrics matriz de confusão e acurácia usando as conf_mat() and accuracy ()

resultado %>% 
  conf_mat(truth = sobreviveu, estimate = pred_sobreviveu)

resultado %>% 
  accuracy(truth = sobreviveu, estimate = pred_sobreviveu)

# Importância das variáveis(feature importance) ----------------------

## Use a vip() do pacote vip e o modelo treinado "model_randforest_train"
model_randforest_train %>% 
  vip()



