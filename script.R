# Machine Learning - Atividade 2 - Fabiano Reis Faleiros
# Prevendo Pre�os de carros

# Dataset escolhido: https://www.kaggle.com/tunguz/used-car-auction-prices

setwd("D:/facul2/tei-3/atividade-2/atividade")
getwd()

# Problema de Neg�cio: Previs�o do valor de venda de carros usados

# Foram estraidos de forma aleat�ria cerca de 5 mil registros do dataset original(que cont�m > 500 mil)
# para treino. E cerca de 70 para teste

# Etapa 1 - Coletando os dados
cars <- read.csv("cars-treino.csv")
View(cars)


# Etapa 2: Explorando e Preparando os Dados
# aqui ser�o removidas algumas colunas sem importancia para a an�lise, ou com intuito de deix�-la mais simplificada

# declarando quais colunas s�o desnecess�rias, saleDate desconsiderado pois a data de venda de ambos era muito proxima
removeColNames <- c("vin", "trim","interior", "seller", "saledate", "mmr", "state") 
cars <- cars[, ! names(cars) %in% removeColNames, drop = F]

str(cars)


# Medias de Tend�ncia Central da vari�vel sellingprice e Histograma
summary(cars$sellingprice)
hist(cars$sellingprice, main = 'Histograma', xlab = 'Pre�o de venda')

# Tabela de quantidade por marca/montadora, cor e ano
table(cars$make)
table(cars$color)
table(cars$year) # h� poucos abaixo de 2004, seria interresante mant�-los na aprendizagem?

# Visualizando relacionamento entre as vari�veis: Scatterplot
?pairs
pairs(cars[c("year", "odometer", "sellingprice")])


# Scatterplot Matrix
install.packages("psych")
library(psych)


# aqui � possivel ver a rela��o do pre�o mais alto para odometro mais baixo e pre�o mais alto para o ano mais alto
pairs.panels(cars[c("year", "odometer", "sellingprice")])



# Etapa 3: Treinando o Modelo (usando os dados de treino)
?lm
modelo <- lm(sellingprice ~ ., data = cars)


# Prevendo despesas m�dicas 
?predict

# Aqui verificamos os gastos previstos pelo modelo que devem ser iguais aos dados de treino
previsao1 <- predict(modelo)
View(previsao1)

# Prevendo os gastos com Dados de teste
carteste <- read.csv("cars-teste.csv")

str(carteste)

carteste <- carteste[, ! names(carteste) %in% removeColNames, drop = F]
View(carteste)
View(cars)

previsao2 <- predict(modelo, carteste)
View(previsao2)



# Etapa 4: Avaliando a Performance do Modelo
# Mais detalhes sobre o modelo
summary(modelo)



# Etapa 5: Otimizando a Performance do Modelo: melhora de 0.8719 para 0.8787 ap�s tratamentos

# Adicionando uma vari�vel com media de odometro com ano e limitando o odometro por faixa (2016 usado pois 2015 � o maior valor do dataset)
cars$odometer2 <- cars$odometer / (2016- cars$year)

cars$odometer <- ifelse(cars$odometer > 10000, cars$odometer, 10000)

cars$odometer <- ifelse(cars$odometer < 100000, cars$odometer, 100000)


# Criando o modelo final
modelo_v2 <- lm(sellingprice ~ ., data = cars)

summary(modelo_v2)


# foram notados warnings ao executar a predict segue exemplo:
# uma predi��o a partir de um ajuste rank-deficient pode ser enganoso
# pesquisei acerca, vi que tentando simplificar o modelo eu poderia conseguir contornar, por�m sem sucesso
# ap�s algumas tentativas.







