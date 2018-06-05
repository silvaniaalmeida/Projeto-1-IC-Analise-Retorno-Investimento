# Classificação KNN 
# Prevendo o resultado do índice S&P (The Standard & Poor's 500) - American stock market index (NYSE or NASDAQ)

# ***** Esta é a versão 2.0 deste script, atualizado em 02/07/2017 *****
# ***** Esse script pode ser executado nas versões 3.3.1, 3.3.2, 3.3.3 e 3.4.0 da linguagem R *****
# ***** Recomendamos a utilização da versão 3.4.0 da linguagem R *****

# Definindo o diretório de trabalho
getwd()
setwd("~/Dropbox/DSA/MachineLearning/R/Cap05")

# Instalando os pacotes
install.packages("ISLR")
install.packages("caret")

# Carregando os pacotes
library(ISLR)
library(caret)

# Definindo o seed
set.seed(300)

# Split do dataset em treino e teste
??Smarket
head(Smarket)
??createDataPartition
indxTrain <- createDataPartition(y = Smarket$Direction, p = 0.75, list = FALSE)
training <- Smarket[indxTrain,]
testing <- Smarket[-indxTrain,]

# Verificando a distirbuição dos dados originais e das partições
prop.table(table(training$Direction)) * 100
prop.table(table(Smarket$Direction)) * 100

# Normalizando os dados
trainX <- training[,names(training) != "Direction"]
preProcValues <- preProcess(x = trainX, method = c("center", "scale"))
preProcValues

# Construindo o Modelo
set.seed(400)
??trainControl
?train
ctrl <- trainControl(method = "repeatedcv", repeats = 3) 
knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

# Modelo KNN
knnFit

# Número de Vizinhos x Acurácia
plot(knnFit)

# Fazendo previsões
knnPredict <- predict(knnFit, newdata = testing )

# Criando a Confusion Matrix
confusionMatrix(knnPredict, testing$Direction )


## Usando outras métricas

# Construindo o modelo
ctrl <- trainControl(method = "repeatedcv", repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

# Modelo KNN
knnFit

# Número de Vizinhos x Acurácia
plot(knnFit, print.thres = 0.5, type="S")

# Fazendo previsões
knnPredict <- predict(knnFit, newdata = testing )

# Criando a Confusion Matrix
confusionMatrix(knnPredict, testing$Direction )

