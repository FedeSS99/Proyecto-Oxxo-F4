# ------------------------------------------------------------------------------
# [DATATHON OXXO]  02_Pruebas_Modelo.R
# 
# Autora    : Semiramis G. de la Cruz
# Revision    : 25.05.2025
# Descripcion : 
#               
# ------------------------------------------------------------------------------

install.packages('xgboost')
install.packages('caret')
install.packages('dplyr')

# Cargar librerías necesarias
library(xgboost)
library(caret)
library(dplyr)

base_modelo_V2 <- read.csv("base_modelo_V2.csv")
base_modelo_test_V2 <- read.csv("base_modelo_test_V2.csv")

# Convertir a factores
base_modelo_V2$EXITO_POND <- as.factor(base_modelo_V2$EXITO_POND)
base_modelo_V2$NIVELSOCIOECONOMICO_DES <- as.factor(base_modelo_V2$NIVELSOCIOECONOMICO_DES)
base_modelo_V2$ENTORNO_DES <- as.factor(base_modelo_V2$ENTORNO_DES)

base_modelo_test_V2$EXITO_POND <- as.factor(base_modelo_test_V2$EXITO_POND)
base_modelo_test_V2$NIVELSOCIOECONOMICO_DES <- as.factor(base_modelo_test_V2$NIVELSOCIOECONOMICO_DES)
base_modelo_test_V2$ENTORNO_DES <- as.factor(base_modelo_test_V2$ENTORNO_DES)

# Alinear niveles de factores en test con train
base_modelo_test_V2$NIVELSOCIOECONOMICO_DES <- factor(
  base_modelo_test_V2$NIVELSOCIOECONOMICO_DES,
  levels = levels(base_modelo_V2$NIVELSOCIOECONOMICO_DES)
)
base_modelo_test_V2$ENTORNO_DES <- factor(
  base_modelo_test_V2$ENTORNO_DES,
  levels = levels(base_modelo_V2$ENTORNO_DES)
)

# Crear conjuntos de entrenamiento y prueba
train_data <- base_modelo_V2
test_data <- base_modelo_test_V2


# Convertir variables categóricas a dummy variables (one-hot encoding)
X_train <- model.matrix(EXITO_POND ~ PUERTASREFRIG_NUM + MTS2VENTAS_NUM +
                          ENTORNO_DES ,
                        data = train_data)[, -1]  # quitar intercepto

X_test <- model.matrix(EXITO_POND ~ PUERTASREFRIG_NUM + MTS2VENTAS_NUM +
                         ENTORNO_DES ,
                       data = test_data)[, -1]

# Convertir la variable respuesta a numérica binaria (0/1)
y_train <- as.numeric(as.character(train_data$EXITO_POND))
y_test <- as.numeric(as.character(test_data$EXITO_POND))


# Crear matrices DMatrix para XGBoost
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Entrenar modelo
parametros <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.5
)

modelo_xgb <- xgb.train(
  params = parametros,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  verbose = 0
)

# Predicciones
pred_prob <- predict(modelo_xgb, newdata = dtest)
pred_clas <- ifelse(pred_prob > 0.5, 1, 0)

# Matriz de confusión
confusionMatrix(
  factor(pred_clas, levels = c(0, 1)),
  factor(y_test, levels = c(0, 1)),
  positive = "1"
)


importancia <- xgb.importance(model = modelo_xgb)
xgb.plot.importance(importancia, top_n = 10)
