# Limpiar el entorno de trabajo

# Cargar las bibliotecas necesarias
library(tidyverse)
library(arrow)
library(tidymodels)
library(tune)
library(ranger)
library(dials)

# Leer los datos desde un archivo Feather
datos = read_feather("datos.feather")

# Preprocesar los datos
datos_modelo = datos %>%
  rename_with(~ str_replace(., "_%", ""), contains("_%")) %>%
  mutate(mode = as.factor(mode)) %>%
  select(-c(1:16))

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123) # Para reproducibilidad
particion_class = initial_split(datos_modelo, prop = 0.8)
datos_class_ent = training(particion_class)
datos_class_test = testing(particion_class)

# Definir especificaciones del modelo Random Forest con parámetros a tunear
rf_class_spec = rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 1000  # Número de árboles fijo
) %>%
  set_engine("randomForest") %>%
  set_mode("classification")

# Crear una receta para preprocesar los datos
preprocesado_class = recipe(mode ~ ., datos_modelo) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric())

# Definir el workflow que une la receta y el modelo
rf_class_wfl = workflow() %>%
  add_recipe(preprocesado_class) %>%
  add_model(rf_class_spec)

# Crear la validación cruzada en el conjunto de entrenamiento con 10 pliegues
cv_folds_class = vfold_cv(datos_class_ent, v = 10, strata = mode)

# Definir los rangos de hiperparámetros a explorar
mtry_values = seq(2, ncol(datos_class_ent) - 1, by = 2)
min_n_values = seq(2, 22, by = 4)

# Crear una cuadrícula de hiperparámetros usando expand_grid
grid_rf_class = expand_grid(mtry = mtry_values, min_n = min_n_values)

# Ajustar el modelo con validación cruzada y tunear los hiperparámetros
resultados_tune_rf_class = rf_class_wfl %>% 
  tune_grid(
    resamples = cv_folds_class,
    grid = grid_rf_class,
    metrics = metric_set(roc_auc, accuracy)
)

# Mostrar los resultados del ajuste de hiperparámetros
resultados_tune_rf_class %>%
  collect_metrics()

rf_class_mejor=resultados_tune_rf_class %>%
  select_best(metric="accuracy")

# Finalizar el modelo con los mejores hiperparámetros
rf_class_wfl_final = rf_class_wfl %>% 
  finalize_workflow(rf_class_mejor)

# Ajustar el modelo final en el conjunto de entrenamiento completo
modelo_rf_class_final = rf_class_wfl_final %>%
  last_fit(particion_class)

# Mostrar matriz de confusión
modelo_rf_class_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = mode, estimate = .pred_class)

# Mostrar métricas de clasificación
modelo_rf_class_final %>%
  collect_predictions() %>% 
  metrics(truth = mode, estimate = .pred_class)

# Generar y visualizar la curva ROC
modelo_rf_class_final %>%
  collect_predictions() %>% 
  roc_curve(truth = mode, .pred_Major) %>% 
  autoplot()

