# Limpiar el entorno de trabajo

# Cargar las bibliotecas necesarias
library(tidyverse)
library(arrow)
library(tidymodels)
library(tune)

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
knn_class_spec = nearest_neighbor(
  neighbors = tune(),       # Número de vecinos
  weight_func = tune(),     # Función de ponderación
  dist_power = tune()       # Potencia de la distancia (Minkowski)
) %>%
  set_engine("kknn") %>%
  set_mode("classification")

# Crear una receta para preprocesar los datos
preprocesado_class = recipe(mode ~ ., datos_modelo) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric())

# Definir el workflow que une la receta y el modelo
knn_class_wfl = workflow() %>%
  add_recipe(preprocesado_class) %>%
  add_model(knn_class_spec)

# Crear la validación cruzada en el conjunto de entrenamiento con 10 pliegues
cv_folds_class = vfold_cv(datos_class_ent, v = 10, strata = mode)

neighbors_values = seq(1, 20, by = 2)
weight_func_values = c("gaussian", "optimal")
dist_power_values = c(1, 2)

# Crear una cuadrícula de hiperparámetros usando expand_grid
grid_knn_class = expand_grid(
  neighbors = neighbors_values,
  weight_func = weight_func_values,
  dist_power = dist_power_values
)

# Ajustar el modelo con validación cruzada y tunear los hiperparámetros
resultados_tune_knn_class = knn_class_wfl %>% 
  tune_grid(
    resamples = cv_folds_class,
    grid = grid_knn_class,
    metrics = metric_set(roc_auc, accuracy)
)

# Mostrar los resultados del ajuste de hiperparámetros
resultados_tune_knn_class %>%
  collect_metrics()

knn_class_mejor=resultados_tune_knn_class %>%
  select_best(metric="accuracy")

# Finalizar el modelo con los mejores hiperparámetros
knn_class_wfl_final = knn_class_wfl %>% 
  finalize_workflow(knn_class_mejor)

# Ajustar el modelo final en el conjunto de entrenamiento completo
modelo_knn_class_final = knn_class_wfl_final %>%
  last_fit(particion_class)

# Mostrar matriz de confusión
modelo_knn_class_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = mode, estimate = .pred_class)

# Mostrar métricas de clasificación
modelo_knn_class_final %>%
  collect_predictions() %>% 
  metrics(truth = mode, estimate = .pred_class)

# Generar y visualizar la curva ROC
modelo_knn_class_final %>%
  collect_predictions() %>% 
  roc_curve(truth = mode, .pred_Major) %>% 
  autoplot()

