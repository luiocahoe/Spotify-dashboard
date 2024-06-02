# Limpiar el entorno de trabajo

# Cargar las bibliotecas necesarias
library(tidyverse)
library(arrow)
library(tidymodels)
library(tune)
library(kernlab)
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

# Definir especificaciones del modelo svm_class con parámetros a tunear
svm_class_spec = svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Crear una receta para preprocesar los datos
preprocesado_class = recipe(mode ~ ., datos_modelo) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric())

# Definir el workflow que une la receta y el modelo
svm_class_wfl = workflow() %>%
  add_recipe(preprocesado_class) %>%
  add_model(svm_class_spec)

# Crear la validación cruzada en el conjunto de entrenamiento con 10 pliegues
cv_folds_class = vfold_cv(datos_class_ent, v = 10, strata = mode)

cost_values = 10^seq(-3, 2, length.out = 5)
sigma_values = 10^seq(-3, 0, length.out = 5)

# Crear una cuadrícula de hiperparámetros usando expand_grid
grid_svm_class = expand_grid(cost = cost_values, rbf_sigma = sigma_values)

# Ajustar el modelo con validación cruzada y tunear los hiperparámetros
resultados_svm_class_tune = svm_class_wfl %>% 
  tune_grid(
    resamples = cv_folds_class,
    grid = grid_svm_class,
    metrics = metric_set(roc_auc, accuracy)
  )

# Mostrar los resultados del ajuste de hiperparámetros
resultados_svm_class_tune %>%
  collect_metrics()

svm_class_mejor=resultados_svm_class_tune %>%
  select_best(metric="accuracy")

# Finalizar el modelo con los mejores hiperparámetros
svm_class_wfl_final = svm_class_wfl %>% 
  finalize_workflow(svm_class_mejor)

# Ajustar el modelo final en el conjunto de entrenamiento completo
modelo_svm_class_final = svm_class_wfl_final %>%
  last_fit(particion_class)

# Mostrar matriz de confusión
modelo_svm_class_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = mode, estimate = .pred_class)

# Mostrar métricas de clasificación
modelo_svm_class_final %>%
  collect_predictions() %>% 
  metrics(truth = mode, estimate = .pred_class)

# Generar y visualizar la curva ROC
modelo_svm_class_final %>%
  collect_predictions() %>% 
  roc_curve(truth = mode, .pred_Major) %>% 
  autoplot()

