# Limpiar el entorno de trabajo

# Cargar las bibliotecas necesarias
library(tidyverse)
library(arrow)
library(tidymodels)

# Leer los datos desde un archivo Feather
datos = read_feather("datos.feather")
# Preprocesar los datos
datos_modelo_class = datos %>%
  rename_with(~ str_replace(., "_%", ""), contains("_%")) %>%
  mutate(mode = as.factor(mode)) %>%
  select(-c(1:16))

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123) # Para reproducibilidad
particion_class = initial_split(datos_modelo_class, prop = 0.8)
datos_class_ent = training(particion_class)
datos_class_test = testing(particion_class)

# Definir especificaciones del modelo de regresión log_classística
log_class_spec = logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# Crear una receta para preprocesar los datos
preprocesado_class = recipe(mode ~ ., datos_modelo_class) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric())

# Definir el workflow que une la receta y el modelo
log_class_wfl = workflow() %>%
  add_recipe(preprocesado_class) %>%
  add_model(log_class_spec)

# Crear la validación cruzada en el conjunto de entrenamiento con 10 pliegues
cv_folds_class = vfold_cv(datos_class_ent, v = 10, strata = mode)

# Ajustar el modelo con validación cruzada en el conjunto de entrenamiento
resultados_log_class_cv = log_class_wfl %>% 
  fit_resamples(
    resamples = cv_folds_class,
    metrics = metric_set(roc_auc, accuracy)
    )

# Recoger y mostrar las métricas de la validación cruzada
resultados_log_class_cv%>%
  collect_metrics()

# Ajustar el modelo final en el conjunto de entrenamiento completo
modelo_log_class_final=log_class_wfl %>% 
  last_fit(particion_class)

# Mostrar matriz de confusión
modelo_log_class_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = mode, estimate = .pred_class)

# Mostrar métricas de clasificación
modelo_log_class_final %>%
  collect_predictions() %>% 
  metrics(truth = mode, estimate = .pred_class)

# Generar y visualizar la curva ROC
modelo_log_class_final %>%
  collect_predictions() %>% 
  roc_curve(truth = mode, .pred_Major) %>% 
  autoplot

