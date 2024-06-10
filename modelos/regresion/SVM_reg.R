
library(tidyverse)
library(arrow)
library(tidymodels)
library(vip)
datos=read_feather("datos.feather")
datos_modelo_reg=datos %>%
  select(-c("released_year",
            "released_month",
            "released_day",
            "track_name", 
            "artist(s)_name")) %>% 
  rename_with(~ str_replace(., "_%", ""), contains("_%"))

particion_reg = datos_modelo_reg %>% 
  initial_split(prop=0.8)

datos_reg_ent= particion_reg %>% training()

datos_reg_test= particion_reg %>% testing()

svm_reg_spec=svm_rbf(cost=tune(),rbf_sigma=tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

preprocesado_reg=recipe(streams ~ .,datos_modelo_reg) %>%
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Definir el workflow que une la receta y el modelo
svm_reg_wf=
  workflow() %>% 
  add_model(svm_reg_spec) %>% 
  add_recipe(preprocesado_reg) 

# Crear la validación cruzada en el conjunto de entrenamiento con 10 pliegues
cv_folds_reg = vfold_cv(datos_reg_ent, v = 10)

# Crear una cuadrícula de hiperparámetros usando expand_grid
grid_svm_reg = expand_grid(cost = 3^(1:4), rbf_sigma = 3^(-5:-2))

# Ajustar el modelo con validación cruzada y tunear los hiperparámetros
resultados_tune_svm_reg = svm_reg_wf %>% 
  tune_grid(
    resamples = cv_folds_reg,
    grid = grid_svm_reg)

# Mostrar los resultados del ajuste de hiperparámetros
resultados_tune_svm_reg %>%
  collect_metrics()

svm_reg_mejor=resultados_tune_svm_reg %>%
  select_best(metric="rsq")

# Finalizar el modelo con los mejores hiperparámetros
svm_reg_wfl_final = svm_reg_wf %>% 
  finalize_workflow(svm_reg_mejor)

# Ajustar el modelo final en el conjunto de entrenamiento completo
modelo_svm_reg_final = svm_reg_wfl_final %>%
  last_fit(particion_reg)

modelo_svm_reg_final %>% 
  collect_metrics() 

# NO SIRVE

# importancia_svm_reg=modelo_svm_reg_final %>% 
#   extract_fit_engine() %>%
#   vip()

