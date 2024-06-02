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
            "artist(s)_name"))

particion_reg = datos_modelo_reg %>% 
  initial_split()

datos_reg_ent= particion_reg %>% training()

datos_reg_test= particion_reg %>% testing()

rf_reg_spec=rand_forest(trees=tune(),
                    min_n=tune()) %>% 
  set_engine("randomForest") %>% 
  set_mode("regression")

# Crear una receta para preprocesar los datos
preprocesado_reg=recipe(streams ~ .,datos_modelo_reg) %>%
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Definir el workflow que une la receta y el modelo
rf_reg_wf=
  workflow() %>% 
  add_model(rf_reg_spec) %>% 
  add_recipe(preprocesado_reg) 

# Crear la validación cruzada en el conjunto de entrenamiento con 10 pliegues
cv_folds_reg = vfold_cv(datos_reg_ent, v = 10)

# Crear una cuadrícula de hiperparámetros usando expand_grid
grid_rf_reg = expand_grid(trees=seq(100,200,by=50),
                           min_n=seq(2,8,by=2))
  
# Ajustar el modelo con validación cruzada y tunear los hiperparámetros
resultados_tune_rf_reg = rf_reg_wf %>% 
  tune_grid(
    resamples = cv_folds_reg,
    grid = grid_rf_reg)

# Mostrar los resultados del ajuste de hiperparámetros
resultados_tune_rf_reg %>%
  collect_metrics()

rf_reg_mejor=resultados_tune_rf_reg %>%
  select_best(metric="rsq")

# Finalizar el modelo con los mejores hiperparámetros
rf_reg_wfl_final = rf_reg_wf %>% 
  finalize_workflow(rf_reg_mejor)

# Ajustar el modelo final en el conjunto de entrenamiento completo
modelo_rf_reg_final = rf_reg_wfl_final %>%
  last_fit(particion_reg)

# Mostrar matriz de confusión
modelo_rf_reg_final %>% 
  collect_metrics() 

modelo_rf_reg_final %>% 
  extract_fit_engine() %>%
  vip()

