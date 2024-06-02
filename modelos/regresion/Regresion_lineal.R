library(tidyverse)
library(arrow)
library(tidymodels)
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

lm_reg_spec=linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

preprocesado_reg=recipe(streams ~ .,datos_modelo_reg) %>%
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

lm_reg_wf=
  workflow() %>% 
  add_model(lm_reg_spec) %>% 
  add_recipe(preprocesado_reg) 

# Crear la validación cruzada en el conjunto de entrenamiento con 10 pliegues
cv_folds_reg=vfold_cv(datos_reg_ent, v = 10)

# Ajustar el modelo con validación cruzada en el conjunto de entrenamiento
resultados_lm_reg_cv <- lm_reg_wf %>% 
  fit_resamples(
    resamples = cv_folds_reg
  )

# Recoger y mostrar las métricas de la validación cruzada
resultados_lm_reg_cv %>%
  collect_metrics()

modelo_lm_reg=lm_reg_wf %>%
  last_fit(particion_reg)

modelo_lm_reg %>% 
  collect_metrics()


