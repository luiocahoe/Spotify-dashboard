# Spotify dashboard

<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/2/26/Spotify_logo_with_text.svg/1200px-Spotify_logo_with_text.svg.png" alt="Spotify Logo" width="300"/>

### Resumen

Este proyecto presenta un panel interactivo desarrollado con flexdashboard y shiny en R y Python, cuyo objetivo es analizar y visualizar las características de las canciones más populares de 2023 según Spotify. Asimismo se usaron modelos de tidymodels para predecir el número de reproducciones (streams) de las canciones y clasificar el modo (mayor o menor) de las canciones.

El conjunto de datos utilizado fue descargado de Kaggle y contiene información detallada sobre varias características de cada canción, su popularidad y su presencia en diferentes plataformas musicales.

### Estructura del Panel

El panel interactivo está dividido en varias pestañas, cada una de las cuales ofrece diferentes tipos de análisis y visualizaciones:

1. Introducción
   - Descripción de los Datos: Se proporciona una descripción detallada del conjunto de datos, incluyendo las variables disponibles y su significado.
   - Importación y Limpieza de Datos: Los datos se importan y limpian utilizando Python. Posteriormente, se guardan en formato Feather para facilitar su importación en R.

2. Resumen
   - Resumen Global: Se presenta un resumen global de todas las canciones utilizando un dashboard interactivo. Aquí se pueden visualizar métricas generales y tendencias sobre la popularidad y las características de las canciones.
   - Resumen por Artistas: Se ofrece un análisis detallado de los artistas del top 10 con más canciones en el dataset. Esta sección se implementa como una aplicación shiny, permitiendo interactividad y exploración dinámica de los datos.

3. Modelos Predictivos
   - Modelos de Regresión: Se ha utilizado un modelo de regresión para predecir el número de reproducciones (streams) de las canciones. Este modelo permite entender mejor los factores que influyen en la popularidad de una canción.
   - Modelos de Clasificación: Se ha empleado un modelo de clasificación para predecir el modo (mayor o menor) de las canciones. Esto ayuda a categorizar las canciones basándose en sus características musicales.
   - Tidymodels y Workflows: Los modelos predictivos se construyeron utilizando el ecosistema tidymodels en R, aprovechando workflows para gestionar y entrenar los modelos de manera estructurada y reproducible.

4. Tecnologías Utilizadas

    - R: Lenguaje de programación utilizado para el análisis de datos y la creación del dashboard.
    - Python: Utilizado para la importación y limpieza de datos.
    - flexdashboard: Framework para la creación de paneles interactivos en R.
    - shiny: Paquete en R que facilita la creación de aplicaciones web interactivas
    - tidyverse: Colección de paquetes de R para la manipulación y visualización de datos.
    - tidymodels: Ecosistema de paquetes para el modelado y machine learning en R, utilizado para construir y evaluar los modelos predictivos.
    workflows: Paquete de tidymodels para gestionar y entrenar modelos de machine learning de manera estructurada y reproducible.
