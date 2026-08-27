# Analisis-Multivariado en R



Talleres y ejercicios de Análisis Estadístico Multivariado en R: normalidad multivariada, componentes principales, esfericidad de Bartlett y distribución normal bivariada.

## Contenido

1. **analisis_exploratorio_y_de_normalidad_multivariada_aplicado.R**
   Análisis exploratorio sobre un dataset hospitalario (consumo energético de quirófano, velocidad de procedimientos, tiempo por cirugía, complicaciones, tipo y tecnología del hospital). Incluye estadística descriptiva, gráficos univariados y bivariados, evaluación de normalidad multivariada (test de Mardia y Shapiro-Wilk), detección de outliers multivariados con distancia de Mahalanobis, prueba de esfericidad de Bartlett, correlaciones de Spearman entre variables, y una comparación de dos poblaciones independientes (hospitales públicos vs. privados) mediante prueba de homogeneidad de varianzas y matriz de covarianza combinada (pooled).

2. **Analisis_PCA.R**
   Análisis de Componentes Principales (ACP) sobre el dataset `mtcars`. Verifica si el ACP es pertinente (prueba de normalidad multivariada de Mardia y prueba de esfericidad de Bartlett), selecciona el número de componentes a retener usando el criterio de varianza acumulada (>70%), identifica qué variables contribuyen más a cada componente, e interpreta las relaciones entre variables y entre individuos (vehículos) mediante un biplot.

3. **Prueba_de_esfericidad_de_Bartlett_densidad_normal_bivariada_descomposicion_espectral_y_region_de_confianza_eliptica.R**
   Taller de tres partes: (1) análisis descriptivo y de correlación del dataset `airquality` (Ozono, Viento, Temperatura), con boxplot de temperatura por mes y prueba de esfericidad de Bartlett; (2) análisis descriptivo multivariado de un dataset simulado de hospitales (pacientes, médicos, presupuesto): medias, matriz de covarianza y matriz de correlación; (3) simulación de una densidad normal bivariada, sus contornos, descomposición espectral (autovalores y autovectores de Σ) y construcción de la región de confianza elíptica que contiene el 90% de la probabilidad.

4. **Sustentacion_densidad_normal_bivariada_descomposicion_espectral_y_region_de_confianza_eliptica.pdf**
   Documento de sustentación con las respuestas, gráficos e interpretaciones del taller del punto anterior (Ozono/Viento/Temperatura, relación pacientes-médicos-presupuesto, y contornos de densidad con su región de confianza elíptica). Trabajo en conjunto con Sumin Han.
