# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
library(openxlsx)
library(tidyr)
library(FSA)

# Cargar los datos desde el archivo Excel
data <- read.xlsx("VIAJES_reducidos.xlsx", sheet = 1)

# Convertir variables categóricas a factores si es necesario
data$GENERO <- as.factor(data$GENERO)
data$EDAD <- as.factor(data$EDAD)
data$PAIS_RESIDENCIA <- as.factor(data$PAIS_RESIDENCIA)
data$CIUDAD_RESIDENCIA <- as.factor(data$CIUDAD_RESIDENCIA)
data$VIAJO <- as.factor(data$VIAJO)
data$PRIMERA_VEZ <- as.factor(data$PRIMERA_VEZ)
data$PROPOSITO_VIAJE <- as.factor(data$PROPOSITO_VIAJE)

# Crear una nueva variable de edad combinada
data$EDAD_COMBINADA <- ifelse(data$EDAD %in% c("18 a 30 años", "31 a 45 años"), "18 a 45 años", as.character(data$EDAD))

# Verificar la nueva variable
table(data$EDAD_COMBINADA)

# Combinar los países de residencia en las categorías especificadas
data$PAIS_RESIDENCIA <- as.character(data$PAIS_RESIDENCIA)
data$PAIS_RESIDENCIA <- ifelse(data$PAIS_RESIDENCIA %in% c("Alemania", "España", "Francia", "Croacia"), "Europa", 
                               ifelse(data$PAIS_RESIDENCIA %in% c("USA", "Canada"), "USA y Canada", 
                                      ifelse(data$PAIS_RESIDENCIA == "Israel", "Asia", "Other American Countries")))
data$PAIS_RESIDENCIA <- as.factor(data$PAIS_RESIDENCIA)

# Verificar la nueva variable de país de residencia
table(data$PAIS_RESIDENCIA)

# Mostrar una vista previa del dataframe modificado
#head(data)

# Simular los datos de turismo urbano (suponiendo que se llamen así en tu archivo)
# Aquí estoy creando datos de ejemplo, pero en tu caso, debes leer los datos de tu archivo Excel
data <- data.frame(
  AMBIENTE_SOCIAL_URBANO = rnorm(100, mean=3.5, sd=1),
  ARQUITECTURA_URBANA = rnorm(100, mean=3.4, sd=1),
  MONUMENTOS_SITIOS = rnorm(100, mean=3.6, sd=1),
  ESPACIOS_PUBLICOS = rnorm(100, mean=3.5, sd=1),
  ALOJAMIENTO_RESTAURANTES = rnorm(100, mean=4.3, sd=1),
  SERVICIOS_PUBLICOS = rnorm(100, mean=3.2, sd=1),
  INFORMACION_TURISTICA = rnorm(100, mean=3.4, sd=1),
  TIENDAS_SERVICIOS = rnorm(100, mean=3.4, sd=1),
  MUSEOS_GALERIAS = rnorm(100, mean=3.3, sd=1),
  ACCESO_SENALIZACION = rnorm(100, mean=3.2, sd=1),
  EXCURSIONES = rnorm(100, mean=3.0, sd=1),
  FESTIVALES_EVENTOS = rnorm(100, mean=2.9, sd=1),
  TEATROS_CONCIERTOS = rnorm(100, mean=3.2, sd=1),
  FERIAS_CONVENCIONES = rnorm(100, mean=3.1, sd=1),
  SEGMENTO = sample(c("Servicios de alojamiento y restaurante", "Múltiples atracciones", "Turismo pasivo"), 100, replace = TRUE)
)

# Convertir la columna SEGMENTO a factor si no lo es
data$SEGMENTO <- as.factor(data$SEGMENTO)

# Variables de interés
variables <- c("AMBIENTE_SOCIAL_URBANO", "ARQUITECTURA_URBANA", "MONUMENTOS_SITIOS", "ESPACIOS_PUBLICOS",
               "ALOJAMIENTO_RESTAURANTES", "SERVICIOS_PUBLICOS", "INFORMACION_TURISTICA", "TIENDAS_SERVICIOS",
               "MUSEOS_GALERIAS", "ACCESO_SENALIZACION", "EXCURSIONES", "FESTIVALES_EVENTOS", "TEATROS_CONCIERTOS",
               "FERIAS_CONVENCIONES")

# Inicializar dataframe para resultados
resultados <- data.frame(Variable = character(),
                         Servicios_aloja_rest = numeric(),
                         Multiples_atracciones = numeric(),
                         Turismo_pasivo = numeric(),
                         Kruskal_Wallis_H = numeric(),
                         Sig = numeric(),
                         Mann_Whitney = character(),
                         stringsAsFactors = FALSE)

# Función para obtener resultados de Mann-Whitney
get_mann_whitney <- function(data, var) {
  mw <- pairwise.wilcox.test(data[[var]], data$SEGMENTO, p.adjust.method = "bonferroni")
  res <- mw$p.value
  comparisons <- c("Servicios de alojamiento y restaurante" = "1",
                   "Múltiples atracciones" = "2",
                   "Turismo pasivo" = "3")
  mw_res <- apply(res, 1, function(row) {
    paste(comparisons[rownames(res)], comparisons[colnames(res)], row, sep = " vs ")
  })
  return(paste(mw_res, collapse = "; "))
}

# Calcular estadísticas
for (var in variables) {
  # Calcular medias por segmento
  medias <- data %>% group_by(SEGMENTO) %>% summarize(mean = mean(get(var), na.rm = TRUE)) %>% pull(mean)
  
  # Prueba de Kruskal-Wallis
  kw <- kruskal.test(get(var) ~ SEGMENTO, data = data)
  
  # Prueba de Mann-Whitney
  mw <- get_mann_whitney(data, var)
  
  # Agregar resultados al dataframe
  resultados <- resultados %>%
    add_row(Variable = var,
            Servicios_aloja_rest = medias[1],
            Multiples_atracciones = medias[2],
            Turismo_pasivo = medias[3],
            Kruskal_Wallis_H = round(kw$statistic, 2),
            Sig = round(kw$p.value, 3),
            Mann_Whitney = mw)
}
# Calcular estadísticas
for (var in variables) {
  # Prueba de Kruskal-Wallis
  kw <- kruskal.test(as.formula(paste(var, "~ SEGMENTO")), data = data)
  
  # Agregar resultados al dataframe
  resultados <- resultados %>%
    add_row(Variable = var,
            Kruskal_Wallis_H = round(kw$statistic, 3))
}

# Imprimir la tabla
print(resultados)

# Opcional: guardar los resultados en un archivo Excel
write.xlsx(resultados, "resultados_segmentacion_turismo_urbano.xlsx")