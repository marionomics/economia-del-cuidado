library(tidyverse)

temp <- tempfile()
download.file('https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_poblacion_csv.zip', temp)
poblacion <- read.csv(unz(temp, 'poblacion.csv'))


poblacion[,c(5,7,9,25,28:31,46,48)] <-
  sapply(poblacion[,c(5,7,9,25,28:31,46,48)],
         function(x) 2-x)

######################################
# Entidades
poblacion$entidad <- floor(poblacion$X...folioviv/100000000)
min(poblacion$entidad)
max(poblacion$entidad)

entidades <- c('ags', 'bc', 'bcs', 'camp',
               'coah', 'col', 'chia', 'chih', 
               'cdmx', 'dur', 'gto', 'gue', 'hgo',
               'jal', 'mex', 'mich', 'mor', 'nay',
               'nl', 'oax', 'pue', 'que', 'qroo',
               'slp', 'sin', 'son', 'tab', 'tamp',
               'tlax', 'ver', 'yuc', 'zac')

poblacion$ent[poblacion$entidad %in% 1:32] <- entidades[match(poblacion$entidad, 1:32)]

# Cambiar las escalas de las preguntas sobre redes sociales para que asemeje una escala de likert
poblacion[,51:56] <- sapply(poblacion[,51:56],
                            function(x) ifelse(x > 2,
                                               abs(((x %% 5)) - 1) + 2,
                                               x))
poblacion$rrss <- rowMeans(poblacion[,51:56], na.rm = T)

# Una dummy con valor 1 si existe algun tipo de discapacidad
poblacion$disc <- ifelse(poblacion$disc1 == 8, 0, 1)

# Existencia de poblaciones de cierta edad
poblacion$edad1a3 <- ifelse(poblacion$edad %in% 1:3, 1, 0)
poblacion$edad4a6 <- ifelse(poblacion$edad %in% 4:6, 1, 0)
poblacion$edad7a14 <- ifelse(poblacion$edad %in% 7:14, 1, 0)
poblacion$edad15a18 <- ifelse(poblacion$edad %in% 15:18, 1, 0)
poblacion$edad65mas <- ifelse(poblacion$edad %in% 65:max(poblacion$edad), 1, 0)

poblacion[,(ncol(poblacion) - 4):ncol(poblacion)] %>%
  colSums()/nrow(poblacion)





##########################
# Cargar  Base de datos de Ingresos
path <- "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_ingresos_csv.zip"

temp <- tempfile()
download.file(path, temp)
ingresos <- read.csv(unz(temp, 'ingresos.csv'))

############################################

# Make final dataset

# Get the data that we need summarized in vivienda

# Ingresos trimestrales por hogar
ing <- ingresos %>%
  group_by(X...folioviv) %>%
  summarize(ing_fam = sum(ing_tri)/3)

df <- poblacion %>%
  group_by(X...folioviv, ent, disc, edad1a3,
           edad4a6, edad15a18, asis_esc) %>%
  summarize(integrantes = n(),
            redes_sociales = mean(rrss, na.rm = TRUE)) %>%
  left_join(ing, by = "X...folioviv")
df

rm(ing, ingresos, poblacion, entidades, path, temp)