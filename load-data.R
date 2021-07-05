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
