########## Inicializacion del script ##########

# Eliminamos las posibles variables de una sesion anterior
rm(list = ls())

# Directorio de trabajo
setwd("C:/Users/sergr/Dropbox/TFM_Sergio_Gracia/results")
#setwd("/home/juaco/Dropbox/TFM_Sergio_Gracia")

# Crear directorios según threshold
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
by = 0.05
thresholds <- seq(0,1-by,by = by)

for(th in thresholds){
  th.form <- specify_decimal(th, 2)
  decimal <- substr(th.form, 3, 4)
  path <- paste0("th_0",decimal)
  dir.create(path, showWarnings = TRUE, recursive = FALSE)
}
