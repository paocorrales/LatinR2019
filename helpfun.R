library(reshape2)
library(lubridate)
library(data.table)
#=========================================================================================#
# inside:
# Crop del dominio global para obtener un dominio circular de radio 40km centrado en el radar.
# Mantiene solo los puntos dentro del dominio
# 
# rrgange y azimuth.
# Convierten lat y lon en grilla del radar. Devuelve el rango y el azimuth.
#=========================================================================================#



inside <- function(lon, lat, lon0 = -60.537289, lat0 = -31.848438, r = 40000) {
  m <- matrix(c(lon, lat), ncol = 2)
  d <- geosphere::distGeo(m, c(lon0, lat0))
  d <= r
}

rrange <- function(lon, lat, lon0 = -60.537289, lat0 = -31.848438) {
  m <- matrix(c(lon, lat), ncol = 2)
  d <- geosphere::distGeo(m, c(lon0, lat0))
  d
}

azimuth <- function(lon, lat, lon0 = -60.537289, lat0 = -31.848438) {
  m <- matrix(c(lon, lat), ncol = 2)
  d <- geosphere::bearingRhumb(m, c(lon0, lat0))
  d <- ConvertLongitude(d - 180)
}

cuad_split <- function(azimut) {
  cuadrants <- cut(azimut, breaks = c(0, 90, 180, 270, 360), labels = as.character(1:4))
  groups <- vector("numeric", length = length(azimut))
  cur.group <- 1
  groups[1] <- cur.group
  for (i in seq_along(azimut)[-1]) {
    if (cuadrants[i] != cuadrants[i-1]) cur.group <- groups[i-1] + 1
    groups[i] <- cur.group
  }
  return(groups)
}

#=========================================================================================#
# errores:
# rms, rre, bias, rmse, rmsens
#=========================================================================================#

rms <- function(p, p_ref){
  resta <- p - p_ref
  n <- length(p)
  rms <- sqrt(sum(resta^2, na.rm = T)/n)
  rms
}

rre <- function(p, p_ref){
  resta <- p - p_ref
  n <- length(p)
  rre = sqrt(sum(resta^2, na.rm = T)/sum(p_ref^2, na.rm = T))
  rre
}


bias.f <- function(mod, obs){
  
  N <- length(mod)
  resta <- mod - obs
  bias <- sum(resta, na.rm = T)/N
  bias
}

rmse.f <- function(mod, obs){
  
  N <- length(mod)
  resta <- mod - obs
  rmse <- sqrt(sum(resta^2, na.rm = T)/N)
  rmse
}


rmsens.f <- function(mod, obs){
  
  N <- length(mod)
  bias <- bias.f(mod, obs)
  resta <- mod - obs - bias
  rmsens <- sqrt(sum(resta^2, na.rm = T)/N)
  rmsens
}

bias.c <- function(dif.circ){
  bias <- sum(dif.circ, na.rm = T)/length(dif.circ)
  bias
}

rmse.c <- function(dif.circ){
  
  rmse <- sqrt(sum(dif.circ^2, na.rm = T)/length(dif.circ))
  rmse
}

rmsens.c <- function(dif.circ){

  bias <- bias.c(dif.circ)
  resta <- dif.circ - bias
  rmsens <- sqrt(sum(resta^2, na.rm = T)/length(dif.circ))
  rmsens
}

geom_label_contour2 <- function(...) {
  list(geom_label_contour(fill = "white", label.r = unit(0, "lines"),
                          label.padding = unit(0.04, "lines"), color = NA, ...),
       geom_text_contour(..., rotate = FALSE))
}
