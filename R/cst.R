# Coordinate transformation algorithms for coordinate systems including WGS-84, 
# GCJ-02 and BD-09

### WGS-84 => GCJ-02 ###
# reference: https://on4wp7.codeplex.com/SourceControl/changeset/view/21483#353936

# Krasovsky 1940 ellipsoid parameters
# semi-major axis
a <- 6378245.0
# inverse flattening: 1/f = 298.3
# flattening
f <- 0.00335233
# semi-minor axis
b <- a * (1 - f)
ee <- (a^2 - b^2) / a^2

wgs2gcj <- function(wgsLat, wgsLon){
  if(outofChina(wgsLat, wgsLon)){
    gcjLat <- wgsLat
    gcjLon <- wgsLat
    return(data.frame(lat = gcjLat, lng = gcjLon))
  }
  
  dLat <- transformLat(wgsLon - 105.0, wgsLat - 35.0)
  dLon <- transformLon(wgsLon - 105.0, wgsLat - 35.0)
  radLat <- wgsLat / 180.0 * pi
  magic <- sin(radLat)
  magic <- 1 - ee * magic * magic
  sqrtMagic <- sqrt(magic)
  dLat <- (dLat * 180.0) / ((a * (1 - ee)) / (magic * sqrtMagic) * pi)
  dLon <- (dLon * 180.0) / (a / sqrtMagic * cos(radLat) * pi)
  gcjLat <- wgsLat + dLat
  gcjLon <- wgsLon + dLon
  return(data.frame(lat = gcjLat, lng = gcjLon))
}

outofChina <- function(lat, lon){
  if(lon < 72.004 | lon > 137.8347) return(TRUE)
  if(lat < 0.8293 | lat > 55.8271) return(TRUE)
  return(FALSE)
}

transformLat <- function(x, y){
  ret <- -100.0 + 2.0 * x + 3.0 * y + 0.2 * y * y + 0.1 * x * y + 0.2 * sqrt(abs(x))
  ret <- ret + (20.0 * sin(6.0 * x * pi) + 20.0 * sin(2.0 * x * pi)) * 2.0 / 3.0
  ret <- ret + (20.0 * sin(y * pi) + 40.0 * sin(y / 3.0 * pi)) * 2.0 / 3.0
  ret <- ret + (160.0 * sin(y / 12.0 * pi) + 320.0 * sin(y * pi / 30.0)) * 2.0 / 3.0
  return(ret)
}

transformLon <- function(x, y){
  ret <- 300.0 + x + 2.0 * y + 0.1 * x * x +  0.1 * x * y + 0.1 * sqrt(abs(x))
  ret <- ret + (20.0 * sin(6.0 * x * pi) + 20.0 * sin(2.0 * x * pi)) * 2.0 / 3.0
  ret <- ret + (20.0 * sin(x * pi) + 40.0 * sin(x / 3.0 * pi)) * 2.0 / 3.0
  ret <- ret + (150.0 * sin(x / 12.0 * pi) + 300.0 * sin(x * pi / 30.0)) * 2.0 / 3.0
  return(ret)
}
### WGS-84 => GCJ-02 ###

### GCJ-02 => WGS-84 ###
# coordinates encrytion algorithm must ensure the relative position correct. 
# If point A is close to point B in GCJ-02 system, this relative spatial 
# relationship persists in WGS-84 system. therefore, when two points are close, 
# we can approximate to the right coordinates with the iteration algorithm.
# wgs => gcj
# offset dV = V' - V
# question: gcj => wgs, namely V = V' - dV'
# V' is known, while dV' is unknown.
# since dV is very close to dV', using dV to estimate dV'; however, to calculate
# dV, V must be known. since V and V' are very close to each other, initially 
# using V' to approximate V.
gcj2wgs <- function(gcjLat, gcjLon){
  g0 <- c(gcjLat, gcjLon)
  w0 <- g0
  g1 <- wgs2gcj(w0[1], w0[2])
  w1 <- w0 - (g1 - g0)
  while(max(abs(w1 - w0)) >= 1e-6){
    w0 <- w1
    g1 <- wgs2gcj(w0[1], w0[2])
    w1 <- w0 - (g1 - g0)
  }
  return(data.frame(lat = w1[1], lng = w1[2]))
}
### GCJ-02 => WGS-84 ###

### GCJ-02 <=> BD-09 ###
# reference: http://blog.csdn.net/coolypf/article/details/8569813
gcj2bd <- function(gcjLat, gcjLon){
  z <- sqrt(gcjLon^2 + gcjLat^2) + 0.00002 * sin(gcjLat * pi * 3000.0 / 180.0)
  theta <- atan2(gcjLat, gcjLon) + 0.000003 * cos(gcjLon * pi * 3000.0 / 180.0)
  bdLon = z * cos(theta) + 0.0065
  bdLat = z * sin(theta) + 0.006
  return(data.frame(lat = bdLat, lng = bdLon))
}

bd2gcj <- function(bdLat, bdLon){
  x <- bdLon - 0.0065
  y <- bdLat - 0.006  
  z <- sqrt(x^2 + y^2) - 0.00002 * sin(y * pi * 3000.0 / 180.0)
  theta <- atan2(y, x) - 0.000003 * cos(x * pi * 3000.0 / 180.0)  
  gcjLon <- z * cos(theta)  
  gcjLat <- z * sin(theta)
  return(data.frame(lat = gcjLat, lng = gcjLon))
}
### GCJ-02 => BD-09 ###

### BD-09 => GCJ-02 ###
# iteration algorithm same to gcj2wgs function
# bd2gcj <- function(bdLat, bdLon){
#   b0 <- c(bdLat, bdLon)
#   g0 <- b0
#   b1 <- gcj2bd(g0[1], g0[2])
#   g1 <- g0 - (b1 - b0)
#   while(max(abs(g1 - g0)) >= 1e-6){
#     g0 <- g1
#     b1 <- gcj2bd(g0[1], g0[2])
#     g1 <- g0 - (b1 - b0)
#   }
#   return(data.frame(lat = g1[1], lng = g[2]))
# }
### BD-09 => GCJ-02 ###

### WGS-84 <=> BD-09 ###
wgs2bd <- function(wgsLat, wgsLon){
  return(gcj2bd(wgs2gcj(wgsLat, wgsLon)[, 'lat'], wgs2gcj(wgsLat, wgsLon)[, 'lng']))
}

bd2wgs <- function(bdLat, bdLon){
  return(gcj2wgs(bd2gcj(bdLat, bdLon)[, 'lat'], bd2gcj(bdLat, bdLon)[, 'lng']))
}
### WGS-84 <=> BD-09 ###

# 北京站
# WGS-84 (39.90105, 116.42079)
# GCJ-02 (39.90245, 116.42703)
# BD-09  (39.90851, 116.43351)
# verifying
# wgs2gcj(39.90105, 116.42079) # correct
# wgs2bd(39.90105, 116.42079) # correct
# gcj2wgs(39.90245, 116.42703) # correct verifying by google earth
# gcj2bd(39.90245, 116.42703) # correct
# bd2gcj(39.90851, 116.43351) # correct
# bd2wgs(39.90851, 116.43351) # correct verifying by google earth
