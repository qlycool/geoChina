library(RCurl)
library(RJSONIO)

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE) {
  require("plyr")
  if(verbose) cat(address,"\n")
  u <- aaply(address,1,construct.geocode.url)
  doc <- aaply(u,1,getURL)
  json <- alply(doc,1,fromJSON,simplify = FALSE)
  coord = laply(json,function(x) {
    if(x$status=="OK") {
      lat <- x$results[[1]]$geometry$location$lat
      lng <- x$results[[1]]$geometry$location$lng
      return(c(lat, lng))
    } else {
      return(c(NA,NA))
    }
  })
  if(length(address)>1) colnames(coord)=c("lat","lng")
  else names(coord)=c("lat","lng")
  return(data.frame(address,coord))
}