source('R/cst.R')
library(plyr)

geocode <- function(address, api = c('google', 'baidu'), key = '', 
                    ocs = c('WGS-84', 'GCJ-02', 'BD-09'), 
                    output = c('latlng', 'latlngc'), messaging = FALSE){
  # check parameters
  stopifnot(is.character(address))
  api <- match.arg(api)
  stopifnot(is.character(key))
  output <- match.arg(output)
  ocs <- match.arg(ocs)
  stopifnot(is.logical(messaging))
  
  # vectorize for many addresses
  if(length(address) > 1){
    if(api == 'google'){
      s <- 'google restricts requests to 2500 requests a day.'
      if(length(address) > 2500) stop(s, call. = F)
      if(length(address) > 200 & messaging) message(paste('Reminder', s, sep = ' : '))
    }
          
    if(output == 'latlng' | output == 'latlngc'){
      return(ldply(as.list(address), geocode, api = api, key = key, ocs = ocs, 
                   output = output, messaging = messaging))
    }
  }
  
  # format url
  if(api == 'google'){
    # http://maps.googleapis.com/maps/api/geocode/json?address=ADDRESS&sensor
    # =false&key=API_KEY
    url_string <- paste('http://maps.googleapis.com/maps/api/geocode/json?address=', 
                        address, '&sensor=false', sep = '')
    if(nchar(key) > 0){
      url_string <- paste(url_string, '&key=', key, sep = '')
    }
  }
  if(api == 'baidu'){
    # http://api.map.baidu.com/geocoder/v2/?address=ADDRESS&output=json&ak=API_KEY
    url_string <- paste('http://api.map.baidu.com/geocoder/v2/?address=', address, 
                        '&output=json&ak=', key, sep = '')
  }
  
  url_string <- URLencode(url_string)
  if(messaging) message(paste('calling ', url_string, ' ... ', sep = ''), appendLF = F)
  
  # gecode
  connect <- url(url_string)
  gc <- fromJSON(paste(readLines(connect, warn = FALSE), collapse = ''))
  if(messaging) message('done.')  
  close(connect)
  
  # format geocoded data
  NULLtoNA <- function(x){
    if(is.null(x)) return(NA)
    x
  }
  
  # geocoding results
  if(api == 'google'){
    # did geocode fail?
    if(gc$status != 'OK'){
      warning(paste('geocode failed with status ', gc$status, ', location = "', 
                    address, '"', sep = ''), call. = FALSE)
      return(data.frame(lat = NA, lng = NA))  
    }
    
    # more than one location found?
    if(length(gc$results) > 1 && messaging){
      message(paste('more than one location found for "', address, 
                    '", using address\n  "', tolower(gc$results[[1]]$formatted_address), 
                    '"\n', sep = ''))
    }
    
    gcdf <- with(gc$results[[1]], {
      data.frame(lat = NULLtoNA(geometry$location['lat']), 
                 lng = NULLtoNA(geometry$location['lng']), 
                 loctype = tolower(NULLtoNA(geometry$location_type)), 
                 row.names = NULL)})
    
    # convert coordinates
    gcdf[, c('lat', 'lng')] <- conv(gcdf[, 'lat'], gcdf[, 'lng'], from = 'GCJ-02', to = ocs)
    
    if(output == 'latlng') return(gcdf[, c('lat', 'lng')])
    if(output == 'latlngc') return(gcdf[, c('lat', 'lng', 'loctype')])
  }
  if(api == 'baidu'){
    # did geocode fail?
    if(gc$status != 0){
      warning(paste('geocode failed with status code ', gc$status, ', location = "', 
                    address, '". see more details in the response code table of Baidu Geocoding API', 
                    sep = ''), call. = FALSE)
      return(data.frame(lat = NA, lng = NA))  
    }
    
    gcdf <- with(gc$result, {data.frame(lat = NULLtoNA(location['lat']), 
                                        lng = NULLtoNA(location['lng']), 
                                        conf = NULLtoNA(confidence), 
                                        row.names = NULL)})
    
    # convert coordinates
    gcdf[, c('lat', 'lng')] <- conv(gcdf[, 'lat'], gcdf[, 'lng'], from = 'BD-09', to = ocs)
    
    if(output == 'latlng') return(gcdf[, c('lat', 'lng')])
    if(output == 'latlngc') return(gcdf[, c('lat', 'lng', 'conf')])
  }
}

# geocode('inexisting place', api = 'google', ocs = 'WGS-84', messaging = TRUE)
# geocode(c('Tsinghua University', 'Beijing railway station'), api = 'google', 
# ocs = 'GCJ-02', messaging = TRUE)
# geocode('Beijing railway station', api = 'google', ocs = 'GCJ-02', output = 'latlngc', 
#         messaging = TRUE)
# geocode(c('Tsinghua University', 'Beijing railway station'), api = 'google', 
#         ocs = 'GCJ-02', output = 'latlngc', messaging = TRUE)
# geocode('北京火车站', api = 'google', ocs = 'GCJ-02', messaging = TRUE)
# geocode('北京站', api = 'google', ocs = 'WGS-84', messaging = TRUE)
# geocode('北京站', api = 'baidu', ocs = 'WGS-84', messaging = TRUE)
# geocode('北京火车站', api = 'baidu', key = 'kgR30zPz0Rp7f36obLDtiEjK', ocs = 'BD-09', 
#         messaging = TRUE)
# geocode(c('清华大学', '北京火车站'), api = 'baidu', key = 'kgR30zPz0Rp7f36obLDtiEjK', ocs = 'BD-09', 
#         messaging = TRUE)
# geocode('北京火车站', api = 'baidu', key = 'kgR30zPz0Rp7f36obLDtiEjK', ocs = 'BD-09', 
#         output = 'latlngc', messaging = TRUE)
# geocode(c('清华大学', '北京火车站'), api = 'baidu', key = 'kgR30zPz0Rp7f36obLDtiEjK', ocs = 'BD-09', 
#         output = 'latlngc', messaging = TRUE)
# geocode('北京市北京火车站', api = 'baidu', key = 'kgR30zPz0Rp7f36obLDtiEjK', ocs = 'GCJ-02', 
#         messaging = TRUE)
# geocode('北京市北京火车站', api = 'baidu', key = 'kgR30zPz0Rp7f36obLDtiEjK', ocs = 'WGS-84', 
#         messaging = TRUE)
