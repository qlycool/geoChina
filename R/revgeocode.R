revgeocode <- function(latlng, ics = c('WGS-84', 'GCJ-02', 'BD-09'), 
                       api = c('google', 'baidu'), key = '', 
                       output = c('address', 'addressc'), messaging = FALSE){
  # check parameters
  stopifnot(is.numeric(latlng) & length(latlng) == 2)
  ics <- match.arg(ics)
  api <- match.arg(api)
  stopifnot(is.character(key))
  output <- match.arg(output)
  stopifnot(is.logical(messaging))
  
  # format url
  if(api == 'google'){
    # convert coordinates
    latlng <- conv(latlng[1], latlng[2], from = ics, to = 'GCJ-02')
    
    # http://maps.googleapis.com/maps/api/geocode/json?latlng=LAT,LNG
    # &sensor=FALSE&key=API_KEY
    url_string <- paste('http://maps.googleapis.com/maps/api/geocode/json?latlng=', 
                        latlng[1], ',', latlng[2], '&sensor=false', sep = '')
    if(nchar(key) > 0){
      url_string <- paste(url_string, '&key=', key, sep = '')
    }
  }
  if(api == 'baidu'){
    # coordinate type lookup table
    code <- c('wgs84ll', 'gcj02ll', 'bd09ll')
    names(code) <- c('WGS-84', 'GCJ-02', 'BD-09')
    coordtype <- code[ics]
    # http://api.map.baidu.com/geocoder/v2/?location=LAT,LNG&coordtype=COORDTYPE
    # &output=json&ak=API_KEY
    url_string <- paste('http://api.map.baidu.com/geocoder/v2/?location=', 
                        latlng[1], ',', latlng[2], '&coordtype=', coordtype, 
                        '&output=json&ak=', key, sep = '')
  }
  
  url_string <- URLencode(url_string)
  if(messaging) message(paste('calling ', url_string, ' ... ', sep = ''), appendLF = F)
  
  # reverse gecode
  connect <- url(url_string)
  rgc <- fromJSON(paste(readLines(connect, warn = FALSE), collapse = ''))
  if(messaging) message('done.')  
  close(connect)
  
  # reverse geocoding results
  if(api == 'google'){
    # did reverse geocoding fail?
    if(rgc$status != 'OK'){
      warning(paste('reverse geocode failed with status ', gc$status, ', location = "', 
                    latlng[1], ', ', latlng[2], '"', sep = ''), call. = FALSE)
      return(data.frame(address = NA))  
    }
    
    # more than one address found?
    if(length(rgc$results) > 1 && messaging){
      message(paste('more than one address found for "', latlng[1], ', ', 
                    latlng[2],  '", reverse geocoding first ...\n', sep = ''))
    }
    
    rgcdf <- with(rgc$results[[1]], {data.frame(address = formatted_address, 
                                                row.names = NULL)})
    for(i in seq_along(rgc$results[[1]]$address_components)){
      rgcdf <- cbind(rgcdf, rgc$results[[1]]$address_components[[i]]$long_name)
    }
    names(rgcdf) <- c('address', sapply(rgc$results[[1]]$address_components, 
                                        function(l) l$types[1]))
  }
  if(api == 'baidu'){
    # did geocode fail?
    if(rgc$status != 0){
      warning(paste('geocode failed with status code ', rgc$status, ', location = "', 
                    latlng[1], ', ', latlng[2],  '". see more details in the response code table of Baidu Geocoding API', 
                    sep = ''), call. = FALSE)
      return(data.frame(address = NA))
    }
    
    rgcdf <- with(rgc$result, {
      data.frame(address = formatted_address, 
                 street_number = NULLtoNA(addressComponent['street_number']), 
                 street = NULLtoNA(addressComponent['street']), 
                 district = NULLtoNA(addressComponent['district']), 
                 city = NULLtoNA(addressComponent['city']), 
                 province = NULLtoNA(addressComponent['province']), 
                 row.names = NULL)})
  }
  
  if(output == 'address') return(rgcdf['address'])
  if(output == 'addressc') return(rgcdf)
}

# revgeocode(c(39.90105, 116.42079), ics = 'WGS-84', api = 'google', output = 'address', 
#            messaging = TRUE)
# revgeocode(c(39.90245, 116.42703), ics = 'GCJ-02', api = 'google', output = 'address', 
#            messaging = TRUE)
# revgeocode(c(39.90245, 116.42703), ics = 'GCJ-02', api = 'google', output = 'addressc', 
#            messaging = TRUE)
# revgeocode(c(39.90851, 116.43351), ics = 'BD-09', api = 'baidu', 
#            key = 'kgR30zPz0Rp7f36obLDtiEjK', output = 'address', messaging = TRUE)
# revgeocode(c(39.90851, 116.43351), ics = 'BD-09', api = 'baidu', 
#            key = 'kgR30zPz0Rp7f36obLDtiEjK', output = 'addressc', messaging = TRUE)
# revgeocode(c(39.90105, 116.42079), ics = 'WGS-84', api = 'baidu', 
#            key = 'kgR30zPz0Rp7f36obLDtiEjK', output = 'address', messaging = TRUE)
# revgeocode(c(39.90245, 116.42703), ics = 'GCJ-02', api = 'baidu', 
#            key = 'kgR30zPz0Rp7f36obLDtiEjK', output = 'address', messaging = TRUE)