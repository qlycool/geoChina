#' Geocode
#'
#' geocodes an address using Google or Baidu Maps.  Note that in most cases by 
#' using this function you are agreeing to the Google Maps API Terms of Service 
#' at \url{https://developers.google.com/maps/terms} or the Baidu Maps API Terms 
#' of Use at \url{http://developer.baidu.com/map/law.htm}.
#' 
#' @param address a character string specifying a location of interest (e.g. 
#' "Tsinghua Univeristy")
#' @param api use google or baidu maps api
#' @param key an api key must be provided when calling baidu maps api. 
#' While it's unnecessary for calling google maps api.
#' @param ocs output coordinate systems including WGS-84, GCJ-02 and BD-09, which
#' are the GCSs of Google Earth, Google Map in China and Baidu Map, respectively.
#' @param output lat/lng coordinates or lat/lng coordinates with confidence
#' @param messaging turn messaging on/off
#' @return a data.frame with variables lat/lng or lat/lng/conf 
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD student from 
#' Center for Earth System Science, Tsinghua University
#' @details note that the google maps api limits to 2500 queries a day.
#' @seealso \code{\link{revgeocode}}.
#' 
#' Google Maps API at \url{http://code.google.com/apis/maps/documentation/geocoding/} 
#' and Baidu Maps API at \url{http://developer.baidu.com/map/webservice-geocoding.htm}
#' @export
#' @import plyr
#' @examples
#' \dontrun{
#' geocode('Tsinghua University', api = 'google', ocs = 'GCJ-02')
#' geocode('Tsinghua University', api = 'google', ocs = 'WGS-84', 
#'         messaging = TRUE)
#' geocode('Beijing railway station', api = 'google', ocs = 'WGS-84', 
#'         output = 'latlngc')
#' geocode(c('Tsinghua University', 'Beijing railway station'), api = 'google', 
#'         ocs = 'GCJ-02')
#' geocode(c('Tsinghua University', 'Beijing railway station'), api = 'google', 
#'         ocs = 'WGS-84', output = 'latlngc', messaging = TRUE)
#' geocode('Beijing railway station', api = 'baidu', key = 'your baidu maps api key', 
#'         ocs = 'BD-09')
#' geocode('Beijing railway station', api = 'baidu', key = 'your baidu maps api key', 
#'         ocs = 'GCJ-02', messaging = TRUE)
#' geocode('Beijing railway station', api = 'baidu', key = 'your baidu maps api key', 
#'         ocs = 'BD-09', output = 'latlngc')
#' geocode(c('Tsinghua University', 'Beijing railway station'), api = 'baidu', 
#'         key = 'your baidu maps api key', ocs = 'BD-09')
#' geocode(c('Tsinghua University', 'Beijing railway station'), api = 'baidu', 
#'         key = 'your baidu maps api key', ocs = 'WGS-84', output = 'latlngc')
#' }

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
      return(ldply(address, geocode, api = api, key = key, ocs = ocs, 
                   output = output, messaging = messaging))
    }
  }
  
  # location encoding
  address <- enc2utf8(address)
  
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
      Encoding(gc$results[[1]]$formatted_address) <- "UTF-8"
      message(paste('more than one location found for "', address, 
                    '", using address\n"', tolower(gc$results[[1]]$formatted_address), 
                    '"\n', sep = ''))
    }
    
    gcdf <- with(gc$results[[1]], {
      data.frame(lat = NULLtoNA(geometry$location['lat']), 
                 lng = NULLtoNA(geometry$location['lng']), 
                 loctype = tolower(NULLtoNA(geometry$location_type)), 
                 row.names = NULL)})
    
    # convert coordinates
    gcdf[c('lat', 'lng')] <- conv(gcdf[, 'lat'], gcdf[, 'lng'], from = 'GCJ-02', to = ocs)
    
    if(output == 'latlng') return(gcdf[c('lat', 'lng')])
    if(output == 'latlngc') return(gcdf[c('lat', 'lng', 'loctype')])
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
    gcdf[c('lat', 'lng')] <- conv(gcdf[, 'lat'], gcdf[, 'lng'], from = 'BD-09', to = ocs)
    
    if(output == 'latlng') return(gcdf[c('lat', 'lng')])
    if(output == 'latlngc') return(gcdf[c('lat', 'lng', 'conf')])
  }
}

# fill NULL with NA
NULLtoNA <- function(x){
  if(is.null(x)) return(NA)
  if(is.character(x) & nchar(x) == 0) return(NA)
  x
}