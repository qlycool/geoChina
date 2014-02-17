#' Reverse geocode
#'
#' reverse geocodes a lat/lng location using Google or Baidu Maps.  Note that in 
#' most cases by using this function you are agreeing to the Google Maps API Terms 
#' of Service at \url{https://developers.google.com/maps/terms} or the Baidu Maps 
#' API Terms of Use at \url{http://developer.baidu.com/map/law.htm}.
#' 
#' @param latlng a location in latitude/longitude format
#' @param ics the coordinate system of inputing location, including WGS-84, GCJ-02 
#' and BD-09, which are the GCSs of Google Earth, Google Map in China and Baidu 
#' Map, respectively.
#' @param api use google or baidu maps api
#' @param key an api key must be provided when calling baidu maps api. 
#' While it's unnecessary for calling google maps api.
#' @param output formatted address or formmatted address with address components
#' @param messaging turn messaging on/off
#' @return a data.frame with variables address or detail address components 
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD student from 
#' Center for Earth System Science, Tsinghua University
#' @details note that the google maps api limits to 2500 queries a day.
#' @seealso \code{\link{geocode}}.
#' 
#' Google Maps API at \url{http://code.google.com/apis/maps/documentation/geocoding/} 
#' and Baidu Maps API at \url{http://developer.baidu.com/map/webservice-geocoding.htm}
#' @export
#' @examples
#' \dontrun{
#' # reverse geocode Beijing railway station
#' revgeocode(c(39.90105, 116.42079), ics = 'WGS-84', api = 'google', 
#'            output = 'address')
#' revgeocode(c(39.90245, 116.42703), ics = 'GCJ-02', api = 'google', 
#'            output = 'address', messaging = TRUE)
#' revgeocode(c(39.90851, 116.43351), ics = 'BD-09', api = 'google', 
#'            output = 'addressc')
#' revgeocode(c(39.90851, 116.43351), ics = 'BD-09', api = 'baidu', 
#'            key = 'your baidu maps api key', output = 'address')
#' revgeocode(c(39.90245, 116.42703), ics = 'GCJ-02', api = 'baidu', 
#'            key = 'your baidu maps api key', output = 'address', messaging = TRUE)
#' revgeocode(c(39.90105, 116.42079), ics = 'WGS-84', api = 'baidu', 
#'            key = 'your baidu maps api key', output = 'addressc')
#' }

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