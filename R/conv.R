#' Convert coordinates
#'
#' the general function that converts lat/lon coordintes from one GCS to another 
#' GCS including WGS-84, GCJ-02 and BD-09 either locally or by calling Baidu 
#' Maps API.
#' 
#' @param lat a numeric latitude
#' @param lon a numeric longitude
#' @param from the inputting GCS
#' @param to the outputting GCS
#' @param api use baidu maps api. Note that baidu maps api only supports the 
#' transformations from WGS-84 or GCJ-02 to BD-09. Other coodinate conversions 
#' must be done locally. As the conversion result is the same, it's recommended 
#' to perform conversions locally.
#' @return a data.frame with variables lat/lng 
#' @author Jun Cai (\email{cai-j12@@mails.tsinghua.edu.cn}), PhD student from 
#' Center for Earth System Science, Tsinghua University
#' @details note that the baidu maps api limits to 20 lat/lon coordinates per query. 
#' Since the coordinate conversion results of Baidu Maps API and local algorithms 
#' are the same, it is recommended to use local algorithms.
#' @seealso \code{\link{wgs2gcj}}, \code{\link{wgs2bd}}, \code{\link{gcj2wgs}}, 
#' \code{\link{gcj2bd}}, \code{\link{bd2wgs}}, \code{\link{bd2gcj}}.
#' @export
#' @import RCurl RJSONIO
#' @examples
#' \dontrun{
#' # latitude/longitude coordinates of Beijing railway station
#' # WGS-84: (39.90105, 116.42079)
#' # GCJ-02: (39.90245, 116.42703)
#' # BD-09:  (39.90851, 116.43351)
#' conv(39.90105, 116.42079, from = 'WGS-84', to = 'GCJ-02')
#' conv(39.90105, 116.42079, from = 'WGS-84', to = 'GCJ-02', api = TRUE)
#' conv(39.90105, 116.42079, from = 'WGS-84', to = 'BD-09')
#' conv(39.90105, 116.42079, from = 'WGS-84', to = 'BD-09', api = TRUE)
#' conv(39.90245, 116.42703, from = 'GCJ-02', to = 'WGS-84')
#' # not supported by baidu map api, return NAs
#' conv(39.90245, 116.42703, from = 'GCJ-02', to = 'WGS-84', api = TRUE)
#' conv(39.90245, 116.42703, from = 'GCJ-02', to = 'BD-09')
#' conv(39.90245, 116.42703, from = 'GCJ-02', to = 'BD-09', api = TRUE)
#' conv(39.90851, 116.43351, from = 'BD-09', to = 'GCJ-02')
#' # not supported by baidu map api, return NAs
#' conv(39.90851, 116.43351, from = 'BD-09', to = 'GCJ-02', api = TRUE)
#' conv(39.90851, 116.43351, from = 'BD-09', to = 'WGS-84')
#' # not supported by baidu map api, return NAs
#' conv(39.90851, 116.43351, from = 'BD-09', to = 'WGS-84', api = TRUE)
#' # convert multiple coordinates
#' lat = c(39.99837, 39.98565)
#' lng = c(116.3203, 116.2998)
#' conv(lat, lng, from = 'WGS-84', to = 'GCJ-02')
#' }

conv <- function(lat, lon, from = c('WGS-84', 'GCJ-02', 'BD-09'), 
                 to = c('WGS-84', 'GCJ-02', 'BD-09'), api = FALSE){  
  # check parameters
  stopifnot(is.numeric(lat))
  stopifnot(is.numeric(lon))
  from <- match.arg(from)
  to <- match.arg(to)
  stopifnot(is.logical(api))
  
  # vectorize
  if(length(lat) > 1){
    return(ldply(seq_along(lat), function(i){conv(lat[i], lon[i], from = from, 
                                                  to = to, api = api) }))
  }
  
  if(from == to){
    return(data.frame(lat = lat, lng = lon))
  } else{
    if(api){
      # coordinate system lookup table
      code <- c(0, 2, 4)
      names(code) <- c('WGS-84', 'GCJ-02', 'BD-09')
      f <- code[from]
      t <- code[to]
      
      # format url
      # http://api.map.baidu.com/ag/coord/convert?x=lon&y=lat&from=FROM&to=TO
      url_string <- paste('http://api.map.baidu.com/ag/coord/convert?x=', lon, 
                          '&y=', lat, '&from=', f, '&to=', t, sep = '')
      url_string <- URLencode(url_string)
      message(paste('calling ', url_string, ' ... ', sep = ''), appendLF = F)
      
      # convert
      connect <- url(url_string)
      cv <- fromJSON(paste(readLines(connect, warn = FALSE), collapse = ''), drop = FALSE)
      message('done.')  
      close(connect)
      
      # did convert fail?
      if(is.list(cv)){
        if(cv$error == 0){
          cvdf <- with(cv, {data.frame(lat = as.numeric(base64(y, FALSE)), 
                                       lng = as.numeric(base64(x, FALSE)), 
                                       row.names = NULL)})
          return(cvdf)
        }
      } else{
        warning(paste('convert failed with error ', cv['error'], 
                      '. conversion is not supported by baidu map api.',
                      sep = ''), 
                call. = FALSE)
        return(data.frame(lat = NA, lng = NA))
      }
    } else{
      if(from == 'WGS-84' & to == 'GCJ-02') return(wgs2gcj(lat, lon))
      if(from == 'WGS-84' & to == 'BD-09') return(wgs2bd(lat, lon))
      if(from == 'GCJ-02' & to == 'WGS-84') return(gcj2wgs(lat, lon))
      if(from == 'GCJ-02' & to == 'BD-09') return(gcj2bd(lat, lon))
      if(from == 'BD-09' & to == 'WGS-84') return(bd2wgs(lat, lon))
      if(from == 'BD-09' & to == 'GCJ-02') return(bd2gcj(lat, lon))
    }
  }
}