library(RCurl)
library(RJSONIO)

### convert by calling baidu api ###
conv <- function(lat, lon, from = c('WGS-84', 'GCJ-02', 'BD-09'), 
                 to = c('WGS-84', 'GCJ-02', 'BD-09'), api = FALSE){
  # check parameters
  stopifnot(is.numeric(lat))
  stopifnot(is.numeric(lon))
  from <- match.arg(from)
  to <- match.arg(to)
  stopifnot(is.logical(api))
  
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
        warning(paste('convert failed with error ', cv['error'], sep = ''), 
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
### convert by calling baidu api ###

# 北京站
# WGS-84 (39.90105, 116.42079)
# GCJ-02 (39.90245, 116.42703)
# BD-09  (39.90851, 116.43351)
# conv(39.90105, 116.42079, from = 'WGS-84', to = 'GCJ-02')
# conv(39.90105, 116.42079, from = 'WGS-84', to = 'GCJ-02', api = TRUE)
# conv(39.90105, 116.42079, from = 'WGS-84', to = 'BD-09')
# conv(39.90105, 116.42079, from = 'WGS-84', to = 'BD-09', api = TRUE)
# conv(39.90245, 116.42703, from = 'GCJ-02', to = 'WGS-84')
# conv(39.90245, 116.42703, from = 'GCJ-02', to = 'WGS-84', api = TRUE)
# conv(39.90245, 116.42703, from = 'GCJ-02', to = 'BD-09')
# conv(39.90245, 116.42703, from = 'GCJ-02', to = 'BD-09', api = TRUE)
# conv(39.90851, 116.43351, from = 'BD-09', to = 'GCJ-02')
# conv(39.90851, 116.43351, from = 'BD-09', to = 'GCJ-02', api = TRUE)
# conv(39.90851, 116.43351, from = 'BD-09', to = 'WGS-84')
# conv(39.90851, 116.43351, from = 'BD-09', to = 'WGS-84', api = TRUE)