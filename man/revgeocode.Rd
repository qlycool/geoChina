\name{revgeocode}
\alias{revgeocode}
\title{Reverse geocode}
\usage{
revgeocode(latlng, ics = c("WGS-84", "GCJ-02", "BD-09"), api = c("google",
  "baidu"), key = "", output = c("address", "addressc"),
  messaging = FALSE)
}
\arguments{
  \item{latlng}{a location in latitude/longitude format}

  \item{ics}{the coordinate system of inputing location,
  including WGS-84, GCJ-02 and BD-09, which are the GCSs of
  Google Earth, Google Map in China and Baidu Map,
  respectively.}

  \item{api}{use google or baidu maps api}

  \item{key}{an api key must be provided when calling baidu
  maps api. While it's unnecessary for calling google maps
  api.}

  \item{output}{formatted address or formmatted address
  with address components}

  \item{messaging}{turn messaging on/off}
}
\value{
a data.frame with variables address or detail address
components
}
\description{
reverse geocodes a lat/lng location using Google or Baidu
Maps.  Note that in most cases by using this function you
are agreeing to the Google Maps API Terms of Service at
\url{https://developers.google.com/maps/terms} or the Baidu
Maps API Terms of Use at
\url{http://developer.baidu.com/map/law.htm}.
}
\details{
note that the google maps api limits to 2500 queries a day.
}
\examples{
\dontrun{
# reverse geocode Beijing railway station
revgeocode(c(39.90105, 116.42079), ics = 'WGS-84', api = 'google',
           output = 'address')
revgeocode(c(39.90245, 116.42703), ics = 'GCJ-02', api = 'google',
           output = 'address', messaging = TRUE)
revgeocode(c(39.90851, 116.43351), ics = 'BD-09', api = 'google',
           output = 'addressc')
revgeocode(c(39.90851, 116.43351), ics = 'BD-09', api = 'baidu',
           key = 'your baidu maps api key', output = 'address')
revgeocode(c(39.90245, 116.42703), ics = 'GCJ-02', api = 'baidu',
           key = 'your baidu maps api key', output = 'address', messaging = TRUE)
revgeocode(c(39.90105, 116.42079), ics = 'WGS-84', api = 'baidu',
           key = 'your baidu maps api key', output = 'addressc')
}
}
\author{
Jun Cai (\email{cai-j12@mails.tsinghua.edu.cn}), PhD
student from Center for Earth System Science, Tsinghua
University
}
\seealso{
\code{\link{geocode}}.

Google Maps API at
\url{http://code.google.com/apis/maps/documentation/geocoding/}
and Baidu Maps API at
\url{http://developer.baidu.com/map/webservice-geocoding.htm}
}

