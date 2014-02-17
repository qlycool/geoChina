\name{wgs2bd}
\alias{wgs2bd}
\title{Convert coordinates}
\usage{
wgs2bd(wgsLat, wgsLon)
}
\arguments{
  \item{wgsLat}{a numeric latitude in WGS-84}

  \item{wgsLon}{a numeric longitude in WGS-84}
}
\value{
a data.frame with variables lat/lng
}
\description{
converts lat/lon coordintes from WGS-84 to BD-09
}
\details{
convert WGS-84 coordinates first into GCJ-02, then into
BD-09.
}
\examples{
\dontrun{
# latitude/longitude coordinates of Beijing railway station
# WGS-84: (39.90105, 116.42079)
# BD-09:  (39.90851, 116.43351)
wgs2bd(39.90105, 116.42079) # correct
}
}
\author{
Jun Cai (\email{cai-j12@mails.tsinghua.edu.cn}), PhD
student from Center for Earth System Science, Tsinghua
University
}
\seealso{
\code{\link{wgs2gcj}}, \code{\link{gcj2bd}},
\code{\link{conv}}.
}

