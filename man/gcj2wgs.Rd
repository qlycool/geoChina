\name{gcj2wgs}
\alias{gcj2wgs}
\title{Convert coordinates}
\usage{
gcj2wgs(gcjLat, gcjLon)
}
\arguments{
  \item{gcjLat}{a numeric latitude in GCJ-02}

  \item{gcjLon}{a numeric longitude in GCJ-02}
}
\value{
a data.frame with variables lat/lng
}
\description{
converts lat/lon coordintes from GCJ-02 to WGS-84
}
\details{
since the encryption function from WGS-84 to GCJ-02 doesn't
have inverse function, the inverse transformation can only
be achieved by numeric algorithm. Coordinates encryption
algorithm must ensure the relative position correct; that
is to say, if point A is close to point B in GCJ-02 system,
this relative spatial relationship persists in WGS-84
system. Therefore, when two points are close, we can
approximate to the right coordinates with the iteration
algorithm.
}
\examples{
\dontrun{
# latitude/longitude coordinates of Beijing railway station
# WGS-84: (39.90105, 116.42079)
# GCJ-02: (39.90245, 116.42703)
gcj2wgs(39.90245, 116.42703) # correct verifying by google earth
}
}
\author{
Jun Cai (\email{cai-j12@mails.tsinghua.edu.cn}), PhD
student from Center for Earth System Science, Tsinghua
University
}
\seealso{
\code{\link{wgs2gcj}}, \code{\link{conv}}.

the comments of blog
\url{http://blog.csdn.net/coolypf/article/details/8686588}
in Chinese
}

