\name{bd2gcj}
\alias{bd2gcj}
\title{Convert coordinates}
\usage{
bd2gcj(bdLat, bdLon)
}
\arguments{
  \item{bdLat}{a numeric latitude in BD-09}

  \item{bdLon}{a numeric longitude in BD-09}
}
\value{
a data.frame with variables lat/lng
}
\description{
converts lat/lon coordintes from BD-09 to GCJ-02
}
\details{
BD-09 coordinate system is used by Baidu Maps and encrypted
based on GCJ-02 coordinates for information safety.
}
\examples{
\dontrun{
# latitude/longitude coordinates of Beijing railway station
# GCJ-02: (39.90245, 116.42703)
# BD-09:  (39.90851, 116.43351)
bd2gcj(39.90851, 116.43351) # correct
}
}
\author{
Jun Cai (\email{cai-j12@mails.tsinghua.edu.cn}), PhD
student from Center for Earth System Science, Tsinghua
University
}
\seealso{
\code{\link{gcj2bd}}, \code{\link{conv}}.

\url{http://blog.csdn.net/coolypf/article/details/8569813}
for C version source code.
}

