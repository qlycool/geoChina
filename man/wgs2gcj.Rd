\name{wgs2gcj}
\alias{wgs2gcj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wgs2gcj(wgsLat, wgsLon)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wgsLat}{
%%     ~~Describe \code{wgsLat} here~~
}
  \item{wgsLon}{
%%     ~~Describe \code{wgsLon} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (wgsLat, wgsLon) 
{
    if (outofChina(wgsLat, wgsLon)) {
        gcjLat <- wgsLat
        gcjLon <- wgsLat
        return(c(gcjLat, gcjLon))
    }
    dLat <- transformLat(wgsLon - 105, wgsLat - 35)
    dLon <- transformLon(wgsLon - 105, wgsLat - 35)
    radLat <- wgsLat/180 * pi
    magic <- sin(radLat)
    magic <- 1 - ee * magic * magic
    sqrtMagic <- sqrt(magic)
    dLat <- (dLat * 180)/((a * (1 - ee))/(magic * sqrtMagic) * 
        pi)
    dLon <- (dLon * 180)/(a/sqrtMagic * cos(radLat) * pi)
    gcjLat <- wgsLat + dLat
    gcjLon <- wgsLon + dLon
    return(c(gcjLat, gcjLon))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
