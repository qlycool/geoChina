\name{gcj2bd}
\alias{gcj2bd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gcj2bd(gcjLat, gcjLon)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gcjLat}{
%%     ~~Describe \code{gcjLat} here~~
}
  \item{gcjLon}{
%%     ~~Describe \code{gcjLon} here~~
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
function (gcjLat, gcjLon) 
{
    z <- sqrt(gcjLon^2 + gcjLat^2) + 2e-05 * sin(gcjLat * pi * 
        3000/180)
    theta <- atan2(gcjLat, gcjLon) + 3e-06 * cos(gcjLon * pi * 
        3000/180)
    bdLon = z * cos(theta) + 0.0065
    bdLat = z * sin(theta) + 0.006
    return(c(bdLat, bdLon))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
