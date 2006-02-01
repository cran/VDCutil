\name{VDCxtabs}
\alias{VDCxtabs}
\title{VDC cross tabs, with pretty printing}
\description{
	This calculate cross tabs with marginals and proportions. 
}
\usage{
VDCxtabs(formula,data=parent.environment(), ...) 
}

\arguments{
  \item{data}{ data to be analyzed }
  \item{formula}{ crosstab formula suitable for \code{xtabs} }
  \item{\dots}{arguments to pass to \code{xtabs}}
}

\value{
	Returns a crosstabulation, essentially a combination of \code{xtabs} with proportions
and marginals.
}
\examples{
	VDCxtabs(Freq~.,as.data.frame(Titanic))

}
\author{
Micah Altman
\email{vdc-dev\@latte.harvard.edu}
\url{http://thedata.org}
}


\seealso{
\code{\link{xtabs}}, 
\code{\link{VDCcrossTabulation}}
}

\keyword{IO}
\keyword{print}
