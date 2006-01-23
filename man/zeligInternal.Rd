\name{zeligDescribeModel}
\alias{zeligDescribeModel}
\alias{zmodel2string}
\alias{zeligInstalledModels}
\alias{zeligListModels}
\alias{zeligModelDependency}
\alias{zeligModelDependencies}
\title{ Zelig interface functions}
\description{
	Zelig interface functions. Used by VDC DSB to  communicate with Zelig.
}
\usage{
	zeligDescribeModel(modelName,force=F,schemaVersion="1.1")
	zmodel2string(modelDescription) 
	zeligInstalledModels(inZeligOnly=T,schemaVersion="1.1")
	zeligListModels(inZeligOnly=T) 
	zeligModelDependencies(inZeligOnly=T,schemaVersion="1.1",uninstalledOnly=T, repos=NULL) 
	zeligModelDependency(modelName,repos) 
}

\arguments{
  \item{modelName}{Name of model as returned by zeligInstalledModels or zeligListModels.}
  \item{modelDescription}{A model description as returned by zeligDescribeModel}
  \item{inZeligOnly}{Flag, include only models in official Zelig distribution}
  \item{uninstalledOnly}{Flag, include only uninstalled models}
  \item{repos}{URL of default repository to use}
  \item{schemaVersion}{version of Zelig schema}
  \item{force}{generate a description even if no custom description supplied}
}

\value{
Use zeligInstalledModels and zeligListModels to determine what models are available in zelig
for a particular schema level. Use zmodel2string(zeligDescribeModel()) to generate an XML
instance describing a model. Use zeligModelDependencies to generate a list of package
dependencies for models.
}

\examples{
	# show all available models
	zeligListModels(inZeligOnly=FALSE)	
	# show installed models
	zeligInstalledModels()
	# show dependency for normal.bayes
	zeligModelDependency("normal.bayes","http://cran.r-project.org/")
	# description of logit
	cat(zmodel2string(zeligDescribeModel("ologit")))

\dontshow{

# test model lists
zd= zeligInstalledModels(schemaVersion="1.1")
if (length(zd)<8 || sum(zd=="ls")!=1   || length(zeligListModels())<25 ) {
	stop("Failed zeligListModels/zeligInstalledModels self test")
}

if (zeligModelDependency("poisson.bayes","")[1]!="MCMCpack") {
	stop("Failed zeligModelDependency self test")
}

if (grep("explanatory",  zmodel2string(zeligDescribeModel("ologit")))!=1) {
	stop("Failed zmodel2string/zeligDescribeModel self test")
}

}

}

\author{
Micah Altman
\email{thedata-users\@lists.sourceforge.net}
\url{http://thedata.org}
}


\seealso{ \link[pkg:Zelig]{zelig}, \link{printZeligSchemaInstance}}

\keyword{IO}
\keyword{print}
