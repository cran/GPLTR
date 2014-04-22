\name{data_pltr}
\alias{data_pltr}
\docType{data}
\title{
gpltr data example
}
\description{
A data frame to test the functions of the package
}
\usage{data(data_pltr)}
\format{
  A data frame with 3000 observations on the following 16 variables.
  \describe{
    \item{\code{G1}}{a numeric vector}
    \item{\code{G2}}{a factor with levels \code{0} \code{1}}
    \item{\code{G3}}{a factor with levels \code{0} \code{1}}
    \item{\code{G4}}{a factor with levels \code{0} \code{1}}
    \item{\code{G5}}{a factor with levels \code{0} \code{1}}
    \item{\code{G6}}{a binary numeric vector}
    \item{\code{G7}}{a binary numeric vector}
    \item{\code{G8}}{a binary numeric vector}
    \item{\code{G9}}{a binary numeric vector}
    \item{\code{G10}}{a binary numeric vector}
    \item{\code{G11}}{a binary numeric vector}
    \item{\code{G12}}{a binary numeric vector}
    \item{\code{G13}}{a binary numeric vector}
    \item{\code{G14}}{a binary numeric vector}
    \item{\code{G15}}{a binary numeric vector}
    \item{\code{Y}}{a binary numeric vector}
  }
}
\details{
The numeric variable \code{G1} is considered as offset in the simulated \code{PLTR} model; the variables \code{G2},...,\code{G5} are used to simulate the tree part, while \code{G6},...,\code{G15} are noise variables.
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(data_pltr)
## maybe str(data_pltr) ...
}
\keyword{datasets}