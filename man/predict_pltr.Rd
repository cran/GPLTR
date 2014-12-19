\name{predict_pltr}
\alias{predict_pltr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
prediction
}
\description{
prediction on new features using a pltr tree and the name of the confounding variable
}
\usage{
predict_pltr(xtree, xdata, Y.name, X.names, newdata, type = "response",
              family = 'binomial', thresshold = seq(0.1, 0.9, by = 0.1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xtree}{a tree obtained with the pltr procedure
   }
   \item{xdata}{the dataframe used to learn the pltr model
   }
   \item{Y.name}{the name of the main variable
   }
   \item{X.names}{the names of the confounding variables
   }
   \item{newdata}{the new data with all the predictors and the dependent variable
   }
   \item{type}{the type of prediction
   }
   \item{family}{the glm family considered
   }
   \item{thresshold}{the thresshold value to consider for binary prediction. It could be a vector, helping to compute the AUC
   }   
}

\value{ A list of two element
\item{predict_glm}{the predicted vector, depending on the family used. For the binomial family with a vector of thresshold, a matrix with each column corresponding to a thresshold value}
\item{ERR_PRED}{either the prediction error of the pltr procedure on the test set or a vector of prediction error when the family is binomial with a vector of thresshold values}

}
\references{
Mbogning, C., Perdry, H., Toussile, W., Broet, P.: A novel tree-based procedure for deciphering the genomic spectrum of clinical disease entities. Journal of Clinical Bioinformatics 4:6, (2014)
}
\author{
Cyprien Mbogning
}

\seealso{
\code{\link{pltr.glm}}, \code{\link[stats]{predict.glm}}
}
\examples{
##
}

\keyword{ documentation}
