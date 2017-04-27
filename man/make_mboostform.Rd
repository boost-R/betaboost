\name{make_mboostform}
\alias{make_mboostform}
\alias{add_bolsform}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Building mboost formulas
}
\description{
Transforms 'classic' formula objects (\code{y ~ x1 + s(x2)}) to \code{mboost} formulas \code{y ~ bols(x1) + bbs(x2)}. 
}
\usage{
make_mboostform(formula, data = NULL)
add_bolsform(formula, data = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
formula object describing a model. 
}
  \item{data}{
data set, only necessary in case of "~ ." formulas
}
}
\value{
formula
}

\examples{
make_mboostform(y ~ x1 + s(x2))

}