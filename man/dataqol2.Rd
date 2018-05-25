\name{dataqol2}
\alias{dataqol2}
\docType{data}
\title{
  Longitudinal quality of life data
}
\description{
  A data frame with 6 quality of life measures for 60 patients, originally published in the \code{QoLR} package.
}
\usage{data(dataqol2)}
\format{
  
  \describe{
    \item{\code{id}}{patient identification number}
    \item{\code{time}}{visit number for quality of life assessment }
    \item{\code{date}}{date of quality of life measure}
    \item{\code{QoL}}{score of global quality of life on a 0-100 scale in order that a high score reflects a high quality of life level}
    \item{\code{pain}}{score of pain on a 0-100 sale in order that a high score reflects a high level of pain}
    \item{\code{arm}}{treatment arm equal to 0 or 1}
    \item{\code{death}}{date of death. Missing if the patient is not died}
  }
}

