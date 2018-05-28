\name{dataqol2}
\alias{dataqol2}
\docType{data}
\title{
  Longitudinal quality of life data
}
\description{
  A data frame with 6 quality of life measures for 60 patients, originally published in the \code{QoLR} package: Analysis of Health-Related Quality of Life in oncology. For more details, see the \href{https://cran.r-project.org/src/contrib/Archive/QoLR}{CRAN archive}, the corresponding \href{https://github.com/cran/QoLR}{GitHub pare}, or the references below. 
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
\author{
  Amelie Anota aanota@chu-besancon.fr
}
\references{
Anota A. et al. Time to Health-related Quality of Life score deterioration as a modality of longitudinal analysis for health-related quality of life studies in oncology: do we need RECIST for quality of life to achieve standardization? Qual Life Res. 2015, 24(1):5-18.

Bonnetain F. et al. Time until definitive deterioration as a means of longitudinal analysis for treatment trials in patients with metastatic pancreatic adenocarcinoma. Eur J Cancer 2010, 46(5): 2753-2762.

Fayers PM. et al. The EORTC QLQC30 scoring manual. 3rd ed. Brussels: EORTC, 2001.

Hamidou Z. et al. Time to deterioration in quality of life score as a modality of longitudinal analysis in patients with breast cancer. The Oncologist 2011, 16(10):1458-1468.
}