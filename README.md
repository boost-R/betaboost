# betaboost

`betaboost` implements a wrapper for boosting beta regression for potentially 
high-dimensional data. The `betaboost` packages uses the same parametrization as `betareg`
to make results directly comparable. The underlying boosting algorithms are implemented via the 
R ad-on packages `mboost` and `gamboostLSS`.

## Installation

Latest version from GitHub:
  ```
  library("devtools")
  install_github("boost-R/betaboost")
  library("betaboost")
  ```
  
To be able to use the `install_github()` command, one needs to install `devtools` first:
  ```
  install.packages("devtools")
  ```


