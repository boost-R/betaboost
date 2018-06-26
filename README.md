# betaboost

[![Build Status (Linux)](https://travis-ci.org/boost-R/betaboost.svg?branch=master)](https://travis-ci.org/boost-R/betaboost)
[![Build status](https://ci.appveyor.com/api/projects/status/gc0ech6kpjniqemg?svg=true)](https://ci.appveyor.com/project/hofnerb/betaboost)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/betaboost)](https://CRAN.R-project.org/package=betaboost)
[![Coverage Status](https://coveralls.io/repos/github/boost-R/betaboost/badge.svg?branch=master)](https://coveralls.io/github/boost-R/betaboost?branch=master)


`betaboost` implements a wrapper for boosting beta regression for potentially 
high-dimensional data. The `betaboost` packages uses the same parametrization as `betareg`
to make results directly comparable. The underlying boosting algorithms are implemented via the 
R add-on packages `mboost` and `gamboostLSS`.

## Installation

Current release from CRAN:
  ```
  install.packages("betaboost")
  library("betaboost")
  ```

Latest development version from GitHub:
  ```
  library("devtools")
  install_github("boost-R/betaboost")
  library("betaboost")
  ```
  
To be able to use the `install_github()` command, one needs to install `devtools` first:
  ```
  install.packages("devtools")
  ```


