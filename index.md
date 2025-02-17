[![R-CMD-check](https://github.com/mrustl/zut/workflows/R-CMD-check/badge.svg)](https://github.com/mrustl/zut/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/mrustl/zut/workflows/pkgdown/badge.svg)](https://github.com/mrustl/zut/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/mrustl/zut/branch/main/graphs/badge.svg)](https://codecov.io/github/mrustl/zut)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/zut)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/zut)](https://kwb-r.r-universe.dev/)

R Package for Checking the Availability of Starting Places
for Zugspritz Trail for Mittenwald Track.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'zut' from GitHub
remotes::install_github("mrustl/zut")
```
