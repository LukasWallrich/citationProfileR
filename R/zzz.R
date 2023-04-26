# install these from github, not CRAN:
# Code from Jthorpe with some modifications https://stackoverflow.com/questions/33764036/include-github-packages-as-imports-in-description
# dyplr code https://github.com/tidyverse/dplyr/blob/bbcfe99e29fe737d456b0d7adc33d3c445a32d9d/R/zzz.r
.onLoad <- function(libname, pkgname) {
  pkglist <- list(
    c(name='bbr',url='metrumresearchgroup/bbr'))

  for(pkg in pkglist){
    if(!suppressWarnings(suppressPackageStartupMessages(require(pkg['name'],
                                                                quietly=TRUE,character.only=TRUE)))){
      devtools::install_github(pkg['url'])
      suppressPackageStartupMessages( library(pkg['name'],character.only=TRUE))
    }
  }
}
