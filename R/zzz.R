# install these from github, not CRAN:
# Code from Jthorpe with some modifications https://stackoverflow.com/questions/33764036/include-github-packages-as-imports-in-description
# dyplr code https://github.com/tidyverse/dplyr/blob/bbcfe99e29fe737d456b0d7adc33d3c445a32d9d/R/zzz.r

#copied function into the package rather than adding the whole package as a dependency (parse_pdf_refs)
#no longer needed
# .onLoad <- function(libname, pkgname) {
#   rlang::run_on_load()
#   pkglist <- list(
#     c(name='bbr',url='metrumresearchgroup/bbr'))
#
#   for(pkg in pkglist){
#     if(!suppressWarnings(suppressPackageStartupMessages(require(pkg['name'],
#                                                                 quietly=TRUE,character.only=TRUE)))){
#       devtools::install_github(pkg['url'])
#       suppressPackageStartupMessages( library(pkg['name'],character.only=TRUE))
#     }
#   }
# }
