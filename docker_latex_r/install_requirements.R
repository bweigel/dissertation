require(devtools)
MIRROR = "http://cran.us.r-project.org"
packages = list(
  c('knitr'),
  c('ggplot2'),
  c('magrittr'),
  c('plyr'),
  c('dplyr'),
  c('gridExtra'),
  c('rgbif')
)

for (item in packages) {
  if (length(item) == 1) {
    install_version(item[1], repos = MIRROR)
  } else{
    install_version(item[1], version = item[2],  repos = MIRROR)
  }
}
