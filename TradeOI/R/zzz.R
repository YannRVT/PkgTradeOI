# zzz.R
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "img",
    directoryPath = system.file(
      "www/img",
      package = "TradeOI"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("img")
}
