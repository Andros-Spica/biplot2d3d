
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("biplot2d3d ready")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Andreas Angourakis",
    devtools.desc.author = '"Andreas Angourakis <andros.spica@gmail.com> [aut, cre]"',
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}

.onUnload <- function(libname, pkgname) {

}
