
.onAttach <- function(libname, pkgname) { # nolint
  packageStartupMessage(
    "\nYou are using aoperatr version ",
    read.dcf(system.file("DESCRIPTION", package = "aoperatr"))[, "Version"]
  )
}