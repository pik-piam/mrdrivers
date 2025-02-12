.onAttach <- function(libname, pkgname) {
  madrat::madratAttach(c(pkgname, "GDPuc"))
}

.onDetach <- function(libpath) {
  madrat::madratDetach(c(libpath, "GDPuc"))
}
