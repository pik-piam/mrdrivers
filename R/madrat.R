.onLoad <- function(libname, pkgname){
  madrat::setConfig(packages = c(madrat::getConfig("packages"), pkgname), 
                   .cfgchecks = FALSE, 
                   .verbose = FALSE)
}

#create an own warning function which redirects calls to vcat (package internal)
warning <- function(...) madrat::vcat(0, ...)

# create a own stop function which redirects calls to stop (package internal)
stop <- if ("crayon" %in% rownames(installed.packages())) {
  function(...) madrat::vcat(-1, crayon::red(...))
} else {
  function(...) madrat::vcat(-1, ...)
}


# create an own cat function which redirects calls to cat (package internal)
cat <- function(...) madrat::vcat(1, ...)
