# This file sets specific environment variables and options to make sure devtools::check,
# which runs in a separate R-session, has the right madrat configuration.

# compare_proxy method for magpie objects. Needs to be explicitly assigned to the GlobalEnv for
# some reason... not required if exported by the package, but that isn't desired for now.
assign("compare_proxy.magpie", compare_proxy.magpie, envir = .GlobalEnv)

# Madrat config
madrat_mainfolder <- "/home/johannes/madrat_GDP_update" # nolint

if (dir.exists(madrat_mainfolder)) {
  oldCfg <- madrat::getConfig()
  madrat::setConfig(mainfolder = madrat_mainfolder, cachefolder = "test_mrdrivers", .verbose = FALSE)
  withr::defer(options(madrat_cfg = oldCfg), teardown_env())
}
