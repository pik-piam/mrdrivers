# This file sets specific environment variables and options to make sure devtools::check,
# which runs in a separate R-session, has the right configuration.

# Madrat config
oldCfg <- getConfig()
setConfig(mainfolder =  "/home/johannes/madrat_folder", # nolint
          cachefolder = "test_mrdrivers",
          .verbose = FALSE)

# compary_proxy method for magpie objects. Needs to be explicitly assigned to the GlobalEnv for
# some reason... not required if exported by the package, but that isn't desired for now.
assign("compare_proxy.magpie", compare_proxy.magpie, envir = .GlobalEnv)
withr::defer(options(madrat_cfg = oldCfg), teardown_env())
