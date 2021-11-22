oldCfg <- getConfig()
setConfig(mainfolder =  "/home/johannes/madrat_folder", # nolint
          cachefolder = "test_mrdrivers",
          .verbose = FALSE)

withr::defer(options(madrat_cfg = oldCfg), teardown_env())
