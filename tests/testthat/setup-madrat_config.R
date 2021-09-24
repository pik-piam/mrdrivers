old_cfg <- getConfig()
setConfig(mainfolder =  "/home/johannes/madrat_folder",
          cachefolder = "test_mrdrivers",
          forcecache = TRUE,
          .verbose = FALSE)

withr::defer(options(madrat_cfg = old_cfg), teardown_env())
