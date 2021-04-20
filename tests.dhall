let conf = ./spago.dhall

in      conf
    //  { dependencies =
              conf.dependencies
            # [ "aff", "console", "effect", "spec", "node-fs", "node-buffer" ]
        , sources = conf.sources # [ "test/**/*.purs" ]
        }
