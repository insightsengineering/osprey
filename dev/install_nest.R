install_nest <- function(pkgnames, ref = "devel") {
  lapply(pkgnames, function(x) {
        devtools::install_github(
            repo = paste0("NEST/", x),
            ref = ref,
            host = "https://github.roche.com/api/v3",
            upgrade = "never",
            dependencies = F,
            repos = NULL
        )
      })
}

install_nest("rtables", ref = "devel")
install_nest("test.nest", ref = "devel")
install_nest("utils.nest", ref = "devel")
install_nest("random.cdisc.data", ref = "devel")
install_nest("tern", ref = "devel")
install_nest("teal", ref = "devel")
install_nest("teal.devel", ref = "devel")
install_nest("teal.modules.clinical", ref = "devel")
install_nest("teal.modules.general", ref = "devel")
install_nest("tlgdown", ref = "devel")
