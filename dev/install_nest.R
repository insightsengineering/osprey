install_nest <- function(pkgnames, ref = "master") {
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

install_nest("rtables", ref = "master")
install_nest("test.nest", ref = "master")
install_nest("utils.nest", ref = "master")
install_nest("random.cdisc.data", ref = "master")
install_nest("tern", ref = "master")
install_nest("teal", ref = "master")
install_nest("teal.devel", ref = "master")
install_nest("teal.modules.clinical", ref = "master")
install_nest("teal.modules.general", ref = "master")
install_nest("tlgdown", ref = "master")
