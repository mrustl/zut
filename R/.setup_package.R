pkg <- list(
  name = "zut",
  title = "R Package for Checking the Availability of Starting Places for Zugspritz Trail for Mittenwald Track",
  desc = paste(
    "R Package for Checking the Availability of Starting Places for Zugspritz Trail for Mittenwald Track."
  )
)

kwb.pkgbuild::use_pkg_skeleton("zut")

kwb.pkgbuild::use_pkg(
  pkg = pkg,
  copyright_holder = list(name = "Michael Rustler", start_year = NULL),
  user = "mrustl"
)

kwb.pkgbuild::use_ghactions()

kwb.pkgbuild::create_empty_branch_ghpages("zut", org = "mrustl")

usethis::use_pipe()
usethis::use_vignette("zut25_mittenwald-trail")
desc::desc_normalize()