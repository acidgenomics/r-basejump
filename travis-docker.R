rcmdcheck::rcmdcheck(args = "--no-manual")
BiocCheck::BiocCheck(`quit-with-status` = FALSE)
lintr::lint_package()
covr::report()
