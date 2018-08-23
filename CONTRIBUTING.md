# Contributing to development

- For all changes, **fork the repository**, and then issue a pull request that will be reviewed.
- Do not commit changes directly to master branch.
- Please avoid creating unnecessary new branches. Use a fork instead.
- Pull requests must pass build checks on Travis CI.
- Support is only provided for the current release version.
- Open an issue for any proposed changes that affect the `bcbioRNASeq` S4 class.

## Package style syntax

Attempt to follow these style guides, by priority:
(1) [Bioconductor coding style](https://www.bioconductor.org/developers/how-to/coding-style/);
(2) [tidyverse style guide](http://style.tidyverse.org);
(3) [Google R style guide](https://google.github.io/styleguide/Rguide.xml).

Please adhere to these guidelines, in particular:

- Object and function parameter arguments should be formated in lowerCamelCase.
- Use spaces instead of tabs. Indent with 4 spaces.
- Use `<-` and not `=` for assignment.
- Explicitly define `if`/`then` statements using opening and closing brackets (`{`, `}`).
- `else` declarations should be on the same line as the closing bracket.
- Use spaces around assignment operators (`<-`), brackets (`{`), and comma delimiters (e.g. `c("foo", "bar")` not `c("foo","bar")`.

## Required checks

Run these commands prior to a pull request:

```r
devtools::document()
devtools::run_examples()
devtools::test()
devtools::check()
BiocCheck::BiocCheck(getwd())
lintr::lint_package()
covr::report()
```

### Rebuild website

```r
unlink("docs", recursive = TRUE)
pkgdown::build_site()
```
