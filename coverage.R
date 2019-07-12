#!/usr/bin/env Rscript

# Check package coverage with covr.
# Updated 2019-07-12.

options(
    error = quote(quit(status = 1L)),
    warning = quote(quit(status = 1L))
)

cov <- covr::package_coverage()
pct <- covr::percent_coverage(cov)

if (pct < 100L) {
    print(cov)
    stop(paste0("Coverage is ", pct, "%."))
}
