#!/usr/bin/env bash
set -Eeuxo pipefail

# Package checks
#
# See also:
# - R CMD build --help
# - R CMD check --help
# - Travis CI recipe
#   https://github.com/travis-ci/travis-build/blob/master/lib/travis/build/script/r.rb

export TZ="America/New_York"

echo "Session information"
Rscript -e 'utils::sessionInfo()'
Rscript -e 'sessioninfo::session_info()'

echo "R CMD check"
R CMD build . --no-build-vignettes --no-manual
R CMD check *.tar.gz --ignore-vignettes --no-manual --timings

# Note that running `R CMD BiocCheck` directly requires `script/BiocCheck` to
# be installed at `/usr/lib64/R/bin`. Otherwise, can run directly using Rscript.
echo "BiocCheck"
Rscript -e 'BiocCheck::BiocCheck(package = ".", `no-check-R-ver` = TRUE, `no-check-bioc-help` = TRUE, `no-check-remotes` = TRUE, `no-check-version-num` = TRUE, `no-check-vignettes` = TRUE, `quit-with-status` = TRUE)'

echo "lintr"
Rscript -e 'if (packageVersion("base") >= "3.6") lintr::lint_package(path = ".")'
