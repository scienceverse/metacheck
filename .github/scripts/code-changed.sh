#!/usr/bin/env bash
# Says whether a change can affect the package, for R-CMD-check.yaml and
# full-tests.yaml, which skip their checks when it cannot.
# Usage: code-changed.sh <base commit> <new commit>
# Prints "code=false" when every changed file is a website or text file
# (below), or DESCRIPTION with only its Version line changed; otherwise
# "code=true". With no base commit, or one this copy of the repository does
# not have (a new branch, a run started by hand), it prints "code=true".
#
# Files that cannot affect the package checks or the tests:
#   pkgdown/                         the website's settings, pages and images
#   .github/workflows/pkgdown.yaml   the website's build
#   NEWS.md, README.md, CONTRIBUTING.md, CODE_OF_CONDUCT.md, LICENSE.md
# NEWS.md is part of the package, but only its web addresses are checked, by
# the url-check job, which always runs.

base="$1"
head="${2:-HEAD}"

if [ -z "$base" ] || ! git cat-file -e "${base}^{commit}" 2>/dev/null; then
  echo "code=true"
  exit 0
fi

code=false
while IFS= read -r f; do
  case "$f" in
    pkgdown/*|.github/workflows/pkgdown.yaml) ;;
    NEWS.md|README.md|CONTRIBUTING.md|CODE_OF_CONDUCT.md|LICENSE.md) ;;
    DESCRIPTION)
      # changed lines other than Version:
      if git diff "$base" "$head" -- DESCRIPTION |
           grep -E '^[+-]' | grep -v -E '^(\+\+\+|---)' |
           grep -v -E '^[+-]Version:' | grep -q .; then
        code=true
      fi
      ;;
    *) code=true ;;
  esac
done < <(git diff --name-only "$base" "$head")

echo "code=$code"
