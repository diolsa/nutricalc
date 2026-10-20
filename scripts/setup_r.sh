#!/usr/bin/env bash
set -euo pipefail

sudo apt-get update
sudo apt-get install -y r-base

Rscript -e 'install.packages(c("stringr", "nnls", "shiny", "bslib", "Ternary", "xml2"), repos = "https://cloud.r-project.org")'
