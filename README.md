<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bcgov/PEMcollectr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bcgov/PEMcollectr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# PEMcollectr
A shiny app for uploading PEM field data.

To install and run in deployment:

```
devtools::install_github('bcgov/PEMcollectr')
PEMcollectr::run_PEM_app(local = FALSE)
```

For local development the shiny app is located in inst/PEMcollectR. 
To run in a development environment while making changes. Either 
build the package (ctrl+shift+b in Rstudio) and run the following:

```
PEMcollectr::run_PEM_app(local = TRUE)
```

or run `devtools::load` (ctrl+shift+l in Rstudio), open the 
inst/PEMcollectR/ui.R or inst/PEMcollectR/ui.R and click Run App.
