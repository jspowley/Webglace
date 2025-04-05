
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{Webglace}`

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of `{Webglace}` like so:

``` r
devtools::install_github("jspowley/Webglace")
```

## Run

Webglace is run in orchestration a noVNC instance and Selenium instance. To run with the supporting applications, navigate to the ini folder and run:

``` bash
docker-compose -p webglace up -d
```

This starts a local network in docker running the application with its dependencies.

To end a session, run:

``` bash
docker-compose -p webglace down
```

For live testing, branch development, and running shiny from RStudio, follow the above procedure from the init/compose_live_testing folder. The docker-compose file at this directory only runs the application dependencies. From here you are free to run the Webglace separately from R in terminal or RStudio.

You can launch the application by running:

``` r
Webglace::run_app()
```

Or when developing branches, by using the following when working in an RProjects workflow:

```r
golem::run_dev()
```
