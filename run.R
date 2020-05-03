#!/usr/bin/Rscript

# source('/home/andy/covid19-sask/scrape.R')

Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

rmarkdown::render('/home/andy/covid19-sask/dashboard/dash.Rmd', output_dir='/home/andy/covid19-sask/dashboard')

file.copy('/home/andy/covid19-sask/dashboard/dash.html', '/srv/shiny-server/covid19/index.html', overwrite=TRUE)
