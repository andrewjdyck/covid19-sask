#!/usr/bin/Rscript

Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

rmarkdown::render('/home/andy/covid19-sask/dash.Rmd', output_dir='/home/andy/covid19-sask')

file.copy('/home/andy/covid19-sask/dash.html', '/srv/shiny-server/covid19/index.html', overwrite=TRUE)
