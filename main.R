# Main ----

## Load libraries #####
# library(groundhog)
# gd="2021-08-01"
# pkgs=c('readr',
# 'lubridate',
# 'tidyverse',
# 'fixest',
# 'here',
# 'rmarkdown')
#
# groundhog.library(pkgs, gd, tolerate.R.version='4.0.3')

library(readr)
library(lubridate)
library(tidyverse)
library(fixest)
library(here)
library(rmarkdown)

## Set up data directory ----

data_dir="~/Dropbox/Summer 2021/"

# Flagging municipalities and Multimarket matrices ----

source(here::here("code","mmm.R"))

# Graphs ----

source(here::here("code","graphs.R"))

# Table ----

source(here::here("code","tab.R"))
source(here::here("code","pop.R"))
source(here::here("code","gdp.R"))

# DiD ----
# Analysis
source(here::here("code","DD.R"))

# Compile reports ----

## First report----
rmarkdown::render(here::here("Rmd","DiD.Rmd"),
                  output_dir = here::here("Output"))

## TWFE ----
rmarkdown::render(here::here("Rmd","DiD_twfe.Rmd"),
                  output_dir = here::here("Output"))

## TWFE plots ----
rmarkdown::render(here::here("Rmd","DiD_twfe_q.Rmd"),
                  output_dir = here::here("Output"))

## Placebo ----
rmarkdown::render(here::here("Rmd","DiD_pbo.Rmd"),
                  output_dir = here::here("Output"))
