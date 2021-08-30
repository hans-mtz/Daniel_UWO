# Main ----

## Load libraries #####
library(readr)
library(lubridate)
library(tidyverse)
library(fixest)
library(here)
library(rmarkdown)

## Set up data directory ----

data_dir="/Volumes/SSD Hans/Dropbox/Dropbox hmarti33/Dropbox/Summer 2021/"

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
rmarkdown::render(here("Rmd","DiD.Rmd"))

## TWFE ----
rmarkdown::render(here("Rmd","DiD_twfe.Rmd"))

## TWFE plots ----
rmarkdown::render(here("Rmd","DiD_twfe_q.Rmd"))

## Placebo ----
rmarkdown::render(here("Rmd","DiD_pbo.Rmd"))
