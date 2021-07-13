# Set up ----
## Load libraries #####
source("tab.R")
library(readxl)
## Reading data ----
pop05_19 <- read_excel(paste0(data_dir,"Mun_Pop_2005to2019.xls"))
pop10 <- read_excel(paste0(data_dir,"Mun_Pop_2010.xls"))

#merging
pop <- pop05_19 %>% left_join(pop10)

# pop05_19$Codigo[1] %in% data$cod_mun

## Generating table

pop_tab <- pop %>% pivot_longer(!c(Sigla, Codigo, Município),
                     names_to = "year",
                     values_to = "population") %>%
  mutate(year=ym(paste0(year,"-12")),
         before_merge=if_else(year<merge, 'Before','After'),
         Codigo=as.numeric(Codigo)) %>%
  left_join(mun_flags, by=c('Codigo'='cod_mun')) %>%
  group_by(before_merge, group) %>%
  summarise(pop=mean(population, na.rm = TRUE)) %>%
  filter(is.na(group)==FALSE)

pop_tab <- pop_tab %>% pivot_wider(names_from = before_merge, values_from = pop) %>%
  mutate(Variables="population")

## Reading previous table

# tab <- read_csv(paste0(data_dir,"table.csv"))
# tab$X1=NULL
# tab

# ft <- tab %>% rbind(pop_tab) %>% arrange(Variables, group) %>%
#   mutate(Difference=After-Before, Perc_change=(Difference/Before)*100) %>%
#   select(group, Variables, Before, After, Difference, Perc_change)

# Use this if running everything from zero. Uncomment and use previous if working
# from population.

ft <- table %>% rbind(pop_tab) %>% arrange(Variables, group) %>%
  mutate(Difference=After-Before, Perc_change=(Difference/Before)*100) %>%
  select(group, Variables, Before, After, Difference, Perc_change)




