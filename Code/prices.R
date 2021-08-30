# Set up ----
## Load libraries #####
# library(readr)
# library(lubridate)
# library(tidyverse)

## setting up directories ----


# data_dir="/Volumes/SSD Hans/Dropbox/Dropbox hmarti33/Dropbox/Summer 2021/"

## reading data ----
prices <- read_csv(paste0(data_dir,"PriceData.csv"),
                   col_types = "DDccccccdcccddccddccDDdcc")

## wrangling the data ----
prices$month <- ym(prices$month) # Getting months in right format

w_p <- prices %>%
  filter(month >= "2010-01-01", month<= "2017-12-01") # Filtering for period

# Creating simplified distributor id
w_p <- w_p %>% mutate(id_x=case_when(dist_id==40 ~  'Cosan', #
                              dist_id==192 ~ 'Shell', #
                              dist_id==172 ~ 'Raizen', #
                              dist_id==7 ~   'Alesat', #
                              dist_id==137 ~ 'BR', #
                              dist_id==98 ~  'Ipiranga', #
                              # is.na(dist_id)==TRUE ~ '',
                              TRUE ~         'Other'),
                      # simplified brand to merge
                      brand_x= case_when(str_detect(brand,"COSAN")==TRUE ~ 'Cosan',
                                         str_detect(brand, "SHELL")==TRUE ~ 'Shell',
                                         str_detect(brand, "ZEN")==TRUE ~ 'Raizen',
                                         str_detect(brand, "ALESAT")==TRUE ~ 'Alesat',
                                         str_detect(brand, "PETROBRAS")==TRUE ~ 'BR',
                                         str_detect(brand, "IPIR")==TRUE ~ 'Ipiranga',
                                         TRUE ~ 'Other'))

# w_p <-w_p %>%
#   mutate(mun_id= paste0(str_remove_all(state,'\\s+'),
#                         str_remove_all(mun,'\\s+')))
# Testing for name inconsistencies
# stion_brand[str_detect(stion_brand, "COSAN")]
# stion_brand[str_detect(stion_brand, "SHELL")] # No Shell ???
# stion_brand[str_detect(stion_brand, "ZEN")]
# stion_brand[str_detect(stion_brand, "ALESAT")]
# stion_brand[str_detect(stion_brand, "PETROBRAS")]
# stion_brand[str_detect(stion_brand, "IPIR")]




#Checking station brand and distributor id. They are different
# w_p %>% summarise(check=sum(brand_x==id_x, na.rm = TRUE),
#                   n=n(),
#                   perc=check/n)
#
# w_p %>% filter(brand_x!=id_x) %>% select(brand, brand_x, dist_id, id_x)
#
# table(w_p$brand, w_p$id_x)

# Collapsing by price:  ----
# mean and median by product type and whether retail
# or wholesale

w_p <- w_p %>% group_by(month, state, mun, brand_x) %>%
  summarise(mean_p_retail_gas=mean(p_sell_gas, na.rm = TRUE),
            median_p_retail_gas=median(p_sell_gas, na.rm = TRUE),
            mean_p_retail_eth=mean(p_sell_eth, na.rm = TRUE),
            median_p_retail_eth=median(p_sell_eth, na.rm = TRUE),
            mean_p_wholesale_gas=mean(p_buy_gas, na.rm = TRUE),
            median_p_wholesale_gas=median(p_buy_gas, na.rm = TRUE),
            mean_p_wholesale_eth=mean(p_buy_eth, na.rm = TRUE),
            median_p_wholesale_eth=median(p_buy_eth, na.rm = TRUE))

# Preparing for merging ----

# Municipalities with same name in different states
# Need to use mun+state
# data <- read_csv(paste0(data_dir,"SellDistMun.csv"))
# transforming strings to dates
# data$month<- ym(data$month)
q_d <- data %>% mutate(dist=case_when(dist_id==40 ~  'Cosan', #
                                      dist_id==192 ~ 'Shell', #
                                      dist_id==172 ~ 'Raizen', #
                                      dist_id==7 ~   'Alesat', #
                                      dist_id==137 ~ 'BR', #
                                      dist_id==98 ~  'Ipiranga', #
                                      TRUE ~         'Other'))

# flags <- data %>% select(dist_id, state, cod_mun, mun, month) %>%
#   mutate(dist=case_when(dist_id==40 ~  'Cosan', #
#                         dist_id==192 ~ 'Shell', #
#                         dist_id==172 ~ 'Raizen', #
#                         dist_id==7 ~   'Alesat', #
#                         dist_id==137 ~ 'BR', #
#                         dist_id==98 ~  'Ipiranga', #
#                         TRUE ~         'Other'),
#          dummy=1) %>%
#   select(-dist_id) %>%
#   pivot_wider(names_from = dist,
#               values_from = dummy,
#               names_sep = '_',
#               values_fill = 0,
#               values_fn = min) %>%
#   mutate(Both=Cosan*Shell, Just_one=(Cosan+Shell-2*Both),
#          None=1-(Both+Just_one)) %>%
#   mutate(two_before = if_else(month %within% two_before, 1, 0),
#          six_before = if_else(month %within% six_before, 1, 0),
#          twelve_before = if_else(month %within% twelve_before, 1, 0)
#   )

# q_d %>% group_by(month, state, cod_mun, mun, dist, prod) %>%
#   summarise(tot_v=sum(vol),
#             other_n=sum(dummy)) %>% arrange(month, cod_mun,desc(other_n))

q_d <- q_d %>% group_by(state, mun, cod_mun, month, dist, prod) %>%
  summarise(tot_v=sum(vol)) %>%
  ungroup()

# Useless flags, better merge with mun_flags
# q_d <- q_d %>% mutate(Cosan=if_else(dist=='Cosan',1,0),
#          Shell=if_else(dist=='Shell',1,0),
#          Both=Cosan*Shell,
#          Just_one=(Cosan+Shell-2*Both),
#          None=1-(Both+Just_one)) %>%
#   mutate(two_before = if_else(month %within% two_before, 1, 0),
#          six_before = if_else(month %within% six_before, 1, 0),
#          twelve_before = if_else(month %within% twelve_before, 1, 0)
#   ) %>%
#   mutate(pre_merge = case_when(two_before==1 ~ 'two_before',
#                                six_before==1 ~ 'six_before',
#                                twelve_before==1 ~ 'twelve_before',
#                                TRUE ~ 'other'),
#          flags = case_when(Both==1 ~ 'Both',
#                            Just_one==1 ~'Just_one',
#                            None==1 ~ 'None'))

#before widening, remove unnecessary vars, save with other name,
# left join with vars of interests
q_d <- q_d %>%
  pivot_wider(names_from = prod,
              values_from = tot_v,
              values_fill = 0,
              values_fn = sum)

#Getting HHI from quantities data

hhi <- q_d %>% mutate(total=sum(ethanolH,gasC,diesel)) %>%
  ungroup() %>%
  group_by(state, mun, cod_mun, month) %>%
  summarise(tot_eth=sum(ethanolH),
            tot_gas=sum(gasC),
            tot_diesel=sum(diesel),
            tot=sum(total),
            hhi_eth=sum(((ethanolH/tot_eth)*100)^2),
            hhi_gas=sum(((gasC/tot_gas)*100)^2),
            hhi_diesel=sum(((diesel/tot_diesel)*100)^2),
            hhi=sum(((total/tot)*100)^2))


## Merging quantities and prices ----

# ab <- left_join(q_d, w_p, by = c('state','mun','dist'='brand_x','month'))





