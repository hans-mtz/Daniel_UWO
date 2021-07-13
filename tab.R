
source('graphs.R')
# prices before and after the merge ####

price_tab <- w_p %>% ungroup() %>%
  mutate(before_merge=if_else(month<merge, 'Before','After')) %>%
  left_join(mun_flags, by=c('state','mun')) %>%
  group_by(before_merge, group) %>%
  summarise(gas_retail_p=mean(mean_p_retail_gas, na.rm=TRUE),
            eth_retail_p=mean(mean_p_retail_eth, na.rm=TRUE),
            gas_ws_p=mean(mean_p_wholesale_gas, na.rm=TRUE),
            eth_ws_p=mean(mean_p_wholesale_eth, na.rm=TRUE))

# HHI ####

hhi_tab <- hhi %>% ungroup() %>%
  mutate(before_merge=if_else(month<merge, 'Before','After')) %>%
  left_join(mun_flags, by=c('state','mun', 'cod_mun')) %>%
  group_by(before_merge, group) %>%
  summarise(hhi=mean(hhi, na.rm = TRUE))

# Quantities ####

quants_tab <- q_d %>% ungroup() %>%
  mutate(before_merge=if_else(month<merge, 'Before','After')) %>%
  left_join(mun_flags, by=c('state','mun', 'cod_mun')) %>%
  group_by(before_merge, group, prod) %>%
  summarise(Quantities=mean(tot_v, na.rm = TRUE)) %>%
  pivot_wider(values_from = Quantities,
              names_from = prod) %>%
  mutate(total_fuel=diesel+ethanolH+gasC)

# Number of main/other distributors ####

n_dist_tab <- g %>% ungroup() %>%
  mutate(before_merge=if_else(month<merge, 'Before','After')) %>%
  group_by(before_merge, group, main) %>%
  summarise(n=mean(n)) %>%
  pivot_wider(values_from = n,
              names_from = main)

# Car fleet ####

fleet_tab <- h %>% ungroup() %>% filter(is.na(group)==FALSE) %>%
  mutate(before_merge=if_else(month<merge, 'Before','After')) %>%
  group_by(before_merge, group) %>%
  summarise(across(c(total, AUTOMOVEL, MOTOCICLETA, MOTONETA), mean)) %>%
  rename(Total_fleet=total)

# Joining the tables

table <- price_tab %>% left_join(hhi_tab) %>%
  left_join(quants_tab) %>%
  left_join(n_dist_tab) %>%
  left_join(fleet_tab, by=c('before_merge','group' )) %>%
  pivot_longer(-c(before_merge,group),
               names_to = 'Variables') %>%
  pivot_wider(names_from = c(before_merge),
              values_from = value)

