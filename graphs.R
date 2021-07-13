
source('prices.R')
## reading frota.csv

frota <- read_csv(paste0(data_dir,'Frota.csv'))
# Avg. number of main distributors ####

## Preparing data ####

# Flags by mun only
mun_flags <- data %>% select(dist_id, state, cod_mun, mun, month) %>%
  mutate(dist=case_when(dist_id==40 ~ 'Cosan',
                        dist_id==192 ~ 'Shell',
                        dist_id==172 ~ 'Raizen',
                        TRUE ~ 'Other'),
         dummy=1) %>%
  select(-dist_id) %>%
  pivot_wider(names_from = dist,
              values_from = dummy,
              names_sep = '_',
              values_fill = 0,
              values_fn = min) %>%
  mutate(Both=Cosan*Shell, Just_one=(Cosan+Shell-2*Both),
         None=1-(Both+Just_one)) %>%
  ungroup() %>%
  group_by(state, mun, cod_mun) %>%
  summarise(Both=max(Both), Just_one=max(Just_one), None=max(None)) %>%
  mutate(group = case_when(Both==1 ~ 'Both',
                           Just_one==1 ~'Just_one',
                           None==1 ~ 'None'))

g <- data %>% select(state, mun, cod_mun, month, dist_id) %>%
  mutate(dist=case_when(dist_id==40 ~  'Cosan', #
                         dist_id==192 ~ 'Shell', #
                         dist_id==172 ~ 'Raizen', #
                         dist_id==7 ~   'Alesat', #
                         dist_id==137 ~ 'BR', #
                         dist_id==98 ~  'Ipiranga', #
                         TRUE ~         'Other'),
          main=case_when(dist=='Other' ~  'other', #
                         TRUE ~         'main'),
         ) %>%
  group_by(state, mun, cod_mun, month, main) %>%
  summarise(n=length(unique(dist_id))) %>%
  left_join(mun_flags, by=c('state','mun','cod_mun')) #%>%
  # mutate(group = case_when(Both==1 ~ 'Both',
  #                          Just_one==1 ~'Just_one',
  #                          None==1 ~ 'None'))

# Check we're good.
# g %>% ungroup %>%
#   mutate(group=Both+Just_one+None) %>%
#   summarise(max=max(group))

## Plotting ####

### Over time, by groups Both, None, Just - Main distributors ####

g %>% filter(main=='main') %>%
  ungroup() %>% group_by(month, group) %>%
  summarise(n=mean(n)) %>%
  ggplot(aes(x=month, y=n, col=group)) +
  geom_line() +
  ylab("") +
  theme_classic() +
  ggtitle("Avg. number of main distributors")

g %>% filter(main=='other') %>%
  ungroup() %>% group_by(month, group) %>%
  summarise(n=mean(n)) %>%
  ggplot(aes(x=month, y=n, col=group)) +
  geom_line() +
  ylab("") +
  theme_classic() +
  ggtitle("Avg. number of other distributors")

## Avg. car fleet ####

h <- frota %>%   left_join(mun_flags, by=c('state','mun')) %>%
  mutate(month=ym(month))

h %>% filter(is.na(group)==FALSE) %>%
  ungroup() %>%  group_by(month, group) %>%
  summarise(car_fleet=mean(total)) %>%
  ggplot(aes(x=month, y=car_fleet, col=group)) +
  geom_line() +
  ylab("") +
  theme_classic() +
  ggtitle("Avg. car fleet")
