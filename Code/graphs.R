
# Plotting ####

## Over time, by groups Both, None, Just - Main distributors ####

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
