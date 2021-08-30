
# Reading data ----

gdp <- read_excel(paste0(data_dir,"gdp_county_2005to2017.xlsx"),
                  skip = 3, na = "...")

gdp <- gdp %>% rename(mun_state=...1)

## Wrangling data ----

# str_extract_all(gdp$mun_state,"(\\w+\\s?)+(?=\\()")
# str_extract_all(gdp$mun_state,"(?<=\\()[:ALPHA:]{2}")

# Generate before/after flag

gdp <- gdp %>%
  pivot_longer(!mun_state, names_to = "year", values_to = "gdp") %>%
  mutate(year= ym(paste0(year,"-12")),
         before_merge=if_else(year<merge, 'Before','After'))

# merge with population to get gdp per capita by  mun and state
gdp <- pop %>% pivot_longer(!c(Sigla, Codigo, Município),
               names_to = "year",
               values_to = "population") %>%
  mutate(id=paste0(Município," (",Sigla,")"),
         year=ym(paste0(year,"-12")),
         Codigo=as.numeric(Codigo))%>%
  select(Sigla, Município, id, year, population) %>%
  left_join(gdp, by=c("id"="mun_state", "year"))

# Generating table ----
# generating ID to merge with gdp data
mun_flags <- mun_flags %>%  mutate(id=paste0(mun," (",state,")"))
#merging mun_flags
gdp_pc_table <- gdp %>% mutate(gdppc=gdp/population) %>%
  filter(is.na(gdppc)==F) %>%
  mutate(id=stringi::stri_trans_general(str_to_upper(id),id="Latin-ASCII")) %>%
  left_join(mun_flags, by="id") %>%
  group_by(before_merge, group) %>%
  summarise(gdp_pc=mean(gdppc, na.rm = TRUE)) %>%
  filter(is.na(group)==FALSE)

# gdp_pc_table

# Merging with previous table ----
## Reading previous table

# tab <- read_csv(paste0(data_dir,"table.csv"),col_types = c("-ccdddd"))
#
# tab

## Appending to previous table ----
ft <- gdp_pc_table %>%
  pivot_wider(names_from = before_merge, values_from = gdp_pc) %>%
  mutate(Variables="gdp_pc",
         Difference=After-Before,
         Perc_change=(Difference/Before)*100) %>%
  rbind(table)

# Number of stations ----

#Reading data
nst <- read_csv(paste0(data_dir,"numberstation_county_2009to2017.csv"))

brand <- unique(nst$brand)
# length(brand) # Six unique identifiers
# brand

# generating num station total table
nst_tot_tab <- nst %>% mutate(gr=case_when(brand=="other"~"Other",
                            brand=="unbranded"~"Unbranded",
                            TRUE ~ "Branded"),
               dt=ym(paste0(year,'-',month)),
               before_merge=if_else(dt<merge, 'Before','After')) %>%
  left_join(mun_flags, by = c("state","county"="mun")) %>%
  group_by(before_merge, group) %>%
  summarise(num_stations=mean(numstations, na.rm = TRUE)) %>%
  filter(is.na(group)==FALSE) %>%
  pivot_wider(names_from = before_merge, values_from = num_stations) %>%
  mutate(Variables="num_stations (total)",
         Difference=After-Before,
         Perc_change=(Difference/Before)*100)

nst_tab <- nst %>% mutate(gr=case_when(brand=="other"~"Other",
                            brand=="unbranded"~"Unbranded",
                            TRUE ~ "Branded"),
               dt=ym(paste0(year,'-',month)),
               before_merge=if_else(dt<merge, 'Before','After')) %>%
  left_join(mun_flags, by = c("state","county"="mun")) %>%
  group_by(before_merge, group, gr) %>%
  summarise(num_stations=mean(numstations, na.rm = TRUE)) %>%
  filter(is.na(group)==FALSE) %>%
  pivot_wider(names_from = before_merge,
              values_from = num_stations) %>%
  mutate(Variables=paste0("num_stations ",gr),
         Difference=After-Before,
         Perc_change=(Difference/Before)*100) %>%
  select(!gr)

# Merging with final table
ft <- nst_tot_tab %>% rbind(nst_tab, ft)

ft <- ft %>% select(group, Variables, Before, After, Difference, Perc_change) %>%
  arrange(Variables, group)

# Exporting ----
write.csv(ft, file=here::here('Output','table.csv'), fileEncoding = 'macintosh')

