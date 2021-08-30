# Diff in Diff ----

## Outcome vars----

### Volumes----
# Monthly data
vol_d <- data %>% pivot_wider(-starts_with("dist"),
                     names_from = prod,
                     values_from = vol,
                     values_fn = sum,
                     names_prefix = "Vol_") %>%
  mutate(Vol_tot=Vol_gasC+Vol_diesel+Vol_ethanolH)

vol_d <- vol_d %>% mutate(month=ym(month))

### Number of distributors
# Monthly data
dist_d <- data %>% select(state, mun, cod_mun, month, dist_id) %>%
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
  pivot_wider(names_from = main,
              values_from = n,
              names_prefix = "Dist_No_",
              values_fill = 0)
rm(data)
### Prices ----
#monthly
w_p <- w_p %>% ungroup() %>% group_by(month, state, mun) %>%
  summarise(mean_p_retail_gas=mean(p_sell_gas, na.rm = TRUE),
            mean_p_retail_eth=mean(p_sell_eth, na.rm = TRUE),
            mean_p_wholesale_gas=mean(p_buy_gas, na.rm = TRUE),
            mean_p_wholesale_eth=mean(p_buy_eth, na.rm = TRUE))

## Cleaning

rm(prices,p_d)
rm(h)
### Number of stations ----
#monthly data
nst_d <- nst %>% mutate(gr=case_when(brand=="other"~"Other",
                             brand=="unbranded"~"Unbranded",
                             TRUE ~ "Branded"),
                            month=ym(paste0(year,'-',month))) %>%
  pivot_wider(-c(brand,year), names_from = gr,
              values_from = numstations,
              values_fill = 0,
              values_fn = sum,
              names_prefix = "No_st_") %>%
  mutate(No_st_total=No_st_Branded+No_st_Unbranded+No_st_Other,
         Ind_st_sh=No_st_Unbranded/No_st_total)

nst_d <- nst_d %>% rename(mun=county)
rm(nst)

### Fleet ----
#monthly data
fleet_d <- frota %>% mutate(month=ym(month)) %>%
  select(state, mun, month, total, AUTOMOVEL, MOTONETA, MOTOCICLETA) %>%
  rename(Fleet_tot=total)
rm(frota)
## Covariates ----
### Population ----
#annual data
pop_d <- pop %>% pivot_longer(!c(Sigla, Codigo, Município),
                     names_to = "year",
                     values_to = "population") %>%
  mutate(Município=stringi::stri_trans_general(Município,id="Latin-ASCII")) %>%
  rename(state=Sigla, cod_mun=Codigo, mun=Município) %>%
  mutate(cod_mun=as.numeric(cod_mun),
         year=as.numeric(year),
         mun=str_to_upper(mun))

rm(pop, pop05_19,pop10)

### GDP pc ----
gdp_d <- gdp %>%
  pivot_longer(!mun_state, names_to = "year", values_to = "gdp") %>%
  mutate(year=as.numeric(year)) %>%
  mutate(mun_state=stringi::stri_trans_general(str_to_upper(mun_state),
                                               id="Latin-ASCII"))

# Getting states
str_extract_all(gdp_d$mun_state,"(?<=\\()[:ALPHA:]{2}", simplify = T)
# Getting Mun
str_extract_all(gdp_d$mun_state,".+(?=\\()", simplify = T)

rm(gdp)

### HHI ----
 rm(q_d, vol_d)

# Merging ----
df <- hhi %>% left_join(w_p) %>%
  left_join(dist_d) %>%
  left_join(nst_d) %>%
  left_join(fleet_d) %>%
  mutate(year=year(month)) %>%
  left_join(pop_d) %>%
  mutate(mun_state=paste0(mun," (",state,")")) %>%
  left_join(gdp_d)

rm(data,hhi,dist_d,nst_d,fleet_d,pop_d,gdp_d,w_p)

## adding flags ----
df <- df %>%
  mutate(merge=if_else(month<merge, 0,1),
         gdp_pc=gdp/population) %>%
  left_join(mun_flags) %>%
  rename(Vol_eth=tot_eth, Vol_gas=tot_gas, Vol_dies=tot_diesel,
              retail_p_gas=mean_p_retail_gas, ws_p_gas=mean_p_wholesale_gas,
              retail_p_eth=mean_p_retail_eth, ws_p_eth=mean_p_wholesale_eth,
              Vol_tot=tot)

df <- df %>% ungroup %>% mutate(qt=lubridate::quarter(month, with_year = TRUE),
                                Both=if_else(group=="Both",1,0),
                                Just_one=if_else(group=="Just_one",1,0),
                                None=if_else(group=="None",1,0))
## Saving and cleaning ####
save(df, file = "DID.Rdata")
rm(list = grep("df",ls(),value = T, invert = T))

# DID ----

outc <- c("retail_p_gas","retail_p_eth","ws_p_gas","ws_p_eth",
          "log(retail_p_gas)","log(retail_p_eth)","log(ws_p_gas)","log(ws_p_eth)",
          "No_st_total","Dist_No_main","Dist_No_other",
          "Ind_st_sh","Vol_tot","Vol_gas","Vol_eth","Vol_dies",
          "log(Vol_tot)","log(Vol_gas)","log(Vol_eth)","log(Vol_dies)",
          "log(No_st_total)","log(No_st_Unbranded)")
covars <- c("i(group,keep='Both')*merge","Fleet_tot","population","gdp_pc","hhi")
covars1 <- c("i(group, keep='Just_one')*merge","Fleet_tot","population","gdp_pc","hhi")

did_b <- list()
did_bn <- list()
did_jn <- list()
k=1
for (i in 1:length(outc)) {
  for (j in 1:length(covars)){
    did_b[[k]] <- feols(xpd(lhs=outc[i], rhs=covars[1:j]),df)
    did_bn[[k]] <- feols(xpd(lhs=outc[i], rhs=covars[1:j]),
                         df %>% filter(group!="Just_one"))
    did_jn[[k]] <- feols(xpd(lhs=outc[i], rhs=covars1[1:j]),
                         df %>% filter(group!="Both"))
    k=k+1
  }
}

save(did_b,did_bn,did_jn, file = here::here("Rmd","DID.Rdata"))
rm(did_b,did_bn,did_jn)

## Viewing results ----

## TWFE ----
x1 <- c("i(group, merge, keep='Both')","Fleet_tot","population","gdp_pc",
        "hhi","i(month, Both, '2011-02-01')")
x2 <- c("i(group, merge, keep='Just_one')","Fleet_tot","population","gdp_pc",
        "hhi","i(month, Just_one, '2011-02-01')")

did_twfe_b <- list()
did_twfe_bn <- list()
did_twfe_jn <- list()
k=1
for (i in 17:length(outc)) {
  for (j in 1:length(x1)){
    did_twfe_b[[k]] <- feols(xpd(y~..x| mun+month,..x=x1[1:j], lhs=outc[i]),
                             df)
    did_twfe_bn[[k]] <- feols(xpd(y~..x| mun+month,..x=x1[1:j], lhs=outc[i]),
                         df %>% filter(group!="Just_one"))
    did_twfe_jn[[k]] <- feols(xpd(y~..x| mun+month,..x=x2[1:j], lhs=outc[i]),
                         df %>% filter(group!="Both"))
    k=k+1
  }
}

save(did_twfe_b,did_twfe_bn,did_twfe_jn,
     file = here::here("Rmd","DID_twfe.Rdata"))
rm(did_twfe_b,did_twfe_bn,did_twfe_jn)

## TWFE plots ----

x1 <- c("i(month, Both, '2011-02-01')","Fleet_tot","population","gdp_pc","hhi")
x2 <- c("i(month, Just_one, '2011-02-01')","Fleet_tot","population","gdp_pc","hhi")

did_twfe_q_b <- list()
did_twfe_q_bn <- list()
did_twfe_q_jn <- list()
k=1
for (i in 1:length(outc)) {
    did_twfe_q_b[[k]] <- feols(xpd(y~..x| mun+month,..x=x1, lhs=outc[i]),
                             df %>% filter(year<=2012)) #Restricting data 2010-2012
    did_twfe_q_bn[[k]] <- feols(xpd(y~..x| mun+month,..x=x1, lhs=outc[i]),
                              df %>% filter(group!="Just_one", year<=2012))
    did_twfe_q_jn[[k]] <- feols(xpd(y~..x| mun+month,..x=x2, lhs=outc[i]),
                              df %>% filter(group!="Both", year<=2012))
    k=k+1
}

save(did_twfe_q_b,did_twfe_q_bn,did_twfe_q_jn,
     file = here::here("Rmd","DID_twfe_q.Rdata"))
rm(did_twfe_q_b,did_twfe_q_bn,did_twfe_q_jn)

## Placebo test ----

# Fake merge data
df <- df %>% mutate(placebo=if_else(month<"2010-11-01",0,1))

x1 <- c("i(group, placebo, keep='Both')","Fleet_tot","population","gdp_pc","hhi")
x2 <- c("i(group, placebo, keep='Just_one')","Fleet_tot","population","gdp_pc","hhi")

did_pbo_b <- list()
did_pbo_bn <- list()
did_pbo_jn <- list()
k=1
for (i in 1:length(outc)) {
  for (j in 1:length(x1)){
    did_pbo_b[[k]] <- feols(xpd(y~..x| mun+month,..x=x1[1:j], lhs=outc[i]),
                               df %>% filter(month<= "2011-02-01")) #restrict until date of "2011-02-01"
    did_pbo_bn[[k]] <- feols(xpd(y~..x| mun+month,..x=x1[1:j], lhs=outc[i]),
                                df %>% filter(group!="Just_one", month<="2011-02-01"))
    did_pbo_jn[[k]] <- feols(xpd(y~..x| mun+month,..x=x2[1:j], lhs=outc[i]),
                                df %>% filter(group!="Both", month<="2011-02-01"))
    k=k+1
  }
}

save(did_pbo_b,did_pbo_bn,did_pbo_jn,
     file = here::here("Rmd","DID_pbo.Rdata"))
rm(did_pbo_b,did_pbo_bn,did_pbo_jn)
