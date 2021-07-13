# Set up ----
## Load libraries #####
library(readr)
library(lubridate)
library(tidyverse)


data_dir="/Volumes/SSD Hans/Dropbox/Dropbox hmarti33/Dropbox/Summer 2021/"

## Reading data ----
data <- read_csv(paste0(data_dir,"SellDistMun.csv"))

data$date <- ym(data$month) # transforming strings to dates
data$month<- ym(data$month)
## Reviewing data ----
firms <- unique(data$dist_full_name) #All distributors
mun <- unique(data$cod_mun) #All municipalities

# Reviewing data, checking distributors and their id's. Just to be safe
# Cosan 40 and 172
unique(data[grepl("C(osan|OSAN)", data$dist_full_name), "dist_id"])
# Alesat 7
unique(data[grepl("ALESAT", data$dist_full_name),"dist_id"])
# Shell 192 and 172
unique(data[grepl("S(HELL|hell)", data$dist_full_name),"dist_id"])
# BR is Petrobras  137
unique(data[grepl("PETROBRAS", data$dist_full_name),"dist_id"])
# Ipiranga -> DNP - Dist Nac de Pet 65 and 98
unique(data[grepl("DNP", data$dist_full_name),"dist_id"])
# Raizen (Cosan, Shell) 172
data %>% filter(dist_id==172) %>%
  select(dist_full_name) %>%
  summarise(names=unique(dist_full_name))

#Confirming : Raizen (172) appears March 2011.
#           Cosan (40) and Shell (192) drop February 2011

data %>% group_by(cod_mun) %>%
  filter(dist_id==172) %>%
  summarise(Raizen_appears_mun=min(date)) %>%
  arrange(Raizen_appears_mun)

data %>% group_by(cod_mun) %>%
  filter(dist_id==40) %>%
  summarise(Cosan_drops_mun=max(date)) %>%
  arrange(desc(Cosan_drops_mun))

data %>% group_by(cod_mun) %>%
  filter(dist_id==192) %>%
  summarise(Shell_drops_mun=max(date)) %>%
  arrange(desc(Shell_drops_mun))

# Flaggin municipalities ----
## Both Shell and Cosan operating ----
## Defining time interval
merge <- ym("2011-03")
two_before <- interval(merge-months(2), merge-months(1))
six_before <- interval(merge-months(6), merge-months(1))
twelve_before <- interval(merge-months(12), merge-months(1))

## Clearing data to select only needed variables
flag <- data %>% select(dist_id, cod_mun, mun, date ) %>%
         mutate(dist=case_when(dist_id==40 ~ 'Cosan',
                               dist_id==192 ~ 'Shell',
                               dist_id==172 ~ 'Raizen',
                               TRUE ~ 'Other'),
                dummy=1) %>%
          mutate(two_before = if_else(date %within% two_before, 1, 0),
                   six_before = if_else(date %within% six_before, 1, 0),
                   twelve_before = if_else(date %within% twelve_before, 1, 0)
                 ) %>%
          select(-dist_id,-date) %>%
          pivot_wider(names_from = dist,
                      values_from = dummy,
                      names_sep = '_',
                      values_fill = 0,
                      values_fn = min) %>%
          mutate(Both=Cosan*Shell, Just_one=(Cosan+Shell-2*Both),
                  None=1-(Both+Just_one)
                  ) #%>%

# Two months before merge: Data to be exported as csv
m2 <- flag %>% arrange(cod_mun) %>%
  filter(two_before==1) %>%
  select(cod_mun, mun, Cosan, Shell, Other, Both, Just_one, None)
# Six months before merge: Data to be exported as csv
m6 <- flag %>% arrange(cod_mun) %>%
  filter(six_before==1, two_before==1) %>%
  select(cod_mun, mun, Cosan, Shell, Other, Both, Just_one, None)
# 12 months before merge: Data to be exported as csv
m12 <- flag %>% arrange(cod_mun) %>%
  filter(twelve_before==1, two_before==1) %>%
  select(cod_mun, mun, Cosan, Shell, Other, Both, Just_one, None)

# Exporting as csv "macintosh" can be converted to "UTF-8" if needed.
# write.csv(m2, file = "m2.csv", fileEncoding = "macintosh")
# write.csv(m6, file = "m6.csv", fileEncoding = "macintosh")
# write.csv(m12, file = "m12.csv", fileEncoding = "macintosh")

# Multimarket matrix ----

## Main dist ----
#Getting main distributors after merge/ before merge: change filter to "date<merge"
data %>% group_by(dist_id, dist_full_name) %>%
  filter(date>=merge) %>%
  summarise(total=sum(vol)) %>%
  arrange(desc(total))# %>%

## Getting matrices ----
#Transforming data to create dummies for distributor
mmc <- data %>% select(dist_id, cod_mun, mun, date) %>%
  mutate(dist=case_when(dist_id==40 ~  'Cosan', #
                        dist_id==192 ~ 'Shell', #
                        dist_id==172 ~ 'Raizen', #
                        dist_id==7 ~   'Alesat', #
                        dist_id==137 ~ 'BR', #
                        dist_id==98 ~  'Ipiranga', #
                        dist_id==216 ~ 'Total',
                        dist_id==34 ~  'Ciapetro',
                        TRUE ~         'Other'),
         b4_merge=if_else(date<merge, 1, 0),
         dummy=1) %>%
  select(mun, cod_mun, date, b4_merge, dist, dummy) %>%
  pivot_wider(names_from = dist,
              values_from = dummy,
              names_sep = '_',
              values_fill = 0,
              values_fn = min) %>%
  mutate(two_before = if_else(date %within% two_before, 1, 0),
         six_before = if_else(date %within% six_before, 1, 0),
         twelve_before = if_else(date %within% twelve_before, 1, 0)
  ) %>%
  arrange(cod_mun, date)

# Generating multimarket matrices percentages

main_d <- names(mmc[5:(length(mmc[1,])-3)]) # Names of the main distributors

sel_m <- cbind(TRUE, mmc$b4_merge==1, #Logical vectors to select
                  mmc$b4_merge==0,   #different samples
                  mmc$two_before==1,
                  mmc$six_before==1,
                  mmc$twelve_before==1)

sample <- c("Whole","Before Merge",
            "After Merge", "2m before",
            "6m before", "12m before") #Names of the different samples
mmc_l <- list() #Initializing list to save matrices
for(k in 1:ncol(sel_m)){
  A=matrix(NaN, ncol = 9, nrow = 9) #temporal matrix to save results
  for(i in 1:(length(main_d))){
    for(j in 1:(length(main_d))){
      sel <- sel_m[,k]
      d1 <- main_d[i]
      d2 <- main_d[j]
      A[i,j] <- mmc[sel,] %>% mutate(x1x2=get(d1)*get(d2)) %>%
                        group_by(date) %>%
                        summarise(no=sum(x1x2)) %>%
                        summarise(avg=mean(no)) %>%
                        pull #Average municipalities across months
      tmun <- mmc[sel,] %>% summarise(tmun=length(unique(cod_mun))) %>%
                            pull #Total number of municipalities
      A[i,j]=A[i,j]/tmun #Proportion of the total mun
    }
  }
  rownames(A)=main_d
  colnames(A)=main_d
  mmc_l[[sample[k]]]=A
}

mmc_lc <- list() #Initializing list to save matrices
for(k in 1:ncol(sel_m)){
  A=matrix(NaN, ncol = 9, nrow = 9) #temporal matrix to save results
  for(i in 1:(length(main_d))){
    for(j in 1:(length(main_d))){
      sel <- sel_m[,k]
      d1 <- main_d[i]
      d2 <- main_d[j]
      A[i,j] <- mmc[sel,] %>% mutate(x1x2=get(d1)*get(d2)) %>%
        group_by(date) %>%
        summarise(no=sum(x1x2)) %>%
        summarise(avg=mean(no)) %>%
        pull #Average municipalities across months
      # tmun <- mmc[sel,] %>% summarise(tmun=length(unique(cod_mun))) %>%
      #   pull #Total number of municipalities
      # A[i,j]=A[i,j]/tmun #Proportion of the total mun
    }
  }
  rownames(A)=main_d
  colnames(A)=main_d
  mmc_lc[[sample[k]]]=A
}

# # Saving to csv files ----
# mapply(function(x,y)write.csv(x,file=paste0(y,".csv")),mmc_l, sample)
#
# mapply(function(x,y)
#   write.csv(x,file=paste0(y,"_count.csv")),
#   mmc_lc, sample)
#
# mapply(function(x,y)
#   write.csv(x,file=paste0(results_dit,y,"_count.csv")),
#   mmc_lc, sample)

