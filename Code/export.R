source('gas_br.R')

# Flags ####
# Cosan and Shell (merged into Raizen)
# Confirming : Raizen (172) appears March 2011.
#           Cosan (40) and Shell (192) drop February 2011
# Both, Just One, None for 2, 6 and 12 months before the merge
## Exporting as csv "macintosh" can be converted to "UTF-8" if needed.
write.csv(m2, file = "m2.csv", fileEncoding = "macintosh")
write.csv(m6, file = "m6.csv", fileEncoding = "macintosh")
write.csv(m12, file = "m12.csv", fileEncoding = "macintosh")

# Multimarket matrices ####

## Saving to csv files ----
mapply(function(x,y)
  write.csv(x,file=paste0(data_dir,y,"_count.csv"),
            fileEncoding = 'macintosh'),
  mmc_lc, sample)

# Tables ####
source('tab.R')
write.csv(table, file=paste0(data_dir,'table.csv'), fileEncoding = 'macintosh')

write.csv(table, file='table.csv', fileEncoding = 'macintosh')

# Final table ####
source('pop.R')
write.csv(ft, file=paste0(data_dir,'table.csv'), fileEncoding = 'macintosh')

# Final table ####
source('gdp.R')
write.csv(ft, file=paste0(data_dir,'table.csv'), fileEncoding = 'macintosh')
