
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(here)

# POTW data -----------------------------------------------------------------------------------

##
# code from DS

#read in data;
#file has two rows for identifying info, so read in header rows first, then combine
header1 <- read.csv(here('data-raw/HDR_as_CSV.csv'), nrows = 1,header = F)
header2 <- read.csv(here('data-raw/HDR_as_CSV.csv'), skip = 1, nrows = 1, header = F)

#read just data
loads <- read.csv(here('data-raw/HDR_as_CSV.csv'), skip=2,header=F)

#clean up second row with units
header3 <- c()
for (x in 1:length(header2)) {
  header3[x]<-ifelse(header2[x] == "Flow mgd", "Flow (mg/d)",
                    ifelse(header2[x] == "Ammonia kg N/d", "NH4 (kg/d)",
                           ifelse(header2[x] == "NOX kg N/d", "NOX (kg/d)",
                                  ifelse(header2[x] == "TIN kg N/d", "DIN (kg/d)",
                                         ifelse(header2[x] == "TP kg P/d", "TP (kg/d)", "XXX"
                                         )))))
}
#combine header1 and header3, and add to loads as column-names
names(loads) <- paste0(header1, "_", header3)

#convert dataframe to long format
loads2 <- pivot_longer(loads,cols = 4:173, names_to = "POTW", values_to = "mag", values_drop_na = FALSE)
#make separate columns for POTW name and parameter
loads3 <- loads2 %>%
  separate_wider_delim(POTW,delim = "_", names = c("POTW2", "param"))
#rename columns
colnames(loads3) <- c("month", "year", "date", "POTW", "param", "mag")

#convert date field from character to represented as date
loads3 <- mutate(loads3, date = mdy(date))

#read file with POTWs' subembayment designations
subs <- read.csv(here('data-raw/POTW_subembayment.csv'), header = TRUE)

#merge loads3 with subembayment designations
loads4 <- full_join(loads3, subs)

# save output
loads <- loads4
save(loads, file = here('data/loads.RData'))

# POTW locations ------------------------------------------------------------------------------

load(file = here('data/loads.RData'))

# get potw names from loads to match with locs
nms <- loads |>
  select(POTW) |>
  distinct()

# import, manually fix or add
locs <- read.csv(here('data-raw/POTW_locations.csv'), header = TRUE) |>
  select(
    POTW = "POTW.Name",
    lat = "Latitude",
    lon = "Longitude"
  ) |>
  mutate(
    POTW = case_when(
      POTW == 'DDSD' ~ 'Delta Diablo',
      POTW == 'FSSD' ~ 'Fairfield-Suisun',
      POTW == 'Mountain View' ~ 'Mt View',
      POTW == 'Pinole/Hercules' ~ 'Pinole',
      POTW == 'SFO' ~ 'SFO Airport',
      POTW == 'SFPUC' ~ 'SFPUC Southeast Plant',
      POTW == 'SJSC' ~ 'San Jose',
      POTW == 'SSF-SB' ~ 'South SF',
      POTW == 'W. County/Richmond' ~ 'West County',
      T ~ POTW
    )
  ) %>%
  left_join(nms, ., by = 'POTW') %>%
  mutate(
    lat = case_when(
      POTW == 'Port Costa' ~ 38.06073,
      POTW == 'Paradise Cove' ~ 37.90081,
      POTW == 'Tiburon' ~ 37.86815,
      POTW == 'SMCSD' ~ 37.84176,
      POTW == 'SVCW' ~ 37.54503,
      T ~ lat
    ),
    lon = case_when(
      POTW == 'Port Costa' ~ -122.22524,
      POTW == 'Paradise Cove' ~ -122.48236,
      POTW == 'Tiburon' ~ -122.45139,
      POTW == 'SMCSD' ~ -122.46750,
      POTW == 'SVCW' ~ -122.23016,
      T ~ lon
    )
  )


save(locs, file = here('data/locs.RData'))
