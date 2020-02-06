# Author: Mike Ackerman
# Purpose: Examine juvenile Chinook salmon emergence and emigration timing as it relates to the hydrograph in the Lemhi River
# Created: 11/12/2019
# Last Modified:
#
# Notes on Chinook salmon emergence timing from Quinn (2005): 
# The relationship between spawning date and emergence stems from the rough equivalence between time and temperature in controlling
# embryonic development, and a combined unit known as 'degree days' or 'temperature units' (TUs) reflects this fact. The developmental
# rate of embryos can be estimated from the product of the number of degrees Celcius above 0 times the number of days. For example, coho
# salmon hatch about 500 TUs after fertilization, and this could be 50 days at 10C or 100 days at 5C.
#
# Chinook salmon TUs appear to be within the range 404-536 (avg = 500, med = 517) days to hatch based on table 8-1 in Quinn (2005).
# Saved time to hatch and emergence information for Chinook salmon from Quinn (2005) in chinook_tus_quinn2005.csv
rm(list = ls())

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(sf)
library(sp)
library(rgeos)
library(maptools)
library(scales)
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

#-----------------------------------------------------------------
# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd('C:/Users/mikea/Dropbox/Projects/BOR/MRA2/analysis/')
getwd()

#-----------------------------------------------------------------
# read in discharge data

# daily discharge from USGS 13305310 LEMHI RIVER BELOW L5 DIVERSION NEAR SALMON ID
l5_discharge = read_delim('data/daily_discharge_13305310.txt', delim = '\t',skip = 30, col_names = F) %>%
  select(X1:X4) %>%
  rename(agency = X1,
         site = X2,
         date = X3,
         cfs = X4) %>%
  filter(date != '2016-02-29') %>%       # remove 2/29 data
  mutate(year = as.factor(year(date)),
         yday = yday(date)) %>%
  mutate(yday = case_when(year == '2016' & yday >= 60 ~ yday - 1,
                          TRUE ~ yday)) # dealing with 366 days in 2016 data

# daily discharge from USGS 13305000 LEMHI RIVER NR LEMHI ID
abv_hayd_discharge = read_delim('data/daily_discharge_13305000.txt', delim = '\t',skip = 34, col_names = F) %>%
  select(X1:X4) %>%
  rename(agency = X1,
         site = X2,
         date = X3,
         cfs = X4) %>%
  filter(date != '2016-02-29') %>%
  mutate(year = as.factor(year(date)),
         yday = yday(date)) %>%
  mutate(yday = case_when(year == '2016' & yday >= 60 ~ yday - 1,
                          TRUE ~ yday)) # dealing with 366 days in 2016 data

# daily data from the L3A0 rotary screw trap 
rst_data = read_csv('data/Summarized Chinook L3A0 Counts With Covariates.csv')

#-----------------------------------------------------------------
# plotting discharges

# first, calculate average daily discharge at L5 site
l5_avg = l5_discharge %>%
  select(yday, cfs) %>%
  group_by(yday) %>%
  summarize(cfs = mean(cfs)) %>%
  ungroup()

# plot L5 discharge by year
l5_p = l5_discharge %>%
  select(cfs, year, yday) %>%
  ggplot() +
  geom_line(aes(x = yday, 
                y = cfs,
                color = year)) +
  theme_classic() +
  labs(x = 'Day of Year',
       y = 'Discharge (cfs)',
       color = 'Year')
l5_p

# plot average daily discharge by year
l5_avg_p = l5_avg %>%
  ggplot() +
  geom_line(aes(x = yday,
                y = cfs)) +
  theme_classic() +
  labs(x = 'Day of Year',
       y = 'Discharge (cfs)',
       color = 'Year')
l5_avg_p

# consider plotting the above two together at some point

#-----------------------------------------------------------------
# temperature data from Kris McNyset

# read in temperature data
lemhi_tmp_2011 = st_read('data/lemhi_temp_shapefiles/Lemhi_2011/Lem_2011_8D_mn.shp') %>%
  mutate_at(vars(starts_with('Tmn')), list(~na_if(., '-9999')))

lemhi_tmp_2012 = st_read('data/lemhi_temp_shapefiles/Lemhi_2012/Lem_2012_8D_Mn.shp') %>%
  mutate_at(vars(starts_with('Tmn')), list(~na_if(., '-9999')))

lemhi_tmp_2013 = st_read('data/lemhi_temp_shapefiles/Lemhi_2013/Lem_2013_8D_Mn.shp') %>%
  mutate_at(vars(starts_with('Tmn')), list(~na_if(., '-9999'))) %>%
  rename(RCAID = LEM_RCAID)

# plot a year of temp data
lem_2012_p = lemhi_tmp_2012 %>%
  ggplot() +
  geom_sf(aes(color = RCAID)) +
  theme_classic()
lem_2012_p

# read in redd data
redd_df = read_csv(file = 'data/ReddData_With_RID_MEAS.csv', 
                   # col_types = cols(
                   #   Year = col_factor(),
                   #   SubBasin = col_character(),
                   #   Stream = col_character(),
                   #   LowBound = col_character(),
                   #   UpBound = col_character(),
                   #   Descriptio = col_character(),
                   #   Datum = col_character(),
                   #   Section = col_character(),
                   #   Pass = col_character(),
                   #   Observers = col_character(),
                   #   SubSection = col_character(),
                   #   RKM = col_double()
                   # )
                   ) %>%
  filter(Species == 'Chinook' & Basin == 'Lemhi') %>%
  select(index, MEAS, Distance, Year, Species, Run, Basin, Stream,
         Latitude, Longitude, NEAR_DIST, NEAR_X, NEAR_Y) %>%
  mutate(Year = as.factor(Year)) %>%
  st_as_sf(coords = c('NEAR_X', 'NEAR_Y'), crs = 4326) %>%               # convert to sf object
  st_transform(st_crs(lemhi_tmp_2012))                         # set crs to same as lemhi_tmp_2012

# plot redds
redd_p = redd_df %>%
  ggplot() +
  geom_sf(aes(color = Year, fill = Year)) +
  theme_classic() +
  scale_x_continuous(labels = number_format(accuracy = 0.01)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01))
redd_p

# plot temp and redds together
tmp_redd_p = ggplot() +
  geom_sf(data = lemhi_tmp_2012) +
  geom_sf(data = redd_df, aes(color = Year, fill = Year)) +
  theme_classic()
tmp_redd_p

# Now let's snap redds to 2011 temperature data
lemhi_tmp_2011_sp = as_Spatial(lemhi_tmp_2011)
redd_df_sp = as_Spatial(redd_df)
redd_pts_11 = snapPointsToLines(points = redd_df_sp,
                                lines = lemhi_tmp_2011_sp,
                                idField = 'RCAID') %>%
  st_as_sf() %>%
  rename(RCAID = nearest_line_id)
unique(redd_pts_11$RCAID)

# And to 2012 temperature data
lemhi_tmp_2012_sp = as_Spatial(lemhi_tmp_2012)
redd_df_sp = as_Spatial(redd_df)
redd_pts_12 = snapPointsToLines(points = redd_df_sp,
                                lines = lemhi_tmp_2012_sp,
                                idField = 'RCAID') %>%
  st_as_sf() %>%
  rename(RCAID = nearest_line_id)
unique(redd_pts_12$RCAID)

# And to 2013 temperature data
lemhi_tmp_2013_sp = as_Spatial(lemhi_tmp_2013)
redd_df_sp = as_Spatial(redd_df)
redd_pts_13 = snapPointsToLines(points = redd_df_sp,
                                lines = lemhi_tmp_2013_sp,
                                idField = 'RCAID') %>%
  st_as_sf() %>%
  rename(RCAID = nearest_line_id)
unique(redd_pts_13$RCAID)

# filter each of the temperature datasets to only include reaches containing redds
rcaid11_exc = c(63,64,10,82,83,799,18,86,230,2,231,62, 17, 85,
                239,190,237,236,386,187,238) # list of reaches to exclude (mostly Hayden)
tmp_11_redds_sf = lemhi_tmp_2011 %>%
  filter(RCAID %in% unique(redd_pts_11$RCAID)) %>%
  filter(!RCAID %in% rcaid11_exc) %>%
  select(RCAID, starts_with('Tmn'))
  
tmp_11_p = tmp_11_redds_sf %>%
  ggplot() +
  geom_sf(aes(color = as.factor(RCAID), fill = as.factor(RCAID))) +
  geom_sf_label(aes(label = RCAID)) +
  theme_classic()
tmp_11_p

# same for 2012
rcaid12_exc = c(190,236,10,9,62,83,18,85,94,84,2,3,17,187,238) # list of reaches to exclude (mostly Hayden)
tmp_12_redds_sf = lemhi_tmp_2012 %>%
  filter(RCAID %in% unique(redd_pts_12$RCAID)) %>%
  filter(!RCAID %in% rcaid12_exc) %>%
  select(RCAID, starts_with('Tmn'))

tmp_12_p = tmp_12_redds_sf %>%
  ggplot() +
  geom_sf(aes(color = as.factor(RCAID), fill = as.factor(RCAID))) +
  geom_sf_label(aes(label = RCAID)) +
  theme_classic()
tmp_12_p

# same for 2013
rcaid13_exc = c(231,2,986,840,433,987,799,797,82,10,386,510,
                83,17,18,86,440,85,94,230) # list of reaches to exclude (mostly Hayden)
tmp_13_redds_sf = lemhi_tmp_2013 %>%
  filter(RCAID %in% unique(redd_pts_13$RCAID)) %>%
  filter(!RCAID %in% rcaid13_exc) %>%
  select(RCAID, starts_with('X13'))

tmp_13_p = tmp_13_redds_sf %>%
  ggplot() +
  geom_sf(aes(color = as.factor(RCAID), fill = as.factor(RCAID))) +
  geom_sf_label(aes(label = RCAID)) +
  theme_classic()
tmp_13_p

# okay, now we have 8-day mean temperature data for those reaches on the mainstem upper Lemhi containing
# redds. now let's average temperatures across reaches for each year.
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

tmp_11_sum = tmp_11_redds_sf %>%
  as.data.frame() %>%
  select(-geometry, -RCAID) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column('datespan') %>%
  mutate(mn2011 = rowMeans(select(., as.character(1:17)), na.rm = T)) %>%
  select(datespan, mn2011) %>%
  mutate(datespan = substrRight(datespan, 3))

tmp_12_sum = tmp_12_redds_sf %>%
  as.data.frame() %>%
  select(-geometry, -RCAID) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column('datespan') %>%
  mutate(mn2012 = rowMeans(select(., as.character(1:18)), na.rm = T)) %>%
  select(datespan, mn2012) %>%
  mutate(datespan = substrRight(datespan, 3))

tmp_13_sum = tmp_13_redds_sf %>%
  as.data.frame() %>%
  select(-geometry, -RCAID) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column('datespan') %>%
  mutate(mn2013 = rowMeans(select(., as.character(1:49)), na.rm = T)) %>%
  select(datespan, mn2013) %>%
  mutate(datespan = substrRight(datespan, 3))

# merge and average across years
tmp_sum = tmp_11_sum %>%
  left_join(tmp_12_sum) %>%
  left_join(tmp_13_sum)
tmp_sum[1,3] <- NA
select_vars = c('mn2011','mn2012','mn2013')
tmp_sum = tmp_sum %>%
  mutate(mn_tmp = rowMeans(select(., select_vars), na.rm = T)) %>%
  select(datespan, mn_tmp) %>%
  mutate(datespan = as.numeric(datespan)) %>%
  rename(yday = datespan)

yday_tmp = data.frame(yday = 1:365) %>%
  left_join(tmp_sum) %>%
  fill(mn_tmp, .direction = 'down')

yday_tmp_p = yday_tmp %>%
  ggplot() +
  geom_line(aes(x = yday,
                y = mn_tmp)) +
  theme_classic() +
  labs(x = 'Day of Year',
       y = 'Temperature (C)')
yday_tmp_p

#-----------------------------------------------------------------
# crude estimation of Chinook hatch and emigration times in the Lemhi River

# temperature unit (TU) information describing hatching and emergence timing for Chinook salmon from Quinn (2005)
chinook_tu = read_csv('data/chinook_tus_quinn2005.csv')

# for now, let's assume median spawn date in Lemhi is 9/1
d = as.Date('2019-09-01')
spawn_day = yday(d)
spawn_day
target_tus = median(chinook_tu$tus)

end_yr = yday_tmp[spawn_day:365,]
beg_yr = yday_tmp[1:spawn_day-1,]
spawn_tmp = rbind(end_yr,beg_yr) %>%
  mutate(cum_tu = cumsum(mn_tmp),
         month = month(as.POSIXct(as.Date(yday, origin = '2019-01-01'))))

hatch_days = min(which(spawn_tmp$cum_tu >= target_tus)) # days to hatching from spawn_day
hatch_julian = spawn_tmp$yday[hatch_days]                 # estimated julian day of hatching

# find date of hatch
hatch_date = as.Date('2019-01-01')
day(hatch_date) = hatch_julian
hatch_date = format(hatch_date, format = '%m-%d')
hatch_date

# so, in above case, we estimate the day of hatching for juvenile Chinook salmon in the Lemhi River to be
# February 6. Now, let's estimate the day of emergence.

# look at average temps by month
mnth_tmps = spawn_tmp %>%
  group_by(month) %>%
  summarize(mn_mnth_tmp = mean(mn_tmp))
mnth_tmps

# We can see that the average monthly temp during the spring (Feb, March, Apr, May) is about 3.3C, which is
# somewhere between the 2C and 5C lines in chinook_tu. If we split the difference in days from hatch to
# emergence for those rows, we might estimate the number of days hatch-to-emergence to be about 101.5
hatch_to_emerge_days = 101
emerge_julian = hatch_julian + hatch_to_emerge_days
emerge_date = as.Date('2019-01-01')
day(emerge_date) = emerge_julian
emerge_date = format(emerge_date, format = '%m_%d')
emerge_date # May 18

# finally, I want to look at peak presmolt (and maybe smolt?) emigration times
head(rst_data)
range(rst_data$SurveyDateTime)
rst = rst_data %>%
  select(SurveyDateTime, Species, BroodYear, NewFish, DailyEfficiency, DailyNEst) %>%
  mutate(SurveyDateTime = as.POSIXct(SurveyDateTime, format = '%m/%d/%Y')) %>%
  mutate(month = month(SurveyDateTime)) %>%
  filter(month >= 9) %>%
  mutate(yday = yday(SurveyDateTime)) %>%
  group_by(yday) %>%
  summarize(mnNewFish = mean(NewFish, na.rm = T))
  
rst_p = rst %>%
  ggplot() +
  geom_line(aes(x = yday,
                y = mnNewFish)) +
  theme_classic() +
  labs(x = 'Day of Year',
       y = 'Discharge')
rst_p

# Let's graph averge Lemhi hydrograph with the hatching and emergence times
# plot average daily discharge by year. In this case, maybe use the Hayden hydrograph
abv_hydn_avg = abv_hayd_discharge %>%
  select(yday, cfs) %>%
  group_by(yday) %>%
  summarize(cfs = mean(cfs)) %>%
  ungroup()

abv_hydn_avg_p = abv_hydn_avg %>%
  ggplot() +
  geom_line(aes(x = yday,
                y = cfs)) +
  theme_classic() +
  labs(x = 'Day of Year',
       y = 'Discharge (cfs)',
       color = 'Year')
abv_hydn_avg_p

day_span = 14
scalar = 70
p = abv_hydn_avg %>%
  ggplot(aes(x = yday)) +
  geom_line(aes(y = cfs), size = 1.25) +
  geom_line(aes(y = yday_tmp$mn_tmp*scalar), color = 'gray') +
  scale_y_continuous(sec.axis = sec_axis(~./scalar, name = expression('Temperature ' ( degree*C)))) +
  geom_vline(xintercept = emerge_julian, color = 'red') +
  annotate('rect', xmin = emerge_julian - day_span, ymin = -Inf,
           xmax = emerge_julian + day_span, ymax = +Inf,
           alpha = 0.2, fill = 'red') +
  geom_vline(xintercept = hatch_julian, color = 'steelblue') +
  annotate('rect', xmin = hatch_julian - day_span, ymin = -Inf,
           xmax = hatch_julian + day_span, ymax = +Inf,
           alpha = 0.2, fill = 'steelblue') +
  annotate('rect', xmin = 280, ymin = -Inf,
           xmax = 325, ymax = +Inf,
           alpha = 0.2, fill = 'green') +
  theme_classic() +
  labs(x = 'Day of Year',
       y = 'Discharge (cfs)') +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10))
p

