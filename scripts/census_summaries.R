# This file produces Census summaries at the needed geographies for 1981 and 2001

# The measures of interest are:

# - population density
# - proportion foreign born
# - proportion in managerial/professional work
# - proportion with degrees or equivalents

library(nomisr)
library(tidyverse)
library(haven)
library(here)

#####
# First load shapefiles

# Shapefiles available from UKDS: https://borders.ukdataservice.ac.uk/easy_download.html
# You will need 1981 counties for England and Wales, 1981 districts for Scotland and 1981 and 2001 wards for all three countries
# Download and save in appropriate folder as below for code to function

library(sf)

#1981 counties

eng_counties_1981 <- read_sf(here("data/shapefiles/1981_counties/england/england_ct_1981.shp"))

wal_counties_1981 <- read_sf(here("data/shapefiles/1981_counties/wales/wales_ct_1981.shp"))

sco_counties_1981 <- read_sf(here("data/shapefiles/1981_counties/scotland/scotland_dt_1981.shp"))

ggplot() +
  geom_sf(data = wal_counties_1981) +
  geom_sf(data = eng_counties_1981) +
  geom_sf(data = sco_counties_1981)

gb_counties_1981 <- bind_rows(eng_counties_1981,wal_counties_1981,sco_counties_1981) 

gb_counties_1981 <- gb_counties_1981 %>%
  group_by(name) %>%
  summarize(geometry = st_union(geometry))

ggplot() + geom_sf(data = gb_counties_1981)

#####
# CALCULATE AREA AND DISTANCE

gb_counties_distance <- st_distance(gb_counties_1981,gb_counties_1981)

rownames(gb_counties_distance) <- gb_counties_1981$name
colnames(gb_counties_distance) <- gb_counties_1981$name

gb_counties_geog <- bind_cols(as_tibble(gb_counties_distance, rownames = "name"), area = st_area(gb_counties_1981)) %>%
  relocate(area, .after = name)

summary(gb_counties_geog)

gb_counties_geog$`Inner London`

write_rds(gb_counties_geog, file = here("data/shapefiles/gb_counties_geog.rds"))

#####
## CREATE LOOKUPS

# 1981

eng_wards_1981 <- read_sf(here("data/shapefiles/1981_wards/england/england_wa_1981.shp"))

wal_wards_1981 <- read_sf(here("data/shapefiles/1981_wards/wales/wales_wa_1981.shp"))

ew_wards_1981 <- bind_rows(eng_wards_1981,wal_wards_1981) 

ew_wards_1981 %>% is.na() %>% summary()

# 2001

eng_wards_2001 <- read_sf(here("data/shapefiles/2001_wards/england/england_caswa_2001.shp"))

eng_wards_2001 <- eng_wards_2001 %>%
  select(name,label=ons_label,geometry)

eng_wards_2001_ua <- read_sf(here("data/shapefiles/2001_wards/england/england_ua_caswa_2001.shp"))

wal_wards_2001 <- read_sf(here("data/shapefiles/2001_wards/wales/wales_caswa_2001.shp"))

sco_wards_2001 <- read_sf(here("data/shapefiles/2001_wards/scotland/scotland_caswa_2001.shp"))

gb_wards_2001 <- bind_rows(eng_wards_2001,eng_wards_2001_ua,wal_wards_2001,sco_wards_2001) 

eng_wards_2001 %>% filter(name=="Bede")

gb_wards_2001 %>% is.na() %>% summary()

gb_wards_2001 <- gb_wards_2001 %>% select(label,name,geometry)

# 1981 wards to counties

# Perform the intersection

ward1981_counties1981 <- st_intersection(ew_wards_1981,gb_counties_1981)

# Calculate the area of each intersection

ward1981_counties1981 <- ward1981_counties1981 %>%
  mutate(intersect_area = st_area(.))

# Create look-up

ward1981_counties1981 <- ward1981_counties1981 %>%
  st_drop_geometry() %>%
  dplyr::select(ward_name = name, ward_code = label, county_name = name.1, intersect_area) %>%
  distinct(ward_name,ward_code,county_name,.keep_all = T)

# Calculate the population weight for each intersection based on the proportion of the area

ward1981_counties1981 <- ward1981_counties1981 %>%
  group_by(ward_code) %>%
  mutate(population_weight = as.numeric(intersect_area / sum(intersect_area))) %>%
  ungroup() %>%
  dplyr::select(-intersect_area)

hist(ward1981_counties1981$population_weight)

#trim the weights

ward1981_counties1981 %>%
  filter(!population_weight < 0.99 & population_weight > 0.01)

write_rds(ward1981_counties1981, file = here("data/shapefiles/ward1981_counties1981.rds"))

# 1981 counties to 2001 wards

ggplot() +
  geom_sf(data = gb_counties_1981) +
  geom_sf(data = gb_wards_2001)

# Perform the intersection

wards2001_counties1981 <- st_intersection(gb_wards_2001,gb_counties_1981)

# Calculate the area of each intersection

wards2001_counties1981 <- wards2001_counties1981 %>%
  mutate(intersect_area = st_area(.))

# Create look-up

wards2001_counties1981 <- wards2001_counties1981 %>%
  st_drop_geometry() %>%
  dplyr::select(ward_name = name, ward_code = label, county_name = name.1, intersect_area) %>%
  distinct(ward_name,ward_code,county_name,.keep_all = T)

# Calculate the population weight for each intersection based on the proportion of the area

wards2001_counties1981 <- wards2001_counties1981 %>%
  group_by(ward_code) %>%
  mutate(population_weight = as.numeric(intersect_area / sum(intersect_area))) %>%
  ungroup() %>%
  dplyr::select(-intersect_area)

wards2001_counties1981 %>%
  filter(is.na(population_weight))

hist(wards2001_counties1981$population_weight)

wards2001_counties1981 %>%
  filter(population_weight < 0.95 & population_weight > 0.05) %>% print(n = Inf)

write_rds(wards2001_counties1981, file = here("data/shapefiles/ward2001_counties1981.rds"))

#####
# Census data summaries

# Using nomisr for England and Wales: https://docs.evanodell.com/nomisr/
# Data is already provided in data folder so no need to replicate unless you prefer to
# You will need to register to obtain a Nomis API key

nomis_api_key()

#1981

#check codes for values and wards

nomis_get_metadata("NM_66_1", "measures")

nomis_get_metadata("NM_66_1", "geography", "TYPE")

#download and store data from nomis

for (id in c1981sas$id[cells]) {
  temp_table <- nomis_get_data(id = "NM_66_1", time = "latest", geography = "TYPE33",
                               measures=20100, cell = id)
  
  temp_table <- pivot_wider(distinct(temp_table[c("GEOGRAPHY_CODE","GEOGRAPHY_NAME",
                                                  "CELL_NAME","OBS_VALUE")]),
                            id_cols = c(GEOGRAPHY_NAME,GEOGRAPHY_CODE),
                            names_from = CELL_NAME, values_from = OBS_VALUE)
  
  write_rds(temp_table, file = paste0(here(),"/data/census/engwal/1981_wards/NM_66_1_",id,".rds"))
}

#read them back in and join them in one tibble

files <- list.files(path = here("data/census/engwal/1981_wards/"), pattern = "\\.rds$", full.names = TRUE)

c1981_ew_sas <- reduce(lapply(files, readRDS), full_join,
                       by = c("GEOGRAPHY_CODE","GEOGRAPHY_NAME"))

c1981_ew_sas$GEOGRAPHY_NAME

#aggregate using lookup

ward1981_counties1981 <- read_rds(file = here("data/shapefiles/ward1981_counties1981.rds"))

c1981_ew_sas

c1981_ew_sas <- left_join(c1981_ew_sas,select(ward1981_counties1981,-ward_name),
                          by = join_by(GEOGRAPHY_CODE == ward_code))

c1981_ew_sas <- c1981_ew_sas %>% filter(!is.na(c1981_ew_sas$`Total : Total`))

table(c1981_ew_sas$county_name)

summary(c1981_ew_sas)

#aggregate at county level using population weight

columns_to_sum <- names(c1981_ew_sas)[sapply(c1981_ew_sas, is.numeric) & !names(c1981_ew_sas) %in% "population_weight"]

c1981_ew_sas <- c1981_ew_sas %>%
  group_by(county_name) %>%
  summarise(across(all_of(columns_to_sum), ~ sum(. * population_weight))) %>%
  ungroup() %>%
  filter(`Total : Total`>1)

c1981_ew_sas$county_name

#calculate proportions

names(c1981_ew_sas)

#start with popbase - total residents

c1981_ew_sas[22]

c1981_ew_sas <- rename(c1981_ew_sas, popbase=`All resident 1981 : Total persons`)

c1981_ew_sas[22]==c1981_ew_sas[23]

#non-UK born: numerator

c1981_ew_sas[c(7:9,18:21)]

#denominator is all residents from same table

c1981_ew_sas[2]-c1981_ew_sas$popbase

c1981_ew_sas[2]

#perform the calculation

c1981_ew_sas$prop_nonukborn <- pull(rowSums(c1981_ew_sas[c(7:9,18:21)])/c1981_ew_sas[2])*100

summary(c1981_ew_sas)

#Industry (proportion working in agriculture)
#from 10% sample

#denominator: all resident

c1981_ew_sas$popbase

#numerator: all in agriculture

c1981_ew_sas[c(26,28)]

#weighting for 10% sample

c1981_ew_sas[23:24]

#can construct a basic 10% sample weight

c1981_ew_sas$weight <- pull(c1981_ew_sas[23]/(c1981_ew_sas[24]))

summary(c1981_ew_sas)

#perform the calculation and reweight

c1981_ew_sas$prop_agri <- (((rowSums(c1981_ew_sas[c(26,28)]))/c1981_ew_sas$popbase)*100)*c1981_ew_sas$weight

summary(c1981_ew_sas)

#Education (proportion with a degree (and professional vocational qualifications))
#from 10% sample

#denominator: all resident

c1981_ew_sas$popbase

#numerator: all with degrees and prof quals

c1981_ew_sas[29:32]

#perform the calculation and reweight

c1981_ew_sas$prop_deg <- (((rowSums(c1981_ew_sas[29:32]))/c1981_ew_sas$popbase)*100)*c1981_ew_sas$weight

summary(c1981_ew_sas)

#restrict to only needed columns

c1981_ew_sas <- c1981_ew_sas %>%
  select(county_name, popbase, prop_nonukborn, prop_deg, prop_agri)

# SCOTLAND
# Districts only available from Casweb - unfortunately not on Nomis!
# Have separately downloaded all tables needed at district level

c1981_sc_sas <- read_csv(here("data/census/scotland/scotland_dist_1981.csv"), skip_empty_rows = T)

#calculate proportions

names(c1981_sc_sas)

#start with popbase - total residents

c1981_sc_sas <- rename(c1981_sc_sas, popbase=`[81sas010043]`)

#non-UK born: numerator

c1981_sc_sas[9:15]

#denominator is popbase (same as all residents from same table)

c1981_sc_sas[4]-c1981_sc_sas$popbase

#perform the calculation

c1981_sc_sas$prop_nonukborn <- (rowSums(c1981_sc_sas[9:15])/c1981_sc_sas$popbase)*100

summary(c1981_sc_sas)

#Industry (proportion in agriculture)
#from 10% sample

names(c1981_sc_sas)

#denominator: all resident

c1981_sc_sas$popbase

#numerator: all in agriculture

c1981_sc_sas[c(19,21)]

#weighting for 10% sample

c1981_sc_sas[16:17]

#can construct a basic 10% sample weight

c1981_sc_sas$weight <- pull(c1981_sc_sas[16]/(c1981_sc_sas[17]))

summary(c1981_sc_sas)

#perform the calculation and reweight

c1981_sc_sas$prop_agri <- (((rowSums(c1981_sc_sas[c(19,21)]))/c1981_sc_sas$popbase)*100)*c1981_sc_sas$weight

summary(c1981_sc_sas)

#Education (proportion with a degree (and professional vocational qualifications))
#from 10% sample

names(c1981_sc_sas)

#denominator: all resident

c1981_sc_sas$popbase

#numerator: all with degrees and prof quals

c1981_sc_sas[22:25]

#perform the calculation and reweight

c1981_sc_sas$prop_deg <- (((rowSums(c1981_sc_sas[22:25]))/c1981_sc_sas$popbase)*100)*c1981_sc_sas$weight

summary(c1981_sc_sas)

#restrict to only needed columns

c1981_sc_sas <- c1981_sc_sas %>%
  select(county_name = `Zone Name`,
         popbase, prop_nonukborn, prop_deg, prop_agri)

c1981_sc_sas %>% print(n=Inf)

#combine and write as RDS

c1981_gb_counties <- bind_rows(c1981_ew_sas,c1981_sc_sas)

write_rds(c1981_gb_counties, file = here("data/census/c1981_gb_counties.rds"))

#####
#2001
#Do England and Wales first as Scotland again stored elsewhere
#from Nomis data so has to be done separately

c2001 <- nomis_search(content_type = "2001census")

c2001$name.value

#each table stored separately
#so identify tables needed

tables <- c(96,4,18,12)

#check if available at constituency-level

c2001$id[tables]

nomis_overview("NM_1613_1")

nomis_get_metadata("NM_1657_1", "geography", "TYPE")

#need to extract 2001 wards 

for (id in c2001$id[tables]) {
  
  temp_table <- nomis_get_data(id = id, time = "latest", geography = "TYPE312", measures=20100)
  
  temp_table <- pivot_wider(distinct(temp_table[c("GEOGRAPHY_CODE","GEOGRAPHY_NAME",
                                                  paste0(nomis_get_metadata(id)[2,2],"_NAME"),
                                                  "OBS_VALUE")]),
                            id_cols = c(GEOGRAPHY_NAME,GEOGRAPHY_CODE),
                            names_from = paste0(nomis_get_metadata(id)[2,2],"_NAME"),
                            values_from = OBS_VALUE,
                            names_prefix = paste0(nomis_data_info(id)$name.value,": "))
  
  write_rds(temp_table, file = paste0(here(),"/data/census/engwal/2001_wards/",id,".rds"))
}

#read them back in and join them in one tibble

files <- list.files(path = here("data/census/engwal/2001_wards/"), pattern = ".rds$", full.names = TRUE)

c2001_ew <- reduce(lapply(files, readRDS), full_join,
                   by = c("GEOGRAPHY_CODE","GEOGRAPHY_NAME"))

c2001_ew

#aggregate using lookup

ward2001_counties1981 <- read_rds(file = here("data/shapefiles/ward2001_counties1981.rds"))

c2001_ew <- left_join(c2001_ew,select(ward2001_counties1981,-ward_name),
                      by = join_by(GEOGRAPHY_CODE == ward_code))

c2001_ew %>% filter(is.na(population_weight))

c2001_ew %>%
  group_by(county_name) %>%
  summarise(n()) %>%
  print(n=Inf)

#aggregate at county level using population weight

columns_to_sum <- names(c2001_ew)[sapply(c2001_ew, is.numeric) & !names(c2001_ew) %in% "population_weight"]

c2001_ew <- c2001_ew %>%
  group_by(county_name) %>%
  summarise(across(all_of(columns_to_sum), ~ sum(. * population_weight))) %>%
  ungroup() %>%
  filter(`UV002 - Population density: All usual residents`>40)

c2001_ew$county_name

#calculate proportions

names(c2001_ew)

#start with popbase - total residents

c2001_ew <- rename(c2001_ew, popbase=`UV002 - Population density: All usual residents`)

c2001_ew[2]==c2001_ew$popbase

#non-UK born: numerator

names(c2001_ew)

c2001_ew[c(7:9)]

#denominator is all residents from same table

c2001_ew[2]-c2001_ew$popbase

c2001_ew[2]

#perform the calculation

c2001_ew$prop_nonukborn <- pull(rowSums(c2001_ew[c(7:9)])/c2001_ew[2])*100

summary(c2001_ew)

#Industry (proportion working in agriculture)

#denominator: all resident

c2001_ew$popbase

#numerator: all in agriculture

c2001_ew[11]

#perform the calculation and reweight

c2001_ew$prop_agri <- ((rowSums(c2001_ew[11]))/c2001_ew$popbase)*100
                       
summary(c2001_ew)

#Education (proportion with a degree (and professional vocational qualifications))

#denominator: all resident

c2001_ew$popbase

#numerator: all with degrees and prof quals

c2001_ew[31]

#perform the calculation and reweight

c2001_ew$prop_deg <- ((rowSums(c2001_ew[31]))/c2001_ew$popbase)*100

summary(c2001_ew)

#restrict to only needed columns

c2001_ew <- c2001_ew %>%
  select(county_name, popbase, prop_nonukborn, prop_deg, prop_agri)

#SCOTLAND
#only available from Scotland's Census website
#have separately downloaded all tables needed at CAS ward and combined in one file
#from here: https://www.scotlandscensus.gov.uk/documents/2001-census-table-data-cas-ward/

c2001_sc <- read_csv(here("data/census/scotland/c2001_sc_ksuv.csv"), skip_empty_rows = T)

#rename some and reduce to only needed variables

grep("Area", colnames(c2001_sc))

c2001_sc[12]

c2001_sc <- c2001_sc[c(1:2,6,12,494,622,736)]

c2001_sc <- rename(c2001_sc, popbase=`2001 Population: All people`)

#need to aggregate at 1981 district level

#import the lookup

ward2001_counties1981 <- read_rds(file = here("data/shapefiles/ward2001_counties1981.rds"))

#check IDs

setdiff(c2001_sc$ward_code,ward2001_counties1981$ward_code)

#join

c2001_sc <- left_join(c2001_sc,select(ward2001_counties1981,-ward_name))

c2001_sc %>% filter(is.na(population_weight))

c2001_sc %>%
  group_by(county_name) %>%
  summarise(n()) %>%
  print(n=Inf)

#aggregate at pcon level using population weight

columns_to_sum <- names(c2001_sc)[sapply(c2001_sc, is.numeric) & !names(c2001_sc) %in% "population_weight"]

c2001_sc <- c2001_sc %>%
  group_by(county_name) %>%
  summarise(across(all_of(columns_to_sum), ~ sum(. * population_weight))) %>%
  ungroup()

summary(c2001_sc)

#start with popbase - total residents

c2001_sc$popbase

#non-UK born: anti-numerator

c2001_sc$`United Kingdom`

#denominator is all residents

c2001_sc$popbase

#perform the calculation

c2001_sc$prop_nonukborn <- ((c2001_sc$popbase-c2001_sc$`United Kingdom`)/c2001_sc$popbase)*100

summary(c2001_sc)

#Population density

#Area in hectares: numerator

c2001_sc$`Area (hectares)`

#denominator is all residents

c2001_sc$popbase

#perform the calculation

c2001_sc$pop_density <- c2001_sc$popbase/c2001_sc$`Area (hectares)`

summary(c2001_sc)

#Education (proportion with a degree (and professional vocational qualifications))

#denominator: all resident

c2001_sc$popbase

#numerator: all with degrees and higher

c2001_sc$`Group 4`

#perform the calculation and reweight

c2001_sc$prop_deg <- (c2001_sc$`Group 4`/c2001_sc$popbase)*100

summary(c2001_sc)

#Proportion working in agriculture

#denominator: all resident

c2001_sc$popbase

#numerator:

c2001_sc$`A. Agriculture, hunting and forestry`

#perform the calculation and reweight

c2001_sc$prop_agri <- (c2001_sc$`A. Agriculture, hunting and forestry`/c2001_sc$popbase)*100

summary(c2001_sc)

#drop english counties

c2001_sc <- c2001_sc %>% 
  filter(popbase>40)

#restrict to only needed columns

c2001_sc <- c2001_sc %>%
  select(county_name, popbase, prop_nonukborn, prop_deg, prop_agri)

#combine to make GB

c2001_gb_counties <- bind_rows(c2001_ew,c2001_sc)

write_rds(c2001_gb_counties, file = here("data/census/c2001_gb_counties.rds"))

#####
#Combine all

c1981_gb_counties <- c1981_gb_counties %>% 
  rename_at(vars(-county_name), function(x) paste0(x,"_81"))

c2001_gb_counties <- c2001_gb_counties %>% 
  rename_at(vars(-county_name), function(x) paste0(x,"_01"))

gb_counties <- left_join(c1981_gb_counties,c2001_gb_counties)

gb_counties <- left_join(gb_counties,gb_counties_geog,by=join_by("county_name"=="name"))

gb_counties <- gb_counties %>%
  mutate(popdens_81 = popbase_81/(area/10000),
         popdens_01 = popbase_01/(area/10000)) %>%
  relocate(popdens_81, .before = prop_nonukborn_81) %>%
  relocate(popdens_01, .before = prop_nonukborn_01)

gb_counties %>%
  select(1:12) %>%
  print(n=Inf)

write_rds(gb_counties, file = here("data/gb_counties.rds"))
