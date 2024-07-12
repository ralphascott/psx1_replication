#####
# Load libraries

library(tidyverse)
library(haven)
library(here)
library(labelled)
library(psych)
library(fuzzyjoin)
library(mice)
library(parameters)
library(lme4)
library(fixest)
library(modelsummary)
library(marginaleffects)
library(patchwork)
library(sf)
library(tmap)

#####
# DATA REQUIRED
# 
# For replication script to function, users will need to apply to access and download
# the following BCS 1970 data from the UKDS and save the appropriate .dta files in the "data" folder.
#
# NB: The geographic data used is special licence access only so requires special permission for use.
# 
# Data citation: University College London, UCL Institute of Education, Centre for Longitudinal Studies. (2023).
# 1970 British Cohort Study. [data series]. 10th Release. UK Data Service. SN: 200001, DOI: http://doi.org/10.5255/UKDA-Series-200001
# 
# SN2666: 1970 British Cohort Study: Birth and 22-Month Subsample, 1970-1972
# SN2699: 1970 British Cohort Study: Age 5, Sweep 2 1975
# SN3723: 1970 British Cohort Study: Age 10, Sweep 3, 1980
# SN3535: 1970 British Cohort Study: Age 16, Sweep 4, 1986
# SN6095: 1970 British Cohort Study: Age 16, Sweep 4 Arithmetic Test, 1986
# SN8288: 1970 British Cohort Study: Age 16, Sweep 4 Reading and Matrices Tests, 1986
# SN3833: 1970 British Cohort Study: Age 26, Sweep 5, 1996
# SN5558: 1970 British Cohort Study: Age 29, Sweep 6, 1999-2000
# SN5641: 1970 British Cohort Study Response Dataset, 1970-2016
# SN7473: 1970 British Cohort Study: Age 42, Sweep 9, 2012
# SN5537: 1970 British Cohort Study Counties Data, 1980-2012: Special Licence Access
# SN8553: Harmonised Childhood Environment and Adult Wellbeing Measures in Three Longitudinal Cohort Studies: 1970 British Cohort Study

#####
#1. CONSTRUCT FULL DATASET

gc()

#1.1 BIRTH SWEEP
#find and import the BCS birth file

bcs_0 <- read_dta(here("data","bcs7072a.dta"))

bcs_0 <- select(bcs_0, bcsid, a0005a, a0009, a0010, a0014, a0018, a0255)

summary(bcs_0)

lookfor(bcs_0)

bcs_0$a0255

table(bcs_0$a0014)

table(bcs_0$a0018)

#recode missing values as NA

bcs_0 <- bcs_0 %>%
  mutate_all(.funs = ~ ifelse(. < 0, NA, .))

#recode female as factor

bcs_0$female <- factor(dplyr::recode(bcs_0$a0255,"1"=0,"2"=1))

#aggregate some ages for parental education

table(bcs_0$a0009)

bcs_0 <- bcs_0 %>%
  mutate(a0009 = factor(case_when(a0009 %in% 6:11 ~ 11,
                                  a0009 %in% 25:31 ~ 25,
                                  .default = a0009)),
         a0010 = factor(case_when(a0010 %in% 6:11 ~ 11,
                                  a0010 %in% 25:38 ~ 25,
                                  .default = a0010)))

table(bcs_0$a0009)

table(bcs_0$a0010)

#create new base datafile

bcs_full <- select(bcs_0, bcsid, female, m_age = a0005a, medu_age = a0009, fedu_age = a0010)

summary(bcs_full)

#import the birth derived variable file

bcs_0d <- read_dta(here("data","bcs1derived.dta"))

bcs_0d <- dplyr::select(bcs_0d, BCSID, BD1PSOC)

summary(bcs_0d)

table(bcs_0d$BD1PSOC)

#recode missing and unknown values as NA

bcs_0d$BD1PSOC[bcs_0d$BD1PSOC==-1] <- NA
bcs_0d$BD1PSOC[bcs_0d$BD1PSOC==2] <- NA

#recode as 5-level variable

bcs_0d$parscr <- relevel(factor(dplyr::recode(as.numeric(bcs_0d$BD1PSOC),"1"=1,"2"=99,"3"=1,"4"=2,"5"=3,"6"=4,"7"=5,"8"=6)), ref="3")

#join

bcs_full <- full_join(bcs_full, select(bcs_0d, bcsid = BCSID, parscr), by="bcsid")

summary(bcs_full)

#import childhood conditions datafile

bcs_wp9 <- read_dta(here("data","bcs70_closer_wp9.dta"))

bcs_wp9

#low birthweight

table(bcs_wp9$lowbwt)

bcs_wp9$lowbwt[bcs_wp9$lowbwt==-111] <- NA
bcs_wp9$lowbwt[bcs_wp9$lowbwt==-999] <- NA

bcs_wp9$lowbwt <- factor(bcs_wp9$lowbwt)

#Overcrowding

table(bcs_wp9$crowd)

bcs_wp9$crowd[bcs_wp9$crowd==-111] <- NA
bcs_wp9$crowd[bcs_wp9$crowd==-999] <- NA

#Home ownership: 1 = rented at both time-points, 2 = owned at one, 3 owned at both

table(bcs_wp9$tenure)

bcs_wp9$tenure[bcs_wp9$tenure==-111] <- NA
bcs_wp9$tenure[bcs_wp9$tenure==-999] <- NA

#full join

bcs_full <- full_join(bcs_full, select(bcs_wp9, bcsid, lowbwt, crowd, chi_ten = tenure), by = "bcsid") 

#1.2 AGE 5 SWEEP
#Mother's questionnaire - for maternal attitudes

bcs_5_mother <- read_dta(here("data","f699a.dta"))

#select attitudinal variables

bcs_5_mother <- select(bcs_5_mother,bcsid,d070,d079,d080,d081,d087,d090,d094,d097,d099,d100,d103,d106,d107)

summary(bcs_5_mother)

#recode missing data as NAs

bcs_5_mother <- bcs_5_mother %>%
  mutate_all(.funs = ~ ifelse(. == -3, NA, .))

#recode so higher score = more authoritarian

bcs_5_mother$d070r <- dplyr::recode(bcs_5_mother$d070, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d079r <- dplyr::recode(bcs_5_mother$d079, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d080r <- dplyr::recode(bcs_5_mother$d080, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d081r <- dplyr::recode(bcs_5_mother$d081, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d087r <- dplyr::recode(bcs_5_mother$d087, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d094r <- dplyr::recode(bcs_5_mother$d094, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d097r <- dplyr::recode(bcs_5_mother$d097, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d099r <- dplyr::recode(bcs_5_mother$d099, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d100r <- dplyr::recode(bcs_5_mother$d100, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d103r <- dplyr::recode(bcs_5_mother$d103, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d106r <- dplyr::recode(bcs_5_mother$d106, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d107r <- dplyr::recode(bcs_5_mother$d107, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)

bcs_5_mother

#join

bcs_full <- full_join(bcs_full, select(bcs_5_mother,bcsid,d070r,d079r,d080r,d081r,d087r,d090,
                                       d094r,d097r,d099r,d100r,d103r,d106r,d107r),by="bcsid")

#Home Interview questionnaire - for parental education

bcs_5_home <- read_dta(here("data","f699b.dta"))

bcs_5_home <- dplyr::select(bcs_5_home, bcsid, e190)

bcs_5_home$pared <- dplyr::recode(as.numeric(bcs_5_home$e190),
                                  "-1"=99,"-2"=99,"-3"=99,"1"=1,"2"=2,"3"=2,
                                  "4"=2,"5"=2,"6"=2,"7"=3,"8"=99)

bcs_5_home$pared[bcs_5_home$pared==99] <- NA

bcs_5_home$pared <- as.factor(bcs_5_home$pared)

#join

bcs_full <- full_join(bcs_full, select(bcs_5_home, bcsid, pared), by="bcsid")

summary(bcs_full)

#1.3 AGE 10 SWEEP for ability scales

bcs_10d <- read_dta(here("data","bcs3derived.dta"))

#create a tibble with only the variables needed and checks NAs

bcs_10d <- dplyr::select(bcs_10d, bcsid, bd3rdage, bd3maths)

summary(bcs_10d)

#recode missing data as NAs

bcs_10d <- bcs_10d %>%
  mutate_all(.funs = ~ ifelse(. == -1, NA, .))

bcs_10d

#rescale variables

bcs_10d$bd3rdage <- scale(bcs_10d$bd3rdage)[,1]
bcs_10d$bd3maths <- scale(bcs_10d$bd3maths)[,1]

#join

bcs_full <- full_join(bcs_full, select(bcs_10d, bcsid, eng10 = bd3rdage, maths10 = bd3maths), by="bcsid")

bcs_full

#1.4 AGE 16 SWEEP
#Attitudes scales

bcs_16 <- read_dta(here("data","bcs7016x.dta"))

#create a tibble with only the variables needed

bcs_16_atts <- dplyr::select(bcs_16, bcsid, c5p8, c5p15, c5p1, c5p14, c5p2, c5p3)

summary(bcs_16_atts)

#code NAs

bcs_16_atts <- bcs_16_atts %>%
  mutate_all(.funs = ~ ifelse(. < 0, NA, .))

#best alpha (0.64) is just these two elements:
#c5p8 - Black people should not marry white people
#c5p15 - Black people are just as good as white people

bcs_16_atts$c5p8r <- dplyr::recode(bcs_16_atts$c5p8, "1" = 3, "2" = 2, "3" = 1)

bcs_16_atts$c5p8r

psych::alpha(bcs_16_atts[c("c5p8r","c5p15")], check.keys = T)

#Best alpha (0.63) is of these two elements:
#c5p1 - Flogging should be brought back for violent crime
#c5p14 - Hanging should be brought back (for murder)

bcs_16_atts$c5p1r <- dplyr::recode(bcs_16_atts$c5p1, "1" = 3, "2" = 2, "3" = 1)
bcs_16_atts$c5p14r <- dplyr::recode(bcs_16_atts$c5p14, "1" = 3, "2" = 2, "3" = 1)

bcs_16_atts

psych::alpha(bcs_16_atts[c("c5p1r","c5p14r")], check.keys = T)

#c5p2 #Trade unions are necessary to represent workers rights
#c5p3 #Strikes should be made illegal

bcs_16_atts$c5p3r <- dplyr::recode(bcs_16_atts$c5p3, "1" = 3, "2" = 2, "3" = 1)

psych::alpha(bcs_16_atts[c("c5p2","c5p3r")], check.keys = T)

#join

bcs_full <- full_join(bcs_full, select(bcs_16_atts, bcsid, c5p8r, c5p15, c5p1r, c5p14r,
                                       c5p2, c5p3r), by="bcsid")

#Qualifications
#Derived score from all grades achieved in public examinations at age 16 (Grade A = 7, Grade 5 CSE = 1) (age 16): (range: 0-99)

bcs_16_quals <- bcs_16[c(1,4551:4646)]

summary(bcs_16_quals)

bcs_16_quals %>% select(ends_with("2_2")) %>%
  lookfor()

#code NAs and drop columns that are all NA

bcs_16_quals <- bcs_16_quals %>%
  mutate_all(.funs = ~ ifelse(. < 0, NA, .)) %>%
  select_if(~!all(is.na(.)))

names(bcs_16_quals)

#recode O-levels

onames <- bcs_16_quals %>% select(ends_with("1_2") | ends_with("1_3")) %>% names()

bcs_16_quals <- bcs_16_quals %>%
  mutate_at(onames, funs(dplyr::recode(.,"1"=7,"2"=6,"3"=5,"4"=4,"5"=3,"6"=0)))

#recode CSEs

csenames <- bcs_16_quals %>% select(ends_with("2_2")) %>% names()

bcs_16_quals <- bcs_16_quals %>%
  mutate_at(csenames, funs(dplyr::recode(.,"1"=5,"2"=4,"3"=3,"4"=2,"5"=1,"6"=0)))

#calculate total score (including NAs if all quals are missing)

allquals <- c(onames,csenames)

bcs_16_quals$q16score <- ifelse(apply(is.na(bcs_16_quals[allquals]),1,all),
                                NA,rowSums(bcs_16_quals[allquals],na.rm=TRUE))

table(bcs_16_quals$q16score)

#Whether CM has a Grade A-C English O-level/GCSE or Grade 1 CSE (age 16, 30): 0=no, 1=yes

table(bcs_16_quals$t2a1_2)

bcs_16_quals$eng16 <- ifelse(bcs_16_quals$t2a1_2 > 4 | bcs_16_quals$t2a2_2 == 5,1,
                             ifelse(apply(is.na(bcs_16_quals[allquals]),1,all),NA,0))

table(bcs_16_quals$eng16)

#Whether CM has a Grade A-C Math O-level/GCSE or Grade 1 CSE (age 16, 30): 0=no, 1=yes

bcs_16_quals$maths16 <- ifelse(bcs_16_quals$t2c1_2 > 4 | bcs_16_quals$t2c2_2 == 5,1,
                               ifelse(apply(is.na(bcs_16_quals[allquals]),1,all),NA,0))

table(bcs_16_quals$maths16)

#join

bcs_full <- full_join(bcs_full, select(bcs_16_quals, bcsid, q16score, eng16, maths16), by="bcsid")

#Cognition at 16: A standardised score from a Principal Components Analysis (PCA) of five assessment scores completed by study members at age 16.
#Appendix from this is useful: https://cls.ucl.ac.uk/wp-content/uploads/2017/07/BCS70-Childhood-cognition-in-the-1970-British-Cohort-Study-Nov-2014-final.pdf

#Vocabulary

bcs_16_d <- read_dta(here("data","bcs4derived.dta"))

bcs_16_d$BD4RREAD[bcs_16_d$BD4RREAD==-1] <- NA

bcs_16_d$vocab16 <- scale(bcs_16_d$BD4RREAD)[,1]

#join

bcs_full <- full_join(bcs_full, select(bcs_16_d, bcsid = BCSID, vocab16), by="bcsid")

bcs_full

#Spelling

bcs_16_spell <- bcs_16[c(1,639:838)]

bcs_16_spell %>% summary()

bcs_16_spell <- bcs_16_spell %>%
  mutate_all(.funs = ~ ifelse(. < 0, NA, .))

bcs_16_spell$total <- ifelse(apply(is.na(bcs_16_spell[2:200]),1,all),
                             NA,rowSums(bcs_16_spell[2:200],na.rm=TRUE))

bcs_16_spell$spell16 <- scale(bcs_16_spell$total)[,1]

#join

bcs_full <- full_join(bcs_full, select(bcs_16_spell, bcsid, spell16), by="bcsid")

bcs_full

#Reading

bcs_16_reading <- read_dta(here("data","bcs1986_reading_matrices.dta"))

bcs_16_reading[177:182] %>% lookfor()

bcs_16_reading <- bcs_16_reading %>% select(BCSID,SCR_A,SCR_B,SCR_C,SCR_D,SCR_E,SCR_M)

summary(bcs_16_reading)

bcs_16_reading <- bcs_16_reading %>%
  mutate_all(.funs = ~ ifelse(. < 0, NA, .))

bcs_16_reading$read16 <- rowSums(bcs_16_reading[2:6])

bcs_16_reading$read16 <- scale(bcs_16_reading$read16)[,1]

#Matrices

table(bcs_16_reading$SCR_M)

bcs_16_reading$matrix16 <- scale(bcs_16_reading$SCR_M)[,1]

#join

bcs_full <- full_join(bcs_full, dplyr::select(bcs_16_reading, bcsid = BCSID, read16, matrix16), by = "bcsid")

bcs_full

#Arithmetic

bcs_16_arith <- read_dta(here("data","bcs70_16-year_arithmetic_data.dta"))

table(bcs_16_arith$mathscore)

bcs_16_arith$arith16 <- scale(bcs_16_arith$mathscore)[,1]

bcs_full <- full_join(bcs_full, dplyr::select(bcs_16_arith, bcsid, arith16), by = "bcsid")

#check scale and form final version

psych::alpha(select(bcs_full,vocab16,spell16,read16,matrix16,arith16))

bcs_full$cog16 <- scale(rowMeans(select(bcs_full,vocab16,spell16,read16,matrix16,arith16), na.rm = T))[,1]

bcs_full$cog16 <- ifelse(is.nan(bcs_full$cog16), NA, bcs_full$cog16)

hist(bcs_full$cog16)

rm(bcs_16)

gc()

#1.5 AGE 26 SWEEP

bcs_26 <- read_dta(here("data","bcs96x.dta"))

#reduce to required variables

bcs_26 <- bcs_26 %>%
  dplyr::select(bcsid,b960219,b960220,b960221,b960120,b960123,b960425,b960428,b960430,
                b960218,hqual26c,b960215,b960165,b960212,b960177,b960175,b960169,b960160,b960154,
                b960148,b960159,b960162,b960153,b960156,b960147,b960150,b960171,b960174,b960129)

bcs_26 <- bcs_26 %>%
  mutate_all(.funs = ~ ifelse(. < 0, NA, .))

#ISCED 2011 mapping
#based on: http://uis.unesco.org/en/isced-mappings

table(bcs_26$b960221)

bcs_26$isced26 <- case_when(bcs_26$b960221==26 ~ "7+",
                            bcs_26$b960220==24 ~ "7+",
                            bcs_26$b960219==23 ~ "6",
                            bcs_26$b960218==22 ~ "5",
                            bcs_26$hqual26c==5 ~ "5",
                            bcs_26$hqual26c==4 ~ "4",
                            bcs_26$b960215==20 ~ "3",
                            bcs_26$b960165==11 ~ "3",
                            bcs_26$b960212==19 ~ "3",
                            bcs_26$b960177==17 ~ "3",
                            bcs_26$b960177==17 ~ "3",
                            bcs_26$b960175>4 ~ "3",
                            bcs_26$b960169>4 ~ "3",
                            bcs_26$b960160>4 ~ "3",
                            bcs_26$b960154>4 ~ "3",
                            bcs_26$b960148>4 ~ "3",
                            bcs_26$hqual26c==3 ~ "3",
                            bcs_26$b960159==8 ~ "2",
                            bcs_26$b960162==9 ~ "2",
                            bcs_26$b960153==5 ~ "2",
                            bcs_26$b960156==6 ~ "2",
                            bcs_26$b960147==2 ~ "2",
                            bcs_26$b960150==3 ~ "2",
                            bcs_26$b960171==14 ~ "2",
                            bcs_26$b960174==16 ~ "2",
                            bcs_26$hqual26c==2 ~ "2",
                            bcs_26$b960129>12 ~ "2",
                            bcs_26$b960129<13 ~ "1")

table(is.na(bcs_26$isced26))

table(bcs_26$isced26)

#join

bcs_full <- full_join(bcs_full,dplyr::select(bcs_26,bcsid,isced26),
                      by = "bcsid")

bcs_full

#1.6 AGE 30 SWEEP
#import both datasets

bcs_30 <- read_dta(here("data","bcs2000.dta"))
bcs_30d <- read_dta(here("data","bcs6derived.dta"))

#code ISCED
#first bring in vocational NVQ coding

bcs_30 <- bcs_30 %>%
  left_join(dplyr::select(bcs_30d,bcsid=BCSID,HIVOC00))

bcs_30$edcse1[bcs_30$edcse1==99] <- NA
bcs_30$edcse2[bcs_30$edcse2==99] <- NA
bcs_30$edolev1[bcs_30$edolev1==99] <- NA
bcs_30$edolev1[bcs_30$edolev1==98] <- NA
bcs_30$edolev2[bcs_30$edolev2==99] <- NA
bcs_30$edolev2[bcs_30$edolev2==98] <- NA
bcs_30$edgcse1[bcs_30$edgcse1==99] <- NA
bcs_30$edgcse2[bcs_30$edgcse2==99] <- NA

#then code variable

bcs_30$isced30 <- case_when(bcs_30$numhghdg>0 ~ "7+",
                            bcs_30$pgceyear==1 ~ "7+",
                            bcs_30$numdeg>0 ~ "6",
                            bcs_30$numothdg>0 ~ "6",
                            bcs_30$eddiped>0 ~ "5",
                            bcs_30$HIVOC00==5 ~ "5",
                            bcs_30$HIVOC00==4 ~ "4",
                            bcs_30$numgcsas>0 ~ "3",
                            bcs_30$numaslvl>0 ~ "3",
                            bcs_30$edolev1>4 ~ "3",
                            bcs_30$edcse1>4 ~ "3",
                            bcs_30$edgcse1>4 ~ "3",
                            bcs_30$edscote>0 ~ "3",
                            bcs_30$edscotd>0 ~ "3",
                            bcs_30$edscotc>0 ~ "3",
                            bcs_30$edscotb>0 ~ "3",
                            bcs_30$HIVOC00==3 ~ "3",
                            bcs_30$edolev2>4 ~ "2",
                            bcs_30$edcse2>4 ~ "2",
                            bcs_30$edgcse2>4 ~ "2",
                            bcs_30$edscota>0 ~ "2",
                            bcs_30$HIVOC00==2 ~ "2",
                            bcs_30$actagel2>12 ~ "2",
                            bcs_30$actagel2<13 ~ "1")

table(bcs_30$isced30)

prop.table(table(bcs_30$isced30))

#slim down variables

bcs_30 <- bcs_30 %>%
  dplyr::select(bcsid, isced30, ethnic, edqsub66, finnow, cnetpay, cnetprd, 
                pnetpay, pnetprd, ar1, ar2, ar3, ar4, ar5, a1, a2, a3, a4, a5, a6)

#Ethnicity

bcs_30$nonbrit30 <- dplyr::recode(bcs_30$ethnic,"1"=0,"2"=1,"3"=1,
                                  "4"=1,"5"=1,"6"=1,"7"=1,"8"=1,"9"=0,"10"=1,
                                  "11"=1,"12"=1,"13"=1,"14"=1,"15"=1,
                                  "16"=1,"98"=99)

bcs_30$nonbrit30[bcs_30$nonbrit30==99] <- NA

bcs_30$nonbrit30 <- as.factor(bcs_30$nonbrit30)

table(bcs_30$nonbrit30)

#Income

#remove missing data

bcs_30$cnetpay[bcs_30$cnetpay==9999998] <- NA
bcs_30$cnetpay[bcs_30$cnetpay==9999999] <- NA
bcs_30$pnetpay[bcs_30$pnetpay==99999998] <- NA
bcs_30$pnetpay[bcs_30$pnetpay==99999999] <- NA

#then carry out the calculation to produce an individual net annual pay variable

bcs_30 <- bcs_30 %>%
  mutate(cmannualpay = case_when(cnetprd == 4 ~ (bcs_30$cnetpay*12),
                                 cnetprd == 3 ~ (bcs_30$cnetpay*13),
                                  cnetprd == 1 ~ (bcs_30$cnetpay*52),
                                  cnetprd == 2 ~ (bcs_30$cnetpay*26),
                                  cnetprd == 5 ~ bcs_30$cnetpay),
         pannualpay = case_when(pnetprd == 4 ~ (bcs_30$pnetpay*12),
                                pnetprd == 3 ~ (bcs_30$pnetpay*13),
                                pnetprd == 1 ~ (bcs_30$pnetpay*52),
                                pnetprd == 2 ~ (bcs_30$pnetpay*26),
                                pnetprd == 5 ~ bcs_30$pnetpay)) %>%
  mutate(hhannualpay = rowSums(select(., cmannualpay, pannualpay), na.rm = TRUE),
         hhannualpay = case_when(is.na(cmannualpay) & is.na(pannualpay) ~ NA_real_,
                                 .default = hhannualpay),
         hhpayquintile = ntile(hhannualpay,5))

summary(bcs_30$hhannualpay)

table(bcs_30$hhpayquintile)

bcs_30 %>% group_by(hhpayquintile) %>%
  summarise(pay_mean = mean(hhannualpay),
            pay_median = median(hhannualpay))

#Attitudes

#first remove coded NAs from all variables

bcs_30 <- bcs_30 %>%
  mutate(across(ar1:a6, ~ ifelse(. > 5 , NA, .)))

#Produce a prejudice scale - need to recode ar5 to be in same direction
#Higher number is more prejudiced

bcs_30$ar5r <- dplyr::recode(bcs_30$ar5, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)

#Produce an authoritarianism scale - higher number is more authoritarian
#All variables need to be recoded

bcs_30$a1r <- dplyr::recode(bcs_30$a1, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_30$a2r <- dplyr::recode(bcs_30$a2, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_30$a3r <- dplyr::recode(bcs_30$a3, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_30$a4r <- dplyr::recode(bcs_30$a4, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_30$a5r <- dplyr::recode(bcs_30$a5, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_30$a6r <- dplyr::recode(bcs_30$a6, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)

#join

bcs_30

bcs_full <- full_join(bcs_full,dplyr::select(bcs_30, bcsid, isced30, nonbrit30, hhpayquintile, 
                                             ar1, ar2, ar3, ar4, ar5r, a1r, a2r, a3r, a4r, a5r, a6r),
                      by = "bcsid")

bcs_full

#code subject string data

bcs_30$degsub <- ifelse(bcs_30$edqsub66=="",NA,bcs_30$edqsub66)

bcs_30_sub <- dplyr::select(bcs_30[complete.cases(bcs_30$degsub),],bcsid,degsub)

#fuzzyjoin package https://bookdown.org/Maxine/r4ds/fuzzyjoin.html

#clean up input vector

bcs_30_sub$degsub <- str_trim(bcs_30_sub$degsub)

bcs_30_sub$degsub <- str_to_lower(bcs_30_sub$degsub, locale = "en")

bcs_30_sub$degsub <- gsub("[[:punct:]]", "", bcs_30_sub$degsub)

bcs_30_sub$degsub <- str_replace(bcs_30_sub$degsub, "1", "NA")

bcs_30_sub$degsub <- str_replace(bcs_30_sub$degsub, "1992", "NA")

bcs_30_sub$degsub <- str_replace(bcs_30_sub$degsub, "maths", "mathematics")

bcs_30_sub$degsub <- ifelse(bcs_30_sub$degsub=="it","information technology",bcs_30_sub$degsub)

#try using simpler lookup

jacs_lookup <- read_csv(here("data","JACS3_lookup.csv"), col_names = T)

JACSjoin <- stringdist_left_join(bcs_30_sub,jacs_lookup, by = c("degsub" = "subject_label"),
                                 method = "dl", ignore_case = T)

JACSjoin

table(is.na(JACSjoin$subject_code))

#1048 coded, 1096 still missing
#So add the remainder using the more extensive HECoS lookup
#Using Jaro distance to find a measure of distance

#subject code lookup from https://www.hesa.ac.uk/support/documentation/hecos#Mapping

hecos_lookup <- read_csv(here("data","JACS3-to-HECoS-mapping_2017-06-28.csv"), col_names = T)

hecos_lookup <- hecos_lookup[c(2,6)]

hecosjoin <- stringdist_left_join(JACSjoin,hecos_lookup,by = c("degsub" = "HECoS_Label"),
                                  method = "jw", p = 0, ignore_case = T, distance_col = "dist")

hecosjoin

hecosjoin <- hecosjoin %>% group_by(bcsid) %>% slice_min(dist, n = 1)

hecosjoin$comb_code <- ifelse(is.na(hecosjoin$subject_code),hecosjoin$JACS3_code,hecosjoin$subject_code)

hecosjoin$comb_code <- str_sub(hecosjoin$comb_code,1,2)

#hardcode a few common duplicates

hecosjoin$comb_code <- ifelse(str_detect(hecosjoin$degsub, "biology"),"C1",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(hecosjoin$degsub=="ecology","C1",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(hecosjoin$degsub=="natural sciences","C1",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(str_detect(hecosjoin$degsub, "comput"),"I1",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(hecosjoin$degsub=="economics","L1",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(hecosjoin$degsub=="cultural studies","P3",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(str_detect(hecosjoin$degsub, "english"),"Q3",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(str_detect(hecosjoin$degsub, "french"),"R1",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(str_detect(hecosjoin$degsub, "german"),"R2",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(str_detect(hecosjoin$degsub, "history"),"V1",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(hecosjoin$degsub=="humanities","V1",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(str_detect(hecosjoin$degsub, "philosophy"),"V5",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(hecosjoin$degsub=="theology","V6",hecosjoin$comb_code)
hecosjoin$comb_code <- ifelse(str_detect(hecosjoin$degsub, "business"),"N1",hecosjoin$comb_code)

#remove duplicates

bcs.deg.codes <- distinct(dplyr::select(hecosjoin,bcsid,degsub,comb_code,HECoS_Label,dist))

write_csv(bcs.deg.codes, "data/bcs.deg.codes.csv")

#import after hand-coding duplicates
#this includes those assigned to Y1 - combined studies

bcs.deg.codes.man <- read_csv(here("data","bcs.deg.codes.man.csv"), col_names = T)

#check number of manual codes = 280

sum(table(bcs.deg.codes.man$man_code))

#create final vector overwriting fuzzy matched codes with manual codes

bcs.deg.codes.man$full_code <- ifelse(is.na(bcs.deg.codes.man$man_code),bcs.deg.codes.man$comb_code,bcs.deg.codes.man$man_code)

table(bcs.deg.codes.man$full_code)

bcs.deg.codes.man[!is.na(bcs.deg.codes.man$man_code),] #check this has worked

#create single code vector

bcs.deg.codes.man$final_code <- str_sub(bcs.deg.codes.man$full_code,1,1)

bcs_30_sub <- bcs.deg.codes.man[complete.cases(bcs.deg.codes.man$final_code),] #remove single NA case

#join

bcs_30

bcs_full <- full_join(bcs_full,dplyr::select(bcs_30_sub,bcsid,degsub,jacs2=full_code,
                                             jacs1=final_code),
                      by = "bcsid")

bcs_full

#1.7 AGE 42 SWEEP

bcs_42 <- read_dta(here("data","bcs70_2012_flatfile.dta"))

#university name: code unknown as NA

table(bcs_42$B9UNIDGN)

bcs_42$B9UNIDGN[bcs_42$B9UNIDGN<0] <- NA

table(is.na(bcs_42$B9UNIDGN))

#University type based on notation in Bolliver article, as follows:
#1 = Russell Group
#2 = non-Russell Group pre-1992
#3 = post-1992
#5 = other

bcs_42$unitype42 <- dplyr::recode(bcs_42$B9UNIDGN,
                                               "1"=3,"2"=2,"3"=3,"4"=2,"5"=3,"6"=2,"7"=2,
                                               "8"=2,"9"=3,"10"=3,"11"=3,"12"=1,"13"=3,"14"=3,
                                               "15"=3,"16"=3,"17"=3,"18"=3,"19"=3,"20"=3,
                                               "21"=1,"22"=2,"23"=5,"24"=3,"25"=1,"26"=3,
                                               "27"=5,"28"=3,"29"=1,"30"=3,"31"=5,"32"=3,
                                               "33"=3,"34"=2,"35"=5,"36"=5,"37"=3,"38"=5,
                                               "39"=3,"40"=3,"41"=3,"42"=2,"43"=1,"44"=2,
                                               "45"=3,"46"=3,"47"=1,"48"=3,"49"=5,"50"=2,
                                               "51"=1,"52"=3,"53"=3,"54"=1,"55"=3,"56"=5,
                                               "57"=3,"58"=3,"59"=2,"60"=3,"61"=5,"62"=3,
                                               "63"=2,"64"=3,"65"=5,"66"=3,"67"=3,"68"=2,
                                               "69"=1,"70"=2,"71"=2,"72"=1,"73"=3,"74"=2,
                                               "75"=1,"76"=5,"77"=3,"78"=3,"79"=2,"80"=3,
                                               "81"=1,"82"=5,"83"=3,"84"=3,"85"=1,"86"=5,
                                               "87"=3,"88"=5,"89"=3,"90"=1,"91"=5,"92"=3,
                                               "93"=2,"94"=1,"95"=3,"96"=3,"97"=1,"98"=3,
                                               "99"=3,"100"=3,"101"=5,"102"=1,"103"=3,
                                               "104"=5,"105"=1,"106"=3,"107"=3,"108"=3,
                                               "109"=3,"110"=3,"111"=1,"112"=1,"113"=5,
                                               "114"=2,"115"=3,"116"=3,"117"=5,"118"=5,
                                               "119"=5,"120"=5,"121"=5,"122"=5,"123"=2,
                                               "124"=5,"125"=5,"126"=5,"127"=3,"128"=2,
                                               "129"=5,"130"=1,"131"=3,"132"=1,"133"=3,
                                               "134"=2,"135"=5,"136"=5,"137"=5,"138"=3,
                                               "139"=2,"140"=5,"141"=2,"142"=3,"143"=2,
                                               "144"=2,"145"=2,"146"=3,"147"=3,"148"=3,
                                               "149"=5,"150"=3,"151"=2,"152"=3,"153"=2,
                                               "154"=3,"155"=1,"156"=3,"157"=3,"158"=3,
                                               "159"=3,"160"=3,"161"=3,"162"=3,"163"=5,
                                               "164"=1,"165"=3,"166"=5)

table(bcs_42$unitype42)

#recode university subject
#first accumulate into a single column - although this creates duplicates due to joint honours
#https://stackoverflow.com/questions/17735859/for-each-row-return-the-column-name-of-the-largest-value

bcs_42 <- bcs_42 %>%
  left_join(
    bcs_42 %>%
      select(BCSID,B9SBDG01:B9SBDG52) %>%
      mutate(across(everything(), ~ ifelse(. < 0 , NA, .))) %>%
      gather(degsub42, cnt, B9SBDG01:B9SBDG52) %>% 
      group_by(BCSID) %>% 
      filter(cnt==1) %>%
      dplyr::select(BCSID,degsub42), 
    by = "BCSID")

bcs_42$degsub42 <- gsub("B9SBDG", "", bcs_42$degsub42)

table(bcs_42$degsub42)

#have 49 categories plus some others/DKs
#recode according to JACS-3 subject areas: https://www.hesa.ac.uk/support/documentation/jacs/jacs3-principal

bcs_42$jacs42_2 <- dplyr::recode(bcs_42$degsub42,"01"="D4","02"="T7","03"="B1","04"="L6",
                                     "05"="V4","06"="K1","07"="W1","08"="C1","09"="K4","10"="N1",
                                     "11"="H8","12"="F1","13"="H2","14"="Q8","15"="I1","16"="A4",
                                     "17"="W4","18"="F6","19"="L1","20"="X1","21"="H6","22"="H1",
                                     "23"="Q3","24"="F8","25"="V1","26"="M1","27"="J1","28"="G1",
                                     "29"="H3","30"="P1","31"="A1","32"="R0","33"="W3","34"="B7",
                                     "35"="B2","36"="V5","37"="F3","38"="L2","39"="C8","40"="V6",
                                     "41"="L4","42"="L5","43"="L3","44"="C6","45"="N8","46"="D1",
                                     "47"="D4","48"="N4","49"="N5","50"="O","52"="O")

table(bcs_42$jacs42_2)

#code as only one letter JACS

bcs_42$degjac42 <- dplyr::recode(bcs_42$degsub42,"01"="D","02"="T","03"="B","04"="L",
                                     "05"="V","06"="K","07"="W","08"="C","09"="K","10"="N",
                                     "11"="H","12"="F","13"="H","14"="Q","15"="I","16"="A",
                                     "17"="W","18"="F","19"="L","20"="X","21"="H","22"="H",
                                     "23"="Q","24"="F","25"="V","26"="M","27"="J","28"="G",
                                     "29"="H","30"="P","31"="A","32"="Q","33"="W","34"="B",
                                     "35"="B","36"="V","37"="F","38"="L","39"="C","40"="V",
                                     "41"="L","42"="L","43"="L","44"="C","45"="N","46"="D",
                                     "47"="D","48"="N","49"="N","50"="O","52"="O")

table(bcs_42$degjac42)

bcs_42 <- bcs_42 %>%
  select(bcsid = BCSID,unitype42,uniname42=B9UNIDGN,degsub42,jacs42_2,degjac42)

#join

bcs_full <- full_join(bcs_full,bcs_42,
                      by = "bcsid")

bcs_full

#1.8 DEGREE VARIABLE

bcs_full$deg30 <- case_when(bcs_full$isced30 == "7+" ~ 1,
                            bcs_full$isced30 == "6" ~ 1,
                            bcs_full$isced26 == "7+" ~ 1,
                            bcs_full$isced26 == "6" ~ 1,
                            !is.na(bcs_full$isced26)|!is.na(bcs_full$isced30) ~ 0)

table(bcs_full$deg30)

#define a final degree subject variable of both age 30 and 42 (only for those with a degree at 30)

bcs_full %>%
  filter(bcs_full$deg30 == 1) %>%
  mutate(jacscomb = ifelse(is.na(jacs1),degjac42,jacs1))

bcs_full$jacscomb <- ifelse(bcs_full$deg30 == 1,
                            ifelse(is.na(bcs_full$jacs1),bcs_full$degjac42,bcs_full$jacs1),NA)

bcs_full$jacsfull <- ifelse(bcs_full$deg30 == 1,
                            ifelse(is.na(bcs_full$jacs2),bcs_full$jacs42_2,bcs_full$jacs2),NA)

length(table(bcs_full$jacsfull))

#4-part subject coding
#1 = STEM: A, B, C, D, F, G, H, I, J
#2 = business, law, agriculture and planning: K, M, N
#3 = arts, humanities and social science: L, P, Q, R, T, V, W, X
#4. other: Y

bcs_full$degsub4 <- dplyr::recode(bcs_full$jacscomb,"A"=1,"B"=1,"C"=1,"D"=2,"F"=1,
                                  "G"=1,"H"=1,"I"=2,"J"=1,"K"=2,"L"=3,"M"=2,"N"=2,"O"=4,
                                  "P"=3,"Q"=3,"R"=3,"T"=3,"V"=3,"W"=3,"X"=3,"Y"=4)

table(bcs_full$degsub4)

#code economics as business and law
#1 = STEM: A, B, C, D, F, G, H, I, J
#2 = economics, business, law, agriculture and planning: K, M, N
#3 = arts, humanities and social science: L, P, Q, R, T, V, W, X
#4. other: Y

bcs_full$degsub4_econ <- case_when(bcs_full$jacs2=="L1" ~ 2,
                                   bcs_full$deg30 == 1 & bcs_full$jacs42_2=="L1" ~ 2,
                                   .default = bcs_full$degsub4)

table(bcs_full$degsub4_econ,bcs_full$degsub4,useNA = "always")

#check mean outcomes by subject

bcs_full$racemarriage16 <- bcs_full$c5p8r

bcs_full$racemarriage30 <- bcs_full$ar1

bcs_full$deathpen16 <- bcs_full$c5p14r

bcs_full$deathpen30 <- bcs_full$a2r

#select out working variables to avoid duplicates

bcs_full <- dplyr::select(bcs_full,-jacs2,-jacs42_2,-jacs1,-degsub,-degsub42,-degjac42,
                          -jacscomb,-jacsfull)

bcs_full <- distinct(bcs_full)

#####
#GEOGRAPHIC DATA

#import special licence data

bcs_counties <- read_dta(here("data","bcs70_counties_data_protect.dta"))

# run script which generates census summaries at county-level and then import file generated
# (summary file is provided so can skip Census script if needed)

# source(here("scripts/census_summaries.R"))

gb_counties <- read_rds(here("data","gb_counties.rds"))

#add Scotland binary

gb_counties$scotland <- ifelse(!str_detect(gb_counties$county_name, "[[:lower:]]"),1,0)

#clean up counties data

clean_counties <- function(x){
  x <- toupper(x)
  x <- str_replace_all(x,"_"," ")
  x <- str_replace_all(x,"CITY OF ","")
  x <- str_replace_all(x," CITY","")
  x <- str_replace_all(x," AND BUTE","")
  x <- str_replace_all(x," AND KILSYTH","")
  x <- str_replace_all(x," ISLANDS","")
  x <- str_replace_all(x,"\\(NOW CLYDESDALE\\)","")
  x <- str_replace_all(x,"MINGAVIE","MILNGAVIE")
  x <- str_replace_all(x,"KIRKALDY","KIRKCALDY")
  x <- str_replace_all(x,"EAST LOTHAIN","EAST LOTHIAN")
  x <- str_replace_all(x,"TWEEDALE","TWEEDDALE")
  x <- str_replace_all(x,"EASTWARD","EASTWOOD")
  x <- str_replace_all(x,"LOUDON","LOUDOUN")
  x <- str_replace_all(x,"BISHOPBRIGGS AND KIRKINTULOCH","STRATHKELVIN")
  x <- str_replace_all(x,"MERRICK","WIGTOWN")
  x <- str_replace_all(x,"CUNNINGHAME","CUNNINGHAM")
  return(x)
}

bcs_counties <- bcs_counties %>%
  select(BCSID, county16 = B4CTY81,county30 = B6CTY81) %>%
  mutate(across(county16:county30, ~ as_factor(.))) %>%
  mutate(across(county16:county30, ~ clean_counties(.))) %>%
  mutate(across(county16:county30, ~ case_when(.=="MISSING OR NOT IN GREAT BRITAIN"~NA_character_,
                                             .default = .)))

#match distances

names(gb_counties)

gb_counties_distances <- gb_counties %>%
  select(county16 = county_name,`ABERDEEN CITY`:west_glamorgan) %>%
  pivot_longer(cols = -county16, names_to = "county30", values_to = "distance") %>%
  mutate(across(county16:county30, ~ clean_counties(.)))

setdiff(unique(bcs_counties$county16),unique(gb_counties_distances$county16))

setdiff(unique(gb_counties_distances$county30),unique(bcs_counties$county30))

bcs_counties <- left_join(bcs_counties,gb_counties_distances)

hist(bcs_counties$distance)

summary(bcs_counties)

#match census vars

#create two files for matching

gb_counties_81 <- gb_counties %>%
  select(county16 = county_name,scotland16 = scotland,popbase_81:prop_agri_81) %>%
  mutate(county16 = clean_counties(county16))

gb_counties_01 <- gb_counties %>%
  select(county30 = county_name,scotland30 = scotland,popbase_01:prop_agri_01) %>%
  mutate(county30 = clean_counties(county30))

#check matching vectors

setdiff(unique(bcs_counties$county16),unique(gb_counties_81$county16))

setdiff(unique(gb_counties_distances$county30),unique(gb_counties_01$county30))

#complete joins

bcs_counties <- left_join(bcs_counties,gb_counties_81)

bcs_counties <- left_join(bcs_counties,gb_counties_01)

#join to main file

bcs_full <- left_join(bcs_full,bcs_counties,by=join_by("bcsid"=="BCSID"))

#maps

eng_counties_1981 <- read_sf(here("data/shapefiles/1981_counties/england/england_ct_1981.shp"))

wal_counties_1981 <- read_sf(here("data/shapefiles/1981_counties/wales/wales_ct_1981.shp"))

sco_counties_1981 <- read_sf(here("data/shapefiles/1981_counties/scotland/scotland_dt_1981.shp"))

gb_counties_shapefiles <- bind_rows(eng_counties_1981,wal_counties_1981,sco_counties_1981)

gb_counties_shapefiles$name <- clean_counties(gb_counties_shapefiles$name)

gb_counties_shapefiles <- left_join(gb_counties_shapefiles, gb_counties_81, by = join_by("name"=="county16"))

gb_counties_shapefiles <- left_join(gb_counties_shapefiles, gb_counties_01, by = join_by("name"=="county30"))

map_popdens_81 <- tm_shape(gb_counties_shapefiles) +
  tm_fill("popdens_81", palette = "Reds", style = "cont", title = "Pop. density \n (1981 Census)") +
  tm_borders(alpha=.4)

map_prop_nonukborn_81 <- tm_shape(gb_counties_shapefiles) +
  tm_fill("prop_nonukborn_81", palette = "Purples", style = "cont", title = "% non-UK born \n (1981 Census)") +
  tm_borders(alpha=.4) 

map_prop_deg_81 <- tm_shape(gb_counties_shapefiles) +
  tm_fill("prop_deg_81", palette = "Blues", style = "cont", title = "% graduates \n (1981 Census)") +
  tm_borders(alpha=.4)

map_prop_agri_81 <- tm_shape(gb_counties_shapefiles) +
  tm_fill("prop_agri_81", palette = "Greens", style = "cont", title = "% in agriculture \n (1981 Census)") +
  tm_borders(alpha=.4)

maps_1981 <- tmap_arrange(map_popdens_81, map_prop_nonukborn_81, map_prop_deg_81, map_prop_agri_81, ncol = 2, nrow = 2)

tmap_save(maps_1981, here("figures/maps_1981.png"), height = 11, width = 8)

map_popdens_01 <- tm_shape(gb_counties_shapefiles) +
  tm_fill("popdens_01", palette = "Reds", style = "cont", title = "Pop. density \n (2001 Census)") +
  tm_borders(alpha=.4)

summary(gb_counties_shapefiles)

map_prop_nonukborn_01 <- tm_shape(gb_counties_shapefiles) +
  tm_fill("prop_nonukborn_01", palette = "Purples", style = "cont", title = "% non-UK born \n (2001 Census)") +
  tm_borders(alpha=.4) 

map_prop_deg_01 <- tm_shape(gb_counties_shapefiles) +
  tm_fill("prop_deg_01", palette = "Blues", style = "cont", title = "% graduates \n (2001 Census)") +
  tm_borders(alpha=.4)

map_prop_agri_01 <- tm_shape(gb_counties_shapefiles) +
  tm_fill("prop_agri_01", palette = "Greens", style = "cont", title = "% in agriculture \n (2001 Census)") +
  tm_borders(alpha=.4)

maps_2001 <- tmap_arrange(map_popdens_01, map_prop_nonukborn_01, map_prop_deg_01, map_prop_agri_01, ncol = 2, nrow = 2)

tmap_save(maps_2001, here("figures/maps_2001.png"), height = 11, width = 8)

#####
# CLEAN UP AND FINALISE

#examine duplicates - 34 duplicated cases due to joint honours even after aggregation

duplicates <- bcs_full[duplicated(bcs_full$bcsid)|duplicated(bcs_full$bcsid,fromLast = T),]

table(duplicates$bcsid)

bcs_full[duplicated(bcs_full$bcsid)|duplicated(bcs_full$bcsid,fromLast = T),] %>%
  select(bcsid,degsub4,degsub4_econ)

#remove duplicate by taking first instance (but treat second instance as robustness check)

degsub4_robust_adds <- bcs_full %>%
  group_by(bcsid) %>%
  slice(2) %>%
  ungroup() %>%
  select(bcsid,degsub4_robust=degsub4)

bcs_full <- bcs_full %>%
  group_by(bcsid) %>%
  slice(1) %>%
  ungroup()

bcs_full <- left_join(bcs_full,degsub4_robust_adds)

bcs_full$degsub4_robust <- ifelse(is.na(bcs_full$degsub4_robust),
                                  bcs_full$degsub4,bcs_full$degsub4_robust)

table(bcs_full$degsub4,bcs_full$degsub4_robust)

#remove those who only have BCSID
#627 of these

bcs_full %>%
  filter(!is.na(bcsid) & if_all(female:degsub4_robust, ~ is.na(.)))

bcs_full <- bcs_full %>%
  filter(!if_all(female:degsub4_robust, ~ is.na(.)))

#Remove those who aren't part of the population any longer

#Import response dataset and join to remove cases who emigrate or die by 30

bcs_resp <- read_dta(here("data","bcs70_response_1970-2016.dta"))

bcs_resp

#code a variable for dead or emigrated at 30

bcs_resp$remove <- ifelse(bcs_resp$OUTCME06 == 7 | bcs_resp$OUTCME06 == 8 ,1,0)

table(bcs_resp$remove)

#join this and response to birth wave

bcs_full <- left_join(bcs_full,dplyr::select(bcs_resp,bcsid=BCSID,birth_resp=OUTCME01,remove))

#filter out those who were dead or permanently emigrated by 30
#or didn't respond to birth wave

bcs_full <- bcs_full %>%
  filter(remove==0 & birth_resp==1)

#check overlap between final and those in response file

setdiff(bcs_full$bcsid,bcs_resp$BCSID)

#then drop these variables

bcs_full <- bcs_full %>%
  dplyr::select(-birth_resp,-remove)

summary(bcs_full)

#write file

write_rds(bcs_full, "data/bcs_full.rds")

#####
#1.12 MULTIPLE IMPUTATION

bcs_full <- read_rds(here("data","bcs_full.rds"))

#code no degree at 30 as 0 in derived variables

bcs_mi <- bcs_full %>% 
  mutate(degsub4 = case_when(deg30==0 & is.na(degsub4) ~ 0, .default = degsub4),
         uniname42 = case_when(deg30==0 & is.na(uniname42) ~ 0, .default = uniname42))

#remove unneeded variables

names(bcs_full)

unneeded_vars <- c("isced26","isced30","degsub4_econ","degsub4_robust",
                   "unitype42","county16","county30","deg30","vocab16","spell16",
                   "read16","matrix16","arith16","popbase_81","popbase_01",
                   "scotland16","scotland30")

bcs_mi <- select(bcs_mi, !all_of(unneeded_vars), nonbrit = nonbrit30)

#specify mixed race marriage items

bcs_mi$racemarriage16 <- bcs_mi$c5p8r

bcs_mi$racemarriage30 <- bcs_mi$ar1

#specify death pen items

bcs_mi$deathpen16 <- bcs_mi$c5p14r

bcs_mi$deathpen30 <- bcs_mi$a2r

#Drop composite scale items

val_comps <- c("d070r","d079r","d080r","d081r","d087r","d090","d094r","d097r",
               "d099r","d100r","d103r","d106r","d107r","c5p8r","c5p15","c5p1r","c5p3r",
               "c5p14r","c5p2","ar1","ar2","ar3","ar4","ar5r","a1r","a2r","a3r","a4r","a5r","a6r")

bcs_mi <- bcs_mi %>% 
  select(!all_of(val_comps))

#clean up data

bcs_mi <- droplevels(bcs_mi)

bcs_mi <- remove_labels(bcs_mi)

#make factors

bcs_mi <- bcs_mi %>%
  mutate(across(hhpayquintile:degsub4, ~ as_factor(.)))

table(bcs_mi$uniname42)

#clean up data again

bcs_mi <- droplevels(bcs_mi)

#carry out imputation

#first derive object without imputations to change settings

ini <- mice(bcs_mi, maxit = 0, print = FALSE)

ini$loggedEvents

#check predictors - don't let uniname predict others

pred_ch2_1 <- ini$pred

pred_ch2_1[,"uniname42"] <- 0

#change methods to predictive mean matching for all except binary

meth_ch2_1 <- ini$method

meth_ch2_1 <- str_replace_all(meth_ch2_1,c("polr"="pmm","polyreg"="pmm"))

#carry out test imputation to review any issues

imp_ch2_1_test <- mice(bcs_mi, pred = pred_ch2_1, method = meth_ch2_1, m = 1, maxit = 1)

imp_ch2_1_test$loggedEvents

#carry out more imputations and iterations

imp_ch2_1_test <- mice(bcs_mi, pred = pred_ch2_1, method = meth_ch2_1, m = 5, maxit = 5)

imp_ch2_1_test$loggedEvents

table(mice::complete(imp_ch2_1_test)$uniname)

#fit a regression and investigate results

ch2_1_fit <- with(imp_ch2_1_test, lm(deathpen30 ~ degsub4 + uniname42 + female + nonbrit +
                                       parscr + pared + eng10 + maths10 + deathpen16))

pool_ch2_1_test <- pool(ch2_1_fit)

pool_ch2_1_test

summary(pool_ch2_1_test)

#run many more imputations using parallel processing
#according to: https://cls.ucl.ac.uk/wp-content/uploads/2020/04/Handling-missing-data-in-the-National-Child-Development-Study-User-Guide.pdf
#should run as many imputations as highest FMI percentage in the model - so 90 rounded to 100 in this case

parallelly::availableCores(logical = TRUE)

bcs_imp_data <- futuremice(bcs_mi, pred = pred_ch2_1, method = meth_ch2_1, m = 100,
                           maxit = 20, parallelseed = 1987)

save(bcs_imp_data, file = here("data/bcs_imp_data.rda"))

load(file = here("data/bcs_imp_data.rda"))

#run a regression again

ch2_1_fit <- with(bcs_imp_data, lm(deathpen30 ~ degsub4 + uniname42 + female + nonbrit +
                                     parscr + pared + eng10 + maths10 + deathpen16))

pool_ch2_1_test <- pool(ch2_1_fit)

pool_ch2_1_test

summary(pool_ch2_1_test)

#####
#CREATE FINAL FILE FOR MODELLING

bcs_full <- read_rds(here("data","bcs_full.rds"))

table(is.na(bcs_full$county16),is.na(bcs_full$county30))

#make ever-Scotland variable

bcs_full$everscot <- ifelse(bcs_full$scotland16==1|bcs_full$scotland30==1,1,0)

#remove unneeded variables

names(bcs_full)

unneeded_vars <- c("female","parscr","pared","nonbrit30","hhpayquintile",
                   "m_age","medu_age","fedu_age","crowd","chi_ten","lowbwt","q16score",
                   "eng10","maths10","eng16","maths16","vocab16","spell16","read16","matrix16",
                   "arith16","cog16","isced26","isced30","county16","county30","scotland16","scotland30")

bcs_cc_42 <- select(bcs_full, !all_of(unneeded_vars), unitype = unitype42, uniname = uniname42)

#specify mixed race marriage items

bcs_cc_42$racemarriage16 <- bcs_cc_42$c5p8r

bcs_cc_42$racemarriage30 <- bcs_cc_42$ar1

#specify death pen items

bcs_cc_42$deathpen16 <- bcs_cc_42$c5p14r

bcs_cc_42$deathpen30 <- bcs_cc_42$a2r

#drop composite scale items

val_comps <- c("d070r","d079r","d080r","d081r","d087r","d090","d094r","d097r",
               "d099r","d100r","d103r","d106r","d107r","c5p8r","c5p15","c5p1r","c5p3r",
               "c5p14r","c5p2","ar1","ar2","ar3","ar4","ar5r","a1r","a2r","a3r","a4r","a5r","a6r")

bcs_cc_42 <- bcs_cc_42 %>% 
  select(!all_of(val_comps))

#set degree/uni variables to 0 for non-grads at 30

bcs_cc_42 <- bcs_cc_42 %>% 
  mutate(unitype = case_when(deg30==0 & is.na(unitype) ~ 0, .default = unitype),
         uniname = case_when(deg30==0 & is.na(uniname) ~ 0, .default = uniname),
         degsub4 = case_when(deg30==0 & is.na(degsub4) ~ 0, .default = degsub4),
         degsub4_robust = case_when(deg30==0 & is.na(degsub4_robust) ~ 0, .default = degsub4_robust),
         degsub4_econ = case_when(deg30==0 & is.na(degsub4_econ) ~ 0, .default = degsub4_econ),
         deg16 = ifelse(!is.na(deg30),0,NA))

#reduce to complete cases

bcs_cc_42 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>% t()

bcs_cc_42 <- bcs_cc_42 %>%
  filter(complete.cases(.))

#save file

write_rds(bcs_cc_42, here("data/bcs_cc_42.rds"))

#####
#DESCRIPTIVES

bcs_cc_42 <- read_rds(here("data/bcs_cc_42.rds"))

load(file = here("data/bcs_imp_data.rda"))

long <- mice::complete(bcs_imp_data, "long", include = T)

#table 1

table(bcs_cc_42$degsub4)

#unique obs of uniname (less the 'none' category)

length(table(bcs_cc_42$uniname))-1

#count obs for figures

long %>%
  group_by(.imp) %>%
  summarise(length(bcsid)) %>% 
  print(Inf)

long_grads %>%
  group_by(.imp) %>%
  summarise(n = length(bcsid)) %>%
  summarise(min = min(n),
            max = max(n))

#basic descriptives for appendix

bcs_cc_42 %>%
  select(`Death penalty attitudes at 16` = deathpen16,
         `Death penalty attitudes at 30` = deathpen30,
         `Interracial marriage attitudes at 16` = racemarriage16,
         `Interracial marriage attitudes at 30` = racemarriage30,
         `Distance between counties at 16 and 30` = distance,
         `Population density of county at 16` = `popdens_81`,
         `% of county residents non-UK born at 16` = `prop_nonukborn_81`,
         `% of county residents with degrees or equivalent at 16` = `prop_deg_81`,
         `% of county residents working in agriculture at 16` = `prop_agri_81`,
         `Population density of county at 30` = `popdens_01`,
         `% of county residents non-UK born at 30` = `prop_nonukborn_01`,
         `% of county residents with degrees or equivalent at 30` = `prop_deg_01`,
         `% of county residents working in agriculture at 30` = `prop_agri_01`) %>%
  datasummary_skim(output = here("tables/descriptives.docx"), align = "c")

#missingness graph

bcs_full

bcs_full$racemarriage16 <- bcs_full$c5p8r

bcs_full$racemarriage30 <- bcs_full$ar1

bcs_full$deathpen16 <- bcs_full$c5p14r

bcs_full$deathpen30 <- bcs_full$a2r

bcs_full <- bcs_full %>% 
  mutate(uniname = case_when(deg30==0 & is.na(uniname42) ~ 0, .default = uniname42),
         degsub4 = case_when(deg30==0 & is.na(degsub4) ~ 0, .default = degsub4))

missing_pc <- bcs_full %>%
  select(deathpen16,racemarriage16,deathpen30,racemarriage30,degsub4,
         uniname,county16,county30) %>%
  gather("var") %>%
  group_by(var) %>%             
  mutate(`% Missing` = (sum(is.na(value))/n())*100) %>%
  select(-value) %>%
  unique() %>%
  ungroup()

missing_pc %>%
  mutate(Type = c(rep("Outcome",4),rep("Treatment",2),rep("Geographic",2)),
         Variable = case_when(var == "deathpen16" ~ "Death penalty\nattitudes at 16",
                              var == "deathpen30" ~ "Death penalty\nattitudes at 30",
                              var == "racemarriage16" ~ "Interracial marriage\nattitudes at 16",
                              var == "racemarriage30" ~ "Interracial marriage\nattitudes at 30",
                              var == "county16" ~ "County at 16",
                              var == "county30" ~ "County at 30",
                              var == "degsub4" ~ "Degree subject",
                              var == "uniname" ~ "HEI name")) %>%
  mutate(Variable = fct_reorder2(Variable, `% Missing`, Type)) %>%
  ggplot(mapping = aes(x=Variable,y=`% Missing`,fill=Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() + scale_fill_brewer(palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust= 1))

ggsave("figures/missingness.png", h = 12, w = 20, units = "cm", type="cairo", bg = "white")

#trajectory plots
#generate means and SDs by age

traj <- bcs_cc_42 %>%
  dplyr::select(degsub4,deathpen16,deathpen30,racemarriage16,racemarriage30) %>%
  mutate(across(deathpen16:racemarriage30, ~ scale(.)[,1])) %>%
  group_by(degsub4) %>%
  summarise(n = n(),
            deathpen16_mean = mean(deathpen16),
            deathpen16_sd = sd(deathpen16),
            deathpen30_mean = mean(deathpen30),
            deathpen30_sd = sd(deathpen30),
            racemarriage16_mean = mean(racemarriage16),
            racemarriage16_sd = sd(racemarriage16),
            racemarriage30_mean = mean(racemarriage30),
            racemarriage30_sd = sd(racemarriage30)) %>% 
  ungroup() %>%
  mutate(degsub4 = fct_relevel(case_when(degsub4 == 0 ~ "Non-graduate",
                                         degsub4 == 1 ~ "STEM",
                                         degsub4 == 2 ~ "Business and Law",
                                         degsub4 == 3 ~ "AHSS"),
                               "Non-graduate","STEM","Business and Law","AHSS"),
         deathpen16_se = deathpen16_sd / sqrt(n),
         deathpen16_lci = deathpen16_mean - qt(1 - (0.05 / 2), n - 1) * deathpen16_se,
         deathpen16_uci = deathpen16_mean + qt(1 - (0.05 / 2), n - 1) * deathpen16_se,
         deathpen30_se = deathpen30_sd / sqrt(n),
         deathpen30_lci = deathpen30_mean - qt(1 - (0.05 / 2), n - 1) * deathpen30_se,
         deathpen30_uci = deathpen30_mean + qt(1 - (0.05 / 2), n - 1) * deathpen30_se,
         racemarriage16_se = racemarriage16_sd / sqrt(n),
         racemarriage16_lci = racemarriage16_mean - qt(1 - (0.05 / 2), n - 1) * racemarriage16_se,
         racemarriage16_uci = racemarriage16_mean + qt(1 - (0.05 / 2), n - 1) * racemarriage16_se,
         racemarriage30_se = racemarriage30_sd / sqrt(n),
         racemarriage30_lci = racemarriage30_mean - qt(1 - (0.05 / 2), n - 1) * racemarriage30_se,
         racemarriage30_uci = racemarriage30_mean + qt(1 - (0.05 / 2), n - 1) * racemarriage30_se) %>%
  pivot_longer(
    cols = c(deathpen16_mean:racemarriage30_uci),
    names_to = c("value",".value"), 
    names_sep = "_",
    values_drop_na = T) %>%
  separate(value, into = c("Outcome", "Age"), sep = "(?<=[A-Za-z])(?=[0-9]{2}$)", convert = TRUE) %>%
  mutate(Outcome = case_when(Outcome == "deathpen" ~ "Death penalty support",
                      Outcome == "racemarriage" ~ "Interracial marriage opposition")) %>%
  drop_na()

traj_plot <- traj %>%
  ggplot(aes(x = Age, y = mean, ymin = lci, ymax = uci, group = degsub4, colour = degsub4)) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_line(aes(linetype = degsub4),position = position_dodge(width = 0.9)) +
  geom_errorbar(width = .1, position = position_dodge(width = 0.9)) + facet_wrap(facets = vars(Outcome)) +
  labs(x = "Age",
       y = "Mean",
       group = "Degree subject",
       colour = "Degree subject",
       linetype = "Degree subject")

traj_plot + theme_minimal() + scale_colour_brewer(palette = "Dark2")

ggsave("figures/traj_plot.png", h = 12, w = 20, units = "cm", type="cairo", bg = "white")

#####
#CONSTRUCTING FE MODELs

bcs_cc_42 <- read_rds("data/bcs_cc_42.rds")

#scale outcome and geographic variables

bcs_cc_42$deathpen16 <- scale(bcs_cc_42$deathpen16)[,1]

bcs_cc_42$deathpen30 <- scale(bcs_cc_42$deathpen30)[,1]

bcs_cc_42$racemarriage16 <- scale(bcs_cc_42$racemarriage16)[,1]

bcs_cc_42$racemarriage30 <- scale(bcs_cc_42$racemarriage30)[,1]

bcs_cc_42 <- bcs_cc_42 %>%
  mutate(across(distance:prop_agri_01, ~ scale(.)[,1]))

#rename distance variable

bcs_cc_42 <- bcs_cc_42 %>% 
  rename(distance30 = distance)

#rename county-level variables

bcs_cc_42 <- bcs_cc_42 %>% 
  rename_at(vars(popbase_81:prop_agri_81), function(x) str_replace_all(x,"_81","16")) %>% 
  rename_at(vars(popbase_01:prop_agri_01), function(x) str_replace_all(x,"_01","30")) %>% 
  rename_at(vars(prop_nonukborn16:prop_agri16), function(x) str_replace_all(x,"prop_","prop")) %>% 
  rename_at(vars(prop_nonukborn30:prop_agri30), function(x) str_replace_all(x,"prop_","prop"))

#align deg and uniname

table(bcs_cc_42$deg30,bcs_cc_42$uniname)

bcs_cc_42$uniname <- ifelse(bcs_cc_42$deg30==0,0,bcs_cc_42$uniname)

#make into long dataset

bcs_cc_42 <- bcs_cc_42 %>% 
  pivot_longer(
    cols = c(deg16,deg30,distance30:propagri30,racemarriage16:deathpen30),
    names_to = c(".value","age"), 
    names_pattern = "([[:alpha:]]+)([[:digit:]]+)")

#make variables factors

bcs_cc_42 <- bcs_cc_42 %>%
  mutate(stem = case_when(degsub4==1 ~ 1,
                          degsub4==2 ~ 0,
                          degsub4==3 ~ 0,
                          degsub4==4 ~ 0),
         bizlaw = case_when(degsub4==1 ~ 0,
                          degsub4==2 ~ 1,
                          degsub4==3 ~ 0,
                          degsub4==4 ~ 0),
         ahss = case_when(degsub4==1 ~ 0,
                            degsub4==2 ~ 0,
                            degsub4==3 ~ 1,
                            degsub4==4 ~ 0),
         stem_robust = case_when(degsub4_robust==1 ~ 1,
                                   degsub4_robust==2 ~ 0,
                                   degsub4_robust==3 ~ 0,
                                   degsub4_robust==4 ~ 0),
         bizlaw_robust = case_when(degsub4_robust==1 ~ 0,
                                   degsub4_robust==2 ~ 1,
                                   degsub4_robust==3 ~ 0,
                                   degsub4_robust==4 ~ 0),
         ahss_robust = case_when(degsub4_robust==1 ~ 0,
                                 degsub4_robust==2 ~ 0,
                                 degsub4_robust==3 ~ 1,
                                 degsub4_robust==4 ~ 0),
         unitype =  factor(case_when(unitype==0 ~ "No degree",
                                     unitype==1 ~ "Russell Group",
                                     unitype==2 ~ "Non-Russell Group pre-1992",
                                     unitype==3 ~ "Post-1992",
                                     unitype==5 ~ "Other"),
                           levels = c("No degree","Russell Group","Non-Russell Group pre-1992","Post-1992","Other")))

#code variables at 16 as 0

length(unique(bcs_cc_42$bcsid))

bcs_cc_42$degsub4[bcs_cc_42$age==16] <- 0
bcs_cc_42$degsub4_robust[bcs_cc_42$age==16] <- 0

bcs_cc_42$stem[bcs_cc_42$age==16] <- 0
bcs_cc_42$stem_robust[bcs_cc_42$age==16] <- 0
bcs_cc_42$bizlaw[bcs_cc_42$age==16] <- 0
bcs_cc_42$bizlaw_robust[bcs_cc_42$age==16] <- 0
bcs_cc_42$ahss[bcs_cc_42$age==16] <- 0
bcs_cc_42$ahss_robust[bcs_cc_42$age==16] <- 0
bcs_cc_42$distance[bcs_cc_42$age==16] <- 0

bcs_cc_42$uniname[bcs_cc_42$age==16] <- bcs_cc_42$uniname[bcs_cc_42$age==30]

bcs_cc_42$degsub4 <- as.factor(bcs_cc_42$degsub4)
bcs_cc_42$degsub4_robust <- as.factor(bcs_cc_42$degsub4_robust)

#fix distance at 16 to lowest value at 30

bcs_cc_42$distance[bcs_cc_42$age==16] <- min(bcs_cc_42$distance,na.rm = T)

#create grad subsample (as otherwise dropped due to missing data for treatment)

bcs_grads <- bcs_cc_42 %>%
  group_by(bcsid) %>%
  filter(1 %in% deg) %>%
  ungroup()

bcs_grads <- bcs_grads %>%
  mutate(across(stem:ahss, ~ as_factor(.)))

#feols models

#death penalty

deathpen_degsub_1 <- feols(deathpen ~ degsub4 + age | bcsid, data = bcs_cc_42)

deathpen_degsub_2 <- feols(deathpen ~ degsub4 + age | bcsid + uniname, data = bcs_cc_42)

deathpen_degsub_3 <- feols(deathpen ~ degsub4 + age + distance | bcsid + uniname, data = bcs_cc_42)

deathpen_degsub_4 <- feols(deathpen ~ degsub4 + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_cc_42)

etable(deathpen_degsub_1,deathpen_degsub_2,deathpen_degsub_3,deathpen_degsub_4)

#produce model summary

modlist <- list(deathpen_degsub_1,deathpen_degsub_2,deathpen_degsub_3,deathpen_degsub_4)

cm <- c('degsub41' = 'Degree subject: STEM',
        'degsub42' = 'Degree subject: Business and Law',
        'degsub43' = 'Degree subject: Arts, Humanities and Social Science',
        'degsub44' = 'Degree subject: Other',
        'age30' = 'Age',
        'distance' = 'Distance from home county',
        'popdens' = 'County change in population density',
        'propnonukborn' = 'County change in % non-UK born',
        'propdeg' = 'County change in % with degrees',
        'propagri' = 'County change in % working in agriculture')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("FE: bcsid", "FE: uniname", "vcov.type", "r.squared", "adj.r.squared", "nobs"), coef_map = cm, output = "tables/deathpen_cc.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#mixed race marriage

racemarriage_degsub_1 <- feols(racemarriage ~ degsub4 + age | bcsid, data = bcs_cc_42)

racemarriage_degsub_2 <- feols(racemarriage ~ degsub4 + age | bcsid + uniname, data = bcs_cc_42)

racemarriage_degsub_3 <- feols(racemarriage ~ degsub4 + age + distance | bcsid + uniname, data = bcs_cc_42)

racemarriage_degsub_4 <- feols(racemarriage ~ degsub4 + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_cc_42)

etable(racemarriage_degsub_1,racemarriage_degsub_2,racemarriage_degsub_3,racemarriage_degsub_4)

#produce model summary

modlist <- list(racemarriage_degsub_1,racemarriage_degsub_2,racemarriage_degsub_3,racemarriage_degsub_4)

cm <- c('degsub41' = 'Degree subject: STEM',
        'degsub42' = 'Degree subject: Business and Law',
        'degsub43' = 'Degree subject: Arts, Humanities and Social Science',
        'degsub44' = 'Degree subject: Other',
        'age30' = 'Age',
        'distance' = 'Distance from home county',
        'popdens' = 'County change in population density',
        'propnonukborn' = 'County change in % non-UK born',
        'propdeg' = 'County change in % with degrees',
        'propagri' = 'County change in % working in agriculture')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("FE: bcsid", "FE: uniname", "vcov.type", "r.squared", "adj.r.squared", "nobs"),
             coef_map = cm, output = "tables/racemarriage_cc.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#graduate models - comparing across subjects

#death penalty

#AHSS become more liberal, STEM less so

#subjects plus HEI and geog mobility

deathpen_ahss <- feols(deathpen ~ ahss + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_grads)

deathpen_stem <- feols(deathpen ~ stem + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_grads)

deathpen_bizlaw <- feols(deathpen ~ bizlaw + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_grads)

#mixed race marriage

#no relationship on this by subject

#subjects plus HEI and geog mobility

racemarriage_ahss <- feols(racemarriage ~ ahss + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_grads)

racemarriage_stem <- feols(racemarriage ~ stem + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_grads)

racemarriage_bizlaw <- feols(racemarriage ~ bizlaw + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_grads)

#produce model summary

etable(deathpen_ahss,deathpen_stem,deathpen_bizlaw,racemarriage_ahss,racemarriage_stem,racemarriage_bizlaw)

modlist <- list(deathpen_ahss,deathpen_stem,deathpen_bizlaw,racemarriage_ahss,racemarriage_stem,racemarriage_bizlaw)

cm <- c('ahss1' = 'Degree subject: Arts, Humanities and Social Science',
        'stem1' = 'Degree subject: STEM',
        'bizlaw1' = 'Degree subject: Business and Law',
        'age30' = 'Age',
        'distance' = 'Distance from home county',
        'popdens' = 'County change in population density',
        'propnonukborn' = 'County change in % non-UK born',
        'propdeg' = 'County change in % with degrees',
        'propagri' = 'County change in % working in agriculture')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("FE: bcsid", "FE: uniname", "vcov.type", "r.squared", "adj.r.squared", "nobs"), coef_map = cm, output = "tables/grads_fullresults.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#Geographic effects

#death penalty

deathpen_distance_1 <- feols(deathpen ~ distance + age | bcsid, data = bcs_cc_42)

deathpen_distance_2 <- feols(deathpen ~ distance + age + popdens + propnonukborn + propdeg + propagri | bcsid, data = bcs_cc_42)

etable(deathpen_distance_1,deathpen_distance_2)

#mixed race marriage

racemarriage_distance_1 <- feols(racemarriage ~ distance + age | bcsid, data = bcs_cc_42)

racemarriage_distance_2 <- feols(racemarriage ~ distance + age + popdens + propnonukborn + propdeg + propagri | bcsid, data = bcs_cc_42)

etable(racemarriage_distance_1,racemarriage_distance_2)

#produce model summary

modlist <- list(deathpen_distance_1,deathpen_distance_2,racemarriage_distance_1,racemarriage_distance_2)

cm <- c('distance' = 'Distance from home county',
        'age30' = 'Age',
        'popdens' = 'County change in population density',
        'propnonukborn' = 'County change in % non-UK born',
        'propdeg' = 'County change in % with degrees',
        'propagri' = 'County change in % working in agriculture')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("FE: bcsid", "FE: uniname", "vcov.type", "r.squared", "adj.r.squared", "nobs"), coef_map = cm,
             output = "tables/mobility.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#####
#MI ANALYSIS

load(file = here("data/bcs_imp_data.rda"))

#create imputed dataset for analysis

long <- mice::complete(bcs_imp_data, "long", include = T)

#Drop auxiliary variables

mi_vars <- c("m_age","medu_age","fedu_age","crowd","chi_ten","lowbwt","q16score","eng16",
             "maths16","cog16")

long <- select(long, !all_of(mi_vars))

#scale outcome and geographic variables

long$deathpen16 <- scale(long$deathpen16)[,1]

long$deathpen30 <- scale(long$deathpen30)[,1]

long$racemarriage16 <- scale(long$racemarriage16)[,1]

long$racemarriage30 <- scale(long$racemarriage30)[,1]

long <- long %>%
  mutate(across(distance:prop_agri_01, ~ scale(.)[,1]))

#rename variables

long <- long %>% 
  rename(distance30 = distance,
         uniname = uniname42)

#rename county-level variables

long <- long %>% 
  rename_at(vars(popdens_81:prop_agri_81), function(x) str_replace_all(x,"_81","16")) %>% 
  rename_at(vars(popdens_01:prop_agri_01), function(x) str_replace_all(x,"_01","30")) %>% 
  rename_at(vars(prop_nonukborn16:prop_agri16), function(x) str_replace_all(x,"prop_","prop")) %>% 
  rename_at(vars(prop_nonukborn30:prop_agri30), function(x) str_replace_all(x,"prop_","prop"))

#make into long dataset

long <- long %>% 
  pivot_longer(
    cols = c(distance30:propagri30,racemarriage16:deathpen30),
    names_to = c(".value","age"), 
    names_pattern = "([[:alpha:]]+)([[:digit:]]+)")

long

#make variables

long <- long %>%
  mutate(stem = case_when(degsub4==1 ~ 1,
                          degsub4==2 ~ 0,
                          degsub4==3 ~ 0,
                          degsub4==4 ~ 0),
         bizlaw = case_when(degsub4==1 ~ 0,
                            degsub4==2 ~ 1,
                            degsub4==3 ~ 0,
                            degsub4==4 ~ 0),
         ahss = case_when(degsub4==1 ~ 0,
                          degsub4==2 ~ 0,
                          degsub4==3 ~ 1,
                          degsub4==4 ~ 0))

#align degsub and unitype variables

long %>%
  filter(.imp!=0) %>%
  group_by(degsub4) %>%
  count(uniname)

long %>%
  group_by(.imp) %>%
  count(degsub4)

long %>%
  group_by(.imp) %>%
  count(uniname)
  
table(long$degsub4,long$uniname)

long$uniname <- ifelse(long$degsub4==0,0,long$uniname)

long$uniname[long$age==16] <- long$uniname[long$age==30]

long <- long %>%
  mutate(uniname =  factor(uniname))

long %>%
  filter(.imp!=0) %>%
  select(bcsid,age,uniname,degsub4) %>%
  print(n=1000)

#code variables at 16 as 0

long %>%
  select(`.imp`,bcsid,age,uniname,degsub4) %>% print(n=1000)

long$degsub4[long$age==16] <- 0
long$stem[long$age==16] <- 0
long$bizlaw[long$age==16] <- 0
long$ahss[long$age==16] <- 0

long <- long %>%
  mutate(across(stem:ahss, ~ as_factor(.)))

long$hhpayquintile <- factor(long$hhpayquintile, levels = c("0",levels(long$hhpayquintile)))
long$hhpayquintile[long$age==16] <- 0

long$distance[long$age==16] <- min(long$distance,na.rm = T)

#remove unimputed data

long <- long %>% filter(`.imp`!="0")

#create grads subset

as.numeric(long$degsub4)

long_grads <- long %>%
  group_by(.imp,bcsid) %>%
  filter(sum(as.numeric(degsub4)-1)>0) %>%
  ungroup()

### Analyze the imputed datasets and pool the results. 
#Death penalty

deathpen_mi_1 <- lapply(1:100, function(i) {
  feols(deathpen ~ degsub4 + age | bcsid, data = long[long$`.imp`==i,])
})

deathpen_mi_1 <- mice::pool(deathpen_mi_1)

deathpen_mi_2 <- lapply(1:100, function(i) {
  feols(deathpen ~ degsub4 + age | bcsid + uniname, data = long[long$`.imp`==i,])
})

deathpen_mi_2 <- mice::pool(deathpen_mi_2)

deathpen_mi_3 <- lapply(1:100, function(i) {
  feols(deathpen ~ degsub4 + age + distance | bcsid + uniname, data = long[long$`.imp`==i,])
})

deathpen_mi_3 <- mice::pool(deathpen_mi_3)

deathpen_mi_4 <- lapply(1:100, function(i) {
  feols(deathpen ~ degsub4 + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = long[long$`.imp`==i,])
})

deathpen_mi_4 <- mice::pool(deathpen_mi_4)

#produce model summary

modlist <- list(deathpen_mi_1,deathpen_mi_2,deathpen_mi_3,deathpen_mi_4)

cm <- c('degsub41' = 'Degree subject: STEM',
        'degsub42' = 'Degree subject: Business and Law',
        'degsub43' = 'Degree subject: Arts, Humanities and Social Science',
        'degsub44' = 'Degree subject: Other',
        'age30' = 'Age',
        'distance' = 'Distance from home county',
        'popdens' = 'County change in population density',
        'propnonukborn' = 'County change in % non-UK born',
        'propdeg' = 'County change in % with degrees',
        'propagri' = 'County change in % working in agriculture')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("FE: bcsid", "FE: uniname", "vcov.type", "r.squared", "adj.r.squared", "nobs"),
             coef_map = cm, output = "tables/deathpen_mi.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#Mixed race marriage

racemarriage_mi_1 <- lapply(1:100, function(i) {
  feols(racemarriage ~ degsub4 + age | bcsid, data = long[long$`.imp`==i,])
})

racemarriage_mi_1 <- mice::pool(racemarriage_mi_1)

racemarriage_mi_2 <- lapply(1:100, function(i) {
  feols(racemarriage ~ degsub4 + age | bcsid + uniname, data = long[long$`.imp`==i,])
})

racemarriage_mi_2 <- mice::pool(racemarriage_mi_2)

racemarriage_mi_3 <- lapply(1:100, function(i) {
  feols(racemarriage ~ degsub4 + age + distance | bcsid + uniname, data = long[long$`.imp`==i,])
})

racemarriage_mi_3 <- mice::pool(racemarriage_mi_3)

racemarriage_mi_4 <- lapply(1:100, function(i) {
  feols(racemarriage ~ degsub4 + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = long[long$`.imp`==i,])
})

racemarriage_mi_4 <- mice::pool(racemarriage_mi_4)

#produce model summary

modlist <- list(racemarriage_mi_1,racemarriage_mi_2,racemarriage_mi_3,racemarriage_mi_4)

cm <- c('degsub41' = 'Degree subject: STEM',
        'degsub42' = 'Degree subject: Business and Law',
        'degsub43' = 'Degree subject: Arts, Humanities and Social Science',
        'degsub44' = 'Degree subject: Other',
        'age30' = 'Age',
        'distance' = 'Distance from home county',
        'popdens' = 'County change in population density',
        'propnonukborn' = 'County change in % non-UK born',
        'propdeg' = 'County change in % with degrees',
        'propagri' = 'County change in % working in agriculture')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("FE: bcsid", "FE: uniname", "vcov.type", "r.squared", "adj.r.squared", "nobs"),
             coef_map = cm, output = "tables/racemarriage_mi.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#graduate models - comparing across subjects

#death penalty

deathpen_mi_ahss <- lapply(1:100, function(i) {
  feols(deathpen ~ ahss + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname,
        data = long_grads[long_grads$`.imp`==i,])
})

deathpen_mi_ahss <- mice::pool(deathpen_mi_ahss)

summary(deathpen_mi_ahss)

deathpen_mi_stem <- lapply(1:100, function(i) {
  feols(deathpen ~ stem + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname,
        data = long_grads[long_grads$`.imp`==i,])
})

deathpen_mi_stem <- mice::pool(deathpen_mi_stem)

deathpen_mi_bizlaw <- lapply(1:100, function(i) {
  feols(deathpen ~ bizlaw + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname,
        data = long_grads[long_grads$`.imp`==i,])
})

deathpen_mi_bizlaw <- mice::pool(deathpen_mi_bizlaw)

#mixed race marriage

racemarriage_mi_ahss <- lapply(1:100, function(i) {
  feols(racemarriage ~ ahss + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname,
        data = long_grads[long_grads$`.imp`==i,])
})

racemarriage_mi_ahss <- mice::pool(racemarriage_mi_ahss)

summary(racemarriage_mi_ahss)

racemarriage_mi_stem <- lapply(1:100, function(i) {
  feols(racemarriage ~ stem + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname,
        data = long_grads[long_grads$`.imp`==i,])
})

racemarriage_mi_stem <- mice::pool(racemarriage_mi_stem)

racemarriage_mi_bizlaw <- lapply(1:100, function(i) {
  feols(racemarriage ~ bizlaw + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname,
        data = long_grads[long_grads$`.imp`==i,])
})

racemarriage_mi_bizlaw <- mice::pool(racemarriage_mi_bizlaw)

#produce model summary

modlist <- list(deathpen_mi_ahss,deathpen_mi_stem,deathpen_mi_bizlaw,racemarriage_mi_ahss,racemarriage_mi_stem,racemarriage_mi_bizlaw)

cm <- c('ahss1' = 'Degree subject: Arts, Humanities and Social Science',
        'stem1' = 'Degree subject: STEM',
        'bizlaw1' = 'Degree subject: Business and Law',
        'age30' = 'Age',
        'distance' = 'Distance from home county',
        'popdens' = 'County change in population density',
        'propnonukborn' = 'County change in % non-UK born',
        'propdeg' = 'County change in % with degrees',
        'propagri' = 'County change in % working in agriculture')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("FE: bcsid", "FE: uniname", "vcov.type", "r.squared", "adj.r.squared", "nobs"),
             coef_map = cm, output = "tables/grads_mi_results.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#Geographic

#Death penalty

deathpen_geog_mi_1 <- lapply(1:100, function(i) {
  feols(deathpen ~ distance + age | bcsid, data = long[long$`.imp`==i,])
})

deathpen_geog_mi_1 <- mice::pool(deathpen_geog_mi_1)

deathpen_geog_mi_2 <- lapply(1:100, function(i) {
  feols(deathpen ~ distance + age + popdens + propnonukborn + propdeg + propagri | bcsid, data = long[long$`.imp`==i,])
})

deathpen_geog_mi_2 <- mice::pool(deathpen_geog_mi_2)

#Interracial marriage

racemarriage_geog_mi_1 <- lapply(1:100, function(i) {
  feols(racemarriage ~ distance + age | bcsid, data = long[long$`.imp`==i,])
})

racemarriage_geog_mi_1 <- mice::pool(racemarriage_geog_mi_1)

racemarriage_geog_mi_2 <- lapply(1:100, function(i) {
  feols(racemarriage ~ distance + age + popdens + propnonukborn + propdeg + propagri | bcsid, data = long[long$`.imp`==i,])
})

racemarriage_geog_mi_2 <- mice::pool(racemarriage_geog_mi_2)

#produce model summary

modlist <- list(deathpen_geog_mi_1,deathpen_geog_mi_2,racemarriage_geog_mi_1,racemarriage_geog_mi_2)

cm <- c('distance' = 'Distance from home county',
        'popdens' = 'County change in population density',
        'propnonukborn' = 'County change in % non-UK born',
        'propdeg' = 'County change in % with degrees',
        'propagri' = 'County change in % working in agriculture',
        'age30' = 'Age')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("FE: bcsid", "FE: uniname", "vcov.type", "r.squared", "adj.r.squared", "nobs"),
             coef_map = cm, output = "tables/geog_mi.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#####
#FIGURES

#main results

cm <- c('degsub41' = 'Degree subject:\nSTEM',
        'degsub42' = 'Degree subject:\nBusiness and Law',
        'degsub43' = 'Degree subject:\nArts, Humanities\nand Social Science',
        'age30' = 'Age',
        'distance' = 'Distance from\nhome county',
        'popdens' = 'Population density',
        'propnonukborn' = '% non-UK born',
        'propdeg' = '% with degrees',
        'propagri' = '% working in\nagriculture')

modlist <- list('Complete cases'=deathpen_degsub_4,'Multiply imputed'=deathpen_mi_4)

deathpen_fig1 <- modelplot(modlist, coef_map = rev(cm)) + geom_vline(xintercept=0, linetype=2, alpha=0.2) +
  labs(title = "A. Support death penalty", colour = "", x = "Coefficient in SDs") + scale_color_brewer(palette = "Dark2")

modlist <- list('Complete cases'=racemarriage_degsub_4,'Multiply imputed'=racemarriage_mi_4)

racemarriage_fig1 <- modelplot(modlist, coef_map = rev(cm)) + geom_vline(xintercept=0, linetype=2, alpha=0.2) +
  labs(title = "B. Oppose interracial marriage", colour = "", x = "Coefficient in SDs") + scale_color_brewer(palette = "Dark2")
  
deathpen_fig1 + racemarriage_fig1 + plot_layout(guides = "collect", axes = "collect") & theme_minimal()

ggsave("figures/fig1.png", h = 12, w = 20, units = "cm", type="cairo", bg = "white")

#plot predictions

deathpen_preds <- predictions(deathpen_degsub_4) %>%
  as_tibble() %>%
  group_by(bcsid) %>%
  mutate(degsub4 = sum(as.numeric(degsub4)-1)) %>%
  ungroup() %>%
  group_by(age,degsub4) %>%
  summarise(estimate = mean(estimate),
            std.error = mean(std.error),
            conf.low = mean(conf.low),
            conf.high = mean(conf.high)) %>%
  ungroup() %>%
  mutate(Outcome = "A. Support death penalty",
         degsub4 = fct_relevel(case_when(degsub4 == 0 ~ "Non-graduate",
                                         degsub4 == 1 ~ "STEM",
                                         degsub4 == 2 ~ "Business and Law",
                                         degsub4 == 3 ~ "AHSS"),
                               "Non-graduate","STEM","Business and Law","AHSS")) %>%
  drop_na()

racemarriage_preds <- predictions(racemarriage_degsub_4) %>%
  as_tibble() %>%
  group_by(bcsid) %>%
  mutate(degsub4 = sum(as.numeric(degsub4)-1)) %>%
  ungroup() %>%
  group_by(age,degsub4) %>%
  summarise(estimate = mean(estimate),
            std.error = mean(std.error),
            conf.low = mean(conf.low),
            conf.high = mean(conf.high)) %>%
  ungroup() %>%
  mutate(Outcome = "B. Oppose interracial marriage",
         degsub4 = fct_relevel(case_when(degsub4 == 0 ~ "Non-graduate",
                                         degsub4 == 1 ~ "STEM",
                                         degsub4 == 2 ~ "Business and Law",
                                         degsub4 == 3 ~ "AHSS"),
                               "Non-graduate","STEM","Business and Law","AHSS")) %>%
  drop_na()

pred_traj_plot <- bind_rows(deathpen_preds,racemarriage_preds) %>%
  ggplot(aes(x = age, y = estimate, ymin = conf.low, ymax = conf.high, group = degsub4, colour = degsub4)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_line(aes(linetype = degsub4),position = position_dodge(width = 0.3)) +
  geom_errorbar(width = .1, position = position_dodge(width = 0.3)) + facet_wrap(facets = vars(Outcome)) +
  labs(x = "Age",
       y = "Mean",
       group = "Degree subject",
       colour = "Degree subject",
       linetype = "Degree subject")

pred_traj_plot + theme_minimal() + scale_colour_brewer(palette = "Dark2")

ggsave("figures/fig2.png", h = 12, w = 20, units = "cm", type="cairo", bg = "white")

#grad only plots

cm <- c('stem1' = 'Degree subject:\nSTEM',
        'bizlaw1' = 'Degree subject:\nBusiness and Law',
        'ahss1' = 'Degree subject:\nArts, Humanities\nand Social Science')

modlist <- list('Complete cases'=deathpen_ahss,'Multiply imputed'=deathpen_mi_ahss,
                'Complete cases'=deathpen_stem,'Multiply imputed'=deathpen_mi_stem,
                'Complete cases'=deathpen_bizlaw,'Multiply imputed'=deathpen_mi_bizlaw)

deathpen_fig3 <- modelplot(modlist, coef_map = rev(cm), draw = F) %>%
  mutate(model = gsub("  | $","",model)) %>%
  ggplot(mapping = aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high, color = model)) +
  geom_pointrange(position = position_dodge(width = .5)) + geom_vline(xintercept=0, linetype=2, alpha=0.2) +
  labs(title = "A. Support death penalty", colour = "", y = "", x = "Coefficient in SDs") + scale_color_brewer(palette = "Dark2")

modlist <- list('Complete cases'=racemarriage_ahss,'Multiply imputed'=racemarriage_mi_ahss,
                'Complete cases'=racemarriage_stem,'Multiply imputed'=racemarriage_mi_stem,
                'Complete cases'=racemarriage_bizlaw,'Multiply imputed'=racemarriage_mi_bizlaw)

racemarriage_fig3 <- modelplot(modlist, coef_map = rev(cm), draw = F) %>%
  mutate(model = gsub("  | $","",model)) %>%
  ggplot(mapping = aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high, color = model)) +
  geom_pointrange(position = position_dodge(width = .5)) + geom_vline(xintercept=0, linetype=2, alpha=0.2) +
  labs(title = "B. Oppose interracial marriage", colour = "", y = "", x = "Coefficient in SDs") + scale_color_brewer(palette = "Dark2")

deathpen_fig3 + racemarriage_fig3 + plot_layout(guides = "collect", axes = "collect") & theme_minimal()

ggsave("figures/fig3.png", h = 12, w = 20, units = "cm", type="cairo", bg = "white")

#geographic results

cm <- c('distance' = 'Distance from\nhome county',
        'popdens' = 'Population density',
        'propnonukborn' = '% non-UK born',
        'propdeg' = '% with degrees',
        'propagri' = '% working in\nagriculture',
        'age30' = 'Age')

modlist <- list('Complete cases'=deathpen_distance_2,'Multiply imputed'=deathpen_geog_mi_2)

deathpen_fig4 <- modelplot(modlist, coef_map = rev(cm)) + geom_vline(xintercept=0, linetype=2, alpha=0.2) +
  labs(title = "A. Support death penalty", colour = "", x = "Coefficient in SDs") + scale_color_brewer(palette = "Dark2")

modlist <- list('Complete cases'=racemarriage_distance_2,'Multiply imputed'=racemarriage_geog_mi_2)

racemarriage_fig4 <- modelplot(modlist, coef_map = rev(cm)) + geom_vline(xintercept=0, linetype=2, alpha=0.2) +
  labs(title = "B. Oppose interracial marriage", colour = "", x = "Coefficient in SDs") + scale_color_brewer(palette = "Dark2")

deathpen_fig4 + racemarriage_fig4 + plot_layout(guides = "collect", axes = "collect") & theme_minimal()

ggsave("figures/fig4.png", h = 12, w = 20, units = "cm", type="cairo", bg = "white")

#####
#ROBUSTNESS

#TREATMENT

#1. Alternative degree measure for joint honours

#graduate models - comparing across subjects

#death penalty

deathpen_ahss_robust <- feols(deathpen ~ ahss_robust + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_grads)

deathpen_stem_robust <- feols(deathpen ~ stem_robust + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_grads)

deathpen_bizlaw_robust <- feols(deathpen ~ bizlaw_robust + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_grads)

#mixed race marriage

racemarriage_ahss_robust <- feols(racemarriage ~ ahss_robust + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_grads)

racemarriage_stem_robust <- feols(racemarriage ~ stem_robust + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_grads)

racemarriage_bizlaw_robust <- feols(racemarriage ~ bizlaw_robust + age + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_grads)

#produce model summary

etable(deathpen_ahss_robust,deathpen_stem_robust,deathpen_bizlaw_robust,racemarriage_ahss_robust,racemarriage_stem_robust,racemarriage_bizlaw_robust)

modlist <- list(deathpen_ahss_robust,deathpen_stem_robust,deathpen_bizlaw_robust,racemarriage_ahss_robust,racemarriage_stem_robust,racemarriage_bizlaw_robust)

cm <- c('ahss_robust' = 'Alternative degree subject: Arts, Humanities and Social Science',
        'stem_robust' = 'Alternative degree subject: STEM',
        'bizlaw_robust' = 'Alternative degree subject: Business and Law',
        'age30' = 'Age',
        'distance' = 'Distance from home county',
        'popdens' = 'County change in population density',
        'propnonukborn' = 'County change in % non-UK born',
        'propdeg' = 'County change in % with degrees',
        'propagri' = 'County change in % working in agriculture')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("FE: bcsid", "FE: uniname", "vcov.type", "r.squared", "adj.r.squared", "nobs"), coef_map = cm, output = "tables/grads_robust.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#COVARIATES

#2. Adding household pay

bcs_hhpay <- read_rds(here("data","bcs_full.rds"))

#specify mixed race marriage items

bcs_hhpay$racemarriage16 <- bcs_hhpay$c5p8r

bcs_hhpay$racemarriage30 <- bcs_hhpay$ar1

#specify death pen items

bcs_hhpay$deathpen16 <- bcs_hhpay$c5p14r

bcs_hhpay$deathpen30 <- bcs_hhpay$a2r

#remove unneeded variables

unneeded_vars <- c("female","parscr","pared","nonbrit30",
                   "m_age","medu_age","fedu_age","crowd","chi_ten","lowbwt","q16score",
                   "eng10","maths10","eng16","maths16","vocab16","spell16","read16","matrix16",
                   "arith16","cog16","isced26","isced30","county16","county30",
                   "d070r","d079r","d080r","d081r","d087r","d090","d094r","d097r",
                   "d099r","d100r","d103r","d106r","d107r","c5p8r","c5p15","c5p1r","c5p3r",
                   "c5p14r","c5p2","ar1","ar2","ar3","ar4","ar5r","a1r","a2r","a3r","a4r","a5r","a6r")

bcs_hhpay <- select(bcs_hhpay, !all_of(unneeded_vars), unitype = unitype42, uniname = uniname42)

#set degree/uni variables to 0 for non-grads at 30

bcs_hhpay <- bcs_hhpay %>% 
  mutate(unitype = case_when(deg30==0 & is.na(unitype) ~ 0, .default = unitype),
         uniname = case_when(deg30==0 & is.na(uniname) ~ 0, .default = uniname),
         degsub4 = case_when(deg30==0 & is.na(degsub4) ~ 0, .default = degsub4),
         degsub4_robust = case_when(deg30==0 & is.na(degsub4_robust) ~ 0, .default = degsub4_robust),
         degsub4_econ = case_when(deg30==0 & is.na(degsub4_econ) ~ 0, .default = degsub4_econ),
         deg16 = ifelse(!is.na(deg30),0,NA))

#reduce to complete cases

bcs_hhpay %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>% t()

bcs_hhpay <- bcs_hhpay %>%
  filter(complete.cases(.))

#scale outcome and geographic variables

bcs_hhpay$deathpen16 <- scale(bcs_hhpay$deathpen16)[,1]

bcs_hhpay$deathpen30 <- scale(bcs_hhpay$deathpen30)[,1]

bcs_hhpay$racemarriage16 <- scale(bcs_hhpay$racemarriage16)[,1]

bcs_hhpay$racemarriage30 <- scale(bcs_hhpay$racemarriage30)[,1]

bcs_hhpay <- bcs_hhpay %>%
  mutate(across(distance:prop_agri_01, ~ scale(.)[,1]))

#rename distance variable

bcs_hhpay <- bcs_hhpay %>% 
  rename(distance30 = distance)

#rename county-level variables

bcs_hhpay <- bcs_hhpay %>% 
  rename_at(vars(popbase_81:prop_agri_81), function(x) str_replace_all(x,"_81","16")) %>% 
  rename_at(vars(popbase_01:prop_agri_01), function(x) str_replace_all(x,"_01","30")) %>% 
  rename_at(vars(prop_nonukborn16:prop_agri16), function(x) str_replace_all(x,"prop_","prop")) %>% 
  rename_at(vars(prop_nonukborn30:prop_agri30), function(x) str_replace_all(x,"prop_","prop"))

#align deg and uniname

table(bcs_hhpay$deg30,bcs_hhpay$uniname)

bcs_hhpay$uniname <- ifelse(bcs_hhpay$deg30==0,0,bcs_hhpay$uniname)

#make into long dataset

bcs_hhpay <- bcs_hhpay %>% 
  pivot_longer(
    cols = c(deg16,deg30,distance30:propagri30,racemarriage16:deathpen30),
    names_to = c(".value","age"), 
    names_pattern = "([[:alpha:]]+)([[:digit:]]+)")

#code variables at 16 as 0

bcs_hhpay$degsub4[bcs_hhpay$age==16] <- 0
bcs_hhpay$hhpayquintile[bcs_hhpay$age==16] <- 0
bcs_hhpay$distance[bcs_hhpay$age==16] <- 0

bcs_hhpay$uniname[bcs_hhpay$age==16] <- bcs_hhpay$uniname[bcs_hhpay$age==30]

bcs_hhpay$degsub4 <- as.factor(bcs_hhpay$degsub4)
bcs_hhpay$hhpayquintile <- as.factor(bcs_hhpay$hhpayquintile)

#fix distance at 16 to lowest value at 30

bcs_hhpay$distance[bcs_hhpay$age==16] <- min(bcs_hhpay$distance,na.rm = T)

#feols models

#death penalty

deathpen_hhpay_1 <- feols(deathpen ~ age + hhpayquintile | bcsid, data = bcs_hhpay)

deathpen_hhpay_2 <- feols(deathpen ~ age + hhpayquintile + degsub4 | bcsid + uniname, data = bcs_hhpay)

deathpen_hhpay_3 <- feols(deathpen ~ age + hhpayquintile + degsub4 + distance | bcsid + uniname, data = bcs_hhpay)

deathpen_hhpay_4 <- feols(deathpen ~ age + hhpayquintile + degsub4 + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_hhpay)

etable(deathpen_hhpay_1,deathpen_hhpay_2,deathpen_hhpay_3,deathpen_hhpay_4)

#produce model summary

modlist <- list(deathpen_hhpay_1,deathpen_hhpay_2,deathpen_hhpay_3,deathpen_hhpay_4)

cm <- c('hhpayquintile1' = 'Income quintile 1',
        'hhpayquintile2' = 'Income quintile 2',
        'hhpayquintile3' = 'Income quintile 3',
        'hhpayquintile4' = 'Income quintile 4',
        'degsub41' = 'Degree subject: STEM',
        'degsub42' = 'Degree subject: Business and Law',
        'degsub43' = 'Degree subject: Arts, Humanities and Social Science',
        'degsub44' = 'Degree subject: Other',
        'distance' = 'Distance from home county',
        'popdens' = 'County change in population density',
        'propnonukborn' = 'County change in % non-UK born',
        'propdeg' = 'County change in % with degrees',
        'propagri' = 'County change in % working in agriculture')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("vcov.type", "adj.r.squared", "nobs"), coef_map = cm, output = "tables/hhpay_deathpen.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#mixed race marriage

racemarriage_hhpay_1 <- feols(racemarriage ~ age + hhpayquintile | bcsid, data = bcs_hhpay)

racemarriage_hhpay_2 <- feols(racemarriage ~ age + hhpayquintile + degsub4 | bcsid + uniname, data = bcs_hhpay)

racemarriage_hhpay_3 <- feols(racemarriage ~ age + hhpayquintile + degsub4 + distance | bcsid + uniname, data = bcs_hhpay)

racemarriage_hhpay_4 <- feols(racemarriage ~ age + hhpayquintile + degsub4 + distance + popdens + propnonukborn + propdeg + propagri | bcsid + uniname, data = bcs_hhpay)

etable(racemarriage_hhpay_1,racemarriage_hhpay_2,racemarriage_hhpay_3,racemarriage_hhpay_4)

#produce model summary

modlist <- list(racemarriage_hhpay_1,racemarriage_hhpay_2,racemarriage_hhpay_3,racemarriage_hhpay_4)

cm <- c('hhpayquintile1' = 'Income quintile 1',
        'hhpayquintile2' = 'Income quintile 2',
        'hhpayquintile3' = 'Income quintile 3',
        'hhpayquintile4' = 'Income quintile 4',
        'degsub41' = 'Degree subject: STEM',
        'degsub42' = 'Degree subject: Business and Law',
        'degsub43' = 'Degree subject: Arts, Humanities and Social Science',
        'degsub44' = 'Degree subject: Other',
        'distance' = 'Distance from home county',
        'popdens' = 'County change in population density',
        'propnonukborn' = 'County change in % non-UK born',
        'propdeg' = 'County change in % with degrees',
        'propagri' = 'County change in % working in agriculture')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("vcov.type", "adj.r.squared", "nobs"), coef_map = cm, output = "tables/hhpay_racemarriage.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))

#3. Mobility interaction with Scotland

#death penalty

deathpen_scotdistance_1 <- feols(deathpen ~ distance*everscot + age | bcsid, data = bcs_cc_42)

deathpen_scotdistance_2 <- feols(deathpen ~ distance*everscot + age + popdens + propnonukborn + propdeg + propagri | bcsid, data = bcs_cc_42)

deathpen_scotdistance_3 <- feols(deathpen ~ degsub4 + distance*everscot + age + popdens + propnonukborn + propdeg + propagri | bcsid, data = bcs_cc_42)

etable(deathpen_scotdistance_1,deathpen_scotdistance_2,deathpen_scotdistance_3)

#mixed race marriage

racemarriage_scotdistance_1 <- feols(racemarriage ~ distance*everscot + age | bcsid, data = bcs_cc_42)

racemarriage_scotdistance_2 <- feols(racemarriage ~ distance*everscot + age + popdens + propnonukborn + propdeg + propagri | bcsid, data = bcs_cc_42)

racemarriage_scotdistance_3 <- feols(racemarriage ~ degsub4 + distance*everscot + age + popdens + propnonukborn + propdeg + propagri | bcsid, data = bcs_cc_42)

etable(racemarriage_scotdistance_1,racemarriage_scotdistance_2,racemarriage_scotdistance_3)

#produce model summary

modlist <- list(deathpen_scotdistance_1,deathpen_scotdistance_2,deathpen_scotdistance_3,
                racemarriage_scotdistance_1,racemarriage_scotdistance_2,racemarriage_scotdistance_3)

cm <- c('distance' = 'Distance from home county',
        'distance:everscot' = 'Distance x Scotland',
        'age30' = 'Age',
        'popdens' = 'County change in population density',
        'propnonukborn' = 'County change in % non-UK born',
        'propdeg' = 'County change in % with degrees',
        'propagri' = 'County change in % working in agriculture',
        'degsub41' = 'Degree subject: STEM',
        'degsub42' = 'Degree subject: Business and Law',
        'degsub43' = 'Degree subject: Arts, Humanities and Social Science',
        'degsub44' = 'Degree subject: Other')

modelsummary(modlist, estimate = "{estimate}{stars}", statistic = "std.error", conf_level = 0.95,
             gof_map = c("FE: bcsid", "FE: uniname", "vcov.type", "r.squared", "adj.r.squared", "nobs"), coef_map = cm,
             output = "tables/mobility_scotland.docx",
             stars = c('*' = .05, '**' = .01, '***' = .001))
