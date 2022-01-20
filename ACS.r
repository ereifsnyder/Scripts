#######################################################################
###### Pulling census data for neighborhood-scale Displacement Measure variables ######
#######################################################################

### Loading required packages and setting defaults
if(!require(pacman)) install.packages("pacman")
p_load(tidyr, dplyr, tigris, tidycensus, yaml, sf, stringr, fst,  googledrive, bit64,magrittr, fs, data.table, tidyverse, spdep)
options(tigris_use_cache = TRUE,
         tigris_class = "sf")
if(!require(lehdr)){devtools::install_github("jamgreen/lehdr")}else{library(lehdr)}
#setwd("Git/hprm")
select <- dplyr::select


# options(width = Sys.getenv('COLUMNS'))

### Set API key
census_api_key('4c26aa6ebbaef54a55d3903212eabbb506ade381') #enter your own key here

#######################################################################
###### 2019 Variables  ################################################
#######################################################################



race_vars_19 <- c(
  "population"= "DP05_0001",
  "adult_pop" = "DP05_0021",
  "med_age" = "B01002_001",
  "white" = "B03002_003",
  "black" = "B03002_004",
  "amind" = "B03002_005",
  "asian" = "B03002_006",
  "pacis" = "B03002_007",
  "other" = "B03002_008",
  "race2" = "B03002_009",
  "latinx" = "B03002_012")


tenure_vars_19 <-c(
  "hh_count" = "DP04_0045",
  "owner" = "DP04_0046P",
  "owner_count" = "DP04_0046",
  "renter" = "DP04_0047P",
  "renter_count" = "DP04_0047",
  "homeval_lower_quartile" = "B25076_001",
  "homeval_med" = "B25077_001",
  "homeval_upper_quartile" = "B25078_001",
  "totalunits" ="B25034_001",
  "built_2005_on" = "B25034_002",
  "built_2000_2004"="B25034_003",
  "built_1990_1999"="B25034_004",
  "built_1980_1989" = "B25034_005",
  "built_1970_1979" ="B25034_006" ,
  "built_1960_1969"= "B25034_007",
  "built_1950_1959"="B25034_008",
  "built_1940_1949" = "B25034_009",
  "built_1939_before" = "B25034_010")

income_vars_19 <- c(
  "medinc" = "S1903_C03_001",
  "income_less_10k" =  "B19001_002",
  "income_10k_15k" =   "B19001_003",
  "income_15k_20k" =   "B19001_004",
  "income_20k_25k" =   "B19001_005",
  "income_25k_30k" =   "B19001_006",
  "income_30k_35k" =   "B19001_007",
  "income_35k_40k" =   "B19001_008",
  "income_40k_45k" =   "B19001_009",
  "income_45k_50k" =   "B19001_010",
  "income_50k_60k" =   "B19001_011",
  "income_60k_75k" =   "B19001_012",
  "income_75k_100k" =  "B19001_013",
  "income_100k_125k" = "B19001_014",
  "income_125k_150k" = "B19001_015",
  "income_150k_200k" = "B19001_016",
  "income_200k_more" = "B19001_017",
  "med_rent" = "B25064_001",
  "med_rent_percent_income" = "B25071_001",
  "rent_burden_1" = "B25070_007",
  "rent_burden_2" = "B25070_008",
  "rent_burden_3" = "B25070_009",
  "rent_burden_4" = "B25070_010",
  "poverty_rate" = "S1701_C03_001",
  "punemployment" = "S2301_C04_001",
  "welfare" = "B19057_002",
  "h_units_w_mortgage_30_35perc" = "B25091_008",
  "h_units_w_mortgage_35_40perc" = "B25091_009",
  "h_units_w_mortgage_40_45perc" = "B25091_010",
  "h_units_w_mortgage_45_50perc" = "B25091_011")

hh_vars_19 <- c(
  "children" = "S1101_C01_010",
  "seniors" = "S1101_C01_011",
  "avg_hh_size" = "B25010_001",
  "hh_2_person_fam" = "B11016_003",
  "hh_3_person_fam" = "B11016_004",
  "hh_4_person_fam" = "B11016_005",
  "hh_5_more_fam_1" = "B11016_007",
  "hh_5_more_fam_2" = "B11016_008",
  "hh_5_more_fam_3" = "B11016_006",
  "hh_1_person" = "B11016_010",
  "hh_2_person_nonfam" = "B11016_011",
  "hh_3_person_nonfam" = "B11016_012",
  "hh_4_more_person_nonfam_1" = "B11016_015",
  "hh_4_more_person_nonfam_2" = "B11016_016",
  "hh_4_more_person_nonfam_3" = "B11016_013",
  "hh_4_more_person_nonfam_4" = "B11016_014",
  "within_county" = "S0701_C02_001",
  "within_state" = "S0701_C03_001",
  "diff_state" = "S0701_C04_001",
  "abroad" = "S0701_C05_001",
  'total_family_hh' = 'B11001_002',
  'family_hh_married_couple' = 'B11001_003',
  'family_hh_other_family' = 'B11001_004',
  'family_hh_male_no_spouse_present' = 'B11001_005',
  'family_hh_female_no_spouse_present' = 'B11001_006',
  'nonfamily_hh' = 'B11001_007',
  'Nonfamily_hh_living_alone' = 'B11001_008',
  'Nonfamily_hh_not_living_alone' = 'B11001_009')

edu_vars_19 <- c(
  "totenrolled" = "B14001_001",
  "totpop25over" = "B15002_001",
  "enrolled_undergrad" = "B14001_008",
  "enrolled_grad" = "B14001_009", #total
  "below_hs_1"  = "S1501_C02_007",
  "below_hs_2" ="S1501_C02_008",
  "phighschool" = "S1501_C02_009",
  "some_college_1" = "S1501_C02_010" ,
  "some_college_2" = "S1501_C02_011",
  "pba_higher" = "S1501_C02_015",
  "pma_higher" = "S1501_C02_013") #percentage


varlist_19 = list(race_vars_19, 
                  tenure_vars_19, 
                  income_vars_19, 
                  hh_vars_19, 
                  edu_vars_19
)

#######################################################################
###### 2014 Variables  ################################################
#######################################################################


race_vars_14 <- c(
  "population"= "DP05_0001",
  "adult_pop" = "DP05_0021",
  "med_age" = "B01002_001",
  "white" = "B03002_003",
  "black" = "B03002_004",
  "amind" = "B03002_005",
  "asian" = "B03002_006",
  "pacis" = "B03002_007",
  "other" = "B03002_008",
  "race2" = "B03002_009",
  "latinx" = "B03002_012")

tenure_vars_14 <-c(
  "hh_count" = "DP04_0044",
  "owner" = "DP04_0045P",
  "owner_count" = "DP04_0045",
  "renter" = "DP04_0046P",
  "renter_count" = "DP04_0046",
  "homeval_lower_quartile" = "B25076_001",
  "homeval_med" = "B25077_001",
  "homeval_upper_quartile" = "B25078_001",
  "totalunits" ="B25034_001",
  "built_2005_on" = "B25034_002",
  "built_2000_2004"="B25034_003",
  "built_1990_1999"="B25034_004",
  "built_1980_1989" = "B25034_005",
  "built_1970_1979" ="B25034_006" ,
  "built_1960_1969"= "B25034_007",
  "built_1950_1959"="B25034_008",
  "built_1940_1949" = "B25034_009",
  "built_1939_before" = "B25034_010"
)




income_vars_14 <- c(
  "medinc" = "B19013_001",  # 19 uses S1903_C03_001
  "income_less_10k" =  "B19001_002",
  "income_10k_15k" =   "B19001_003",
  "income_15k_20k" =   "B19001_004",
  "income_20k_25k" =   "B19001_005",
  "income_25k_30k" =   "B19001_006",
  "income_30k_35k" =   "B19001_007",
  "income_35k_40k" =   "B19001_008",
  "income_40k_45k" =   "B19001_009",
  "income_45k_50k" =   "B19001_010",
  "income_50k_60k" =   "B19001_011",
  "income_60k_75k" =   "B19001_012",
  "income_75k_100k" =  "B19001_013",
  "income_100k_125k" = "B19001_014",
  "income_125k_150k" = "B19001_015",
  "income_150k_200k" = "B19001_016",
  "income_200k_more" = "B19001_017",
  "med_rent" = "B25064_001",
  "med_rent_percent_income" = "B25071_001",
  "rent_burden_1" = "B25070_007",
  "rent_burden_2" = "B25070_008",
  "rent_burden_3" = "B25070_009",
  "rent_burden_4" = "B25070_010",
  "poverty_rate" = "S1701_C03_001",
  "punemployment" = "S2301_C04_001",
  "welfare" = "B19057_002",
  'h_units_w_mortgage_30_35perc' = 'B25091_008',
  'h_units_w_mortgage_35_40perc' = 'B25091_009',
  'h_units_w_mortgage_40_45perc' = 'B25091_010',
  'h_units_w_mortgage_45_50perc' = 'B25091_011'
)

hh_vars_14 <- c(
  "children" = "S1101_C01_010",
  "seniors" = "S1101_C01_011",
  "avg_hh_size" = "B25010_001",
  "hh_2_person_fam" = "B11016_003",
  "hh_3_person_fam" = "B11016_004",
  "hh_4_person_fam" = "B11016_005",
  "hh_5_more_fam_1" = "B11016_007",
  "hh_5_more_fam_2" = "B11016_008",
  "hh_5_more_fam_3" = "B11016_006",
  "hh_1_person" = "B11016_010",
  "hh_2_person_nonfam" = "B11016_011",
  "hh_3_person_nonfam" = "B11016_012",
  "hh_4_more_person_nonfam_1" = "B11016_015",
  "hh_4_more_person_nonfam_2" = "B11016_016",
  "hh_4_more_person_nonfam_3" = "B11016_013",
  "hh_4_more_person_nonfam_4" = "B11016_014",
  "within_county" = "S0701_C02_001",
  "within_state" = "S0701_C03_001",
  "diff_state" = "S0701_C04_001",
  "abroad" = "S0701_C05_001",
  'total_family_hh' = 'B11001_002',
  'family_hh_married_couple' = 'B11001_003',
  'family_hh_other_family' = 'B11001_004',
  'family_hh_male_no_spouse_present' = 'B11001_005',
  'family_hh_female_no_spouse_present' = 'B11001_006',
  'nonfamily_hh' = 'B11001_007',
  'Nonfamily_hh_living_alone' = 'B11001_008',
  'Nonfamily_hh_not_living_alone' = 'B11001_009'
  
)

edu_vars_14 <- c(
  "totenrolled" = "B14001_001",
  "totpop25over" = "B15002_001",
  "enrolled_undergrad" = "B14001_008",
  "enrolled_grad" = "B14001_009",
  "below_hs_1"  = "S1501_C02_007",
  "below_hs_2" ="S1501_C02_008",
  "phighschool" = "S1501_C02_009",
  "some_college_1" = "S1501_C02_010" ,
  "some_college_2" = "S1501_C02_011",
  "pba_higher" = "S1501_C02_015",
  "pma_higher" = "S1501_C02_013")

varlist_14 = list(race_vars_14, tenure_vars_14, income_vars_14, hh_vars_14, edu_vars_14 )


#######################################################################
###### 2009 Variables  ################################################
#######################################################################


race_vars_09 <- c(
  "population"= "DP05_0001",
  "adult_pop" = "DP05_0021",
  "med_age" = "B01002_001",
  "white" =  "B03002_003",
  "black" =  "B03002_004",
  "amind" =  "B03002_005",
  "asian" =  "B03002_006",
  "pacis" =  "B03002_007",
  "other" =  "B03002_008",
  "race2" =  "B03002_009",
  "latinx" = "B03002_012")

tenure_vars_09 <-c(
  "hh_count" = "DP04_0044",
  "owner" = "B25003_002",
  "renter" = "B25003_003",
  "homeval_lower_quartile" = "B25076_001",
  "homeval_med" = "B25077_001",
  "homeval_upper_quartile" = "B25078_001",
  "totalunits" = "B25034_001",
  "built_2005_on" =   "B25034_002",
  "built_2000_2004" = "B25034_003",
  "built_1990_1999" = "B25034_004",
  "built_1980_1989" = "B25034_005",
  "built_1970_1979" = "B25034_006" ,
  "built_1960_1969" = "B25034_007",
  "built_1950_1959" = "B25034_008",
  "built_1940_1949" =  "B25034_009",
  "built_1939_before" = "B25034_010"
)


# 2019 uses S1903_C03_001 for med income
income_vars_09 <- c(
  "medinc" = "B19013_001",  
  "income_less_10k" =  "B19001_002",
  "income_10k_15k" =   "B19001_003",
  "income_15k_20k" =   "B19001_004",
  "income_20k_25k" =   "B19001_005",
  "income_25k_30k" =   "B19001_006",
  "income_30k_35k" =   "B19001_007",
  "income_35k_40k" =   "B19001_008",
  "income_40k_45k" =   "B19001_009",
  "income_45k_50k" =   "B19001_010",
  "income_50k_60k" =   "B19001_011",
  "income_60k_75k" =   "B19001_012",
  "income_75k_100k" =  "B19001_013",
  "income_100k_125k" = "B19001_014",
  "income_125k_150k" = "B19001_015",
  "income_150k_200k" = "B19001_016",
  "income_200k_more" = "B19001_017",
  "med_rent" = "B25064_001",
  "med_rent_percent_income" = "B25071_001",
  "rent_burden_1" =  "B25070_007",
  "rent_burden_2" =  "B25070_008",
  "rent_burden_3" =  "B25070_009",
  "rent_burden_4" =  "B25070_010",
  "poverty_rate"  =  "B17001_002",
  "unemployment_1" =  "B23001_008",
  "unemployment_2" =  "B23001_015",
  "unemployment_3" =  "B23001_022", 
  "unemployment_4" =  "B23001_029",
  "unemployment_5" =  "B23001_036",
  "unemployment_6" =  "B23001_043", 
  "unemployment_7" =  "B23001_050",
  "unemployment_8" =  "B23001_057",
  "unemployment_9" =  "B23001_064",
  "unemployment_10" = "B23001_071",
  "unemployment_11" = "B23001_076",
  "unemployment_12" = "B23001_081",
  "unemployment_13" = "B23001_086", 
  "unemployment_14" = "B23001_094", 
  "unemployment_15" = "B23001_101",
  "unemployment_16" = "B23001_108",
  "unemployment_17" = "B23001_115",
  "unemployment_18" = "B23001_122",
  "unemployment_19" = "B23001_129",
  "unemployment_20" = "B23001_136",
  "unemployment_21" = "B23001_143",
  "unemployment_22" = "B23001_150", 
  "unemployment_23" = "B23001_157",
  "unemployment_24" = "B23001_162", 
  "unemployment_25" = "B23001_167",
  "unemployment_26" = "B23001_172",
  "welfare" = "B19057_002"
)

hh_vars_09 <- c(
  "children" = "B11005_002",
  "seniors" = "B11006_002",
  "avg_hh_size" = "B25010_001",
  "hh_2_person_fam" = "B11016_003",
  "hh_3_person_fam" = "B11016_004",
  "hh_4_person_fam" = "B11016_005",
  "hh_5_more_fam_1" = "B11016_007",
  "hh_5_more_fam_2" = "B11016_008",
  "hh_5_more_fam_3" = "B11016_006",
  "hh_1_person" = "B11016_010",
  "hh_2_person_nonfam" = "B11016_011",
  "hh_3_person_nonfam" = "B11016_012",
  "hh_4_more_person_nonfam_1" = "B11016_015",
  "hh_4_more_person_nonfam_2" = "B11016_016",
  "hh_4_more_person_nonfam_3" = "B11016_013",
  "hh_4_more_person_nonfam_4" = "B11016_014",
  "within_county" = "B07003_007",
  "within_state" = "B07003_010",
  "diff_state" = "B07003_013",
  "abroad" = "B07003_016",
  'total_family_hh' = 'B11001_002',
  'family_hh_married_couple' = 'B11001_003',
  'family_hh_other_family' = 'B11001_004',
  'family_hh_male_no_spouse_present' = 'B11001_005',
  'family_hh_female_no_spouse_present' = 'B11001_006',
  'nonfamily_hh' = 'B11001_007',
  'Nonfamily_hh_living_alone' = 'B11001_008',
  'Nonfamily_hh_not_living_alone' = 'B11001_009',
  'h_units_w_mortgage_30_35perc' = 'B25091_008',
  'h_units_w_mortgage_35_40perc' = 'B25091_009',
  'h_units_w_mortgage_40_45perc' = 'B25091_010',
  'h_units_w_mortgage_45_50perc' = 'B25091_011'
)

edu_vars_09 <- c(
  "totenrolled" = "B14001_001",
  "enrolled_undergrad" = "B14001_008",
  "enrolled_grad" = "B14001_009",
  "totpop25over" = "B15002_001",
  "totpop16over" = "B20001_001",
  "below_hs_1" = "B15002_003",
  "below_hs_2" = "B15002_004",
  "below_hs_3" ="B15002_005",
  "below_hs_4" ="B15002_006",
  "below_hs_5" ="B15002_007",
  "below_hs_6" ="B15002_008",
  "below_hs_7" ="B15002_009",
  "below_hs_8" ="B15002_010",
  "highschool_1" = "B15002_011",
  "highschool_2" = "B15002_028")

ma_vars_09 <-c(
  "ma_higher_1" = "B15002_016", 
  "ma_higher_2" = "B15002_017", 
  "ma_higher_3" = "B15002_018",
  "ma_higher_4" = "B15002_033", 
  "ma_higher_5" = "B15002_034",
  "ma_higher_6" = "B15002_035")

college_vars_09 <-c(
  "some_college_1" = "B15002_012",
  "some_college_2" = "B15002_013",
  "some_college_3" = "B15002_014",
  "some_college_4" = "B15002_029",
  "some_college_5" = "B15002_030", 
  "some_college_6" = "B15002_031")

ba_vars_09 <- c(
  "ba_higher_1" = "B15002_015",
  "ba_higher_2" ="B15002_016",
  "ba_higher_3" = "B15002_017",
  "ba_higher_4" =  "B15002_018",
  "ba_higher_5" = "B15002_032", 
  "ba_higher_6" = "B15002_033",
  "ba_higher_7" =  "B15002_034", 
  "ba_higher_8" = "B15002_035")

varlist_09 = list(race_vars_09, 
                  tenure_vars_09, 
                  income_vars_09, 
                  hh_vars_09, 
                  ma_vars_09, 
                  college_vars_09, 
                  edu_vars_09 ,
                  ba_vars_09
)


#######################################################################
###### 2010 Variables  ################################################
#######################################################################

income_vars_10 <- c(
  "medinc" = "B19013_001",  
  "income_less_10k" = "S1901_C01_002",
  "income_10k_15k" = "S1901_C01_003",
  "income_15k_25k" = "S1901_C01_004",
  "income_25k_35k" = "S1901_C01_005",
  "income_35k_50k" = "S1901_C01_006",
  "income_50k_75k" = "S1901_C01_007",
  "income_75k_100k" = "S1901_C01_008",
  "income_100k_150k" = "S1901_C01_009",
  "income_150k_200k" = "S1901_C01_010",
  "income_200k_more" = "S1901_C01_011")





#######################################################################
###### ACS Pulling Function  ##########################################
#######################################################################
# this code loops through counties and years supplied below. For each #
# county, the code finds the correct 5 year acs survey and pulls the  #
# correct group of variables, listed above. Then, each group of vari- #
# bles are joined by GEOID and labeled by county and survey.          #
#######################################################################

#countylist <- c(097, 095)
yearlist <- c(2019, 2014, 2009)

acs_09 <- data.frame(
  GEOID = character(),
  estimate = numeric())

acs_14 <- data.frame(
  GEOID = character(),
  estimate = numeric())

acs_19 <- data.frame(
  GEOID = character(),
  estimate = numeric())


for(year in yearlist){
  if(year == 2019){
    for(var in varlist_19){
      acs_19 <- bind_rows(acs_19, get_acs(geography = "tract", 
                                          variables = var, 
                                          state = "CA",
                                          year = 2019,
                                          geometry = FALSE,
                                          cache_table = FALSE,
                                          key = CENSUS_API_KEY,
                                          survey = "acs5") %>%
                            mutate(year = year))  
    }
  }else if(year == 2014){ 
    for(var in varlist_14){
      acs_14 <- bind_rows(acs_14, get_acs(geography = "tract", 
                                          variables = var, 
                                          state = "CA",
                                          year = 2014,
                                          geometry = FALSE,
                                          cache_table = FALSE,
                                          key = CENSUS_API_KEY,
                                          survey = "acs5")%>%
                            mutate(year = year)) 
    }    
  }else if (year == 2009){
    for(var in varlist_09){
      acs_09 <-bind_rows(acs_09, get_acs(geography = "tract", 
                                         variables = var, 
                                         state = "CA",
                                         year = 2009,
                                         geometry = FALSE,
                                         cache_table = FALSE,
                                         key = CENSUS_API_KEY,
                                         survey = "acs5")%>%
                           mutate(year = year))
    }    
  }
  
  acs_09[is.na(acs_09)] <- 0
  acs_14[is.na(acs_14)] <- 0
  acs_19[is.na(acs_19)] <- 0
  print('Done')

}

#########################################################################################
############## acs00 to 10 crosswalk ####################################################
#########################################################################################
meds <- c('med_age','med_rent',  'homeval_lower_quartile', 'homeval_med', 
          'homeval_upper_quartile', 'medinc', 'avg_hh_size', "med_rent_percent_income" )

ltdb  =  read.csv('~/data/projects/displacement_measure/LTDB_weights/crosswalk_2000_2010.csv', 
                  colClasses=c(
                    "trtid00"="character", 
                    "trtid10"="character")) 






#Weighted sums for crosswalk 
temp <- ltdb %>% inner_join( 
  acs_09 %>% subset(!(variable %in% meds )), 
  by = c("trtid00"="GEOID")) %>%
  mutate(estimate = estimate*weight) %>%
  group_by(trtid10, variable) %>%
  summarise(estimate = sum(estimate)) %>%
  rename(GEOID = trtid10)



# weighted averages for median values crosswalked
ltdb <- ltdb %>%     
  group_by(trtid10) %>%
  mutate(sum = sum(weight)) %>%
  mutate(adjusted_weight = weight/sum) %>%
  rename(GEOID = trtid10)


temp_meds <- ltdb %>% inner_join( 
  acs_09 %>% subset((variable %in% meds)), 
  by = c("trtid00"="GEOID")) %>% mutate(
    adjusted_estimate = estimate*adjusted_weight) %>%
  dplyr::group_by(GEOID, variable) %>% 
  summarise(estimate = sum(adjusted_estimate))

acs_09_full <- rbind(temp, temp_meds)


census_wide09 <- acs_09_full %>%
  ungroup() %>%
  select(GEOID, variable, estimate) %>%
  spread(variable, estimate, fill = NA) %>% 
  mutate(year = 2009)

census_wide14 <- acs_14 %>%
  ungroup() %>%
  select(GEOID, variable, estimate) %>%
  spread(variable, estimate, fill = NA)

census_wide19 <- acs_19 %>%
  ungroup() %>%
  select(GEOID, variable, estimate) %>%
  spread(variable, estimate, fill = NA)



##########################################################################
# Inflation Adujstment 
##########################################################################

# change in rent 
# change in income
# Change in 


CPI_09_19 = 1.20
CPI_14_19 = 1.08

vars09 <-  c( 'homeval_lower_quartile', 'homeval_med', 'homeval_upper_quartile', 'medinc', 'med_rent')
vars14 <- c( 'homeval_lower_quartile', 'homeval_med', 'homeval_upper_quartile', 'medinc', 'med_rent')

acs_09 <- 
  census_wide09 %>%
  mutate(
    across(
      # select selected columns
      .cols = vars09,
      # find value of data multiplied by weight
      ~ . * CPI_09_19,
      # attach name in the following pattern
    )
  ) 


acs_14 <- 
  census_wide14 %>%
  mutate(
    across(
      # select selected Census/ACS data (defaults to everything)
      .cols = vars14,
      # find value of data multiplied by weight
      ~ . * CPI_14_19,
      # attach name in the following pattern
    )
  ) 




#######################################################################
###### Manipulating Columns  ##########################################
#######################################################################


acs_09 <- 
  acs_09 %>% 
  mutate(
    ba_higher = ba_higher_1 + ba_higher_2 + ba_higher_3 + ba_higher_4 + ba_higher_5 + ba_higher_6 + ba_higher_7 + ba_higher_8,
    ma_higher = ma_higher_1 + ma_higher_2 + ma_higher_3 + ma_higher_4 + ma_higher_5 + ma_higher_6,
    below_hs = below_hs_1 + below_hs_2 + below_hs_3 + below_hs_4 + below_hs_5 + below_hs_6 + below_hs_7 + below_hs_8, 
    some_college = some_college_1 + some_college_2 + some_college_3 + some_college_4 + some_college_5 + some_college_6,
    highschool = highschool_1 + highschool_2,
    hh_5_more_person_fam = hh_5_more_fam_1 + hh_5_more_fam_2 + hh_5_more_fam_3,
    hh_4_more_person_nonfam = hh_4_more_person_nonfam_1 + hh_4_more_person_nonfam_2 + hh_4_more_person_nonfam_3 + hh_4_more_person_nonfam_4,
    rent_burden = rent_burden_1 + rent_burden_2 + rent_burden_3 + rent_burden_4,
    unemployment = unemployment_1 + unemployment_2 + unemployment_3 + unemployment_4 + unemployment_5 + unemployment_6 + unemployment_7 + unemployment_8 + unemployment_9 + unemployment_10
    + unemployment_10 + unemployment_11 + unemployment_12 + unemployment_13 + unemployment_14 + unemployment_15 + unemployment_16 + unemployment_17 
    + unemployment_18 + unemployment_19 + unemployment_20 + unemployment_21 + unemployment_22 + unemployment_23 + unemployment_24 + unemployment_25
    + unemployment_25 + unemployment_26,
    pWhite = white/population,
    pAsian = asian/population,
    pBlack = black/population,
    pLatinx = latinx/population,
    pOther = (population - sum(white, asian, black, latinx, na.rm = TRUE))/population,
    penrolled_undergrad = enrolled_undergrad/population,
    penrolled_grad = enrolled_grad/population,
    pba_higher = ba_higher/totpop25over,
    pma_higher = ma_higher/ totpop25over,
    phighschool= highschool/totpop25over,
    pbelow_hs = below_hs/ totpop25over,
    psome_college = some_college/totpop25over,
    punemployment = unemployment/totpop16over,
    pincome_less_10k = income_less_10k/hh_count,
    pincome_10k_15k = income_10k_15k/hh_count,
    pincome_15k_20k = income_15k_20k/hh_count,
    pincome_20k_25k = income_20k_25k/hh_count,
    pincome_25k_30k = income_25k_30k/hh_count,
    pincome_30k_35k = income_30k_35k/hh_count,
    pincome_35k_40k = income_35k_40k/hh_count,
    pincome_40k_45k = income_40k_45k/hh_count,
    pincome_45k_50k = income_45k_50k/hh_count,
    pincome_50k_60k = income_50k_60k/hh_count,
    pincome_60k_75k = income_60k_75k/hh_count,
    pincome_75k_100k = income_75k_100k/hh_count,
    pincome_100k_125k = income_100k_125k/hh_count,
    pincome_125k_150k = income_125k_150k/hh_count,
    pincome_150k_200k = income_150k_200k/hh_count,
    pincome_200k_more = income_200k_more/hh_count,
    prent_burden =rent_burden/hh_count,
    phh_2_person_fam = hh_2_person_fam/hh_count,
    phh_3_person_fam = hh_3_person_fam/hh_count,
    phh_4_person_fam = hh_4_person_fam/hh_count,
    phh_5_more_person_fam = hh_5_more_person_fam/hh_count,
    phh_1_person = hh_1_person/hh_count,
    phh_2_person_nonfam = hh_2_person_nonfam/hh_count,
    phh_3_person_nonfam = hh_3_person_nonfam/hh_count,
    phh_4_more_person_nonfam = hh_4_more_person_nonfam/hh_count,
    pwithin_state = within_state/population,
    pwithin_county = within_county/population,
    pdiff_state = diff_state/population,
    pabroad = abroad/population,
    pchildren = children/hh_count,
    pseniors = seniors/hh_count,
    pbuilt_2005_on = built_2005_on/totalunits,
    pbuilt_2000_2004 = built_2000_2004/totalunits,
    pbuilt_1990_1999 = built_1990_1999/totalunits,
    pbuilt_1980_1989 = built_1980_1989/totalunits,
    pbuilt_1970_1979 = built_1970_1979/totalunits ,
    pbuilt_1960_1969 = built_1960_1969/totalunits,
    pbuilt_1950_1959 = built_1950_1959/totalunits,
    pbuilt_1940_1949 = built_1940_1949/totalunits,
    pbuilt_1939_before = built_1939_before/totalunits
  )


acs_14 <-
  acs_14 %>% 
  mutate(
    pbelow_hs = below_hs_1 + below_hs_2, 
    psome_college = some_college_1 + some_college_2,
    hh_5_more_person_fam = hh_5_more_fam_1 + hh_5_more_fam_2 + hh_5_more_fam_3,
    hh_4_more_person_nonfam = hh_4_more_person_nonfam_1 + hh_4_more_person_nonfam_2 + hh_4_more_person_nonfam_3 + hh_4_more_person_nonfam_4,
    rent_burden = rent_burden_1 + rent_burden_2 + rent_burden_3 + rent_burden_4,
    pWhite = white/population,
    pAsian = asian/population,
    pBlack = black/population,
    pLatinx = latinx/population,
    pOther = (population - sum(white, asian, black, latinx, na.rm = TRUE))/population,
    penrolled_undergrad = enrolled_undergrad/population,
    penrolled_grad = enrolled_grad/population,
    prent_burden =rent_burden/hh_count,
    phh_2_person_fam = hh_2_person_fam/hh_count,
    phh_3_person_fam = hh_3_person_fam/hh_count,
    phh_4_person_fam = hh_4_person_fam/hh_count,
    phh_5_more_person_fam = hh_5_more_person_fam/hh_count,
    phh_1_person = hh_1_person/hh_count,
    phh_2_person_nonfam = hh_2_person_nonfam/hh_count,
    phh_3_person_nonfam = hh_3_person_nonfam/hh_count,
    phh_4_more_person_nonfam = hh_4_more_person_nonfam/hh_count,
    pwithin_state = within_state/population,
    pwithin_county = within_county/population,
    pdiff_state = diff_state/population,
    pabroad = abroad/100,
    pchildren = children/100,
    pseniors = seniors/100,
    punemployment = punemployment/100,
    pba_higher = pba_higher/100,
    pma_higher = pma_higher/100,
    pbuilt_2005_on = built_2005_on/totalunits,
    pbuilt_2000_2004 = built_2000_2004/totalunits,
    pbuilt_1990_1999 = built_1990_1999/totalunits,
    pbuilt_1980_1989 = built_1980_1989/totalunits,
    pbuilt_1970_1979 = built_1970_1979/totalunits ,
    pbuilt_1960_1969 = built_1960_1969/totalunits,
    pbuilt_1950_1959 = built_1950_1959/totalunits,
    pbuilt_1940_1949 = built_1940_1949/totalunits,
    pbuilt_1939_before = built_1939_before/totalunits,
    pincome_less_10k = income_less_10k/hh_count,
    pincome_10k_15k = income_10k_15k/hh_count,
    pincome_15k_20k = income_15k_20k/hh_count,
    pincome_20k_25k = income_20k_25k/hh_count,
    pincome_25k_30k = income_25k_30k/hh_count,
    pincome_30k_35k = income_30k_35k/hh_count,
    pincome_35k_40k = income_35k_40k/hh_count,
    pincome_40k_45k = income_40k_45k/hh_count,
    pincome_45k_50k = income_45k_50k/hh_count,
    pincome_50k_60k = income_50k_60k/hh_count,
    pincome_60k_75k = income_60k_75k/hh_count,
    pincome_75k_100k = income_75k_100k/hh_count,
    pincome_100k_125k = income_100k_125k/hh_count,
    pincome_125k_150k = income_125k_150k/hh_count,
    pincome_150k_200k = income_150k_200k/hh_count,
    pincome_200k_more = income_200k_more/hh_count
  )



acs_19 <- 
  census_wide19 %>% 
  mutate(
    pbelow_hs = below_hs_1 + below_hs_2, 
    psome_college = some_college_1 + some_college_2,
    hh_5_more_person_fam = hh_5_more_fam_1 + hh_5_more_fam_2 + hh_5_more_fam_3,
    hh_4_more_person_nonfam = hh_4_more_person_nonfam_1 + hh_4_more_person_nonfam_2 + hh_4_more_person_nonfam_3 + hh_4_more_person_nonfam_4,
    rent_burden = rent_burden_1 + rent_burden_2 + rent_burden_3 + rent_burden_4,
    pWhite = white/population,
    pAsian = asian/population,
    pBlack = black/population,
    pLatinx = latinx/population,
    pOther = (population - sum(white, asian, black, latinx, na.rm = TRUE))/population,
    penrolled_undergrad = enrolled_undergrad/population,
    penrolled_grad = enrolled_grad/population,
    prent_burden =rent_burden/hh_count,
    phh_2_person_fam = hh_2_person_fam/hh_count,
    phh_3_person_fam = hh_3_person_fam/hh_count,
    phh_4_person_fam = hh_4_person_fam/hh_count,
    phh_5_more_person_fam = hh_5_more_person_fam/hh_count,
    phh_1_person = hh_1_person/hh_count,
    phh_2_person_nonfam = hh_2_person_nonfam/hh_count,
    phh_3_person_nonfam = hh_3_person_nonfam/hh_count,
    phh_4_more_person_nonfam = hh_4_more_person_nonfam/hh_count,
    pwithin_state = within_state/population,
    pwithin_county = within_county/population,
    pdiff_state = diff_state/population,
    pabroad = abroad/100,
    pchildren = children/100,
    pseniors = seniors/100,
    punemployment = punemployment/100,
    pba_higher = pba_higher/100,
    pma_higher = pma_higher/100,
    pbuilt_2005_on = built_2005_on/totalunits,
    pbuilt_2000_2004 = built_2000_2004/totalunits,
    pbuilt_1990_1999 = built_1990_1999/totalunits,
    pbuilt_1980_1989 = built_1980_1989/totalunits,
    pbuilt_1970_1979 = built_1970_1979/totalunits ,
    pbuilt_1960_1969 = built_1960_1969/totalunits,
    pbuilt_1950_1959 = built_1950_1959/totalunits,
    pbuilt_1940_1949 = built_1940_1949/totalunits,
    pbuilt_1939_before = built_1939_before/totalunits,
    pincome_less_10k = income_less_10k/hh_count,
    pincome_10k_15k = income_10k_15k/hh_count,
    pincome_15k_20k = income_15k_20k/hh_count,
    pincome_20k_25k = income_20k_25k/hh_count,
    pincome_25k_30k = income_25k_30k/hh_count,
    pincome_30k_35k = income_30k_35k/hh_count,
    pincome_35k_40k = income_35k_40k/hh_count,
    pincome_40k_45k = income_40k_45k/hh_count,
    pincome_45k_50k = income_45k_50k/hh_count,
    pincome_50k_60k = income_50k_60k/hh_count,
    pincome_60k_75k = income_60k_75k/hh_count,
    pincome_75k_100k = income_75k_100k/hh_count,
    pincome_100k_125k = income_100k_125k/hh_count,
    pincome_125k_150k = income_125k_150k/hh_count,
    pincome_150k_200k = income_150k_200k/hh_count,
    pincome_200k_more = income_200k_more/hh_count
    
  )



#########################################################################
### Drop component Variables ############################################
#########################################################################


drop09 <- c( "Group.1", "ma_higher_1", "cbsa10","metdiv10","ccflag10","changetype", "ma_higher_2", "ma_higher_3", 
             "ma_higher_4", "ma_higher_5", "ma_higher_6", "below_hs", "totpop16over",
             "some_college_1", "some_college_2", "some_college_3", "some_college_4", "some_college_5",
             "some_college_6", "ba_higher_1", "ba_higher_2" ,"ba_higher_3", "ba_higher_4", "ba_higher_5","ba_higher", "ma_higher",
             "ba_higher_6", "ba_higher", "ma_higher", "ba_higher_7", "ba_higher_8", "below_hs_1", "below_hs_2", "below_hs_3",
             "below_hs_4", "below_hs_5", "below_hs_6", "below_hs_7", "below_hs_8", "highschool_1" , "some_college", "highschool",
             "highschool_2", "hh_4_more_person_nonfam_1","hh_4_more_person_nonfam_2", "hh_4_more_person_nonfam_3",
             "hh_4_more_person_nonfam_4", "hh_5_more_fam_1","hh_5_more_fam_2", "hh_5_more_fam_3", "rent_burden_1",
             "rent_burden_2" , "rent_burden_3", "rent_burden_4", "unemployment", "unemployment_1", "unemployment_2","unemployment_3",
             "unemployment_4" ,"unemployment_5", "unemployment_6", "unemployment_7" ,"unemployment_8","unemployment_9", "unemployment_10",
             "unemployment_11" ,"unemployment_12" ,"unemployment_13",  "unemployment_14", "unemployment_15", "unemployment_16" ,"unemployment_17",
             "unemployment_18" , "unemployment_19" ,"unemployment_20" ,"unemployment_21", "unemployment_22" , "unemployment_23" , "unemployment_24",
             "unemployment_25" , "unemployment_26" , "trtid00.y"  , "trtid00.x", "weight", "placefp10" 
)

acs_09 = acs_09[,!(names(acs_09) %in% drop09)]

drop14 <- c(  "ma_higher_1", "ma_higher_2", "ma_higher_3", "ma_higher_4", "ma_higher_5", "ma_higher_6", 
              "some_college_1", "some_college_2", "some_college_3", "some_college_4", "some_college_5",
              "some_college_6", "ba_higher_1", "ba_higher_2" ,"ba_higher_3", "ba_higher_4", "ba_higher_5",
              "ba_higher_6", "ba_higher_7", "ba_higher_8", "below_hs_1", "below_hs_2", "below_hs_3",
              "below_hs_4", "below_hs_5", "below_hs_6", "below_hs_7",  "NAM", "owner_count", "renter_count", 
              "below_hs_8", "highschool_1" , 
              "highschool_2", "hh_4_more_person_nonfam_1","hh_4_more_person_nonfam_2", "hh_4_more_person_nonfam_3",
              "hh_4_more_person_nonfam_4", "hh_5_more_fam_1","hh_5_more_fam_2", "hh_5_more_fam_3", "rent_burden_1",
              "rent_burden_2" , "rent_burden_3", "rent_burden_4", "unemployment_1", "unemployment_2","unemployment_3",
              "unemployment_4" ,"unemployment_5", "unemployment_6", "unemployment_7" ,"unemployment_8","unemployment_9", "unemployment_10",
              "unemployment_11" ,"unemployment_12" ,"unemployment_13",  "unemployment_14", "unemployment_15", "unemployment_16" ,"unemployment_17",
              "unemployment_18" , "unemployment_19" ,"unemployment_20" ,"unemployment_21", "unemployment_22" , "unemployment_23" , "unemployment_24",
              "unemployment_25" , "unemployment_26")

acs_14 = acs_14[,!(names(acs_14) %in% drop14)]

drop19 <- c(  "ma_higher_1", "ma_higher_2", "ma_higher_3", "ma_higher_4", "ma_higher_5", "ma_higher_6", 
              "some_college_1", "some_college_2", "some_college_3", "some_college_4", "some_college_5",
              "some_college_6", "ba_higher_1", "ba_higher_2" ,"ba_higher_3", "ba_higher_4", "ba_higher_5",
              "ba_higher_6", "ba_higher_7", "NAM", "owner_count", "renter_count", 
              "ba_higher_8", "below_hs_1", "below_hs_2", "below_hs_3",
              "below_hs_4", "below_hs_5", "below_hs_6", "below_hs_7", "below_hs_8", "highschool_1" , 
              "highschool_2", "hh_4_more_person_nonfam_1","hh_4_more_person_nonfam_2", "hh_4_more_person_nonfam_3",
              "hh_4_more_person_nonfam_4", "hh_5_more_fam_1","hh_5_more_fam_2", "hh_5_more_fam_3", "rent_burden_1",
              "rent_burden_2" , "rent_burden_3", "rent_burden_4", "unemployment_1", "unemployment_2","unemployment_3",
              "unemployment_4" ,"unemployment_5", "unemployment_6", "unemployment_7" ,"unemployment_8","unemployment_9", "unemployment_10",
              "unemployment_11" ,"unemployment_12" ,"unemployment_13",  "unemployment_14", "unemployment_15", "unemployment_16" ,"unemployment_17",
              "unemployment_18" , "unemployment_19" ,"unemployment_20" ,"unemployment_21", "unemployment_22" , "unemployment_23" , "unemployment_24",
              "unemployment_25" , "unemployment_26")

acs_19 = acs_19[,!(names(acs_19) %in% drop19)]


################################################################################
########### Creating change over time Variables ################################
################################################################################


cov09 = select(acs_09, 
               c("med_rent","homeval_lower_quartile" , 
                 "homeval_med" ,  "homeval_upper_quartile" , 
                 "medinc", "punemployment" ,
                 "pba_higher","pma_higher", "GEOID" )) %>%
  mutate(
    across(
      .names = "{col}_09"
    ))

cov14 = select(acs_14, 
               c("med_rent","homeval_lower_quartile" ,
                 "homeval_med" ,  "homeval_upper_quartile" , 
                 "medinc", "punemployment" ,
                 "pba_higher","pma_higher",  "GEOID" )) %>%
  mutate(
    across(
      .names = "{col}_14"
    ))

cov19 = select(acs_19, 
               c("med_rent","homeval_lower_quartile" ,
                 "homeval_med" ,  "homeval_upper_quartile" , 
                 "medinc", "punemployment" ,
                 "pba_higher","pma_higher" , "GEOID")) %>%
  mutate(
    across(
      .names = "{col}_19"
    ))


covdf <-
  cov19 %>%
  left_join(cov14, by = "GEOID")

covdf <-
  covdf %>%
  left_join(cov09, by = "GEOID")


## Creating change over time variables
covdf <- 
  covdf %>% mutate(
    c_medrent_09_19 = med_rent_19 - med_rent_09,
    c_medrent_14_19 = med_rent_19 - med_rent_14,
    c_medinc_09_19 = medinc_19 - medinc_09,
    c_medinc_14_19 = medinc_19 - medinc_14,
    c_homeval_med_09_19 = homeval_med_19 - homeval_med_09,
    c_homeval_med_14_19 = homeval_med_19 - homeval_med_14,
    c_homeval_lower_quartile_09_19 = homeval_lower_quartile_19 - homeval_lower_quartile_09,
    c_homeval_lower_quartile_14_19 = homeval_lower_quartile_19 - homeval_lower_quartile_14,
    c_homeval_upper_quartile_09_19 = homeval_upper_quartile_19 - homeval_upper_quartile_09,
    c_homeval_upper_quartile_14_19 = homeval_upper_quartile_19 - homeval_upper_quartile_14,
    c_punemployment_09_19 = punemployment_19 - punemployment_09,
    c_punemployment_14_19 = punemployment_19 - punemployment_14,
    c_pba_higher_09_19 = pba_higher_19 - pba_higher_09,
    c_pba_higher_14_19 = pba_higher_19 - pba_higher_14,
    c_pma_higher_09_19 = pma_higher_19 - pma_higher_09,
    c_pma_higher_14_19 = pma_higher_19 - pma_higher_14
  )



### Select only variables showing change over time
vars <- c('c_medrent_09_19', 'c_medrent_14_19', 'c_medinc_09_19', 'c_medinc_14_19', 'c_homeval_med_09_19',
          'c_homeval_med_14_19', 'c_homeval_lower_quartile_09_19', 'c_homeval_lower_quartile_14_19',
          'c_homeval_upper_quartile_09_19', 'c_homeval_upper_quartile_14_19', 'c_punemployment_09_19',
          'c_punemployment_14_19', 'c_pba_higher_09_19', 'c_pba_higher_14_19', 'c_pma_higher_09_19', 'c_pma_higher_14_19', 'GEOID')

covdfjoin <- select(covdf, vars)


acs_09_df <-
  acs_09 %>%
  full_join(covdfjoin, by = "GEOID") %>% 
  mutate( year = '2009',
          COUNTY = substr(GEOID, 1, 5)
  )

acs_14_df <-
  acs_14 %>%
  full_join(covdfjoin, by = "GEOID") %>% 
  mutate( year = '2014',
          COUNTY = substr(GEOID, 1, 5)
  )

acs_19_df <-
  acs_19 %>%
  full_join(covdfjoin, by = "GEOID") %>% 
  mutate( year = '2019', 
          COUNTY = substr(GEOID, 1, 5)
  )




df <- rbind(acs_19_df, acs_14_df, acs_09_df)
#######################################################################
###### Income Interpolation  ##########################################
#######################################################################




# ==========================================================================
# Curate HH income group dataset
# ==========================================================================

#
# vector of income groups to rename values in proceeding dataframe
# --------------------------------------------------------------------------

inc_names <- c(
  'income_less_10k' = 1000, 
  'income_10k_15k' = 14999, 
  'income_15k_20k' = 19999, 
  'income_20k_25k' = 24999, 
  'income_25k_30k' = 29999, 
  'income_30k_35k' = 34999,
  'income_35k_40k' = 39999, 
  'income_40k_45k' = 44999, 
  'income_45k_50k' = 49999, 
  'income_50k_60k' = 59999,
  'income_60k_75k' = 74999,
  'income_75k_100k' = 99999,
  'income_100k_125k' = 124999,
  'income_125k_150k' = 149999,
  'income_150k_200k' = 199999,
  'income_200k_more' = 200000 
)
#
# Create income group dataframe
# --------------------------------------------------------------------------

inc_df <- data.frame(
  GEOID = character())


for (year in yearlist){  
  output <- get_acs(geography = "county", 
                    variables = c('county_medinc' = 'B19013_001'), 
                    state = "CA",
                    year = year,
                    geometry = FALSE,
                    cache_table = FALSE,
                    output = "wide",
                    key = CENSUS_API_KEY,
                    survey = "acs5") %>%
    dplyr::select(-dplyr::ends_with("M")) %>% 
    mutate(year = as.character(year))
  
  inc_df <- rbind(inc_df, output)
  
}

df <- df %>% left_join(inc_df, by = c('COUNTY' = 'GEOID', 'year' ='year'))



df_vli <-
  df %>% 
  select(
    GEOID,
    year, 
    COUNTY,
    county_medincE,
    income_less_10k:income_200k_more) %>%
  group_by(COUNTY, year) %>%
  mutate(co_mhhinc = median(county_medincE, na.rm = TRUE),
         co_MI_val = 1.2*co_mhhinc,
         co_LI_val = .8*co_mhhinc,
         co_VLI_val = .5*co_mhhinc,
         co_ELI_val = .3*co_mhhinc) %>% 
  ungroup() %>%
  gather(medinc_cat, medinc_count, income_less_10k:income_200k_more) %>% 
  mutate(medinc_cat = recode(medinc_cat, !!!inc_names)) %>% 
  mutate(bottom_inccat = 
           case_when(
             medinc_cat > 0 & medinc_cat <= 49999 ~ medinc_cat - 4999,
             medinc_cat == 59999 ~ medinc_cat - 9999,
             medinc_cat == 74999 | medinc_cat == 99999 ~ medinc_cat - 14999,
             medinc_cat == 124999 | medinc_cat == 149999 ~ medinc_cat - 24999,
             medinc_cat == 199999 ~ medinc_cat - 49999,
             medinc_cat == 200000 ~ medinc_cat,
             # for owner and renter income
             # medinc_cat > 9999 &
             # medinc_cat <= 49999 ~ medinc_cat - 4999,
             # medinc_cat == 59999 ~ medinc_cat - 9999,
             # medinc_cat > 59999 &
             # medinc_cat <= 149999 ~ medinc_cat - 24999,
             # medinc_cat >= 199999 ~ medinc_cat - 49999,
             TRUE ~ NA_real_),
         top_inccat = medinc_cat,
         HI = case_when(
           bottom_inccat <= co_MI_val & top_inccat >= co_MI_val ~ 
             (top_inccat - co_MI_val)/(top_inccat - bottom_inccat),
           bottom_inccat >= co_MI_val ~ 1, 
           TRUE ~ 0), 
         MI = case_when(
           bottom_inccat <= co_LI_val & top_inccat >= co_LI_val ~ 
             (top_inccat - co_LI_val)/(top_inccat - bottom_inccat),
           bottom_inccat >= co_LI_val & top_inccat <= co_MI_val ~ 1, 
           bottom_inccat <= co_MI_val & top_inccat >= co_MI_val ~ 
             (co_MI_val - bottom_inccat)/(top_inccat - bottom_inccat),
           TRUE ~ 0), 
         LI = case_when(
           bottom_inccat <= co_VLI_val & top_inccat >= co_VLI_val ~ 
             (top_inccat - co_VLI_val)/(top_inccat - bottom_inccat),
           bottom_inccat >= co_VLI_val & top_inccat <= co_LI_val ~ 1, 
           bottom_inccat <= co_LI_val & top_inccat >= co_LI_val ~ 
             (co_LI_val - bottom_inccat)/(top_inccat - bottom_inccat),
           TRUE ~ 0), 
         VLI = case_when(
           bottom_inccat <= co_ELI_val & top_inccat >= co_ELI_val ~ 
             (top_inccat - co_ELI_val)/(top_inccat - bottom_inccat),
           bottom_inccat >= co_ELI_val & top_inccat <= co_VLI_val ~ 1, 
           bottom_inccat <= co_VLI_val & top_inccat >= co_VLI_val ~ 
             (co_VLI_val - bottom_inccat)/(top_inccat - bottom_inccat),
           TRUE ~ 0), 
         ELI = case_when(
           top_inccat <= co_ELI_val ~ 1, 
           bottom_inccat <= co_ELI_val & top_inccat >= co_ELI_val ~ 
             (co_ELI_val - bottom_inccat)/(top_inccat - bottom_inccat),
           TRUE ~ 0)) %>% 
  group_by(GEOID, year) %>%
  mutate(
    tot_hh = sum(medinc_count, na.rm = TRUE),
    High = sum(HI*medinc_count, na.rm = TRUE),
    Moderate = sum(MI*medinc_count, na.rm = TRUE),
    Low = sum(LI*medinc_count, na.rm = TRUE),
    `Very Low` = sum(VLI*medinc_count, na.rm = TRUE),
    `Extremely Low` = sum(ELI*medinc_count, na.rm = TRUE)
    # tr_HI_prop = tr_HI_count/tr_totinc_count,
    # tr_MI_prop = tr_MI_count/tr_totinc_count,
    # tr_LI_prop = tr_LI_count/tr_totinc_count,
    # tr_VLI_prop = tr_VLI_count/tr_totinc_count,
    # tr_ELI_prop = tr_ELI_count/tr_totinc_count
  ) %>% 
  select(year, GEOID, tot_hh:`Extremely Low`) %>%
  distinct() %>% 
  gather(inc_group, num_hh, High:`Extremely Low`) %>% 
  mutate(perc_hh = num_hh/tot_hh) 



fdf <- df_vli %>%
  ungroup() %>%
  pivot_wider(names_from = inc_group, 
              names_glue = "{inc_group}_{.value}",
              values_from =  c(num_hh, perc_hh)) %>%
  left_join(df, by = c('GEOID' = 'GEOID', 'year' ='year'))





################################################################################
############## write to fst ####################################################
################################################################################


write_fst(fdf, "~/data/projects/displacement_measure/acs_vars/ACS.fst", compress = 50, uniform_encoding = TRUE)


################################################################################

vd09 <- load_variables(2009, "acs5/profile", cache = TRUE)
v09 <- load_variables(2009, "acs5", cache = TRUE)
v14 <- load_variables(2014, "acs5", cache = TRUE)
v19 <- load_variables(2019, "acs5", cache = TRUE)
vs10 <- load_variables(2010, "acs5/subject", cache = TRUE)
vd19 <- load_variables(2019, "acs5/profile", cache = TRUE)
vd14 <- load_variables(2014, "acs5/profile", cache = TRUE)





################################################################################
############  Create rent gap and extra local change in rent
################################################################################
# 
# #
# # Tract data
# # --------------------------------------------------------------------------
# # Note: Make sure to extract tracts that surround cities. For example, in 
# # Memphis and Chicago, TN, MO, MS, and AL are within close proximity of 
# # Memphis and IN is within close proximity of Chicago. 
# 
# ### Tract data extraction function: add your state here
# df<-acs_09
# 
# 
# st<- c("CA")
# 
# tr_rent <- function(year, state){
#   get_acs(
#     geography = "tract",
#     variables = c('medrent' = 'B25064_001'),
#     state = state,
#     county = NULL,
#     geometry = FALSE,
#     cache_table = TRUE,
#     output = "tidy",
#     year = year,
#     keep_geo_vars = TRUE
#   ) %>%
#     select(-moe) %>% 
#     rename(medrent = estimate) %>% 
#     mutate(
#       county = str_sub(GEOID, 3,5), 
#       state = str_sub(GEOID, 1,2),
#       year = str_sub(year, 3,4) 
#     )
# }
# 
# ### Loop (map) across different states
# tr_rents19 <- 
#   map_dfr(st, function(state){
#     tr_rent(year = 2019, state) %>% 
#       mutate(COUNTY = substr(GEOID, 1, 5))
#   })
# 
# tr_rents09 <- 
#   map_dfr(st, function(state){
#     tr_rent(year = 2009, state) %>% 
#       mutate(
#         COUNTY = substr(GEOID, 1, 5),
#         medrent = medrent*1.20)
#   })
# gc()
# 
# 
# tr_rents <- 
#   bind_rows(tr_rents19, tr_rents09) %>% 
#   unite("variable", c(variable,year), sep = "") %>% 
#   group_by(variable) %>% 
#   spread(variable, medrent) %>% 
#   group_by(COUNTY) %>%
#   mutate(
#     tr_medrent19 = 
#       case_when(
#         is.na(medrent19) ~ median(medrent19, na.rm = TRUE),
#         TRU ~ medrent19
#       ),
#     tr_medrent09 = 
#       case_when(
#         is.na(medrent09) ~ median(medrent09, na.rm = TRUE),
#         TRU ~ medrent09),
#     tr_chrent = tr_medrent19 - tr_medrent09,
#     tr_pchrent = (tr_medrent19 - tr_medrent09)/tr_medrent09,
#     rm_medrent19 = median(tr_medrent19, na.rm = TRUE), 
#     rm_medrent09 = median(tr_medrent09, na.rm = TRUE)) %>% 
#   select(-medrent09, -medrent19) %>% 
#   distinct() %>% 
#   group_by(GEOID) %>% 
#   filter(row_number()==1) %>% 
#   ungroup()
# 
# 
# 
# states <- 
#   raster::union(
#     tracts("IL", cb = TRUE, class = 'sp'), 
#     tracts("GA", cb = TRUE, class = 'sp')) %>%
#   raster::union(tracts("AR", cb = TRUE, class = 'sp')) %>%  
#   raster::union(tracts("TN", cb = TRUE, class = 'sp')) %>%
#   raster::union(tracts("CO", cb = TRUE, class = 'sp')) %>%
#   raster::union(tracts("MS", cb = TRUE, class = 'sp')) %>%
#   raster::union(tracts("AL", cb = TRUE, class = 'sp')) %>%
#   raster::union(tracts("KY", cb = TRUE, class = 'sp')) %>%
#   raster::union(tracts("MO", cb = TRUE, class = 'sp')) %>%
#   raster::union(tracts("IN", cb = TRUE, class = 'sp')) %>%
#   raster::union(tracts("CA", cb = TRUE, class = 'sp')) %>%
#   raster::union(tracts("WA", cb = TRUE, class = 'sp')) %>%
#   raster::union(tracts("OH", cb = TRUE, class = 'sp')) %>%
#   raster::union(tracts("MA", cb = TRUE, class = 'sp')) %>%
#   raster::union(tracts("NH", cb = TRUE, class = 'sp'))
# 
# stsp <- 
#   tracts("CA", cb = TRUE, class = 'sp')
# 
# stsp <- states
# 
# library(sp)
# # join data to these tracts
# stsp@data <-
#   left_join(
#     stsp@data %>% 
#       mutate(GEOID = case_when(
#         !is.na(GEOID.1) ~ GEOID.1, 
#         !is.na(GEOID.2) ~ GEOID.2, 
#         !is.na(GEOID.1.1) ~ GEOID.1.1, 
#         !is.na(GEOID.1.2) ~ GEOID.1.2, 
#         !is.na(GEOID.1.3) ~ GEOID.1.3, 
#         !is.na(GEOID.1.4) ~ GEOID.1.4, 
#         !is.na(GEOID.1.5) ~ GEOID.1.5), 
#       ), 
#     tr_rents, 
#     by = "GEOID") %>% 
#   select(GEOID:rm_medrent09)
# 
# # Create neighbor matrix
# # --------------------------------------------------------------------------
# #-------------------------------------------------------------------------
# coords <- coordinates(stsp)
# IDs <- row.names(as(stsp, "data.frame"))
# stsp_nb <- poly2nb(stsp) # nb
# lw_bin <- nb2listw(stsp_nb, style = "W", zero.policy = TRUE)
# 
# kern1 <- knn2nb(knearneigh(coords, k = 1), row.names=IDs)
# dist <- unlist(nbdists(kern1, coords)); summary(dist)
# max_1nn <- max(dist)
# dist_nb <- dnearneigh(coords, d1=0, d2 = .1*max_1nn, row.names = IDs)
# spdep::set.ZeroPolicyOption(TRUE)
# spdep::set.ZeroPolicyOption(TRUE)
# dists <- nbdists(dist_nb, coordinates(stsp))
# idw <- lapply(dists, function(x) 1/(x^2))
# lw_dist_idwW <- nb2listw(dist_nb, glist = idw, style = "W")
# 
# 
# # Create select lag variables
# # --------------------------------------------------------------------------
# 
# stsp$tr_pchrent.lag <- lag.listw(lw_dist_idwW,stsp$tr_pchrent)
# stsp$tr_chrent.lag <- lag.listw(lw_dist_idwW,stsp$tr_chrent)
# stsp$tr_medrent18.lag <- lag.listw(lw_dist_idwW,stsp$tr_medrent18)
# 
# # ==========================================================================
# # Join lag vars with df
# # ==========================================================================
# 
# lag <-  
#   left_join(
#     df, 
#     stsp@data %>% 
#       mutate(GEOID = as.numeric(GEOID)) %>%
#       select(GEOID, tr_medrent18:tr_medrent18.lag)) %>%
#   mutate(
#     tr_rent_gap = tr_medrent18.lag - tr_medrent18, 
#     tr_rent_gapprop = tr_rent_gap/((tr_medrent18 + tr_medrent18.lag)/2),
#     rm_rent_gap = median(tr_rent_gap, na.rm = TRUE), 
#     rm_rent_gapprop = median(tr_rent_gapprop, na.rm = TRUE), 
#     rm_pchrent = median(tr_pchrent, na.rm = TRUE),
#     rm_pchrent.lag = median(tr_pchrent.lag, na.rm = TRUE),
#     rm_chrent.lag = median(tr_chrent.lag, na.rm = TRUE),
#     rm_medrent17.lag = median(tr_medrent18.lag, na.rm = TRUE), 
#     dp_PChRent = case_when(tr_pchrent > 0 & 
#                              tr_pchrent > rm_pchrent ~ 1, # ∆ within tract
#                            tr_pchrent.lag > rm_pchrent.lag ~ 1, # ∆ nearby tracts
#                            TRU ~ 0),
#     dp_RentGap = case_when(tr_rent_gapprop > 0 & tr_rent_gapprop > rm_rent_gapprop ~ 1,
#                            TRU ~ 0),
#   ) 


# 
# #####################################################
# ##### Prepare median and average values
# ###################################################
# 
# acs_09meds <- select(census_wide09, meds)
# 
# 
# # FIPS filter from Census/ACS data
# fips_filter <-
# paste(acs_09meds %>% select(GEOID) %>% pull(), collapse = "|")
# 
# # Filter crosswalk data by relevant FIPS (reduces interpolation time)
# crosswalk_filter <-
#   ltdb %>%
#   filter(str_detect(trtid00, fips_filter))
# 
# # join to filtered crosswalk
# acs_09meds_xwalk <-
#   acs_09meds %>%
#   full_join(crosswalk_filter, by =c( "GEOID" = "trtid00" ))
#   # group by 2010 tracts
# 
# #aggregate means by new tract id
# acs_09meds_xwalk <- aggregate(acs_09meds_xwalk, list(acs_09meds_xwalk$trtid10), mean)
#   
# 
# ### Join all back together
# 
# acs_09 <-
#   acs_09meds_xwalk %>%
#   full_join(acs_090, by = c("trtid10" = "GEOID"))
# # group by 2010 tracts
# 
# #restore GEOID variable
# acs_09 <-
#   acs_09 %>% 
#     rename(
#       GEOID = trtid10) %>% 
#   mutate(
#     GEOID = as.character(GEOID),
#     GEOID = str_pad(GEOID, 11, side = "left",  pad = "0")
#         ))
#       
# 
# rm(acs_09meds_xwalk)
# rm(acs_09meds)
# rm(acs_090)
