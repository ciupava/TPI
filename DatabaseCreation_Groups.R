#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +                                                                        +
# +  File: DatabaseCreation_Groups                                         +
# +  Aim: making up database for TPI, from databasesfor demographic (US    +
# +       census) and accessibility data (UNIminnesota)                    +
# +  Description: The script uses US Census data at Block Group (BG)       +
# +               level. Inputs: total blocks list (from 2010 US census),  +
# +               metros blocks list (from Uni Minnesota accessibility     +
# +               observatory), groups and tracts from 2014 ACS Tiger US   +
# +               census database                                          +
# +  Authors: Anna Zanchetta                                               +
# +  Date: ~March 2019                                                     +
# +  Version: 1                                                            +
# +                                                                        +
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



library(rgdal) # Necessary for accessing and managing gdb files.
library(raster) # Necessary for accessing and managing gdb files.
library(dplyr) # Necessary to join the two dataframes (Accessibility data and the gdb file) by the value in the columnn Geoid (in the shp the column is called 'geoid'  while in the gdb it is called 'GEOID')
library(sf) # Necessary for reading the (non-spatial) tables from the gdb file, via st_read(dsn = "path_to_gdb", layer = "layer_name")
library(foreign) # Necessary to upload dbf ("database", collateral file of shp) files
library(rgeos)
library(maptools)
#library(plyr) # Necessary for the 'join' function
library(data.table) # for using rbindlist()
library(sqldf) # sql functions in R
library(stringr) # strings manipulation
#library(tidyverse)

detach(package:plyr)

path_to_folder <- '/home/anna/Technion/Research/Data'  # To be changed depending on own need. Path to folder containing all the files, used for the upload


# ~ NEW STUFF ----

# ~X. Uploading Metros names/codes/... ----
metros_total_table_name <- 'MetrosCBSAandCounties.csv' # 417 entries
path_to_metros <- paste(path_to_folder, 
                        metros_total_table_name,
                        sep='/') # Example: "/home/anna/Technion/Research/Trials/MetrosCBSAandCounties.csv"
metros_total_table <-read.csv(path_to_metros,
                              stringsAsFactors=FALSE)
metros_total_df <- as.data.frame(metros_total_table)
## Getting [state + county] code for the 49 metros (5 digits for each country: 2 for the state, 3 for the county in that state):
ID_statecounty_per_metros <- paste(formatC(metros_total_table$StateCode,width = 2, flag = "0"),
                                   formatC(metros_total_table$CountyCode, width = 3, flag = "0"), sep = '')  # Creating the list of useful state+county codes (list) by using a specific width for the column of state (2) and county (3)
ID_statecounty_per_metros -> metros_total_df[, "County_FIPS"] # 417 entries
CBSA_list <- metros_total_table$CBSA
CBSA_singleFIPSvalues <- unique(CBSA_list) #list of single values for metros FIPS (49 values)
metros_names_list <- unique(metros_total_df$MetroName)

metros_codes_list <- data.frame("CBSAfips" = CBSA_singleFIPSvalues,
                                "MetroName" = metros_names_list) #,
write.csv(metros_codes_list, file = "metros_codes_list.csv")

# ~1. UPLOADING all US blocks ----
# (These are the demographic data by blocks from 2010 US Census)
#Reading multiple dbf files, and vertically concatenating them to one big one
list_of_dbfs <- list()
counter <- 1
for (i in list.files(path = paste(path_to_folder,
                                  "UScensus",
                                  "tabblock2010", 
                                  sep = '/'),
                     pattern="*.dbf", 
                     full.names=T, 
                     recursive=FALSE)) {
   print(i)
   single_dbf <- read.dbf(i, 
                          as.is = TRUE) # reads the columns as they are, i.e. character and integer
   single_dbf <- single_dbf[c("BLOCKID10","POP10")]
   list_of_dbfs[[counter]]  <- single_dbf
   counter <- counter + 1
}
full_census_data_by_block_dbf <- rbindlist(list_of_dbfs) # 11078297 obs of 2 variables
str(full_census_data_by_block_dbf)
is.unsorted(full_census_data_by_block_dbf$BLOCKID10) # TRUE ... checking if the vector is sorted or not. Need to need to sort it
full_census_data_by_block_dbf <- full_census_data_by_block_dbf[order(full_census_data_by_block_dbf$BLOCKID10),]

write.csv(full_census_data_by_block_dbf, file = "full_census_data_by_block_dbf.csv")
# once saved, can upload it directly from the csv file:
# full_census_data_by_block_dbf_temp <- read.csv(file = "full_census_data_by_block_dbf.csv")
# full_census_data_by_block_dbf <- full_census_data_by_block_dbf_temp[-1]
# convertingIDs <- as.character(full_census_data_by_block_dbf_temp$BLOCKID10)
# full_census_data_by_block_dbf$BLOCKID10 <- str_pad(as.character(convertingIDs),
#                                                    width=15, pad="0") # .... order them as well?

# ~2. UPLOADING PT ACCESSIBILITY DATA by blocks ----
# "PT" stands for Public Transport, analogous of "Transit" as it is called in the UniMinnesota database/website
## Upload the data accessibility by transit (per block):
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NB: differently from previous versions, where we used the shapefiles, here we are using the csv files. Advantages: the shapefile is heavier and was created by merging in Qgis all the shp of the involved States (this requires long machine computing time and might generate errors)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

try <- "/home/anna/Technion/Research/Data/UniMinnesota/ALL_REGIONS_CSV_tr_2015_0700-0859/47900_tr_2015_0700-0859.csv" # trial with one single metro (47900)
trunk <- gsub(".*0859/(.+)_tr.*", 
              "\\1", 
              try) # Regex sintax... ? Complex, anyways taken from here: https://stackoverflow.com/questions/39086400/extracting-a-string-between-other-two-strings-in-r
# Basically taking everything after "0859/" and before "_tr"
# Explained here: https://stackoverflow.com/questions/23518325/how-to-extract-substring-between-symbol-and
# > it gives "47900"
#nrow(single_csv)
#col <- rep("a", nrow(single_csv))
list_of_csv <- list()
counter <- 1
for (i in list.files(path = paste(path_to_folder,
                                  "UniMinnesota",
                                  "ALL_REGIONS_CSV_tr_2015_0700-0859", 
                                  sep = '/'),
                     pattern = "*.csv", 
                     full.names = T, 
                     recursive = FALSE)) {
   #print(i)
   cbsaFIPS <- gsub(".*0859/(.+)_tr.*", 
                     "\\1", 
                     i)
   print(cbsaFIPS)
   single_csv <- read.csv(i, # reading the single metro csv file
                          as.is = TRUE)
   single_csv$geoid <- str_pad(as.character(single_csv$geoid), # converting the ID to character
                               width = 15,
                               pad = '0')
   single_csv <- dplyr::select(single_csv, # excluding the column 'threshold' from the df
                        IDblock = geoid,
                        JobACC = jobs)
   single_csv <- mutate(single_csv, # adding the CBSA fips (metro code) column
                        cbsaFIPS = rep(cbsaFIPS, # replicating the code n-times with n = df's rows
                                       nrow(single_csv))
                        )
#   single_csv$cbsa_fips <- rep(cbsaFIPS, 
#                               nrow(single_csv))
   list_of_csv[[counter]]  <- single_csv # adding last metro to the total metros list
   counter <- counter + 1
}
# Binding the csv together:
full_PTaccessibility_data_by_block <- rbindlist(list_of_csv) # 3019133 obs of 3 variables
# Checking the final df:
str(full_PTaccessibility_data_by_block)
# Checking if the df is sorted or not, and sorting it:
is.unsorted(full_PTaccessibility_data_by_block$geoid) # FALSE 
full_census_data_by_block_dbf <- full_census_data_by_block_dbf[order(full_census_data_by_block_dbf$BLOCKID10),]

# # checking known towns:
# raleigh <- subset(full_PTaccessibility_data_by_block,
#                   cbsaFIPS == "39580") # 19810 obs. as expected

# saving file in output:
write.csv(full_PTaccessibility_data_by_block, file = "full_PTaccessibility_data_by_block.csv")


# ~2a. Adding population to PT acc data -----
# Adding to the metros blocks (accessibility) their population, from the total list of US blocks
tmp_block_data <- sqldf('SELECT cbsaFIPS as CBSAfips,
                                BLOCKID10 as IDblock,
                                POP10 as POPblock,
                                JobACC as TransitAcc
                        FROM full_census_data_by_block_dbf
                        JOIN full_PTaccessibility_data_by_block
                        ON full_census_data_by_block_dbf.BLOCKID10=full_PTaccessibility_data_by_block.IDblock')
# 3019133 obs. of  3 variables, like the PTaccessibility
is.unsorted(tmp_block_data$ID_block) # FALSE

# ~2b. Adding a column with metros` names to the blocks df:----
tmp_block_data$MetroName <- metros_codes_list[match(tmp_block_data$CBSAfips,
                                                    metros_codes_list$CBSAfips),
                                      "MetroName"] 



# Saving file in output:
write.csv(tmp_block_data, "tmp_block_data.csv")

# #~ Checks -------
# raleigh <- subset(tmp_block_data,
#                   CBSAfips == "39580")
# charlotte <- subset(tmp_block_data,
#                     CBSAfips == "16740")
# counties_charlotte <- unique(substr(charlotte$IDblock, 
#                                     1, 
#                                     5))  # they are only 6: "37007" "37025" "37071" "37119" "37179" "45091"
# tracts_charlotte <- unique(substr(charlotte$IDblock, 
#                                     1, 
#                                     11))
# charlotte$state <- substr(charlotte$IDblock, 
#                        1, 
#                        2)
# metro_shp_name <- sprintf('%s_tr_2015_0700-0859', "16740")
# metro_shp <- readOGR(dsn = "/home/anna/Technion/Research/Data/UniMinnesota/ALL_REGIONS_SHP_tr_2015_0700-0859",
#                      layer = metro_shp_name)
# charlotte_shp <- SpatialPolygonsDataFrame(metro_shp,
#                                           charlotte, 
#                                           match.ID = F)
# writeOGR(obj = charlotte_shp,
#          dsn = '/home/anna/Technion/Research/Results/',
#          layer = "charlotte",
#          driver = "ESRI Shapefile",
#          overwrite_layer = FALSE)
#
# tmp_block_data$state <- substr(tmp_block_data$ID_block, 
#                                1, 
#                                2)
# 
# northcarolina <- subset(tmp_block_data, 
#                         state == "37")

# ~2c. Passing from Blocks to Groups----
# Creating column for Group identification:
block_data <- mutate(tmp_block_data,
                     IDgroup = substr(IDblock, 0, 12))
 
# # group_data_acc <- transform(block_data,
# #                             group_by(IDgroupblock),
# #                             weighted_acc = weighted.mean(TransitAcc, POPblock, na.rm = TRUE))
# # 
# # group_data_acc <- block_data %>%
# #    group_by(IDgroupblock) %>%
# #    transform(weighted_income = weighted.mean(TransitAcc, POPblock, na.rm = TRUE))

# Creating PT accessibility data at BG level by weighted average of the Block data:
group_data_acc <- block_data %>% 
   group_by(IDgroup) %>% # grouping block_data by the group ID
   summarise(PTacc_groupmean = round(weighted.mean(TransitAcc, 
                                             POPblock, 
                                             na.rm = TRUE)
                                     )
             )# weighted average of TransitAcc on block population
# NB: 110694 obs.

# (Re-)adding the CBSA fips by matching the ID group value with the ones from the original block df
group_data_acc$CBSAfips <- block_data[match(group_data_acc$IDgroup, 
                                           block_data$IDgroup),
                                           "CBSAfips"]

# -----~ 3. UPLOADING and TREATING DEMOGRAPHIC DATA ------
# ~3a. GROUP data ----
# Uploading the demographic US census data by Group (gdb files): car ownership and population
census_gdb_name_BG = 'ACS_2014_5YR_BG.gdb'
path_to_census_gdb_BG = paste(path_to_folder,
                           "UScensus",
                           census_gdb_name_BG,
                           sep='/') # Example: path_to_gdb <-'/home/anna/Technion/Research/Trials/ACS_2016_5YR_BG.gdb'
## Uploading specific tables (table) from that gdb:
# (this allows to get the demographic and car-owenrship data)
POPtable_name <- 'X01_AGE_AND_SEX' # Table name
CARStable_name <- 'X25_HOUSING_CHARACTERISTICS'
POPtable_BG <- st_read(path_to_census_gdb_BG,
                    POPtable_name) # Actually uploading the table
POPtable_BG <- POPtable_BG[order(POPtable_BG$GEOID),] # sorting the table
CARStable_BG <- st_read(path_to_census_gdb_BG,
                     CARStable_name)
CARStable_BG <- CARStable_BG[order(CARStable_BG$GEOID),] # sorting the table

# # Obtaining number of vehicles per Block Group by the field B25046e1 from the table 'X25_HOUSING_CHARACTERISTICS'
# cars_per_group <- with(CARStable_BG,
#                        B25046e1)
# # Obtaining the total populatio per group by the field B01001e1 from the table 'X01_AGE_AND_SEX'
# population_per_group <- with(POPtable_BG,
#                          B01001e1)
# # Calculating population over 18 per group by subtracting from the total population (field B01001e1) several fields: ...
# adults_per_group <- with(POPtable_BG,
#                          B01001e1 - B01001e3 - B01001e4 - B01001e5 - B01001e6 - B01001e27 - B01001e28 - B01001e29 - B01001e30)

# Join only the one column with cars data to pop table: # (This is the fastes way to obtain the final results, below, since you always deal with the same table and like this you also make sure that the rows are properly selected)

BGtable_cars <- CARStable_BG %>%
   select(GEOID, B25046e1)

BGtable_join <- left_join(POPtable_BG,
                          BGtable_cars,
                          by = "GEOID")

#can also use this alternative:
BGtable_join <- merge(x = POPtable_BG,
                      y = CARStable_BG[, c("GEOID", "B25046e1")],
                                       by = "GEOID",
                                       all.x = TRUE)
# After the join (whichever way you do it):
# Creating BG dataframe with the relevant columns
group_data <- BGtable_join %>% 
   transmute(IDgroup = substring(GEOID, 8),
             PopGROUP = B01001e1,
             AdultsGROUP = B01001e1 - B01001e3 - B01001e4 - B01001e5 - B01001e6 - B01001e27 - B01001e28 - B01001e29 - B01001e30,
             CarsGROUP = B25046e1
             )

# group_data_df_unordered <- data.frame("IDGROUP" = ID_group,
#                                        "PopGROUP" = population_per_group,
#                                        "AdultsGROUP" = adults_per_group,
#                                        "CarsGROUP" = cars_per_group,
#                                        stringsAsFactors = FALSE)
# number of entries: 220333 groups (in all the US states)
ID_group <- substring(POPtable_BG$GEOID, 8) # Obtaining the group ID by cutting of the "15000US" string at the beginning -> this means selecting the characters from the one at position '8' of the string until the end
ID_group <- sort(ID_group)
group_data_df <- group_data_df[order(group_data_df_unordered$IDGROUP),] # 220333 obs. of 4 variables

write.csv(group_data,
           file = "group_data.csv") # Saving the group data for all the states

# ~3b. TRACTS data ----
## Uploading the demographic from US census data by Tract (gdb files): poverty status and population
census_gdb_name_TR = 'ACS_2014_5YR_TRACT.gdb'
path_to_census_gdb_TR = paste(path_to_folder,
                           "UScensus",
                           census_gdb_name_TR,
                           sep='/') # Example: path_to_gdb <-'/home/anna/Technion/Research/Trials/ACS_2016_5YR_BG.gdb'.
## Uploading specific tables from that gdb:
# (in order to obtain the demographic and car-ownership data)
POPtable_name <- 'X01_AGE_AND_SEX' # Table name
POVERTYtable_name <- 'X17_POVERTY'

POPtable_TR <- st_read(path_to_census_gdb_TR,
                    POPtable_name)
POPtable_TR <- POPtable_TR[order(POPtable_TR$GEOID),] # sorting the table
# \

POVERTYtable_TR <- st_read(path_to_census_gdb_TR,
                        POVERTYtable_name)
POVERTYtable_TR <- POVERTYtable_TR[order(POVERTYtable_TR$GEOID), ] # sorting the table

# ID_tract <- substring(POPtable$GEOID, 8) # Obtaining the tract ID by cutting of the "15000US" string at the beginning -> this means selecting the characters from the one at position '8' of the string until the end
# ID_tract <- sort(ID_tract)

# #Obtaining population by tract from the field B01001e1 - Total population
# pop_tract <- POPtable_tract[order(POPtable_tract$GEOID), 
#                             "B01001e1"] # NB: ordering by Geoid, otherwise creates
# 
# pop_tractPOV <- POVERTYtable[order(POVERTYtable$GEOID), 
#                              "B17001e1"] #POVERTYtable$B17001e1
# difference_pop <- pop_tract - pop_tractPOV
# Dealing with the 'Population for whom poverty status is determined' per tract, from the table 'X17_POVERTY'
# Adults with (income < poverty level) for whom the poverty status is determined (APl):
# pooradultsPOV_tract <- with(POVERTYtable_TR,
#                             (B17001e2 - B17001e4 - B17001e5 - B17001e6 - B17001e7 - B17001e8 - B17001e9 - B17001e18 - B17001e19 - B17001e20 - B17001e21 - B17001e22 - B17001e23)
# )
# # Adults with (income >= poverty level) for whom the poverty status is determined (APh)
# richadultsPOV_tract <- with(POVERTYtable_TR,
#                             (B17001e31 - B17001e33 - B17001e34 - B17001e35 - B17001e36 - B17001e37 - B17001e38 - B17001e47 - B17001e48 - B17001e49 - B17001e50 - B17001e51 - B17001e52)
# )
# # Adults (total) for whom poverty status is determined (AP = APl + APh):
# adultsPOV_tract <- pooradultsPOV_tract + richadultsPOV_tract
# # Adults (total) per tract (A):
# adults_tract <- with(POPtable_tract,
#                      (B01001e1 - B01001e3 - B01001e4 - B01001e5 - B01001e6 - B01001e27 - B01001e28 - B01001e29 - B01001e30))
# # Ratio poor people per tract (R = APl / AP ):
# ratio_poor <- round( pooradultsPOV_tract / adultsPOV_tract, 3)
# # Poor adults per tract (with income < poverty level) (Al):
# pooradults_tract <- round(ratio_poor * adults_tract)
# 
# dfTract <- data.frame("APh" = richadultsPOV_tract,
#                       "APl" = pooradultsPOV_tract,
#                       "AP" = adultsPOV_tract,
#                       "R" = ratio_poor,
#                       "A" = adults_tract,
#                       "Al" = pooradults_tract)
# write.csv(dfTract, file = "TractPopulation.csv")
# diff <- adults_tract - pooradults_tract

# 
# tract_data_df_unordered <- data.frame("IDtract" = ID_tract,
#                                       "AdultsTRACT" = adultsPOV_tract,
#                                       "PoorAdultsTRACT" = pooradultsPOV_tract,
#                                       "CarsTRACT" = cars_per_tract,
#                                       stringsAsFactors = FALSE)
# tract_data_df <- tract_data_df_unordered[order(tract_data_df_unordered$IDtract),] # 74001 objects
# 
# # count(is.na(tract_data_df)) # none
# # tract_data_df$SharePercTRACT[is.na(tract_data_df$SharePercTRACT)] <- 0
# write.csv(tract_data_df,
#           file = "tract_data_fromR.csv") # Saving the tract data for all the states

tract_data <- POVERTYtable_TR %>% 
   transmute(IDtract = substring(GEOID, 8),
             PoorAdultsPOV_tract = B17001e2 - B17001e4 - B17001e5 - B17001e6 - B17001e7 - B17001e8 - B17001e9 - B17001e18 - B17001e19 - B17001e20 - B17001e21 - B17001e22 - B17001e23,
             RichAdultsPOV_tract = B17001e31 - B17001e33 - B17001e34 - B17001e35 - B17001e36 - B17001e37 - B17001e38 - B17001e47 - B17001e48 - B17001e49 - B17001e50 - B17001e51 - B17001e52,
             AdultsPOV_tract = PoorAdultsPOV_tract + RichAdultsPOV_tract)
   

# ~4. CONVERTING TRACTS to GROUPS DATA ----
#~4a. Calculate ratio BGpopulation / tract population (from the BG data) = RAT
group_data_withTract <- group_data %>% # Creating temporary df with one column the tract ID
   mutate(IDtract = substring(IDgroup, 0,11))

group_data_pop <- group_data_withTract %>%
   group_by(IDtract) %>% # grouping by tract ID
   mutate(TotPOPtract_BG = sum(AdultsGROUP), # creating the ratio by popBG / popTR
          RAT = round(AdultsGROUP / TotPOPtract_BG, 3)
   )
#~4b. Adding POPfwd from the tract df to the group df
group_data_pop$AdultsPOV_tract <- tract_data[match(group_data_pop$IDtract,
                                                    tract_data$IDtract),
                                              "AdultsPOV_tract"]
group_data_pop$PoorAdultsPOV_tract <- tract_data[match(group_data_pop$IDtract,
                                                   tract_data$IDtract),
                                             "PoorAdultsPOV_tract"]

#~4b. Calculate Population for Whom Poverty status is determined at BG level as:
# POPfwd_BG = [RAT * POPfwd_tract]
#~4c. Calculate Poor Population for Whom Poverty status is determined at BG level as:
# PoorPOPfwd_BG = [RAT * PoorPOPfwd_tract]
#~4d. Calculating car-less people as CarlessPOP_BG = POPfwd_BG - cars
#~4e. Obtaining ratio poor people: RATpoor = PoorPOPfwd_BG / POPfwd_BG
#~4f. Obtaining ratio carless people: RARcarless = CarlessPOPfwd_BG / POPfwd_BG
group_data_tot <- group_data_pop %>%
   transmute(IDgroup = IDgroup,
             POPfwd = round(RAT * AdultsPOV_tract),
             PoorPOPfwd = round(RAT * PoorAdultsPOV_tract),
             CarlessPOP = case_when(!(POPfwd - CarsGROUP >0) ~ 0, # assignig value 0 when cars>pop (ie the difference [pop - cars] is negative)
                                    TRUE ~ POPfwd - CarsGROUP), # assigning value pop - cars elsewhere
             RATpoor = round (PoorPOPfwd / POPfwd, 3),
             RATcarless = round(CarlessPOP / POPfwd, 3)
   )

#~5. FINALIZING DATABASE ----
# Selecting metros from the total US database (ie putting together accessibility and demographics data)

group_data_metros <- sqldf('SELECT group_data_tot.IDgroup as IDgroup,
                           CBSAfips as CBSAfips,
                           POPfwd as POP,
                           PoorPOPfwd as PoorPOP,
                           CarlessPOP as CarlessPOP,
                           RATpoor as RATpoor,
                           RATcarless as RATcarless,
                           PTacc_groupmean as PTacc
                        FROM group_data_tot
                        JOIN group_data_acc
                        ON group_data_tot.IDgroup=group_data_acc.IDgroup')

# ~2b. Adding a column with metros` names to the groups df:
group_data_metros$MetroName <- metros_codes_list[match(group_data_metros$CBSAfips,
                                                    metros_codes_list$CBSAfips),
                                              "MetroName"] 
# Reordering columns:
group_data_metros <- group_data_metros %>%
   select (-2, everything())

# NB: they are only 110692 obs. instead of 110694... why??

write.csv(group_data_metros,
          file = "group_data_metros.csv")

# ~ OLD STUFF ----

# * 3 - it was: Dealing with the metros ------
# metros_total_table_name <- 'MetrosCBSACounties_EDITED.csv' # 430 entries
# path_to_metros <- paste(path_to_folder, 
#                         metros_total_table_name,
#                         sep='/') # Example: "/home/anna/Technion/Research/Trials/MetrosCBSAandCounties.csv"
# metros_total_table <-read.csv(path_to_metros, # 430 entries
#                               stringsAsFactors=FALSE)
# metros_total_df <- as.data.frame(metros_total_table)
# metros_total_df <- metros_total_df[order(metros_total_df$CBSA), ]
# ## Getting [state + county] code for the 49 metros (5 digits for each country: 2 for the state, 3 for the county in that state):
# ID_statecounty_per_metros <- paste(formatC(metros_total_table$StateCode,width = 2, flag = "0"),
#                                    formatC(metros_total_table$CountyCode, width = 3, flag = "0"), sep = '')  # Creating the list of useful state+county codes (list) by using a specific width for the column of state (2) and county (3)
# ID_statecounty_per_metros -> metros_total_df[, "County_FIPS"] # 430 entries
# CBSA_list <- metros_total_table$CBSA
# CBSA_singleFIPSvalues <- unique(CBSA_list) #list of single values for metros FIPS (49 values)
# metros_names_list <- unique(metros_total_df$MetroName)
# metros_codes_list <- data.frame("CBSAfips" = CBSA_singleFIPSvalues,
#                                 "MetroName" = metros_names_list) 
# #write.csv(metros_codes_list, file = "metros_codes_list.csv")

# * 4 - it was: accessibility data uploading ----
# * 5 - it was: DEALING WITH COUNTIES AND METROS CODES ----
# Getting the list of county codes [state + county] for the 49 metros
# counties_per_metro_df <- data.frame()
# blocks_list <- unique(substr(ID_block, 1, 5)) # 407 objects
# for (i in ID_statecounty_per_metros) { # looping in the list counties from the metros list
#    cbsa <- metros_total_table[metros_total_df$County_FIPS == i, "CBSA"]
#    if(i %in% blocks_list){ # selecting block IDs with same first 5 digits as ID_block
#       #print(i) ## For Raleigh (39580), in North Carolina (state code 37), this gives: [1] "37069", [1] "37101", [1] "37183"
#       county <- i #counties_per_metro_list <- append(counties_per_metro_list,i)
#       #counties_per_metro_df <- rbindlist(list(counties_per_metro_df,data.frame(ID_county = i, CBSA = cbsa)))
#       counties_per_metro_df <- rbind(counties_per_metro_df,data.frame(ID_county = i, CBSA = cbsa))
#    }
# }
# str(counties_per_metro_df) # 394 entries
# 
# write.csv(ID_statecounty_per_metros, file = "statecountyID_metros_fromR.csv") # 430 entries, from metroCBSAcounties list
# write.csv(counties_per_metro_df, file = "counties_permetro_fromR.csv") # 407 entries, from transit acc
# counties_per_metro_df$ID_county
# # Because the two lists have different size (the transit acc have 23 entries less than the list obtained from the file I found), trying to understand what is the difference between them:
# counties_not_in_transitAcc <- ID_statecounty_per_metros[!(
#    ID_statecounty_per_metros %in% counties_per_metro_df$ID_county)] # selecting counties that are in the big list but not in the transit acc # 23 obj.
# pro <- as.character(counties_per_metro_df$ID_county) #  creating column for next step
# counties_not_in_totaltable <- pro[!(
#    pro %in% ID_statecounty_per_metros)] # selecting counties that are in the transit acc list but not in the big list # 0 obj
# # In another way, same thing:
# count(setdiff(ID_statecounty_per_metros, pro)) # 23
# count(setdiff(pro, ID_statecounty_per_metros)) # 0
# # Conclusion: all the transit acc are included in the big list, but there are 23 counties (from the big list) that have not been included in the transit acc study
# # can't know why



# * 6 - it was: CREATING GROUPS DATA -----

# ## BACK TO GROUPS, selecting only groups that belong to the 49 metros, by using the ID_block list
# # loop on the groups to obtain the ratio adults/pop and car/pop for the metro area
# #group_metros_data <- data.frame() #creating empty dataframe to select group data for a specific metro from the State database, and has new column necessary for AFI calculation (adults to pop ratio and car to pop ratio at the group level)
# 
# group_data_df$CountyID <- substr(ID_group, 1, 5)
# 
# group_metros_selection <- subset(group_data_df, CountyID %in% metros_total_df$County_FIPS) #112246 obs.
# group_metros_data_tmp <- transform(group_metros_selection,
#                                    AdultsPopRatio = round(AdultsGROUP / PopGROUP, 3))
# group_metros_data_tmp2 <- transform(group_metros_data_tmp,
#                                CarsPopRatio = round(CarsGROUP / AdultsGROUP, 3))
# group_metros_data <- group_metros_data_tmp2
# group_metros_data$CBSAfips <- metros_total_df[match(group_metros_data$CountyID, 
#                                                     metros_total_df$County_FIPS), 
#                                               "CBSA"]
# rownames(group_metros_data) <- seq(length = nrow(group_metros_data))
# write.csv(group_metros_data, file = "group_metros_data.csv") # 112246 obs.
# 
# 
# ## DEBUGGING:
# # Averaging the CarsPopRatio in order to add it to the groups missing data (will be necessary to have a value for the CarsPopRatio once calculating the people without cars, at the block level)
# cars_pop_ratio_mean <- numeric()
# for (i in CBSA_singleFIPSvalues){
#    # mean of [car to pop ratio] calculated for each metros
#    mean_carspop <- as.numeric(mean(group_metros_data[group_metros_data$CBSAfips == i, "CarsPopRatio"], na.rm = TRUE)) ## in case it doesn't work using saved copy of the metros df, try using colMeans instead of the regular mean function
#    cars_pop_ratio_mean <- append(cars_pop_ratio_mean, mean_carspop)
# }
# cars_pop_ratio_mean_df <- data.frame("CBSA" = CBSA_singleFIPSvalues, "CarsPopRatio_Metro" = round(cars_pop_ratio_mean, 3)  )
# 
# data_frame_saving <- group_metros_data # just in case the following is gonna make problems to the df
# 
# sum(is.nan(group_metros_data$CarsPopRatio)) # > 15900 checking the number of NA values in the CarsPopRatio column
# rownumber <- 0
# for (i in group_metros_data$CarsPopRatio){
#    rownumber <- rownumber + 1
#    if (is.na(i)) { # condition for NA values
#       # assigning value to the NA cell (rownumber = number of iteration) depending on the CBSA value on the same row - when this is equal to the CBSA values from the df cars_pop_ratio_mean
#       # c <- cars_pop_ratio_mean_df[cars_pop_ratio_mean_df$CBSA == group_metros_data[rownumber, "CBSAfips"], "CarsPopRatio_Metro"]
#       # print(c)
#       group_metros_data[rownumber, "CarsPopRatio"] <- cars_pop_ratio_mean_df[cars_pop_ratio_mean_df$CBSA == group_metros_data[rownumber, "CBSAfips"], "CarsPopRatio_Metro"]
#    }
# }
# 
# # Setting AdultsToPop Ratio equal to zero for the cases when both adults and population (group) are equal to zero, giving a NaN ("not a number") value. Actually this means there is no population, so the value "0" is used instead
# #sum(is.nan(group_metros_data$AdultsPopRatio)) # >604 checking for NaN values in the AdultsPopRatio column
# group_metros_data$AdultsPopRatio[is.nan(group_metros_data$AdultsPopRatio)] <- 0
# 
# write.csv(group_metros_data, file = "group_metros_fromR.csv")
# # # once saved, can upload it directly from the csv file:
# group_metros_data_temp <- read.csv(file = "group_metros_fromR.csv",
#                                    stringsAsFactors = FALSE)
# group_metros_data <- group_metros_data_temp[-1] # taking off first column, made of the IDs (1,2,3,...) and called "X"
# group_metros_data$IDGROUP <- str_pad(as.character(group_metros_data$IDGROUP),
#                                       width = 12,
#                                       pad = '0')
# group_metros_data <- group_metros_data[order(group_metros_data$IDGROUP), ]
# # summary(group_metros_data_temp)
# # 
# 
# # * 7 - it was: CREATING TRACTS DATA ----
# 
# tract_data_df$CountyID <- substr(ID_tract, 1, 5)
# tract_metros_selection <- subset(tract_data_df, CountyID %in% metros_total_df$County_FIPS) # 38544 obs.
# # tract_metros_data_tmp <- transform(tract_metros_selection,
# #                                    PoorPopRatio = round( PopPoorTRACT / PopTRACT, 3 ))
# tract_metros_data <- tract_metros_selection
# #count(tract_metros_data$PopPoorTRACT > tract_metros_data$PopTRACT) # NONE
# tract_metros_data$CBSAfips <- metros_total_df[match(tract_metros_data$CountyID, 
#                                                     metros_total_df$County_FIPS), 
#                                               "CBSA"]
# rownames(tract_metros_data) <- seq(length = nrow(tract_metros_data))
# write.csv(tract_metros_data, file = "tract_metros_data.csv") # 38544 obs.
# 
# #* 8 - it was: BLOCKS ANALYSIS ------
# 
# ### Adding a column for the next step, on order to be able to access the groups/tracts IDs with the blocks IDs
# tmp_block_data["ID_block_group"] <- sort(substr(tmp_block_data$ID_block, 1, 12))
# tmp_block_data["ID_block_tract"] <- sort(substr(tmp_block_data$ID_block, 1, 11))
# 
# # 8a. Treating BLOCKS for car analysis (GROUPS)
# ### 2nd STEP: creating a database with all the info by block necessary for the AFI calculation, selecting the metros blocks from the group data
# tmp_block_data2 <- sqldf('SELECT CBSAfips, ID_block, PopBLOCK, TransitAcc, AdultsPopRatio, CarsPopRatio
#                          FROM tmp_block_data 
#                          JOIN group_metros_data 
#                          ON tmp_block_data.ID_block_group=group_metros_data.IDGROUP') # finally 3019133 obs # 3019075 obs. # 2998599 obs.
# 
# # 3019133 - 2998599 = 20534 # Entries available in transitacc that miss in the group_metros_data (obtained by the 2014 US Census by Groups)
# # Adding the missing counties, brought to have only 58 blocks left (= 3019133 - 3019075), then added the missing groups (2) to the Los Angeles county, that being the only one available in the metro doesn't add counties to the list... Found the two missing groups from the lines below
# # blocks_not_in_groups <- tmp_block_data[!(tmp_block_data$ID_block %in% tmp_block_data2$ID_block),] # 58
# # write.csv(blocks_not_in_groups, file = "missing_blocks_LAST.csv") # previous file, with 20534 blocks, is 'missing_blocks.csv'
# # missing_groups <- unique(substr(blocks_not_in_groups$ID_block, 1, 12)) # "060378002043" "060379304011"
# # # ... solved this by adding the two groups to the groups_data (see above)
# 
# count(tmp_block_data2$CarsPopRatio>1) # 648502 # 441042
# 
# block_metros_dataCAR <- tmp_block_data2
# block_metros_dataCAR <- transform (block_metros_dataCAR, 
#                                    AdultsBLOCK = round(AdultsPopRatio * PopBLOCK))
# count(block_metros_dataCAR$CarsPopRatio >= 1) # 653890 # 444327
# ### ???? block_metros_data$CarsPopRatio[block_metros_data$CarsPopRatio >= 1] <- 0
# 
# # This can't work with IF statement: block_metros_data <- transform (block_metros_data, PeopleNoCarBLOCK = round(AdultsBLOCK * (1 - CarsPopRatio)))
# testFunc <- function(adults, carpopratio){
#    if (carpopratio >= 1){
#       #print(sprintf("carpopratio is >=1: %f", carpopratio))
#       people_without_car <- 0
#       #print(sprintf("**** no car people: %f", people_without_car))
#    } else {
#       people_without_car <- round(adults * (1 - carpopratio))
#       #print(sprintf("carpopratio is ok: %f", carpopratio))
#       #print(sprintf("no car people: %f", people_without_car))
#    }
#    people_without_car
# }
# test <- mapply(testFunc, block_metros_dataCAR$AdultsBLOCK, block_metros_dataCAR$CarsPopRatio)
# block_metros_dataCAR$PeopleNoCarBLOCK <- test
# 
# percFunc <- function(carpopratio){
#    if (carpopratio >= 1){
#       #print(sprintf("carpopratio is >=1: %f", carpopratio))
#       percNoCar <- 0
#       #print(sprintf("**** no car people: %f", people_without_car))
#    } else {
#       percNoCar <- round((1 - carpopratio), 3)
#       #print(sprintf("carpopratio is ok: %f", carpopratio))
#       #print(sprintf("no car people: %f", people_without_car))
#    }
#    percNoCar
# }
# perc <- mapply(percFunc, block_metros_dataCAR$CarsPopRatio)
# block_metros_dataCAR$PercNoCar <- perc
# 
# write.csv(block_metros_dataCAR, file = "block_metros_data_CARS.csv")
# 
# # 8b. Treating BLOCKS for income analysis (TRACTS)
# tmp_block_data2inc <- sqldf('SELECT CBSAfips, ID_block, PopBLOCK, TransitAcc, RatioPoorPopTRACT as PoorPopRatio
#               FROM tmp_block_data
#               JOIN tract_metros_data
#               ON tmp_block_data.ID_block_tract=tract_metros_data.IDTRACT')
# 
# block_metros_dataINCOME <- tmp_block_data2inc
# block_metros_dataINCOME <- transform (block_metros_dataINCOME, 
#                                    PoorBLOCK = round(PoorPopRatio * PopBLOCK))
# write.csv(block_metros_dataINCOME, file = "block_metros_data_INCOME.csv")
# 
# # 8c. Joining the two tables
# block_metros_dataTOT <- sqldf('SELECT block_metros_dataINCOME.CBSAfips, block_metros_dataINCOME.ID_block , block_metros_dataINCOME.TransitAcc, block_metros_dataINCOME.PopBLOCK, AdultsPopRatio, AdultsBLOCK, PoorPopRatio, PoorBLOCK, PercNoCar, PeopleNoCarBLOCK
#               FROM block_metros_dataINCOME
#               JOIN block_metros_dataCAR USING(ID_block)')
# # block_metros_dataTOT <- sqldf('SELECT block_metros_dataINCOME.CBSAfips, block_metros_dataINCOME.ID_block , block_metros_dataINCOME.TransitAcc, block_metros_dataINCOME.PopBLOCK, AdultsPopRatio, AdultsBLOCK, PoorPopRatio, PoorBLOCK, PercNoCar, PeopleNoCarBLOCK
# #               FROM block_metros_dataINCOME
# #               JOIN block_metros_dataCAR
# #               ON block_metros_dataINCOME.ID_block = block_metros_dataCAR.ID_block')
# 
# write.csv(block_metros_dataTOT, file = "block_metros_data_TOT.csv")
# 
#  
# #* 9 - it was: dealing with the total population blocks/tracts ----
# blocks <- block_metros_dataTOT
# blocks$IDtractblock <- substring(blocks$ID_block, 1, 11)
# c <- unique(blocks$IDtractblock) # 38050
# h <- unique(blocks$IDtractgroup) # 38050
# summing_pop_tract <- aggregate(blocks$PopBLOCK, 
#                                by = list(tractblockID = blocks$IDtractblock),
#                                FUN = sum) # 38050, I was expecting 38544
# d <- setdiff(c, tract_metros_data$IDTRACT ) # 0
# e <- setdiff(tract_metros_data$IDTRACT, c) # 494
# write.csv(e, "blocks_NOTinTransitAcc.csv")
# 
# #----------------------  HERE!!!!!!!!!!!!!!!!!
# # decide whether to add the new info on block pop on the total tract data or on the metros tract data
# # then graphs?
# 
# tract_pop_temp <- tract_pop_df # tract_metros_data
# #tract_pop_df$popBLOCKtract <- summing_pop_tract$x ## can't work, because the num of rows is different (38050 instead of 38544)
# 
# 
# tract_group_metros_pop <- subset(tract_pop_df,
#                                  IDTRACT %in% tract_metros_data$IDTRACT)
# 
# tract_group_metros_pop$popBLOCKtract <- summing_pop_tract[match(tract_group_metros_pop$IDTRACT,
#                                                         summing_pop_tract$tractblockID),
#                                                         "x"]
# 
# metros_pop_tract_final <- transform(tract_group_metros_pop,
#                               ratioBlockTractPOP = round(popBLOCKtract / PopTRACT ,2))
#    
# 
# write.csv(metros_pop_tract_final, file = "tract_data_POPcomparison.csv")
# 
# dput(summary(metros_pop_tract_final),file="summary_POPmetros_tract.csv",control="all")
# 
# out <-capture.output(summary(metros_pop_tract_final))
# cat(out,file="summary_POPmetros_tract.doc",sep="\n",append=TRUE)
# 
# # Understanding the issue:
# #In fact in the tract_metros_data or also in group_metros_data some objects are available that don't exist in the transit_accessibility_dbf (494 blocks). My guess is that the PopBLOCK in these blocks is 0, and therefore they have not been taken into account in the transit acc analysis.
# # The only way is to check in the original big database for blocks, but I am scared that R is gonna crash again with all these datas uploaded.
# id_blockstracts <- substring(full_census_data_by_block_dbf$BLOCKID10, 1, 11)
# newdf <- full_census_data_by_block_dbf
# newdf$IDtracts <- id_blockstracts
# try <- subset(newdf, IDtracts %in% e)
# summary(try)
