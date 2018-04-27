library(tidyverse)
# Tidy data for all Port wine sales, 2007 to 2016
# to create the csv file:
# Data from www.ivdp.pt
# 1. open xls file in Portuguese language.
# 2. Save as .csv.
# 3. set field delimiter to ';'.
# 4. Use read_csv2() for ; delimited files.
#
# Only 5 years at a time can be selected, that's why there is two 5 year files. 2017 came later
# I have translated the country name using google translate. I manually added the continent and region columns.

#read in the csv files
ports_07_11 <- read_csv2("all-ports-07-11.csv", skip = 4 , na="---")
ports_12_16 <- read_csv2("all-ports-12-16.csv", skip = 4 , na="---")
ports_17 <- read_csv2("all-ports-17.csv", skip = 4 , na="---")


# Tidying data port type by port type (11 types!). Should replace with a function
#####################
# White

White_litres_07_11<-ports_07_11 %>% 
        select(country, continent, region, Litros, Litros_11, Litros_22, Litros_33, Litros_44) %>% 
        rename('2007_White'= 'Litros', '2008_White'='Litros_11', 
               '2009_White'='Litros_22', '2010_White'='Litros_33', '2011_White'='Litros_44') %>% 
        gather('2007_White','2008_White','2009_White','2010_White','2011_White', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

White_euro_07_11<-ports_07_11 %>% 
        select(country, continent, region, '€', '€_11', '€_22', '€_33', '€_44') %>% 
        rename('2007_White'= '€', '2008_White'='€_11', 
               '2009_White'='€_22', '2010_White'='€_33', '2011_White'='€_44') %>% 
        gather('2007_White','2008_White','2009_White','2010_White','2011_White', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

White_07_11 <- left_join(White_litres_07_11, White_euro_07_11, by = c('country', 'continent','region', 'year', "port"))

White_litres_12_16<-ports_12_16 %>% 
        select(country, continent, region, Litros, Litros_11, Litros_22, Litros_33, Litros_44) %>% 
        rename('2012_White'= 'Litros', '2013_White'='Litros_11', 
               '2014_White'='Litros_22', '2015_White'='Litros_33', '2016_White'='Litros_44') %>% 
        gather('2012_White','2013_White','2014_White','2015_White','2016_White', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

White_euro_12_16<-ports_12_16 %>% 
        select(country, continent, region, '€', '€_11', '€_22', '€_33', '€_44') %>% 
        rename('2012_White'= '€', '2013_White'='€_11', 
               '2014_White'='€_22', '2015_White'='€_33', '2016_White'='€_44') %>% 
        gather('2012_White','2013_White','2014_White','2015_White','2016_White', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

White_12_16 <- left_join(White_litres_12_16, White_euro_12_16, by = c('country','continent','region', 'year', "port"))

White_litres_17<-ports_17 %>% 
        select(country, continent, region, Litros) %>% 
        rename('2017_White'= 'Litros') %>% 
        gather('2017_White', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

White_euro_17<-ports_17 %>% 
        select(country, continent, region, '€') %>% 
        rename('2017_White'= '€') %>% 
        gather('2017_White', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")
White_17 <-left_join(White_litres_17, White_euro_17, by = c('country','continent','region', 'year', "port"))
White_07_17 <- bind_rows(White_07_11,White_12_16, White_17)


#############
#Tawny

Tawny_litres_07_11<-ports_07_11 %>% 
        select(country, continent, region, Litros_1, Litros_12, Litros_23, Litros_34, Litros_45) %>% 
        rename('2007_Tawny'= 'Litros_1', '2008_Tawny'='Litros_12', 
               '2009_Tawny'='Litros_23', '2010_Tawny'='Litros_34', '2011_Tawny'='Litros_45') %>% 
        gather('2007_Tawny','2008_Tawny','2009_Tawny','2010_Tawny','2011_Tawny', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Tawny_euro_07_11<-ports_07_11 %>% 
        select(country, continent, region, '€_1', '€_12', '€_23', '€_34', '€_45') %>% 
        rename('2007_Tawny'= '€_1', '2008_Tawny'='€_12', 
               '2009_Tawny'='€_23', '2010_Tawny'='€_34', '2011_Tawny'='€_45') %>% 
        gather('2007_Tawny','2008_Tawny','2009_Tawny','2010_Tawny','2011_Tawny', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Tawny_07_11 <- left_join(Tawny_litres_07_11, Tawny_euro_07_11, by = c('country', 'continent','region', 'year', "port"))

Tawny_litres_12_16<-ports_12_16 %>% 
        select(country, continent, region, Litros_1, Litros_12, Litros_23, Litros_34, Litros_45) %>% 
        rename('2012_Tawny'= 'Litros_1', '2013_Tawny'='Litros_12', 
               '2014_Tawny'='Litros_23', '2015_Tawny'='Litros_34', '2016_Tawny'='Litros_45') %>% 
        gather('2012_Tawny','2013_Tawny','2014_Tawny','2015_Tawny','2016_Tawny', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Tawny_euro_12_16<-ports_12_16 %>% 
        select(country, continent, region, '€_1', '€_12', '€_23', '€_34', '€_45') %>% 
        rename('2012_Tawny'= '€_1', '2013_Tawny'='€_12', 
               '2014_Tawny'='€_23', '2015_Tawny'='€_34', '2016_Tawny'='€_45') %>% 
        gather('2012_Tawny','2013_Tawny','2014_Tawny','2015_Tawny','2016_Tawny', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")
Tawny_12_16 <- left_join(Tawny_litres_12_16, Tawny_euro_12_16, by = c('country','continent','region', 'year', "port"))

Tawny_litres_17<-ports_17 %>% 
        select(country, continent, region, Litros_1) %>% 
        rename('2017_Tawny'= 'Litros_1') %>% 
        gather('2017_Tawny', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Tawny_euro_17<-ports_17 %>% 
        select(country, continent, region, '€_1') %>% 
        rename('2017_Tawny'= '€_1') %>% 
        gather('2017_Tawny', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")
Tawny_17 <-left_join(Tawny_litres_17, Tawny_euro_17, by = c('country','continent','region', 'year', "port"))
Tawny_07_17 <- bind_rows(Tawny_07_11,Tawny_12_16, Tawny_17)


##############################
# Ruby

Ruby_litres_07_11<-ports_07_11 %>% 
        select(country, continent, region, Litros_2, Litros_13, Litros_24, Litros_35, Litros_46) %>% 
        rename('2007_Ruby'= 'Litros_2', '2008_Ruby'='Litros_13', 
               '2009_Ruby'='Litros_24', '2010_Ruby'='Litros_35', '2011_Ruby'='Litros_46') %>% 
        gather('2007_Ruby','2008_Ruby','2009_Ruby','2010_Ruby','2011_Ruby', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Ruby_euro_07_11<-ports_07_11 %>% 
        select(country, continent, region, '€_2', '€_13', '€_24', '€_35', '€_46') %>% 
        rename('2007_Ruby'= '€_2', '2008_Ruby'='€_13', 
               '2009_Ruby'='€_24', '2010_Ruby'='€_35', '2011_Ruby'='€_46') %>% 
        gather('2007_Ruby','2008_Ruby','2009_Ruby','2010_Ruby','2011_Ruby', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Ruby_07_11 <- left_join(Ruby_litres_07_11, Ruby_euro_07_11, by = c('country', 'continent','region', 'year', "port"))

Ruby_litres_12_16<-ports_12_16 %>% 
        select(country, continent, region, Litros_2, Litros_13, Litros_24, Litros_35, Litros_46) %>% 
        rename('2012_Ruby'= 'Litros_2', '2013_Ruby'='Litros_13', 
               '2014_Ruby'='Litros_24', '2015_Ruby'='Litros_35', '2016_Ruby'='Litros_46') %>% 
        gather('2012_Ruby','2013_Ruby','2014_Ruby','2015_Ruby','2016_Ruby', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Ruby_euro_12_16<-ports_12_16 %>% 
        select(country, continent, region, '€_2', '€_13', '€_24', '€_35', '€_46') %>% 
        rename('2012_Ruby'= '€_2', '2013_Ruby'='€_13', 
               '2014_Ruby'='€_24', '2015_Ruby'='€_35', '2016_Ruby'='€_46') %>% 
        gather('2012_Ruby','2013_Ruby','2014_Ruby','2015_Ruby','2016_Ruby', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Ruby_12_16 <- left_join(Ruby_litres_12_16, Ruby_euro_12_16, by = c('country','continent','region', 'year', "port"))

Ruby_litres_17<-ports_17 %>% 
        select(country, continent, region, Litros_2) %>% 
        rename('2017_Ruby'= 'Litros_2') %>% 
        gather('2017_Ruby', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Ruby_euro_17<-ports_17 %>% 
        select(country, continent, region, '€_2') %>% 
        rename('2017_Ruby'= '€_2') %>% 
        gather('2017_Ruby', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")
Ruby_17 <-left_join(Ruby_litres_17, Ruby_euro_17, by = c('country','continent','region', 'year', "port"))

Ruby_07_17 <- bind_rows(Ruby_07_11,Ruby_12_16, Ruby_17)

#########################
# Rose

Rose_litres_07_11<-ports_07_11 %>% 
        select(country, continent, region, Litros_3, Litros_14, Litros_25, Litros_36, Litros_47) %>% 
        rename('2007_Rose'= 'Litros_3', '2008_Rose'='Litros_14', 
               '2009_Rose'='Litros_25', '2010_Rose'='Litros_36', '2011_Rose'='Litros_47') %>% 
        gather('2007_Rose','2008_Rose','2009_Rose','2010_Rose','2011_Rose', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Rose_euro_07_11<-ports_07_11 %>% 
        select(country, continent, region, '€_3', '€_14', '€_25', '€_36', '€_47') %>% 
        rename('2007_Rose'= '€_3', '2008_Rose'='€_14', 
               '2009_Rose'='€_25', '2010_Rose'='€_36', '2011_Rose'='€_47') %>% 
        gather('2007_Rose','2008_Rose','2009_Rose','2010_Rose','2011_Rose', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Rose_07_11 <- left_join(Rose_litres_07_11, Rose_euro_07_11, by = c('country', 'continent','region', 'year', "port"))

Rose_litres_12_16<-ports_12_16 %>% 
        select(country, continent, region, Litros_3, Litros_14, Litros_25, Litros_36, Litros_47) %>% 
        rename('2012_Rose'= 'Litros_3', '2013_Rose'='Litros_14', 
               '2014_Rose'='Litros_25', '2015_Rose'='Litros_36', '2016_Rose'='Litros_47') %>% 
        gather('2012_Rose','2013_Rose','2014_Rose','2015_Rose','2016_Rose', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Rose_euro_12_16<-ports_12_16 %>% 
        select(country, continent, region, '€_3', '€_14', '€_25', '€_36', '€_47') %>% 
        rename('2012_Rose'= '€_3', '2013_Rose'='€_14', 
               '2014_Rose'='€_25', '2015_Rose'='€_36', '2016_Rose'='€_47') %>% 
        gather('2012_Rose','2013_Rose','2014_Rose','2015_Rose','2016_Rose', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Rose_12_16 <- left_join(Rose_litres_12_16, Rose_euro_12_16, by = c('country','continent','region', 'year', "port"))

Rose_litres_17<-ports_17 %>% 
        select(country, continent, region, Litros_3) %>% 
        rename('2017_Rose'= 'Litros_3') %>% 
        gather('2017_Rose', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Rose_euro_17<-ports_17 %>% 
        select(country, continent, region, '€_3') %>% 
        rename('2017_Rose'= '€_3') %>% 
        gather('2017_Rose', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")
Rose_17 <-left_join(Rose_litres_17, Rose_euro_17, by = c('country','continent','region', 'year', "port"))

Rose_07_17 <- bind_rows(Rose_07_11,Rose_12_16, Rose_17)

###############################
# Vintage

Vintage_litres_07_11<-ports_07_11 %>% 
        select(country, continent, region, Litros_4, Litros_15, Litros_26, Litros_37, Litros_48) %>% 
        rename('2007_Vintage'= 'Litros_4', '2008_Vintage'='Litros_15', 
               '2009_Vintage'='Litros_26', '2010_Vintage'='Litros_37', '2011_Vintage'='Litros_48') %>% 
        gather('2007_Vintage','2008_Vintage','2009_Vintage','2010_Vintage','2011_Vintage', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Vintage_euro_07_11<-ports_07_11 %>% 
        select(country, continent, region, '€_4', '€_15', '€_26', '€_37', '€_48') %>% 
        rename('2007_Vintage'= '€_4', '2008_Vintage'='€_15', 
               '2009_Vintage'='€_26', '2010_Vintage'='€_37', '2011_Vintage'='€_48') %>% 
        gather('2007_Vintage','2008_Vintage','2009_Vintage','2010_Vintage','2011_Vintage', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Vintage_07_11 <- left_join(Vintage_litres_07_11, Vintage_euro_07_11, by = c('country', 'continent','region', 'year', "port"))

Vintage_litres_12_16<-ports_12_16 %>% 
        select(country, continent, region, Litros_4, Litros_15, Litros_26, Litros_37, Litros_48) %>% 
        rename('2012_Vintage'= 'Litros_4', '2013_Vintage'='Litros_15', 
               '2014_Vintage'='Litros_26', '2015_Vintage'='Litros_37', '2016_Vintage'='Litros_48') %>% 
        gather('2012_Vintage','2013_Vintage','2014_Vintage','2015_Vintage','2016_Vintage', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Vintage_euro_12_16<-ports_12_16 %>% 
        select(country, continent, region, '€_4', '€_15', '€_26', '€_37', '€_48') %>% 
        rename('2012_Vintage'= '€_4', '2013_Vintage'='€_15', 
               '2014_Vintage'='€_26', '2015_Vintage'='€_37', '2016_Vintage'='€_48') %>% 
        gather('2012_Vintage','2013_Vintage','2014_Vintage','2015_Vintage','2016_Vintage', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Vintage_12_16 <- left_join(Vintage_litres_12_16, Vintage_euro_12_16, by = c('country','continent','region', 'year', "port"))

Vintage_litres_17<-ports_17 %>% 
        select(country, continent, region, Litros_4) %>% 
        rename('2017_Vintage'= 'Litros_4') %>% 
        gather('2017_Vintage', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Vintage_euro_17<-ports_17 %>% 
        select(country, continent, region, '€_4') %>% 
        rename('2017_Vintage'= '€_4') %>% 
        gather('2017_Vintage', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")
Vintage_17 <-left_join(Vintage_litres_17, Vintage_euro_17, by = c('country','continent','region', 'year', "port"))

Vintage_07_17 <- bind_rows(Vintage_07_11,Vintage_12_16, Vintage_17)

#######################
# LBV

LBV_litres_07_11<-ports_07_11 %>% 
        select(country, continent, region, Litros_5, Litros_16, Litros_27, Litros_38, Litros_49) %>% 
        rename('2007_LBV'= 'Litros_5', '2008_LBV'='Litros_16', 
               '2009_LBV'='Litros_27', '2010_LBV'='Litros_38', '2011_LBV'='Litros_49') %>% 
        gather('2007_LBV','2008_LBV','2009_LBV','2010_LBV','2011_LBV', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

LBV_euro_07_11<-ports_07_11 %>% 
        select(country, continent, region, '€_5', '€_16', '€_27', '€_38', '€_49') %>% 
        rename('2007_LBV'= '€_5', '2008_LBV'='€_16', 
               '2009_LBV'='€_27', '2010_LBV'='€_38', '2011_LBV'='€_49') %>% 
        gather('2007_LBV','2008_LBV','2009_LBV','2010_LBV','2011_LBV', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

LBV_07_11 <- left_join(LBV_litres_07_11, LBV_euro_07_11, by = c('country', 'continent','region', 'year', "port"))

LBV_litres_12_16<-ports_12_16 %>% 
        select(country, continent, region, Litros_5, Litros_16, Litros_27, Litros_38, Litros_49) %>% 
        rename('2012_LBV'= 'Litros_5', '2013_LBV'='Litros_16', 
               '2014_LBV'='Litros_27', '2015_LBV'='Litros_38', '2016_LBV'='Litros_49') %>% 
        gather('2012_LBV','2013_LBV','2014_LBV','2015_LBV','2016_LBV', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

LBV_euro_12_16<-ports_12_16 %>% 
        select(country, continent, region, '€_5', '€_16', '€_27', '€_38', '€_49') %>% 
        rename('2012_LBV'= '€_5', '2013_LBV'='€_16', 
               '2014_LBV'='€_27', '2015_LBV'='€_38', '2016_LBV'='€_49') %>% 
        gather('2012_LBV','2013_LBV','2014_LBV','2015_LBV','2016_LBV', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

LBV_12_16 <- left_join(LBV_litres_12_16, LBV_euro_12_16, by = c('country','continent','region', 'year', "port"))

LBV_litres_17<-ports_17 %>% 
        select(country, continent, region, Litros_5) %>% 
        rename('2017_LBV'= 'Litros_5') %>% 
        gather('2017_LBV', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

LBV_euro_17<-ports_17 %>% 
        select(country, continent, region, '€_5') %>% 
        rename('2017_LBV'= '€_5') %>% 
        gather('2017_LBV', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

LBV_17 <-left_join(LBV_litres_17, LBV_euro_17, by = c('country','continent','region', 'year', "port"))

LBV_07_17 <- bind_rows(LBV_07_11,LBV_12_16, LBV_17)
#####################
# Colheitas

Colheitas_litres_07_11<-ports_07_11 %>% 
        select(country, continent, region, Litros_6, Litros_17, Litros_28, Litros_39, Litros_50) %>% 
        rename('2007_Colheitas'= 'Litros_6', '2008_Colheitas'='Litros_17', 
               '2009_Colheitas'='Litros_28', '2010_Colheitas'='Litros_39', '2011_Colheitas'='Litros_50') %>% 
        gather('2007_Colheitas','2008_Colheitas','2009_Colheitas','2010_Colheitas','2011_Colheitas', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Colheitas_euro_07_11<-ports_07_11 %>% 
        select(country, continent, region, '€_6', '€_17', '€_28', '€_39', '€_50') %>% 
        rename('2007_Colheitas'= '€_6', '2008_Colheitas'='€_17', 
               '2009_Colheitas'='€_28', '2010_Colheitas'='€_39', '2011_Colheitas'='€_50') %>% 
        gather('2007_Colheitas','2008_Colheitas','2009_Colheitas','2010_Colheitas','2011_Colheitas', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Colheitas_07_11 <- left_join(Colheitas_litres_07_11, Colheitas_euro_07_11, by = c('country', 'continent','region', 'year', "port"))

Colheitas_litres_12_16<-ports_12_16 %>% 
        select(country, continent, region, Litros_6, Litros_17, Litros_28, Litros_39, Litros_50) %>% 
        rename('2012_Colheitas'= 'Litros_6', '2013_Colheitas'='Litros_17', 
               '2014_Colheitas'='Litros_28', '2015_Colheitas'='Litros_39', '2016_Colheitas'='Litros_50') %>% 
        gather('2012_Colheitas','2013_Colheitas','2014_Colheitas','2015_Colheitas','2016_Colheitas', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Colheitas_euro_12_16<-ports_12_16 %>% 
        select(country, continent, region, '€_6', '€_17', '€_28', '€_39', '€_50') %>% 
        rename('2012_Colheitas'= '€_6', '2013_Colheitas'='€_17', 
               '2014_Colheitas'='€_28', '2015_Colheitas'='€_39', '2016_Colheitas'='€_50') %>% 
        gather('2012_Colheitas','2013_Colheitas','2014_Colheitas','2015_Colheitas','2016_Colheitas', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Colheitas_12_16 <- left_join(Colheitas_litres_12_16, Colheitas_euro_12_16, by = c('country','continent','region', 'year', "port"))

Colheitas_litres_17<-ports_17 %>% 
        select(country, continent, region, Litros_6) %>% 
        rename('2017_Colheitas'= 'Litros_6') %>% 
        gather('2017_Colheitas', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Colheitas_euro_17<-ports_17 %>% 
        select(country, continent, region, '€_6') %>% 
        rename('2017_Colheitas'= '€_6') %>% 
        gather('2017_Colheitas', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Colheitas_17 <-left_join(Colheitas_litres_17, Colheitas_euro_17, by = c('country','continent','region', 'year', "port"))

Colheitas_07_17 <- bind_rows(Colheitas_07_11,Colheitas_12_16, Colheitas_17)

##########################
#IndicationOfAge

IndicationOfAge_litres_07_11<-ports_07_11 %>% 
        select(country, continent, region, Litros_7, Litros_18, Litros_29, Litros_40, Litros_51) %>% 
        rename('2007_Indication of Age'= 'Litros_7', '2008_Indication of Age'='Litros_18', 
               '2009_Indication of Age'='Litros_29', '2010_Indication of Age'='Litros_40', '2011_Indication of Age'='Litros_51') %>% 
        gather('2007_Indication of Age','2008_Indication of Age','2009_Indication of Age','2010_Indication of Age','2011_Indication of Age', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

IndicationOfAge_euro_07_11<-ports_07_11 %>% 
        select(country, continent, region, '€_7', '€_18', '€_29', '€_40', '€_51') %>% 
        rename('2007_Indication of Age'= '€_7', '2008_Indication of Age'='€_18', 
               '2009_Indication of Age'='€_29', '2010_Indication of Age'='€_40', '2011_Indication of Age'='€_51') %>% 
        gather('2007_Indication of Age','2008_Indication of Age','2009_Indication of Age','2010_Indication of Age','2011_Indication of Age', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

IndicationOfAge_07_11 <- left_join(IndicationOfAge_litres_07_11, IndicationOfAge_euro_07_11, by = c('country', 'continent','region', 'year', "port"))

IndicationOfAge_litres_12_16<-ports_12_16 %>% 
        select(country, continent, region, Litros_7, Litros_18, Litros_29, Litros_40, Litros_51) %>% 
        rename('2012_Indication of Age'= 'Litros_7', '2013_Indication of Age'='Litros_18', 
               '2014_Indication of Age'='Litros_29', '2015_Indication of Age'='Litros_40', '2016_Indication of Age'='Litros_51') %>% 
        gather('2012_Indication of Age','2013_Indication of Age','2014_Indication of Age','2015_Indication of Age','2016_Indication of Age', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_") 

IndicationOfAge_euro_12_16<-ports_12_16 %>% 
        select(country, continent, region, '€_7', '€_18', '€_29', '€_40', '€_51') %>% 
        rename('2012_Indication of Age'= '€_7', '2013_Indication of Age'='€_18', 
               '2014_Indication of Age'='€_29', '2015_Indication of Age'='€_40', '2016_Indication of Age'='€_51') %>% 
        gather('2012_Indication of Age','2013_Indication of Age','2014_Indication of Age','2015_Indication of Age','2016_Indication of Age', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

IndicationOfAge_12_16 <- left_join(IndicationOfAge_litres_12_16, IndicationOfAge_euro_12_16, by = c('country','continent','region', 'year', "port"))

IndicationOfAge_litres_17<-ports_17 %>% 
        select(country, continent, region, Litros_7) %>% 
        rename('2017_Indication of Age'= 'Litros_7') %>% 
        gather('2017_Indication of Age', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

IndicationOfAge_euro_17<-ports_17 %>% 
        select(country, continent, region, '€_7') %>% 
        rename('2017_Indication of Age'= '€_7') %>% 
        gather('2017_Indication of Age', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

IndicationOfAge_17 <-left_join(IndicationOfAge_litres_17, IndicationOfAge_euro_17, by = c('country','continent','region', 'year', "port"))

IndicationOfAge_07_17 <- bind_rows(IndicationOfAge_07_11,IndicationOfAge_12_16, IndicationOfAge_17)

############################
# Reserve

Reserve_litres_07_11<-ports_07_11 %>% 
        select(country, continent, region, Litros_8, Litros_19, Litros_30, Litros_41, Litros_52) %>% 
        rename('2007_Reserve'= 'Litros_8', '2008_Reserve'='Litros_19', 
               '2009_Reserve'='Litros_30', '2010_Reserve'='Litros_41', '2011_Reserve'='Litros_52') %>% 
        gather('2007_Reserve','2008_Reserve','2009_Reserve','2010_Reserve','2011_Reserve', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Reserve_euro_07_11<-ports_07_11 %>% 
        select(country, continent, region, '€_8', '€_19', '€_30', '€_41', '€_52') %>% 
        rename('2007_Reserve'= '€_8', '2008_Reserve'='€_19', 
               '2009_Reserve'='€_30', '2010_Reserve'='€_41', '2011_Reserve'='€_52') %>% 
        gather('2007_Reserve','2008_Reserve','2009_Reserve','2010_Reserve','2011_Reserve', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Reserve_07_11 <- left_join(Reserve_litres_07_11, Reserve_euro_07_11, by = c('country', 'continent','region', 'year', "port"))

Reserve_litres_12_16<-ports_12_16 %>% 
        select(country, continent, region, Litros_8, Litros_19, Litros_30, Litros_41, Litros_52) %>% 
        rename('2012_Reserve'= 'Litros_8', '2013_Reserve'='Litros_19', 
               '2014_Reserve'='Litros_30', '2015_Reserve'='Litros_41', '2016_Reserve'='Litros_52') %>% 
        gather('2012_Reserve','2013_Reserve','2014_Reserve','2015_Reserve','2016_Reserve', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Reserve_euro_12_16<-ports_12_16 %>% 
        select(country, continent, region, '€_8', '€_19', '€_30', '€_41', '€_52') %>% 
        rename('2012_Reserve'= '€_8', '2013_Reserve'='€_19', 
               '2014_Reserve'='€_30', '2015_Reserve'='€_41', '2016_Reserve'='€_52') %>% 
        gather('2012_Reserve','2013_Reserve','2014_Reserve','2015_Reserve','2016_Reserve', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Reserve_12_16 <- left_join(Reserve_litres_12_16, Reserve_euro_12_16, by = c('country','continent','region', 'year', "port"))

Reserve_litres_17<-ports_17 %>% 
        select(country, continent, region, Litros_8) %>% 
        rename('2017_Reserve'= 'Litros_8') %>% 
        gather('2017_Reserve', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Reserve_euro_17<-ports_17 %>% 
        select(country, continent, region, '€_8') %>% 
        rename('2017_Reserve'= '€_8') %>% 
        gather('2017_Reserve', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Reserve_17 <-left_join(Reserve_litres_17, Reserve_euro_17, by = c('country','continent','region', 'year', "port"))

Reserve_07_17 <- bind_rows(Reserve_07_11,Reserve_12_16, Reserve_17)

######################
# Reserve Tawny

TawnyReserve_litres_07_11<-ports_07_11 %>% 
        select(country, continent, region, Litros_9, Litros_20, Litros_31, Litros_42, Litros_53) %>% 
        rename('2007_Tawny Reserve'= 'Litros_9', '2008_Tawny Reserve'='Litros_20', 
               '2009_Tawny Reserve'='Litros_31', '2010_Tawny Reserve'='Litros_42', '2011_Tawny Reserve'='Litros_53') %>% 
        gather('2007_Tawny Reserve','2008_Tawny Reserve','2009_Tawny Reserve','2010_Tawny Reserve','2011_Tawny Reserve', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

TawnyReserve_euro_07_11<-ports_07_11 %>% 
        select(country, continent, region, '€_9', '€_20', '€_31', '€_42', '€_53') %>% 
        rename('2007_Tawny Reserve'= '€_9', '2008_Tawny Reserve'='€_20', 
               '2009_Tawny Reserve'='€_31', '2010_Tawny Reserve'='€_42', '2011_Tawny Reserve'='€_53') %>% 
        gather('2007_Tawny Reserve','2008_Tawny Reserve','2009_Tawny Reserve','2010_Tawny Reserve','2011_Tawny Reserve', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

TawnyReserve_07_11 <- left_join(TawnyReserve_litres_07_11, TawnyReserve_euro_07_11, by = c('country', 'continent','region', 'year', "port"))

TawnyReserve_litres_12_16<-ports_12_16 %>% 
        select(country, continent, region, Litros_9, Litros_20, Litros_31, Litros_42, Litros_53) %>% 
        rename('2012_Tawny Reserve'= 'Litros_9', '2013_Tawny Reserve'='Litros_20', 
               '2014_Tawny Reserve'='Litros_31', '2015_Tawny Reserve'='Litros_42', '2016_Tawny Reserve'='Litros_53') %>% 
        gather('2012_Tawny Reserve','2013_Tawny Reserve','2014_Tawny Reserve','2015_Tawny Reserve','2016_Tawny Reserve', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

TawnyReserve_euro_12_16<-ports_12_16 %>% 
        select(country, continent, region, '€_9', '€_20', '€_31', '€_42', '€_53') %>% 
        rename('2012_Tawny Reserve'= '€_9', '2013_Tawny Reserve'='€_20', 
               '2014_Tawny Reserve'='€_31', '2015_Tawny Reserve'='€_42', '2016_Tawny Reserve'='€_53') %>% 
        gather('2012_Tawny Reserve','2013_Tawny Reserve','2014_Tawny Reserve','2015_Tawny Reserve','2016_Tawny Reserve', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

TawnyReserve_12_16 <- left_join(TawnyReserve_litres_12_16, TawnyReserve_euro_12_16, by = c('country','continent','region', 'year', "port"))

TawnyReserve_litres_17<-ports_17 %>% 
        select(country, continent, region, Litros_9) %>% 
        rename('2017_Tawny Reserve'= 'Litros_9') %>% 
        gather('2017_Tawny Reserve', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

TawnyReserve_euro_17<-ports_17 %>% 
        select(country, continent, region, '€_9') %>% 
        rename('2017_Tawny Reserve'= '€_9') %>% 
        gather('2017_Tawny Reserve', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

TawnyReserve_17 <-left_join(TawnyReserve_litres_17, TawnyReserve_euro_17, by = c('country','continent','region', 'year', "port"))

TawnyReserve_07_17 <- bind_rows(TawnyReserve_07_11,TawnyReserve_12_16, TawnyReserve_17)

#########################
# Crusted

Crusted_litres_07_11<-ports_07_11 %>% 
        select(country, continent, region, Litros_10, Litros_21, Litros_32, Litros_43, Litros_54) %>% 
        rename('2007_Crusted'= 'Litros_10', '2008_Crusted'='Litros_21', 
               '2009_Crusted'='Litros_32', '2010_Crusted'='Litros_43', '2011_Crusted'='Litros_54') %>% 
        gather('2007_Crusted','2008_Crusted','2009_Crusted','2010_Crusted','2011_Crusted', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Crusted_euro_07_11<-ports_07_11 %>% 
        select(country, continent, region, '€_10', '€_21', '€_32', '€_43', '€_54') %>% 
        rename('2007_Crusted'= '€_10', '2008_Crusted'='€_21', 
               '2009_Crusted'='€_32', '2010_Crusted'='€_43', '2011_Crusted'='€_54') %>% 
        gather('2007_Crusted','2008_Crusted','2009_Crusted','2010_Crusted','2011_Crusted', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Crusted_07_11 <- left_join(Crusted_litres_07_11, Crusted_euro_07_11, by = c('country', 'continent','region', 'year', "port"))

Crusted_litres_12_16<-ports_12_16 %>% 
        select(country, continent, region, Litros_10, Litros_21, Litros_32, Litros_43, Litros_54) %>% 
        rename('2012_Crusted'= 'Litros_10', '2013_Crusted'='Litros_21', 
               '2014_Crusted'='Litros_32', '2015_Crusted'='Litros_43', '2016_Crusted'='Litros_54') %>% 
        gather('2012_Crusted','2013_Crusted','2014_Crusted','2015_Crusted','2016_Crusted', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Crusted_euro_12_16<-ports_12_16 %>% 
        select(country, continent, region, '€_10', '€_21', '€_32', '€_43', '€_54') %>% 
        rename('2012_Crusted'= '€_10', '2013_Crusted'='€_21', 
               '2014_Crusted'='€_32', '2015_Crusted'='€_43', '2016_Crusted'='€_54') %>% 
        gather('2012_Crusted','2013_Crusted','2014_Crusted','2015_Crusted','2016_Crusted', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Crusted_12_16 <- left_join(Crusted_litres_12_16, Crusted_euro_12_16, by = c('country','continent','region', 'year', "port"))

Crusted_litres_17<-ports_17 %>% 
        select(country, continent, region, Litros_10) %>% 
        rename('2017_Crusted'= 'Litros_10') %>% 
        gather('2017_Crusted', key = "year", value = "litres") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Crusted_euro_17<-ports_17 %>% 
        select(country, continent, region, '€_10') %>% 
        rename('2017_Crusted'= '€_10') %>% 
        gather('2017_Crusted', key = "year", value = "euros") %>% 
        separate("year", into = c("year", "port"), sep = "_")

Crusted_17 <-left_join(Crusted_litres_17, Crusted_euro_17, by = c('country','continent','region', 'year', "port"))

Crusted_07_17<- bind_rows(Crusted_07_11,Crusted_12_16, Crusted_17)

total_all_ports<-bind_rows(White_07_17,Tawny_07_17,Ruby_07_17,Rose_07_17,Vintage_07_17,LBV_07_17,
                      Colheitas_07_17,IndicationOfAge_07_17,Reserve_07_17,TawnyReserve_07_17,Crusted_07_17)

# write to a .csv file

write_csv(total_all_ports, 'tidy-port-wine-data-07-17.csv')
