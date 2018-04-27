library(tidyverse)

# for tab 3 all wines 2008 to 2017 by Douro region
douro<- read_csv2('all-wines-douro.csv',  na="")
douro<- gather(douro, "Lower Corgo", "Upper Corgo",
               "Upper Douro", "Outside Douro Region", "All Regions",
               key = "Region", value = "Litres")
write_csv(douro, "tidy-all-wines-douro.csv")

########################
# tidy data for tab 1 of dash board : 2017 port sales by euro, top 25 countries

sales17 <- read_csv2('2017PortEuros.csv', skip =10, col_names = FALSE)
sales17 <- sales17 %>% 
        select(X1, X2, X4, X6, X7, X9, X10, X12, X13, X14, X16, X17, X19, X20, X22, X23) %>% 
        rename('Rank'=X1, 'Country'=X2, 'Market Percent'= X4, 'Euros'= X6, 'Change'= X7,
               'Regular Port'= X9,'Change'= X10, 'Special Port'= X12, 'Change'= X13, 'Percent of Total'=X14,
               'Cost per Litre'=X16,'Change'= X17, 'Regular Port per Litre'= X19, 'Change'= X20, 'Special Port per Litre'= X22, 'Change' = X23)
write_csv(sales17, 'sales2017.csv')

####################
# tab 2 top 10 regular and special ports

reg<- read_csv2("regular-ports-17.csv",  skip = 4 , na="---")
reg<- reg %>% 
        select('country','€','€_1','€_2','€_3') %>% 
        rename(White='€',Tawny='€_1', Ruby='€_2', Rose='€_3') %>% 
        gather(White, Tawny, Ruby, Rose, key = port, value = euro) %>% 
        group_by(country) %>% 
        summarise(percent=list(euro/sum(euro)*100)) %>% 
        unnest() %>% 
        add_column(port =c('White','Tawny','Ruby','Rose','White','Tawny','Ruby','Rose','White','Tawny','Ruby','Rose','White','Tawny','Ruby','Rose','White','Tawny','Ruby','Rose', 
                   'White','Tawny','Ruby','Rose','White','Tawny','Ruby','Rose','White','Tawny','Ruby','Rose','White','Tawny','Ruby','Rose','White','Tawny','Ruby','Rose', 'White','Tawny','Ruby','Rose'))
write_csv(reg, 'tidy-regular-ports-17.csv')

special<- read_csv2("special-ports-17.csv",  skip = 4 , na="---")
special<- special %>% 
        select('country','€','€_1','€_2','€_3','€_4','€_5','€_6') %>% 
        rename(Vintage='€',LBV='€_1', Colheitas='€_2', IndicationOfAge='€_3', Reserve='€_4', TawnyReserve='€_5', Crusted='€_6') %>% 
        gather(Vintage, LBV, Colheitas, IndicationOfAge, Reserve, TawnyReserve, Crusted, key = port, value = euro) %>% 
        group_by(country) %>% 
        
        summarise(percent=list(euro/sum(euro)*100)) %>%
        unnest() %>% 
        add_column(port =c('Vintage', 'LBV', 'Colheitas', 'IndicationOfAge', 'Reserve', 'TawnyReserve', 'Crusted',
                           'Vintage', 'LBV', 'Colheitas', 'IndicationOfAge', 'Reserve', 'TawnyReserve', 'Crusted',
                           'Vintage', 'LBV', 'Colheitas', 'IndicationOfAge', 'Reserve', 'TawnyReserve', 'Crusted',
                           'Vintage', 'LBV', 'Colheitas', 'IndicationOfAge', 'Reserve', 'TawnyReserve', 'Crusted',
                           'Vintage', 'LBV', 'Colheitas', 'IndicationOfAge', 'Reserve', 'TawnyReserve', 'Crusted',
                           'Vintage', 'LBV', 'Colheitas', 'IndicationOfAge', 'Reserve', 'TawnyReserve', 'Crusted',
                           'Vintage', 'LBV', 'Colheitas', 'IndicationOfAge', 'Reserve', 'TawnyReserve', 'Crusted',
                           'Vintage', 'LBV', 'Colheitas', 'IndicationOfAge', 'Reserve', 'TawnyReserve', 'Crusted',
                           'Vintage', 'LBV', 'Colheitas', 'IndicationOfAge', 'Reserve', 'TawnyReserve', 'Crusted',
                           'Vintage', 'LBV', 'Colheitas', 'IndicationOfAge', 'Reserve', 'TawnyReserve', 'Crusted',
                           'Vintage', 'LBV', 'Colheitas', 'IndicationOfAge', 'Reserve', 'TawnyReserve', 'Crusted'
                           ))
write_csv(special, 'tidy-special-ports-17.csv')

######################
# top 6 NVD# tab 4
# "tidy_port_wine_data_07_17.csv" is the result of tidying the original 
# data performed in "wine_tidying.R" script using 3 other csv files
# "all_ports_07_11.csv", "all_ports_12_16.csv", "all_ports_17.csv"
allport<-read_csv("tidy-port-wine-data-07-17.csv")

top_6_17<-allport %>%
        filter(country!='All countries', year == '2017') %>% 
        group_by(country) %>% 
        summarise(total = sum(euros, na.rm = TRUE)/1000000) %>% 
        arrange(desc(total), country) %>% 
        top_n(6) %>% 
        add_column(year='2017')

top_6_16<-allport %>%
        filter(country!='All countries', year == '2016') %>% 
        group_by(country) %>% 
        summarise(total = sum(euros, na.rm = TRUE)/1000000) %>% 
        arrange(desc(total), country) %>% 
        top_n(6) %>% 
        add_column(year='2016')
top_6_15<-allport %>%
        filter(country!='All countries', year == '2015') %>% 
        group_by(country) %>% 
        summarise(total = sum(euros, na.rm = TRUE)/1000000) %>% 
        arrange(desc(total), country) %>% 
        top_n(6) %>% 
        add_column(year='2015')
top_6_14<-allport %>%
        filter(country!='All countries', year == '2014') %>% 
        group_by(country) %>% 
        summarise(total = sum(euros, na.rm = TRUE)/1000000) %>% 
        arrange(desc(total), country) %>% 
        top_n(6) %>% 
        add_column(year='2014')
top_6_13<-allport %>%
        filter(country!='All countries', year == '2013') %>% 
        group_by(country) %>% 
        summarise(total = sum(euros, na.rm = TRUE)/1000000) %>% 
        arrange(desc(total)) %>% 
        top_n(6) %>% 
        add_column(year='2013')
        

topsix13_17 <-bind_rows(top_6_17,top_6_16,top_6_15,top_6_14,top_6_13)
write_csv(topsix13_17,"tidy-top-six-countries.csv")
# spread the data for table
top_six_wide <- topsix13_17 %>% 
        spread(country, total)
write_csv(top_six_wide, 'top-six-wide.csv')

##################
# tab 5 datatable showing change in sales between 2008 and 2017
# first tidy csv file
totals_by_country<-read_csv2("total-by-country-08-17.csv", skip = 3 , na="---")

totals_litres_08<-totals_by_country %>% 
        select(country, continent, region, Litros) %>% 
        rename(year08= 'Litros') %>% 
        gather(year08, key = "year", value = "litres")

totals_euro_08<-totals_by_country %>% 
        select(country, continent, region, '€') %>% 
        rename(year08= '€') %>% 
        gather(year08, key = "year", value = "euros")
totals_08 <- left_join(totals_litres_08, totals_euro_08, by = c('country','continent','region', 'year')) %>% 
        spread(year, euros)

totals_litres_17<-totals_by_country %>% 
        select(country, continent, region, Litros_1) %>% 
        rename(year17= 'Litros_1') %>% 
        gather(year17, key = "year", value = "litres")
       

totals_euro_17<-totals_by_country %>% 
        select(country, continent, region, '€_1') %>% 
        rename(year17= '€_1') %>% 
        gather(year17, key = "year", value = "euros")

totals_17 <- left_join(totals_litres_17, totals_euro_17, by = c('country','continent','region', 'year')) %>% 
        spread(year, euros)

totals_by_country_08_17 <- left_join(totals_17,totals_08, by =  c('country','continent','region'))

write_csv(totals_by_country_08_17, "tidy-totals-by-country-08-17.csv")

# Next, using "tidy-totals-by-country-08-17.csv", create a file for the dashboard dataframe to display difference between 2008 and 2017 sales
total_port_by_country <-read_csv("tidy-totals-by-country-08-17.csv")
change<-total_port_by_country %>% 
        select(country,continent, region, year08,year17) %>% 
        mutate('Increase/Decrease'= round((year17-year08), digits=2))

write_csv(change,"port-sales-change-08-17.csv")


