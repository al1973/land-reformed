# R script to format the data for port wine sales 2013 and 2006
# download the .xls files for 2013 and 2006 for total Port wine sales (www.ivdp.pt)
# save the files with .xlsx format and with a more descriptive name as below
# load libraries necessary for formatting the files
library(xlsx)
library(dplyr)
# read in the fies and give them descriptive names
port06<-read.xlsx("PortSales2006.xlsx",1)
names(port06)<-c("country","vinho","year","litros","euros", "euro.litro")
port13<-read.xlsx("PortSales2013.xlsx",1)
names(port13)<-c("country","vinho","year","litros","euros", "euro.litro")
# merge the 2 data frames by "country"
mrg<-merge(port13,port06,by="country")
# select the required variables
mrg<-select(mrg,country,euros.y,euros.x)
# add a percentage change column
mrg<-mutate(mrg,percent.change=round((((euros.x-euros.y)/euros.y)*100),digits=2))
# sort by percentage column
mrg<-arrange(mrg,percent.change)
# rename columns again
names(mrg)<-c("Country","2006 €","2013 €","Change %")
# write the result to a .csv file
port<-write.csv(mrg,"porttable.csv")
