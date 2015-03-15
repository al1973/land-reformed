# script to tidy the full data file into a smaller subset #
# read in the full IST data .csv file
ist<-read.csv("ist.csv")
# subset a selected number of columns
subist<-ist[,c("SEX","AGE","COUNTRY","DDIAGISC","DDIAGHA","DDIAGUN","DNOSTRK","OCCODE","RXASP","RXHEP")]
# rename the column names
subist<-rename(subist, sex=SEX,age=AGE,country=COUNTRY,ischaemic=DDIAGISC, haemorrhagic=DDIAGHA,indeterminate=DDIAGUN, NotStroke=DNOSTRK,
               sixMonthOutcome=OCCODE,aspirin=RXASP,heparin=RXHEP)
# melt the data to create a narrow data set
meltist<-melt(subist,id=c("sex","age","country","sixMonthOutcome","aspirin","heparin"),measure.vars=c("ischaemic","haemorrhagic","indeterminate","NotStroke"))

# retain only yes values
meltist<-filter(meltist,value=="Y")
#remove unneeded col
meltist<-meltist[,-8]
# rename "variable" column
meltist<- rename(meltist,stroke=variable)

# rename 6 month outcome categories
meltist$sixMonthOutcome[which(meltist$sixMonthOutcome=="1")]<-"dead"
meltist$sixMonthOutcome[which(meltist$sixMonthOutcome=="2")]<-"dependent"
meltist$sixMonthOutcome[which(meltist$sixMonthOutcome=="3")]<-"not recovered"
meltist$sixMonthOutcome[which(meltist$sixMonthOutcome=="4")]<-"recovered"
meltist$sixMonthOutcome[which(meltist$sixMonthOutcome=="0")]<-"missing"
meltist$sixMonthOutcome[which(meltist$sixMonthOutcome=="9")]<-"missing"

#rename herapin categories
meltist$heparin<- as.character(meltist$heparin)
meltist$heparin[meltist$heparin == "L"] <- "low"
meltist$heparin[meltist$heparin == "H"] <- "high"
meltist$heparin[meltist$heparin == "M"] <- "high"
meltist$heparin[meltist$heparin == "N"] <- "avoid"
meltist$heparin <- as.factor(meltist$heparin) 

## create table of outcome vs stroke type ##
table1<-table(meltist$sixMonthOutcome,meltis$stroke)
names(table1)<-c("stroke","outcome","n")

## create table of aspirin vs  6 month outcome ##
table2<-table(meltist$sixMonthOutcome,meltist$aspirin)
table2<-as.data.frame(table2)
names(table2)<-c("outcome","aspirin","cases")

## create table of heparin vs  6 month outcome ##
table3<-table(meltist$sixMonthOutcome,meltist$heparin)
table3<-as.data.frame(table3)
names(table3)<-c("outcome","heparin","cases")

## create table of sex vs  6 month outcome ##
table4<-table(meltist$sixMonthOutcome,meltist$sex)
table4<-as.data.frame(table4)
names(table4)<-c("outcome","sex","cases")

## create table of country vs cases ##
table5<-table(meltist$country)
table5<-as.data.frame(table5)
names(table5)<-c("country","cases")

# create the .csv files to use in app #
write.csv(meltist,"subist.csv")
write.csv(table1,"table1.csv")
write.csv(table2,"table2.csv")
write.csv(table3,"table3.csv")
write.csv(table4,"table4.csv")
write.csv(table5,"table5.csv")