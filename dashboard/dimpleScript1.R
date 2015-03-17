## script to tidy data for first dimple graph ##
## the data is downloaded from www.ivdp.pt as an .xls file  ##
#save the .xls file as a .csv file and read it in
spec<-read.csv("special2013.csv", header=FALSE,as.is=1:22) 
#load required R libraries
library(dplyr)
# rename the columns
names(spec)<-c("Country","Litros","€","€/l","Litros","€","€/l","Litros","€","€/l","Litros","€","€/l","Litros","€","€/l","Litros","€","€/l","Litros","€","€/l")
spec<-spec[-c(1:5),]      # remove unwanted rows, 1 to 5
spec<-spec[,c(1:22)]      # creates unique names for columns
# Add new cols for port types
spec$Porto<-"Vintage"
spec$Porto.1<-"LBV"
spec$Porto.2<-"Colheitas"
spec$Porto.3<-"Indicação de Idade"
spec$Porto.4<-"Reserva"
spec$Porto.5<-"Reserva Tawny"
spec$Porto.6<-"Crusted"
# Add a country column for each wine type
spec$Country.1<-c("Alemanha","Bélgica","Canadá","Dinamarca","Espanha","EUA","França","Holanda","Portugal","Reino Unido","TOTAIS")
spec$Country.2<-c("Alemanha","Bélgica","Canadá","Dinamarca","Espanha","EUA","França","Holanda","Portugal","Reino Unido","TOTAIS")
spec$Country.3<-c("Alemanha","Bélgica","Canadá","Dinamarca","Espanha","EUA","França","Holanda","Portugal","Reino Unido","TOTAIS")
spec$Country.4<-c("Alemanha","Bélgica","Canadá","Dinamarca","Espanha","EUA","França","Holanda","Portugal","Reino Unido","TOTAIS")
spec$Country.5<-c("Alemanha","Bélgica","Canadá","Dinamarca","Espanha","EUA","França","Holanda","Portugal","Reino Unido","TOTAIS")
spec$Country.6<-c("Alemanha","Bélgica","Canadá","Dinamarca","Espanha","EUA","França","Holanda","Portugal","Reino Unido","TOTAIS")
# Reorder cols
spec <- spec[, c("Country","Porto","Litros","€","€/l",
               "Country.1","Porto.1","Litros.1","€.1","€/l.1",
                "Country.2","Porto.2","Litros.2","€.2","€/l.2",
                "Country.3","Porto.3","Litros.3","€.3","€/l.3",
                "Country.4","Porto.4","Litros.4","€.4","€/l.4",
                "Country.5","Porto.5","Litros.5","€.5","€/l.5",
                "Country.6","Porto.6","Litros.6","€.6","€/l.6")]
# rename cols: necessary for rbind in next step to work
names(spec)<-c("Country","Porto","Litros","€","€/l",
               "Country","Porto","Litros","€","€/l",
               "Country","Porto","Litros","€","€/l",
               "Country","Porto","Litros","€","€/l",
               "Country","Porto","Litros","€","€/l",
               "Country","Porto","Litros","€","€/l",
               "Country","Porto","Litros","€","€/l")
#create individual data frames for each of the wine types
a<-data.frame(spec[1:5])
b<-data.frame(spec[6:10])
c<-data.frame(spec[11:15])
d<-data.frame(spec[16:20])
e<-data.frame(spec[21:25])
f<-data.frame(spec[26:30])
g<-data.frame(spec[31:35])

# row bind the data frames ie create a narrow data set instead of wide
spec<-rbind(a,b,c,d,e,f,g)

spec<-filter(spec,Litros!="---") #remove empty values

#for each country calculate the percentage
al<-filter(spec,Country=="Alemanha")#filter out individual countries
al$X.<-as.numeric(al$X.) # make Euro col numeric
tot<-sum(al$X.) #get total for each country
al<-mutate(al,percent.euro=round((X./tot)*100, digits=2))

be<-filter(spec,Country=="Bélgica")#filter out individual countries
be$X.<-as.numeric(be$X.) # make Euro col numeric
tot<-sum(be$X.) #get total fotr each country
be<-mutate(be,percent.euro=round((X./tot)*100,digits=2))

ca<-filter(spec,Country=="Canadá")#filter out individual countries
ca$X.<-as.numeric(ca$X.) # make Euro col numeric
tot<-sum(ca$X.) #get total for each country
ca<-mutate(ca,percent.euro=round((X./tot)*100, digits=2))

di<-filter(spec,Country=="Dinamarca") #filter out individual countries
di$X.<-as.numeric(di$X.) # make Euro col numeric
tot<-sum(di$X.) #get total for each country
di<-mutate(di,percent.euro=round((X./tot)*100, digits=2))

es<-filter(spec,Country=="Espanha")#filter out individual countries
es$X.<-as.numeric(es$X.) # make Euro col numeric
tot<-sum(es$X.) #get total for each country
es<-mutate(es,percent.euro=round((X./tot)*100, digits=2))

eu<-filter(spec,Country=="EUA")#filter out individual countries
eu$X.<-as.numeric(eu$X.) # make Euro col numeric
tot<-sum(eu$X.) #get total for each country
eu<-mutate(eu,percent.euro=round((X./tot)*100, digits=2))

fr<-filter(spec,Country=="França")  #filter out individual countries
fr$X.<-as.numeric(fr$X.) # make Euro col numeric
tot<-sum(fr$X.) #get total for each country
fr<-mutate(fr,percent.euro=round((X./tot)*100,digits=2))

ho<-filter(spec,Country=="Holanda")#filter out individual countries
ho$X.<-as.numeric(ho$X.) # make Euro col numeric
tot<-sum(ho$X.) #get total for each country
ho<-mutate(ho,percent.euro=round((X./tot)*100, digits=2))

po<-filter(spec,Country=="Portugal")#filter out individual countries
po$X.<-as.numeric(po$X.) # make Euro col numeric
tot<-sum(po$X.) #get total for each country
po<-mutate(po,percent.euro=round((X./tot)*100, digits=2))

ru<-filter(spec,Country=="Reino Unido") #filter out individual countries
ru$X.<-as.numeric(ru$X.) # make Euro col numeric
tot<-sum(ru$X.) #get total for each country
ru<-mutate(ru,percent.euro=round((X./tot)*100, digits=2))

to<-filter(spec,Country=="TOTAIS")#filter out individual countries
to$X.<-as.numeric(to$X.) # make Euro col numeric
tot<-sum(to$X.) #get total for each country
to<-mutate(to,percent.euro=round((X./tot)*100,digits=2))

# again bind the rows, this time with a percentage column
spec<-rbind(al,be,ca,di,es,eu,fr,ho,po,ru,to)
#select only the columns needed
spec<- select(spec,Country,Porto,X.,X..l,percent.euro)
#rename the columns
spec<-rename(spec,sales.euro=X.,preço.litro=X..l)
# write a .csv file to  be used in the app script
write.csv(spec,"special.csv")
