## script to tidy data for second dimple graph ##
## the data is downloaded from www.ivdp.pt as an .xls file  ##
## run the script with r
#save the .xls file as a .csv file and read it in to r
not<-read.csv("notspecial2013.csv", header=FALSE,as.is=1:13) 
names(not)<-c("Country","Litros","€","€/l","Litros","€","€/l","Litros","€","€/l","Litros","€","€/l")  #set col names
not<-not[-c(1:5),]      # remove unwanted rows, 1 to 4
not<-not[,c(1:13)] 	#creates unique names for columns
# Add new cols for port types and countries
not$Porto<-"Branco"
not$Porto.1<-"Tawny"
not$Porto.2<-"Ruby"
not$Porto.3<-"Rosé"
not$Country.1<-c("Alemanha","Bélgica","Canadá","Dinamarca","Espanha","EUA","França","Holanda","Portugal","ReinoUnido","TOTAIS")
not$Country.2<-c("Alemanha","Bélgica","Canadá","Dinamarca","Espanha","EUA","França","Holanda","Portugal","ReinoUnido","TOTAIS")
not$Country.3<-c("Alemanha","Bélgica","Canadá","Dinamarca","Espanha","EUA","França","Holanda","Portugal","ReinoUnido","TOTAIS")
# Reorder cols
not <- not[, c("Country","Porto","Litros","€","€/l","Country.1","Porto.1","Litros.1","€.1","€/l.1","Country.2","Porto.2","Litros.2","€.2","€/l.2","Country.3","Porto.3","Litros.3","€.3","€/l.3")]
#rename columns
names(not)<-c("Country","Porto","Litros","€","€/l","Country","Porto","Litros","€","€/l","Country","Porto","Litros","€","€/l","Country","Porto","Litros","€","€/l")
#create individual data frames for each of the wine types
a<-data.frame(not[1:5])
b<-data.frame(not[6:10])
c<-data.frame(not[11:15])
d<-data.frame(not[16:20])

#row bind the data frames ie create a narrow data set instead of wide
not<-rbind(a,b,c,d)

#for each country calculate the percentage
al<-filter(not,Country=="Alemanha")#filter out individual countries
al$X.<-as.numeric(al$X.) # make Euro col numeric
tot<-sum(al$X.) #get total for each country
al<-mutate(al,percent.euro=round((X./tot)*100, digits=2))

be<-filter(not,Country=="Bélgica")#filter out individual countries
be$X.<-as.numeric(be$X.) # make Euro col numeric
tot<-sum(be$X.) #get total for each country
be<-mutate(be,percent.euro=round((X./tot)*100,digits=2))

ca<-filter(not,Country=="Canadá")#filter out individual countries
ca$X.<-as.numeric(ca$X.) # make Euro col numeric
tot<-sum(ca$X.) #get total for each country
ca<-mutate(ca,percent.euro=round((X./tot)*100, digits=2))

di<-filter(not,Country=="Dinamarca") #filter out individual countries
di$X.<-as.numeric(di$X.) # make Euro col numeric
tot<-sum(di$X.) #get total for each country
di<-mutate(di,percent.euro=round((X./tot)*100, digits=2))

es<-filter(not,Country=="Espanha")#filter out individual countries
es$X.<-as.numeric(es$X.) # make Euro col numeric
tot<-sum(es$X.) #get total for each country
es<-mutate(es,percent.euro=round((X./tot)*100, digits=2))

eu<-filter(not,Country=="EUA")#filter out individual countries
eu$X.<-as.numeric(eu$X.) # make Euro col numeric
tot<-sum(eu$X.) #get total for each country
eu<-mutate(eu,percent.euro=round((X./tot)*100, digits=2))

fr<-filter(not,Country=="França")  #filter out individual countries
fr$X.<-as.numeric(fr$X.) # make Euro col numeric
tot<-sum(fr$X.) #get total for each country
fr<-mutate(fr,percent.euro=round((X./tot)*100,digits=2))

ho<-filter(not,Country=="Holanda")#filter out individual countries
ho$X.<-as.numeric(ho$X.) # make Euro col numeric
tot<-sum(ho$X.) #get total for each country
ho<-mutate(ho,percent.euro=round((X./tot)*100, digits=2))

po<-filter(not,Country=="Portugal")#filter out individual countries
po$X.<-as.numeric(po$X.) # make Euro col numeric
tot<-sum(po$X.) #get total for each country
po<-mutate(po,percent.euro=round((X./tot)*100, digits=2))

ru<-filter(not,Country=="ReinoUnido")#filter out individual countries
ru$X.<-as.numeric(ru$X.) # make Euro col numeric
tot<-sum(ru$X.) #get total for each country
ru<-mutate(ru,percent.euro=round((X./tot)*100, digits=2))

to<-filter(not,Country=="TOTAIS")#filter out individual countries
to$X.<-as.numeric(to$X.) # make Euro col numeric
tot<-sum(to$X.) #get total for each country
to<-mutate(to,percent.euro=round((X./tot)*100,digits=2))

# again bind the rows, this time with a percentage column
not<-rbind(al,be,ca,di,es,eu,fr,ho,po,ru,to)
#select only the columns needed
not<- select(not,Country,Porto,X.,percent.euro)
#rename the columns
not<-rename(not,sales.euro=X.)
# write a .csv file to  be used in the app script
write.csv(not,"not_special.csv")
