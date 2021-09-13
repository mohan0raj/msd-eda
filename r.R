msd<-read.csv(file.choose())
msd<-copymsd
str(msd)
msd$Runs<-as.numeric(msd$Runs)
unique(msd$Runs)
unique(copymsd$Runs)
copymsd$Runs[which(copymsd$Runs=="-")]
unique(msd)
skim(msd)
View(msd)
install.packages('skimr')
library("skimr")
msd$B.F<-as.numeric(msd$B.F)
msd$S.R<-as.numeric(msd$S.R)
msd$Date[i:length(msd$Date)]=gsub("-","/",msd$Date[i])
msd$Date=as.Date(msd$Date)
webpage<-read_html("https://en.wikipedia.org/wiki/List_of_One_Day_International_cricket_grounds")
table_css<-"table.wikitable:nth-child(8)"
country<-webpage %>% 
  html_element(css = table_css) %>% 
  html_table()
countrycopy<- country
country<-as.data.frame(country)
head(country)
View(msd)
head(msd$Ground)
for(i in 1:length(msd$Ground)) {
 t<-country$Country[grep(msd$Ground[i],country$Stadium,fixed=F)]
 if (sum(grepl(msd$Ground[i],country$Stadium,fixed=F))==1){
 z[i]<-t
 }else {
   z[i]<-NA
 }
}

country_stadium<-as.data.frame(cbind(z,msd$Ground))
nastadium<-unique(msd$Ground[is.na(z)])
names(country_stadium)<-c("venue_country","stadium")


nastadium
manual_country<-c("India","India","Sri Lanka","India","India","India","India","Pakistan","India","UAE","South Africa","Bangladesh",
                  "India","England","England","England","India","Sri Lanka","India",'')
updated_country<-as.data.frame(cbind(nastadium,manual_country))
dim(updated_country)

View(updated_country)
View(country_stadium)

for (i in 1:length(country_stadium$country)){
  
  if (is.na(country_stadium$country[i])){
   country_stadium$country[i]<-updated_country$manual_country[country_stadium$stadium[i]==updated_country$nastadium]
  }
}


for(i in 1:length(country_stadium$stadium)) {
  t<-updated_country$manual_country[grep(country_stadium$stadium[i],updated_country$naStadium,fixed=F)]
  if (sum(grep(country_stadium$stadium[i],updated_country$naStadium,fixed=F))==1){
    country_stadium$country[i]<-t
  }else {
    country_stadium$country[i]<-country_stadium[i]
  }
}





unique(msd$How.Dismissed)

for (i in 1:length(msd$How.Dismissed)){

  if (msd$How.Dismissed[i]=="run out"){
    dismissal[i]<-msd$How.Dismissed[i]
    
  }else if(msd$How.Dismissed[i]=="not out"){
    dismissal[i]<-msd$How.Dismissed[i]
  }else if (msd$How.Dismissed[i]=="did not bat"){
    dismissal[i]<-msd$How.Dismissed[i]
  }else if (substr(msd$How.Dismissed[i],1,1)=="c"){
    dismissal[i]<-"caught out"
  }else if (substr(msd$How.Dismissed[i],1,1)=="b"){
    dismissal[i]<-"bowled out"
  }else if (substr(msd$How.Dismissed[i],1,3)=="lbw"){
    dismissal[i]<-"leg by wicket"
  }else if (substr(msd$How.Dismissed[i],1,2)=="st"){
    dismissal[i]<-"stumped"
  }else{
    dismissal[i]<-NA
  }
}   
bowler<-c()
for (i in 1:length(msd$How.Dismissed)){
  if (dismissal[i] %in% c("caught out","bowled out","leg by wicket","stumped") ){
  
    bowler[i]<-unlist(strsplit(msd$How.Dismissed[i]," b ",fixed = TRUE))[2]
  }else{ 
    bowler[i]<-NA
    
  }

}
head(bowler)

final_data<-cbind(msd[,c(2,3,4,6,8,9,10,11,12,13)],dismissal,bowler,country_stadium$venue_country)
names(final_data)[13]<-"venue_country"
names(final_data)[10]<-"avg_SR"
final_data<-final_data[-351,]
head(final_data)

write.csv(final_data,file = "final_data10092021.csv")
View(final_data)
hist(na.omit(final_data$Runs))
na.omit(final_data$Runs)
ggplot(data=dismissal)+
  geom_bar(mapping=(aes(x=dismissal)),na.rm=TRUE)
