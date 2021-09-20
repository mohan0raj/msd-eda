library("tidyverse")
rawdata<-read.csv(file.choose())
copyrawdata<-rawdata

str(rawdata)
library("skimr")
skim(rawdata)
View(rawdata)
rawdata<-rawdata[-351,]

rawdata$Runs<-as.numeric(rawdata$Runs)
unique(rawdata$Runs)
unique(copyrawdata$Runs)
copyrawdata[which(copyrawdata$Runs=="-"),]
rawdata$B.F<-as.numeric(rawdata$B.F)
rawdata$S.R<-as.numeric(rawdata$S.R)
rawdata$Date=gsub("-","/",rawdata$Date)
library("lubridate")

rawdata$Date<-dmy(rawdata$Date)
library("rvest")
webpage<-read_html("https://en.wikipedia.org/wiki/List_of_One_Day_International_cricket_grounds")
table_css<-"table.wikitable:nth-child(8)"
country<-webpage %>% 
  html_element(css = table_css) %>% 
  html_table()
countrycopy<- country
country<-as.data.frame(country)
head(country)
View(rawdata)
head(rawdata$Ground)
z=c() #temporary variable
for(i in 1:length(rawdata$Ground)) {
 t<-country$Country[grep(rawdata$Ground[i],country$Stadium,fixed=F)]
 if (sum(grepl(rawdata$Ground[i],country$Stadium,fixed=F))==1){
 z[i]<-t
 }else {
   z[i]<-NA
 }
}

country_stadium<-as.data.frame(cbind(z,rawdata$Ground))
nastadium<-unique(rawdata$Ground[is.na(z)])
names(country_stadium)<-c("venue_country","stadium")

View(cbind(z,rawdata$Ground))
nastadium
manual_country<-c("India","India","Sri Lanka","India","India","India","India","Pakistan","India","UAE","South Africa","Bangladesh",
                  "India","England","England","England","India","Sri Lanka","India")
updated_country<-as.data.frame(cbind(nastadium,manual_country))

View(updated_country)
View(country_stadium)

for (i in 1:length(country_stadium$venue_country)){
  
  if (is.na(country_stadium$venue_country[i])==1){
   country_stadium$venue_country[i]<-updated_country$manual_country[country_stadium$stadium[i]==updated_country$nastadium]
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





unique(rawdata$How.Dismissed)
dismissal<-c()
for (i in 1:length(rawdata$How.Dismissed)){

  if (rawdata$How.Dismissed[i]=="run out"){
    dismissal[i]<-rawdata$How.Dismissed[i]
    
  }else if(rawdata$How.Dismissed[i]=="not out"){
    dismissal[i]<-rawdata$How.Dismissed[i]
  }else if (rawdata$How.Dismissed[i]=="did not bat"){
    dismissal[i]<-rawdata$How.Dismissed[i]
  }else if (substr(rawdata$How.Dismissed[i],1,1)=="c"){
    dismissal[i]<-"caught out"
  }else if (substr(rawdata$How.Dismissed[i],1,1)=="b"){
    dismissal[i]<-"bowled out"
  }else if (substr(rawdata$How.Dismissed[i],1,3)=="lbw"){
    dismissal[i]<-"leg by wicket"
  }else if (substr(rawdata$How.Dismissed[i],1,2)=="st"){
    dismissal[i]<-"stumped"
  }else{
    dismissal[i]<-NA
  }
} 


bowler<-c()
for (i in 1:length(rawdata$How.Dismissed)){
  if (dismissal[i] %in% c("caught out","leg by wicket","stumped") ){
  
    bowler[i]<-unlist(strsplit(rawdata$How.Dismissed[i]," b ",fixed = TRUE))[2]
  }else if(dismissal[i] %in% c("bowled out")){
    bowler[i]<-unlist(strsplit(rawdata$How.Dismissed[i],"b ",fixed = TRUE))[2]
  }else{ 
    bowler[i]<-NA
    
  }

}
head(bowler)

View(cbind(bowler,dismissal,rawdata$How.Dismissed))
sum(is.na(bowler))
table(bowler)
table(dismissal)

home_away<-c()

for (i in 1 : length(country_stadium$venue_country)){
  if (country_stadium$venue_country[i]=="India"){
    home_away[i]<-"home"
  
  }else {
    home_away[i]<-"away"
  }
}

final_data<-cbind(rawdata[,c(2,3,4,6,8,9,10,11,12,13)],dismissal,home_away,country_stadium$venue_country)
View(final_data)
names(final_data)[13]<-"venue_country"
names(final_data)[10]<-"avg_SR"
head(final_data)

write.csv(final_data,file = "final_data10092021.csv")
View(final_data)

table(dismissal)[order(-table(dismissal))]

na.omit(final_data) %>%
  group_by(Versus) %>%
  summarise(no_of_innings=n(),total_run = sum(Runs),avg_Runs=mean(Runs),avg_strike_rate=mean(S.R),highest_score=max(Runs)) %>% arrange(-total_run)

na.omit(final_data) %>%
  group_by(Versus) %>%
  summarise(sum_run = sum(Runs))%>%
  ggplot()+geom_col(mapping=aes(y=reorder(Versus,sum_run),x=sum_run,),show.legend = FALSE)+
  geom_text(aes(y=reorder(Versus,sum_run),x=sum_run,label=sum_run),hjust=-.125,vjust=0)+theme_classic()
ggsave("runs.jpeg")

bowlerlist<-table(na.omit(bowler))

bowlerchart<-as.data.frame(head(bowlerlist[order(-bowlerlist)],5))

ggplot(bowlerchart)+geom_point(mapping = aes(x=Freq,y=reorder(Var1,Freq)))+xlim(0,6)




ggplot(data=na.omit(final_data))+geom_density(mapping=aes(x=Runs,fill=home_away,alpha=.50))+xlim(0,200)+theme_minimal()

dismissal_chart<- table(dismissal[which(dismissal!="did not bat")])[order(table(dismissal[which(dismissal!="did not bat")]))]

ggplot(as.data.frame(dismissal_chart))+
  geom_col(aes(x=Var1,y=Freq,fill=Var1),show.legend = FALSE)+
  ylim(0,150)+
  geom_text(aes(x=Var1,y=Freq,label=Freq),vjust=-1)
  
ggplot(final_data)+geom 
 

na.omit(final_data) %>%
  ggplot()+
  geom_point(mapping = aes(y=Versus,x=Runs))
  
library("gganimate")
animated<-na.omit(final_data) %>% 
  ggplot()+
  geom_area((mapping = aes(x=Date,Runs)))+transition_states(Date)+shadow_mark()+view_follow()

na.omit(final_data) %>%
  ggplot()+
  geom_histogram(mapping = aes(x=Runs,color=9))





 na.omit(final_data) %>%
   ggplot()+
   geom_boxplot(mapping = aes(x=Runs,y=home_away))

na.omit(final_data) %>% 
  ggplot()+geom_point(mapping=aes(y=Runs,x=B.F,color=home_away))
 
hist(na.omit(rawdata$Runs))
ggsave('hist.jpeg')
hist(na.omit(final_data$Runs))
na.omit(final_data$Runs)
