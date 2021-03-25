# Scaraping and a posteriori classification

# Loading the required libraries
library(rvest)
library(tidyverse)

# Vector to store passers names
qbs_all<-vector(mode="character",length=1)

# Loop over the year to scrape passers from 2001 till 2020
for(year in 2001:2020) {
# Converting numeric year to string year
yearc<-as.character(year)
# url to the page with the passer stats
url1 <-str_c("https://www.pro-football-reference.com/years/",yearc,"/passing.htm",collapse=NULL)
# Extracting the table from the webpage and storing it in passing_1
passing_1 <- url1 %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="passing"]') %>%
  html_table()

# Making sure only QBs are selected
qb_aux<-subset(passing_1[[1]],passing_1[[1]]["Rk"][,1]!="Rk"&(passing1[[1]]["Pos"][,1]=="QB"|passing_1[[1]]["Pos"][,1]=="qb"))

# Storing the QB names
qbs_all<-c(qbs_all,qb_aux[,2])
}

# Cleaning-up qbs_all: replacing characters non-alphabetical characters
qbs_all<-gsub("\\+","",gsub("\\*","",gsub(" \\*","",qbs_all)))
# Ordering qbs_all
qbs_ordered<-qbs_all[order(qbs_all)]
# Selecting unique names from qbs_ordered
qbs<-unique(qbs_ordered)
# Removing the first empty element
qbs<-qbs[2:length(qbs)]

# Vectors to store first and last name
qbs_f<-vector(mode="character",length=length(qbs))
qbs_l<-vector(mode="character",length=length(qbs))

# Getting the first and last name of the QB and storing it in vectors
for(i in 1:length(qbs)) {
spap<-gregexpr(pattern =' ',qbs[i])[[1]][1]
qbs_f[i]<-substr(qbs[i],1,spap-1)
qbs_l[i]<-substr(qbs[i],spap+1,nchar(qbs[i]))
}

# Frame with first and last name
qbs_frame<-data.frame(first_name=qbs_f,last_name=qbs_l)

# Ordered by last name
qbs_frame<-qbs_frame[order(qbs_frame$last_name),]

# Resets row names
row.names(qbs_frame)<-NULL

# List where we store the stats
qb_stats<-list(mode="character",length=length(qbs))

# Loop over names where we scrape the QB stats
for(i in 1:dim(qbs_frame)[1]) {

# To get the correct url we need part of first and last name
first_name<-qbs_frame[i,1]
last_name<-qbs_frame[i,2]

ln_len=nchar(as.character(last_name))
if(substr(last_name,ln_len-2,ln_len)=="Jr.") {
last_name<-substr(last_name,1,ln_len-3)
}
if(gregexpr(pattern =' ',last_name)[[1]][1]>-1) {
last_name<-gsub(" ","-",last_name)
}

print(i)
l_name<-substr(last_name,1,4)
fl_l_name<-substr(last_name,1,1)
f_name<-substr(first_name,1,2)

if(gregexpr(pattern ='\'',last_name)[[1]][1]>-1) {
l_name<-str_c(substr(last_name,1,1),substr(last_name,3,5),collapse=NULL)
}

if(gregexpr(pattern ='-',last_name)[[1]][1]>-1) {
l_name<-substr(gsub("-","",last_name),1,4)
}

print(first_name)
print(last_name)

#

# In the next we check if the player is a QB again and we loop over Num till we find a QB or the page does not exist\
# In the process we recover the QB stats
# This has to be done because of players with the same name in different positions

# Position of text with the position name in the page
x_path='//*[@id="meta"]/div[2]/p[2]'

Num<-0

if(last_name=="McGloin") {
Num<-1
}

url0 <-str_c("https://www.pro-football-reference.com/players/",fl_l_name,"/",l_name,f_name,"0",Num,".htm",collapse=NULL)
qb_check <- url0 %>%
  read_html() %>%
  html_nodes(xpath=x_path) %>%
  html_text()

if(length(qb_check)==0) {
x_path='//*[@id="meta"]/div/p[2]'

qb_check <- url0 %>%
  read_html() %>%
  html_nodes(xpath=x_path) %>%
  html_text()
}

# Looping over Num till QB found
while(substr(qb_check,13,14)!="QB") {
Num<-Num+1
url0 <-str_c("https://www.pro-football-reference.com/players/",fl_l_name,"/",l_name,f_name,"0",as.character(Num),".htm",collapse=NULL)
qb_check <- url0 %>%
  read_html() %>%
  html_nodes(xpath=x_path) %>%
  html_text()
}

print(substr(qb_check,13,14))

# Obtaining the stats from the table with correct Num
#
# converting numeric Num to character Num
Num<-as.character(Num)
print(Num)
url2 <-str_c("https://www.pro-football-reference.com/players/",fl_l_name,"/",l_name,f_name,"0",Num,".htm",collapse=NULL)
print(url2)
qb_table <- url2 %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="passing"]') %>%
  html_table()

if(length(qb_table)!=0) {
qb_stats[i]<-qb_table
}


}

# Removing a problematic row in the qbs_frame
qbs_frame<-qbs_frame[-80,]
# Resets the row names
row.names(qbs_frame)<-NULL

# Removing empty rows
qb_stats<-qb_stats[!sapply(qb_stats,is.null)]

#####################################

# Constructing the true classification from QBs with long enough career
y_true<-c()

library(dplyr)

# Looping over qb_stats tables
for(i in 1:length(qb_stats)) {

# Treating different number of columns
qb_car_a<-qb_stats[[i]][c(1:(which(qb_stats[[i]]$Year=="Career")-1)),c(1:dim(qb_stats[[i]])[2])]
qb_car_a<-qb_car_a[,-which(names(qb_car_a) %in% c("1D","QBR","4QC","GWD"))]
vv<-c(which(qb_stats[[i]]$Year %in% ""))
if(length(vv)>0) {qb_car<-qb_car_a[-vv,]} else {qb_car<-qb_car_a}
qb_car[is.na(qb_car)] <- 0

# Treating missed season as 0s
for(j in 1:dim(qb_car)[1]) {
if(substr(qb_car[j,'Tm'],1,13)=="Missed season") {
qb_car[j,colnames(qb_car)[c(3:dim(qb_car)[2])]]<-0
}
}

# Career length
car_len<-dim(qb_car)[1]
# Last career year
car_end<-substr(tail(qb_car['Year'],n=1),1,4)
# Average starter games
sg_ave<-sum(as.numeric(qb_car$GS))/dim(qb_car['GS'])[1]
# Average starter games in second half
sg_ave2<-sum(as.numeric(qb_car$GS[c(floor(car_len/2+1):car_len)]))/(car_len-floor(car_len/2))

# Processing QB record into a single number
QBrecc<-vector(mode="numeric",length=car_len)
for(k in 1:car_len) {
aux<-qb_car$QBrec[k]
p1<-gregexpr(pattern ='-',aux)[[1]][1]
p2<-gregexpr(pattern ='-',aux)[[1]][2]
QBrecc[k]<-(as.numeric(substr(aux,1,p1-1))+as.numeric(substr(aux,p1,p2-1)))
if(qb_car$G[k]==0) {qb_car[c(6:dim(qb_car[k])[2])]<-0}
}
QBrecc[is.na(QBrecc)] <- 0

# Replacing the QB record with the single numeric QB record
qb_car$QBrec<-as.numeric(QBrecc)


qb_car <- qb_car %>%
    mutate_if(is.factor, ~as.numeric(as.character(.)))

X_table<-as.data.frame(colSums(sapply(qb_car[c(1:3), c(6:dim(qb_car)[2])],function(x) as.numeric(as.character(x)))))
colnames(X_table)<-i
X_table <- as.data.frame(t(as.matrix(X_table)))
if(i==1) {X<-qb_car[0,]}
print(i)

if(i==60) {print(qb_car)}

X<-rbind(X,X_table)

if(is.na(X_table[3])&car_len>2) {print(str_c("car_len ",car_len))}

y_true[i]<-NA

# Assigning values to careers
if(car_len>=6) {
  if(sg_ave>10&sg_ave2>8) {
  y_true[i]<-1
  }
}
if(car_len>=6) {
  if(sg_ave<=10|sg_ave2<=8) {
  y_true[i]<-0
  }
}

if(car_len<6&car_end!="2020") {
y_true[i]<-0
}

if(car_len<3) {
y_true[i]<--1
}

}
discr<-is.na(y_true)
y_true_m1<-gsub(-1,NA,y_true)
discr2<-is.na(y_true_m1)
discr_neg<-!discr2

qb_stats_sub_train<-X[discr_neg,]
y_train<-y_true[discr_neg]

qb_stats_sub_pred<-X[discr,]

# Writing the data into csv files
write.csv(qb_stats_sub_train,"QBs-train.csv")
write.csv(y_train,"QBs-Y-train.csv")
write.csv(qb_stats_sub_pred,"QBs-pred.csv")
write.csv(qbs_frame,"QBs-names.csv")
