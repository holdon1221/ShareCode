# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())

setwd("D:\\research\\cnt")


# install.packages("digest")
library('reshape')
library('sqldf')
library('devtools')
library(dplyr)
#For information on how to use the socialmixr package↓
#https://cran.r-project.org/web/packages/socialmixr/vignettes/introduction.html



# contact data type1 ------------------------------------------------------

survey_rawdata=read.csv("data\\#200023_사회적 접촉 행태 설문조사_rawdata.csv")
head(survey_rawdata)

col_idx=data.frame(colnames(survey_rawdata))
col_idx_1=col_idx[substr(col_idx[,1], 1, 4)=='a016',1]


cont_id = c()
part_id = c()

n = nrow(survey_rawdata)
n1=length(col_idx_1)
for (i in 1:n){
  for (j in 1:n1){
    contact_id = paste(i,'-1-',j, sep = '')
    cont_id = c(cont_id,contact_id)
    part_id = c(part_id,i)
    
  }
}

m=length(cont_id)
cnt_age_exact = rep(0,m)
cnt_age_est = rep(0,m)
cnt_gender = rep(0,m)
frequency_multi = rep(0,m)
duration_multi = rep(0,m)
phys_contact1 = rep(0,m)
phys_contact2 = rep(0,m)
cnt_place1 = rep(0,m)
cnt_place2 = rep(0,m)
cnt_place3 = rep(0,m)
cnt_place4 = rep(0,m)


for (i in 1:n){
  for (j in 1:9){
    cnt_age_exact[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a01110',j, sep = '')]
    cnt_age_est[n1*(i-1)+j] = as.character(survey_rawdata[i,colnames(survey_rawdata)==paste('a01120',j, sep = '')])
    cnt_gender[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a0120',j, sep = '')]
    phys_contact1[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a0130',j,'1', sep = '')]
    phys_contact2[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a0130',j,'2', sep = '')]
    frequency_multi[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a0140',j, sep = '')]
    duration_multi[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a0160',j, sep = '')]
    
    cnt_place1[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a0150',j,'1', sep = '')]
    cnt_place2[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a0150',j,'2', sep = '')]
    cnt_place3[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a0150',j,'3', sep = '')]
    cnt_place4[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a0150',j,'4', sep = '')]
    
  }
  for (j in 10:n1){
    cnt_age_exact[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a0111',j, sep = '')]
    cnt_age_est[n1*(i-1)+j] =  as.character(survey_rawdata[i,colnames(survey_rawdata)==paste('a0112',j, sep = '')])
    cnt_gender[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a012',j, sep = '')]
    phys_contact1[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a013',j,'1', sep = '')]
    phys_contact2[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a013',j,'2', sep = '')]
    frequency_multi[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a014',j, sep = '')]
    duration_multi[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a016',j, sep = '')]
    
    cnt_place1[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a015',j,'1', sep = '')]
    cnt_place2[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a015',j,'2', sep = '')]
    cnt_place3[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a015',j,'3', sep = '')]
    cnt_place4[n1*(i-1)+j] = survey_rawdata[i,colnames(survey_rawdata)==paste('a015',j,'4', sep = '')]
  }
}

cnt_age = rep(999,length(cnt_age_est))
cnt_age_est_min = rep(999,length(cnt_age_est))
cnt_age_est_max = rep(999,length(cnt_age_est))
for (i in 1:length(cnt_age_est)){
  if (cnt_age_est[i]!="" | !is.na(cnt_age_exact[i])){
    
    if (substr(cnt_age_est[i],5,6)=="초반") {
      cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_est[i],1,1),0,sep=''))
      cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_est[i],1,1),4,sep=''))
      cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
    } else if (substr(cnt_age_est[i],5,6)=="중반") {
      cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_est[i],1,1),3,sep=''))
      cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_est[i],1,1),7,sep=''))
      cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
    } else if (substr(cnt_age_est[i],5,6)=="후반") {
      cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_est[i],1,1),5,sep=''))
      cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_est[i],1,1),9,sep=''))
      cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
    } else if (substr(cnt_age_est[i],5,7)=="중후반") {
      cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_est[i],1,1),5,sep=''))
      cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_est[i],1,1),9,sep=''))
      cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
    } else if (substr(cnt_age_est[i],3,3)=="대") {
      cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_est[i],1,1),0,sep=''))
      cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_est[i],1,1),9,sep=''))
      cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
    } else if (substr(cnt_age_est[i],3,3)=="~") {
      cnt_age_est_min[i] = as.numeric(substr(cnt_age_est[i],1,2))
      cnt_age_est_max[i] = as.numeric(substr(cnt_age_est[i],4,5))
      cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
    } else if (substr(cnt_age_est[i],2,2)=="~") {
      cnt_age_est_min[i] = as.numeric(substr(cnt_age_est[i],1,1))
      cnt_age_est_max[i] = as.numeric(substr(cnt_age_est[i],3,3))
      cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
    } else if (cnt_age_est[i]!="") {
      cnt_age_est_min[i] = as.numeric(cnt_age_est[i])
      cnt_age_est_max[i] = as.numeric(cnt_age_est[i])
      cnt_age_est[i] = as.numeric(cnt_age_est[i])
      cnt_age[i] = as.numeric(cnt_age_est[i])
    } else if (!is.na(cnt_age_exact[i])) {
      cnt_age_est_min[i] = as.numeric(cnt_age_exact[i])
      cnt_age_est_max[i] = as.numeric(cnt_age_exact[i])
      cnt_age_est[i] = as.numeric(cnt_age_exact[i])
      cnt_age[i]=cnt_age_exact[i]
    }  
  }
}
cnt_age_est_min[cnt_age_est=="30대 중반 40대 초"] = 35
cnt_age_est_max[cnt_age_est=="30대 중반 40대 초"] = 45
cnt_age[cnt_age_est=="30대 중반 40대 초"] = 40
cnt_age_est_min[cnt_age_est=="30대 후반~40대 초반"] = 37
cnt_age_est_max[cnt_age_est=="30대 후반~40대 초반"] = 43
cnt_age[cnt_age_est=="30대 후반~40대 초반"] = 40

survey.data = data.frame(cont_id,part_id,cnt_age_exact,cnt_age_est,cnt_age_est_min,cnt_age_est_max,cnt_gender,frequency_multi,phys_contact1,phys_contact2,duration_multi,cnt_place1,cnt_place2,cnt_place3,cnt_place4,cnt_age)
survey.data = survey.data[!is.na(survey.data$frequency_multi),]

m1 = length(survey.data[,1])
phys_contact = rep(0,m1)
for (i in 1:m1){
  if (survey.data$phys_contact1[i]==1 & !is.na(survey.data$phys_contact1[i])){
    phys_contact[i]=1
  }
  else{
    phys_contact[i]=0
  }
}


df=survey.data[c(1,12:15)]
head(df)

for (i in 1:m1){
  for (j in 2:5){
    if (is.na(df[i,j])){
      df[i,j]=0
    }
  }
}
head(df)
df$cnt_place1 = as.factor(df$cnt_place1)
df$cnt_place2 = as.factor(df$cnt_place2)
df$cnt_place3 = as.factor(df$cnt_place3)
df$cnt_place4 = as.factor(df$cnt_place4)

head(df)
head(model.matrix(~-1+cnt_place1, data=df))
head(model.matrix(~-1+cnt_place2, data=df))
head(model.matrix(~-1+cnt_place1+cnt_place2+cnt_place3+cnt_place4, data=df))

cnt_place=model.matrix(~-1+cnt_place1+cnt_place2+cnt_place3+cnt_place4, data=df)
head(cnt_place)
#length(cnt_home)==length(cnt_place[,2])

cnt_home = cnt_place[,"cnt_place11"]
cnt_school = cnt_place[,"cnt_place12"]
cnt_work = cnt_place[,"cnt_place13"]+cnt_place[,"cnt_place23"]
cnt_transport = cnt_place[,"cnt_place14"]+cnt_place[,"cnt_place24"]
cnt_leisure = cnt_place[,"cnt_place15"]+cnt_place[,"cnt_place25"]+cnt_place[,"cnt_place35"]
cnt_hospital = cnt_place[,"cnt_place16"]+cnt_place[,"cnt_place36"]
cnt_otherplace = cnt_place[,"cnt_place17"]+cnt_place[,"cnt_place27"]+cnt_place[,"cnt_place37"]+cnt_place[,"cnt_place47"]
survey.data = data.frame(survey.data,phys_contact,cnt_home,cnt_school,cnt_work,cnt_transport,cnt_leisure,cnt_hospital,cnt_otherplace)

survey.contacts.1 = survey.data
head(survey.contacts.1)
#X = survey.contacts.1[is.na(survey.contacts.1$cnt_age_exact),c(1,4)]
#X
#class(survey.contacts.1$cnt_age_est)

#cnt_age[cnt_age==999]
#summary(cnt_age)
#length(cnt_age)
#class(cnt_age)


cnt_age_grp = rep(999,length(survey.contacts.1$cnt_age_exact))
for (i in 1:length(survey.contacts.1$cnt_age_exact)){
  if (!is.na(survey.contacts.1$cnt_age_exact[i])){
    if (survey.contacts.1$cnt_age_exact[i]<10) {cnt_age_grp[i]=0}
    else if (survey.contacts.1$cnt_age_exact[i]<20) {cnt_age_grp[i]=1}
    else if (survey.contacts.1$cnt_age_exact[i]<30) {cnt_age_grp[i]=2}
    else if (survey.contacts.1$cnt_age_exact[i]<40) {cnt_age_grp[i]=3}
    else if (survey.contacts.1$cnt_age_exact[i]<50) {cnt_age_grp[i]=4}
    else if (survey.contacts.1$cnt_age_exact[i]<60) {cnt_age_grp[i]=5}
    else if (survey.contacts.1$cnt_age_exact[i]<70) {cnt_age_grp[i]=6}
    else if (survey.contacts.1$cnt_age_exact[i]<80) {cnt_age_grp[i]=7}
    else if (survey.contacts.1$cnt_age_exact[i]<90) {cnt_age_grp[i]=8}
    else {cnt_age_grp[i]=9}
  }
  else if (survey.contacts.1$cnt_age_est[i]!=""){
    if (substr(survey.contacts.1$cnt_age_est[i],1,2)=="7~"){cnt_age_grp[i]=0}
    else if (as.numeric(substr(survey.contacts.1$cnt_age_est[i],1,2))<10 ) {cnt_age_grp[i]=0}
    else if (as.numeric(substr(survey.contacts.1$cnt_age_est[i],1,2))<20) {cnt_age_grp[i]=1}
    else if (as.numeric(substr(survey.contacts.1$cnt_age_est[i],1,2))<30) {cnt_age_grp[i]=2}
    else if (as.numeric(substr(survey.contacts.1$cnt_age_est[i],1,2))<40) {cnt_age_grp[i]=3}
    else if (as.numeric(substr(survey.contacts.1$cnt_age_est[i],1,2))<50) {cnt_age_grp[i]=4}
    else if (as.numeric(substr(survey.contacts.1$cnt_age_est[i],1,2))<60) {cnt_age_grp[i]=5}
    else if (as.numeric(substr(survey.contacts.1$cnt_age_est[i],1,2))<70) {cnt_age_grp[i]=6}
    else if (as.numeric(substr(survey.contacts.1$cnt_age_est[i],1,2))<80) {cnt_age_grp[i]=7}
    else if (as.numeric(substr(survey.contacts.1$cnt_age_est[i],1,2))<90) {cnt_age_grp[i]=8}
    else {cnt_age_grp[i]=9}}
}  

survey.contacts.1 = data.frame(survey.contacts.1,cnt_age_grp)
head(survey.contacts.1)
survey.contacts.1[survey.contacts.1$cnt_age==999,]

# -------------------------------------------------------------------------
#survey.contacts type 2 

col_idx=data.frame(colnames(survey_rawdata))
#col_idx_2=col_idx[col_idx[,1]=='id'|substr(col_idx[,1], 1, 3)=='a02' | substr(col_idx[,1], 1, 3)=='b02',1]
survey_rawdata_type2 = survey_rawdata[,colnames(survey_rawdata)=='id'|substr(colnames(survey_rawdata),1,3)=='a02'|substr(colnames(survey_rawdata),1,3)=='b02'|substr(colnames(survey_rawdata),1,3)=='c02']


cont_id = c()
part_id = c()

survey_type2_1 = survey_rawdata_type2[!is.na(survey_rawdata_type2$a02301),] #a02301 = 접촉자수
survey_type2_2 = survey_rawdata_type2[!is.na(survey_rawdata_type2$a02302),] 

for (i in 1:nrow(survey_type2_1)){
  for (j in 1:ncol(survey_type2_1)){
    if (is.na(survey_type2_1[i,j])) {survey_type2_1[i,j]=0}
  }
}

for (i in 1:nrow(survey_type2_2)){
  for (j in 1:ncol(survey_type2_2)){
    if (is.na(survey_type2_2[i,j])) {survey_type2_2[i,j]=0}
  }
}  


cnt_age_avg1 = c()
cnt_age_avg2 = c()
duration = c()
phys_contact = c()
cnt_place = c()
cnt_behav = c()


n1 = nrow(survey_type2_1)
for (i in 1:n1){
  m1 = survey_type2_1$a02301[i]
  for (j in 1:m1){
    contact_id = paste(survey_type2_1$id[i],'-2-',j, sep = '')
    participant_id = survey_type2_1$id[i]
    cont_id = c(cont_id,contact_id)
    part_id = c(part_id,participant_id)
    
    cnt_age_avg1 = c(cnt_age_avg1,as.character(survey_type2_1$a02401[i]))
    cnt_age_avg2 = c(cnt_age_avg2,survey_type2_1$b02401[i])
    duration = c(duration,survey_type2_1$a02501[i])
    
    if (survey_type2_1$a022011[i]==1){
      phys_contact = c(phys_contact,1)
    }
    else {phys_contact = c(phys_contact,0)}
    
    cnt_place = c(cnt_place,survey_type2_1$b02101[i])
    cnt_behav = c(cnt_behav,survey_type2_1$c02101[i]) 
  }
}


n2 = nrow(survey_type2_2)
for (i in 1:n2){
  m1 = survey_type2_1$a02301[i]
  m2 = survey_type2_2$a02302[i]
  
  for (j in (m1+1):(m1+m2)){
    contact_id = paste(survey_type2_2$id[i],'-2-',j, sep = '')
    participant_id = survey_type2_2$id[i]
    
    cont_id = c(cont_id,contact_id)
    part_id = c(part_id,participant_id)
    
    cnt_age_avg1 = c(cnt_age_avg1,as.character(survey_type2_2$a02402[i]))
    cnt_age_avg2 = c(cnt_age_avg2,survey_type2_2$b02402[i])
    duration = c(duration,survey_type2_2$a02502[i])
    
    if (survey_type2_2$a022021[i]==1){
      phys_contact = c(phys_contact,1)
    }
    else {phys_contact = c(phys_contact,0)}
    
    cnt_place = c(cnt_place,survey_type2_2$b02102[i])
    cnt_behav = c(cnt_behav,survey_type2_2$c02102[i]) 
    
    
  }
}

duration_multi = rep(0,length(part_id))
duration_multi[duration<5]=1
duration_multi[duration>=5 & duration<15]=2
duration_multi[duration>=15 & duration<60]=3
duration_multi[duration>=60 & duration<240]=4
duration_multi[duration>=240 & duration<480]=5
duration_multi[duration>=480]=6

duration_multi


frequency_multi = rep(1,length(part_id))
cnt_work = rep(1,length(part_id))
cnt_home = rep(0,length(part_id))
cnt_school = rep(0,length(part_id))
cnt_transport = rep(0,length(part_id))
cnt_leisure = rep(0,length(part_id))
cnt_hospital = rep(0,length(part_id))
cnt_otherplace = rep(0,length(part_id))

cnt_age = rep(999,length(cnt_age_avg1))
cnt_age_est_min = rep(999,length(cnt_age_avg1))
cnt_age_est_max = rep(999,length(cnt_age_avg1))
for (i in 1:length(cnt_age_avg1)){
  if (substr(cnt_age_avg1[i],5,6)=="초반") {
    cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),0,sep=''))
    cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),4,sep=''))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (substr(cnt_age_avg1[i],5,6)=="중반") {
    cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),3,sep=''))
    cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),7,sep=''))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (substr(cnt_age_avg1[i],5,6)=="후반") {
    cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),5,sep=''))
    cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),9,sep=''))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (substr(cnt_age_avg1[i],5,7)=="중후반") {
    cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),5,sep=''))
    cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),9,sep=''))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (substr(cnt_age_avg1[i],3,3)=="대") {
    cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),0,sep=''))
    cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),9,sep=''))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (substr(cnt_age_avg1[i],3,3)=="~") {
    cnt_age_est_min[i] = as.numeric(substr(cnt_age_avg1[i],1,2))
    cnt_age_est_max[i] = as.numeric(substr(cnt_age_avg1[i],4,5))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (substr(cnt_age_avg1[i],2,2)=="~") {
    cnt_age_est_min[i] = as.numeric(substr(cnt_age_avg1[i],1,1))
    cnt_age_est_max[i] = as.numeric(substr(cnt_age_avg1[i],3,4))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (cnt_age_avg1[i]!="") {
    cnt_age_est_min[i] = as.numeric(cnt_age_avg1[i])
    cnt_age_est_max[i] = as.numeric(cnt_age_avg1[i])
    cnt_age[i] = as.numeric(cnt_age_avg1[i])
  } else if (!is.na(cnt_age_exact[i])) {
    cnt_age_est_min[i] = cnt_age_exact[i]
    cnt_age_est_max[i] = cnt_age_exact[i]
    cnt_age[i]=cnt_age_exact[i]
  }  
}


#cnt_age
summary(cnt_age)
survey.contacts.2 = data.frame(cont_id,part_id,cnt_age_avg1,cnt_age_avg2,frequency_multi,phys_contact,duration,duration_multi,cnt_place,cnt_behav,cnt_home, cnt_school, cnt_work, cnt_transport, cnt_leisure, cnt_hospital, cnt_otherplace,cnt_age,cnt_age_est_min,cnt_age_est_max)


#survey.contacts.2[survey.contacts.2$part_id==275,]
#nchar(cnt_age_avg[944])
#X = cnt_age_avg[nchar(cnt_age_avg)>2]
#X = data.frame(X)
#sqldf('SELECT DISTINCT * FROM X ')

cnt_age_grp = survey.contacts.2$cnt_age_avg2
survey.contacts.2 = data.frame(survey.contacts.2,cnt_age_grp)
head(survey.contacts.1)
head(survey.contacts.2)

# -------------------------------------------------------------------------
#survey.contacts type 3 

survey_rawdata_type3 = survey_rawdata[,colnames(survey_rawdata)=='id'|substr(colnames(survey_rawdata),1,3)=='a03'|substr(colnames(survey_rawdata),1,3)=='b03']
head(survey_rawdata_type3)
cont_id = c()
part_id = c()

for(i in 1:8) { 
  nam = paste("survey_type3_", i, sep = "")
  survey_rawdata_type3_x = cbind(survey_rawdata_type3[,1],survey_rawdata_type3[,(2+(i-1)*8):(9+(i-1)*8)])
  colnames(survey_rawdata_type3_x)[1]="id"
  survey_rawdata_type3_x = survey_rawdata_type3_x[!is.na(survey_rawdata_type3_x[,3]),]#접촉자수 NA인 대상 제외
  assign(nam,survey_rawdata_type3_x)
  
}

#summary((is.na(survey_type3_6)))

head(survey_type3_1)

cnt_age_avg1 = c()
cnt_age_avg2 = c()
duration = c()
phys_contact = c()
cnt_place = c()


n1 = nrow(survey_type3_1)
n2 = nrow(survey_type3_2)
n3 = nrow(survey_type3_3)
n4 = nrow(survey_type3_4)
n5 = nrow(survey_type3_5)
n6 = nrow(survey_type3_6)
n7 = nrow(survey_type3_7)
n8 = nrow(survey_type3_8)

for (i in 1:n1){
  survey_type3=survey_type3_1
  m1 = survey_type3[i,4]
  for (j in 1:m1){
    contact_id = paste(survey_type3[i,1],'-3-',j, sep = '')
    participant_id = survey_type3[i,1]
    cont_id = c(cont_id,contact_id)
    part_id = c(part_id,participant_id)
    
    cnt_age_avg1 = c(cnt_age_avg1,as.character(survey_type3[i,5]))
    cnt_age_avg2 = c(cnt_age_avg2,survey_type3[i,6])
    duration = c(duration,survey_type3[i,9])
    
    phys_contact = c(phys_contact,0)
    
    cnt_place = c(cnt_place,survey_type3[i,3])
  }
}


for (i in 1:n2){
  survey_type3=survey_type3_2
  m1 = survey_type3_1[i,4]
  m2 = survey_type3[i,4]
  for (j in (m1+1):(m1+m2)){
    contact_id = paste(survey_type3[i,1],'-3-',j, sep = '')
    participant_id = survey_type3[i,1]
    cont_id = c(cont_id,contact_id)
    part_id = c(part_id,participant_id)
    
    cnt_age_avg1 = c(cnt_age_avg1,as.character(survey_type3[i,5]))
    cnt_age_avg2 = c(cnt_age_avg2,survey_type3[i,6])
    duration = c(duration,survey_type3[i,9])
    
    phys_contact = c(phys_contact,0)
    
    cnt_place = c(cnt_place,survey_type3[i,3])
  }
}


for (i in 1:n3){
  survey_type3=survey_type3_3
  m1 = survey_type3_1[i,4]+survey_type3_2[i,4]
  m2 = survey_type3[i,4]
  for (j in (m1+1):(m1+m2)){
    contact_id = paste(survey_type3[i,1],'-3-',j, sep = '')
    participant_id = survey_type3[i,1]
    cont_id = c(cont_id,contact_id)
    part_id = c(part_id,participant_id)
    
    cnt_age_avg1 = c(cnt_age_avg1,as.character(survey_type3[i,5]))
    cnt_age_avg2 = c(cnt_age_avg2,survey_type3[i,6])
    duration = c(duration,survey_type3[i,9])
    
    phys_contact = c(phys_contact,0)
    
    cnt_place = c(cnt_place,survey_type3[i,3])
  }
}

for (i in 1:n4){
  survey_type3=survey_type3_4
  m1 = survey_type3_1[i,4]+survey_type3_2[i,4]+survey_type3_3[i,4]
  m2 = survey_type3[i,4]
  for (j in (m1+1):(m1+m2)){
    contact_id = paste(survey_type3[i,1],'-3-',j, sep = '')
    participant_id = survey_type3[i,1]
    cont_id = c(cont_id,contact_id)
    part_id = c(part_id,participant_id)
    
    cnt_age_avg1 = c(cnt_age_avg1,as.character(survey_type3[i,5]))
    cnt_age_avg2 = c(cnt_age_avg2,survey_type3[i,6])
    duration = c(duration,survey_type3[i,9])
    
    phys_contact = c(phys_contact,0)
    
    cnt_place = c(cnt_place,survey_type3[i,3])
  }
}

for (i in 1:n5){
  survey_type3=survey_type3_5
  m1 = survey_type3_1[i,4]+survey_type3_2[i,4]+survey_type3_3[i,4]+survey_type3_4[i,4]
  m2 = survey_type3[i,4]
  for (j in (m1+1):(m1+m2)){
    contact_id = paste(survey_type3[i,1],'-3-',j, sep = '')
    participant_id = survey_type3[i,1]
    cont_id = c(cont_id,contact_id)
    part_id = c(part_id,participant_id)
    
    cnt_age_avg1 = c(cnt_age_avg1,as.character(survey_type3[i,5]))
    cnt_age_avg2 = c(cnt_age_avg2,survey_type3[i,6])
    duration = c(duration,survey_type3[i,9])
    
    phys_contact = c(phys_contact,0)
    
    cnt_place = c(cnt_place,survey_type3[i,3])
  }
}

for (i in 1:n6){
  survey_type3=survey_type3_6
  m1 = survey_type3_1[i,4]+survey_type3_2[i,4]+survey_type3_3[i,4]+survey_type3_4[i,4]+survey_type3_5[i,4]
  m2 = survey_type3[i,4]
  for (j in (m1+1):(m1+m2)){
    contact_id = paste(survey_type3[i,1],'-3-',j, sep = '')
    participant_id = survey_type3[i,1]
    cont_id = c(cont_id,contact_id)
    part_id = c(part_id,participant_id)
    
    cnt_age_avg1 = c(cnt_age_avg1,as.character(survey_type3[i,5]))
    cnt_age_avg2 = c(cnt_age_avg2,survey_type3[i,6])
    duration = c(duration,survey_type3[i,9])
    
    phys_contact = c(phys_contact,0)
    
    cnt_place = c(cnt_place,survey_type3[i,3])
  }
}

for (i in 1:n7){
  survey_type3=survey_type3_7
  m1 = survey_type3_1[i,4]+survey_type3_2[i,4]+survey_type3_3[i,4]+survey_type3_4[i,4]+survey_type3_5[i,4]+survey_type3_6[i,4]
  m2 = survey_type3[i,4]
  for (j in (m1+1):(m1+m2)){
    contact_id = paste(survey_type3[i,1],'-3-',j, sep = '')
    participant_id = survey_type3[i,1]
    cont_id = c(cont_id,contact_id)
    part_id = c(part_id,participant_id)
    
    cnt_age_avg1 = c(cnt_age_avg1,as.character(survey_type3[i,5]))
    cnt_age_avg2 = c(cnt_age_avg2,survey_type3[i,6])
    duration = c(duration,survey_type3[i,9])
    
    phys_contact = c(phys_contact,0)
    
    cnt_place = c(cnt_place,survey_type3[i,3])
  }
}

for (i in 1:n8){
  survey_type3=survey_type3_8
  m1 = survey_type3_1[i,4]+survey_type3_2[i,4]+survey_type3_3[i,4]+survey_type3_4[i,4]+survey_type3_5[i,4]+survey_type3_6[i,4]+survey_type3_7[i,4]
  m2 = survey_type3[i,4]
  for (j in (m1+1):(m1+m2)){
    contact_id = paste(survey_type3[i,1],'-3-',j, sep = '')
    participant_id = survey_type3[i,1]
    cont_id = c(cont_id,contact_id)
    part_id = c(part_id,participant_id)
    
    cnt_age_avg1 = c(cnt_age_avg1,as.character(survey_type3[i,5]))
    cnt_age_avg2 = c(cnt_age_avg2,survey_type3[i,6])
    duration = c(duration,survey_type3[i,9])
    
    phys_contact = c(phys_contact,0)
    
    cnt_place = c(cnt_place,survey_type3[i,3])
  }
}


duration_multi = rep(0,length(part_id))
duration_multi[duration<5]=1
duration_multi[duration>=5 & duration<15]=2
duration_multi[duration>=15 & duration<60]=3
duration_multi[duration>=60 & duration<240]=4
duration_multi[duration>=240 & duration<480]=5
duration_multi[duration>=480]=6

#duration_multi


#frequency_multi = rep(1,length(part_id))
cnt_work = rep(0,length(part_id))
cnt_home = rep(0,length(part_id))
cnt_school = rep(0,length(part_id))
cnt_transport = rep(0,length(part_id))
cnt_leisure = rep(0,length(part_id))
cnt_hospital = rep(0,length(part_id))
cnt_otherplace = rep(0,length(part_id))

place_code = read.csv("data\\cnt_place.csv")
place_code

survey.contacts.3 = data.frame(cont_id,part_id,cnt_age_avg1,cnt_age_avg2,phys_contact,duration,duration_multi,cnt_place)
survey.contacts.3 = merge(x = survey.contacts.3, y = place_code, by ='cnt_place',all.x = TRUE,no.dups = TRUE)

cnt_home[survey.contacts.3$place_code==1]=1
cnt_work[survey.contacts.3$place_code==3]=1
cnt_school[survey.contacts.3$place_code==2]=1
cnt_transport[survey.contacts.3$place_code==4]=1
cnt_leisure[survey.contacts.3$place_code==5]=1
cnt_hospital[survey.contacts.3$place_code==6]=1
cnt_otherplace[survey.contacts.3$place_code==7]=1


frequency_multi = rep(6,length(part_id))
cnt_age_grp = survey.contacts.3$cnt_age_avg2

cnt_age = rep(999,length(cnt_age_avg1))
cnt_age_est_min = rep(999,length(cnt_age_avg1))
cnt_age_est_max = rep(999,length(cnt_age_avg1))
for (i in 1:length(cnt_age_avg1)){
  if (substr(cnt_age_avg1[i],5,6)=="초반") {
    cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),0,sep=''))
    cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),4,sep=''))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (substr(cnt_age_avg1[i],5,6)=="중반") {
    cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),3,sep=''))
    cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),7,sep=''))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (substr(cnt_age_avg1[i],5,6)=="후반") {
    cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),5,sep=''))
    cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),9,sep=''))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (substr(cnt_age_avg1[i],5,7)=="중후반") {
    cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),5,sep=''))
    cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),9,sep=''))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (substr(cnt_age_avg1[i],3,3)=="대") {
    cnt_age_est_min[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),0,sep=''))
    cnt_age_est_max[i] = as.numeric(paste(substr(cnt_age_avg1[i],1,1),9,sep=''))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (substr(cnt_age_avg1[i],3,3)=="~") {
    cnt_age_est_min[i] = as.numeric(substr(cnt_age_avg1[i],1,2))
    cnt_age_est_max[i] = as.numeric(substr(cnt_age_avg1[i],4,5))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (substr(cnt_age_avg1[i],2,2)=="~") {
    cnt_age_est_min[i] = as.numeric(substr(cnt_age_avg1[i],1,1))
    cnt_age_est_max[i] = as.numeric(substr(cnt_age_avg1[i],3,4))
    cnt_age[i] = (cnt_age_est_min[i]+cnt_age_est_max[i])/2
  } else if (cnt_age_avg1[i]!="") {
    cnt_age_est_min[i] = as.numeric(cnt_age_avg1[i])
    cnt_age_est_max[i] = as.numeric(cnt_age_avg1[i])
    cnt_age[i] = as.numeric(cnt_age_avg1[i])
  } else if (!is.na(cnt_age_exact[i])) {
    cnt_age_est_min[i] = cnt_age_exact[i]
    cnt_age_est_max[i] = cnt_age_exact[i]
    cnt_age[i]=cnt_age_exact[i]
  }  
}

#cnt_age
summary(cnt_age)

survey.contacts.3 = data.frame(survey.contacts.3,frequency_multi,cnt_home, cnt_school, cnt_work, cnt_transport, cnt_leisure, cnt_hospital, cnt_otherplace, cnt_age_grp,cnt_age,cnt_age_est_min,cnt_age_est_max)


head(survey.contacts.1)
head(survey.contacts.2)
head(survey.contacts.3)

#write.csv(survey.contacts.1,"survey_contatcs_1.csv")
#write.csv(survey.contacts.2,"survey_contatcs_2.csv")
#write.csv(survey.contacts.3,"survey_contatcs_3.csv")


# -------------------------------------------------------------------------
#survey$participants build

head(survey_rawdata)
part_id = survey_rawdata$id
part_age = survey_rawdata$age
part_birth = survey_rawdata$birth
part_gender = survey_rawdata$sex
part_occupation = survey_rawdata$a041
part_occupation_hospital = survey_rawdata$a0411
part_occupation_detail = survey_rawdata$a042
part_occupation_blood1 = survey_rawdata$a0431
part_occupation_blood2 = survey_rawdata$a0432
part_region = survey_rawdata$area
part_region_detail = survey_rawdata$area1
part_influenza = survey_rawdata$a06
part_hypertension = survey_rawdata$a0701
part_diabetes = survey_rawdata$a0702
part_arthritis = survey_rawdata$a0703
part_heart = survey_rawdata$a0704
part_depression = survey_rawdata$a0705
part_asthma = survey_rawdata$a0706
part_tuber = survey_rawdata$a0707
part_Hepatitis_A = survey_rawdata$a0708
part_Hepatitis_B = survey_rawdata$a0709
part_Hepatitis_C = survey_rawdata$a0710
part_other_disease = survey_rawdata$a0711
part_other_disease2 = survey_rawdata$xa0711

part = data.frame(part_id,part_age,part_birth,part_gender,part_occupation,part_occupation_hospital,part_occupation_detail,part_occupation_blood1,part_occupation_blood2,
                  part_region,part_region_detail,part_influenza,part_hypertension,part_diabetes,part_arthritis,part_heart,part_depression,part_asthma,part_tuber,part_Hepatitis_A
                  ,part_Hepatitis_B,part_Hepatitis_C,part_other_disease,part_other_disease2) 

hh_id = paste('MH20HH',part_id,sep='')
hh_size = survey_rawdata$a05
hh_age1 = survey_rawdata$a053101
hh_age1_est = survey_rawdata$a053201
hh_age2 = survey_rawdata$a053102
hh_age2_est = survey_rawdata$a053202
hh_age3 = survey_rawdata$a053103
hh_age3_est = survey_rawdata$a053203
hh_age4 = survey_rawdata$a053104
hh_age4_est = survey_rawdata$a053204
hh_age5 = survey_rawdata$a053105
hh_age5_est = survey_rawdata$a053205
hh_age6 = survey_rawdata$a053106
hh_age6_est = survey_rawdata$a053206
hh_age7 = survey_rawdata$a053107
hh_age7_est = survey_rawdata$a053207

hh = data.frame(hh_id,hh_size,hh_age1,hh_age1_est,hh_age2,hh_age2_est,hh_age3,hh_age3_est,hh_age4,hh_age4_est,hh_age5,hh_age5_est,hh_age6,hh_age6_est,hh_age7,hh_age7_est)

survey_date = paste(survey_rawdata$year,'-',survey_rawdata$mon,'-',survey_rawdata$day,sep='')
contact_date = paste('2020-',survey_rawdata$date1,'-',survey_rawdata$date2,sep='')
contact_day = survey_rawdata$date3

survey.participants = data.frame(part,hh,survey_date,contact_date,contact_day)

part_age_grp = rep(999,length(survey.participants$part_age))
part_age_grp[survey.participants$part_age<10]=0
part_age_grp[survey.participants$part_age>=10&survey.participants$part_age<20]=1
part_age_grp[survey.participants$part_age>=20&survey.participants$part_age<30]=2
part_age_grp[survey.participants$part_age>=30&survey.participants$part_age<40]=3
part_age_grp[survey.participants$part_age>=40&survey.participants$part_age<50]=4
part_age_grp[survey.participants$part_age>=50&survey.participants$part_age<60]=5
part_age_grp[survey.participants$part_age>=60&survey.participants$part_age<70]=6
part_age_grp[survey.participants$part_age>=70&survey.participants$part_age<80]=7
part_age_grp[survey.participants$part_age>=80&survey.participants$part_age<90]=8
part_age_grp[survey.participants$part_age>=90]=9

part_age_grp_2 = part_age_grp
part_age_grp_2[part_age_grp_2==8|part_age_grp_2==9]=7

part_age_grp_3 = part_age_grp
part_age_grp_3[part_age_grp==8|part_age_grp==9]=8

part_age_grp_4 = rep(999,length(survey.participants$part_age))
part_age_grp_4[survey.participants$part_age<5]=0
part_age_grp_4[survey.participants$part_age>=5&survey.participants$part_age<10]=1
part_age_grp_4[survey.participants$part_age>=10&survey.participants$part_age<15]=2
part_age_grp_4[survey.participants$part_age>=15&survey.participants$part_age<20]=3
part_age_grp_4[survey.participants$part_age>=20&survey.participants$part_age<25]=4
part_age_grp_4[survey.participants$part_age>=25&survey.participants$part_age<30]=5
part_age_grp_4[survey.participants$part_age>=30&survey.participants$part_age<35]=6
part_age_grp_4[survey.participants$part_age>=35&survey.participants$part_age<40]=7
part_age_grp_4[survey.participants$part_age>=40&survey.participants$part_age<45]=8
part_age_grp_4[survey.participants$part_age>=45&survey.participants$part_age<50]=9
part_age_grp_4[survey.participants$part_age>=50&survey.participants$part_age<55]=10
part_age_grp_4[survey.participants$part_age>=55&survey.participants$part_age<60]=11
part_age_grp_4[survey.participants$part_age>=60&survey.participants$part_age<65]=12
part_age_grp_4[survey.participants$part_age>=65&survey.participants$part_age<70]=13
part_age_grp_4[survey.participants$part_age>=70&survey.participants$part_age<75]=14
part_age_grp_4[survey.participants$part_age>=75]=15


part_region_2 = rep(999,length(survey.participants$part_region))
for (i in 1:length(survey.participants$part_region)){
  if (survey.participants$part_region[i]==1|survey.participants$part_region[i]==4|survey.participants$part_region[i]==8|survey.participants$part_region[i]==9) {part_region_2[i]=1}
  else if (survey.participants$part_region[i]==10|survey.participants$part_region[i]==6|survey.participants$part_region[i]==11|survey.participants$part_region[i]==12) {part_region_2[i]=2}
  else if (survey.participants$part_region[i]==2|survey.participants$part_region[i]==3|survey.participants$part_region[i]==7|survey.participants$part_region[i]==15|survey.participants$part_region[i]==16) {part_region_2[i]=3}
  else if (survey.participants$part_region[i]==5|survey.participants$part_region[i]==13|survey.participants$part_region[i]==14|survey.participants$part_region[i]==17) {part_region_2[i]=4}
  #else if (survey.participants$part_region[i]==17) {part_region_2[i]=6}
}

table(part_region_2)

survey.participants = data.frame(survey.participants,part_age_grp,part_region_2,part_age_grp_2,part_age_grp_3,part_age_grp_4)

hh_size_2 = survey.participants$hh_size
hh_size_2[hh_size_2>4] = 'GE5'
survey.participants = data.frame(survey.participants,hh_size_2)
head(survey.participants)

X1 = table(survey.participants$part_age_grp_2,survey.participants$hh_size_2)
X1

X1 = melt(X1/sum(X1))
colnames(X1)=c('part_age_grp_2','hh_size_2','sample')

X2 = table(survey.participants$part_age_grp_4,survey.participants$hh_size_2)
X2

X2 = melt(X2/sum(X2))
colnames(X2)=c('part_age_grp_4','hh_size_2','sample_2')


survey.participants = merge(x = survey.participants, y = X1, by.x = c("part_age_grp_2",'hh_size_2'), all.x = TRUE,no.dups = TRUE)
survey.participants = merge(x = survey.participants, y = X2, by.x = c("part_age_grp_4",'hh_size_2'), by.y = c("part_age_grp_4",'hh_size_2'), all.x = TRUE,no.dups = TRUE)


X3 = read.csv("data\\age_hhsize.csv")
X3
colnames(X3) = c('part_age_grp_2','hh_size_2','census')
X3[,3] = X3[,3]/sum(X3[,3])

X4 = read.csv("data\\age_hhsize_2.csv")
X4
colnames(X4) = c('part_age_grp_4','hh_size_2','census_2')
X4[,3] = X4[,3]/sum(X4[,3])

survey.participants = merge(x = survey.participants, y = X3, by.x =c("part_age_grp_2","hh_size_2"),by.y =c("part_age_grp_2","hh_size_2") , all.x = TRUE, no.dups = TRUE)
head(survey.participants)
survey.participants = merge(x = survey.participants, y = X4, by.x =c("part_age_grp_4","hh_size_2"),by.y =c("part_age_grp_4","hh_size_2") , all.x = TRUE, no.dups = TRUE)
head(survey.participants)
weight = survey.participants$census/survey.participants$sample
weight2 = survey.participants$census_2/survey.participants$sample_2

survey.participants =  data.frame(survey.participants,weight,weight2)
head(survey.participants)

survey.participants[is.na(survey.participants$weight),length(survey.participants)]=0
head(survey.participants)
write.csv(survey.participants,"data\\epi\\survey_participants.csv")


# survey.participants+survey.contact --------------------------------------

head(survey.contacts.1)
head(survey.contacts.2)
head(survey.contacts.3)

contact_type = rep(1,length(survey.contacts.1$part_id))
duration = rep(-999,length(survey.contacts.1$part_id))
survey.contacts.1 = data.frame(survey.contacts.1, contact_type, duration) 
contact_type = rep(2,length(survey.contacts.2$part_id))
survey.contacts.2 = data.frame(survey.contacts.2, contact_type) 
contact_type = rep(3,length(survey.contacts.3$part_id))
survey.contacts.3 = data.frame(survey.contacts.3, contact_type) 

selected_columns = c('cont_id', 'part_id', 'cnt_age', 'cnt_age_est_min', 'cnt_age_est_max', 'phys_contact', 'frequency_multi', 'duration', 'duration_multi', 'cnt_age_grp','cnt_work','cnt_home','cnt_school','cnt_transport','cnt_leisure','cnt_hospital','cnt_otherplace','contact_type')

survey.contacts = rbind(select(survey.contacts.1, selected_columns),select(survey.contacts.2, selected_columns),select(survey.contacts.3, selected_columns))
head(survey.contacts)
nrow(survey.contacts)

cnt_age_grp_2 = rep(999,length(survey.contacts$cnt_age))
cnt_age_grp_2[survey.contacts$cnt_age<5]=0
cnt_age_grp_2[survey.contacts$cnt_age>=5&survey.contacts$cnt_age<10]=1
cnt_age_grp_2[survey.contacts$cnt_age>=10&survey.contacts$cnt_age<15]=2
cnt_age_grp_2[survey.contacts$cnt_age>=15&survey.contacts$cnt_age<20]=3
cnt_age_grp_2[survey.contacts$cnt_age>=20&survey.contacts$cnt_age<25]=4
cnt_age_grp_2[survey.contacts$cnt_age>=25&survey.contacts$cnt_age<30]=5
cnt_age_grp_2[survey.contacts$cnt_age>=30&survey.contacts$cnt_age<35]=6
cnt_age_grp_2[survey.contacts$cnt_age>=35&survey.contacts$cnt_age<40]=7
cnt_age_grp_2[survey.contacts$cnt_age>=40&survey.contacts$cnt_age<45]=8
cnt_age_grp_2[survey.contacts$cnt_age>=45&survey.contacts$cnt_age<50]=9
cnt_age_grp_2[survey.contacts$cnt_age>=50&survey.contacts$cnt_age<55]=10
cnt_age_grp_2[survey.contacts$cnt_age>=55&survey.contacts$cnt_age<60]=11
cnt_age_grp_2[survey.contacts$cnt_age>=60&survey.contacts$cnt_age<65]=12
cnt_age_grp_2[survey.contacts$cnt_age>=65&survey.contacts$cnt_age<70]=13
cnt_age_grp_2[survey.contacts$cnt_age>=70&survey.contacts$cnt_age<75]=14
cnt_age_grp_2[survey.contacts$cnt_age>=75]=15


survey.contacts = cbind(survey.contacts,cnt_age_grp_2)
head(survey.contacts)


head(survey.participants)
nrow(survey.participants)

survey.merge = merge(x = survey.participants, y = survey.contacts, by  = 'part_id',sort = TRUE, all.x = TRUE, no.dups = TRUE )

head(survey.merge)
nrow(survey.merge)
survey.merge[is.na(survey.merge$duration_multi),c("part_id","part_age","weight")]

survey.merge[survey.merge$part_id==279|survey.merge$part_id==332,]

#data 정리 -------------------------------------------------------------------------
##The number of observed contact ( with duration level)
#duration(less than 5 min, 5???15 min,15 min to 1 h, 1???4 h, 4-8 h, or 8 h or more)
# average values of left and right end point for each range.
survey.merge[survey.merge$part_id==279,colnames(survey.merge)=="duration"] = 0
survey.merge[survey.merge$part_id==332,colnames(survey.merge)=="duration"] = 0


# survey.merge = survey.merge[!is.na(survey.merge$duration_multi),]
survey.merge[is.na(survey.merge$weight),]


summary(survey.merge$duration_multi)

table(survey.merge$duration_multi)
survey.merge[survey.merge$contact_type!=1, colnames(survey.merge)=="duration_multi"]

head(survey.merge)

table(survey.contacts.1$duration)
table(survey.contacts.1$duration_multi)
table(survey.contacts.2$duration)
table(survey.contacts.3$duration)


cont = rep(0,length(survey.merge$duration_multi))

for (i in 1:length(survey.merge$duration_multi)){
  if (survey.merge$duration[i] < 0){
    if (survey.merge$duration_multi[i]==1){
      cont[i]=5/60
    }
    else if (survey.merge$duration_multi[i]==2){
      cont[i]=10/60
    }
    else if (survey.merge$duration_multi[i]==3){
      cont[i]=75/120
    }
    else if (survey.merge$duration_multi[i]==4){
      cont[i]=2.5
    }
    else if (survey.merge$duration_multi[i]==5){
      cont[i]=6
    }
    else if (survey.merge$duration_multi[i]==6){
      cont[i]=8
    }
  }
  else {
    cont[i]=survey.merge$duration[i]/60
  }
}
summary(cont)

#nrow(survey.merge)
survey.merge = data.frame(survey.merge,cont)


# duration에 duration_multi 로 환산되는 시간의 median 값 넣어주기 ----------------------------------------------------------

table(survey.merge$duration_multi,survey.merge$contact_type)


summary(survey.merge$duration_multi)
survey.merge[survey.merge$part_id==279,colnames(survey.merge)=="duration_multi"] = 0
survey.merge[survey.merge$part_id==332,colnames(survey.merge)=="duration_multi"] = 0

for (i in 1:length(survey.merge$duration_multi)){
  if (survey.merge$duration_multi[i]==1){
    survey.merge$duration[i]=5/60
  }
  else if (survey.merge$duration_multi[i]==2){
    survey.merge$duration[i]=10/60
  }
  else if (survey.merge$duration_multi[i]==3){
    survey.merge$duration[i]=75/120
  }
  else if (survey.merge$duration_multi[i]==4){
    survey.merge$duration[i]=2.5
  }
  else if (survey.merge$duration_multi[i]==5){
    survey.merge$duration[i]=6
  }
  else if (survey.merge$duration_multi[i]==6){
    survey.merge$duration[i]=8
  }
}

table(survey.merge$duration,survey.merge$contact_type)




write.csv(survey.merge,"data\\epi\\survey_merge.csv")
head(survey.merge)
tail(survey.merge)

##6. build contact age (since there are bunch of NA values for cnt_age_exact) 
#나이 모르면 추정 나이구간 median
#summary(survey.merge$part_age)
survey.merge[is.na(survey.merge$part_id),]

survey.participants[is.na(survey.participants$part_id),]
survey.contacts[is.na(survey.contacts$part_id),]
survey.contacts[survey.contacts$cnt_age>=95,]
survey.contacts[survey.contacts$part_id>=95,]

tail(table(survey.contacts$part_id))
table(survey.participants$part_id)

survey.merge[survey.merge$cnt_age>=95,]




