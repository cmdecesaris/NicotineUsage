library(MASS)
library(tidyverse)

setwd("C:/Users/chris/OneDrive/Desktop/STA 223/Project2/NicotineUsage/")

drug = read.csv("data/drug_consumption.csv",header=F, col.names = c("ID","Age","Gender","Education","Country","Ethnicity","Nscore","Escore","Oscore","Ascore","Cscore","Impulsive","SS","Alcohol","Amphet","Amyl","Benzos","Caff","Cannabis","Choc","Coke","Crack","Ecstasy","Heroin","Ketamine","Legalh","LSD","Meth","Mushrooms","Nicotine","Semer","VSA"))


#processing the numeric values into their represented data

#Gender
drug = drug%>%
  mutate(Gender = case_when(Gender < 0 ~"Male",
                            Gender > 0 ~ "Female"))
#Age
drug = drug%>%
  mutate(Age = case_when(Age ==  -0.95197 ~"18-24",
                         Age == -0.07854 ~ "25-34",
                         Age == 0.49788  ~ "35-44",
                         Age == 1.09449  ~ "45-54",
                         Age == 1.82213  ~ "55-64",
                         Age == 2.59171  ~ "65+"))

#Education
drug = drug%>%
  mutate(Education = case_when(Education %in% c(-2.43591,-1.73790,-1.43719) ~ "Some HS",
                               Education == -1.22751 ~ "HS Grad",
                               Education == -0.61113  ~ "Some College",
                               Education == -0.05921  ~ "Certificate/Trade Degree",
                               Education %in% c(1.16365,1.98437,0.45468) ~ "University"))


#Ethnicity
drug = drug%>%
  mutate(Ethnicity = case_when(Ethnicity %in% c(1.90725,0.12600,-0.22166) ~ "Mixed",
                               Ethnicity == -0.50212 ~ "Asian",
                               Ethnicity == -1.10702  ~ "Black",
                               Ethnicity == -0.31685  ~ "White",
                               Ethnicity == 0.11440  ~ "Other"))

#Country

drug = drug%>%
  mutate(Country = case_when(Country == -0.09765~ "Australia",
                             Country == 0.24923 ~ "Canada",
                             Country == -0.46841  ~ "New Zealand",
                             Country == -0.28519  ~ "Other",
                             Country == 0.21128  ~ "Ireland",
                             Country == 0.96082  ~ "UK",
                             Country == -0.57009  ~ "USA"))

nscore_key=as.data.frame(cbind(c(-3.46436,
                                 -3.15735,-2.75696,-2.52197,-2.42317,-2.34360,-2.21844,-2.05048,-1.86962,-1.69163,-1.55078,-1.43907,-1.32828,-1.19430,-1.05308,-0.92104,-0.79151,-0.67825,-0.58016,-0.46725,-0.34799,-0.24649,-0.14882,-0.05188,
                                 0.04257 ,0.13606 ,0.22393 ,0.31287 ,0.41667 ,0.52135 ,0.62967 ,0.73545,0.82562,0.91093,1.02119,1.13281,1.23461,1.37297,1.49158,1.60383,1.72012,1.83990,1.98437,2.12700,2.28554,
                                 2.46262,2.61139,2.82196,3.27393),seq(12,60,1)))


for(val in seq(1,dim(nscore_key)[1],1)){
  drug[drug$Nscore==nscore_key$V1[val],]$Nscore =  nscore_key$V2[val]}



escore_key=as.data.frame(cbind(c(-3.27393,-3.00537,-2.72827,-2.53830,-2.44904,-2.32338,-2.21069,-2.11437,-2.03972,-1.92173,-1.76250,-1.63340,-1.50796,-1.37639,-1.23177,-1.09207,-0.94779,-0.80615,-0.69509,-0.57545,-0.43999,-0.30033,-0.15487,0.00332,0.16767,0.32197,0.47617,0.63779,0.80523,0.96248,1.11406,1.28610,1.45421,1.58487,1.74091,1.93886,2.12700,2.32338,2.57309,2.85950,3.00537,3.27393),c(16, 18,19,20,21,22,23,24,25,26,27,28,29,30,31 ,
                                                                                                                                                                                                                                                                                                                                                                                                           32 ,33 ,34 ,35 ,36 ,37 ,38 ,39 , 40, 41, 42, 43, 44, 45, 46 ,47 ,
                                                                                                                                                                                                                                                                                                                                                                                                           48 ,49 ,50 ,51 , 52, 53, 54, 55, 56, 58, 59)))


for(val in seq(1,dim(escore_key)[1],1)){
  drug[drug$Escore==escore_key$V1[val],]$Escore =  escore_key$V2[val]}


oscore_key=as.data.frame(cbind(c(-3.27393  ,-2.85950  ,-2.63199  , -2.39883,-2.21069  ,-2.09015  , -1.97495, -1.82919, -1.68062, -1.55521, -1.42424, -1.27553,-1.11902,-0.97631,-0.84732,-0.71727,-0.58331,-0.45174,-0.31776,-0.17779,-0.01928,0.14143 ,0.29338 ,0.44585,0.58331,0.72330,0.88309,1.06238,1.24033,1.43533,1.65653,1.88511,2.15324,2.44904,2.90161),c(24,26,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60)))






for(val in seq(1,dim(oscore_key)[1],1)){
  if(is_empty(drug[drug$Oscore==oscore_key$V1[val],])==F){
    drug[drug$Oscore==oscore_key$V1[val],]$Oscore =  oscore_key$V2[val]}}


ascore_key=as.data.frame(cbind(c(-3.46436,-3.15735,-3.00537,-2.90161,-2.78793,-2.70172,-2.53830,-2.35413,-2.21844,-2.07848,-1.92595,-1.77200,-1.62090,-1.47955,-1.34289,-1.21213,-1.07533,-0.91699,-0.76096,-0.60633,-0.45321,-0.30172,-0.15487,-0.01729,0.13136,0.28783,0.43852,0.59042,0.76096,0.94156,1.11406,1.2861,1.45039,1.61108,1.81866,2.03972,2.23427,2.46262,2.75696,3.15735,3.46436),c(12,16,18,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60)))


for(val in seq(1,dim(ascore_key)[1],1)){
  if(is_empty(drug[drug$Ascore==ascore_key$V1[val],])==F){
    drug[drug$Ascore==ascore_key$V1[val],]$Ascore =  ascore_key$V2[val]}}




cscore_key=as.data.frame(cbind(c(-3.46436,-3.15735,-2.90161,-2.72827,-2.57309,-2.42317,-2.30408,-2.18109,-2.04506,-1.92173,-1.78169,-1.64101,-1.51840,-1.38502,-1.25773,-1.13788,-1.01450,-0.89891,-0.78155,-0.65253,-0.52745,-0.40581,-0.27607,-0.14277,-0.00665, 0.12331, 0.25953, 0.41594,0.58489,0.7583,0.93949,1.13407,1.30612,1.46191,1.63088,1.81175,2.04506,2.33337,2.63199,3.00537,3.46436),c(17,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59)))

for(val in seq(1,dim(cscore_key)[1],1)){
  if(is_empty(drug[drug$Cscore==cscore_key$V1[val],])==F){
    drug[drug$Cscore==cscore_key$V1[val],]$Cscore =  cscore_key$V2[val]}}


iscore_key=as.data.frame(cbind(c(-2.55524,-1.37983,-0.71126,-0.21712,0.19268,0.52975,0.88113,1.29221,1.86203,2.90161),c(20,276,307,355,257,216,195,148,104,7)))


for(val in seq(1,dim(iscore_key)[1],1)){
  if(is_empty(drug[drug$Impulsive==iscore_key$V1[val],])==F){
    drug[drug$Impulsive==iscore_key$V1[val],]$Impulsive =  iscore_key$V2[val]}}


sscore_key=as.data.frame(cbind(c(-2.07848,-1.54858,-1.18084,-0.84637,-0.52593,-0.21575,0.07987,0.40148,0.76540 ,1.22470,1.92173),c(71,87,132,169,211,223,219,249,211,210,103)))


for(val in seq(1,dim(sscore_key)[1],1)){
  if(is_empty(drug[drug$SS==sscore_key$V1[val],])==F){
    drug[drug$SS==sscore_key$V1[val],]$SS =  sscore_key$V2[val]}}


#Drug Usage
decode_usage = function(ROW) #use sapply
{switch(ROW,
        "CL0" ="Never Used",
        "CL1"= "Used over a Decade Ago",
        "CL2" ="Used in Last Decade",
        "CL3" ="Used in Last Year",
        "CL4" ="Used in Last Month",
        "CL5" ="Used in Last Week",
        "CL6" ="Used in Last Day")}


decoded=c(0)
for(col in drug[14:32]){
  decoded = cbind(decoded,(as.data.frame(sapply(col, decode_usage))))
}

drug[14:32] = decoded[2:20]






drug=drug[c(1:13,30)]
drug$NicotineL = drug$Nicotine
drug = drug%>%
  mutate(NicotineL = case_when(NicotineL %in% c("Used in Last Month",
                                                "Used in Last Week",
                                                "Used in Last Day") ~ "Recent User",
                               NicotineL %in% c("Used in Last Year",
                                                "Used in Last Decade",
                                                "Used over a Decade Ago") ~ "Past User",
                               NicotineL == "Never Used"  ~ "Never Used"))

drug= drop_na(drug)