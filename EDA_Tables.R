source("preprocessing.R")


library(kableExtra)

raw = read.csv("data/drug_consumption.csv",header=F, col.names = c("ID","Age","Gender","Education","Country","Ethnicity","Nscore","Escore","Oscore","Ascore","Cscore","Impulsive","SS","Alcohol","Amphet","Amyl","Benzos","Caff","Cannabis","Choc","Coke","Crack","Ecstasy","Heroin","Ketamine","Legalh","LSD","Meth","Mushrooms","Nicotine","Semer","VSA"))




variables = c("ID","Age","Gender","Education","Country","Ethnicity","Nscore","Escore","Oscore","Ascore","Cscore","Impulsive","SS","Nicotine")

description = c("Observation ID",
                "Age Range Group",
                "Gender",
                "Education Level",
                "Country of Orgin",
                "Ethnicity of Person",
                "Neuroticism Score: the long-term tendency to experience negative emotions such
as nervousness, tension, anxiety and depression",
                "Extraversion Score: manifested in outgoing, warm, active, assertive, talkative,
cheerful, and in search of stimulation characteristics",
                "Openness Score: a general appreciation for art, unusual ideas, and
imaginative, creative, unconventional, and wide interests",
                "Agreeableness Score: a dimension of interpersonal relations, characterized by
altruism, trust, modesty, kindness, compassion and cooperativeness",
                "Conscientiousness Score: a tendency to be organized and dependable,
strong-willed, persistent, reliable, and efficient",
                "Impulsivity Score: the tendincy to act on impulse",
                "Sensation Seeking Score:  the tendency to pursue new and different sensations, feelings, and experiences",
                "Nicotine Usage Status")


cbind(variables,description) %>%kable(col.names =c("Variable Name","Variable Description"))%>% kable_classic(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:2,
               border_left = T, 
               border_right = T)

head(raw[c(1:13,30)],3)%>%kable%>% kable_classic(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:14,
               border_left = T, 
               border_right = T)



usage_codes = cbind(c("CL0","CL1","CL2", "CL3", "CL4", "CL5", "CL6"),
                    c("Never Used","Used over a Decade Ago","Used in Last Decade",
                      "Used in Last Year","Used in Last Month","Used in Last Week",
                      "Used in Last Day"),c("Never Used","Past User","Past User","Past User","Recent User","Recent User","Recent User"))


usage_codes %>%kable(col.names =c("Usage Code","Usage Label","Simplified Labels"))%>% kable_classic(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:3,
               border_left = T, 
               border_right = T)


head(drug,3)%>%kable%>% kable_classic(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:15,
               border_left = T, 
               border_right = T)%>%
  column_spec(c(15), bold = F, color = "black", background = "gold")


age_key = cbind(c("18-24",
                  "25-34",
                  "35-44",
                  "45-54",
                  "55-64",
                  "65+",""),c("-0.95197",
                              "-0.07854",
                              "0.49788",  
                              "1.09449",  
                              "1.82213",  
                              "2.59171","") ) 

gender_key = cbind((c("Male","Female","","","","","")),c("-0.482", "0.482","","","","",""))

Education_key = cbind(c("Some HS 
\n \n 
(Left before 16,
                        \n  Left at 16,
                        \n  Left at 17)",
                        "HS Grad",
                        "Some College",
                        "Certificate/Trade Degree",
                        "Bach",
                        ">Bach \n \n 
(Masters degree,\n 

\n Doctorate degree)",""),
                      c("-2.43591, 


-1.73790,

-1.43719","-1.22751",
                        "-0.61113",
                        "-0.05921",
                        "0.45468",
                        "1.16365,
\n 1.98437",""))

ethnicity_key = cbind(c("Mixed \n \n 
                        \n (Mixed-Black/Asian,
                        \n Mixed-White/Asian,
                        \n Mixed-White/Black)","Asian","Black","White","Other","",""),c("\n \n \n 
                                                                                                                                1.90725,\n \n
                                                                                        0.12600,\n -0.22166","-0.50212","-1.10702","-0.31685","0.11440","",""))

country_key = cbind(c("Australia","Canada","New Zealand","Other","Ireland","UK","USA"),c("-0.09765", "0.24923","-0.46841","-0.28519", "0.21128", "0.96082","-0.57009"))


catvars = do.call("cbind",list(country_key, Education_key,age_key,ethnicity_key,gender_key))


data.frame(catvars) %>%
  kable(caption = '',
        col.names = c("Country","Country Code","Education","Education Code","Age Group","Age Code","Ethnicity","Ethnicity Code","Gender","Gender Code"))%>% kable_classic_2(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:10,
               border_left = T, 
               border_right = T)



numvars1 = do.call("cbind",list(rev(nscore_key),
                                rev(rbind(escore_key,cbind(c("","","","","","",""),c("","","","","","","")))),
                                
                                
                                
                                rev(rbind(ascore_key,cbind(c("","","","","","","",""),c("","","","","","","","")))),
                                
                                rev(rbind(cscore_key,cbind(c("","","","","","","",""),c("","","","","","","","")))),
                                rev(rbind(oscore_key,cbind(c("","","","","","","","","","","","","",""),c("","","","","","","","","","","","","",""))))
                                
))


numvars2 = cbind(
  rev(sscore_key),rev(rbind(iscore_key,cbind("",""))))





data.frame(numvars1) %>%
  kable(caption = '',
        col.names = c("Neuroticism Score","Neuroticism Code","Extraversion Score","Extraversion Code","Agreeableness Score","Agreeableness Code","Conscientiousness Score","Conscientiousness Code","Openness to Experience Score","Openness to Experience Code"))%>% kable_classic_2(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:10,
               border_left = T, 
               border_right = T)



data.frame(numvars2) %>%
  kable(caption = '',
        col.names = c("Sensation Seeking Score","Sensation Seeking Code","Impulsivity Score","Impulsivity Code"))%>% kable_classic_2(full_width =F,lightable_options   =c("striped","bordered") )%>% 
  column_spec (1:4,
               border_left = T, 
               border_right = T)

