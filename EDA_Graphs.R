source("preprocessing.R")
head(drug)




#graphs for variables of interest
drug %>% ggplot(aes(x=Nicotine, y=Nscore, fill=Nicotine ))+
  geom_boxplot()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 15,
                                   hjust = 0.5,
                                   vjust = 1))

drug %>% ggplot(aes(x=Nicotine, y=SS, fill=Nicotine ))+
  geom_boxplot()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 15,
                                   hjust = 0.5,
                                   vjust = 1))

drug %>% ggplot(aes(x=Nicotine, y=Impulsive, fill=Nicotine ))+
  geom_boxplot()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 15,
                                   hjust = 0.5,
                                   vjust = 1))

drug %>% ggplot(aes(x=Nicotine, y=Escore, fill=Nicotine ))+
  geom_boxplot()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 15,
                                   hjust = 0.5,
                                   vjust = 1))

drug %>% ggplot(aes(x=Nicotine, y=Oscore, fill=Nicotine ))+
  geom_boxplot()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 15,
                                   hjust = 0.5,
                                   vjust = 1))

drug %>% ggplot(aes(x=Nicotine, y=Ascore,
                    fill=Nicotine ))+
  geom_boxplot()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 15,
                                   hjust = 0.5,
                                   vjust = 1))

drug %>% ggplot(aes(x=Nicotine, y=Cscore,
                    fill=Nicotine ))+
  geom_boxplot()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 15,
                                   hjust = 0.5,
                                   vjust = 1))




#drop missing values, very few in this data
drug= drop_na(drug)

#graph the personality scores by Nicotine status, couple of patterns present, concientious between never and recent..etc couple patterns.

ns = drug %>% ggplot(aes(x=NicotineL, y=Nscore, fill=NicotineL ))+
  geom_boxplot()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x = "Nicotine Usage Status", y = "Neuroticism Score",title="Neuroticism")

ss = drug %>% ggplot(aes(x=NicotineL, y=SS, fill=NicotineL ))+
  geom_boxplot()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x = "Nicotine Usage Status", y = "Sensation Seeking Score",title="Sensation Seeking")

is = drug %>% ggplot(aes(x=NicotineL, y=Impulsive, fill=NicotineL ))+
  geom_boxplot()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x = "Nicotine Usage Status", y = "Impulsivity Score",title="Impulsivity")

es = drug %>% ggplot(aes(x=NicotineL, y=Escore, fill=NicotineL ))+
  geom_boxplot()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x = "Nicotine Usage Status", y = "Extraversion Score", title="Extraversion")

os = drug %>% ggplot(aes(x=NicotineL, y=Oscore, fill=NicotineL ))+
  geom_boxplot()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x = "Nicotine Usage Status", y = "Openness Score", title="Openness to Experiences")

as = drug %>% ggplot(aes(x=NicotineL, y=Ascore,
                         fill=NicotineL ))+
  geom_boxplot()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x = "Nicotine Usage Status", y = "Agreeableness Score", title="Agreeableness")

cs = drug %>% ggplot(aes(x=NicotineL, y=Cscore,
                         fill=NicotineL ))+
  geom_boxplot()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x = "Nicotine Usage Status", y = "Conscientiousness Score", title= "Conscientiousness")

gridExtra::grid.arrange(ns,es,as,cs)

gridExtra::grid.arrange(os,ss,is, ncol=2)

gridExtra::grid.arrange(os,cs, ncol=1)






drug%>%group_by(NicotineL)%>%count
length(which(drug$NicotineL=="Recent User"))




#get labels, percent age for each value. Unfortunatly not used too much because there is a more efficent way in ggplot using ..prop..
generate_labels =  function(COL){
  levels = unique(COL)
  labels = c()
  for (level in levels){
    
    lvln=(count(as.data.frame(which(COL==level))))
    labels=rbind(labels, c(lvln,
                           (round(lvln/length(COL), digits=3)*100)))
  }
  labels=(t(labels))
  colnames(labels)= levels
  return(labels)
}





#mutate labels so we can grasp how many in each category in each outcome level
drug%>%ggplot(aes(x=Education,fill=Education))+
  geom_bar(stat = "count")+
  facet_wrap(~NicotineL)+
  theme(axis.text.x = element_text(angle=40))

drug=drug%>%
  mutate(NicotineL=case_when(NicotineL == "Never Used" ~ paste("Never Used,",428,"People"),
                             NicotineL == "Recent User" ~ paste("Recent User,",875,"People"),
                             NicotineL == "Past User" ~ paste("Past User,",582,"People")))



ggplot(drug, aes(x= Gender,  group=NicotineL)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..,accuracy =  0.1L),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="day", title = "Gender Breakdown") +
  facet_grid(~NicotineL) +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))






ggplot(drug, aes(x= Education,  group=NicotineL)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..,accuracy =  0.1L),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="day", title="Education Breakdown") +
  facet_grid(~NicotineL) +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "none", axis.text.x = element_text(angle=60, hjust = 1), plot.title = element_text(hjust = 0.5))


ggplot(drug, aes(x= Ethnicity,  group=NicotineL)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..,accuracy =  0.1L),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="day", title = "Ethnicity Breakdown") +
  facet_grid(~NicotineL) +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "none", axis.text.x = element_text(angle=60, hjust = 1), plot.title = element_text(hjust = 0.5))



ggplot(drug, aes(x= Age,  group=NicotineL)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..,accuracy =  0.1L),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="day", title = "Age Breakdown") +
  facet_grid(~NicotineL) +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "none", axis.text.x = element_text(angle=60, hjust = 1), plot.title = element_text(hjust = 0.5))



ggplot(drug, aes(x= Country,  group=NicotineL)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..,accuracy =  0.1L),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="day", title = "Country Breakdown") +
  facet_grid(~NicotineL) +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "none", axis.text.x = element_text(angle=60, hjust = 1), plot.title = element_text(hjust = 0.5))





ggplot(drug, aes(x= Gender,  group=NicotineL)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..,accuracy =  0.1L),
                 y= ..prop.. ), stat= "count", vjust = -.5,size=5) +
  labs(y = "Percent", fill="day", title = "Gender Breakdown") +
  facet_grid(~NicotineL) +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  theme(
    axis.text = element_text(size=19),
    strip.text.x = element_text(size = 19),
    title = element_text(size = 22),
    legend.text = element_text(size=19),
    legend.title = element_text(size=20))

#get levels break down with percents

#easier to just make the data for this
leveldat <- data.frame(
  label=c("Never Used","Recent User","Past User"),
  value=c(428,875,582)
)


leveldat <- leveldat %>% 
  arrange(desc(label)) %>%
  mutate(prop = round(value / sum(leveldat$value) *100,2)) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
leveldat%>%
  ggplot( aes(x="", y=prop, fill=label)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  geom_text(aes(y = ypos, label = paste(as.character(value),",","\n",prop,'%',sep="")), color = "black", size=7)+
  labs(fill="Nicotine Status",x="",y="", title="Nicotine Status Breakdown")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 10),
        title = element_text(size = 22),
        legend.text = element_text(size=19),
        legend.title = element_text(size=20))

# Basic piechart
leveldat%>%
  ggplot(aes(x="", y=value, fill=label)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)







ggplot(drug, aes(x= Country,  group=NicotineL)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..,accuracy =  0.1L),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="day", title = "Country Breakdown") +
  facet_grid(~NicotineL) +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "none", axis.text.x = element_text(angle=60, hjust = 1), plot.title = element_text(hjust = 0.5))


