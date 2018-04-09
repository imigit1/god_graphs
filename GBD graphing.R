## Creating a new sub region 

library("dplyr")
gbd <- IHME.GBD_2016_DATA.068f70be.1
head(gbd)
dim(gbd)

summary(gbd)
as_tibble(gbd)

##Createing subset tables 
y<- split(gbd, gbd$location_name)

## extracting each country from a list 
France <- y[["France"]]
as.data.frame(France)
as_tibble(France)

## splitting to a new dta per country 
install.packages("tidyverse")
library(tidyverse)
gbd %>%  split(.$location_name) %>%
  map2(.x = ., .y = paste0(names(.),".csv"), ~write.csv(.x, .y, row.names = F))

##Graphing incidence of cancer in france)
library(dplyr)
head(France)
colnames(France)
## creating a subset of just incidence data for france POC and ordering 
prev_f <- filter(France,measure_name=="Prevalence", metric_name=="Percent", sex_name=="Both") %>% arrange(year)
## creating a tibble 
as.tibble(prev_f)
dim(prev_f)
## Change from scientific notation 
options(scipen = 999)
##using ggplot to create a line graph 
library(ggplot2)
f_prev_plot <- ggplot(data=prev_f, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Prevalence of blood cancers in France",x="Year", y = "Prevalence")
 
##Data nanagment 
Incidence_UK <- filter(gbd,location_name=="United Kingdom",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_SWI<- filter(gbd,location_name=="Sweden",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both" , cause_name !="Leukemia") %>% arrange(year)
Incidence_SPN <- filter(gbd,location_name=="Spain",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Slov <- filter(gbd,location_name=="Slovenia",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Slk <- filter(gbd,location_name=="Slovakia",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Srb <- filter(gbd,location_name=="Serbia",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Rom <- filter(gbd,location_name=="Romania",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Por <- filter(gbd,location_name=="Portugal",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Pol <- filter(gbd,location_name=="Poland",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Nor <- filter(gbd,location_name=="Norway",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Neth <- filter(gbd,location_name=="Netherlands",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Mal <- filter(gbd,location_name=="Malta",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_lux <- filter(gbd,location_name=="Luxembourg",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_lith <- filter(gbd,location_name=="Lithuania",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_lat <- filter(gbd,location_name=="Latvia",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_ltl <- filter(gbd,location_name=="Italy",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Irl <- filter(gbd,location_name=="Ireland",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Hun <- filter(gbd,location_name=="Hungary",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Grc <- filter(gbd,location_name=="Greece",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Ger <- filter(gbd,location_name=="Germany",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Fra <- filter(gbd,location_name=="France",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Fin <- filter(gbd,location_name=="Finland",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Est <- filter(gbd,location_name=="Estonia",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Dmrk <- filter(gbd,location_name=="Denmark",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Czr <- filter(gbd,location_name=="Czech Republic",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Cyp <- filter(gbd,location_name=="Cyprus",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Crot <- filter(gbd,location_name=="Croatia",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Bul <- filter(gbd,location_name=="Bulgaria",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Bel <- filter(gbd,location_name=="Belgium",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)
Incidence_Ast <- filter(gbd,location_name=="Austria",measure_name=="Incidence", metric_name=="Rate", sex_name=="Both", cause_name !="Leukemia") %>% arrange(year)


## Changing the names of key vars to make the legend easier. 

ast_inc <- Incidence_Ast %>%
  mutate(cause_name = recode(cause_name, "Non-Hodgkin lymphoma" = "NHL",
                             "Chronic lymphoid leukemia" = "CLL",
                             
                             "Chronic myeloid leukemia" = "CML"))
dmk_inc<- Incidence_Dmrk %>%
  mutate(cause_name = recode(cause_name, "Non-Hodgkin lymphoma" = "NHL",
                             "Chronic lymphoid leukemia" = "CLL",
                             
                             "Chronic myeloid leukemia" = "CML"))
mal_inc<- Incidence_Mal %>%
  mutate(cause_name = recode(cause_name, "Non-Hodgkin lymphoma" = "NHL",
                             "Chronic lymphoid leukemia" = "CLL",
                            
                             "Chronic myeloid leukemia" = "CML"))


## Creating plots for each country 

library(ggplot2)
Aus_inc <- ggplot(data=ast_inc, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Austria",x="Year", y = "Incidence")+
  theme(legend.position="bottom", legend.title=element_blank())

Aus_inc

Bel_inc <- ggplot(data=Incidence_Bel, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Belgium",x="Year", y = "Incidence")+
  theme(legend.position="none")

Bul_inc <- ggplot(data=Incidence_Bul, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Bulgaria",x="Year", y = "Incidence")+
  theme(legend.position="none")

Cro_inc <- ggplot(data=Incidence_Crot, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Croatia",x="Year", y = "Incidence")+
  theme(legend.position="none")

Cyp_inc <- ggplot(data=Incidence_Cyp, aes(x=year, y=val, group=cause_name)) +
geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
geom_line(aes(color=cause_name))+
geom_point(aes(color=cause_name)) +
labs(title="Incidence of blood cancers in Cyprus",x="Year", y = "Incidence")+
  theme(legend.position="none")
                        
Czr_inc <- ggplot(data=Incidence_Czr, aes(x=year, y=val, group=cause_name)) +
geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
geom_line(aes(color=cause_name))+
geom_point(aes(color=cause_name)) +
labs(title="Incidence of blood cancers in Czech Republic",x="Year", y = "Incidence")+
  theme(legend.position="none")

Dmr_inc <- ggplot(data=dmk_inc, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Demark",x="Year", y = "Incidence")+
  theme(legend.position="bottom", legend.title=element_blank())

Est_inc <- ggplot(data=Incidence_Est, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Estonia",x="Year", y = "Incidence")+
  theme(legend.position="none")

Fin_inc <- ggplot(data=Incidence_Fin, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Finland",x="Year", y = "Incidence")+
  theme(legend.position="none")

Fra_inc <- ggplot(data=Incidence_Fra, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in France",x="Year", y = "Incidence")+
  theme(legend.position="none")

Ger_inc <- ggplot(data=Incidence_Ger, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Germany",x="Year", y = "Incidence")+
  theme(legend.position="none")

Grc_inc <- ggplot(data=Incidence_Grc, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Greece",x="Year", y = "Incidence")+
  theme(legend.position="none")

Hun_inc <- ggplot(data=Incidence_Hun, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Hungary",x="Year", y = "Incidence")+
  theme(legend.position="none")

Irl_inc <- ggplot(data=Incidence_Irl, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Ireland",x="Year", y = "Incidence")+
  theme(legend.position="none")

Itl_inc <- ggplot(data=Incidence_ltl, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Italy",x="Year", y = "Incidence")+
  theme(legend.position="none")

Lat_inc <- ggplot(data=Incidence_lat, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Latvia",x="Year", y = "Incidence")+
  theme(legend.position="none")

Lith_inc <- ggplot(data=Incidence_lith, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Lithuania",x="Year", y = "Incidence")+
  theme(legend.position="none")

Lux_inc <- ggplot(data=Incidence_lux, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Luxembourg",x="Year", y = "Incidence")+
  theme(legend.position="none")

Mal_inc <- ggplot(data= mal_inc, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Malta",x="Year", y = "Incidence")+
  theme(legend.position="bottom", legend.title=element_blank())

Neth_inc <- ggplot(data=Incidence_Neth, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Netherlands",x="Year", y = "Incidence")+
  theme(legend.position="none")

Nor_inc <- ggplot(data=Incidence_Nor, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Norway",x="Year", y = "Incidence")+
  theme(legend.position="none")

Pol_inc <- ggplot(data=Incidence_Pol, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Poland",x="Year", y = "Incidence")+
  theme(legend.position="none")

Nor_inc <- ggplot(data=Incidence_Nor, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Norway",x="Year", y = "Incidence")+
  theme(legend.position="none")

Por_inc <- ggplot(data=Incidence_Por, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Portugal",x="Year", y = "Incidence")+
  theme(legend.position="none")

Rom_inc <- ggplot(data=Incidence_Rom, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Romania",x="Year", y = "Incidence")+
  theme(legend.position="none")

Srb_inc <- ggplot(data=Incidence_Srb, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Serbia",x="Year", y = "Incidence")+
  theme(legend.position="none")

Slk_inc <- ggplot(data=Incidence_Slk, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Slovakia",x="Year", y = "Incidence")+
  theme(legend.position="none")

Slov_inc <- ggplot(data=Incidence_Slov, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Slovenia",x="Year", y = "Incidence")+
  theme(legend.position="none")

Spn_inc <- ggplot(data=Incidence_SPN, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Spain",x="Year", y = "Incidence") +
  theme(legend.position="none")

Swe_inc <- ggplot(data=Incidence_SWI, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in Sweden",x="Year", y = "Incidence")+theme(legend.position="none")

UK_inc <- ggplot(data=Incidence_UK, aes(x=year, y=val, group=cause_name)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_line(aes(color=cause_name))+
  geom_point(aes(color=cause_name)) +
  labs(title="Incidence of blood cancers in the UK",x="Year", y = "Incidence") +
  theme(legend.position="none") 



##Combining 
install.packages("ggpubr")
library(ggpubr)
par(mfrow=c(2,2))

install.packages("gridExtra")
library(gridExtra)
plot_group1 <- grid.arrange(UK_inc, Rom_inc,Swe_inc, Spn_inc, Slov_inc,
Slk_inc, Srb_inc, Por_inc,Pol_inc,Nor_inc,Neth_inc,
Mal_inc)

plot_group2 <- grid.arrange(Lux_inc, Lith_inc,Lat_inc, Itl_inc, Irl_inc,
Hun_inc, Grc_inc, Ger_inc,Fra_inc, Fin_inc, Est_inc,
Dmr_inc)

plot_group3 <- grid.arrange(Czr_inc, Cyp_inc,Cro_inc, Bul_inc, Bel_inc,
Aus_inc)
