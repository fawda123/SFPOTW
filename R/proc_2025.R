library(tidyverse)
library(readxl)
library(here)
dat25<-read_xlsx(here('data-raw','GAR_output_data_blended (Discharge Data only, for SFEI).xlsx'))%>%
  mutate(date = as_date(paste0(Year,"-",Month,"-","-01")),
         Unit = ifelse(Unit=="MGD","mg/d","kg/d"),
         Parameter = case_when(Parameter == "NOx"~"NOX",
                               Parameter == "Ammonia"~"NH4",
                               Parameter == "TIN"~"DIN",
                               TRUE~Parameter),
         param = paste0(Parameter," (",Unit,")"))%>%
  select(-c(index,Location,Parameter,Unit))%>%
  set_names("year","month","POTW","mag","sub_name","date","param")

save(dat25,file=here('data','loads25.rData'))
