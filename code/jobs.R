library(tidyverse)
library(tidycensus)
library(tigris)
library(openxlsx)
library(curl)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
#####
read_crosswalk <- function(state, download_dir=getwd()) {
  state <- tolower(state)
  sprintf("https://lehd.ces.census.gov/data/lodes/LODES7/%s/%s_xwalk.csv.gz", state, state) -> URL
  sprintf("defunct/huge/%s_xwalk.csv.gz",state) -> FILENAME
  curl_download(URL,FILENAME)
  as_tibble(read.csv(gzfile(FILENAME), stringsAsFactors=FALSE)) %>%
    mutate(geoid = tabblk2010, tabblk2010 = NULL) %>% distinct() %>%
    select(geoid,everything()) %>% arrange(geoid)}
read_wac <- function(state,year,download_dir=getwd()) {
  state <- tolower (state)
  yr <- str_sub(year,-2,-1)
  sprintf("https://lehd.ces.census.gov/data/lodes/LODES7/%s/wac/%s_wac_S000_JT00_%s.csv.gz", state, state, year) -> URL
  sprintf("defunct/huge/%s_%s.csv.gz",state,year) -> FILENAME
  sprintf("%s_jobs_%s",state,year) -> var1
  curl_download(URL,FILENAME)
  as_tibble(read.csv(gzfile(FILENAME), stringsAsFactors=FALSE)) %>%
    select(-(CA01:createdate)) %>% mutate(
      geoid = w_geocode, jobs = C000,
      w_geocode = NULL, C000 = NULL) %>%
    rename_at(vars(-geoid), ~ paste0(., yr))}
lodes.cols <- c("GEOID","jobs_02","jobs_03","jobs_04","jobs_05","jobs_06","jobs_07","jobs_08","jobs_09","jobs_10","jobs_11","jobs_12","jobs_13","jobs_14","jobs_15")
#####
###--------------------------------IOWA---------------###
iaxw <- read_crosswalk("ia")
ia02 <- read_wac("IA","2002")
ia03 <- read_wac("IA","2003")
ia04 <- read_wac("IA","2004")
ia05 <- read_wac("IA","2005")
ia06 <- read_wac("IA","2006")
ia07 <- read_wac("IA","2007")
ia08 <- read_wac("IA","2008")
ia09 <- read_wac("IA","2009")
ia10 <- read_wac("IA","2010")
ia11 <- read_wac("IA","2011")
ia12 <- read_wac("IA","2012")
ia13 <- read_wac("IA","2013")
ia14 <- read_wac("IA","2014")
ia15 <- read_wac("IA","2015")
###
ia_lodes <- full_join(ia02, ia03, by = 'geoid') %>%
  full_join(.,ia04, by = 'geoid') %>%
  full_join(.,ia05, by = 'geoid') %>%
  full_join(.,ia06, by = 'geoid') %>%
  full_join(.,ia07, by = 'geoid') %>%
  full_join(.,ia08, by = 'geoid') %>%
  full_join(.,ia09, by = 'geoid') %>%
  full_join(.,ia10, by = 'geoid') %>%
  full_join(.,ia11, by = 'geoid') %>%
  full_join(.,ia12, by = 'geoid') %>%
  full_join(.,ia13, by = 'geoid') %>%
  full_join(.,ia14, by = 'geoid') %>%
  full_join(.,ia15, by = 'geoid')
###
iowa <- inner_join(iaxw,ia_lodes,by = "geoid") %>%
  filter(stplc != 9999999) %>%
  select(stplc,stplcname,jobs02:jobs15) %>%
  group_by(stplc) %>%
  mutate_at(vars(jobs02:jobs15), ~replace_na(., 0)) %>%
  summarise_at(vars(jobs02:jobs15), sum, na.rm = TRUE) %>%
  distinct() %>%
  `colnames<-`(lodes.cols)
#####
###--------------------------------WISCONSIN---------------###
wixw <- read_crosswalk("wi")
wi02 <- read_wac("wi","2002")
wi03 <- read_wac("wi","2003")
wi04 <- read_wac("wi","2004")
wi05 <- read_wac("wi","2005")
wi06 <- read_wac("wi","2006")
wi07 <- read_wac("wi","2007")
wi08 <- read_wac("wi","2008")
wi09 <- read_wac("wi","2009")
wi10 <- read_wac("wi","2010")
wi11 <- read_wac("wi","2011")
wi12 <- read_wac("wi","2012")
wi13 <- read_wac("wi","2013")
wi14 <- read_wac("wi","2014")
wi15 <- read_wac("wi","2015")
###
wi_lodes <- full_join(wi02, wi03, by = 'geoid') %>%
  full_join(.,wi04, by = 'geoid') %>%
  full_join(.,wi05, by = 'geoid') %>%
  full_join(.,wi06, by = 'geoid') %>%
  full_join(.,wi07, by = 'geoid') %>%
  full_join(.,wi08, by = 'geoid') %>%
  full_join(.,wi09, by = 'geoid') %>%
  full_join(.,wi10, by = 'geoid') %>%
  full_join(.,wi11, by = 'geoid') %>%
  full_join(.,wi12, by = 'geoid') %>%
  full_join(.,wi13, by = 'geoid') %>%
  full_join(.,wi14, by = 'geoid') %>%
  full_join(.,wi15, by = 'geoid')
###
wisconsin <- inner_join(wixw,wi_lodes,by = "geoid") %>%
  filter(stplc != 9999999) %>%
  select(stplc,stplcname,jobs02:jobs15) %>%
  group_by(stplc) %>%
  mutate_at(vars(jobs02:jobs15), ~replace_na(., 0)) %>%
  summarise_at(vars(jobs02:jobs15), sum, na.rm = TRUE) %>%
  distinct() %>%
  `colnames<-`(lodes.cols)
#####
###--------------------------------NORTH CAROLINA---------------###
ncxw <- read_crosswalk("nc")
nc02 <- read_wac("nc","2002")
nc03 <- read_wac("nc","2003")
nc04 <- read_wac("nc","2004")
nc05 <- read_wac("nc","2005")
nc06 <- read_wac("nc","2006")
nc07 <- read_wac("nc","2007")
nc08 <- read_wac("nc","2008")
nc09 <- read_wac("nc","2009")
nc10 <- read_wac("nc","2010")
nc11 <- read_wac("nc","2011")
nc12 <- read_wac("nc","2012")
nc13 <- read_wac("nc","2013")
nc14 <- read_wac("nc","2014")
nc15 <- read_wac("nc","2015")
###
nc_lodes <- full_join(nc02, nc03, by = 'geoid') %>%
  full_join(.,nc04, by = 'geoid') %>%
  full_join(.,nc05, by = 'geoid') %>%
  full_join(.,nc06, by = 'geoid') %>%
  full_join(.,nc07, by = 'geoid') %>%
  full_join(.,nc08, by = 'geoid') %>%
  full_join(.,nc09, by = 'geoid') %>%
  full_join(.,nc10, by = 'geoid') %>%
  full_join(.,nc11, by = 'geoid') %>%
  full_join(.,nc12, by = 'geoid') %>%
  full_join(.,nc13, by = 'geoid') %>%
  full_join(.,nc14, by = 'geoid') %>%
  full_join(.,nc15, by = 'geoid')
###
northcarolina <- inner_join(ncxw,nc_lodes,by = "geoid") %>%
  filter(stplc != 9999999) %>%
  select(stplc,stplcname,jobs02:jobs15) %>%
  group_by(stplc) %>%
  mutate_at(vars(jobs02:jobs15), ~replace_na(., 0)) %>%
  summarise_at(vars(jobs02:jobs15), sum, na.rm = TRUE) %>%
  distinct() %>%
  `colnames<-`(lodes.cols)
#####
###--------------------------------TEXAS---------------###
txxw <- read_crosswalk("TX")
tx02 <- read_wac("TX","2002")
tx03 <- read_wac("TX","2003")
tx04 <- read_wac("TX","2004")
tx05 <- read_wac("TX","2005")
tx06 <- read_wac("TX","2006")
tx07 <- read_wac("TX","2007")
tx08 <- read_wac("TX","2008")
tx09 <- read_wac("TX","2009")
tx10 <- read_wac("TX","2010")
tx11 <- read_wac("TX","2011")
tx12 <- read_wac("TX","2012")
tx13 <- read_wac("TX","2013")
tx14 <- read_wac("TX","2014")
tx15 <- read_wac("TX","2015")
###
tx_lodes <- full_join(tx02, tx03, by = 'geoid') %>%
  full_join(.,tx04, by = 'geoid') %>%
  full_join(.,tx05, by = 'geoid') %>%
  full_join(.,tx06, by = 'geoid') %>%
  full_join(.,tx07, by = 'geoid') %>%
  full_join(.,tx08, by = 'geoid') %>%
  full_join(.,tx09, by = 'geoid') %>%
  full_join(.,tx10, by = 'geoid') %>%
  full_join(.,tx11, by = 'geoid') %>%
  full_join(.,tx12, by = 'geoid') %>%
  full_join(.,tx13, by = 'geoid') %>%
  full_join(.,tx14, by = 'geoid') %>%
  full_join(.,tx15, by = 'geoid')
###
texas <- inner_join(txxw,tx_lodes,by = "geoid") %>%
  filter(stplc != 9999999) %>%
  select(stplc,stplcname,jobs02:jobs15) %>%
  group_by(stplc) %>%
  mutate_at(vars(jobs02:jobs15), ~replace_na(., 0)) %>%
  summarise_at(vars(jobs02:jobs15), sum, na.rm = TRUE) %>%
  distinct() %>%
  `colnames<-`(lodes.cols)
#####
###--------------------------------MAINE---------------###
mexw <- read_crosswalk("me")
me02 <- read_wac("me","2002")
me03 <- read_wac("me","2003")
me04 <- read_wac("me","2004")
me05 <- read_wac("me","2005")
me06 <- read_wac("me","2006")
me07 <- read_wac("me","2007")
me08 <- read_wac("me","2008")
me09 <- read_wac("me","2009")
me10 <- read_wac("me","2010")
me11 <- read_wac("me","2011")
me12 <- read_wac("me","2012")
me13 <- read_wac("me","2013")
me14 <- read_wac("me","2014")
me15 <- read_wac("me","2015")
###
me_lodes <- full_join(me02, me03, by = 'geoid') %>%
  full_join(.,me04, by = 'geoid') %>%
  full_join(.,me05, by = 'geoid') %>%
  full_join(.,me06, by = 'geoid') %>%
  full_join(.,me07, by = 'geoid') %>%
  full_join(.,me08, by = 'geoid') %>%
  full_join(.,me09, by = 'geoid') %>%
  full_join(.,me10, by = 'geoid') %>%
  full_join(.,me11, by = 'geoid') %>%
  full_join(.,me12, by = 'geoid') %>%
  full_join(.,me13, by = 'geoid') %>%
  full_join(.,me14, by = 'geoid') %>%
  full_join(.,me15, by = 'geoid')
###
maine <- inner_join(mexw,me_lodes,by = "geoid") %>%
  filter(stplc != 9999999) %>%
  select(stplc,stplcname,jobs02:jobs15) %>%
  group_by(stplc) %>%
  mutate_at(vars(jobs02:jobs15), ~replace_na(., 0)) %>%
  summarise_at(vars(jobs02:jobs15), sum, na.rm = TRUE) %>%
  distinct() %>%
  `colnames<-`(lodes.cols)
#####
###--------------------------------MICHIGAN---------------###
mixw <- read_crosswalk("mi")
mi02 <- read_wac("mi","2002")
mi03 <- read_wac("mi","2003")
mi04 <- read_wac("mi","2004")
mi05 <- read_wac("mi","2005")
mi06 <- read_wac("mi","2006")
mi07 <- read_wac("mi","2007")
mi08 <- read_wac("mi","2008")
mi09 <- read_wac("mi","2009")
mi10 <- read_wac("mi","2010")
mi11 <- read_wac("mi","2011")
mi12 <- read_wac("mi","2012")
mi13 <- read_wac("mi","2013")
mi14 <- read_wac("mi","2014")
mi15 <- read_wac("mi","2015")
###
mi_lodes <- full_join(mi02, mi03, by = 'geoid') %>%
  full_join(.,mi04, by = 'geoid') %>%
  full_join(.,mi05, by = 'geoid') %>%
  full_join(.,mi06, by = 'geoid') %>%
  full_join(.,mi07, by = 'geoid') %>%
  full_join(.,mi08, by = 'geoid') %>%
  full_join(.,mi09, by = 'geoid') %>%
  full_join(.,mi10, by = 'geoid') %>%
  full_join(.,mi11, by = 'geoid') %>%
  full_join(.,mi12, by = 'geoid') %>%
  full_join(.,mi13, by = 'geoid') %>%
  full_join(.,mi14, by = 'geoid') %>%
  full_join(.,mi15, by = 'geoid')
###
michigan <- inner_join(mixw,mi_lodes,by = "geoid") %>%
  filter(stplc != 9999999) %>%
  select(stplc,stplcname,jobs02:jobs15) %>%
  group_by(stplc) %>%
  mutate_at(vars(jobs02:jobs15), ~replace_na(., 0)) %>%
  summarise_at(vars(jobs02:jobs15), sum, na.rm = TRUE) %>%
  distinct() %>%
  `colnames<-`(lodes.cols)
#####
###--------------------------------MINNESOTA---------------###
mnxw <- read_crosswalk("mn")
mn02 <- read_wac("mn","2002")
mn03 <- read_wac("mn","2003")
mn04 <- read_wac("mn","2004")
mn05 <- read_wac("mn","2005")
mn06 <- read_wac("mn","2006")
mn07 <- read_wac("mn","2007")
mn08 <- read_wac("mn","2008")
mn09 <- read_wac("mn","2009")
mn10 <- read_wac("mn","2010")
mn11 <- read_wac("mn","2011")
mn12 <- read_wac("mn","2012")
mn13 <- read_wac("mn","2013")
mn14 <- read_wac("mn","2014")
mn15 <- read_wac("mn","2015")
###
mn_lodes <- full_join(mn02, mn03, by = 'geoid') %>%
  full_join(.,mn04, by = 'geoid') %>%
  full_join(.,mn05, by = 'geoid') %>%
  full_join(.,mn06, by = 'geoid') %>%
  full_join(.,mn07, by = 'geoid') %>%
  full_join(.,mn08, by = 'geoid') %>%
  full_join(.,mn09, by = 'geoid') %>%
  full_join(.,mn10, by = 'geoid') %>%
  full_join(.,mn11, by = 'geoid') %>%
  full_join(.,mn12, by = 'geoid') %>%
  full_join(.,mn13, by = 'geoid') %>%
  full_join(.,mn14, by = 'geoid') %>%
  full_join(.,mn15, by = 'geoid')
###
minnesota <- inner_join(mnxw,mn_lodes,by = "geoid") %>%
  filter(stplc != 9999999) %>%
  select(stplc,stplcname,jobs02:jobs15) %>%
  group_by(stplc) %>%
  mutate_at(vars(jobs02:jobs15), ~replace_na(., 0)) %>%
  summarise_at(vars(jobs02:jobs15), sum, na.rm = TRUE) %>%
  distinct() %>%
  `colnames<-`(lodes.cols)
#####
###--------------------------------OHIO---------------###
ohxw <- read_crosswalk("oh")
oh02 <- read_wac("oh","2002")
oh03 <- read_wac("oh","2003")
oh04 <- read_wac("oh","2004")
oh05 <- read_wac("oh","2005")
oh06 <- read_wac("oh","2006")
oh07 <- read_wac("oh","2007")
oh08 <- read_wac("oh","2008")
oh09 <- read_wac("oh","2009")
oh10 <- read_wac("oh","2010")
oh11 <- read_wac("oh","2011")
oh12 <- read_wac("oh","2012")
oh13 <- read_wac("oh","2013")
oh14 <- read_wac("oh","2014")
oh15 <- read_wac("oh","2015")
###
oh_lodes <- full_join(oh02, oh03, by = 'geoid') %>%
  full_join(.,oh04, by = 'geoid') %>%
  full_join(.,oh05, by = 'geoid') %>%
  full_join(.,oh06, by = 'geoid') %>%
  full_join(.,oh07, by = 'geoid') %>%
  full_join(.,oh08, by = 'geoid') %>%
  full_join(.,oh09, by = 'geoid') %>%
  full_join(.,oh10, by = 'geoid') %>%
  full_join(.,oh11, by = 'geoid') %>%
  full_join(.,oh12, by = 'geoid') %>%
  full_join(.,oh13, by = 'geoid') %>%
  full_join(.,oh14, by = 'geoid') %>%
  full_join(.,oh15, by = 'geoid')
###
ohio <- inner_join(ohxw,oh_lodes,by = "geoid") %>%
  filter(stplc != 9999999) %>%
  select(stplc,stplcname,jobs02:jobs15) %>%
  group_by(stplc) %>%
  mutate_at(vars(jobs02:jobs15), ~replace_na(., 0)) %>%
  summarise_at(vars(jobs02:jobs15), sum, na.rm = TRUE) %>%
  distinct() %>%
  `colnames<-`(lodes.cols)
#####
###--------------------------------VIRGINIA---------------###
vaxw <- read_crosswalk("va")
va02 <- read_wac("va","2002")
va03 <- read_wac("va","2003")
va04 <- read_wac("va","2004")
va05 <- read_wac("va","2005")
va06 <- read_wac("va","2006")
va07 <- read_wac("va","2007")
va08 <- read_wac("va","2008")
va09 <- read_wac("va","2009")
va10 <- read_wac("va","2010")
va11 <- read_wac("va","2011")
va12 <- read_wac("va","2012")
va13 <- read_wac("va","2013")
va14 <- read_wac("va","2014")
va15 <- read_wac("va","2015")
###
va_lodes <- full_join(va02, va03, by = 'geoid') %>%
  full_join(.,va04, by = 'geoid') %>%
  full_join(.,va05, by = 'geoid') %>%
  full_join(.,va06, by = 'geoid') %>%
  full_join(.,va07, by = 'geoid') %>%
  full_join(.,va08, by = 'geoid') %>%
  full_join(.,va09, by = 'geoid') %>%
  full_join(.,va10, by = 'geoid') %>%
  full_join(.,va11, by = 'geoid') %>%
  full_join(.,va12, by = 'geoid') %>%
  full_join(.,va13, by = 'geoid') %>%
  full_join(.,va14, by = 'geoid') %>%
  full_join(.,va15, by = 'geoid')
###
virginia <- inner_join(vaxw,va_lodes,by = "geoid") %>%
  filter(stplc != 9999999) %>%
  select(stplc,stplcname,jobs02:jobs15) %>%
  group_by(stplc) %>%
  mutate_at(vars(jobs02:jobs15), ~replace_na(., 0)) %>%
  summarise_at(vars(jobs02:jobs15), sum, na.rm = TRUE) %>%
  distinct() %>%
  `colnames<-`(lodes.cols)
#####
###--------------------------------COLORADO---------------###
coxw <- read_crosswalk("co")
co02 <- read_wac("co","2002")
co03 <- read_wac("co","2003")
co04 <- read_wac("co","2004")
co05 <- read_wac("co","2005")
co06 <- read_wac("co","2006")
co07 <- read_wac("co","2007")
co08 <- read_wac("co","2008")
co09 <- read_wac("co","2009")
co10 <- read_wac("co","2010")
co11 <- read_wac("co","2011")
co12 <- read_wac("co","2012")
co13 <- read_wac("co","2013")
co14 <- read_wac("co","2014")
co15 <- read_wac("co","2015")
###
co_lodes <- full_join(co02, co03, by = 'geoid') %>%
  full_join(.,co04, by = 'geoid') %>%
  full_join(.,co05, by = 'geoid') %>%
  full_join(.,co06, by = 'geoid') %>%
  full_join(.,co07, by = 'geoid') %>%
  full_join(.,co08, by = 'geoid') %>%
  full_join(.,co09, by = 'geoid') %>%
  full_join(.,co10, by = 'geoid') %>%
  full_join(.,co11, by = 'geoid') %>%
  full_join(.,co12, by = 'geoid') %>%
  full_join(.,co13, by = 'geoid') %>%
  full_join(.,co14, by = 'geoid') %>%
  full_join(.,co15, by = 'geoid')
###
colorado <- inner_join(coxw,co_lodes,by = "geoid") %>%
  filter(stplc != 9999999) %>%
  select(stplc,stplcname,jobs02:jobs15) %>%
  group_by(stplc) %>%
  mutate_at(vars(jobs02:jobs15), ~replace_na(., 0)) %>%
  summarise_at(vars(jobs02:jobs15), sum, na.rm = TRUE) %>%
  distinct() %>%
  `colnames<-`(lodes.cols)
#####
msp.jobs <- rbind(colorado,iowa,maine,michigan,minnesota,northcarolina,ohio,texas,virginia,wisconsin) %>%
  arrange(GEOID) %>%
  write_csv("data/csv/jobs.csv")
