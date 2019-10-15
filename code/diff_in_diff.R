suppressMessages({library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
library(plm)
library(did)
library(fastDummies)
library(panelr)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

df <- read_csv("data/csv/panel.csv") %>% mutate(time = str_replace_all(time, "-", "m"))

df_dummy<- dummy_cols(df, select_columns = c("cz", "time", "cal_year", "town")) %>%
  janitor::clean_names() %>% 
  write_csv("panel_dummy.csv")

test <- df_dummy %>% 
  select(sort(names(.)))

results <- lm(downtown_jobs ~ treated + 
                time_m3 + time_m2 + time_m1 + time_1 + time_2 + time_3 + time_4 + time_5 +
                ### YEAR FIXED EFFECTS
                cal_year_1997 + cal_year_1998 + cal_year_1999 + cal_year_2000 + cal_year_2001 + cal_year_2002 +
                cal_year_2003 + cal_year_2004 + cal_year_2005 + cal_year_2006 + cal_year_2007 + cal_year_2008 +
                cal_year_2009 + cal_year_2010 + cal_year_2011 + cal_year_2012 + cal_year_2013 + cal_year_2014 +
                cal_year_2015 + cal_year_2016 + #cal_year_2017 + 
                ### TOWN FIXED EFFECTS
                town_albia + town_bloomfield + town_boone + town_britt + town_brooklyn + 
                town_burlington + town_carroll + town_centerville + town_clarksville + 
                town_clear_lake + town_colfax + town_coon_rapids + town_corydon + town_eldon + 
                town_elkader + town_fayette + town_forest_city + town_fort_madison + 
                town_garner + town_grand_junction + town_greene + town_greenfield + 
                town_grinnell + town_grundy_center + town_guttenberg + town_independence + 
                town_jefferson + town_keokuk + town_keota + town_knoxville + 
                town_la_porte_city + town_lake_mills + town_lamoni + town_leon + 
                town_madrid + town_manly + town_manning + town_marengo + town_marshalltown + 
                town_mc_gregor + town_melcher_dallas + town_monona + town_monroe + 
                town_montezuma + town_mount_pleasant + town_muscatine + town_nevada + 
                town_new_london + town_new_sharon + town_newton + town_northwood + 
                town_oelwein + town_ogden + town_osceola + town_oskaloosa + town_parkersburg + 
                town_pella + town_postville + town_reinbeck + town_sigourney + town_state_center + 
                town_strawberry_point + town_sumner + town_tama + town_tipton + town_toledo + 
                town_traer + town_tripoli + town_wapello + town_waukon + town_waverly + 
                town_wellman + town_west_burlington + town_west_liberty + town_williamsburg + 
                town_wilton + #town_winfield +
                ### CZ FIXED EFFECTS
                #cz_15 + cz_161 + cz_192 + cz_230 + cz_304 + cz_393 + cz_455 + cz_470 + cz_526 + #cz_621 +
                ### INTERACTION
                time_m3*treated + time_m2*treated + time_m1*treated + time_1*treated + 
                time_2*treated + time_3*treated + time_4*treated + time_5*treated,
              data = df_dummy)
summary(results)

stargazer::stargazer(results, out = "results/jobs.html", digits = 1)
