suppressMessages({library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
library(plm)
library(did)
library(scales)
library(broom)
library(fastDummies)
library(panelr)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Import/Clean Data -------------------------------------------------------
df <- read_csv("data/csv/panel.csv") %>% mutate(time = str_replace_all(time, "-", "m"))
df_dummy<- dummy_cols(df, select_columns = c("cz", "time", "cal_year", "town")) %>%
  janitor::clean_names() %>% 
  write_csv("panel_dummy.csv")

# Regressions -------------------------------------------------------------
reg0 <- lm(downtown_jobs ~ treated + 
             time_m3 + time_m2 + time_m1 + time_1 + time_2 + time_3 + time_4 + time_5 +
             ### YEAR FIXED EFFECTS
             #cal_year_1997 + cal_year_1998 + cal_year_1999 + cal_year_2000 + cal_year_2001 + cal_year_2002 +
             #cal_year_2003 + cal_year_2004 + cal_year_2005 + cal_year_2006 + cal_year_2007 + cal_year_2008 +
             #cal_year_2009 + cal_year_2010 + cal_year_2011 + cal_year_2012 + cal_year_2013 + cal_year_2014 +
             #cal_year_2015 + cal_year_2016 + #cal_year_2017 + 
             ### TOWN FIXED EFFECTS
             #town_albia + town_bloomfield + town_boone + town_britt + town_brooklyn + 
             #town_burlington + town_carroll + town_centerville + town_clarksville + 
             #town_clear_lake + town_colfax + town_coon_rapids + town_corydon + town_eldon + 
             #town_elkader + town_fayette + town_forest_city + town_fort_madison + 
             #town_garner + town_grand_junction + town_greene + town_greenfield + 
             #town_grinnell + town_grundy_center + town_guttenberg + town_independence + 
             #town_jefferson + town_keokuk + town_keota + town_knoxville + 
             #town_la_porte_city + town_lake_mills + town_lamoni + town_leon + 
             #town_madrid + town_manly + town_manning + town_marengo + town_marshalltown + 
             #town_mc_gregor + town_melcher_dallas + town_monona + town_monroe + 
           #town_montezuma + town_mount_pleasant + town_muscatine + town_nevada + 
           #town_new_london + town_new_sharon + town_newton + town_northwood + 
           #town_oelwein + town_ogden + town_osceola + town_oskaloosa + town_parkersburg + 
           #town_pella + town_postville + town_reinbeck + town_sigourney + town_state_center + 
           #town_strawberry_point + town_sumner + town_tama + town_tipton + town_toledo + 
           #town_traer + town_tripoli + town_wapello + town_waukon + town_waverly + 
           #town_wellman + town_west_burlington + town_west_liberty + town_williamsburg + 
           #town_wilton + #town_winfield +
           ### CZ FIXED EFFECTS
           #cz_15 + cz_161 + cz_192 + cz_230 + cz_304 + cz_393 + cz_455 + cz_470 + cz_526 + #cz_621 +
           ### INTERACTION
           time_m3*treated + time_m2*treated + time_m1*treated + time_1*treated + 
             time_2*treated + time_3*treated + time_4*treated + time_5*treated,
           data = df_dummy)

reg1 <- lm(downtown_jobs ~ treated + 
             time_m3 + time_m2 + time_m1 + time_1 + time_2 + time_3 + time_4 + time_5 +
             ### YEAR FIXED EFFECTS
             cal_year_1997 + cal_year_1998 + cal_year_1999 + cal_year_2000 + cal_year_2001 + cal_year_2002 +
             cal_year_2003 + cal_year_2004 + cal_year_2005 + cal_year_2006 + cal_year_2007 + cal_year_2008 +
             cal_year_2009 + cal_year_2010 + cal_year_2011 + cal_year_2012 + cal_year_2013 + cal_year_2014 +
             cal_year_2015 + cal_year_2016 + #cal_year_2017 + 
             ### TOWN FIXED EFFECTS
             #town_albia + town_bloomfield + town_boone + town_britt + town_brooklyn + 
             #town_burlington + town_carroll + town_centerville + town_clarksville + 
             #town_clear_lake + town_colfax + town_coon_rapids + town_corydon + town_eldon + 
             #town_elkader + town_fayette + town_forest_city + town_fort_madison + 
             #town_garner + town_grand_junction + town_greene + town_greenfield + 
             #town_grinnell + town_grundy_center + town_guttenberg + town_independence + 
             #town_jefferson + town_keokuk + town_keota + town_knoxville + 
             #town_la_porte_city + town_lake_mills + town_lamoni + town_leon + 
             #town_madrid + town_manly + town_manning + town_marengo + town_marshalltown + 
             #town_mc_gregor + town_melcher_dallas + town_monona + town_monroe + 
             #town_montezuma + town_mount_pleasant + town_muscatine + town_nevada + 
             #town_new_london + town_new_sharon + town_newton + town_northwood + 
             #town_oelwein + town_ogden + town_osceola + town_oskaloosa + town_parkersburg + 
             #town_pella + town_postville + town_reinbeck + town_sigourney + town_state_center + 
             #town_strawberry_point + town_sumner + town_tama + town_tipton + town_toledo + 
             #town_traer + town_tripoli + town_wapello + town_waukon + town_waverly + 
             #town_wellman + town_west_burlington + town_west_liberty + town_williamsburg + 
             #town_wilton + #town_winfield +
             ### CZ FIXED EFFECTS
             #cz_15 + cz_161 + cz_192 + cz_230 + cz_304 + cz_393 + cz_455 + cz_470 + cz_526 + #cz_621 +
             ### INTERACTION
             time_m3*treated + time_m2*treated + time_m1*treated + time_1*treated + 
             time_2*treated + time_3*treated + time_4*treated + time_5*treated,
           data = df_dummy)

reg2 <- lm(downtown_jobs ~ treated + 
             time_m3 + time_m2 + time_m1 + time_1 + time_2 + time_3 + time_4 + time_5 +
             ### YEAR FIXED EFFECTS
             #cal_year_1997 + cal_year_1998 + cal_year_1999 + cal_year_2000 + cal_year_2001 + cal_year_2002 +
             #cal_year_2003 + cal_year_2004 + cal_year_2005 + cal_year_2006 + cal_year_2007 + cal_year_2008 +
             #cal_year_2009 + cal_year_2010 + cal_year_2011 + cal_year_2012 + cal_year_2013 + cal_year_2014 +
             #cal_year_2015 + cal_year_2016 + #cal_year_2017 + 
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

reg3 <- lm(downtown_jobs ~ treated + 
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

# LaTeX output ------------------------------------------------------------
stargazer::stargazer(reg0,reg1,reg2,reg3,digits = 2,
                     font.size = "tiny", no.space = T,
                     out = c("results/jobs.tex","results/jobs.html"),
                     dep.var.labels = "Downtown Jobs",
                     keep = c("treated","treated:time_m3","treated:time_m2","treated:time_m1",
                              "treated:time_1","treated:time_2","treated:time_3","treated:time_4",
                              "treated:time_5","Constant"),
                     covariate.labels = c("Treated", "Treated * 3 yrs. before", "Treated * 2 yrs. before",
                                          "Treated * 1 yr. before", "Treated * 1 yr. after", 
                                          "Treated * 2 yrs. after","Treated * 3 yrs. after",
                                          "Treated * 4 yrs. after","Treated * 5 yrs. after"),
                     add.lines = list(c("Year Fixed effects?", "No", "Yes", "No", "Yes"),
                                      c("Community Fixed effects?", "No", "No", "Yes", "Yes")),
                     omit.stat = c("ser", "f","adj.rsq"))

# Plotting Visually -------------------------------------------------------
coeffs <- tidy(reg3) %>% 
  filter(str_detect(term, 'treated'),) %>% 
  select(-statistic)
coeffs[nrow(coeffs) + 1,] = c("treated:time_0","0","0","1") 

coeff <- (coeffs[c(1:4,10,5:9),]) %>% 
  mutate(year = row_number() - 5,
         sig = ifelse(p.value < 0.05,1,0),
         estimate = as.numeric(estimate)) %>% 
  filter(year > -4) 

ggplot(data = coeff, aes(x = year, y = estimate, group = 1)) +
  geom_vline(xintercept = 0, linetype="dashed", color = "#878787") +
  geom_line()+
  geom_point(aes(color = as.factor(sig)), size = 4) +
  scale_y_continuous(breaks= pretty_breaks()) +
  scale_x_continuous(breaks = coeff$year) +
  theme_minimal() +
  annotate("text", x=0, y=450, label= "Treatment Year", color = "#878787",
           size=6, family="Roboto Condensed", angle = 90, vjust = 1.25) + 
  labs(title="Jobs Added in Downtown Relative to Main Street Program Adoption",
       x ="Year", y = "Treatment Effect: New Downtown Jobs") +
  scale_color_manual(name = "Statistical\nSignificance",
                     labels = c("p \u2265 0.05", "p < 0.05"),
                     values = c("black","#BB0000")) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(plot.title = element_text(size=26, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=20, face="bold", margin = margin(t = 12)),
        axis.title.y = element_text(size=20, face="bold", margin = margin(r = 12)),
        legend.title=element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        legend.text=element_text(size=16),
        text=element_text(family="Roboto Condensed")) +
  ggsave("plot/slides/results_graph.png", height=8)
 