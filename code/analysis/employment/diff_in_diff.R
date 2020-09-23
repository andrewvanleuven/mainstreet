suppressMessages({library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
library(scales)
library(broom)
library(fastDummies)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Import/Clean Data -------------------------------------------------------
df <- read_csv("data/csv/panel.csv") %>% mutate(time = str_replace_all(time, "-", "m"))
df_dummy<- dummy_cols(df, select_columns = c("time")) %>% #"cz", 
  janitor::clean_names() %>% 
  write_csv("panel_dummy.csv")

#foreign::write.dta(df_dummy,"stata_msp.dta")

# Regressions -------------------------------------------------------------
reg0 <- lm(downtown_jobs ~ 
             time_m3*treated + time_m2*treated + time_m1*treated + time_1*treated + 
             time_2*treated + time_3*treated + time_4*treated + time_5*treated,
           data = df_dummy)

reg1 <- lm(downtown_jobs ~            
             time_m3*treated + time_m2*treated + time_m1*treated + time_1*treated + 
             time_2*treated + time_3*treated + time_4*treated + time_5*treated +
             factor(cal_year), #+ factor(town)
           data = df_dummy)

reg2 <- lm(downtown_jobs ~            
             time_m3*treated + time_m2*treated + time_m1*treated + time_1*treated + 
             time_2*treated + time_3*treated + time_4*treated + time_5*treated +
             #factor(cal_year) +
             factor(town),
           data = df_dummy)

reg3 <- lm(downtown_jobs ~            
             time_m3*treated + time_m2*treated + time_m1*treated + time_1*treated + 
             time_2*treated + time_3*treated + time_4*treated + time_5*treated +
             factor(cal_year) +
             factor(town),
           data = df_dummy)

# LaTeX output ------------------------------------------------------------
stargazer::stargazer(reg0,reg1,reg2,reg3,digits = 2,
                     font.size = "tiny", no.space = T,
                     out = c("results/employment/jobs.tex","results/employment/jobs.html"),
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
  mutate(term = str_replace(term,'time_m3:treated','treated:time_m3')) %>% 
  select(-statistic) %>% 
  rbind(list("treated:time_0","0","0","1")) %>% 
  arrange(factor(term, levels = c("treated",
                                 "treated:time_m3",
                                 "treated:time_m2",
                                 "treated:time_m1",
                                 "treated:time_0",
                                 "treated:time_1",
                                 "treated:time_2",
                                 "treated:time_3",
                                 "treated:time_4",
                                 "treated:time_5"))) %>% 
  #filter(term != "treated") %>% 
  mutate(year = row_number() - 5,
         sig = ifelse(p.value < 0.05,1,0),
         estimate = as.numeric(estimate)) %>% 
  filter(year > -4) 

ggplot(data = coeffs, aes(x = year, y = estimate, group = 1)) +
  geom_vline(xintercept = 0, linetype="dashed", color = "#878787") +
  #geom_hline(yintercept = 0, color = "#878787") +
  geom_line()+
  geom_point(aes(color = as.factor(sig)), size = 4) +
  scale_y_continuous(breaks= pretty_breaks()) +
  scale_x_continuous(breaks = coeffs$year) +
  theme_minimal() +
  annotate("text", x=0, y=450, label= "Treatment Year", color = "#878787",
           size=6, family="LM Roman 10", angle = 90, vjust = 1.25) + 
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
        text=element_text(family="LM Roman 10")) +
  ggsave("plot/slides/results_graph.png", height=10, width = 16)
 