suppressMessages({library(tidyverse)
library(rleuven)
library(plm)
library(scales)
library(broom)
library(fastDummies)})

# Import/Clean Data -------------------------------------------------------
dfn <- read_csv("data/csv/employment/epanel_nn.csv") %>% 
  mutate(outcome_var = (jobs/(pop_2010/100))) #%>% filter(rucc > 3, pop_2010 < 30000)

dfs <- read_csv("data/csv/employment/epanel_stack.csv") %>% 
  filter(distance > 5 | distance == 0) %>% 
  mutate(outcome_var = (jobs/(pop_2010/100))) #%>% filter(rucc > 3, pop_2010 < 30000)

#### KEY ####
## rs = regression using stacked panel
## rn = regression using nearest-neighbor panel
## p = regression using post-treatment parameter
## y = regression using relative year (event study) parameter
## a,i,m,o,w = panel data subset by state (all, IA, MI, OH, or WI)

# Regressions - all states ------------------------------------------------
rspa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
                data = dfs, 
                index = c("id","rel_yr"),
                method = "within",
                effect = "twoways")
rnpa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
                data = dfn, 
                index = c("id","rel_yr"),
                method = "within",
                effect = "twoways")
rsya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
                  treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
                  treated*rel_yr_5 + factor(cz)*factor(cal_yr),
                data = dfs, 
                index = c("id","rel_yr"),
                method = "within",
                effect = "twoways")
rnya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
                  treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
                  treated*rel_yr_5 + factor(cz)*factor(cal_yr),
                data = dfn, 
                index = c("id","rel_yr"),
                method = "within",
                effect = "twoways")
# Output Tables -----------------------------------------------------------
stargazer::stargazer(rspa,rnpa,rsya,rnya,
                     digits = 2,
                     type="text",
                     out = "results/employment/jobs.html",
                     #column.labels   = c("All", "Iowa","Ohio", "Michigan","Wisconsin"),
                     #column.separate = c(4,4,4,4,4),
                     omit = c("cz","cal_yr"),
                     omit.stat = c("ser", "f","adj.rsq"))
beepr::beep()


# Regressions - Iowa ------------------------------------------------------
rspi <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = dfs %>% filter(str_detect(town, ', IA')),
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpi <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = dfn %>% filter(str_detect(town, ', IA')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsyi <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = dfs %>% filter(str_detect(town, ', IA')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnyi <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = dfn %>% filter(str_detect(town, ', IA')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

# Regressions - Ohio ------------------------------------------------------
rspo <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = dfs %>% filter(str_detect(town, ', OH')),
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpo <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = dfn %>% filter(str_detect(town, ', OH')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsyo <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = dfs %>% filter(str_detect(town, ', OH')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnyo <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = dfn %>% filter(str_detect(town, ', OH')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

# Regressions - Mich ------------------------------------------------------
rspm <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
           data = dfs %>% filter(str_detect(town, ', MI')),
           index = c("id","rel_yr"),
           method = "within",
           effect = "twoways")
rnpm <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
           data = dfn %>% filter(str_detect(town, ', MI')), 
           index = c("id","rel_yr"),
           method = "within",
           effect = "twoways")
rsym <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
             treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
             treated*rel_yr_5 + factor(cz)*factor(cal_yr),
           data = dfs %>% filter(str_detect(town, ', MI')), 
           index = c("id","rel_yr"),
           method = "within",
           effect = "twoways")
rnym <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = dfn %>% filter(str_detect(town, ', MI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

# Regressions - Wisc ------------------------------------------------------
rspw <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
           data = dfs %>% filter(str_detect(town, ', WI')),
           index = c("id","rel_yr"),
           method = "within",
           effect = "twoways")
rnpw <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
           data = dfn %>% filter(str_detect(town, ', WI')), 
           index = c("id","rel_yr"),
           method = "within",
           effect = "twoways")
rsyw <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
             treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
             treated*rel_yr_5 + factor(cz)*factor(cal_yr),
           data = dfs %>% filter(str_detect(town, ', WI')), 
           index = c("id","rel_yr"),
           method = "within",
           effect = "twoways")
rnyw <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = dfn %>% filter(str_detect(town, ', WI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

#rspi,rnpi,rsyi,rnyi,
#rspo,rnpo,rsyo,rnyo,
#rspm,rnpm,rsym,rnym,
#rspw,rnpw,rsyw,rnyw,