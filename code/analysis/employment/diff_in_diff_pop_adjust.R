suppressMessages({library(tidyverse)
  library(rleuven)
  library(plm)
  library(scales)
  library(broom)
  library(fastDummies)})

# Import/Clean Data -------------------------------------------------------
#retail jobs per capita
dfn <- read_csv("data/csv/employment/rpanel_nn_buff2.csv") %>% filter(pop_2010 <= 35000) %>% 
  mutate(outcome_var = (jobs/(pop_2010/100)))

dfs <- read_csv("data/csv/employment/rpanel_stack_buff2.csv") %>% filter(pop_2010 <= 35000) %>% 
  filter(distance > 5 | distance == 0) %>% 
  mutate(outcome_var = (jobs/(pop_2010/100)))

#retail establishments per capita
dfen <- read_csv("data/csv/employment/rpanel_nn_buff2.csv") %>% filter(pop_2010 <= 35000) %>% 
  mutate(outcome_var = (ests/(pop_2010/100)))

dfes <- read_csv("data/csv/employment/rpanel_stack_buff2.csv") %>% filter(pop_2010 <= 35000) %>% 
  filter(distance > 5 | distance == 0) %>% 
  mutate(outcome_var = (ests/(pop_2010/100)))

#retail jobs pk lead
ldfn <- read_csv("data/csv/employment/rpanel_nn_buff2.csv") %>% filter(pop_2010 <= 35000) %>% 
  mutate(outcome_var = (jobs_lead/(pop_2010/100)))

ldfs <- read_csv("data/csv/employment/rpanel_stack_buff2.csv") %>% filter(pop_2010 <= 35000) %>% 
  filter(distance > 5 | distance == 0) %>% 
  mutate(outcome_var = (jobs_lead/(pop_2010/100)))

#retail ests pk lead
ldfen <- read_csv("data/csv/employment/rpanel_nn_buff2.csv") %>% filter(pop_2010 <= 35000) %>% 
  mutate(outcome_var = (ests_lead/(pop_2010/100)))

ldfes <- read_csv("data/csv/employment/rpanel_stack_buff2.csv") %>% filter(pop_2010 <= 35000) %>% 
  filter(distance > 5 | distance == 0) %>% 
  mutate(outcome_var = (ests_lead/(pop_2010/100)))

#### KEY ####
## rs = regression using stacked panel
## rn = regression using nearest-neighbor panel
## p = regression using post-treatment parameter
## y = regression using relative year (event study) parameter
## a,i,m,o,w = panel data subset by state (all, IA, MI, OH, or WI)

# Jobs regressions - all states -------------------------------------------
data_source_nn <- dfn
data_source_stack <- dfs

rspa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

stargazer::stargazer(rnpa,rnya,rspa,rsya,
                     digits = 2,
                     type="text",
                     out = "results/employment/pop_adjust/jobs.html",
                     covariate.labels = c("Treated * Post", "Treated * Rel. Year -3", "Treated * Rel. Year -1",
                                          "Treated * Rel. Year 0", "Treated * Rel. Year 1", "Treated * Rel. Year 2",
                                          "Treated * Rel. Year 3", "Treated * Rel. Year 4", "Treated * Rel. Year 5"),
                     dep.var.caption = "Dependent Variable: Downtown Jobs Per-Capita",
                     dep.var.labels = "",
                     column.labels   = c("Nearest Neighbor", "CZ Stacks"),
                     column.separate = c(2,2),
                     omit = c("cz","cal_yr"),
                     omit.stat = c("ser", "f","adj.rsq"))

# Establishments regressions - All States ---------------------------------

data_source_nn <- dfen
data_source_stack <- dfes

rspa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

stargazer::stargazer(rnpa,rnya,rspa,rsya,
                     digits = 2,
                     type="text",
                     out = "results/employment/pop_adjust/ests.html",
                     covariate.labels = c("Treated * Post", "Treated * Rel. Year -3", "Treated * Rel. Year -1",
                                          "Treated * Rel. Year 0", "Treated * Rel. Year 1", "Treated * Rel. Year 2",
                                          "Treated * Rel. Year 3", "Treated * Rel. Year 4", "Treated * Rel. Year 5"),
                     dep.var.caption = "Dependent Variable: Downtown Establishments Per-Capita",
                     dep.var.labels = "",
                     column.labels   = c("Nearest Neighbor", "CZ Stacks"),
                     column.separate = c(2,2),
                     omit = c("cz","cal_yr"),
                     omit.stat = c("ser", "f","adj.rsq"))

# Jobs lead regressions - all ---------------------------------------------

data_source_nn <- ldfn
data_source_stack <- ldfs

rspa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

stargazer::stargazer(rnpa,rnya,rspa,rsya,
                     digits = 2,
                     type="text",
                     out = "results/employment/pop_adjust/jobs_lead.html",
                     covariate.labels = c("Treated * Post", "Treated * Rel. Year -3", "Treated * Rel. Year -1",
                                          "Treated * Rel. Year 0", "Treated * Rel. Year 1", "Treated * Rel. Year 2",
                                          "Treated * Rel. Year 3", "Treated * Rel. Year 4", "Treated * Rel. Year 5"),
                     dep.var.caption = "Dependent Variable: Downtown Jobs Per-Capita (Lead)",
                     dep.var.labels = "",
                     column.labels   = c("Nearest Neighbor", "CZ Stacks"),
                     column.separate = c(2,2),
                     omit = c("cz","cal_yr"),
                     omit.stat = c("ser", "f","adj.rsq"))

# Establishments lead regressions - all -----------------------------------

data_source_nn <- ldfen
data_source_stack <- ldfes

rspa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

stargazer::stargazer(rnpa,rnya,rspa,rsya,
                     digits = 2,
                     type="text",
                     out = "results/employment/pop_adjust/ests_lead.html",
                     covariate.labels = c("Treated * Post", "Treated * Rel. Year -3", "Treated * Rel. Year -1",
                                          "Treated * Rel. Year 0", "Treated * Rel. Year 1", "Treated * Rel. Year 2",
                                          "Treated * Rel. Year 3", "Treated * Rel. Year 4", "Treated * Rel. Year 5"),
                     dep.var.caption = "Dependent Variable: Downtown Establishments Per-Capita (Lead)",
                     dep.var.labels = "",
                     column.labels   = c("Nearest Neighbor", "CZ Stacks"),
                     column.separate = c(2,2),
                     omit = c("cz","cal_yr"),
                     omit.stat = c("ser", "f","adj.rsq"))

# Job Regressions - by state ------------------------------------------------------
data_source_nn <- dfn
data_source_stack <- dfs

rspi <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', IA')),
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpi <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', IA')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsyi <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', IA')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnyi <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', IA')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

rspo <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', OH')),
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpo <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', OH')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsyo <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', OH')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnyo <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', OH')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

rspm <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', MI')),
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpm <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', MI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsym <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', MI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnym <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', MI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

rspw <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', WI')),
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpw <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', WI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsyw <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', WI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnyw <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', WI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

rspa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

stargazer::stargazer(rnpa,rnya,rspa,rsya,
                     rnpi,rnyi,rspi,rsyi,
                     rnpm,rnym,rspm,rsym,
                     rnpo,rnyo,rspo,rsyo,
                     rnpw,rnyw,rspw,rsyw,
                     digits = 2,
                     type="text",
                     out = "results/employment/pop_adjust/jobs_states.html",
                     covariate.labels = c("Treated * Post", "Treated * Rel. Year -3", "Treated * Rel. Year -1",
                                          "Treated * Rel. Year 0", "Treated * Rel. Year 1", "Treated * Rel. Year 2",
                                          "Treated * Rel. Year 3", "Treated * Rel. Year 4", "Treated * Rel. Year 5"),
                     dep.var.caption = "Dependent Variable: Downtown Jobs Per-Capita",
                     dep.var.labels = "",
                     column.labels   = c('All','Iowa','Michigan','Ohio','Wisconsin'),
                     column.separate = c(4,4,4,4,4),
                     omit = c("cz","cal_yr"),
                     omit.stat = c("ser", "f","adj.rsq"),
                     add.lines = list(c("Nearest Neighbor Match?","Yes","Yes","","","Yes","Yes","","","Yes","Yes","","","Yes","Yes","","","Yes","Yes","",""),
                                      c("CZ Stack Match?","","","Yes","Yes","","","Yes","Yes","","","Yes","Yes","","","Yes","Yes","","","Yes","Yes")))

# Est. Regressions - by state ------------------------------------------------------
data_source_nn <- dfen
data_source_stack <- dfes

rspi <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', IA')),
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpi <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', IA')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsyi <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', IA')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnyi <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', IA')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

rspo <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', OH')),
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpo <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', OH')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsyo <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', OH')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnyo <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', OH')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

rspm <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', MI')),
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpm <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', MI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsym <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', MI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnym <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', MI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

rspw <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', WI')),
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpw <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', WI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsyw <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack %>% filter(str_detect(town, ', WI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnyw <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn %>% filter(str_detect(town, ', WI')), 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

rspa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_stack, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnpa <- plm(outcome_var ~ treated*post + factor(cz)*factor(cal_yr),
            data = data_source_nn, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rsya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_stack, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")
rnya <- plm(outcome_var ~ treated*rel_yr_m3 + treated*rel_yr_m1 + treated*rel_yr_0 + 
              treated*rel_yr_1 + treated*rel_yr_2 + treated*rel_yr_3 + treated*rel_yr_4 + 
              treated*rel_yr_5 + factor(cz)*factor(cal_yr),
            data = data_source_nn, 
            index = c("id","rel_yr"),
            method = "within",
            effect = "twoways")

stargazer::stargazer(rnpa,rnya,rspa,rsya,
                     rnpi,rnyi,rspi,rsyi,
                     rnpm,rnym,rspm,rsym,
                     rnpo,rnyo,rspo,rsyo,
                     rnpw,rnyw,rspw,rsyw,
                     digits = 2,
                     type="text",
                     out = "results/employment/pop_adjust/ests_states.html",
                     covariate.labels = c("Treated * Post", "Treated * Rel. Year -3", "Treated * Rel. Year -1",
                                          "Treated * Rel. Year 0", "Treated * Rel. Year 1", "Treated * Rel. Year 2",
                                          "Treated * Rel. Year 3", "Treated * Rel. Year 4", "Treated * Rel. Year 5"),
                     dep.var.caption = "Dependent Variable: Downtown Establishments Per-Capita",
                     dep.var.labels = "",
                     column.labels   = c('All','Iowa','Michigan','Ohio','Wisconsin'),
                     column.separate = c(4,4,4,4,4),
                     omit = c("cz","cal_yr"),
                     omit.stat = c("ser", "f","adj.rsq"),
                     add.lines = list(c("Nearest Neighbor Match?","Yes","Yes","","","Yes","Yes","","","Yes","Yes","","","Yes","Yes","","","Yes","Yes","",""),
                                      c("CZ Stack Match?","","","Yes","Yes","","","Yes","Yes","","","Yes","Yes","","","Yes","Yes","","","Yes","Yes")))
beepr::beep()




