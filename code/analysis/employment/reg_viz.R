suppressMessages({library(tidyverse)
  library(rleuven)
  library(plm)
  library(scales)
  library(broom)})

rtail <- read_csv("data/stata/results/csv/all_retail.csv") %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  rename(relyr = na) %>% 
  filter(relyr != "N") %>% 
  pivot_longer(!relyr,names_to = "param") %>% 
  mutate(model = case_when(str_detect(param,"_2") ~ 'b',
                           str_detect(param,"_3") ~ 'c',
                           str_detect(param,"_4") ~ 'd',
                           str_detect(param,"_", negate = T) ~ 'a'),
         param = str_replace(param,"_",""),
         param = str_replace(param,"[:digit:]","")) %>% 
  arrange(relyr) %>% 
  pivot_wider(names_from = param, values_from = value) %>% 
  mutate(b = as.numeric(b),
         se = as.numeric(se),
         relyr = as.numeric(relyr)) %>% 
  mutate(se = round(se,1),
         sig = if_else(abs(b/se)>=1.645,1,0))
riowa <- read_csv("data/stata/results/csv/iowa.csv") %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  rename(relyr = na) %>% 
  filter(relyr != "N") %>% 
  pivot_longer(!relyr,names_to = "param") %>% 
  mutate(model = case_when(str_detect(param,"_2") ~ 'b',
                           str_detect(param,"_3") ~ 'c',
                           str_detect(param,"_4") ~ 'd',
                           str_detect(param,"_", negate = T) ~ 'a'),
         param = str_replace(param,"_",""),
         param = str_replace(param,"[:digit:]","")) %>% 
  arrange(relyr) %>% 
  pivot_wider(names_from = param, values_from = value) %>% 
  mutate(b = as.numeric(b),
         se = as.numeric(se),
         relyr = as.numeric(relyr)) %>% 
  mutate(se = round(se,1),
         sig = if_else(abs(b/se)>=1.645,1,0))

# Plot Retail Jobs/Estabs (all states) -----------------------------------------
r_model_names <- c(
  `a` = "Retail Jobs Per\n1,000 Residents",
  `b` = "Retail Establishments\nPer 1,000 Residents")

tx <- tibble(x = 1:5, y = 1) %>% 
  mutate(y = if_else(x == 1, 1,0))

ggplot() +
  geom_vline(xintercept = -.5, linetype="dashed", color = "#878787") +
  geom_hline(yintercept = 0) +
  geom_ribbon(data = rtail, aes(x = relyr, y = b, ymin=b-(1.645*se), ymax=b+(1.645*se)), alpha=.1) +
  geom_line(data = rtail, aes(x = relyr, y = b, color = factor(model)), size = 1) +
  geom_point(data = rtail, aes(x = relyr, y = b, fill = factor(sig)), shape = 21, size = 3, color = "black") +
  geom_point(data = tx, aes(x = x, y = y, fill = factor(y)), shape = 21, size = 2, color = "black", alpha = 0) +
  labs(x ="Year Relative to Treatment", 
       #title = "Impact of the Main Street Program in Iowa",
       y = " ") +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3,4,5),
                     labels=c('3 Yrs.\nPrior','2 Yrs.\nPrior','1 Yr.\nPrior','Baseline\nYear','1 Yr.\nPost',
                              '2 Yrs.\nPost','3 Yrs.\nPost','4 Yrs.\nPost','5 Yrs.\nPost')) + 
  scale_fill_manual(name = "Statistical Significance",
                    labels = rev(c("p < 0.1", "p > 0.1")),
                    values = (c("white","grey40")),
                    guide = guide_legend(reverse = TRUE),
                    drop = FALSE) +
  scale_color_manual(values = c('#BB0000','#F1B82D'),
                        labels = r_model_names,
                        name = "Outcome Variable") +
  facet_grid(vars(model),scales = "free_y",labeller = as_labeller(r_model_names), switch = "y") +
  #theme_minimal(base_family = "Helvetica") +
  #theme(plot.title = element_text(size=20, face="bold", hjust = 0.5),
  #      axis.title.x = element_text(size=16, margin = margin(t = 12)),
  #      axis.text.x = element_text(size=12, family="Helvetica Light"),
  #      axis.text.y = element_text(size=12, family="Helvetica Light"),
  #      legend.title=element_text(size=12 ,hjust = 0.5, face = "bold"),
  #      legend.text=element_text(size=10, family="Helvetica Light"),
  #      panel.spacing = unit(1.25, "lines"),
  #      strip.text.y = element_text(size = 13),
  #      strip.placement = "outside",
  #      legend.position = "bottom") +
  #ggsave("plot/manuscripts/all_retail_edq.png", height=7.5, width = 10)
  theme_minimal(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=16, margin = margin(t = 12)),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=10),
        panel.spacing = unit(1.25, "lines"),
        strip.text.y = element_text(size = 13),
        strip.placement = "outside",
        legend.position = "bottom") +
  ggsave("plot/manuscripts/all_retail.png", height=7, width = 10)

clr_cons()

# Plot Iowa Retail --------------------------------------------------------
model_names <- c(
  `a` = "Total Jobs Per\n1,000 Residents",
  `b` = "Retail Jobs Per\n1,000 Residents",
  `c` = "Retail Establishments\nPer 1,000 Residents",
  `d` = "Real Taxable Retail\nSales Per-Capita")

ggplot() +
  geom_vline(xintercept = -.5, linetype="dashed", color = "#878787") +
  geom_hline(yintercept = 0) +
  geom_ribbon(data = riowa, aes(x = relyr, y = b, ymin=b-(1.645*se), ymax=b+(1.645*se)), alpha=.1) +
  geom_line(data = riowa, aes(x = relyr, y = b, color = factor(model)), size = 1) +
  geom_point(data = riowa, aes(x = relyr, y = b, fill = factor(sig)), shape = 21, size = 3, color = "black") +
  labs(x ="Year Relative to Treatment", 
       #title = "Impact of the Main Street Program in Iowa",
       y = " ") +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3,4,5),
                     labels=c('3 Yrs.\nPrior','2 Yrs.\nPrior','1 Yr.\nPrior','Baseline\nYear','1 Yr.\nPost',
                              '2 Yrs.\nPost','3 Yrs.\nPost','4 Yrs.\nPost','5 Yrs.\nPost')) + 
  scale_color_manual(name = "",
                     labels = c('Total Jobs','Retail Jobs','Retail Ests.','Retail Sales'),
                     values = c('#BB0000','#F1B82D','#7BAFD4','#007A33')) +
  #scale_linetype_manual(values = c('dotted','twodash','dotdash','longdash'),
  #                      labels = c('Total Jobs','Retail Jobs','Retail Ests.','Retail Sales'),
  #                      name = "Outcome Variable") +
  scale_fill_manual(name = "Statistical Significance",
                    labels = rev(c("p < 0.1", "p > 0.1")),
                    values = (c("white","grey40")),
                    guide = guide_legend(reverse = TRUE)) +
  facet_grid(vars(model),scales = "free_y",labeller = as_labeller(model_names), switch = "y") +
  theme_minimal(base_family = "Helvetica") +
  #theme(plot.title = element_text(size=20, face="bold", hjust = 0.5),
  #      axis.title.x = element_text(size=16, margin = margin(t = 12)),
  #      axis.text.x = element_text(size=12, family="Helvetica Light",hjust = 1),
  #      axis.text.y = element_text(size=12, family="Helvetica Light"),
  #      legend.title=element_text(size=12 ,hjust = 0.5, face = "bold"),
  #      legend.text=element_text(size=12, family="Helvetica Light"),
  #      panel.spacing = unit(1.25, "lines"),
  #      legend.key.width=unit(1.75,"cm"),
  #      legend.box="vertical",
  #      strip.text.y = element_text(size = 13),
  #      strip.placement = "outside",
  #      legend.position = "bottom") +
  #ggsave("plot/manuscripts/iowa_edq.png", height=10, width = 10)
  theme_minimal(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=14, margin = margin(t = 12)),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=10),
        panel.spacing = unit(1.25, "lines"),
        strip.text.y = element_text(size = 14),
        strip.placement = "outside",
        #legend.box="vertical",
        legend.position = "bottom") +
    ggsave("plot/manuscripts/iowa.png", height=10, width = 10)
