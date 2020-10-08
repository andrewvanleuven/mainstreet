clear
import delimited "/Users/andrew/Documents/GitHub/mainstreet/data/csv/employment/i_panel_nn.csv", stringcols(26) 
xtset id rel_yr

xtreg sales treated##post i.cz##i.cal_yr , fe
est table, keep(treated##post) b se p

xtreg sales treated##post i.cz##i.cal_yr [aweight=weight], fe
est table, keep(treated##post) b se p

xtreg sales treated##rel_yr_m3 treated##rel_yr_m1 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr, fe
est table, keep(treated##rel_yr_m3 treated##rel_yr_m1 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5) b se p

xtreg sales treated##rel_yr_m3 treated##rel_yr_m1 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr [aweight=weight], fe
est table, keep(treated##rel_yr_m3 treated##rel_yr_m1 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5) b se p
