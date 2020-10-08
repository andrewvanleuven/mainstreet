clear
import delimited "/Users/andrew/Documents/GitHub/mainstreet/data/csv/employment/rpanel_nn_buff2.csv", stringcols(26) 
label variable cz_yr "cz_yr"
destring rel_yr, replace
gen ests_p100k = (ests/(pop_2010/10000))
xtset id rel_yr
xtreg ests_p100k treated##post i.cz##i.cal_yr, fe
est table, keep(treated##post) b se p

xtreg ests_p100k treated##rel_yr_m3 treated##rel_yr_m1 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr, fe
est table, keep(treated##rel_yr_m3 treated##rel_yr_m1 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5) b se p
