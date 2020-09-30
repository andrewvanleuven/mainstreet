clear
use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/msp_stack.dta"
label variable cz_yr "cz_yr"
generate weight = matched
replace weight = 1 if treated == 1 & weight == .
destring rel_yr, replace
egen town_id = group(town)
gen log_jobs = ln(jobs)
gen jobs_p100k = (jobs/(pop_2010/100))
xtset id rel_yr
xtreg jobs_p100k treated##post i.cz##i.cal_yr iweight(weight), fe
xtreg jobs_p100k treated##rel_yr_m3 treated##rel_yr_m1 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr aweight=weight, fe

*est table, keep(treated post treated#post) b se
