use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/stata_msp_nearest.dta"
xtset city_fips
xtreg jobs treated post treated#post i.cal_yr, fe
