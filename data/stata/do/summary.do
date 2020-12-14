clear
use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/sum.dta"
estpost summarize pop_2010 pct_nonwhite_2010 median_age_2010 median_inc_2010 cal_yr rucc treated jobs ests jobs_p1k ests_p10k, listwise

esttab, cells("mean(fmt(1)) sd(fmt(1)) min(fmt(0)) max(fmt(0))") nomtitle nonumber ///
varlabels(pop_2010 "Population (2010)" pct_nonwhite_2010 "Percent Non-White (2010)" median_age_2010 "Median Age (2010)" median_inc_2010 "Median Household Income (2010)" cal_yr "Year" rucc "Rural-Urban Continuum Code" treated "Treatment (MSP Adopted?)" jobs "Downtown Jobs" ests "Downtown Establishments" jobs_p1k "Downtown Jobs Per 1k" ests_p10k "Downtown Establishments Per 1k" ) 

*esttab using "results/tex/sum.tex", cells("mean(fmt(1)) sd(fmt(1)) min(fmt(0)) max(fmt(0))") ///
*varlabels(pop_2010 "Population (2010)" pct_nonwhite_2010 "Percent Non-White (2010)" median_age_2010 "Median Age (2010)" median_inc_2010 "Median Household Income (2010)" cal_yr "Year" rucc "Rural-Urban Continuum Code" treated "Treatment (MSP Adopted?)" jobs "Downtown Jobs" ests "Downtown Establishments" jobs_p1k "Downtown Jobs Per 1k" ests_p10k "Downtown Establishments Per 1k" ) ///
*nomtitle nonumber title("Summary Statistics") replace booktabs

esttab using "results/states/sum.html", cells("mean(fmt(1)) sd(fmt(1)) min(fmt(0)) max(fmt(0))") ///
varlabels(pop_2010 "Population (2010)" pct_nonwhite_2010 "Percent Non-White (2010)" median_age_2010 "Median Age (2010)" median_inc_2010 "Median Household Income (2010)" cal_yr "Year" rucc "Rural-Urban Continuum Code" treated "Treatment (MSP Adopted?)" jobs "Downtown Jobs" ests "Downtown Establishments" jobs_p1k "Downtown Jobs Per 1k" ests_p10k "Downtown Establishments Per 1k" ) ///
nomtitle nonumber title("Summary Statistics") replace 
