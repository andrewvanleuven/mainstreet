clear
use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/nn.dta"
xtset id rel_yr

quietly xtreg jobsl_p1k treated##post i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight], fe cluster(st)
estimates store s00, title("All")

quietly xtreg jobs_p1k treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight], fe cluster(st)
estimates store s01, title("All")

clear
use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/r_est_nn.dta"
xtset id rel_yr

quietly xtreg jobsl_p1k treated##post i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight], fe cluster(st)
estimates store s02, title("All")

quietly xtreg jobs_p1k treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight], fe cluster(st)
estimates store s03, title("All")

quietly xtreg estsl_p10k treated##post i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight], fe cluster(st)
estimates store s04, title("All")

quietly xtreg ests_p10k treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 cty_pop lag_unemp i.cz##i.cal_yr [aweight=weight], fe cluster(st)
estimates store s05, title("All")

esttab s00 s01 s02 s03 s04 s05 using "results/states/all_all.html", keep(1.treated#1.post 1.treated#1.rel_yr_m3 1.treated#1.rel_yr_m2 1.treated#1.rel_yr_0 1.treated#1.rel_yr_1 1.treated#1.rel_yr_2 1.treated#1.rel_yr_3 1.treated#1.rel_yr_4 1.treated#1.rel_yr_5) legend varlabels(1.treated#1.post "Treated * Post" 1.treated#1.rel_yr_m3 "Treated * Year 3 Prior" 1.treated#1.rel_yr_m2 "Treated * Year 2 Prior" 1.treated#1.rel_yr_0 "Treated * Baseline Year" 1.treated#1.rel_yr_1 "Treated * Year 1 Post" 1.treated#1.rel_yr_2 "Treated * Year 2 Post" 1.treated#1.rel_yr_3 "Treated * Year 3 Post" 1.treated#1.rel_yr_4 "Treated * Year 4 Post" 1.treated#1.rel_yr_5 "Treated * Year 5 Post") ///
	mlabels("Total Jobs" "" "Retail Jobs" "" "Retail Estabs." "") collabels(none) ///
	cells(b(star fmt(2)) se(par fmt(1))) ///
	title("Impact of the Main Street Program in Iowa") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace ///
	stats(N r2, fmt(0 2) label("Observations" "R<sup>2</sup>")) 
