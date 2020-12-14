clear
use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/nn.dta"
xtset id rel_yr

*quietly xtreg estsl_p10k treated##post i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight], fe cluster(id)
*estimates store s00, title("All")

quietly xtreg ests_p10k treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 cty_pop lag_unemp i.cz##i.cal_yr [aweight=weight], fe cluster(id)
estimates store s0, title("All")

*quietly xtreg estsl_p10k treated##post i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight] if st=="IA", fe cluster(id)
*estimates store s1, title("Iowa")

quietly xtreg ests_p10k treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 cty_pop lag_unemp i.cz##i.cal_yr [aweight=weight] if st=="IA", fe cluster(id)
estimates store s2, title("Iowa")

*quietly xtreg estsl_p10k treated##post i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight] if st=="MI", fe cluster(id)
*estimates store s3, title("Michigan")

quietly xtreg ests_p10k treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight] if st=="MI", fe cluster(id)
estimates store s4, title("Michigan")

*quietly xtreg estsl_p10k treated##post i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight] if st=="OH", fe cluster(id)
*estimates store s5, title("Ohio")

quietly xtreg ests_p10k treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight] if st=="OH", fe cluster(id)
estimates store s6, title("Ohio")

*quietly xtreg estsl_p10k treated##post i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight] if st=="WI", fe cluster(id)
*estimates store s7, title("Wisconsin")

quietly xtreg ests_p10k treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr cty_pop lag_unemp [aweight=weight] if st=="WI", fe cluster(id)
estimates store s8, title("Wisconsin")

esttab s0 s2 s4 s6 s8 using "results/states/nn_ests.html", keep(1.treated#1.rel_yr_m3 1.treated#1.rel_yr_m2 1.treated#1.rel_yr_0 1.treated#1.rel_yr_1 1.treated#1.rel_yr_2 1.treated#1.rel_yr_3 1.treated#1.rel_yr_4 1.treated#1.rel_yr_5) legend varlabels(1.treated#1.rel_yr_m3 "Treated * Year 3 Prior" 1.treated#1.rel_yr_m2 "Treated * Year 2 Prior" 1.treated#1.rel_yr_0 "Treated * Baseline Year" 1.treated#1.rel_yr_1 "Treated * Year 1 Post" 1.treated#1.rel_yr_2 "Treated * Year 2 Post" 1.treated#1.rel_yr_3 "Treated * Year 3 Post" 1.treated#1.rel_yr_4 "Treated * Year 4 Post" 1.treated#1.rel_yr_5 "Treated * Year 5 Post") ///
	mlabels("All" "Iowa" "Michigan" "Ohio" "Wisconsin") collabels(none) ///
	cells(b(star fmt(2)) se(par fmt(1))) ///
	title("Downtown Retail Establishments Per 1,000 Residents") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace width(75%) ///
	stats(N r2, fmt(0 2) label("Observations" "R<sup>2</sup>"))
