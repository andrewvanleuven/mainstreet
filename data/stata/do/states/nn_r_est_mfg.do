clear
use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/r_est_nn_mfg.dta"
xtset id rel_yr

quietly xtreg ests_p10k lq_mfg treated##post i.cz##i.cal_yr [aweight=weight], fe cluster(cty_fips)
estimates store s00, title("All")

quietly xtreg ests_p10k lq_mfg treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr [aweight=weight], fe cluster(cty_fips)
estimates store s0, title("All")

quietly xtreg ests_p10k lq_mfg treated##post i.cz##i.cal_yr [aweight=weight] if st=="IA", fe cluster(cty_fips)
estimates store s1, title("Iowa")

quietly xtreg ests_p10k lq_mfg treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr [aweight=weight] if st=="IA", fe cluster(cty_fips)
estimates store s2, title("Iowa")

quietly xtreg ests_p10k lq_mfg treated##post i.cz##i.cal_yr [aweight=weight] if st=="MI", fe cluster(cty_fips)
estimates store s3, title("Michigan")

quietly xtreg ests_p10k lq_mfg treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr [aweight=weight] if st=="MI", fe cluster(cty_fips)
estimates store s4, title("Michigan")

quietly xtreg ests_p10k lq_mfg treated##post i.cz##i.cal_yr [aweight=weight] if st=="OH", fe cluster(cty_fips)
estimates store s5, title("Ohio")

quietly xtreg ests_p10k lq_mfg treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr [aweight=weight] if st=="OH", fe cluster(cty_fips)
estimates store s6, title("Ohio")

quietly xtreg ests_p10k lq_mfg treated##post i.cz##i.cal_yr [aweight=weight] if st=="WI", fe cluster(cty_fips)
estimates store s7, title("Wisconsin")

quietly xtreg ests_p10k lq_mfg treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr [aweight=weight] if st=="WI", fe cluster(cty_fips)
estimates store s8, title("Wisconsin")

esttab s00 s0 s1 s2 s3 s4 s5 s6 s7 s8 using "results/states/nn_r_est_mfg.html", keep(1.treated#1.post 1.treated#1.rel_yr_m3 1.treated#1.rel_yr_m2 1.treated#1.rel_yr_0 1.treated#1.rel_yr_1 1.treated#1.rel_yr_2 1.treated#1.rel_yr_3 1.treated#1.rel_yr_4 1.treated#1.rel_yr_5 lq_mfg) legend varlabels(1.treated#1.post "Treated * Post" 1.treated#1.rel_yr_m3 "Treated * Year 3 Prior" 1.treated#1.rel_yr_m2 "Treated * Year 2 Prior" 1.treated#1.rel_yr_0 "Treated * Baseline Year" 1.treated#1.rel_yr_1 "Treated * Year 1 Post" 1.treated#1.rel_yr_2 "Treated * Year 2 Post" 1.treated#1.rel_yr_3 "Treated * Year 3 Post" 1.treated#1.rel_yr_4 "Treated * Year 4 Post" 1.treated#1.rel_yr_5 "Treated * Year 5 Post" lq_mfg "Manufacturing LQ") ///
	mlabels("All" "" "Iowa" "" "Michigan" "" "Ohio" "" "Wisconsin" "") collabels(none) ///
	cells(b(star fmt(1)) se(par fmt(1))) ///
	title("Downtown Establishments Per 10,000 Residents") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace width(75%) ///
	stats(r2 N stk nn wt, fmt(2 0) label("R<sup>2</sup>" N "CZ Stack Match?" "Nearest Neighbor Match?" "Weights"))
