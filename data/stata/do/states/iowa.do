clear
use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/nn_bea.dta"
xtset id rel_yr

quietly xtreg jobsl_p1k treated##post cty_pop lag_unemp i.cz##i.cal_yr [aweight=weight] if st=="IA", fe cluster(id)
estimates store s00, title("Iowa")

quietly xtreg jobs_p1k treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 cty_pop lag_unemp i.cz##i.cal_yr [aweight=weight] if st=="IA", fe cluster(id)
estimates store s0, title("Iowa")

clear
use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/r_est_nn_bea.dta"
xtset id rel_yr

quietly xtreg jobsl_p1k treated##post cty_pop lag_unemp i.cz##i.cal_yr [aweight=weight] if st=="IA", fe cluster(id)
estimates store s1, title("Iowa")

quietly xtreg jobs_p1k treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 cty_pop lag_unemp i.cz##i.cal_yr [aweight=weight] if st=="IA", fe cluster(id)
estimates store s2, title("Iowa")

quietly xtreg estsl_p10k treated##post cty_pop lag_unemp i.cz##i.cal_yr [aweight=weight] if st=="IA", fe cluster(id)
estimates store s3, title("Iowa")

quietly xtreg ests_p10k treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 cty_pop lag_unemp i.cz##i.cal_yr [aweight=weight] if st=="IA", fe cluster(id)
estimates store s4, title("Iowa")

use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/iowa_nn.dta"
xtset id rel_yr

quietly xtreg sales_lead treated##post cty_pop real_inc_pc i.cz##i.cal_yr [aweight=weight], fe cluster(id)
estimates store s5, title("Pre/Post + Weights") 

quietly xtreg sales treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 cty_pop real_inc_pc i.cz##i.cal_yr [aweight=weight], fe cluster(id) 
estimates store s6, title("Event Study + Weights")

esttab s00 s0 s1 s2 s3 s4 s5 s6  using "results/states/iowa.html", keep(1.treated#1.post 1.treated#1.rel_yr_m3 1.treated#1.rel_yr_m2 1.treated#1.rel_yr_0 1.treated#1.rel_yr_1 1.treated#1.rel_yr_2 1.treated#1.rel_yr_3 1.treated#1.rel_yr_4 1.treated#1.rel_yr_5) legend varlabels(1.treated#1.post "Treated * Post" 1.treated#1.rel_yr_m3 "Treated * Year 3 Prior" 1.treated#1.rel_yr_m2 "Treated * Year 2 Prior" 1.treated#1.rel_yr_0 "Treated * Baseline Year" 1.treated#1.rel_yr_1 "Treated * Year 1 Post" 1.treated#1.rel_yr_2 "Treated * Year 2 Post" 1.treated#1.rel_yr_3 "Treated * Year 3 Post" 1.treated#1.rel_yr_4 "Treated * Year 4 Post" 1.treated#1.rel_yr_5 "Treated * Year 5 Post") ///
	mlabels("All Jobs" "" Jobs "" Ests. "" Sales "") collabels(none) ///
	cells(b(star fmt(1)) se(par fmt(1))) ///
	title("MSP Impact: Downtown Retail in Iowa") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace ///
	stats(r2 N, fmt(2 0) label("R<sup>2</sup>" N)) 

esttab s00 s0 s1 s2 s3 s4 s5 s6 using "results/tex/iowa.tex", keep(1.treated#1.post 1.treated#1.rel_yr_m3 1.treated#1.rel_yr_m2 1.treated#1.rel_yr_0 1.treated#1.rel_yr_1 1.treated#1.rel_yr_2 1.treated#1.rel_yr_3 1.treated#1.rel_yr_4 1.treated#1.rel_yr_5) legend varlabels(1.treated#1.post "Treated * Post" 1.treated#1.rel_yr_m3 "Treated * Year 3 Prior" 1.treated#1.rel_yr_m2 "Treated * Year 2 Prior" 1.treated#1.rel_yr_0 "Treated * Baseline Year" 1.treated#1.rel_yr_1 "Treated * Year 1 Post" 1.treated#1.rel_yr_2 "Treated * Year 2 Post" 1.treated#1.rel_yr_3 "Treated * Year 3 Post" 1.treated#1.rel_yr_4 "Treated * Year 4 Post" 1.treated#1.rel_yr_5 "Treated * Year 5 Post") ///
	mlabels("All Jobs" "" Jobs "" Ests. "" Sales "") collabels(none) ///
	cells(b(star fmt(2)) se(par fmt(1))) ///
	title("Impact of the Main Street Program in Iowa") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace ///
	stats(r2 N, fmt(2 0)) 
	
esttab s0 s2 s4 s6 using "results/tex/iowa_es.tex", keep(1.treated#1.rel_yr_m3 1.treated#1.rel_yr_m2 1.treated#1.rel_yr_0 1.treated#1.rel_yr_1 1.treated#1.rel_yr_2 1.treated#1.rel_yr_3 1.treated#1.rel_yr_4 1.treated#1.rel_yr_5) legend varlabels(1.treated#1.post "Treated * Post" 1.treated#1.rel_yr_m3 "Treated * Year 3 Prior" 1.treated#1.rel_yr_m2 "Treated * Year 2 Prior" 1.treated#1.rel_yr_0 "Treated * Baseline Year" 1.treated#1.rel_yr_1 "Treated * Year 1 Post" 1.treated#1.rel_yr_2 "Treated * Year 2 Post" 1.treated#1.rel_yr_3 "Treated * Year 3 Post" 1.treated#1.rel_yr_4 "Treated * Year 4 Post" 1.treated#1.rel_yr_5 "Treated * Year 5 Post") ///
	mlabels("All Jobs" "Retail Jobs" "Retail Estabs." "Retail Sales") collabels(none) ///
	cells(b(star fmt(2)) se(par fmt(1))) ///
	title("Impact of the Main Street Program in Iowa") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace ///
	stats(r2 N, fmt(2 0) label("R\textsuperscript{2} " "Observations ")) 
	
esttab s0 s2 s4 s6 using "results/csv/iowa.csv", replace wide plain b se keep(1.treated#1.rel_yr_m3 1.treated#1.rel_yr_m2 1.treated#1.rel_yr_0 1.treated#1.rel_yr_1 1.treated#1.rel_yr_2 1.treated#1.rel_yr_3 1.treated#1.rel_yr_4 1.treated#1.rel_yr_5) legend varlabels(1.treated#1.rel_yr_m3 "-3" 1.treated#1.rel_yr_m2 "-2" 1.treated#1.rel_yr_0 "0" 1.treated#1.rel_yr_1 "1" 1.treated#1.rel_yr_2 "2" 1.treated#1.rel_yr_3 "3" 1.treated#1.rel_yr_4 "4" 1.treated#1.rel_yr_5 "5")

