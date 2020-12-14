clear
* CZ Stack Matching Method
use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/stack.dta"
xtset id rel_yr

quietly xtreg estsl_p10k treated##post i.cz##i.cal_yr, fe
estadd local stk Yes
estimates store m1, title("Pre/Post")

quietly xtreg estsl_p10k treated##post i.cz##i.cal_yr [aweight=weight], fe
estadd local stk Yes
estadd local wt Yes
estimates store m2, title("Pre/Post + Weights")

quietly xtreg estsl_p10k treated##rel_yr_m3 treated##rel_yr_m1 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr, fe
estadd local stk Yes
estimates store m3, title("Event Study")

quietly xtreg estsl_p10k treated##rel_yr_m3 treated##rel_yr_m1 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr [aweight=weight], fe 
estadd local stk Yes
estadd local wt Yes
estimates store m4, title("Event Study + Weights")

* Nearest Neighbor Matching Method	
use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/nn.dta"
xtset id rel_yr

quietly xtreg estsl_p10k treated##post i.cz##i.cal_yr, fe
estadd local nn Yes
estimates store m1nn, title("Pre/Post")

quietly xtreg estsl_p10k treated##post i.cz##i.cal_yr [aweight=weight], fe
estadd local nn Yes
estadd local wt Yes
estimates store m2nn, title("Pre/Post + Weights")

quietly xtreg estsl_p10k treated##rel_yr_m3 treated##rel_yr_m1 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr, fe
estadd local nn Yes
estimates store m3nn, title("Event Study")

quietly xtreg estsl_p10k treated##rel_yr_m3 treated##rel_yr_m1 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr [aweight=weight], fe 
estadd local nn Yes
estadd local wt Yes
estimates store m4nn, title("Event Study + Weights")

estout m1 m2 m3 m4 m1nn m2nn m3nn m4nn, keep(1.treated#1.post 1.treated#1.rel_yr_m3 1.treated#1.rel_yr_m1 1.treated#1.rel_yr_0 1.treated#1.rel_yr_1 1.treated#1.rel_yr_2 1.treated#1.rel_yr_3 1.treated#1.rel_yr_4 1.treated#1.rel_yr_5) legend label varlabels(1.treated#1.post "Treated * Post" 1.treated#1.rel_yr_m3 "Treated * Year 3 Prior" 1.treated#1.rel_yr_m1 "Treated * Year 1 Prior" 1.treated#1.rel_yr_0 "Treated * Baseline Year" 1.treated#1.rel_yr_1 "Treated * Year 1 Post" 1.treated#1.rel_yr_2 "Treated * Year 2 Post" 1.treated#1.rel_yr_3 "Treated * Year 3 Post" 1.treated#1.rel_yr_4 "Treated * Year 4 Post" 1.treated#1.rel_yr_5 "Treated * Year 5 Post") ///
	cells(b(star fmt(1)) se(par fmt(1))) collabels(none) ///
	title("Downtown Establishments Per 10,000 Residents (Lead)") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stats(r2 N stk nn wt, fmt(3 0) label(R^2 N "CZ Stack Match?" "Nearest Neighbor Match?" "Weights"))  
	
esttab m1 m2 m3 m4 m1nn m2nn m3nn m4nn using "results/ests_lead.html", keep(1.treated#1.post 1.treated#1.rel_yr_m3 1.treated#1.rel_yr_m1 1.treated#1.rel_yr_0 1.treated#1.rel_yr_1 1.treated#1.rel_yr_2 1.treated#1.rel_yr_3 1.treated#1.rel_yr_4 1.treated#1.rel_yr_5) legend varlabels(1.treated#1.post "Treated * Post" 1.treated#1.rel_yr_m3 "Treated * Year 1 Prior" 1.treated#1.rel_yr_m1 "Treated * Year 1 Prior" 1.treated#1.rel_yr_0 "Treated * Baseline Year" 1.treated#1.rel_yr_1 "Treated * Year 1 Post" 1.treated#1.rel_yr_2 "Treated * Year 2 Post" 1.treated#1.rel_yr_3 "Treated * Year 3 Post" 1.treated#1.rel_yr_4 "Treated * Year 4 Post" 1.treated#1.rel_yr_5 "Treated * Year 5 Post") ///
	mlabels(none) collabels(none) ///
	cells(b(star fmt(2)) se(par fmt(1))) ///
	title("Downtown Establishments Per 10,000 Residents (Lead)") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace width(100%) ///
	stats(r2 N stk nn wt, fmt(2 0) label("R<sup>2</sup>" N "CZ Stack Match?" "Nearest Neighbor Match?" "Weights"))  
	
esttab m1 m2 m3 m4 m1nn m2nn m3nn m4nn using "results/ests_lead.tex", keep(1.treated#1.post 1.treated#1.rel_yr_m3 1.treated#1.rel_yr_m1 1.treated#1.rel_yr_0 1.treated#1.rel_yr_1 1.treated#1.rel_yr_2 1.treated#1.rel_yr_3 1.treated#1.rel_yr_4 1.treated#1.rel_yr_5) legend varlabels(1.treated#1.post "Treated * Post" 1.treated#1.rel_yr_m3 "Treated * Year 1 Prior" 1.treated#1.rel_yr_m1 "Treated * Year 1 Prior" 1.treated#1.rel_yr_0 "Treated * Baseline Year" 1.treated#1.rel_yr_1 "Treated * Year 1 Post" 1.treated#1.rel_yr_2 "Treated * Year 2 Post" 1.treated#1.rel_yr_3 "Treated * Year 3 Post" 1.treated#1.rel_yr_4 "Treated * Year 4 Post" 1.treated#1.rel_yr_5 "Treated * Year 5 Post") ///
	mlabels(none) collabels(none) ///
	cells(b(star fmt(2)) se(par fmt(1))) ///
	title("Downtown Establishments Per 10,000 Residents (Lead)") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace booktabs ///
	stats(r2 N stk nn wt, fmt(2 0) label("R<sup>2</sup>" N "CZ Stack Match?" "Nearest Neighbor Match?" "Weights")) 
