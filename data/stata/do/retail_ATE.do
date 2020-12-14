use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/r_est_nn.dta"
xtset id rel_yr

quietly xtreg jobs_p1k treated##post i.cz##i.cal_yr [aweight=weight], fe
estadd local nn Yes
estadd local wt Yes
estimates store m1, title("Pre/Post + Weights")

quietly xtreg ests_p10k treated##post i.cz##i.cal_yr [aweight=weight], fe
estadd local nn Yes
estadd local wt Yes
estimates store m2, title("Pre/Post + Weights")

use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/iowa_nn.dta"
xtset id rel_yr

quietly xtreg sales treated##post i.cz##i.cal_yr [aweight=weight], fe
estadd local nn Yes
estadd local wt Yes
estimates store m3, title("Pre/Post + Weights")

esttab m1 m2 using "results/pre_post_retail.html", keep(1.treated#1.post) ///
	legend varlabels(1.treated#1.post "Average Treatment Effect") ///
	mlabels(Jobs Ests. Sales) collabels(none) ///
	cells(b(star fmt(1)) se(par fmt(1))) ///
	title("MSP Impact: Downtown Retail") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace ///
	stats(r2 N, fmt(2 0) label("R<sup>2</sup>" N)) 

esttab m1 m2 using "results/tex/pre_post_retail.tex", keep(1.treated#1.post) ///
	legend varlabels(1.treated#1.post "Average Treatment Effect") ///
	mlabels(Jobs Ests. Sales) collabels(none) ///
	cells(b(star fmt(1)) se(par fmt(1))) ///
	title("MSP Impact: Downtown Retail") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace ///
	stats(r2 N, fmt(2 0) label("R<sup>2</sup>" N)) 
