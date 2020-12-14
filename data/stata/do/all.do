use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/nn.dta"
xtset id rel_yr

quietly xtreg jobs_p1k treated##post i.cz##i.cal_yr [aweight=weight], fe cluster(st)
estimates store s1

quietly xtreg jobs_p1k treated##rel_yr_m3 treated##rel_yr_m2 treated##rel_yr_0 treated##rel_yr_1 treated##rel_yr_2 treated##rel_yr_3 treated##rel_yr_4 treated##rel_yr_5 i.cz##i.cal_yr [aweight=weight], fe cluster(st)
estimates store s2

esttab s1 s2 using "results/states/all_raw.html", keep(1.treated#1.post 1.treated#1.rel_yr_m3 1.treated#1.rel_yr_m2 1.treated#1.rel_yr_0 1.treated#1.rel_yr_1 1.treated#1.rel_yr_2 1.treated#1.rel_yr_3 1.treated#1.rel_yr_4 1.treated#1.rel_yr_5) legend varlabels(1.treated#1.post "Treated * Post" 1.treated#1.rel_yr_m3 "Treated * Year 3 Prior" 1.treated#1.rel_yr_m2 "Treated * Year 2 Prior" 1.treated#1.rel_yr_0 "Treated * Baseline Year" 1.treated#1.rel_yr_1 "Treated * Year 1 Post" 1.treated#1.rel_yr_2 "Treated * Year 2 Post" 1.treated#1.rel_yr_3 "Treated * Year 3 Post" 1.treated#1.rel_yr_4 "Treated * Year 4 Post" 1.treated#1.rel_yr_5 "Treated * Year 5 Post") ///
	mlabels(none) collabels(none) ///
	cells(b(star fmt(1)) se(par fmt(1))) ///
	title("Downtown Jobs Per 1,000 Residents") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace ///
	stats(r2 N, fmt(2 0) label("R<sup>2</sup>" N)) 

esttab s1 s2 using "results/tex/all_jobs.tex", keep(1.treated#1.post 1.treated#1.rel_yr_m3 1.treated#1.rel_yr_m2 1.treated#1.rel_yr_0 1.treated#1.rel_yr_1 1.treated#1.rel_yr_2 1.treated#1.rel_yr_3 1.treated#1.rel_yr_4 1.treated#1.rel_yr_5) legend varlabels(1.treated#1.post "Treated * Post" 1.treated#1.rel_yr_m3 "Treated * Year 3 Prior" 1.treated#1.rel_yr_m2 "Treated * Year 2 Prior" 1.treated#1.rel_yr_0 "Treated * Baseline Year" 1.treated#1.rel_yr_1 "Treated * Year 1 Post" 1.treated#1.rel_yr_2 "Treated * Year 2 Post" 1.treated#1.rel_yr_3 "Treated * Year 3 Post" 1.treated#1.rel_yr_4 "Treated * Year 4 Post" 1.treated#1.rel_yr_5 "Treated * Year 5 Post") ///
	mlabels(none) collabels(none) ///
	cells(b(star fmt(1)) se(par fmt(1))) ///
	title("Downtown Jobs Per 1,000 Residents") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace ///
	stats(r2 N, fmt(2 0) label("R<sup>2</sup>" N)) 
