clear
use "/Users/andrew/Documents/GitHub/mainstreet/hidden/datatree/cleaned/datatree_model.dta"

quietly regress ln_realsaleprice ln_distance in_downtown, cluster(city_fips)
estimates store m1

quietly regress ln_realsaleprice ln_distance in_downtown ln_lotsize ln_sqft age total_rooms stories, cluster(city_fips)
estimates store m2

quietly regress ln_realsaleprice ln_distance in_downtown ln_lotsize ln_sqft age total_rooms stories nbhood_age nbhood_nonwhite nbhood_bachelors nbhood_unempl, cluster(city_fips)
estimates store m3

quietly regress ln_realsaleprice ln_distance in_downtown ln_lotsize ln_sqft age total_rooms stories pool basement deck nbhood_age nbhood_nonwhite nbhood_bachelors nbhood_unempl msp_lag2, cluster(city_fips)
estimates store m4

quietly regress ln_realsaleprice in_downtown ln_lotsize ln_sqft age total_rooms stories pool basement deck nbhood_age nbhood_nonwhite nbhood_bachelors nbhood_unempl c.ln_distance##i.msp_lag2, cluster(city_fips)
estimates store m5

estout m1 m2 m3 m4 m5, drop(0.msp_lag2 _cons) ///
	cells(b(star fmt(2)) se(par fmt(1))) collabels(none) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stats(r2 N, fmt(3 0) label(R^2 N)) 


esttab m1 m2 m3 m4 m5 using "/Users/andrew/Documents/GitHub/mainstreet/hidden/datatree/cleaned/results/datatree3.html", drop(0.msp_lag2 0.msp_lag2#c.ln_distance _cons pool basement deck) legend varlabels(ln_distance "Downtown Distance" in_downtown "Property Located Downtown?" ln_lotsize "Lot Size (log)" ln_sqft "Square Footage (log)" age "Home Age" total_rooms "Total Rooms" stories "Stories" nbhood_age "Neighborhood Median Age" nbhood_nonwhite "Neighborhood Pct. Non-White" nbhood_bachelors "Neighborhood Pct. w/Bachelors+	" nbhood_unempl "Neighborhood Unemployment Rate" msp_lag2 "MSP Adopted?" 1.msp_lag2 "MSP Adopted?" 1.msp_lag2#c.ln_distance "Distance*MSP interaction") ///
	cells(b(star fmt(2)) se(par fmt(2))) collabels(none) mlabels(none) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace ///
	stats(r2 N, fmt(3 0) label("R<sup>2</sup>" N)) 

quietly regress ln_realsaleprice in_downtown ln_lotsize ln_sqft age total_rooms stories pool basement deck nbhood_age nbhood_nonwhite nbhood_bachelors nbhood_unempl c.ln_distance##i.msp_lag2 if distance <= 12, cluster(city_fips)
estimates store b1

quietly regress ln_realsaleprice in_downtown ln_lotsize ln_sqft age total_rooms stories pool basement deck nbhood_age nbhood_nonwhite nbhood_bachelors nbhood_unempl c.ln_distance##i.msp_lag2 if distance <= 8, cluster(city_fips)
estimates store b2

estout m5 b1 b2, keep(in_downtown ln_distance 1.msp_lag2 1.msp_lag2#c.ln_distance) ///
	cells(b(star fmt(2)) se(par fmt(1))) collabels(none) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stats(r2 N, fmt(3 0) label(R^2 N)) 
	
	
esttab m5 b1 b2 using "/Users/andrew/Documents/GitHub/mainstreet/hidden/datatree/cleaned/results/datatree5.html", keep(ln_distance in_downtown 1.msp_lag2 1.msp_lag2#c.ln_distance) legend varlabels(ln_distance "Downtown Distance" in_downtown "Property Located Downtown?" 1.msp_lag2 "MSP Adopted?" 1.msp_lag2#c.ln_distance "Distance*MSP interaction") ///
	cells(b(star fmt(2)) se(par fmt(2))) collabels(none) mlabels(none) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace ///
	stats(r2 N, fmt(3 0) label("R<sup>2</sup>" N)) 

quietly regress ln_realsaleprice in_downtown ln_lotsize ln_sqft age total_rooms stories pool basement deck nbhood_age nbhood_nonwhite nbhood_bachelors nbhood_unempl c.ln_distance##i.msp_at_sale, cluster(city_fips)
estimates store t0

quietly regress ln_realsaleprice in_downtown ln_lotsize ln_sqft age total_rooms stories pool basement deck nbhood_age nbhood_nonwhite nbhood_bachelors nbhood_unempl c.ln_distance##i.msp_lag1, cluster(city_fips)
estimates store t1

quietly regress ln_realsaleprice in_downtown ln_lotsize ln_sqft age total_rooms stories pool basement deck nbhood_age nbhood_nonwhite nbhood_bachelors nbhood_unempl c.ln_distance##i.msp_lag2, cluster(city_fips)
estimates store t2

quietly regress ln_realsaleprice in_downtown ln_lotsize ln_sqft age total_rooms stories pool basement deck nbhood_age nbhood_nonwhite nbhood_bachelors nbhood_unempl c.ln_distance##i.msp_lag3, cluster(city_fips)
estimates store t3

quietly regress ln_realsaleprice in_downtown ln_lotsize ln_sqft age total_rooms stories pool basement deck nbhood_age nbhood_nonwhite nbhood_bachelors nbhood_unempl c.ln_distance##i.msp_lag5, cluster(city_fips)
estimates store t5

estout t0 t1 t2 t3 t5, drop(_cons ln_lotsize ln_sqft age total_rooms stories pool basement deck nbhood_age nbhood_nonwhite nbhood_bachelors nbhood_unempl 0.msp_lag3 0.msp_lag5 0.msp_lag2 0.msp_lag1 0.msp_at_sale) ///
	cells(b(star fmt(2)) se(par fmt(1))) collabels(none) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stats(r2 N, fmt(3 0) label(R^2 N)) 

esttab t0 t1 t2 t3 t5 using "/Users/andrew/Documents/GitHub/mainstreet/hidden/datatree/cleaned/results/datatree4.html", keep(ln_distance in_downtown 1.msp_at_sale 1.msp_at_sale#c.ln_distance 1.msp_lag1 1.msp_lag1#c.ln_distance 1.msp_lag2 1.msp_lag2#c.ln_distance 1.msp_lag3 1.msp_lag3#c.ln_distance 1.msp_lag5 1.msp_lag5#c.ln_distance) legend varlabels(ln_distance "Downtown Distance" in_downtown "Property Located Downtown?" 1.msp_lag5 "MSP Adopted?" 1.msp_lag3 "MSP Adopted?" 1.msp_lag2 "MSP Adopted?" 1.msp_lag1 "MSP Adopted?" 1.msp_at_sale "MSP Adopted?" 1.msp_at_sale#c.ln_distance "Distance*MSP interaction" 1.msp_lag1#c.ln_distance "Distance*MSP interaction" 1.msp_lag2#c.ln_distance "Distance*MSP interaction" 1.msp_lag3#c.ln_distance "Distance*MSP interaction" 1.msp_lag5#c.ln_distance "Distance*MSP interaction") ///
	cells(b(star fmt(2)) se(par fmt(2))) collabels(none) mlabels(none) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) replace ///
	stats(r2 N, fmt(3 0) label("R<sup>2</sup>" N)) 













