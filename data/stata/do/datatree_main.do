use "/Users/andrew/Documents/GitHub/mainstreet/hidden/datatree/cleaned/datatree_model.dta"
reg ln_realsaleprice in_downtown ln_lotsize ln_sqft age total_rooms stories pool basement deck nbhood_age nbhood_nonwhite nbhood_bachelors nbhood_unempl c.ln_distance##msp_lag2 c.log_dist2##msp_lag2, noconstant
