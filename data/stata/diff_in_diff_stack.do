clear
use "/Users/andrew/Documents/GitHub/mainstreet/data/stata/msp_stack.dta"
label variable cz_yr "cz_yr"
xtset id
xtreg jobs treated post treated#post i.cz_yr, fe
est table, keep(treated post treated#post) b se
