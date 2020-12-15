*------------------------------------------------------------------------------*
*-----------Hourly pattern                            12/14/2010  -------------*
*------------------------------------------------------------------------------*
use "IL_coefficents_residential_order_event.dta", clear 
gen states = "IL" 
gen policy= "order"

append using "IL_coefficents_residential_school_event.dta"  
replace states = "IL" if states==""
replace policy = "school" if policy==""

append using "AZ_coefficents_residential_order_event.dta"
replace states = "AL" if states==""
replace policy = "order" if policy==""

append using "AZ_coefficents_residential_school_event.dta"
replace states = "AL" if states==""
replace policy = "school" if policy==""

replace beta=beta*100
replace se=se*100
replace lo=lo*100
replace hi=hi*100

gen shhourchange_adjusted1= shhourchange+0.11
gen shhourchange_adjusted2= shhourchange+0.22
gen shhourchange_adjusted3= shhourchange+0.33


twoway rcap lo hi shhourchange if states=="IL" & policy=="order", lcolor(navy) lwidth(medium) msize(*.5) || ///
rcap lo hi shhourchange_adjusted1 if states=="IL" & policy=="school", lcolor(navy) lwidth(medium) msize(*.5) || ///
rcap lo hi shhourchange_adjusted2 if states=="AL" & policy=="order", lcolor(orange) lwidth(medium) msize(*.5) || ///
rcap lo hi shhourchange_adjusted3 if states=="AL" & policy=="school", msize(*.3) xlabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12" 13 "13" 14 "14" 15 "15" 16 "16" 17 "17" 18 "18" 19 "19" 20 "20" 21 "21" 22 "22" 23 "23" 24 "24", ///
angle(horizontal) labsize(small)) lcolor(orange) lwidth(thin) ytitle("Percentage Change in hourly electricity consumption change(%)", size(small) margin(0 3 -5 0)) ylabel(-30 (10) 20, nogrid labsize(small)) plotregion(style(none)) graphregion(fcolor(white)) || ///
(scatter beta shhourchange if states=="IL" & policy=="order", connect(l) lpattern(shortdash) lwidth(medthick) lcolor(navy) ms(o) msize(medium) mcolor(navy) xtitle("Hour-of-day") yline(0,lcolor(black) lpattern(dash))) ///
(scatter beta shhourchange_adjusted1 if states=="IL" & policy=="school", connect(l) lwidth(medthick) lcolor(navy)  ms(o) msize(medium) mcolor(navy)) ///
(scatter beta shhourchange_adjusted2 if states=="AL" & policy=="order", connect(l)  lpattern(shortdash) lwidth(medthick) lcolor(orange)  ms(o) msize(medium) mcolor(orange)) ///
(scatter beta shhourchange_adjusted3 if states=="AL" & policy=="school", connect(l) lwidth(medthick) lcolor(orange)  ms(o) msize(medium) mcolor(orange)), ///
legend( order( 5 "IL:Order " 6 "IL:School" 7 "AZ: Order"  8 "AZ: School")   cols(4)  region(lcolor(white)) textwidth(28) symxsize(*.4))
graph export "resiential_hour.eps", replace as(eps)
