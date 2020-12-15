*----------------------------------------------------------------------------------*
*-----------------(RD- two steps  whole residential data	10/5/2020 -------------*
*----------------------------------------------------------------------------------*
ssc install rdrobust
*use "/Volumes/TOSHIBA/Lucy/IL/Analysis/Stata/thirdcut/test/1920didpoint1_step1C2301p.dta", clear
************************************************************************************
*----------------------------------------------------------------------------------*
*-----------   STEP ONE: ONLY ESTIMATE SCHOOL EFFECTS              ----------------*
*----------------------------------------------------------------------------------*
************************************************************************************

*use "/Volumes/TOSHIBA/Lucy/IL/Analysis/Stata/test/residentialsocial_1p.dta", clear


tempfile c26step1file
use "newdata/2020residential.dta", clear 
drop if year==.
mkspline tem_sp = tem , cubic displayknots

replace preci=0 if preci==.
drop precisq
gen precisq=preci^2

sort accountid daily_date hour
egen timeid=group(daily_date hour)
destring  accountid, replace
xtset accountid timeid

gen dayoftreat=22 
gen timeToTreat = day - dayoftreat

gen dayoftreat1=17
gen timeToTreat1 = day - dayoftreat1

gen order1=0
replace order1=daily_date>=mdy(03,17,2020)

**** Specifiy Policy Date ****

gen order=0
replace order=daily_date>=mdy(03,22,2020)

**** Generate Polynomial ****
gen poly =daily_date-mdy(03,22,2020)
gen polya= poly*(1-order)
gen polyb= poly*order
orthpoly poly, generate(poly*) deg(8) poly(P)


gen polynew =daily_date-mdy(03,17,2020)
gen polynewa= polynew*(1-order1)
gen polynewb= polynew*order1
orthpoly polynew, generate(polynew*) deg(8) poly(P)


**** Residuals ****
xtreg logece tem_sp1-tem_sp4 preci precisq pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
predict residuals, resid

save `c26step1file', replace

*----------------------------------------------------------------------------------*
*-----------off-peak   08/4/2020---------------------------------------------------*
*----------------------------------------------------------------------------------*
clear
use `c26step1file'

keep if inlist(hour,23,24,0,1,2,3,4,5,6)

******************************** two steps AT ***********************************
reg residuals order1 if  timeToTreat1 >= -4 & timeToTreat1 <=4
reg residuals order1 if  timeToTreat1 >= -5 & timeToTreat1 <=5
reg residuals order1 if  timeToTreat1 >= -10 & timeToTreat1 <=10
reg residuals order1 if  timeToTreat1 >= -16 & timeToTreat1 <=4

reg residuals order1 
xtreg logece order1 tem_sp1-tem_sp4 preci precisq pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********global estimation*********
xtreg logece order1 polynew1-polynew3 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew4 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew5 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew6 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew8 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)

********local estimation*********
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat1 >= -4 & timeToTreat1 <=4
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat1 >= -16 & timeToTreat1 <=4

********global estimation residuals *********
xtreg residuals order1 polynew1-polynew3, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew4, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew5, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew6, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew7, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew8, fe vce(cluster accountid)

******* local estimation residuals *********
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid)
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid), if  timeToTreat1 >= -4 & timeToTreat1 <=4
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid), if  timeToTreat1 >= -16 & timeToTreat1 <=4

******* aggregation to daily ***************

collapse (mean) electricusage order order1 residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order1  = 0 if order1 < 1

****  Build Polynomial ****
regress residuals order1 polynew1-polynew3
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

**** global graphic 3rd****
graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_global3rd_school.eps", replace as(eps)


**** global graphic 4th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew4
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_global4th_school.eps", replace as(eps)

**** global graphic 5th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew5
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_global5th_school.eps", replace as(eps)


**** global graphic 6th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew6
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_global6th_school.eps", replace as(eps)

*** local *******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<17, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=17, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline( 17, lcolor(navy) lpattern(dash)) legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Offpeak, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/offpeak_localonestages_school.eps", replace as(eps)


**** local linear****
drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_locallinear_school.eps", replace as(eps)


**** local linear 16-4 days ****
keep if polynew >= -16 & polynew <=4

drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_locallinear_school16_4.eps", replace as(eps)

**** local linear 4 days ****
keep if polynew >= -4 & polynew <=4

drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(12 "-5" 16.7 "0" 22"+5"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_locallinear_school4.eps", replace as(eps)

*----------------------------------------------------------------------------------*
*-----------daypeak   08/4/2020----------------------------------------------------*
*----------------------------------------------------------------------------------*
clear
use `c26step1file'
keep if inlist(hour,7,8,9,10,11,12,13,14)

******************************** two steps AT ***********************************
reg residuals order1 if  timeToTreat1 >= -4 & timeToTreat1 <=4
reg residuals order1 if  timeToTreat1 >= -5 & timeToTreat1 <=5
reg residuals order1 if  timeToTreat1 >= -10 & timeToTreat1 <=10
reg residuals order1 if  timeToTreat1 >= -16 & timeToTreat1 <=4

reg residuals order1 
xtreg logece order1 tem_sp1-tem_sp4 preci precisq pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********global estimation*********
xtreg logece order1 polynew1-polynew3 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew4 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew5 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew6 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew8 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)

********local estimation*********
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat1 >= -4 & timeToTreat1 <=4
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat1 >= -16 & timeToTreat1 <=4

********global estimation residuals *********
xtreg residuals order1 polynew1-polynew3, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew4, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew5, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew6, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew7, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew8, fe vce(cluster accountid)

******* local estimation residuals *********
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid)
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid), if  timeToTreat1 >= -4 & timeToTreat1 <=4
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid), if  timeToTreat1 >= -16 & timeToTreat1 <=4

******* aggregation to daily ***************

collapse (mean) electricusage order order1 residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order1  = 0 if order1 < 1

****  Build Polynomial ****
regress residuals order1 polynew1-polynew3
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

**** global graphic 3rd****
graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_global3rh_school.eps", replace as(eps)

**** global graphic 4th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew4
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_global4th_school.eps", replace as(eps)

**** global graphic 5th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew5
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_global5th_school.eps", replace as(eps)


**** global graphic 6th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew6
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_global6th_school.eps", replace as(eps)

*** local *******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<17, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=17, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline( 17, lcolor(navy) lpattern(dash)) legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Day peak, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/daypeak_localonestages_school.eps", replace as(eps)


**** local linear****
drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_locallinear_school.eps", replace as(eps)

**** local linear 16-4 days ****
keep if polynew >= -16 & polynew <=4

drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_locallinear_school16_4.eps", replace as(eps)

**** local linear 4 days ****
keep if polynew >= -4 & polynew <=4

drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(12 "-5" 16.7 "0" 22"+5"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_locallinear_school4.eps", replace as(eps)


*----------------------------------------------------------------------------------*
*-----------evening  peak   08/2/2020----------------------------------------------*
*----------------------------------------------------------------------------------*
clear
use `c26step1file'
keep if inlist(hour,20,21,22)

******************************** two steps AT ***********************************
reg residuals order1 if  timeToTreat1 >= -4 & timeToTreat1 <=4
reg residuals order1 if  timeToTreat1 >= -5 & timeToTreat1 <=5
reg residuals order1 if  timeToTreat1 >= -10 & timeToTreat1 <=10
reg residuals order1 if  timeToTreat1 >= -16 & timeToTreat1 <=4

reg residuals order1 
xtreg logece order1 tem_sp1-tem_sp4 preci precisq pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********global estimation*********
xtreg logece order1 polynew1-polynew3 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew4 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew5 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew6 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)

********local estimation*********
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat1 >= -4 & timeToTreat1 <=4
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat1 >= -16 & timeToTreat1 <=4

********global estimation residuals *********
xtreg residuals order1 polynew1-polynew3, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew4, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew5, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew6, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew7, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew8, fe vce(cluster accountid)

******* local estimation residuals *********
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid)
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid), if  timeToTreat1 >= -4 & timeToTreat1 <=4
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid), if  timeToTreat1 >= -16 & timeToTreat1 <=4

******* aggregation to daily ***************

collapse (mean) electricusage order order1 residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order1  = 0 if order1 < 1

****  Build Polynomial ****
regress residuals order1 polynew1-polynew3
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

**** global graphic 3rd****
graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_global3rd_school.eps", replace as(eps)

**** global graphic 4th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew4
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_global4th_school.eps", replace as(eps)

**** global graphic 5th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew5
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_global5th_school.eps", replace as(eps)


**** global graphic 6th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew6
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_global6th_school.eps", replace as(eps)


*** local *******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<17, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=17, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline( 17, lcolor(navy) lpattern(dash)) legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Evening peak, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/eveningpeak_localonestages_school.eps", replace as(eps)

**** local linear****
drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_locallinear_school.eps", replace as(eps)


**** local linear 16-4 days ****
keep if polynew >= -16 & polynew <=4

drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" ))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_locallinear_school16_4.eps", replace as(eps)

**** local linear 4 days ****
keep if polynew >= -4 & polynew <=4

drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(12 "-5" 16.7 "0" 22"+5"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_locallinear_school4.eps", replace as(eps)

*----------------------------------------------------------------------------------*
*-----------Super  peak   08/4/2020------------------------------------------------*
*----------------------------------------------------------------------------------*
clear
use `c26step1file'
keep if inlist(hour,15,16,17,18,19)

******************************** two steps AT ***********************************
reg residuals order1 if  timeToTreat1 >= -4 & timeToTreat1 <=4
reg residuals order1 if  timeToTreat1 >= -5 & timeToTreat1 <=5
reg residuals order1 if  timeToTreat1 >= -10 & timeToTreat1 <=10
reg residuals order1 if  timeToTreat1 >= -16 & timeToTreat1 <=4
reg residuals order1 
xtreg logece order1 tem_sp1-tem_sp4 preci precisq pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********global estimation*********
xtreg logece order1 polynew1-polynew3 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew4 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew5 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew6 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)

********local estimation*********
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat1 >= -4 & timeToTreat1 <=4
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat1 >= -16 & timeToTreat1 <=4

********global estimation residuals *********
xtreg residuals order1 polynew1-polynew3, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew4, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew5, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew6, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew7, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew8, fe vce(cluster accountid)

******* local estimation residuals *********
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid)
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid), if  timeToTreat1 >= -4 & timeToTreat1 <=4
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid), if  timeToTreat1 >= -16 & timeToTreat1 <=4

******* aggregation to daily ***************

collapse (mean) electricusage order order1 residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order1  = 0 if order1 < 1

****  Build Polynomial ****
regress residuals order1 polynew1-polynew3
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

**** global graphic 3rd****
graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_3rd.eps_school", replace as(eps)

**** global graphic 4th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew4
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_global4th_school.eps", replace as(eps)

**** global graphic 5th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew5
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_global5th_school.eps", replace as(eps)


**** global graphic 6th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew6
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_global6th_school.eps", replace as(eps)


*** local *******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<17, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=17, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline( 17, lcolor(navy) lpattern(dash)) legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Super peak, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/superpeak_localonestages_school.eps", replace as(eps)

**** local linear****
drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_locallinear_school.eps", replace as(eps)

**** local linear 16-4 days ****
keep if polynew >= -16 & polynew <=4

drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_locallinear_school16_4.eps", replace as(eps)

**** local linear 4 days ****
keep if polynew >= -4 & polynew <=4

drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(12 "-5" 16.7 "0" 22"+5"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_locallinear_school4.eps", replace as(eps)
*----------------------------------------------------------------------------------*
*-----------Daily   08/4/2020------------------------------------------------------*
*----------------------------------------------------------------------------------*
clear
use `c26step1file'

******************************** two steps AT ***********************************
reg residuals order1 if  timeToTreat1 >= -4 & timeToTreat1 <=4
reg residuals order1 if  timeToTreat1 >= -5 & timeToTreat1 <=5
reg residuals order1 if  timeToTreat1 >= -10 & timeToTreat1 <=10
reg residuals order1 if  timeToTreat1 >= -16 & timeToTreat1 <=4

reg residuals order1 
xtreg logece order1 tem_sp1-tem_sp4 preci precisq pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********global estimation*********
xtreg logece order1 polynew1-polynew3 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew4 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew5 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew6 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynew1-polynew7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)

********local estimation*********
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat1 >= -4 & timeToTreat1 <=4
xtreg logece order1 polynewa polynewb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat1 >= -16 & timeToTreat1 <=4

********global estimation residuals *********
xtreg residuals order1 polynew1-polynew3, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew4, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew5, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew6, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew7, fe vce(cluster accountid)
xtreg residuals order1 polynew1-polynew8, fe vce(cluster accountid)

******* local estimation residuals *********
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid)
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid), if  timeToTreat1 >= -4 & timeToTreat1 <=4
xtreg residuals order1 polynewa polynewb, fe vce(cluster accountid), if  timeToTreat1 >= -16 & timeToTreat1 <=4

******* aggregation to daily ***************

collapse (mean) electricusage order order1 residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order1  = 0 if order1 < 1


drop  poly*
gen polynew =daily_date-mdy(03,17,2020)
gen polynewa= polynew*(1-order1)
gen polynewb= polynew*order1
orthpoly polynew, generate(polynew*) deg(8) poly(P)

****  Build Polynomial ****
regress residuals order1 polynew1-polynew3
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

**** global graphic 3rd****
graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_global3rd_school.eps", replace as(eps)


**** global graphic 4th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew4
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_global4th_school.eps", replace as(eps)

**** global graphic 5th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew5
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_global5th_school.eps", replace as(eps)


**** global graphic 6th***
drop line regline line1 line2 
regress residuals order1 polynew1-polynew6
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_global6th_school.eps", replace as(eps)


*** local *******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<17, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=17, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline( 17, lcolor(navy) lpattern(dash)) legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Daily, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/daily_localonestages_school.eps", replace as(eps)


**** local linear****
drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" 27 "+10" 31 "+14"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_locallinear_school.eps", replace as(eps)


**** local linear 16-4 days ****
keep if polynew >= -16 & polynew <=4

drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(0 "-17" 2 "-15" 7 "-10" 12 "-5" 16.7 "0" 22"+5" ))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_locallinear_school16_4.eps", replace as(eps)

**** local linear 4 days ****
keep if polynew >= -4 & polynew <=4

drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(16.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 12 "-5" 16.7 "0" 22"+5" ))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_locallinear_school4.eps", replace as(eps)

************************************************************************************
*----------------------------------------------------------------------------------*
*-----------      STEP TWO: ESTIMATE ORDER EFFECT    ------------------------------*
*----------------------------------------------------------------------------------*
************************************************************************************

tempfile c26step2file
use "newdata/2020residential.dta", clear 
drop if year==.

mkspline tem_sp = tem , cubic displayknots

replace preci=0 if preci==.
drop precisq
gen precisq=preci^2

sort accountid daily_date hour
egen timeid=group(daily_date hour)
destring  accountid, replace
xtset accountid timeid

gen dayoftreat=22 
gen timeToTreat = day - dayoftreat

**** Specifiy Policy Date ****

gen order=0
replace order=daily_date>=mdy(03,22,2020)

**** Generate Polynomial ****
gen poly =daily_date-mdy(03,22,2020)
gen polya= poly*(1-order)
gen polyb= poly*order
orthpoly poly, generate(poly*) deg(8) poly(P)

**** Residuals ****
xtreg logece  tem_sp1-tem_sp4 preci precisq pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
predict residuals, resid

save `c26step2file', replace

*----------------------------------------------------------------------------------*
*-----------off-peak   08/4/2020---------------------------------------------------*
*----------------------------------------------------------------------------------*
clear
use `c26step2file'

keep if inlist(hour,23,24,0,1,2,3,4,5,6)

******************************** two steps AT ***********************************
reg residuals order if  timeToTreat >= -1 & timeToTreat <= 1
reg residuals order if  timeToTreat >= -2 & timeToTreat <= 2
reg residuals order if  timeToTreat >= -3 & timeToTreat <= 3
reg residuals order if  timeToTreat >= -4 & timeToTreat <= 4
reg residuals order if  timeToTreat >= -5 & timeToTreat <= 5
reg residuals order if  timeToTreat >= -5 & timeToTreat <= 9

reg residuals order 
xtreg logece order tem_sp1-tem_sp4 preci precisq pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********global estimation*********
xtreg logece order poly1-poly3 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly4 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly5 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly6 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly8 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********local estimation*********
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 5
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 9

********global estimation residuals *********
xtreg residuals order poly1-poly3, fe vce(cluster accountid)
xtreg residuals order poly1-poly4, fe vce(cluster accountid)
xtreg residuals order poly1-poly5, fe vce(cluster accountid)
xtreg residuals order poly1-poly6, fe vce(cluster accountid)
xtreg residuals order poly1-poly7, fe vce(cluster accountid)
xtreg residuals order poly1-poly8, fe vce(cluster accountid)

******* local estimation residuals *********
xtreg residuals order polya polyb, fe vce(cluster accountid)
xtreg residuals order polya polyb, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 5
xtreg residuals order polya polyb, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 9

******* aggregation to daily ***************

collapse (mean) electricusage order residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order  = 0 if order < 1

****  Build Polynomial ****
regress residuals order poly1-poly3
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

**** global graphic 3rd****
graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_global3rd_order.eps", replace as(eps)


**** global graphic 4th***
drop line regline line1 line2 
regress residuals order poly1-poly4
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_global4th_order.eps", replace as(eps)

**** global graphic 5th***
drop line regline line1 line2 
regress residuals order poly1-poly5
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_global5th_order.eps", replace as(eps)


**** global graphic 6th***
drop line regline line1 line2 
regress residuals order poly1-poly6
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_global6th_order.eps", replace as(eps)

*** local*******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<22, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=22, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline(22, lcolor(navy) lpattern(dash)) legend(off) xlabel(0(1)31 ,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Offpeak, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/offpeak_localonestages_order.eps", replace as(eps)

**** local linear****
drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_locallinear_order.eps", replace as(eps)


**** local linear 16-4 days ****
keep if poly >= -5 & poly <=9

drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_locallinear5_9_order.eps", replace as(eps)

**** local linear 4 days ****
keep if poly >= -5 & poly<=5
drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/offpeak_locallinear5_5_order.eps", replace as(eps)

*----------------------------------------------------------------------------------*
*-----------daypeak   08/4/2020----------------------------------------------------*
*----------------------------------------------------------------------------------*
clear
use `c26step2file'
keep if inlist(hour,7,8,9,10,11,12,13,14)

******************************** two steps AT ***********************************
reg residuals order if  timeToTreat >= -1 & timeToTreat <= 1
reg residuals order if  timeToTreat >= -2 & timeToTreat <= 2
reg residuals order if  timeToTreat >= -3 & timeToTreat <= 3
reg residuals order if  timeToTreat >= -4 & timeToTreat <= 4
reg residuals order if  timeToTreat >= -5 & timeToTreat <= 5
reg residuals order if  timeToTreat >= -5 & timeToTreat <= 9

reg residuals order 
xtreg logece order tem_sp1-tem_sp4 preci precisq pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********global estimation*********
xtreg logece order poly1-poly3 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly4 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly5 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly6 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly8 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********local estimation*********
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 5
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 9

********global estimation residuals *********
xtreg residuals order poly1-poly3, fe vce(cluster accountid)
xtreg residuals order poly1-poly4, fe vce(cluster accountid)
xtreg residuals order poly1-poly5, fe vce(cluster accountid)
xtreg residuals order poly1-poly6, fe vce(cluster accountid)
xtreg residuals order poly1-poly7, fe vce(cluster accountid)
xtreg residuals order poly1-poly8, fe vce(cluster accountid)

******* local estimation residuals *********
xtreg residuals order polya polyb, fe vce(cluster accountid)
xtreg residuals order polya polyb, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 5
xtreg residuals order polya polyb, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 9

******* aggregation to daily ***************

collapse (mean) electricusage order residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order  = 0 if order < 1

****  Build Polynomial ****
regress residuals order poly1-poly3
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

**** global graphic 3rd****
graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_global3rd_order.eps", replace as(eps)

**** global graphic 4th***
drop line regline line1 line2 
regress residuals order poly1-poly4
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_global4th_order.eps", replace as(eps)

**** global graphic 5th***
drop line regline line1 line2 
regress residuals order poly1-poly5
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_global5th_order.eps", replace as(eps)


**** global graphic 6th***
drop line regline line1 line2 
regress residuals order poly1-poly6
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_global6th_order.eps", replace as(eps)


*** local*******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<22, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=22, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline(22, lcolor(navy) lpattern(dash)) legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Day peak, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/daypeak_localonestages_order.eps", replace as(eps)

**** local linear****
drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_locallinear_order.eps", replace as(eps)


**** local linear 16-4 days ****
keep if poly >= -5 & poly <=9

drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_locallinear5_9_order.eps", replace as(eps)

**** local linear 4 days ****
keep if poly >= -5 & poly<=5
drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daypeak_locallinear5_5_order.eps", replace as(eps)

*----------------------------------------------------------------------------------*
*-----------evening  peak   08/2/2020----------------------------------------------*
*----------------------------------------------------------------------------------*
clear
use `c26step2file'
keep if inlist(hour,20,21,22)

******************************** two steps AT ***********************************
reg residuals order if  timeToTreat >= -1 & timeToTreat <= 1
reg residuals order if  timeToTreat >= -2 & timeToTreat <= 2
reg residuals order if  timeToTreat >= -3 & timeToTreat <= 3
reg residuals order if  timeToTreat >= -4 & timeToTreat <= 4
reg residuals order if  timeToTreat >= -5 & timeToTreat <= 5
reg residuals order if  timeToTreat >= -5 & timeToTreat <= 9

reg residuals order 
xtreg logece order tem_sp1-tem_sp4 preci precisq pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********global estimation*********
xtreg logece order poly1-poly3 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly4 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly5 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly6 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly8 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********local estimation*********
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 5
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 9

********global estimation residuals *********
xtreg residuals order poly1-poly3, fe vce(cluster accountid)
xtreg residuals order poly1-poly4, fe vce(cluster accountid)
xtreg residuals order poly1-poly5, fe vce(cluster accountid)
xtreg residuals order poly1-poly6, fe vce(cluster accountid)
xtreg residuals order poly1-poly7, fe vce(cluster accountid)
xtreg residuals order poly1-poly8, fe vce(cluster accountid)

******* local estimation residuals *********
xtreg residuals order polya polyb, fe vce(cluster accountid)
xtreg residuals order polya polyb, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 5
xtreg residuals order polya polyb, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 9

******* aggregation to daily ***************

collapse (mean) electricusage order residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order  = 0 if order < 1

****  Build Polynomial ****
regress residuals order poly1-poly3
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

**** global graphic 3rd****
graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_global3rd_order.eps", replace as(eps)

**** global graphic 4th***
drop line regline line1 line2 
regress residuals order poly1-poly4
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_global4th_order.eps", replace as(eps)

**** global graphic 5th***
drop line regline line1 line2 
regress residuals order poly1-poly5
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_global5th_order.eps", replace as(eps)


**** global graphic 6th***
drop line regline line1 line2 
regress residuals order poly1-poly6
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_global6th_order.eps", replace as(eps)


*** local*******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<22, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=22, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline(22, lcolor(navy) lpattern(dash)) legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Evening peak, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/eveningpeak_localonestages_order.eps", replace as(eps)

**** local linear****
drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_locallinear_order.eps", replace as(eps)


**** local linear 16-4 days ****
keep if poly >= -5 & poly <=9

drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_locallinear5_9_order.eps", replace as(eps)

**** local linear 4 days ****
keep if poly >= -5 & poly<=5
drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/eveningpeak_locallinear5_5_order.eps", replace as(eps)

*----------------------------------------------------------------------------------*
*-----------Super  peak   08/4/2020------------------------------------------------*
*----------------------------------------------------------------------------------*
clear
use `c26step2file'
keep if inlist(hour,15,16,17,18,19)

******************************** two steps AT ***********************************
reg residuals order if  timeToTreat >= -1 & timeToTreat <= 1
reg residuals order if  timeToTreat >= -2 & timeToTreat <= 2
reg residuals order if  timeToTreat >= -3 & timeToTreat <= 3
reg residuals order if  timeToTreat >= -4 & timeToTreat <= 4
reg residuals order if  timeToTreat >= -5 & timeToTreat <= 5
reg residuals order if  timeToTreat >= -5 & timeToTreat <= 9

reg residuals order 
xtreg logece order tem_sp1-tem_sp4 preci precisq pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********global estimation*********
xtreg logece order poly1-poly3 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly4 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly5 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly6 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly8 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********local estimation*********
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 5
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 9

********global estimation residuals *********
xtreg residuals order poly1-poly3, fe vce(cluster accountid)
xtreg residuals order poly1-poly4, fe vce(cluster accountid)
xtreg residuals order poly1-poly5, fe vce(cluster accountid)
xtreg residuals order poly1-poly6, fe vce(cluster accountid)
xtreg residuals order poly1-poly7, fe vce(cluster accountid)
xtreg residuals order poly1-poly8, fe vce(cluster accountid)

******* local estimation residuals *********
xtreg residuals order polya polyb, fe vce(cluster accountid)
xtreg residuals order polya polyb, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 5
xtreg residuals order polya polyb, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 9

******* aggregation to daily ***************

collapse (mean) electricusage order residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order  = 0 if order < 1

****  Build Polynomial ****
regress residuals order poly1-poly3
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

**** global graphic 3rd****
graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_global3rd_order.eps", replace as(eps)

**** global graphic 4th***
drop line regline line1 line2 
regress residuals order poly1-poly4
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_global4th_order.eps", replace as(eps)

**** global graphic 5th***
drop line regline line1 line2 
regress residuals order poly1-poly5
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_global5th_order.eps", replace as(eps)


**** global graphic 6th***
drop line regline line1 line2 
regress residuals order poly1-poly6
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_global6th_order.eps", replace as(eps)

*** local*******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<22, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=22, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline(22, lcolor(navy) lpattern(dash)) legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Super peak, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/superpeak_localonestages_order.eps", replace as(eps)

**** local linear****
drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_locallinear_order.eps", replace as(eps)


**** local linear 16-4 days ****
keep if poly >= -5 & poly <=9

drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_locallinear5_9_order.eps", replace as(eps)

**** local linear 4 days ****
keep if poly >= -5 & poly<=5
drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/superpeak_locallinear5_5_order.eps", replace as(eps)

*----------------------------------------------------------------------------------*
*-----------Daily   08/4/2020------------------------------------------------------*
*----------------------------------------------------------------------------------*
clear
use `c26step2file'

******************************** two steps AT ***********************************
reg residuals order if  timeToTreat >= -1 & timeToTreat <= 1
reg residuals order if  timeToTreat >= -2 & timeToTreat <= 2
reg residuals order if  timeToTreat >= -3 & timeToTreat <= 3
reg residuals order if  timeToTreat >= -4 & timeToTreat <= 4
reg residuals order if  timeToTreat >= -5 & timeToTreat <= 5
reg residuals order if  timeToTreat >= -5 & timeToTreat <= 9

reg residuals order 
xtreg logece order tem_sp1-tem_sp4 preci precisq pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********global estimation*********
xtreg logece order poly1-poly3 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly4 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly5 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly6 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly7 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 
xtreg logece order poly1-poly8 tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid) 

********local estimation*********
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid)
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 5
xtreg logece order polya polyb tem_sp1-tem_sp4 preci* pressure wind humidity i.week_days i.hour, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 9

********global estimation residuals *********
xtreg residuals order poly1-poly3, fe vce(cluster accountid)
xtreg residuals order poly1-poly4, fe vce(cluster accountid)
xtreg residuals order poly1-poly5, fe vce(cluster accountid)
xtreg residuals order poly1-poly6, fe vce(cluster accountid)
xtreg residuals order poly1-poly7, fe vce(cluster accountid)
xtreg residuals order poly1-poly8, fe vce(cluster accountid)

******* local estimation residuals *********
xtreg residuals order polya polyb, fe vce(cluster accountid)
xtreg residuals order polya polyb, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 5
xtreg residuals order polya polyb, fe vce(cluster accountid), if  timeToTreat >= -5 & timeToTreat <= 9

******* aggregation to daily ***************

collapse (mean) electricusage order residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order  = 0 if order < 1

****  Build Polynomial ****
regress residuals order poly1-poly3
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

**** global graphic 3rd****
graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_global3rd_order.eps", replace as(eps)

**** global graphic 4th***
drop line regline line1 line2 
regress residuals order poly1-poly4
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_global4th_order.eps", replace as(eps)

**** global graphic 5th***
drop line regline line1 line2 
regress residuals order poly1-poly5
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_global5th_order.eps", replace as(eps)


**** global graphic 6th***
drop line regline line1 line2 
regress residuals order poly1-poly6
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order ==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(2 "-20" 7 "-15" 12 "-10" 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_global6th_order.eps", replace as(eps)

*** local*******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<22, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=22, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline(22, lcolor(navy) lpattern(dash)) legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Daily, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/daily_localonestages_order.eps", replace as(eps)

**** local linear****
drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_locallinear_order.eps", replace as(eps)


**** local linear 16-4 days ****
keep if poly >= -5 & poly <=9

drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_locallinear5_9_order.eps", replace as(eps)

**** local linear 4 days ****
keep if poly >= -5 & poly<=5
drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals day, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21.7,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 17 "-5" 21.7 "0" 27 "+5" 31 "+9"))  ///
(line line1 day, lcolor(black) lpattern(solid)) ///
(line line2 day, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/residential/daily_locallinear5_5_order.eps", replace as(eps)



******* aggregation to daily ***************
clear
use `c26step2file'


collapse (mean) electricusage order residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order  = 0 if order < 1


*** local *******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<=17, bw(2)  kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=17 & day<22, bw(2) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=22, bw(2) kernel(gau)   lwidth(thin) lcolor(gs6)), ///
xline( 17 22, lwidth(thin) lcolor(navy) lpattern(dash))legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Offpeak, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/offpeak_localtwostages.eps", replace as(eps)


***************************************************************************************
*-----------daypeak   08/4/2020------------------------------------------------*
***************************************************************************************
clear
use `c26step2file'
keep if inlist(hour,7,8,9,10,11,12,13,14)

******* aggregation to daily ***************

collapse (mean) electricusage order residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order  = 0 if order < 1


*** local*******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<=17, bw(2)  kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=17 & day<22, bw(2) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=22, bw(2) kernel(gau)   lwidth(thin) lcolor(gs6)), ///
xline( 17 22, lwidth(thin) lcolor(navy) lpattern(dash))legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Day peak, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/daypeak_localtwostages.eps", replace as(eps)

***************************************************************************************
*-----------evening  peak   08/2/2020------------------------------------------------*
***************************************************************************************
clear
use `c26step2file'
keep if inlist(hour,20,21,22)

collapse (mean) electricusage order residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order  = 0 if order < 1

set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<=17, bw(2)  kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=17 & day<22, bw(2) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=22, bw(2) kernel(gau)   lwidth(thin) lcolor(gs6)), ///
xline( 17 22, lwidth(thin) lcolor(navy) lpattern(dash))legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Evening peak, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/eveningpeak_localtwostages.eps", replace as(eps)


***************************************************************************************
*-----------Super  peak   08/4/2020---------------------------------------------------*
***************************************************************************************
clear
use `c26step2file'
keep if inlist(hour,20,21,22)

******* aggregation to daily ***************

collapse (mean) electricusage order residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order  = 0 if order < 1

*** local*******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<=17, bw(2)  kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=17 & day<22, bw(2) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=22, bw(2) kernel(gau)   lwidth(thin) lcolor(gs6)), ///
xline( 17 22, lwidth(thin) lcolor(navy) lpattern(dash))legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Super peak, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/superpeak_localtwostages.eps", replace as(eps)


***************************************************************************************
*-----------Daily   08/4/2020---------------------------------------------------*
***************************************************************************************
clear
use `c26step2file'

******* aggregation to daily ***************

collapse (mean) electricusage order residuals day week_days daily_date  hdd* preci* pressure wind humidity poly* hour*, by(date)
gen logece=log(electricusage)
replace order  = 0 if order < 1

*** local*******
set scheme s1mono
twoway (scatter residuals day, msymbol(smcircle_hollow)) ///
(lpoly residuals day if day<=17, bw(2)  kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=17 & day<22, bw(2) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals day if day>=22, bw(2) kernel(gau)   lwidth(thin) lcolor(gs6)), ///
xline( 17 22, lwidth(thin) lcolor(navy) lpattern(dash))legend(off) xlabel(0(1)31,labsize(small)) ///
title(Residential, size(medlarge) color(black)) subtitle(Daily, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/residential/daily_localtwostages.eps", replace as(eps)
