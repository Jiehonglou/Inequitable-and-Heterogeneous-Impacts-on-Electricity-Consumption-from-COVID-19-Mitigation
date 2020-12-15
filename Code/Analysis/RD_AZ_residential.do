*use "/Volumes/TOSHIBA/Lucy/AZ/Raw/Residential/residentialheat_1p.dta", clear
*drop __000000

*----------------------------------------------------------------------------------*
*-----------Reduced ID  (Entire)  09/28/2020	                     --------------*
*-----------regress residuals on time trend                     	 --------------*
*----------------------------------------------------------------------------------*

use "social/residentialheatreduced.dta", clear

tempfile residentialorderwhole2020

**** spline 
mkspline tem_sp = tem , cubic displayknots

*keep if VHOMEHEAT=="Gas"
*keep if VHOMEHEAT=="Electricity"


replace preci=0 if preci==.
gen precisq=preci^2

keep if inlist(month, 1,2,3,4)
*holiday 
gen byte holiday = 0
replace holiday = 1 if year==2019 & month==1 & day==1
replace holiday = 1 if year==2019 & month==1 & day==21
replace holiday = 1 if year==2019 & month==2 & day==18
replace holiday = 1 if year==2019 & month==5 & day==27
replace holiday = 1 if year==2019 & month==7 & day==4
replace holiday = 1 if year==2019 & month==7 & day==5
replace holiday = 1 if year==2019 & month==9 & day==2
replace holiday = 1 if year==2019 & month==10 & day==14
replace holiday = 1 if year==2019 & month==11 & day==11
replace holiday = 1 if year==2019 & month==11 & day==27
replace holiday = 1 if year==2019 & month==11 & day==28
replace holiday = 1 if year==2019 & month==12 & day==24
replace holiday = 1 if year==2019 & month==12 & day==25
replace holiday = 1 if year==2019 & month==12 & day==31

replace holiday = 1 if year==2020 & month==1 & day==1
replace holiday = 1 if year==2020 & month==1 & day==20
replace holiday = 1 if year==2020 & month==2 & day==17

drop if BILACCT_K==996300001
drop if BILACCT_K==128965004
drop if BILACCT_K==147260003

**** regenerate year, month and day 
drop year month day 

gen date1=dofc(date)
gen daily_date=dofc(date)
format daily_date %td


generate year=year(date1)
generate month=month(date1)
generate day=day(date1)

*Convert to clock then to date
gen week_days = dow(daily_date)
gen weekd=0
replace weekd=1 if week_days==1|week_days==2|week_days==3|week_days==4|week_days==5
destring hour, replace

*-----------------treatment date                   8/31/2020	     --------------*

* stay-at-home dummy (April 1)
gen order = (date1>=22006) & !missing(day)
*school close (March 16)
gen order1 =(date1>=21990) & !missing(day)

gen dayoftreat= 22006
gen timeToTreat = date1 - dayoftreat
gen dayoftreat1= 21990
gen timeToTreat1 = date1 - dayoftreat1

tabulate hour, gen(ihour) 

foreach n of numlist 1/24{
gen order_hour`n' = ihour`n' *order
}

foreach n of numlist 1/24{
gen order1_hour`n' = ihour`n' *order1
}

gen week=week_days+1

gen week2 = week(date1)
replace week2=week2+52 if year==2020

gen loghe=log(he)

sort BILACCT_K date1
by BILACCT_K date1: egen temmax=max(tem)
by BILACCT_K date1: egen temmin=min(tem)
gen hdd=65-(temmax+temmin)/2
gen cdd=(temmax+temmin)/2 -65
replace hdd=0 if  hdd<=0
replace cdd=0 if  cdd<=0
gen hddsq=hdd^2
gen hddcub=hdd^3
gen cddsq=cdd^2
gen cddcub=cdd^3

sort BILACCT_K daily_date  hour
egen timeid=group(daily_date hour)

destring  BILACCT_K, replace
xtset BILACCT_K timeid

*-----------------RD in time   order                  8/31/2020	     --------------*

**** Residuals ****
xtreg loghe  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K) 
predict residuals, resid

save `residentialorderwhole2020', replace

******************************** two steps AT ***********************************
reg residuals order if  timeToTreat >= -1 & timeToTreat <= 1
reg residuals order if  timeToTreat >= -2 & timeToTreat <= 2
reg residuals order if  timeToTreat >= -3 & timeToTreat <= 3
reg residuals order if  timeToTreat >= -4 & timeToTreat <=4
reg residuals order if  timeToTreat >= -5 & timeToTreat <=5
reg residuals order if  timeToTreat >= -6 & timeToTreat <= 6
reg residuals order if  timeToTreat >= -7 & timeToTreat <=7
reg residuals order if  timeToTreat >= -8 & timeToTreat <=8
reg residuals order if  timeToTreat >= -9 & timeToTreat <=9
reg residuals order if  timeToTreat >= -10 & timeToTreat <=10
reg residuals order if  timeToTreat >= -15 & timeToTreat <=15
reg residuals order if  timeToTreat >= -16 & timeToTreat <=16
reg residuals order if  timeToTreat >= -20 & timeToTreat <=20
reg residuals order if  timeToTreat >= -30 & timeToTreat <=30

xtreg residuals order if  timeToTreat >= -16 & timeToTreat <=16, fe vce(cluster BILACCT_K) 

reg residuals order
xtreg residuals order, fe vce(cluster BILACCT_K) 

xtreg loghe order tem_sp1-tem_sp4 preci* pressure wind humidity  i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K) 

keep if year==2020

gen poly =daily_date-mdy(04,01,2020)
gen polya= poly*(1-order)
gen polyb= poly*order
orthpoly poly, generate(poly*) deg(8) poly(P)

sum poly

********global estimation*********
xtreg loghe order poly1-poly3   tem_sp1-tem_sp4   preci* pressure wind humidity  i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K)
xtreg loghe order poly1-poly4   tem_sp1-tem_sp4   preci* pressure wind humidity  i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K) 
xtreg loghe order poly1-poly5   tem_sp1-tem_sp4   preci* pressure wind humidity  i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K) 
xtreg loghe order poly1-poly6   tem_sp1-tem_sp4   preci* pressure wind humidity  i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K) 
xtreg loghe order poly1-poly7   tem_sp1-tem_sp4   preci* pressure wind humidity  i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K) 
xtreg loghe order poly1-poly8   tem_sp1-tem_sp4   preci* pressure wind humidity  i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K) 

********local estimation*********
xtreg loghe order polya polyb   tem_sp1-tem_sp4   preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K)
xtreg loghe order polya polyb   tem_sp1-tem_sp4   preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K), if timeToTreat >= -7 & timeToTreat <=7
xtreg loghe order polya polyb   tem_sp1-tem_sp4   preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K), if timeToTreat >= -10 & timeToTreat <=10
xtreg loghe order polya polyb   tem_sp1-tem_sp4   preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K), if timeToTreat >= -15 & timeToTreat <=15
xtreg loghe order polya polyb   tem_sp1-tem_sp4   preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K), if timeToTreat >= -16 & timeToTreat <=16
xtreg loghe order polya polyb   tem_sp1-tem_sp4   preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K), if timeToTreat >= -29 & timeToTreat <=29
xtreg loghe order polya polyb   tem_sp1-tem_sp4   preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K), if timeToTreat >= -16 & timeToTreat <=29

********global estimation residuals *********
xtreg residuals order poly1-poly3, fe vce(cluster BILACCT_K)
xtreg residuals order poly1-poly4, fe vce(cluster BILACCT_K)
xtreg residuals order poly1-poly5, fe vce(cluster BILACCT_K)
xtreg residuals order poly1-poly6, fe vce(cluster BILACCT_K)
xtreg residuals order poly1-poly7, fe vce(cluster BILACCT_K)
xtreg residuals order poly1-poly8, fe vce(cluster BILACCT_K)

********local estimation residuals *********
xtreg residuals order polya polyb, fe vce(cluster BILACCT_K)
xtreg residuals order polya polyb, fe vce(cluster BILACCT_K), if timeToTreat >= -7 & timeToTreat <=7
xtreg residuals order polya polyb, fe vce(cluster BILACCT_K), if timeToTreat >= -10 & timeToTreat <=10
xtreg residuals order polya polyb, fe vce(cluster BILACCT_K), if timeToTreat >= -15 & timeToTreat <=15
xtreg residuals order polya polyb, fe vce(cluster BILACCT_K), if timeToTreat >= -16 & timeToTreat <=16
xtreg residuals order polya polyb, fe vce(cluster BILACCT_K), if timeToTreat >= -29 & timeToTreat <=29
xtreg residuals order polya polyb, fe vce(cluster BILACCT_K), if timeToTreat >= -16 & timeToTreat <=29

********global estimation residuals1*********
reg residuals order poly1-poly3
reg residuals order poly1-poly4
reg residuals order poly1-poly5
reg residuals order poly1-poly6
reg residuals order poly1-poly7
reg residuals order poly1-poly8

********local estimation residuals1*********
reg residuals order polya polyb
reg residuals order polya polyb if timeToTreat >= -7 & timeToTreat <=7
reg residuals order polya polyb if timeToTreat >= -10 & timeToTreat <=10
reg residuals order polya polyb if timeToTreat >= -15 & timeToTreat <=15
reg residuals order polya polyb if timeToTreat >= -16 & timeToTreat <=16
reg residuals order polya polyb if timeToTreat >= -29 & timeToTreat <=29
reg residuals order polya polyb if timeToTreat >= -16 & timeToTreat <=29

******* aggregation to daily ***************

collapse (mean) he order residuals day week_days month date1 daily_date  hdd* cdd* preci* pressure wind humidity poly* hour*, by(date)

gen loghe=log(he)

drop order poly*
gen order = (date1>=22006) & !missing(day)
gen poly =daily_date-mdy(04,01,2020)
gen polya= poly*(1-order)
gen polyb= poly*order
orthpoly poly, generate(poly*) deg(8) poly(P)

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

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(22005.5,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 21916 "-90" 21931 "-75"  21946 "-60" 21961 "-45" 21976 "-30" 21990 "-16" 22005.5 "0" 22036 "+30"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_global3rd_order.eps", replace as(eps)


**** global graphic 4th****
drop line regline line1 line2 
regress residuals order poly1-poly4
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(22005.5,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 21916 "-90" 21931 "-75"  21946 "-60" 21961 "-45" 21976 "-30" 21990 "-16" 22005.5 "0" 22036 "+30"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_global4th_order.eps", replace as(eps)


**** global graphic 5th****
drop line regline line1 line2 
regress residuals order poly1-poly5
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(22005.5,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 21916 "-90" 21931 "-75"  21946 "-60" 21961 "-45" 21976 "-30" 21990 "-16" 22005.5 "0" 22036 "+30"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_global5th_order.eps", replace as(eps)


**** global graphic 6th****
drop line regline line1 line2 
regress residuals order poly1-poly6
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(22005.5,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 21916 "-90" 21931 "-75"  21946 "-60" 21961 "-45" 21976 "-30" 21990 "-16" 22005.5 "0" 22036 "+30"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_global6th_order.eps", replace as(eps)


**** global graphic 7th****
drop line regline line1 line2 
regress residuals order poly1-poly7
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(22005.5,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 21916 "-90" 21931 "-75"  21946 "-60" 21961 "-45" 21976 "-30" 21990 "-16" 22005.5 "0" 22036 "+30"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_global7th_order.eps", replace as(eps)

**** global graphic 8th****
drop line regline line1 line2 
regress residuals order poly1-poly8
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(22005.5,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 21916 "-90" 21931 "-75"  21946 "-60" 21961 "-45" 21976 "-30" 21990 "-16" 22005.5 "0" 22036 "+30"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_global8th_order.eps", replace as(eps)


*** local*******
keep if poly >= -16 & poly <=29
set scheme s1mono
twoway (scatter residuals date1, msymbol(smcircle_hollow)) ///
(lpoly residuals date1 if date1<22006, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals date1 if date1>=22006, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline(22006, lcolor(navy) lpattern(dash)) legend(off) xlabel(21990(5)22036,labsize(small)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/reduced/whole/daily_localonestages_order_30.eps", replace as(eps)


**** global graphic 8th****
drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(22006,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(21990 "-16" 22006 "0" 22021 "15" 22036 "30" 22041 "45"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_localonestages_order_local.eps", replace as(eps)

**** global graphic 8th****
keep if poly >= -16 & poly <=16
drop line regline line1 line2 
regress residuals order polya polyb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order < 1
replace line2 = regline if order==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(22006,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(21990 "-16" 21995 "-10" 22001 "-5" 22006 "0"  22011 "5" 22016 "10" 22022 "16"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_localonestages_order_local16.eps", replace as(eps)


************************************************************************************
*-----------------RD in time        school            8/23/2020	     --------------*
************************************************************************************

clear
use `residentialorderwhole2020'

******************************** two steps AT ***********************************
reg residuals order1 if  timeToTreat1 >= -1 & timeToTreat1 <= 1
reg residuals order1 if  timeToTreat1 >= -2 & timeToTreat1 <= 2
reg residuals order1 if  timeToTreat1 >= -3 & timeToTreat1 <= 3
reg residuals order1 if  timeToTreat1 >= -4 & timeToTreat1 <=4
reg residuals order1 if  timeToTreat1 >= -5 & timeToTreat1 <=5
reg residuals order1 if  timeToTreat1 >= -6 & timeToTreat1 <= 6
reg residuals order1 if  timeToTreat1 >= -7 & timeToTreat1 <=7
reg residuals order1 if  timeToTreat1 >= -8 & timeToTreat1 <=8
reg residuals order1 if  timeToTreat1 >= -9 & timeToTreat1 <=9
reg residuals order1 if  timeToTreat1 >= -10 & timeToTreat1 <=10
reg residuals order1 if  timeToTreat1 >= -15 & timeToTreat1 <=15
reg residuals order1 if  timeToTreat1 >= -16 & timeToTreat1 <=16
reg residuals order1 if  timeToTreat1 >= -20 & timeToTreat1 <=20
reg residuals order1 if  timeToTreat1 >= -30 & timeToTreat1 <=30
reg residuals order1 if  timeToTreat1 >= -45 & timeToTreat1 <=45
reg residuals order1 if  timeToTreat1 >= -30 & timeToTreat1 <=15

xtreg residuals order if  timeToTreat >= -15 & timeToTreat <=15, fe vce(cluster BILACCT_K) 
reg residuals order1
xtreg residuals order1, fe vce(cluster BILACCT_K) 

xtreg loghe order1  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K) 

keep if year==2020 
**** Generate Polynomial ****
gen polynew =daily_date-mdy(03,16,2020)
gen polynewa= polynew*(1-order1)
gen polynewb= polynew*order1
orthpoly polynew, generate(polynew*) deg(8) poly(P)

********global estimation*********
xtreg loghe order1 polynew1-polynew3  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K)
xtreg loghe order1 polynew1-polynew4  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K)
xtreg loghe order1 polynew1-polynew5  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K)
xtreg loghe order1 polynew1-polynew6  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K)
xtreg loghe order1 polynew1-polynew7  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K)
xtreg loghe order1 polynew1-polynew8  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K)

********local estimation*********

xtreg loghe order1 polynewa polynewb  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K)
xtreg loghe order1 polynewa polynewb  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K), if timeToTreat1 >= -7 & timeToTreat1 <=7
xtreg loghe order1 polynewa polynewb  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K), if timeToTreat1 >= -14 & timeToTreat1 <=14
xtreg loghe order1 polynewa polynewb  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K), if timeToTreat1 >= -30 & timeToTreat1 <=30
xtreg loghe order1 polynewa polynewb  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K), if timeToTreat1 >= -45 & timeToTreat1 <45
xtreg loghe order1 polynewa polynewb  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K), if timeToTreat1 >= -15 & timeToTreat1 <15


********global estimation residuals *********
xtreg residuals order1 polynew1-polynew3, fe vce(cluster BILACCT_K)
xtreg residuals order1 polynew1-polynew4, fe vce(cluster BILACCT_K)
xtreg residuals order1 polynew1-polynew5, fe vce(cluster BILACCT_K)
xtreg residuals order1 polynew1-polynew6, fe vce(cluster BILACCT_K)
xtreg residuals order1 polynew1-polynew7, fe vce(cluster BILACCT_K)
xtreg residuals order1 polynew1-polynew8, fe vce(cluster BILACCT_K)

********local estimation residuals*********

xtreg residuals order1 polynewa polynewb, fe vce(cluster BILACCT_K)
xtreg residuals order1 polynewa polynewb, fe vce(cluster BILACCT_K), if timeToTreat1 >= -7 & timeToTreat1 <=7
xtreg residuals order1 polynewa polynewb, fe vce(cluster BILACCT_K), if timeToTreat1 >= -14 & timeToTreat1 <=14
xtreg residuals order1 polynewa polynewb, fe vce(cluster BILACCT_K), if timeToTreat1 >= -30 & timeToTreat1 <=30
xtreg residuals order1 polynewa polynewb, fe vce(cluster BILACCT_K), if timeToTreat1 >= -45 & timeToTreat1 <45
xtreg residuals order1 polynewa polynewb, fe vce(cluster BILACCT_K), if timeToTreat1 >= -15 & timeToTreat1 <15


********global estimation residuals1 *********
reg residuals order1 polynew1-polynew3
reg residuals order1 polynew1-polynew4
reg residuals order1 polynew1-polynew5
reg residuals order1 polynew1-polynew6
reg residuals order1 polynew1-polynew7
reg residuals order1 polynew1-polynew8

********local estimation residuals1*********

reg residuals order1 polynewa polynewb
reg residuals order1 polynewa polynewb if timeToTreat1 >= -7 & timeToTreat1 <=7
reg residuals order1 polynewa polynewb if timeToTreat1 >= -14 & timeToTreat1 <=14
reg residuals order1 polynewa polynewb if timeToTreat1 >= -30 & timeToTreat1 <=30
reg residuals order1 polynewa polynewb if timeToTreat1 >= -45 & timeToTreat1 <45
reg residuals order1 polynewa polynewb if timeToTreat1 >= -15 & timeToTreat1 <15

********global estimation residuals1 *********
reg residuals order1 polynew1-polynew3
reg residuals order1 polynew1-polynew4
reg residuals order1 polynew1-polynew5
reg residuals order1 polynew1-polynew6
reg residuals order1 polynew1-polynew7
reg residuals order1 polynew1-polynew8

********local estimation residuals1*********

reg loghe order1 polynewa polynewb
reg loghe order1 polynewa polynewb if timeToTreat1 >= -7 & timeToTreat1 <=7
reg loghe order1 polynewa polynewb if timeToTreat1 >= -14 & timeToTreat1 <=14
reg loghe order1 polynewa polynewb if timeToTreat1 >= -30 & timeToTreat1 <=30
reg loghe order1 polynewa polynewb if timeToTreat1 >= -45 & timeToTreat1 <45
reg loghe order1 polynewa polynewb if timeToTreat1 >= -15 & timeToTreat1 <15

******* aggregation to weekly ***************

collapse (mean) he order1 residuals day week_days month date1 daily_date  hdd* cdd* preci* pressure wind humidity poly* hour*, by(date)
gen loghe=log(he)
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
graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21989.5,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(21915 "-75"  21930 "-60" 21945 "-45" 21960 "-30" 21975 "-15" 21989.5 "0" 22005 "+15" 22020 "+30" 22035 "+45"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_global3rd_school.eps", replace as(eps)

**** global graphic 4th****
drop line regline line1 line2 
regress residuals order1 polynew1-polynew4
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21989.5,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(21915 "-75"  21930 "-60" 21945 "-45" 21960 "-30" 21975 "-15" 21989.5 "0" 22005 "+15" 22020 "+30" 22035 "+45"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_global4th_school.eps", replace as(eps)

**** global graphic 5th****
drop line regline line1 line2 
regress residuals order1 polynew1-polynew5
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21989.5,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(21915 "-75"  21930 "-60" 21945 "-45" 21960 "-30" 21975 "-15" 21989.5 "0" 22005 "+15" 22020 "+30" 22035 "+45"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_global5th_school.eps", replace as(eps)

**** global graphic 6th****
drop line regline line1 line2 
regress residuals order1 polynew1-polynew6
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21989.5,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(21915 "-75"  21930 "-60" 21945 "-45" 21960 "-30" 21975 "-15" 21989.5 "0" 22005 "+15" 22020 "+30" 22035 "+45"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_global6th_school.eps", replace as(eps)


**** global graphic 7th****
drop line regline line1 line2 
regress residuals order1 polynew1-polynew7
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21989.5,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(21915 "-75"  21930 "-60" 21945 "-45" 21960 "-30" 21975 "-15" 21989.5 "0" 22005 "+15" 22020 "+30" 22035 "+45"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_global7th_school.eps", replace as(eps)


**** global graphic 8th****
drop line regline line1 line2 
regress residuals order1 polynew1-polynew8
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21989.5,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(21915 "-75"  21930 "-60" 21945 "-45" 21960 "-30" 21975 "-15" 21989.5 "0" 22005 "+15" 22020 "+30" 22035 "+45"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_global8th_school.eps", replace as(eps)


******* local ***************

keep if polynew >= -45 & polynew <=45
drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21990,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel( 21945 "-45" 21960 "-30" 21975 "-15" 21990 "0" 22005 "15" 22020 "30" 22035 "45"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_localonestages_school_local.eps", replace as(eps)


keep if polynew >= -45 & polynew <=45
set scheme s1mono
twoway (scatter residuals date1, msymbol(smcircle_hollow)) ///
(lpoly residuals date1 if date1<21990, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals date1 if date1>=21990, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline(21990, lcolor(navy) lpattern(dash)) legend(off) xlabel(21945 (5) 22035 ,labsize(small)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/reduced/whole/daily_localonestages_school_45.eps", replace as(eps)


keep if polynew >= -30 & polynew <=30
set scheme s1mono
twoway (scatter residuals date1, msymbol(smcircle_hollow)) ///
(lpoly residuals date1 if date1<21990, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals date1 if date1>=21990, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline(21990, lcolor(navy) lpattern(dash)) legend(off) xlabel(21960 (5) 22020 , labsize(small))  ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/reduced/whole/daily_localonestages_school_30.eps", replace as(eps)

/*
set scheme s1mono
twoway (scatter residuals date1, msymbol(smcircle_hollow)) ///
(lpoly residuals date1 if date1<21990, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals date1 if date1>=21990, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline(21990, lcolor(navy) lpattern(dash)) legend(off) xlabel(21960 "-30" 21970 "-20" 21980 "-10" 21200 "+10" 21210 "+20" 22020 "+30", labsize(small)) ///
title(Single family, size(medlarge) color(black)) subtitle(Daily, size(medsmall) margin(bottom)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))

*/

keep if polynew >= -30 & polynew <=15
set scheme s1mono
twoway (scatter residuals date1, msymbol(smcircle_hollow)) ///
(lpoly residuals date1 if date1<21990, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)) ///
(lpoly residuals date1 if date1>=21990, bw(2.5) kernel(gau) lwidth(thin) lcolor(gs6)), ///
xline(21990, lcolor(navy) lpattern(dash)) legend(off) xlabel(21960(5)22005,labsize(small)) ///
xtitle(Date) ytitle(Residuals of hourly electricity usage (in log))
graph export "RD/reduced/whole/daily_localonestages_school_15.eps", replace as(eps)


**** global graphic 8th****
keep if polynew >= -15 & polynew <=15
drop line regline line1 line2 
regress residuals order1 polynewa polynewb
predict line if e(sample), xb
sum line

gen regline = line 
gen line1 = .
gen line2 = .
replace line1 = regline if order1 < 1
replace line2 = regline if order1==1

graph twoway (scatter residuals date1, msize(*1) msymbol(o) mcolor(gs9) ///
xline(21990,lcolor(black) lwidth(vthin)) scheme(s1mono) plotregion(style(none)) ///
xlabel(21975 "-15" 21980 "-10" 21985 "-5" 21990 "0"  21995 "5" 22000 "10" 22005 "15"))  ///
(line line1 date1, lcolor(black) lpattern(solid)) ///
(line line2 date1, lcolor(black) lpattern(solid)), ///
ytitle("Residuals of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD/reduced/whole/daily_localonestages_school_local15.eps", replace as(eps)
