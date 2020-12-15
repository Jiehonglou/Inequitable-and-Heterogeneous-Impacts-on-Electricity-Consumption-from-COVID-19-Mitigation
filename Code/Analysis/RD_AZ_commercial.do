*------------------------------------------------------------------------------*
*-----------RD AZ Commercial version entire dataset keep 2020   9/28/2020  ----*
*------------------------------------------------------------------------------*
use "commercial.dta", replace
tempfile commercialorder

**** spline temperature
mkspline tem_sp = tem , cubic displayknots

**** holiday 
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

**** regenerate year, month and day 
drop year month day 
gen daily_date=date
format daily_date %td

generate year=year(date1)
generate month=month(date1)
generate day=day(date1)
destring hour, replace

*-----------------treatment date                   9/28/2020 	     ----------*

**** stay-at-home dummy (April 1)
gen order = (date1>=22006) & !missing(day)
**** school close (March 16)
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

gen loghe=log(he)
gen precisq=preci^2

sort BILACCT_K daily_date  hour
egen timeid=group(daily_date hour)
destring  BILACCT_K, replace
xtset BILACCT_K timeid

*-----------------RD in time   order                  9/28/2020 	 ----------*

**** Residuals ****
xtreg loghe  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K) 
predict residuals, resid
save `commercialorder', replace

**** two steps event study
reg residuals order if  timeToTreat >= -4 & timeToTreat <=4
reg residuals order if  timeToTreat >= -5 & timeToTreat <=5
reg residuals order if  timeToTreat >= -15 & timeToTreat <=15
reg residuals order 
xtreg loghe order tem_sp1-tem_sp4 preci* pressure wind humidity  i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K) 

keep if year==2020
**** Generate Polynomial
gen poly =daily_date-mdy(04,01,2020)
gen polya= poly*(1-order)
gen polyb= poly*order
orthpoly poly, generate(poly*) deg(8) poly(P)

**** global RD estimation residuals 
xtreg residuals order poly1-poly4, fe vce(cluster BILACCT_K)

**** local RD estimation residuals
xtreg residuals order polya polyb, fe vce(cluster BILACCT_K), if timeToTreat >= -15 & timeToTreat <=15

**** Figures aggregation to daily 

collapse (mean) he order residuals day week_days month date1 daily_date  hdd* cdd* preci* pressure wind humidity poly* hour*, by(date)
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
ytitle("Commercial of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "AZ_commercial_order.eps", replace as(eps)


*-----------------RD in time   School.                    9/28/2020 	 ----------*
clear
use `commercialorder'

**** two steps event study
reg residuals order1 if  timeToTreat1 >= -5 & timeToTreat1 <=5
reg residuals order1 if  timeToTreat1 >= -15 & timeToTreat1 <=15
reg residuals order1 
xtreg loghe order1  tem_sp1-tem_sp4  preci* pressure wind humidity i.month i.week_days i.hour i.holiday, fe vce(cluster BILACCT_K) 

keep if year==2020 
**** Generate Polynomial 
gen polynew =daily_date-mdy(03,16,2020)
gen polynewa= polynew*(1-order1)
gen polynewb= polynew*order1
orthpoly polynew, generate(polynew*) deg(8) poly(P)

**** global RD estimation residuals 
xtreg residuals order1 polynew1-polynew5, fe vce(cluster BILACCT_K)
**** local RD estimation residuals
xtreg residuals order1 polynewa polynewb, fe vce(cluster BILACCT_K), if timeToTreat1 >= -15 & timeToTreat1 <15

**** Figures aggregation to daily 
collapse (mean) he order1 residuals day week_days month date1 daily_date  hdd* cdd* preci* pressure wind humidity poly* hour*, by(date)
replace order1  = 0 if order1 < 1

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
ytitle("Commercial of hourly electricity usage (in log)") xtitle("Date") legend(off)
graph export "RD_commercial_school.eps", replace as(eps)

