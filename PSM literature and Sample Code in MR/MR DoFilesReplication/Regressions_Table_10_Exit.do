


clear all

capture log close

* set path
global path ""
cd $path


set linesize 200
log using Log\Regressions_Table_10_Exit.log, replace

/*

Do file produces results presented in table 10 of the paper (and some results for the appendix)

1) merge data sets

2) prepare exit variable

3) run regressions - OLS

4) wild bootstrap

5) regressions for appendix

*/


global BootReps 1000


// merge data set
* firm-level variables
use Data\firm_exp_imp_v04.dta, clear

bys ind5 year: egen sales_ind=total(domsales)
g lnsales_ind=ln(sales_ind)

* sector level retail import variables
merge m:1 ind5 year using Data\Imp_Sector_Retail_2.dta, update
tab year if _merge==1
tab ind_id if _merge==1
drop if _merge==2
drop _merge

* sector level non-retail import variables
merge m:1 ind5 year using Data\Imp_Sector_NonRetail.dta, update
tab year if _merge==1
tab ind_id if _merge==1
drop if _merge==2
drop _merge

tab year

drop if year<1999
drop if year>2008


// Prepare firm exit variable
* ensure consistency of exit variable
bys cvrnr: egen max_year3=max(year)

g resign_year=year(date_resign)
tab resign_year
drop if resign_year<1999
bys cvrnr: egen max_year_rd=max(resign_year)

/*
note:
- max_year computed in Data_Management_1_Firms
- max_year2 computed in Data_Management_3_Combine
*/

tab max_year
tab year
g max_year4=max_year
replace max_year4=2009 if max_year4>2008
replace max_year3=2009 if max_year3>2008
replace max_year2=2009 if max_year2>2008
drop max_year_rd

// drop firms that switch sector (i.e. to non-retail sector)
* max_year4 based on initial sample before dropping any obs
* max_year3 based on current sample after dropping obs (i.e. only retail firms)
* hence, differences in max year imply changes in sector affiliation
g tmp=1 if max_year3!=max_year4 & max_year4<2009 
count if max_year3<max_year4 & max_year4<2009 
count if max_year3>max_year4 & max_year4<2009 
tab year tmp
count
bys cvrnr: egen tmp2=max(tmp)
count if tmp2==1
drop if tmp2==1
drop tmp*

// exit variable based on resign date should not contradict max_year4
g tmp=(year==max_year4)
g tmp2=(year==resign_year)
tab year tmp
tab year tmp2
tab tmp tmp2

* replace with zero if firm is still in sample acc. max year, even though resign date indicates something else
count if tmp2==1 & tmp==0 & year<2009
replace tmp2=0 if tmp==0 & year<2009

tab resign_year
count 

* account for cases where resign date indicate exit in t+1
* e.g. firm's last year in sample in 2005 and resign data states year 2006
count if resign_year==max_year4+1 & year==max_year4
count if resign_year==max_year4+2 & year==max_year4
count if resign_year==max_year4+3 & year==max_year4
replace resign_year=. if resign_year>2009
tab resign_year, mi
tab year tmp2
sort cvrnr year
order cvrnr year max_year max_year4 resign_year tmp*
replace tmp2 = 1 if resign_year==max_year4+1  & year==max_year4
replace tmp2 = 1 if resign_year==max_year4+2  & year==max_year4
tab year tmp2

* cases where resign data does not equal ".", even though the firms does not exit
* happens, if resign is indicated in years prior to exit 
* e.g. firm exits in 2007 and resign data indicates this already in 2006, even though
* the firm in in the sample in 2007
count if tmp2==0 & resign_year!=.
g tmp3 =1 if tmp2==0 & resign_year!=.
bys cvrnr: egen tmp4=max(tmp3)
sort cvrnr year
order cvrnr year tmp*

tab tmp tmp2
replace tmp2=0 if tmp2==1 & tmp==0 // resign_date says firm left, but it is still there
tab year tmp2


// firms switching sectors are not considered as exit (dropped above)
* exit based on max_year
g exit=0
replace exit=1 if tmp==1
replace exit=1 if tmp2==1
tab year exit

* exit based on resign date
g exit2=0
replace exit2=1 if tmp2==1
tab year exit2
drop tmp*

tab exit exit2

* drop firms that exit acc. to max_year but not acc. to resign_date
g tmp=1 if exit==1 & exit2!=1
bys cvrnr: egen tmp2=max(tmp)
tab year tmp
su tmp tmp2
drop if tmp2==1
drop tmp*


* focus on exit defined by resign date
drop exit
ren exit2 exit
tab year exit

g l1d_imp_neu=(l1imp_it_neu>0) if l1imp_it_neu!=.
su l1imp_it_neu l1d_imp_neu year

// final exit variable: 
* in sample in t-1 and exit in t 
* i.e.: firms should be in smple for at least two consecutive periods
tab year
xtset cvrnr year, yearly
g tmp=.
bys cvrnr (year): replace tmp=0 if l.exit==0 & exit==0
bys cvrnr (year): replace tmp=1 if l.exit==0 & exit==1

tab year tmp, mi
drop if year<2000
drop if tmp==.
replace exit=tmp
drop tmp
tab year exit, mi

drop if lnbalance_sum==.
drop if imp_r<=0 | imp_r==.
drop if imp_w<=0 | imp_w==.

save Data\Exit_Estimation_Sample.dta, replace



///////////////////////

//  Run regressions  //

///////////////////////


** Prepare interaction terms

use Data\Exit_Estimation_Sample.dta, clear

g lnshock_r=ln(imp_r)
g lnshock_w=ln(imp_w)

su lnshock_r lnshock_w,d

foreach v of varlist lnshock* {
	egen tmp=mean(`v')
	replace `v'=`v'-tmp
	drop tmp
}

egen p25=pctile(lnbalance_sum), p(25)
su p25
g d_lnbalance_sum=(lnbalance_sum<p25)
tab d_lnbalance_sum

g intera=lnshock_r*d_imp_neu
g intera2=lnshock_r*d_lnbalance_sum

g intera_w=lnshock_w*d_imp_neu
g intera2_w=lnshock_w*d_lnbalance_sum

tab exit, mi
su exit d_imp_neu lnshock* d_lnbalance_sum lnsales_ind


** regressions

*** one-way clustering ***

reg exit d_imp_neu lnshock_r d_lnbalance_sum intera lnsales_ind i.ind5 i.year, cluster(ind5)
est store impall_dimp_r
reg exit d_imp_neu lnshock_r d_lnbalance_sum intera2 lnsales_ind i.ind5 i.year, cluster(ind5)
est store impall_dsize_r
reg exit d_imp_neu lnshock_r d_lnbalance_sum intera intera2 lnsales_ind i.ind5 i.year, cluster(ind5)
est store impall_both_r

xml_tab imp* , ///
 save(Log\Results_Table10_Exit.xml) replace ///
 sheet(OneWay_R) sd below stats(N r2_a r2) ///
 keep(d_imp_neu15 d_lnbalance_sum lnshock_r intera intera2 lnsales_ind)
est clear

qui tab year, g(dyr)
qui tab ind5, g(dind)


*** one-way clustering - boostrap ***

** direct imports only

** Impose H0 for shock and intera

* number of parameters: enforce null for 4, not for secsales + 7 year dummies + 23 sector dummies
cgmwildboot exit d_imp_neu lnshock_r d_lnbalance_sum intera lnsales_ind dyr2-dyr8 dind2-dind24, cluster(ind5) bootcluster(ind5) reps($BootReps) seed(23445232) null(. 0 . 0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .)
* number of parameters: enforce null for 4, not for secsales + 7 year dummies + 23 sector dummies
cgmwildboot exit d_imp_neu lnshock_r d_lnbalance_sum intera2 lnsales_ind dyr2-dyr8 dind2-dind24, cluster(ind5) bootcluster(ind5) reps($BootReps) seed(23445232) null(. 0 . 0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .)
* number of parameters: enforce null for 5, not for secsales + 7 year dummies + 23 sector dummies
cgmwildboot exit d_imp_neu lnshock_r d_lnbalance_sum intera intera2 lnsales_ind dyr2-dyr8 dind2-dind24, cluster(ind5) bootcluster(ind5) reps($BootReps) seed(23445232) null(. 0 . 0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .)
cgmwildboot exit d_imp_neu lnshock_r d_lnbalance_sum intera intera2 lnsales_ind dyr2-dyr8 dind2-dind24, cluster(ind5) bootcluster(ind5) reps($BootReps) seed(23445232) null(. 0 . . 0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .)





////////////////////////////

//  Appendix regressions  //

////////////////////////////


** Prepare interaction terms

use Data\Exit_Estimation_Sample.dta, clear

g lnshock_r=ln(imp_r)
g lnshock_w=ln(imp_w)

su lnshock_r lnshock_w,d

foreach v of varlist lnshock* {
	egen tmp=mean(`v')
	replace `v'=`v'-tmp
	drop tmp
}

egen p25=pctile(lnbalance_sum), p(25)
su p25
g d_lnbalance_sum=(lnbalance_sum<p25)
tab d_lnbalance_sum

g intera=lnshock_r*d_imp_neu
g intera2=lnshock_r*d_lnbalance_sum

g intera_w=lnshock_w*d_imp_neu
g intera2_w=lnshock_w*d_lnbalance_sum

tab exit, mi
su exit d_imp_neu lnshock* d_lnbalance_sum lnsales_ind


*** one-way clustering ***

* direct and indirect imports
reg exit d_imp_neu lnshock_r lnshock_w d_lnbalance_sum intera intera_w lnsales_ind i.ind5 i.year, cluster(ind5)
est store impall_dimp_rw
reg exit d_imp_neu lnshock_r lnshock_w d_lnbalance_sum intera2 intera2_w lnsales_ind i.ind5 i.year, cluster(ind5)
est store impall_dsize_rw
reg exit d_imp_neu lnshock_r lnshock_w d_lnbalance_sum intera intera2 intera_w intera2_w lnsales_ind i.ind5 i.year, cluster(ind5)
est store impall_both_rw

xml_tab imp* , ///
 save(Log\Results_Table10_Exit.xml) append ///
 sheet(OneWay_RW) sd below stats(N r2 r2_a) ///
 keep(d_imp_neu15 d_lnbalance_sum lnshock_r intera intera2 ///
 lnshock_w intera_w intera2_w lnsales_ind)
est clear
 


** Impose H0 for shock and intera


qui tab year, g(dyr)
qui tab ind5, g(dind)


* number of parameters: enforce null for 4, not for secsales + 7 year dummies + 23 sector dummies
* retail
cgmwildboot exit d_imp_neu lnshock_r lnshock_w d_lnbalance_sum intera intera_w lnsales_ind dyr2-dyr8 dind2-dind24, cluster(ind5) bootcluster(ind5) reps($BootReps) seed(23445232) null(. 0 . . 0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .)
* wolesale
cgmwildboot exit d_imp_neu lnshock_r lnshock_w d_lnbalance_sum intera intera_w lnsales_ind dyr2-dyr8 dind2-dind24, cluster(ind5) bootcluster(ind5) reps($BootReps) seed(23445232) null(. . 0 . . 0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .)

* number of parameters: enforce null for 4, not for secsales + 7 year dummies + 23 sector dummies
* retail
cgmwildboot exit d_imp_neu lnshock_r lnshock_w d_lnbalance_sum intera2 intera2_w lnsales_ind dyr2-dyr8 dind2-dind24, cluster(ind5) bootcluster(ind5) reps($BootReps) seed(23445232) null(. 0 . . 0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .)
* wolesale
cgmwildboot exit d_imp_neu lnshock_r lnshock_w d_lnbalance_sum intera2 intera2_w lnsales_ind dyr2-dyr8 dind2-dind24, cluster(ind5) bootcluster(ind5) reps($BootReps) seed(23445232) null(. . 0 . . 0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .)

* both
* number of parameters: enforce null for 5, not for secsales + 7 year dummies + 23 sector dummies
* retail - intera
cgmwildboot exit d_imp_neu lnshock_r lnshock_w d_lnbalance_sum intera intera2 intera_w intera2_w lnsales_ind dyr2-dyr8 dind2-dind24, cluster(ind5) bootcluster(ind5) reps($BootReps) seed(23445232) null(. 0 . . 0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .)
* retail - intera2
cgmwildboot exit d_imp_neu lnshock_r lnshock_w d_lnbalance_sum intera intera2 intera_w intera2_w lnsales_ind dyr2-dyr8 dind2-dind24, cluster(ind5) bootcluster(ind5) reps($BootReps) seed(23445232) null(. 0 . . . 0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .)
* wolesale - intera
cgmwildboot exit d_imp_neu lnshock_r lnshock_w d_lnbalance_sum intera intera2 intera_w intera2_w lnsales_ind dyr2-dyr8 dind2-dind24, cluster(ind5) bootcluster(ind5) reps($BootReps) seed(23445232) null(. . 0 . . . 0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .)
* wolesale - intera2
cgmwildboot exit d_imp_neu lnshock_r lnshock_w d_lnbalance_sum intera intera2 intera_w intera2_w lnsales_ind dyr2-dyr8 dind2-dind24, cluster(ind5) bootcluster(ind5) reps($BootReps) seed(23445232) null(. . 0 . . . . 0 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .)
 


 
log close
exit, clear
