*This do file is for data and variable construction 

global dir "C:\Users\Ritika Agarwal\Desktop\Paris School of Economics\M2\Thesis\Data\Merge"
global tables "C:\Users\Ritika Agarwal\Desktop\Paris School of Economics\M2\Thesis\Data\Merge\Tables"
global graphs "C:\Users\Ritika Agarwal\Desktop\Paris School of Economics\M2\Thesis\Graphs_Thesis"

use "$dir\a_indresp.dta", clear
destring intdatd_dv intdatm_dv intdaty_dv, replace 
gen doi = mdy(intdatm_dv, intdatd_dv, intdaty_dv)
format %td doi 
save, replace

use "$dir\b_indresp.dta" 
destring intdatd_dv intdatm_dv intdaty_dv, replace 
gen doi = mdy(intdatm_dv, intdatd_dv, intdaty_dv)
format %td doi 
save, replace

use "$dir\c_indresp.dta", clear 
destring intdatd_dv intdatm_dv intdaty_dv, replace 
gen doi = mdy(intdatm_dv, intdatd_dv, intdaty_dv)
format %td doi 
save, replace

use "$dir\d_indresp.dta", clear 
destring intdatd_dv intdatm_dv intdaty_dv, replace 
gen doi = mdy(intdatm_dv, intdatd_dv, intdaty_dv)
format %td doi 
save, replace

use "$dir\e_indresp.dta", clear  
destring intdatd_dv intdatm_dv intdaty_dv, replace 
gen doi = mdy(intdatm_dv, intdatd_dv, intdaty_dv)
format %td doi 
save, replace

use "$dir\f_indresp.dta", clear  
destring intdatd_dv intdatm_dv intdaty_dv, replace 
gen doi = mdy(intdatm_dv, intdatd_dv, intdaty_dv)
format %td doi 
save, replace

use "$dir\g_indresp.dta", clear
destring intdatd_dv intdatm_dv intdaty_dv, replace 
gen doi = mdy(intdatm_dv, intdatd_dv, intdaty_dv)
format %td doi 
save, replace

use "$dir\h_indresp.dta" 
destring intdatd_dv intdatm_dv intdaty_dv, replace 
gen doi = mdy(intdatm_dv, intdatd_dv, intdaty_dv)
format %td doi 
save, replace 

use "$dir\i_indresp.dta", clear 
destring intdatd_dv intdatm_dv intdaty_dv, replace 
gen doi = mdy(intdatm_dv, intdatd_dv, intdaty_dv)
format %td doi 
save, replace

use "$dir\j_indresp.dta", clear 
destring intdatd_dv intdatm_dv intdaty_dv, replace 
gen doi = mdy(intdatm_dv, intdatd_dv, intdaty_dv)
format %td doi 
save, replace

use "$dir\k_indresp.dta", clear  
destring intdatd_dv intdatm_dv intdaty_dv, replace 
gen doi = mdy(intdatm_dv, intdatd_dv, intdaty_dv)
format %td doi 
save, replace

**appending** 

clear
set maxvar 120000

use "$dir\a_indresp", clear
append using "$dir/b_indresp.dta" "$dir/c_indresp.dta" "$dir/d_indresp.dta" "$dir/e_indresp.dta" "$dir/f_indresp.dta" "$dir/g_indresp.dta" "$dir/h_indresp.dta" "$dir/i_indresp.dta" "$dir/j_indresp.dta" "$dir/k_indresp.dta", gen(filnum) nolabel

save "$dir\data_doi"
use "$dir\data_doi"
compress 

gen day = doi
destring day, replace
format day %td

order  pidp wave pid hidp pno month quarter sampst sex dvage birthy doi day 
sort pidp wave 
save "$dir\data_doi_1" 

use "$dir\data_doi_1"
keep pidp wave pid hidp month quarter pno sampst sex dvage birthy doi day jbstat qfhigh basrate ovtnsa jbsat
save "$dir\toy_data_doi_1"

****reducing the dataset again: to make it more relevant to our study*** 

use "$dir\data_doi_1" 

keep pidp wave pid hidp pno month quarter sampst sex dvage birthy doi day hhorig memorig psu strata istrtdatd istrtdatm istrtdaty mvever mvyr jbstat mlstat mlstatchk ukborn plbornc yr2uk4 citzn1 citzn2 citzn3 qfhigh school scend schlloc fenow pacob payruk macob mayruk mayruk1 paedqf maedqf natid1 natid2 natid3 natid4 natid5 natid6 natid97 racel sf1 sf3b sf3a jboff jbhas jbterm1 jbterm2 jbsemp jbbgy jbsize jbsect jbsectpub jbhrs jbot jbotpd jbpl paygwc paynwc payusl payu payuwc payug paytyp ovtpay extrate basnsa basrate basrest ovtnsa ovtrate jbpen jbpenm jshrs jspayu jbsat jbhad jlendy jlsemp jlsize j2has j2pay jobdeny eed12 resjobdeny97 resjobdeny96 resjobdeny9 resjobdeny8 resjobdeny7 resjobdeny6 resjobdeny5 resjobdeny4 resjobdeny3 resjobdeny2 resjobdeny1 ccare pjbptft pjsptft prearn prearna_w1 paygu_dv payg_dv paynu_dv payn_dv seearngrs_dv seearnnet_dv j2pay_dv j2paynet_dv age_dv intdatd_dv intdatm_dv intdatm_dv intdaty_dv doby_dv qfhighfl_dv qfhigh_dv hiqual_dv jbft_dv depenth1 depenth2 depenth2 depenth3 depenth4 depenth5 depenth6 jblkcha jbxpcha jblkchb jbxpchb jblkchc jbxpchc jblkchd jbxpchd jbxpche jbsec scsf1 simjob siminc njobhist sclfsat1 sclfsat2 sclfsat7 sclfsato scghqa scghqb scghqc scghqd scghqe scghqf scghqg scghqh scghqi scghqj scghqk scghql scghq1_dv scghq2_dv gor_dv


save "$dir\data_final"
**********************************************************************************************************************

use "$dir\data_final"

mvdecode _all, mv(-9, -8, -7, -1, -2)

*birthy: 901 missing values generated

****creating the MW thresholds again: this time based on dates of implementation

* we need to include the fact that an individual could be 25 years of age etc etc cause haha 
*thus depending on the age of the individual on the doi we will allocate the MW 

gen MW_21= . 
replace MW_21 = 5.73 if day < 18171 & dvage >= 21 
*this is for interviews which took place before Oct 2009 
replace MW_21 = 5.80 if day >= 18171 & day < 18536 & dvage >= 21 
*this is for interviews which took place after Oct 2009 but before Oct 2010
replace MW_21 = 5.93 if day >= 18536 & day < 18901 & dvage >= 21 
*interview after Oct 2010 but before Oct 2011 
replace MW_21 = 6.08 if day >= 18901 & day < 19267 & dvage >= 21 
*after Oct 2011 but before Oct 2012 
replace MW_21 = 6.19 if day>= 19267 & day < 19632 & dvage >= 21 
*after Oct 2012 but before Oct 2013 
replace MW_21 = 6.31 if day>= 19632 & day <19997 & dvage >= 21 
*after Oct 2013 and before Oct 2014 
replace MW_21 = 6.50 if day >= 19997 & day <20362 & dvage >= 21 
*after Oct 2014 and before Oct 2015
replace MW_21 = 6.70 if day>=20362 & day < 20728 & dvage >=21 
*after Oct 2015 and before Oct 2016 

*now adding the complication of age? 

replace MW_21 = 6.95 if day>= 20728 & day <20910 & dvage >= 21 & dvage < 25 
*after oct 2016 but before April 2017 
replace MW_21 = 7.05 if day >= 20910 & day <21275 & dvage >=21 & dvage <25  
*after Apr 2017 and before April 2018
replace MW_21 = 7.38 if day >= 21275 & day <21640 & dvage >=21 & dvage <25 
*after apr 2018 before apr 2019 
replace MW_21 = 7.70 if day>= 21640 & day <22006 & dvage >= 21 & dvage <25 
*after apr 2019 before apr 2020
replace MW_21 = 8.20 if day>= 22006 & day <22371 & dvage >=21 & dvage <25 
*after apr 2020 and before apr 2021 
replace MW_21 = 8.36 if day >= 22371 & day <22736 & dvage >=21 & dvage <23 

*creating a new variable for ages 25, which takes the same value as 21 uptil 2015 and then changes value from April 2016 

gen MW_25 = . 
replace MW_25 = 5.73 if day < 18171 & dvage >= 21 
*this is for interviews which took place before Oct 2009 
replace MW_25 = 5.80 if day >= 18171 & day < 18536 & dvage >=21 
*this is for interviews which took place after Oct 2009 but before Oct 2010
replace MW_25 = 5.93 if day >= 18536 & day < 18901 & dvage >= 21 
*interview after Oct 2010 but before Oct 2011 
replace MW_25 = 6.08 if day >= 18901 & day < 19267 & dvage >=21 
*after Oct 2011 but before Oct 2012 
replace MW_25 = 6.19 if day>= 19267 & day < 19632 & dvage >=21 
*after Oct 2012 but before Oct 2013 
replace MW_25 = 6.31 if day>= 19632 & day <19997 &dvage >=21 
*after Oct 2013 and before Oct 2014 
replace MW_25 = 6.50 if day >= 19997 & day <20362 & dvage>=21 
*after Oct 2014 and before Oct 2015
replace MW_25 = 6.70 if day>=20362 & day < 20545 & dvage >=21 
*after Oct 2015 and before Apr 2016 

*now adding the complication of age? 

replace MW_25 = 7.20 if day>= 20545 & day <20910 & dvage >= 25 
*after apr 2016 but before April 2017 
replace MW_25 = 7.50 if day >= 20910 & day <21275 &dvage >= 25 
*after Apr 2017 and before April 2018
replace MW_25 = 7.83 if day >= 21275 & day <21640 & dvage >=25 
*after apr 2018 before apr 2019 
replace MW_25 = 8.21 if day>= 21640 & day <22006 & dvage>=25 
*after apr 2019 before apr 2020
replace MW_25 = 8.72 if day>= 22006 & day <22371 &dvage >=25 
*after apr 2020 and before apr 2021 
replace MW_25 = 8.91 if day >= 22371 & day <22736 & dvage >=23 
*aftr apr 2021 but before apr 2022 

 
********************************************************************************
*Creation of variables needed for analysis:

gen post =. 
replace post= 0 if day >= 20301 & day <20545 
replace post= 1 if day >= 20545 & day <20789 
*8 months around the date of reform 
*1st Aug 2015 = 20301 
*1st Dec 2016 = 20789 

gen post0 =. 
replace post0 = 0 if day>=19936 & day <20179
replace post0 = 1 if day>= 20179 & day <20423 
*8 months around 1 April 2015 

gen post_placebo =. 
replace post_placebo = 0 if day >= 19844 & day <20089 
replace post_placebo = 1 if day >= 20089 & day <20332
*20089 corresponds to 1 Jan 2015 & this is 8 months after 1 Jan 
*8 months before 1 Jan 2015 

gen post6 =. 
replace post6 = 0 if day> 20362 & day < 20545
replace post6 = 1 if day> 20545 & day < 20728
*this is 6 months around the date of reform 

gen z_day= day - 20545 
*(7 missing values generated)

gen age_gr =.
replace age_gr= 1 if dvage >= 20 & dvage <=30 & dvage !=.

gen female = sex == 2 

*dummy for London
gen region =.
replace region = 1 if gor_dv == 7 
replace region = 0 if gor_dv != 7 

*dummy for race
gen white =. 
replace white = 1 if racel == 1 
replace white = 0 if racel != 1

*marital status dummies 
gen single = mlstat == 1 
gen married = mlstat == 2|mlstat == 3 
gen separated = mlstat == 4 | mlstat == 5 | mlstat == 7 | mlstat == 8  
gen widowed = mlstat == 6 | mlstat == 9 

*educ categories dummies 
gen noeduc = qfhigh_dv == 96 /*no qualifications*/ 
gen low_educ = qfhigh_dv == 16|qfhigh_dv == 15|qfhigh_dv == 14|qfhigh_dv == 13  /*age 16+ qualifications*/ 
gen med_educ = qfhigh_dv == 12 | qfhigh_dv == 11 | qfhigh_dv == 10 | qfhigh_dv == 9 | qfhigh_dv == 8 | qfhigh_dv == 7 /*age 18+ qualifications*/
gen high_educ = qfhigh_dv == 6 | qfhigh_dv == 5 | qfhigh_dv == 4 | qfhigh_dv == 3 | qfhigh_dv == 2 | qfhigh_dv == 1 /*tertiary qualifications*/ 

*job related dummies 
gen pvt_sect = jbsect == 1 
gen hrwage= paytyp == 3
gen z_prev = day - 20179 
 
*outcomes 
gen jbsat1= . 
replace jbsat1= jbsat if jbsat != -10
gen lfsat= . 
replace lfsat= sclfsato if sclfsato != -10
gen paysat= . 
replace paysat= sclfsat2 if sclfsat2 != -10

gen emp =. 
replace emp = 1 if jbsemp == 1 & jbstat == 2 & jbterm1 == 1
replace emp = 1 if jbsemp == 1 & jbstat == 2 & jbterm2 == 1 
replace emp = 1 if jbsemp == 1 & jbstat == 2 & jbterm2 == 2 
replace emp = 1 if jbsemp == 1 & jbstat == 2 & jbterm2 == 3 
replace emp = 1 if jbsemp == 1 & jbstat == 2 & jbterm2 == 4 

gen low_wage =. 
replace low_wage = 1 if basrate > 0 & basrate <= 14

gen z_placebo = day - 20089

*first treatment variable
gen eligible = birthy <= 1991  /*these are people who are eligible for the reform: will be 25 in 2016*/ 

*as we lack information on marital status: simplified dummies 
gen single_1 = mlstat == 1 
gen married_1 = mlstat == 2 | mlstat == 3 

*interaction 
gen treat1 = eligible*post6

xtset pidp wave 
gen lag1 = l1.basrate if post6!=. /*this is the lagged basrate for everyone who gave an interview 8 months before & after the reform*/

gen treat2 =. 
replace treat2 = 1 if lag1 < 7.2 & eligible == 1 /*the ones treated because of wage rate*/ 
replace treat2 = 0 if lag1 >= 7.19 & lag1 <= 10.8 & eligible!=. /*the control by wage rate*/
replace treat2 = 0 if eligible == 0 

*second treatment variable 
*gen treat2_new =. 
*replace treat2_new = 1 if treat2 == 1 & basrate >= 7.19
*replace treat2_new = 0 if treat2 == 0 

*interaction 
gen treat3 = treat2*post6



*MAIN STRATEGY
xtset pidp wave 
gen wage_pre = l1.basrate if post6!=. 

*third treatment variable 
gen wage_e =. 
replace wage_e = 1 if wage_pre < 7.2 
replace wage_e = 0 if wage_pre > 7.19 & wage_pre <= 10.8 

*interaction 
gen treat5= post6*wage_e


*Strategy 1: this is to compare 25yo and above to below 25 yo and seeing the differences in outcomes 

*creating samples 

global c1 "if jbsemp==1 & jbstat ==2 & jbterm1 == 1 & dvage > 20 & dvage <= 30 & jbhrs !=. & lfsat!=. & jbsat1 !=. & paysat !=." /*permanent sample*/ 
global c2 "if emp ==1 & dvage >20 & dvage <=30 & jbhrs !=. & lfsat!=. & jbsat1 !=. & paysat !=." /*all workers*/ 
global c3 "if emp ==1 & dvage >20 & dvage <=30 & low_wage ==1 & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=." /*all low wagers*/ 
global c4 "if emp ==1 & dvage >20 & dvage <=30  & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=. & post6 == 0" /*for descriptive statistics*/
global c5 "if emp ==1 & dvage >20 & dvage <=30  & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=. & post6 == 1" /*for descriptive statistics*/
*removed the low_wage condition as it stands right now
global c6 "if jbsemp==1 & jbstat ==2 & jbterm1 == 1 & dvage > 20 & jbhrs !=. & lfsat!=. & jbsat1 !=. & paysat !=." /*permanent sample of all people older than 20*/
global c7 "if emp ==1 & dvage >20 & jbhrs !=. & lfsat!=. & jbsat1 !=. & paysat !=." /*all workers older than 20*/
global c8 "if emp ==1 & dvage >20 & low_wage ==1 & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=." /*all low wagers older than 20*/ 
global c9 "if jbsemp==1 & jbstat ==2 & jbterm1 == 1 & dvage > 20 & low_wage ==1 & jbhrs !=. & lfsat!=. & jbsat1 !=. & paysat !=." /*permanent sample of all people older than 20 earning low wage*/
global c10 "if jbsemp==1 & jbstat ==2 & jbterm1 == 1 & dvage > 20 & dvage <= 30 & low_wage ==1 & jbhrs !=. & lfsat!=. & jbsat1 !=. & paysat !=." /*permanent sample of all people older than 20 & younger than 30 earning low wage*/
global c11 "if emp ==1 & dvage >20 & dvage <=30 & lowwage2 ==1 & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=."
global c12 "if emp ==1 & dvage >20 & dvage <=30  & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=." /*all wagers*/ 


*DiD: Cross-Sectional Approach 

lab var basrate "Hourly Wage Rate"
lab var post6 "Post"
lab var lfsat "Life Sat"
lab var jbhrs "Hours of Work"
lab var eligible "25yo+"
lab var female "Female"
lab var treat1 "Treated by Age"
lab var married_1 "Married/Couple"
lab var low_educ "Age 16+"
lab var med_educ "Age 18+"
lab var high_educ "University"
lab var pvt_sect "Private Sector"
lab var treat2 "Treated by Wage + Age"
lab var treat3 "Double Treated"
lab var dvage "Age"
lab var single_1 "Single"
lab var noeduc "No qual"
lab var jbsat1 "Job Sat"
lab var paysat "Pay Sat"
lab var treat5 "Treat*Post"
lab var wage_e "Treat"
lab var gor_dv "Region"
lab var region "Region"
lab var white "Race"

*To start with the other strategy of creating a different control group

global a1 "if jbsemp==1 & jbstat ==2 & jbterm1 == 1 & dvage > = 25 & jbhrs !=. & lfsat!=. & jbsat1 !=. & paysat !=." /*permanent sample*/ 
global a2 "if emp ==1 & dvage >= 25 & jbhrs !=. & lfsat!=. & jbsat1 !=. & paysat !=." /*all workers*/ 
global a3 "if emp ==1 & dvage >= 25 & low_wage ==1 & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=." /*all low wagers*/ 

global a6 "if jbsemp==1 & jbstat ==2 & jbterm1 == 1 & dvage >= 25 & low_wage == 1 & jbhrs !=. & lfsat!=. & jbsat1 !=. & paysat !=." /*permanent sample of all people older than 25 earning low wages*/	
global a7 "if emp ==1 & dvage >= 25 & low_wage ==1 & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=. & post == 1" /*for descriptive statistics*/
global a8 "if emp ==1 & dvage >= 25 & low_wage ==1 & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=. & post == 0" /*for descriptive statistics*/

global a9 "if emp ==1 & dvage >= 25 & low_wage ==1 & jbhrs >=30 & lfsat!=. & jbsat1 !=. & paysat !=." /*all low wages working equal to or more than 30hrs per week*/ 
global a10 "if jbsemp==1 & jbstat ==2 & jbterm1 == 1 & dvage >= 25 & low_wage == 1 & jbhrs >= 30 & lfsat!=. & jbsat1 !=. & paysat !=." /*permanent sample working equal to or more than 30hrs per week*/ 			
global a11 "if emp ==1 & dvage >= 25 & low_wage ==1 & jbhrs >=30 & pvt_sect == 1 & lfsat!=. & jbsat1 !=. & paysat !=." /*all low wagers, pvt sect, >30*/ 
global a12 "if jbsemp==1 & jbstat ==2 & jbterm1 == 1 & dvage >= 25 &  pvt_sect == 1 & low_wage == 1 & jbhrs >= 30 & lfsat!=. & jbsat1 !=. & paysat !=." /*perm low wagers, pvt sect, >30*/
global a13 "if emp ==1 & birthy <= 1991 & dvage <=65  & jbhrs !=. & lfsat!=. & jbsat1 !=. & paysat !=."


*some new thoughts:

*1. Should we look at only below 65 yo for the 2nd empirical strategy 
*2. Sample creation should include the birthy for identification of 25 and above rather than dvage for the same reasons 
*3. Does this change results?
 
*FOR DESCRIPTIVE STATISTICS: 
*OUR TREATED GROUP WAGE_E == 1 IS THE ONE THAT EARNED BELOW 7.2 IN THEIR PREVIOUS WAGE RATE 
*OUR CONTROL GROUP WAGE_E == 0 IS THE ONE THAT EARNED ABOVE 7.2 BUT UPTILL 10.8 IN THEIR PREVIOUS RATE 


global a4 "if emp ==1 & birthy <= 1991  & low_wage == 1 & dvage <= 65 & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=. & post6 == 0" /*for descriptive statistics*/
global a5 "if emp ==1 &  birthy <= 1991  & low_wage == 1 & dvage <=65 & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=. & post6 == 1" /*for descriptive statistics*/
global a14 "if emp ==1 & birthy<= 1991 & dvage <=65 & low_wage ==1 & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=." /*all low wagers*/ 
global a9 "if emp ==1 & birthy<=1991 & low_wage ==1 & dvage <= 65 & jbhrs >=30 & lfsat!=. & jbsat1 !=. & paysat !=." 
global a12 "if jbsemp==1 & jbstat ==2 & jbterm1 == 1 & birthy <= 1991 & dvage <= 65 &  pvt_sect == 1 & low_wage == 1 & jbhrs >= 30 & lfsat!=. & jbsat1 !=. & paysat !=." /*perm low wagers, pvt sect, >30*/
global a6 "if jbsemp==1 & jbstat ==2 & jbterm1 == 1 & birthy <= 1991 & dvage <= 65 & low_wage == 1 & jbhrs !=. & lfsat!=. & jbsat1 !=. & paysat !=." /*permanent sample of all people older than 25 earning low wages*/	



global c13 "if emp==1 & birthy<=1995 & birthy>=1968 & low_wage ==1 & jbhrs !=. & lfsat!=. & jbsat1 !=. & paysat!=. "
global c14 "if jbsemp==1 & jbstat ==2 & jbterm1 == 1 & birthy<=1995 & birthy>=1968 & pvt_sect == 1 & low_wage ==1 & jbhrs >=30 & lfsat!=. & jbsat1 !=. & paysat!=. "

global c4 "if emp ==1 & birthy<=1995 & birthy>=1968  & low_wage == 1  & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=. & post6 == 0" /*for descriptive statistics*/
global c5 "if emp ==1 &  birthy<=1995 & birthy>=1968  & low_wage == 1  & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=. & post6 == 1" /*for descriptive*/

global a15 "if emp ==1 & birthy<= 1991 & birthy >=1968 & low_wage ==1 & jbhrs!=. & lfsat!=. & jbsat1 !=. & paysat !=." /*all low wagers*/
