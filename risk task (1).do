*****************************************************
*					BEH Analysis					*
*													*
*				Risk Attitude Task					*
*													*
*													*
*****************************************************

clear

*Loading the data
global dir "E:\E Drive\Hilke Internship Work"
import delimited "$dir\Risk task\RiskData.csv", encoding(ISO-8859-1) clear
save "$dir\Risk task\RiskData.dta", replace
import delimited "$dir\Risk task\Risk_S1_alpha.csv", encoding(ISO-8859-1) clear
save "$dir\Risk task\Risk_S1_alpha.dta", replace
import delimited "$dir\Risk task\Risk_S2_alpha.csv", encoding(ISO-8859-1) clear
save "$dir\Risk task\Risk_S2_alpha.dta", replace


*Session 1 analysis
use "$dir\Risk task\RiskData.dta", clear
*count if ((s1_risk_01==2)&(group==5))
*44 Observation: Out of 48 Verum participants, 44 choose Certain Option
*
*	S1_Risk_01
*Group	1	2	Total
*			
*5	    4	44	48 
*6	    0	48	48 
*			
*Total	4	92	96 
*

*Labeling the options from 1,2 to Risky and Certain
label define choice 1"Risky" 2"Certain"
label values s1_risk_01 choice
label values s1_risk_02 choice
label values s1_risk_03 choice
label values s1_risk_04 choice
label values s1_risk_05 choice
label values s1_risk_06 choice
label values s1_risk_07 choice
label values s1_risk_08 choice
label values s1_risk_09 choice
label values s1_risk_10 choice
label values s1_risk_11 choice
label values s1_risk_12 choice
label values s1_risk_13 choice
label values s1_risk_14 choice
label values s1_risk_15 choice
label values s1_risk_16 choice
label values s1_risk_17 choice
label values s1_risk_18 choice
label values s1_risk_19 choice
label values s1_risk_20 choice
*Labeling the different groups 
label define group 5"Verum" 6"Placebo"
label values group group

label values s2_risk_01 choice
label values s2_risk_02 choice
label values s2_risk_03 choice
label values s2_risk_04 choice
label values s2_risk_05 choice
label values s2_risk_06 choice
label values s2_risk_07 choice
label values s2_risk_08 choice
label values s2_risk_09 choice
label values s2_risk_10 choice
label values s2_risk_11 choice
label values s2_risk_12 choice
label values s2_risk_13 choice
label values s2_risk_14 choice
label values s2_risk_15 choice
label values s2_risk_16 choice
label values s2_risk_17 choice
label values s2_risk_18 choice
label values s2_risk_19 choice
label values s2_risk_20 choice

*Task wise risk study
*graph pie, over(s1_risk_01) plabel(_all percent) by(, title("Risk Task 1_S1")) by(group)



tabulate group riskpercents1, column

sum riskpercents1 if group==5
*33.33% risky option chosen for Verum Group
sum riskpercents1 if group==6
*31.14% risky option chosen for Placebo Group
sum certainpercents1 if group==5
sum certainpercents1 if group==6

*graph box riskpercents1, yscale(range(0 80)) by(, title("Session 1")) by(group)

*graph box riskpercents2, yscale(range(0 80)) by(, title("Session 2")) by(group)

sort group

by group: egen med1 = median(riskpercents1)

by group: egen lqt1 = pctile(riskpercents1), p(25)

by group: egen uqt1 = pctile(riskpercents1), p(75)

by group: egen iqr1 = iqr(riskpercents1)

by group: egen mean1 = mean(riskpercents1)


twoway (rbar lqt1 mean1 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean1 uqt1 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter riskpercents1 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("Risky Choice %") title("Session 1") name(, replace)
graph save Graph "G:\E Drive\Hilke Internship Work\Risk task\Session 1.gph", replace

sort group

by group: egen med2 = median(riskpercents2)

by group: egen lqt2 = pctile(riskpercents2), p(25)

by group: egen uqt2 = pctile(riskpercents2), p(75)

by group: egen iqr2 = iqr(riskpercents2)

by group: egen mean2 = mean(riskpercents2)


twoway (rbar lqt2 mean2 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean2 uqt2 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter riskpercents2 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("Risky Choice %") title("Session 2") name(, replace)
graph save Graph "E:\E Drive\Hilke Internship Work\Risk task\Session 2.gph", replace

gen riskpercents21 = riskpercents2 - riskpercents1

sort group

by group: egen med21 = median(riskpercents21)

by group: egen lqt21 = pctile(riskpercents21), p(25)

by group: egen uqt21 = pctile(riskpercents21), p(75)

by group: egen iqr21 = iqr(riskpercents21)

by group: egen mean21 = mean(riskpercents21)


twoway (rbar lqt21 mean21 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean21 uqt21 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter riskpercents21 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("Risky Choice %") title("Difference (Session 2- Session 1)") name(s21, replace)
graph save Graph "E:\E Drive\Hilke Internship Work\Risk task\Session Difference.gph", replace


graph combine "E:\E Drive\Hilke Internship Work\Risk task\Session 1.gph" "E:\E Drive\Hilke Internship Work\Risk task\Session 2.gph", ycommon name(combined, replace)
graph save Graph "E:\E Drive\Hilke Internship Work\Risk task\combined.gph", replace

histogram riskpercents21 if group == 5, title("DiD (Verum)") name(, replace)

histogram riskpercents21 if group == 6, title("DiD (Placebo)") name(, replace)
 
ttest riskpercents21==0 if group ==5

ttest riskpercents21==0 if group ==6

ttest riskpercents21, by(group)

save "$dir\Risk task\RiskData.dta", replace


**************************************************************

*Correlating SS% with RC% per time point and the diff scores, plots
import delimited "$dir\Risk task\Discount_DTI.csv", encoding(ISO-8859-1) clear
save "$dir\Risk task\Discount_DTI.dta", replace
rename gb_id studyid

save "$dir\Risk task\Discount_DTI.dta", replace


use "$dir\Risk task\RiskData.dta", clear

merge 1:1 studyid using "$dir\Risk task\Discount_DTI.dta"

drop if _merge==2

gen percll_t21 = percll_t2 - percll_t1

twoway (lfit percll_t1 riskpercents1 if group==5) (scatter percll_t1 riskpercents1 if group==5), ytitle(Percentage of larger later) xtitle(Percent of Risky Choice) title("Verum_S1")
graph save Graph "$dir\Risk task\Graph_51.gph", replace

twoway (lfit percll_t1 riskpercents1 if group==6) (scatter percll_t1 riskpercents1 if group==6), ytitle(Percentage of larger later) xtitle(Percent of Risky Choice) title("Placebo_S1")
graph save Graph "$dir\Risk task\Graph_61.gph", replace
graph combine "$dir\Risk task\Graph_51.gph" "$dir\Risk task\Graph_61.gph", title("Session 1")

graph export "$dir\Risk task\Session 1_LL_RC.png", as(png) replace


twoway (lfit percll_t2 riskpercents2 if group==5) (scatter percll_t2 riskpercents2 if group==5), ytitle(Percentage of larger later) xtitle(Percent of Risky Choice) title("Verum_S2")
graph save Graph "$dir\Risk task\Graph_52.gph", replace

twoway (lfit percll_t2 riskpercents2 if group==6) (scatter percll_t2 riskpercents2 if group==6), ytitle(Percentage of larger later) xtitle(Percent of Risky Choice) title("Placebo_S2")
graph save Graph "$dir\Risk task\Graph_62.gph", replace
graph combine "$dir\Risk task\Graph_52.gph" "$dir\Risk task\Graph_62.gph", title("Session 2")

graph export "$dir\Risk task\Session 2_LL_RC.png", as(png) replace

twoway (lfit percll_t21 riskpercents21 if group==5) (scatter percll_t21 riskpercents21 if group==5), ytitle(Percentage of larger later) xtitle(Percent of Risky Choice) title("Verum_S2-S1")
graph save Graph "$dir\Risk task\Graph_521.gph", replace

twoway (lfit percll_t21 riskpercents21 if group==6) (scatter percll_t21 riskpercents21 if group==6), ytitle(Percentage of larger later) xtitle(Percent of Risky Choice) title("Placebo_S2-S1")
graph save Graph "$dir\Risk task\Graph_621.gph", replace
graph combine "$dir\Risk task\Graph_521.gph" "$dir\Risk task\Graph_621.gph", title("Session 2- Session 1")

graph export "$dir\Risk task\Session 2-1_LL_RC.png", as(png) replace

cor percll_t1 riskpercents1 if group==5
cor percll_t1 riskpercents1 if group==6
cor percll_t2 riskpercents2 if group==5
cor percll_t2 riskpercents2 if group==6
cor percll_t21 riskpercents21 if group==5
cor percll_t21 riskpercents21 if group==6

pwcorr percll_t1  riskpercents1 if group==5, sig star(5)
pwcorr percll_t1  riskpercents1 if group==6, sig star(5)
pwcorr percll_t2  riskpercents2 if group==5, sig star(5)
pwcorr percll_t2  riskpercents2 if group==6, sig star(5)
pwcorr percll_t21  riskpercents21 if group==5, sig star(5)
pwcorr percll_t21  riskpercents21 if group==6, sig star(5)

spearman percll_t1 riskpercents1 if group==5
spearman percll_t1 riskpercents1 if group==6
spearman percll_t2 riskpercents2 if group==5
spearman percll_t2 riskpercents2 if group==6
spearman percll_t21 riskpercents21 if group==5
spearman percll_t21 riskpercents21 if group==6



tabulate group, gen(n)
 
rename n1 verum_group

rename n2 placebo_group

reg percll_t21 verum_group

reg percll_t21 verum_group riskpercents21

twoway (scatter percll_t21 verum_group) (lfit percll_t21 verum_group)



**************************************************************
**********Calculating individual alpha************************
**************************************************************

*****Session 1*****
use "$dir\Risk task\Risk_S1_alpha.dta", clear

ds, has(type numeric)
foreach v in `r(varlist)' {
ipolate indifferencealpha `v' in 1/21, gen (alpha_`v') epolate
}


twoway (scatter gb001 indifferencealpha in 1/20, sort) (rline gb001 gb001 indifferencealpha in 1/20, sort)

*****Session 2*****
use "$dir\Risk task\Risk_S2_alpha.dta", clear

ds, has(type numeric)
foreach v in `r(varlist)' {
ipolate indifference_alpha_task `v' in 1/21, gen (alpha_`v') epolate
}


twoway (scatter gb001 indifference_alpha_task in 1/20, sort) (rline gb001 gb001 indifference_alpha_task in 1/20, sort)




*wsanova
*wsanova
*wsanova hunger time, id(SubjectID) bet(SubjectType)
*hist ssPicked_food, freq
*kdensity ssPicked_food, norm
*sktest ssPicked_food
*swilk  ssPicked_food


*********************************************************************

use "$dir\Risk task\RiskData.dta", clear

sort group

by group: egen med_1 = median(alpha_s1)

by group: egen lqt_1 = pctile(alpha_s1), p(25)

by group: egen uqt_1 = pctile(alpha_s1), p(75)

by group: egen iqr_1 = iqr(alpha_s1)

by group: egen mean_1 = mean(alpha_s1)


twoway (rbar lqt_1 mean_1 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean_1 uqt_1 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter alpha_s1 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("Alpha") title("Session 1") name(, replace)
graph save Graph "$dir\Risk task\Session_1.gph", replace

sort group

by group: egen med_2 = median(alpha_s2)

by group: egen lqt_2 = pctile(alpha_s2), p(25)

by group: egen uqt_2 = pctile(alpha_s2), p(75)

by group: egen iqr_2 = iqr(alpha_s2)

by group: egen mean_2 = mean(alpha_s2)


twoway (rbar lqt_2 mean_2 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean_2 uqt_2 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter alpha_s2 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("Alpha") title("Session 2") name(, replace)
graph save Graph "$dir\Risk task\Session_2.gph", replace

gen alpha_s21 = alpha_s2 - alpha_s1

sort group

by group: egen med_21 = median(alpha_s21)

by group: egen lqt_21 = pctile(alpha_s21), p(25)

by group: egen uqt_21 = pctile(alpha_s21), p(75)

by group: egen iqr_21 = iqr(alpha_s21)

by group: egen mean_21 = mean(alpha_s21)


twoway (rbar lqt_21 mean_21 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean_21 uqt_21 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter alpha_s21 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("Alpha") title("Difference (Session 2- Session 1)") name(s21, replace)
graph save Graph "$dir\Risk task\Session_Difference.gph", replace


graph combine "$dir\Risk task\Session_1.gph" "$dir\Risk task\Session_2.gph", ycommon name(combined, replace)
graph save Graph "$dir\Risk task\combined_21.gph", replace

histogram alpha_s21 if group == 5, title("DiD (Verum)") name(, replace)

histogram alpha_s21 if group == 6, title("DiD (Placebo)") name(, replace)
 
ttest alpha_s21==0 if group ==5

ttest alpha_s21==0 if group ==6

ttest alpha_s21, by(group)

save "$dir\Risk task\RiskData.dta", replace

**************************************************************

*Correlating k with alpha per time point and the diff scores, plots
import delimited "$dir\Risk task\Discount_DTI.csv", encoding(ISO-8859-1) clear
save "$dir\Risk task\Discount_DTI.dta", replace
rename gb_id studyid

save "$dir\Risk task\Discount_DTI.dta", replace


use "$dir\Risk task\RiskData.dta", clear

merge 1:1 studyid using "$dir\Risk task\Discount_DTI.dta"

drop if _merge==2

gen logk_t21 = logk_t2 - logk_t1
 
twoway (lfit logk_t1 alpha_s1 if group==5) (scatter logk_t1 alpha_s1 if group==5), ytitle(k-parameter) xtitle(alpha) title("Verum_S1")
graph save Graph "$dir\Risk task\Graph_051.gph", replace

twoway (lfit logk_t1 alpha_s1 if group==6) (scatter logk_t1 alpha_s1 if group==6), ytitle(k-parameter) xtitle(alpha) title("Placebo_S1")
graph save Graph "$dir\Risk task\Graph_061.gph", replace
graph combine "$dir\Risk task\Graph_051.gph" "$dir\Risk task\Graph_061.gph", title("Session 1")

graph export "$dir\Risk task\Session 1_k_a.png", as(png) replace


twoway (lfit logk_t2 alpha_s2 if group==5) (scatter logk_t2 alpha_s2 if group==5), ytitle(k-parameter) xtitle(alpha) title("Verum_S2")
graph save Graph "$dir\Risk task\Graph_052.gph", replace

twoway (lfit logk_t2 alpha_s2 if group==6) (scatter logk_t2 alpha_s2 if group==6), ytitle(k-parameter) xtitle(alpha) title("Placebo_S2")
graph save Graph "$dir\Risk task\Graph_062.gph", replace
graph combine "$dir\Risk task\Graph_052.gph" "$dir\Risk task\Graph_062.gph", title("Session 2")

graph export "$dir\Risk task\Session 2_k_a.png", as(png) replace

twoway (lfit logk_t21 alpha_s21 if group==5) (scatter logk_t21 alpha_s21 if group==5), ytitle(k-parameter) xtitle(alpha) title("Verum_S2-S1")
graph save Graph "$dir\Risk task\Graph_0521.gph", replace

twoway (lfit logk_t21 alpha_s21 if group==6) (scatter logk_t21 alpha_s21 if group==6), ytitle(k-parameter) xtitle(alpha) title("Placebo_S2-S1")
graph save Graph "$dir\Risk task\Graph_0621.gph", replace
graph combine "$dir\Risk task\Graph_0521.gph" "$dir\Risk task\Graph_0621.gph", title("Session 2- Session 1")

*graph export "$dir\Risk task\Session 2-1_k_a.png", as(png) replace

cor logk_t1 alpha_s1 if group==5
cor logk_t1 alpha_s1 if group==6
cor logk_t2 alpha_s2 if group==5
cor logk_t2 alpha_s2 if group==6
cor logk_t21 alpha_s21 if group==5
cor logk_t21 alpha_s21 if group==6

pwcorr logk_t1 alpha_s1 if group==5, sig star(5)
pwcorr logk_t1 alpha_s1 if group==6, sig star(5)
pwcorr logk_t2 alpha_s2 if group==5, sig star(5)
pwcorr logk_t2 alpha_s2 if group==6, sig star(5)
pwcorr logk_t21 alpha_s21 if group==5, sig star(5)
pwcorr logk_t21 alpha_s21 if group==6, sig star(5)

spearman logk_t1 alpha_s1 if group==5
spearman logk_t1 alpha_s1 if group==6
spearman logk_t2 alpha_s2 if group==5
spearman logk_t2 alpha_s2 if group==6
spearman logk_t21 alpha_s21 if group==5
spearman logk_t21 alpha_s21 if group==6


tabulate group, gen(n)
 
rename n1 verum_group

rename n2 placebo_group

reg logk_t21 verum_group

reg logk_t21 verum_group alpha_s21 

reg logk_t21 verum_group alpha_s21 age income educ

reg alpha_s21 verum_group 

reg alpha_s21 verum_group age income educ 

reg alpha_s21 verum_group logk_t21

reg alpha_s21 verum_group logk_t21 age income educ 

twoway (scatter logk_t21 verum_group) (lfit logk_t21 verum_group)

save "$dir\Risk task\RiskData.dta", replace




*********************************************************************************


***********************  TRANSITIVITY TASK   ************************************


*********************************************************************************

*Loading the data
import delimited "$dir\Transitivity task\Transitivity data.csv", encoding(ISO-8859-1) clear
save "$dir\Transitivity task\Transitivity data.dta", replace

*Session 1 analysis
use "$dir\Transitivity task\Transitivity data.dta", clear

*Difference in score given to tasks by the subject_ Session 1 for mugs
gen task1_2_m = s1_trans_mug_points_1 - s1_trans_mug_points_2
gen task1_3_m = s1_trans_mug_points_1 - s1_trans_mug_points_3
gen task1_4_m = s1_trans_mug_points_1 - s1_trans_mug_points_4
gen task1_5_m = s1_trans_mug_points_1 - s1_trans_mug_points_5
gen task2_3_m = s1_trans_mug_points_2 - s1_trans_mug_points_3
gen task2_4_m = s1_trans_mug_points_2 - s1_trans_mug_points_4
gen task2_5_m = s1_trans_mug_points_2 - s1_trans_mug_points_5
gen task3_4_m = s1_trans_mug_points_3 - s1_trans_mug_points_4
gen task3_5_m = s1_trans_mug_points_3 - s1_trans_mug_points_5
gen task4_5_m = s1_trans_mug_points_4 - s1_trans_mug_points_5

*logical operation to count the number of errors in ordering. 1 = inconsistency in tranitivity task, 0 = otherwise_ (Session 1 for mugs)
gen task_01_m1_score = 1 if s1_trans_mug_01 == 1 & task1_2_m < 0 | s1_trans_mug_01 == 2 & task1_2_m > 0
gen task_02_m1_score = 1 if s1_trans_mug_02 == 1 & task1_3_m < 0 | s1_trans_mug_01 == 3 & task1_3_m > 0
gen task_03_m1_score = 1 if s1_trans_mug_03 == 1 & task1_4_m < 0 | s1_trans_mug_01 == 4 & task1_4_m > 0
gen task_04_m1_score = 1 if s1_trans_mug_04 == 1 & task1_5_m < 0 | s1_trans_mug_01 == 5 & task1_5_m > 0
gen task_05_m1_score = 1 if s1_trans_mug_05 == 2 & task2_3_m < 0 | s1_trans_mug_01 == 3 & task2_3_m > 0
gen task_06_m1_score = 1 if s1_trans_mug_06 == 2 & task2_4_m < 0 | s1_trans_mug_01 == 4 & task2_4_m > 0
gen task_07_m1_score = 1 if s1_trans_mug_07 == 2 & task2_5_m < 0 | s1_trans_mug_01 == 5 & task2_5_m > 0
gen task_08_m1_score = 1 if s1_trans_mug_08 == 3 & task3_4_m < 0 | s1_trans_mug_01 == 4 & task3_4_m > 0
gen task_09_m1_score = 1 if s1_trans_mug_09 == 3 & task3_5_m < 0 | s1_trans_mug_01 == 5 & task3_5_m > 0
gen task_10_m1_score = 1 if s1_trans_mug_10 == 4 & task4_5_m < 0 | s1_trans_mug_01 == 5 & task4_5_m > 0

replace task_01_m1_score = 0 if task_01_m1_score == .
replace task_02_m1_score = 0 if task_02_m1_score == .
replace task_03_m1_score = 0 if task_03_m1_score == .
replace task_04_m1_score = 0 if task_04_m1_score == .
replace task_05_m1_score = 0 if task_05_m1_score == .
replace task_06_m1_score = 0 if task_06_m1_score == .
replace task_07_m1_score = 0 if task_07_m1_score == .
replace task_08_m1_score = 0 if task_08_m1_score == .
replace task_09_m1_score = 0 if task_09_m1_score == .
replace task_10_m1_score = 0 if task_10_m1_score == .

gen error_score_m1 = task_01_m1_score + task_02_m1_score + task_03_m1_score + task_04_m1_score + task_05_m1_score + task_06_m1_score + task_07_m1_score + task_08_m1_score + task_09_m1_score + task_10_m1_score


*Session 2 analysis

*Difference in score given to tasks by the subject_ Session 2 for mugs

gen task1_2_m2 = s2_trans_mug_points_1 - s2_trans_mug_points_2
gen task1_3_m2 = s2_trans_mug_points_1 - s2_trans_mug_points_3
gen task1_4_m2 = s2_trans_mug_points_1 - s2_trans_mug_points_4
gen task1_5_m2 = s2_trans_mug_points_1 - s2_trans_mug_points_5
gen task2_3_m2 = s2_trans_mug_points_2 - s2_trans_mug_points_3
gen task2_4_m2 = s2_trans_mug_points_2 - s2_trans_mug_points_4
gen task2_5_m2 = s2_trans_mug_points_2 - s2_trans_mug_points_5
gen task3_4_m2 = s2_trans_mug_points_3 - s2_trans_mug_points_4
gen task3_5_m2 = s2_trans_mug_points_3 - s2_trans_mug_points_5
gen task4_5_m2 = s2_trans_mug_points_4 - s2_trans_mug_points_5

*logical operation to count the number of errors in ordering. 1 = inconsistency in tranitivity task, 0 = otherwise (Session 2 for mugs)
gen task_01_m2_score = 1 if s2_trans_mug_01 == 1 & task1_2_m2 < 0 | s2_trans_mug_01 == 2 & task1_2_m2 > 0
gen task_02_m2_score = 1 if s2_trans_mug_02 == 1 & task1_3_m2 < 0 | s2_trans_mug_01 == 3 & task1_3_m2 > 0
gen task_03_m2_score = 1 if s2_trans_mug_03 == 1 & task1_4_m2 < 0 | s2_trans_mug_01 == 4 & task1_4_m2 > 0
gen task_04_m2_score = 1 if s2_trans_mug_04 == 1 & task1_5_m2 < 0 | s2_trans_mug_01 == 5 & task1_5_m2 > 0
gen task_05_m2_score = 1 if s2_trans_mug_05 == 2 & task2_3_m2 < 0 | s2_trans_mug_01 == 3 & task2_3_m2 > 0
gen task_06_m2_score = 1 if s2_trans_mug_06 == 2 & task2_4_m2 < 0 | s2_trans_mug_01 == 4 & task2_4_m2 > 0
gen task_07_m2_score = 1 if s2_trans_mug_07 == 2 & task2_5_m2 < 0 | s2_trans_mug_01 == 5 & task2_5_m2 > 0
gen task_08_m2_score = 1 if s2_trans_mug_08 == 3 & task3_4_m2 < 0 | s2_trans_mug_01 == 4 & task3_4_m2 > 0
gen task_09_m2_score = 1 if s2_trans_mug_09 == 3 & task3_5_m2 < 0 | s2_trans_mug_01 == 5 & task3_5_m2 > 0
gen task_10_m2_score = 1 if s2_trans_mug_10 == 4 & task4_5_m2 < 0 | s2_trans_mug_01 == 5 & task4_5_m2 > 0

replace task_01_m2_score = 0 if task_01_m2_score == .
replace task_02_m2_score = 0 if task_02_m2_score == .
replace task_03_m2_score = 0 if task_03_m2_score == .
replace task_04_m2_score = 0 if task_04_m2_score == .
replace task_05_m2_score = 0 if task_05_m2_score == .
replace task_06_m2_score = 0 if task_06_m2_score == .
replace task_07_m2_score = 0 if task_07_m2_score == .
replace task_08_m2_score = 0 if task_08_m2_score == .
replace task_09_m2_score = 0 if task_09_m2_score == .
replace task_10_m2_score = 0 if task_10_m2_score == .

gen error_score_m2 = task_01_m2_score + task_02_m2_score + task_03_m2_score + task_04_m2_score + task_05_m2_score + task_06_m2_score + task_07_m2_score + task_08_m2_score + task_09_m2_score + task_10_m2_score

gen error_score_m21 = error_score_m2 - error_score_m1

histogram error_score_m1 if group == 5, title("DiD (Verum)") name(, replace)


***************    POSTCARDS   ***********************
*Difference in score given to tasks by the subject_ Session 1 for postcards
gen task1_2_p = s1_trans_post_points_1 - s1_trans_post_points_2
gen task1_3_p = s1_trans_post_points_1 - s1_trans_post_points_3
gen task1_4_p = s1_trans_post_points_1 - s1_trans_post_points_4
gen task1_5_p = s1_trans_post_points_1 - s1_trans_post_points_5
gen task2_3_p = s1_trans_post_points_2 - s1_trans_post_points_3
gen task2_4_p = s1_trans_post_points_2 - s1_trans_post_points_4
gen task2_5_p = s1_trans_post_points_2 - s1_trans_post_points_5
gen task3_4_p = s1_trans_post_points_3 - s1_trans_post_points_4
gen task3_5_p = s1_trans_post_points_3 - s1_trans_post_points_5
gen task4_5_p = s1_trans_post_points_4 - s1_trans_post_points_5

*logical operation to count the number of errors in ordering. 1 = inconsistency in tranitivity task, 0 = otherwise_ (Session 1 for postcardss)
gen task_01_p1_score = 1 if s1_trans_post_01 == 1 & task1_2_p < 0 | s1_trans_post_01 == 2 & task1_2_p > 0
gen task_02_p1_score = 1 if s1_trans_post_02 == 1 & task1_3_p < 0 | s1_trans_post_01 == 3 & task1_3_p > 0
gen task_03_p1_score = 1 if s1_trans_post_03 == 1 & task1_4_p < 0 | s1_trans_post_01 == 4 & task1_4_p > 0
gen task_04_p1_score = 1 if s1_trans_post_04 == 1 & task1_5_p < 0 | s1_trans_post_01 == 5 & task1_5_p > 0
gen task_05_p1_score = 1 if s1_trans_post_05 == 2 & task2_3_p < 0 | s1_trans_post_01 == 3 & task2_3_p > 0
gen task_06_p1_score = 1 if s1_trans_post_06 == 2 & task2_4_p < 0 | s1_trans_post_01 == 4 & task2_4_p > 0
gen task_07_p1_score = 1 if s1_trans_post_07 == 2 & task2_5_p < 0 | s1_trans_post_01 == 5 & task2_5_p > 0
gen task_08_p1_score = 1 if s1_trans_post_08 == 3 & task3_4_p < 0 | s1_trans_post_01 == 4 & task3_4_p > 0
gen task_09_p1_score = 1 if s1_trans_post_09 == 3 & task3_5_p < 0 | s1_trans_post_01 == 5 & task3_5_p > 0
gen task_10_p1_score = 1 if s1_trans_post_10 == 4 & task4_5_p < 0 | s1_trans_post_01 == 5 & task4_5_p > 0

replace task_01_p1_score = 0 if task_01_p1_score == .
replace task_02_p1_score = 0 if task_02_p1_score == .
replace task_03_p1_score = 0 if task_03_p1_score == .
replace task_04_p1_score = 0 if task_04_p1_score == .
replace task_05_p1_score = 0 if task_05_p1_score == .
replace task_06_p1_score = 0 if task_06_p1_score == .
replace task_07_p1_score = 0 if task_07_p1_score == .
replace task_08_p1_score = 0 if task_08_p1_score == .
replace task_09_p1_score = 0 if task_09_p1_score == .
replace task_10_p1_score = 0 if task_10_p1_score == .

gen error_score_p1 = task_01_p1_score + task_02_p1_score + task_03_p1_score + task_04_p1_score + task_05_p1_score + task_06_p1_score + task_07_p1_score + task_08_p1_score + task_09_p1_score + task_10_p1_score


*Session 2 analysis

*Difference in score given to tasks by the subject_ Session 2 for postcards

gen task1_2_p2 = s2_trans_post_points_1 - s2_trans_post_points_2
gen task1_3_p2 = s2_trans_post_points_1 - s2_trans_post_points_3
gen task1_4_p2 = s2_trans_post_points_1 - s2_trans_post_points_4
gen task1_5_p2 = s2_trans_post_points_1 - s2_trans_post_points_5
gen task2_3_p2 = s2_trans_post_points_2 - s2_trans_post_points_3
gen task2_4_p2 = s2_trans_post_points_2 - s2_trans_post_points_4
gen task2_5_p2 = s2_trans_post_points_2 - s2_trans_post_points_5
gen task3_4_p2 = s2_trans_post_points_3 - s2_trans_post_points_4
gen task3_5_p2 = s2_trans_post_points_3 - s2_trans_post_points_5
gen task4_5_p2 = s2_trans_post_points_4 - s2_trans_post_points_5

*logical operation to count the number of errors in ordering. 1 = inconsistency in tranitivity task, 0 = otherwise (Session 2 for postcards)
gen task_01_p2_score = 1 if s2_trans_post_01 == 1 & task1_2_p2 < 0 | s2_trans_post_01 == 2 & task1_2_p2 > 0
gen task_02_p2_score = 1 if s2_trans_post_02 == 1 & task1_3_p2 < 0 | s2_trans_post_01 == 3 & task1_3_p2 > 0
gen task_03_p2_score = 1 if s2_trans_post_03 == 1 & task1_4_p2 < 0 | s2_trans_post_01 == 4 & task1_4_p2 > 0
gen task_04_p2_score = 1 if s2_trans_post_04 == 1 & task1_5_p2 < 0 | s2_trans_post_01 == 5 & task1_5_p2 > 0
gen task_05_p2_score = 1 if s2_trans_post_05 == 2 & task2_3_p2 < 0 | s2_trans_post_01 == 3 & task2_3_p2 > 0
gen task_06_p2_score = 1 if s2_trans_post_06 == 2 & task2_4_p2 < 0 | s2_trans_post_01 == 4 & task2_4_p2 > 0
gen task_07_p2_score = 1 if s2_trans_post_07 == 2 & task2_5_p2 < 0 | s2_trans_post_01 == 5 & task2_5_p2 > 0
gen task_08_p2_score = 1 if s2_trans_post_08 == 3 & task3_4_p2 < 0 | s2_trans_post_01 == 4 & task3_4_p2 > 0
gen task_09_p2_score = 1 if s2_trans_post_09 == 3 & task3_5_p2 < 0 | s2_trans_post_01 == 5 & task3_5_p2 > 0
gen task_10_p2_score = 1 if s2_trans_post_10 == 4 & task4_5_p2 < 0 | s2_trans_post_01 == 5 & task4_5_p2 > 0

replace task_01_p2_score = 0 if task_01_p2_score == .
replace task_02_p2_score = 0 if task_02_p2_score == .
replace task_03_p2_score = 0 if task_03_p2_score == .
replace task_04_p2_score = 0 if task_04_p2_score == .
replace task_05_p2_score = 0 if task_05_p2_score == .
replace task_06_p2_score = 0 if task_06_p2_score == .
replace task_07_p2_score = 0 if task_07_p2_score == .
replace task_08_p2_score = 0 if task_08_p2_score == .
replace task_09_p2_score = 0 if task_09_p2_score == .
replace task_10_p2_score = 0 if task_10_p2_score == .

gen error_score_p2 = task_01_p2_score + task_02_p2_score + task_03_p2_score + task_04_p2_score + task_05_p2_score + task_06_p2_score + task_07_p2_score + task_08_p2_score + task_09_p2_score + task_10_p2_score

gen error_score_1 = error_score_m1 + error_score_p1

gen error_score_2 = error_score_m2 + error_score_p2

gen error_score_21 = error_score_2 - error_score_1

histogram error_score_21 if group == 5, title("DiD (Verum)") name(, replace)

histogram error_score_21 if group == 6, title("DiD (Placebo)") name(, replace)

*For graphs-Session 1
sort group

by group: egen med1 = median(error_score_1)

by group: egen lqt1 = pctile(error_score_1), p(25)

by group: egen uqt1 = pctile(error_score_1), p(75)

by group: egen iqr1 = iqr(error_score_1)

by group: egen mean1 = mean(error_score_1)


twoway (rbar lqt1 mean1 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean1 uqt1 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter error_score_1 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("# of Inconsistent Choices ") title("Session 1") name(, replace)
graph save Graph "G:\E Drive\Hilke Internship Work\Transitivity task\Session 1.gph", replace

**session 2
sort group

by group: egen med2 = median(error_score_2)

by group: egen lqt2 = pctile(error_score_2), p(25)

by group: egen uqt2 = pctile(error_score_2), p(75)

by group: egen iqr2 = iqr(error_score_2)

by group: egen mean2 = mean(error_score_2)


twoway (rbar lqt1 mean1 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean1 uqt1 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter error_score_2 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("# of Inconsistent Choices ") title("Session 2") name(, replace)
graph save Graph "G:\E Drive\Hilke Internship Work\Transitivity task\Session 2.gph", replace

**diff-diff graph

sort group

by group: egen med21 = median(error_score_21)

by group: egen lqt21 = pctile(error_score_21), p(25)

by group: egen uqt21 = pctile(error_score_21), p(75)

by group: egen iqr21 = iqr(error_score_21)

by group: egen mean21 = mean(error_score_21)


twoway (rbar lqt1 mean1 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean1 uqt1 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter error_score_21 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("# of Inconsistent Choices ") title("Session 2- Session 1 (Diff-in-Diff)") name(, replace)
graph save Graph "G:\E Drive\Hilke Internship Work\Transitivity task\Session Difference.gph", replace


graph combine "G:\E Drive\Hilke Internship Work\Transitivity task\Session 1.gph" "G:\E Drive\Hilke Internship Work\Transitivity task\Session 2.gph", ycommon name(combined, replace)
graph save Graph "G:\E Drive\Hilke Internship Work\Transitivity task\combined.gph", replace

ttest error_score_21==0 if group ==5

ttest error_score_21==0 if group ==6

ttest error_score_21, by(group)


save "$dir\Transitivity task\Transitivity data.dta", replace

********************************************************************
****    Merging transitivty task into risk + ITC task   ************
********************************************************************

use "$dir\Risk task\RiskData.dta", clear

drop _merge

merge 1:1 studyid using "$dir\Transitivity task\Transitivity data.dta"

cor logk_t1 error_score_1 if group==5
cor logk_t1 error_score_1 if group==6
cor logk_t2 error_score_2 if group==5
cor logk_t2 error_score_2 if group==6
cor logk_t21 error_score_21 if group==5
cor logk_t21 error_score_21 if group==6


pwcorr logk_t1 error_score_1 if group==5, sig star(5)
pwcorr logk_t1 error_score_1 if group==6, sig star(5)
pwcorr logk_t2 error_score_2 if group==5, sig star(5)
pwcorr logk_t2 error_score_2 if group==6, sig star(5)
pwcorr logk_t21 error_score_21 if group==5, sig star(5)
pwcorr logk_t21 error_score_21 if group==6, sig star(5)


twoway (lfit logk_t21 error_score_21 if group==5) (scatter logk_t21 error_score_21 if group==5), ytitle(k-parameter) xtitle(inconsistent preferences) title("Verum_S2-S1")
graph save Graph "$dir\Transitivity task\Graph_0521.gph", replace

twoway (lfit logk_t21 error_score_21 if group==6) (scatter logk_t21 error_score_21 if group==6), ytitle(k-parameter) xtitle(inconsistent preferences) title("Placebo_S2-S1")
graph save Graph "$dir\Transitivity task\Graph_0621.gph", replace
graph combine "$dir\Transitivity task\Graph_0521.gph" "$dir\Transitivity task\Graph_0621.gph", title("Session 2- Session 1")


reg logk_t21 verum_group alpha_s21 error_score_21

save "$dir\Risk task\RiskData.dta", replace


*********************************************************************************


*******************************  CRT TASK   *************************************


*********************************************************************************

clear

*Loading the data
global dir "G:\E Drive\Hilke Internship Work"
import delimited "$dir\CRT\CRT_Data.csv"
save "$dir\CRT\CRT_Data.dta", replace

use "$dir\CRT\CRT_Data.dta", clear

gen s1_1_score = 1 if s1_crt_01_1_1 == 5
gen s1_2_score = 1 if s1_crt_02_1_1 == 47
gen s1_3_score = 1 if s1_crt_03_1_1 == 2
gen s1_4_score = 1 if s1_crt_04_1_1 == 8
gen s1_5_score = 1 if s1_crt_05_1_1=="Anne"
gen s1_6_score = 1 if s1_crt_06_1_1 == 27


replace s1_1_score = 0 if s1_1_score == .
replace s1_2_score = 0 if s1_2_score == .
replace s1_3_score = 0 if s1_3_score == .
replace s1_4_score = 0 if s1_4_score == .
replace s1_5_score = 0 if s1_5_score == .
replace s1_6_score = 0 if s1_6_score == .


gen s2_1_score = 1 if s2_crt_01_1_1 == 5
gen s2_2_score = 1 if s2_crt_02_1_1 == 47
gen s2_3_score = 1 if s2_crt_03_1_1 == 2
gen s2_4_score = 1 if s2_crt_04_1_1 == 8
gen s2_5_score = 1 if s2_crt_05_1_1=="Anne"
gen s2_6_score = 1 if s2_crt_06_1_1 == 27


replace s2_1_score = 0 if s2_1_score == .
replace s2_2_score = 0 if s2_2_score == .
replace s2_3_score = 0 if s2_3_score == .
replace s2_4_score = 0 if s2_4_score == .
replace s2_5_score = 0 if s2_5_score == .
replace s2_6_score = 0 if s2_6_score == .

gen s1_score_mistake = 3 - (s1_1_score + s1_2_score + s1_3_score + s1_4_score + s1_5_score + s1_6_score)
gen s2_score_mistake = 3 - (s2_1_score + s2_2_score + s2_3_score + s2_4_score + s2_5_score + s2_6_score)


gen CRT_error_score_21 = s2_score_mistake - s1_score_mistake

histogram CRT_error_score_21 if group == 5, title("DiD (Verum)") name(, replace)

kdensity CRT_error_score_21 if group == 5, title("DiD (Verum)") name(, replace)

histogram CRT_error_score_21 if group == 6, title("DiD (Placebo)") name(, replace)

kdensity CRT_error_score_21 if group == 6, title("DiD (Placebo)") name(, replace)

ttest CRT_error_score_21==0 if group ==5

ttest CRT_error_score_21==0 if group ==6

ttest CRT_error_score_21, by(group)

*For graphs-Session 1
sort group

by group: egen med1 = median(s1_score_mistake)

by group: egen lqt1 = pctile(s1_score_mistake), p(25)

by group: egen uqt1 = pctile(s1_score_mistake), p(75)

by group: egen iqr1 = iqr(s1_score_mistake)

by group: egen mean1 = mean(s1_score_mistake)


twoway (rbar lqt1 mean1 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean1 uqt1 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter s1_score_mistake group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("# of mistakes") title("Session 1") name(, replace)
graph save Graph "G:\E Drive\Hilke Internship Work\CRT\Session 1.gph", replace

**session 2
sort group

by group: egen med2 = median(s2_score_mistake)

by group: egen lqt2 = pctile(s2_score_mistake), p(25)

by group: egen uqt2 = pctile(s2_score_mistake), p(75)

by group: egen iqr2 = iqr(s2_score_mistake)

by group: egen mean2 = mean(s2_score_mistake)


twoway (rbar lqt1 mean1 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean1 uqt1 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter s2_score_mistake group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("# of mistakes ") title("Session 2") name(, replace)
graph save Graph "G:\E Drive\Hilke Internship Work\CRT\Session 2.gph", replace

**diff-diff graph

sort group

by group: egen med21 = median(CRT_error_score_21)

by group: egen lqt21 = pctile(CRT_error_score_21), p(25)

by group: egen uqt21 = pctile(CRT_error_score_21), p(75)

by group: egen iqr21 = iqr(CRT_error_score_21)

by group: egen mean21 = mean(CRT_error_score_21)


twoway (rbar lqt1 mean1 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean1 uqt1 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter CRT_error_score_21 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("# of mistakes ") title("Session 2- Session 1 (Diff-in-Diff)") name(, replace)
graph save Graph "G:\E Drive\Hilke Internship Work\CRT\Session Difference.gph", replace


graph combine "G:\E Drive\Hilke Internship Work\CRT\Session 1.gph" "G:\E Drive\Hilke Internship Work\CRT\Session 2.gph", ycommon name(combined, replace)
graph save Graph "G:\E Drive\Hilke Internship Work\CRT\combined.gph", replace


save "$dir\CRT\CRT_Data.dta", replace

********************************************************************

*******   Merging CRT task into RISK + ITC + Transtivity   *********

********************************************************************

use "$dir\Risk task\RiskData.dta", clear

drop _merge

merge 1:1 studyid using "$dir\CRT\CRT_Data.dta"

cor logk_t1 s1_score_mistake if group==5
cor logk_t1 s1_score_mistake if group==6
cor logk_t2 s2_score_mistake if group==5
cor logk_t2 s2_score_mistake if group==6
cor logk_t21 CRT_error_score_21 if group==5
cor logk_t21 CRT_error_score_21 if group==6


pwcorr logk_t1 s1_score_mistake if group==5, sig star(5)
pwcorr logk_t1 s1_score_mistake if group==6, sig star(5)
pwcorr logk_t2 s2_score_mistake if group==5, sig star(5)
pwcorr logk_t2 s2_score_mistake if group==6, sig star(5)
pwcorr logk_t21 CRT_error_score_21 if group==5, sig star(5)
pwcorr logk_t21 CRT_error_score_21 if group==6, sig star(5)


twoway (lfit logk_t21 CRT_error_score_21 if group==5) (scatter logk_t21 CRT_error_score_21 if group==5), ytitle(k-parameter) xtitle(CRT) title("Verum_S2-S1")
graph save Graph "$dir\CRT\Graph_0521.gph", replace

twoway (lfit logk_t21 CRT_error_score_21 if group==6) (scatter logk_t21 CRT_error_score_21 if group==6), ytitle(k-parameter) xtitle(CRT) title("Placebo_S2-S1")
graph save Graph "$dir\CRT\Graph_0621.gph", replace
graph combine "$dir\CRT\Graph_0521.gph" "$dir\CRT\Graph_0621.gph", title("Session 2- Session 1")


reg logk_t21 verum_group alpha_s21 error_score_21 CRT_error_score_21

reg logk_t21 verum_group alpha_s21 error_score_21 CRT_error_score_21 age educ income

*********************************************************************************


*****************************  HEDONIC TASK   ***********************************


*********************************************************************************


*Loading the data
import delimited "$dir\Hedonic Task\Hedonic Task.csv", encoding(ISO-8859-1) clear
save "$dir\Hedonic Task\Hedonic Task.dta", replace

*manipulation check for trail 1 vs trail 20
sum s1_1_hedonic_pizza_enjoy_1 
sum s1_20_hedonic_pizza_enjoy_1 
ttest s1_1_hedonic_pizza_enjoy_1 == s1_20_hedonic_pizza_enjoy_1

sum s1_1_hedonic_steak_enjoy_1 
sum s1_20_hedonic_steak_enjoy_1 
ttest s1_1_hedonic_steak_enjoy_1 == s1_20_hedonic_steak_enjoy_1 

ttest s2_1_hedonic_pizza_enjoy_1 == s2_20_hedonic_pizza_enjoy_1
ttest s2_1_hedonic_steak_enjoy_1 == s2_20_hedonic_steak_enjoy_1

graph bar (mean) s1_1_hedonic_pizza_enjoy_1 (mean) s1_20_hedonic_pizza_enjoy_1, bargap(2)

*manipulation check group wise
ttest s1_1_hedonic_pizza_enjoy_1, by(group)
ttest s2_1_hedonic_pizza_enjoy_1, by(group)
ttest s1_1_hedonic_steak_enjoy_1 , by(group)
ttest s2_1_hedonic_steak_enjoy_1 , by(group)

*Manipulation check (anchoring bias)
tab1 s1_anchoring_pizza_low s1_anchoring_pizza_high
tab1 s1_anchoring_steak_low s1_anchoring_steak_high

*sort group

*by group: egen med_1 = median(s1_1_hedonic_pizza_enjoy_1)
*by group: egen lqt_1 = pctile(s1_1_hedonic_pizza_enjoy_1), p(25)
*by group: egen uqt_1 = pctile(s1_1_hedonic_pizza_enjoy_1), p(75)
*by group: egen iqr_1 = iqr(s1_1_hedonic_pizza_enjoy_1)
*by group: egen mean_1 = mean(s1_1_hedonic_pizza_enjoy_1)

*twoway (rbar lqt_1 mean_1 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean_1 uqt_1 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter s1_1_hedonic_pizza_enjoy_1 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("Alpha") title("Session 1") name(, replace)

***********Modified version---Steak plus pizza*******************88

gen s1_trial_1 = (s1_1_hedonic_pizza_enjoy_1 + s1_1_hedonic_steak_enjoy_1)/2
gen s1_trial_2 = (s1_2_hedonic_pizza_enjoy_1 + s1_2_hedonic_steak_enjoy_1)/2
gen s1_trial_3 = (s1_3_hedonic_pizza_enjoy_1 + s1_3_hedonic_steak_enjoy_1)/2
gen s1_trial_4 = (s1_4_hedonic_pizza_enjoy_1 + s1_4_hedonic_steak_enjoy_1)/2
gen s1_trial_5 = (s1_5_hedonic_pizza_enjoy_1 + s1_5_hedonic_steak_enjoy_1)/2
gen s1_trial_6 = (s1_6_hedonic_pizza_enjoy_1 + s1_6_hedonic_steak_enjoy_1)/2
gen s1_trial_7 = (s1_7_hedonic_pizza_enjoy_1 + s1_7_hedonic_steak_enjoy_1)/2
gen s1_trial_8 = (s1_8_hedonic_pizza_enjoy_1 + s1_8_hedonic_steak_enjoy_1)/2
gen s1_trial_9 = (s1_9_hedonic_pizza_enjoy_1 + s1_9_hedonic_steak_enjoy_1)/2
gen s1_trial_10 = (s1_10_hedonic_pizza_enjoy_1 + s1_10_hedonic_steak_enjoy_1)/2
gen s1_trial_11 = (s1_11_hedonic_pizza_enjoy_1 + s1_11_hedonic_steak_enjoy_1)/2
gen s1_trial_12 = (s1_12_hedonic_pizza_enjoy_1 + s1_12_hedonic_steak_enjoy_1)/2
gen s1_trial_13 = (s1_13_hedonic_pizza_enjoy_1 + s1_13_hedonic_steak_enjoy_1)/2
gen s1_trial_14 = (s1_14_hedonic_pizza_enjoy_1 + s1_14_hedonic_steak_enjoy_1)/2
gen s1_trial_15 = (s1_15_hedonic_pizza_enjoy_1 + s1_15_hedonic_steak_enjoy_1)/2
gen s1_trial_16 = (s1_16_hedonic_pizza_enjoy_1 + s1_16_hedonic_steak_enjoy_1)/2 
gen s1_trial_17 = (s1_17_hedonic_pizza_enjoy_1 + s1_17_hedonic_steak_enjoy_1)/2
gen s1_trial_18 = (s1_18_hedonic_pizza_enjoy_1 + s1_18_hedonic_steak_enjoy_1)/2
gen s1_trial_19 = (s1_19_hedonic_pizza_enjoy_1 + s1_19_hedonic_steak_enjoy_1)/2
gen s1_trial_20 = (s1_20_hedonic_pizza_enjoy_1 + s1_20_hedonic_steak_enjoy_1)/2


gen s2_trial_1 = (s2_1_hedonic_pizza_enjoy_1 + s2_1_hedonic_steak_enjoy_1)/2
gen s2_trial_2 = (s2_2_hedonic_pizza_enjoy_1 + s2_2_hedonic_steak_enjoy_1)/2
gen s2_trial_3 = (s2_3_hedonic_pizza_enjoy_1 + s2_3_hedonic_steak_enjoy_1)/2
gen s2_trial_4 = (s2_4_hedonic_pizza_enjoy_1 + s2_4_hedonic_steak_enjoy_1)/2
gen s2_trial_5 = (s2_5_hedonic_pizza_enjoy_1 + s2_5_hedonic_steak_enjoy_1)/2
gen s2_trial_6 = (s2_6_hedonic_pizza_enjoy_1 + s2_6_hedonic_steak_enjoy_1)/2
gen s2_trial_7 = (s2_7_hedonic_pizza_enjoy_1 + s2_7_hedonic_steak_enjoy_1)/2
gen s2_trial_8 = (s2_8_hedonic_pizza_enjoy_1 + s2_8_hedonic_steak_enjoy_1)/2
gen s2_trial_9 = (s2_9_hedonic_pizza_enjoy_1 + s2_9_hedonic_steak_enjoy_1)/2
gen s2_trial_10 = (s2_10_hedonic_pizza_enjoy_1 + s2_10_hedonic_steak_enjoy_1)/2
gen s2_trial_11 = (s2_11_hedonic_pizza_enjoy_1 + s2_11_hedonic_steak_enjoy_1)/2
gen s2_trial_12 = (s2_12_hedonic_pizza_enjoy_1 + s2_12_hedonic_steak_enjoy_1)/2
gen s2_trial_13 = (s2_13_hedonic_pizza_enjoy_1 + s2_13_hedonic_steak_enjoy_1)/2
gen s2_trial_14 = (s2_14_hedonic_pizza_enjoy_1 + s2_14_hedonic_steak_enjoy_1)/2
gen s2_trial_15 = (s2_15_hedonic_pizza_enjoy_1 + s2_15_hedonic_steak_enjoy_1)/2
gen s2_trial_16 = (s2_16_hedonic_pizza_enjoy_1 + s2_16_hedonic_steak_enjoy_1)/2 
gen s2_trial_17 = (s2_17_hedonic_pizza_enjoy_1 + s2_17_hedonic_steak_enjoy_1)/2
gen s2_trial_18 = (s2_18_hedonic_pizza_enjoy_1 + s2_18_hedonic_steak_enjoy_1)/2
gen s2_trial_19 = (s2_19_hedonic_pizza_enjoy_1 + s2_19_hedonic_steak_enjoy_1)/2
gen s2_trial_20 = (s2_20_hedonic_pizza_enjoy_1 + s2_20_hedonic_steak_enjoy_1)/2

sort group

by group: egen med_1 = median(s1_trial_1)
by group: egen lqt_1 = pctile(s1_trial_1), p(25)
by group: egen uqt_1 = pctile(s1_trial_1), p(75)
by group: egen iqr_1 = iqr(s1_trial_1)
by group: egen mean_1 = mean(s1_trial_1)

twoway (rbar lqt_1 mean_1 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean_1 uqt_1 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter s1_trial_1 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("Mean") title("Trial 1") name(, replace)
graph save Graph "$dir\Hedonic Task\Graph_1.gph", replace

sort group

by group: egen med_20 = median(s1_trial_20)
by group: egen lqt_20 = pctile(s1_trial_20), p(25)
by group: egen uqt_20 = pctile(s1_trial_20), p(75)
by group: egen iqr_20 = iqr(s1_trial_20)
by group: egen mean_20 = mean(s1_trial_20)

twoway (rbar lqt_20 mean_20 group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean_20 uqt_20 group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter s1_trial_20 group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("Mean") title("Trial 20") name(, replace)
graph save Graph "$dir\Hedonic Task\Graph_2.gph", replace

graph combine "$dir\Hedonic Task\Graph_1.gph" "$dir\Hedonic Task\Graph_2.gph", ycommon name(combined, replace) title("Session 1")

graph box s1_trial_1 s1_trial_20, over(group) title("Session 1")
graph box s2_trial_1 s2_trial_20, over(group) title("Session 2")

graph box s1_trial_1 s1_trial_20, box(1, fcolor(dkorange) fintensity(inten100) lcolor(black)) box(2, fcolor(dkorange) fintensity(inten70) lcolor(black)) box(3, fcolor(gs0) fintensity(inten50) lcolor(black)) box(4, fcolor(gs0) fintensity(inten30) lcolor(black)) title("Session 1") subtitle("", position(1))
graph save Graph "$dir\Hedonic Task\Graph_3.gph", replace

graph box s2_trial_1 s2_trial_20, box(1, fcolor(gs2) fintensity(inten60) lcolor(black)) box(2, fcolor(gs2) fintensity(inten40) lcolor(black)) box(3, fcolor(gs0) fintensity(inten50) lcolor(black)) box(4, fcolor(gs0) fintensity(inten30) lcolor(black)) title("Session 2") subtitle("", position(1))
graph save Graph "$dir\Hedonic Task\Graph_4.gph", replace

graph combine "$dir\Hedonic Task\Graph_3.gph" "$dir\Hedonic Task\Graph_4.gph", ycommon name(combined_1, replace) title("Manipulation Check")

ttest s1_trial_1 == s1_trial_20
ttest s2_trial_1 == s2_trial_20

ttest s1_trial_1 == s1_trial_20 if group==5
ttest s1_trial_1 == s1_trial_20 if group==6

ttest s2_trial_1 == s2_trial_20 if group==5
ttest s2_trial_1 == s2_trial_20 if group==6

gen s1_diff = s1_trial_1 - s1_trial_20
gen s2_diff = s2_trial_1 - s2_trial_20

sort group

by group: egen med_1d = median(s1_diff)
by group: egen lqt_1d = pctile(s1_diff), p(25)
by group: egen uqt_1d = pctile(s1_diff), p(75)
by group: egen iqr_1d = iqr(s1_diff)
by group: egen mean_1d = mean(s1_diff)

twoway (rbar lqt_1d mean_1d group, fcolor(gs12) lcolor(black)) (rbar mean_1d uqt_1d group, fcolor(gs12) lcolor(black)) (scatter s1_diff group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo") title("Session 1") name(, replace)

*twoway (rbar lqt_1d mean_1d group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean_1d uqt_1d group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter s1_diff group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("Hedonic Satiation #1- #20") title("Session 1") name(, replace)
graph save Graph "$dir\Hedonic Task\Graph_5.gph", replace

sort group

by group: egen med_2d = median(s2_diff)
by group: egen lqt_2d = pctile(s2_diff), p(25)
by group: egen uqt_2d = pctile(s2_diff), p(75)
by group: egen iqr_2d = iqr(s2_diff)
by group: egen mean_2d = mean(s2_diff)

twoway (rbar lqt_2d mean_2d group, fcolor(gs12) lcolor(black)) (rbar mean_2d uqt_2d group, fcolor(gs12) lcolor(black)) (scatter s2_diff group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("Hedonic Satiation #1- #20") title("Session 2") name(, replace) yscale(off)

*twoway (rbar lqt_2d mean_2d group, fcolor(gs12) lcolor(black) barwidth(0.5)) (rbar mean_2d uqt_2d group, fcolor(gs12) lcolor(black) barwidth(.5)) (scatter s2_diff group, mcolor(black) msymbol(circle) mfcolor(gs15)), xlabel( 5 "Verum" 6 "Placebo")  ytitle("Hedonic Satiation #1- #20") title("Session 2") name(, replace) yscale(off)
graph save Graph "$dir\Hedonic Task\Graph_6.gph", replace

graph combine "$dir\Hedonic Task\Graph_5.gph" "$dir\Hedonic Task\Graph_6.gph", ycommon name(combined_2, replace) col(2)
ssc install vioplot
vioplot s1_diff
