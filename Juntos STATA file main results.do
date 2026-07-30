/*
Paper title: "Impact of Juntos conditional cash transfer program on nutritional and cognitive outcomes in Peru: ///
Comparison between younger and older initial exposure"
Authors: Alan Sánchez, Guido Melendez, Jere Behrman
Code written by: Alan Sánchez & Guido Melendez
Note: this analysis uses datafile "index_sibR1R4", which contains merged data from different rounds of the Young Lives Study in Peru.
*/

clear all
set matsize 5000
set memory 4000m
set maxvar 20000
set more off

* insert directory here
loc ruta   "C:\Users\Ritika Agarwal\Desktop\Paris School of Economics\Econometrics Project\Metrics Project"
loc rutaf  "C:\Users\Ritika Agarwal\Desktop\Paris School of Economics\Econometrics Project\Metrics Project\Output"
loc rutagr "C:\Users\Ritika Agarwal\Desktop\Paris School of Economics\Econometrics Project\Metrics Project\Graphs"
 
/* This do-file reports main results of the paper and some tables of the 
Appendix part */

cd "`ruta'"

use stata_dta, clear



***************************************************************************
*                                                                         *
*                                                                         *
*                     V. REGRESSION ANALYSIS                              *
*                                                                         *
*                                                                         *
***************************************************************************                                                                        *

* Defining globals

global household "MUMED CETH wiR1"
global child "female i.age i.ybirth"
global child2 "female i.age i.ppvtlang i.ybirth"
global district "prom_nbi"
global cluster "CHILDID"
global clusterid "ubigeoR1"
global condition "quintil!=."
global sample_j1 "sample==1"
global sample_j2 "cogsample==1"
global datejuntos "baselinejuntos==1"
global criticalperiod "years03==1"
global cogcontrol "i.ppvtlang"
global treatmentr3      "rec_juntos round                 aftjuntosR3"
global fetreatmentr3    "           round                 aftjuntosR3"
global treatmentr3s     "rec_juntos round                 aftjuntosR3"
global fetreatmentr3s   "           round                 aftjuntosR3"
global treatmentr3s_all "rec_juntos round                 aftjuntosR3"
global treatmentr4      "juntosr4   round aftjuntosR4"
global fetreatmentr4    "           round aftjuntosR4"

* Defining sample for nutrition
keep if $criticalperiod 
xi: areg zhfa $household $child $district $treatmentr3 if $condition & baselinejuntos, abs($cluster)
gen junksample=e(sample)

duplicates tag CHILDID if junksample==1, gen(rep)
tab rep
gen sample=0
replace sample=1 if rep==3

tab rec_juntos if rep==3 & round==0 & sib==0 & (juntosgroup==1 | juntosgroup==3)

drop junksample rep

* Defining sample for cognitive outcomes

// Those children who took the PPVT in Spanish
tempvar junkppvtlang0
gen `junkppvtlang0'=ppvtlang if round==0
tempvar ppvtlang0
bys pid: egen `ppvtlang0'=min(`junkppvtlang0')

tempvar junkppvtlang1
gen `junkppvtlang1'=ppvtlang if round==1
tempvar ppvtlang1
bys pid: egen `ppvtlang1'=min(`junkppvtlang1')

gen bothspanishppvt=(`ppvtlang0'==4 & `ppvtlang1'==4)
replace bothspanishppvt=. if PPVT==.
label var bothspanishppvt "Language used by child during PPVT test is Spanish in both rounds"

// Final sample
reg std_PPVT zhfa $household $child $district $treatmentr3 if $condition & baselinejuntos, abs($cluster)
gen junksample=e(sample)

duplicates tag CHILDID sib if junksample==1, gen(rep) 
gen cogsample=0
replace cogsample=1 if rep==1
replace cogsample=0 if cogsample==1 & bothspanishppvt==0
drop junksample rep

gen juntosgroup1=0
replace juntosgroup1=1 if juntosgroup==1

***********************************************************************************
*                                                                                 *
*  								TABLE 1											  *	                          
*                                                                                 *
*                                                                                 *
***********************************************************************************

/* This part of the do-file replicates table of the Descriptive statistics section */

* Tabulation of age in years, haz sample
tab age round if sib==0 & $sample_j1 & juntosgroup!=2
tab age round if sib==1 & $sample_j1 & juntosgroup!=2
* Tabulation of age in years, PPVT sample
tab age round if sib==0 & $sample_j2 & juntosgroup!=2
tab age round if sib==1 & $sample_j2 & juntosgroup!=2

*  Number of observations
tab sample sib if juntosgroup==1 & round==0
tab sample sib if juntosgroup==3 & round==0

* Age in years
ttest age if round==0 & sib==0 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)
ttest age if round==0 & sib==1 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)
ttest age if round==1 & sib==0 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)
ttest age if round==1 & sib==1 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)

* Height for age
ttest zhfa if round==0 & sib==0 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)
ttest zhfa if round==0 & sib==1 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)
ttest zhfa if round==1 & sib==0 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)
ttest zhfa if round==1 & sib==1 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)

* Stunting
ttest stunted if round==0 & sib==0 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)
ttest stunted if round==0 & sib==1 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)
ttest stunted if round==1 & sib==0 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)
ttest stunted if round==1 & sib==1 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)

* Severe stunting

ttest estunted if round==0 & sib==0 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)
ttest estunted if round==0 & sib==1 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)
ttest estunted if round==1 & sib==0 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)
ttest estunted if round==1 & sib==1 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)

* Mothers education

ttest CAREED if round==0 & sib==0 & $sample_j1, by(juntosgroup1)
ttest CAREED if round==0 & sib==1 & $sample_j1 & juntosgroup!=2, by(juntosgroup1)

* Cognitive achievement

tab cogsample sib if juntosgroup==1 & round==0
tab cogsample sib if juntosgroup==3 & round==0

ttest age if round==0 & sib==0 & $sample_j2 & juntosgroup!=2, by(juntosgroup1)
ttest age if round==0 & sib==1 & $sample_j2 & juntosgroup!=2, by(juntosgroup1)

sum std_PPVT   if sib==0 & juntosgroup==1 & round==0 & $sample_j2
sum std_PPVT   if sib==0 & juntosgroup==3 & round==0 & $sample_j2
ttest std_PPVT if round==0 & sib==0 & $sample_j2 & juntosgroup!=2, by(juntosgroup1)

sum std_PPVT   if sib==1 & juntosgroup==1 & round==0 & $sample_j2
sum std_PPVT   if sib==1 & juntosgroup==3 & round==0 & $sample_j2
ttest std_PPVT if round==0 & sib==1 & $sample_j2 & juntosgroup!=2, by(juntosgroup1)

sum std_PPVT   if sib==0 & juntosgroup==1 & round==1 & $sample_j2
sum std_PPVT   if sib==0 & juntosgroup==3 & round==1 & $sample_j2
ttest std_PPVT if round==1 & sib==0 & $sample_j2 & juntosgroup!=2, by(juntosgroup1)

sum std_PPVT   if sib==1 & juntosgroup==1 & round==1 & $sample_j2
sum std_PPVT   if sib==1 & juntosgroup==3 & round==1 & $sample_j2
ttest std_PPVT if round==1 & sib==1 & $sample_j2 & juntosgroup!=2, by(juntosgroup1)

*** School attendance for the index child
ttest  ATTNDSCHL if $sample_j1 & sib==0 & round==0, by(juntosgroup1)
ttest  ATTNDSCHL if $sample_j1 & sib==0 & round==1, by(juntosgroup1)

***********************************************************************************
*                                                                                 *
* 							Appendix A: Figure A.1 								  *	                          
*                                                                                 *
*                                                                                 *
***********************************************************************************
/* Age in months during first exposure to Juntos CCT (sample with anthropometric data available) */

set scheme s1mono
twoway (hist agemonths_firstT if juntosgroup==1 & round==0 & sib==1 & $sample_j1, bfcolor(black) blcolor(white)) ///
(hist agemonths_firstT if juntosgroup==1 & round==0 & sib==0 & $sample_j1, bfcolor(none) legend(label(1 "Younger siblings") label(2 "Index children")))
graph save Graph "`rutagr'\Figure_A1.gph", replace
graph export "`rutagr'\Figure_A1.eps", as(eps) preview(off) replace

***********************************************************************************
*                                                                                 *
* 							Appendix A: Figure A.2 								  *	                          
*                                                                                 *
*                                                                                 *
***********************************************************************************

/* Age in months during first exposure to Juntos CCT (sample with cognitive data available) */

twoway (hist agemonths_firstT if juntosgroup==1 & round==0 & sib==1 & $sample_j2, bfcolor(black) blcolor(white)) ///
(hist agemonths_firstT if juntosgroup==1 & round==0 & sib==0 & $sample_j1, bfcolor(none) legend(label(1 "Younger siblings") label(2 "Index children")))
graph save Graph "`rutagr'\Figure_A2.gph", replace
graph export "`rutagr'\Figure_A2.eps", as(eps) preview(off) replace

***********************************************************************************
*                                                                                 *
*                                                                                 *
*                                TABLE 2 RESULTS                                  *
*                                                                                 *
*                                                                                 *
***********************************************************************************

************************************************************************************
* Stunting Index, comparing treated in R2-R3 and (never treated)
************************************************************************************

* III. Child fix effects
xi: xtreg stunted  $fetreatmentr3 $household $child      if sib==0 & $sample_j1 & (juntosgroup==1 | juntosgroup==3), fe cluster($cluster)
estimates store stunting0

************************************************************************************
* Severe stunting Index, comparing treated in R2-R3 and (never treated)
************************************************************************************

* III. Child fix effects
xi: xtreg estunted  $fetreatmentr3 $household $child      if sib==0 & $sample_j1 & (juntosgroup==1 | juntosgroup==3), fe cluster($cluster)
estimates store sstunting0

************************************************************************************
* HAZ Index, comparing treated in R2-R3 and (never treated)
************************************************************************************

* III. Child fix effects
xi: xtreg zhfa  $fetreatmentr3 $household $child             if sib==0 & $sample_j1 & (juntosgroup==1 | juntosgroup==3), fe cluster($cluster)
estimates store haz0

************************************************************************************
* PPVT Index, comparing treated in R2-R3 and (never treated)
************************************************************************************
* III. Child fix effects
xi: xtreg std_PPVT  $fetreatmentr3 $household $child2      if $sample_j2 & sib==0 & (juntosgroup==1 | juntosgroup==3), fe cluster($cluster)
estimates store ppvt0

************************************************************************************
* Stunting Sibling, comparing treated in R2-R3 and never treated 
************************************************************************************

* III. Child fix effects
xi: xtreg stunted  $fetreatmentr3s $household $child      if sib==1 & $sample_j1 & (juntosgroup==1 | juntosgroup==3), fe cluster($cluster)
estimates store stunting1

************************************************************************************
* Severe stunting Sibling, comparing treated in R2-R3 and never treated 
************************************************************************************

* III. Child fix effects
xi: xtreg estunted  $fetreatmentr3s $household $child        if sib==1 & $sample_j1 & (juntosgroup==1 | juntosgroup==3), fe cluster($cluster)
estimates store sstunting1

************************************************************************************
* HAZ Sibling, comparing treated in R2-R3 and never treated 
************************************************************************************

* III. Child fix effects
xi: xtreg zhfa  $fetreatmentr3s $household $child      if sib==1 & $sample_j1 & (juntosgroup==1 | juntosgroup==3), fe cluster($cluster)
estimates store haz1

************************************************************************************
* PPVT Sibling, comparing treated in R2-R3 and never treated
************************************************************************************

* III. Child fix effects
xi: xtreg std_PPVT  $fetreatmentr3s $household $child2      if $sample_j2 & sib==1 & (juntosgroup==1 | juntosgroup==3), fe cluster($cluster)
estimates store ppvt1

* Export results in excel table
xml_tab stunting0 sstunting0 haz0 ppvt0, replace sheet(table2panelA) ///
save("`rutaf'\table2_to_3_b10.xls") title("Effects of first expansion of Juntos Program, Index Children") below stats(N r2 r2_a) ///
keep (round aftjuntosR3)  
xml_tab stunting1 sstunting1 haz1 ppvt1, append sheet(table2panelB) ///
save("`rutaf'\table2_to_3_b10.xls") title("Effects of first expansion of Juntos Program, Younger Siblings") below stats(N r2 r2_a) ///
keep (round aftjuntosR3)  

***********************************************************************************
*                                                                                 *
*                                                                                 *
*                                TABLE 3 RESULTS                                  *
*                                                                                 *
*                                                                                 *
***********************************************************************************

* Before and after dummy
gen time=1
replace time=2 if round==1 & sib==0
replace time=2 if round==0 & sib==1
replace time=3 if round==1 & sib==1
label define timex 1 "Round 2" 2 "Round 3" 3 "Round 4"
label values time timex 
label var time "Round"

tab time, gen(time_)
rename time_1 round2
rename time_2 round3
rename time_3 round4

gen round3xjuntos=rec_juntos*round3
label var round3xjuntos "beneficiary*round 3"

gen round4xjuntos=rec_juntos*round4
label var round4xjuntos "beneficiary*round 4"

gen round4xjuntos_0_2=rec_juntos*round4*enroll_0_2
gen round4xjuntos_3_4=rec_juntos*round4*enroll_3_4

global juntos "round3 round4 rec_juntos round3xjuntos round4xjuntos"
global test "round3xjuntos=round4xjuntos"

matrix M2=J(1,4,.)
matrix colnames M2= "hatstunted" "hatestunted" "hathaz" "hatppvt" 
matrix rownames M2= "p-value (c4=c5)"

* 1. Stunting
xi: xtreg stunted $juntos $household  $child $district  if $sample_j1 & (juntosgroup==1 | juntosgroup==3), cluster($cluster) fe
estimates store test1stunt
test $test
local pvalue1=r(p)
* 2. Severe stunting
xi: xtreg estunted $juntos $household $child $district  if $sample_j1 & (juntosgroup==1 | juntosgroup==3), cluster($cluster) fe
estimates store test1extstunt
test $test
local pvalue2=r(p)
* 3. HAZ
xi: xtreg zhfa     $juntos $household $child $district  if $sample_j1 & (juntosgroup==1 | juntosgroup==3), cluster($cluster) fe
estimates store test1haz
test $test
local pvalue3=r(p)
* 4. PPVT
xi: xtreg std_PPVT $juntos $household $child2 $district if $sample_j2 & (juntosgroup==1 | juntosgroup==3), cluster($cluster) fe
estimates store test1ppvt
test $test
local pvalue4=r(p)

xml_tab test1stunt test1extstunt test1haz test1ppvt, append sheet(table3) ///
save("`rutaf'\table2_to_3_b10.xls") title("Pooled estimates") below stats(N r2 r2_a) ///
keep ($juntos)

matrix M2[1,1]=`pvalue1'
matrix M2[1,2]=`pvalue2'
matrix M2[1,3]=`pvalue3'
matrix M2[1,4]=`pvalue4'

* Export results in excel table
xml_tab M2, append sheet(test_table3) save ("`rutaf'\table2_to_3_b10.xls") title("P value") 

***********************************************************************************
*                                                                                 *
*                                                                                 *
*          					APPENDIX B: Table B.10                                *
*                                                                                 *
*                                                                                 *
***********************************************************************************

/* This part of the do-file splits estimations between treated 0-2 vs treated betwwen 3 to 4 */


***************************************
***Splitting younger siblings: treated btw 0 to vs treated btw 3 to 4
***************************************

global juntos_sib "enroll_0_2 enroll_3_4 rec_juntos round4xjuntos_0_2 round4xjuntos_3_4"

* 1. Stunting
xi: xtreg stunted $juntos_sib $household  $child $district  if $sample_j1 & (juntosgroup==1 | juntosgroup==3) & sib==1, cluster($cluster) fe
estimates store test1stunt
test round4xjuntos_0_2=round4xjuntos_3_4
local pvalue1=r(p)
* 2. Severe stunting
xi: xtreg estunted $juntos_sib $household $child $district  if $sample_j1 & (juntosgroup==1 | juntosgroup==3) & sib==1, cluster($cluster) fe
estimates store test1extstunt
test round4xjuntos_0_2=round4xjuntos_3_4
local pvalue2=r(p)
* 3. HAZ
xi: xtreg zhfa     $juntos_sib $household $child $district  if $sample_j1 & (juntosgroup==1 | juntosgroup==3) & sib==1, cluster($cluster) fe
estimates store test1haz
test round4xjuntos_0_2=round4xjuntos_3_4
local pvalue3=r(p)
* 4. PPVT
xi: xtreg std_PPVT $juntos_sib $household $child2 $district if $sample_j2 & (juntosgroup==1 | juntosgroup==3) & sib==1, cluster($cluster) fe
estimates store test1ppvt
test round4xjuntos_0_2=round4xjuntos_3_4
local pvalue4=r(p)

* Export results in excel table
xml_tab test1stunt test1extstunt test1haz test1ppvt, append sheet(tableb10) ///
save("`rutaf'\table2_to_3_b10.xls") title("Pooled estimates: Sibling 0-2 and 3-4") below stats(N r2 r2_a) ///
keep ($juntos_sib)

matrix M2[1,1]=`pvalue1'
matrix M2[1,2]=`pvalue2'
matrix M2[1,3]=`pvalue3'
matrix M2[1,4]=`pvalue4'

xml_tab M2, append sheet(test_tableb10) save ("`rutaf'\table2_to_3_b10.xls") title("P value") 
