clear
set matsize 5000	
	
	use "$dbroot\1data\analysis\analysis_anonymized.dta", clear

	

	

	log using "$dbroot\2progs\analysis.log", replace
	
	****Table II
	preserve
	bysort anom_id: keep if _n==1
	foreach x in final_gpa sat_verb sat_math cumcr underclassperson parttime age female englishnative asian black hispanic white other  {
	mean `x' , over(origassign)
	sum `x' if assign~=.
	}
	
	foreach x of varlist final_gpa sat_verb sat_math cumcr  age  {
	egen `x'_mean=mean(`x')
	generate `x'_miss=`x'==.
	replace `x'=`x'_mean if `x'==.
	}

	foreach x of varlist  underclassperson parttime female englishnative {
	generate `x'_miss=`x'==.
	replace `x'=0 if `x'==.
	}
	generate assign_a=origassign==1
	replace assign_a=. if origassign==.
	tab assign
	logit assign_a final_gpa sat_verb sat_math cumcr underclassperson parttime age female englishnative asian black hisp white other  *_miss  if origassign~=.

	restore
	
	preserve
	keep if test_num=="final"
	bysort anom_id: keep if _n==1
	foreach x in final_gpa sat_verb sat_math cumcr underclassperson parttime age female englishnative asian black hispanic white other  {
	mean `x' , over(origassign)
	sum `x' if assign~=.
	}
	
	foreach x of varlist final_gpa sat_verb sat_math cumcr  age  {
	egen `x'_mean=mean(`x')
	generate `x'_miss=`x'==.
	replace `x'=`x'_mean if `x'==.
	}

	foreach x of varlist  underclassperson parttime female englishnative {
	generate `x'_miss=`x'==.
	replace `x'=0 if `x'==.
	}
	generate assign_a=origassign==1
	replace assign_a=. if origassign==.
	tab assign
	logit assign_a final_gpa sat_verb sat_math cumcr underclassperson parttime age female englishnative asian black hisp white other  *_miss  if origassign~=.

	restore

	
/*	
	esttab using "$dbroot\ECO1001_SP18\08results\Appendix_table_paper_results.tex", keep(assignb) style(tex) ///
 stats(Mean N Students, labels("Untreated Mean Group" "\# of Obs" "\# of students") fmt(%9.1f %9.0fc %9.0fc %9.1f)) ///
 label replace ///
star( * .1 ** .05 *** .01) b(%12.1f) se(%12.1f) nogaps  ///
mtitles("\makecell{(1)}" "\makecell{(2)}" "\makecell{(3)}" "\makecell{(4)}" "\makecell{(5)}" "\makecell{(6)}") ///
mgroups("Attempted Problem set" "Number of Attempts" "Time on first Attempt" "Total Time" "Score Given Attempting", pattern(1 0 1 0 1 0 1 0 1 0) span prefix(\multicolumn{@span}{c}{)  suffix(}) erepeat(\cmidrule(lr){@span}) ) ///
nonotes nonum addnotes("\begin{minipage}{`linewidth'\linewidth} \footnotesize \smallskip  \textbf{Note:} Coefficients are estimated via OLS. All specifications are clustered by individual. All models have individual level fixed effects. Significance levels are indicated by $*$ $<.1$, ** $<.05$,  *** $<.01$.   \end{minipage}" )
	
*/
	
	/*
	esttab using "$dbroot\ECO1001_SP18\08results\Appendix_table_paper_results.tex", keep(assignb) style(tex) ///
 stats(Mean N Students, labels("Untreated Mean Group" "\# of Obs" "\# of students") fmt(%9.1f %9.0fc %9.0fc %9.1f)) ///
 label replace ///
star( * .1 ** .05 *** .01) b(%12.1f) se(%12.1f) nogaps  ///
mtitles("\makecell{(1)}" "\makecell{(2)}" "\makecell{(3)}" "\makecell{(4)}" "\makecell{(5)}" "\makecell{(6)}") ///
mgroups("Attempted Problem set" "Number of Attempts" "Time on first Attempt" "Total Time" "Score Given Attempting", pattern(1 0 1 0 1 0 1 0 1 0) span prefix(\multicolumn{@span}{c}{)  suffix(}) erepeat(\cmidrule(lr){@span}) ) ///
nonotes nonum addnotes("\begin{minipage}{`linewidth'\linewidth} \footnotesize \smallskip  \textbf{Note:} Coefficients are estimated via OLS. All specifications are clustered by individual. All models have individual level fixed effects. Significance levels are indicated by $*$ $<.1$, ** $<.05$,  *** $<.01$.   \end{minipage}" )
*/


*Table III	
	
*Just for table presentation purposes it's nicer to have them not be in probability but rather percentage points since I can present more detail in less space	
	eststo clear
	replace attempted=attempted*100
	
*Setting up some scalars to include in table	

	areg attempted grade nudge gradenudge i.week, cluster(anom_id) absorb(anom_id)
	scalar students=e(N_clust)
	areg time_total i.grade##i.nudge i.week, cluster(anom_id) absorb(anom_id)
	scalar students4=e(N_clust)
	areg score i.grade##i.nudge i.week, cluster(anom_id) absorb(anom_id)
	scalar students5=e(N_clust)


	sum attempted if grade==0 & nudge==0
	scalar mean1=r(mean)
	sum attemptsx if grade==0 & nudge==0 & attemptsx>0
	scalar mean2=r(mean)
	sum time_on_attempts_1 if grade==0 & nudge==0
	scalar mean3=r(mean)
	sum time_total if grade==0 & nudge==0
	scalar mean4=r(mean)
	sum score if grade==0 & nudge==0
	scalar mean5=r(mean)
	sum correct if grade==0 & nudge==0
	scalar mean6=r(mean)
	

	
	eststo clear
*Running estimations for paper table 3	
	
	eststo: areg attempted grade nudge gradenudge i.week, cluster(anom_id) absorb(anom_id)
	estadd scalar Mean = scalar(mean1)
	estadd local Students = scalar(students)
	
	eststo: areg attempted grade i.problem, cluster(anom_id) absorb(anom_id)
	estadd scalar Mean = scalar(mean1)
	estadd local Students = scalar(students)
	
	eststo: areg attempted grade_1 grade_2 grade_3 nudge gradenudge i.week, cluster(anom_id) absorb(anom_id)
	estadd scalar Mean = scalar(mean1)
	estadd local Students = scalar(students)	
	
	eststo: areg attempted grade_1 grade_2 grade_3 i.problem, cluster(anom_id) absorb(anom_id)
	estadd scalar Mean = scalar(mean1)
	estadd local Students = scalar(students)
	
	eststo: areg time_total grade nudge gradenudge i.week, cluster(anom_id) absorb(anom_id)
	estadd scalar Mean = scalar(mean4)
	estadd local Students = scalar(students4)
	
	eststo: areg time_total grade i.problem, cluster(anom_id) absorb(anom_id)
	estadd scalar Mean = scalar(mean4)
	estadd local Students = scalar(students4)
	

	eststo: areg score grade nudge gradenudge i.week, cluster(anom_id) absorb(anom_id)
	estadd scalar Mean = scalar(mean5)
	estadd local Students = scalar(students5)
	
	eststo: areg score grade i.problem, cluster(anom_id) absorb(anom_id)
	estadd scalar Mean = scalar(mean5)
	estadd local Students = scalar(students5)

	
	
	
/*
	local linewidth = 2
	/*esttab using "$dbroot\ECO1001_SP18\08results\paper_results_table2.rtf", b(%12.2f) se(%12.2f) keep(assignb grade grade_1 grade_2 grade_3 nudge gradenudge) label nomtitles mgroups("Attempted Problem set" "Number of Attempts" "Time on first Attempt" "Total Time" "Score Given Attempting", pattern(1 0 1 0 1 0 1 0 1 0))   stats(Mean Students N  r2, labels("Mean of Ungraded/Unudged Group" "\# of Obs" "\# of students" "R2") fmt(%9.2f %9.0fc %9.0fc %9.2f)) title("Table 2" "First Stage") order(assignb grade grade_1 grade_2 grade_3 nudge gradenudge)  compress se replace */
esttab  using "$dbroot/ECO1001_SP18\4work\firststage_IB_a.tex", keep(grade grade_1 grade_2 grade_3 nudge gradenudge) style(tex) ///
 stats(Mean N Students, labels("Untreated Mean Group" "\# of Obs" "\# of students") fmt(%9.1f %9.0fc %9.0fc %9.1f)) ///
 label replace ///
star( * .1 ** .05 *** .01) b(%12.1f) se(%12.1f) nogaps  ///
mtitles("\makecell{(1)}" "\makecell{(2)}" "\makecell{(3)}" "\makecell{(4)}" "\makecell{(5)}" "\makecell{(6)}" "\makecell{(7)}" "\makecell{(8)}" "\makecell{(9)}" "\makecell{(10)}") ///
mgroups("Attempted Problem set" "Number of Attempts" "Time on first Attempt" "Total Time" "Score Given Attempting", pattern(1 0 1 0 1 0 1 0 1 0) span prefix(\multicolumn{@span}{c}{)  suffix(}) erepeat(\cmidrule(lr){@span}) ) ///
nonotes nonum addnotes("\begin{minipage}{`linewidth'\linewidth} \footnotesize \smallskip  \textbf{Note:} Coefficients are estimated via OLS. All specifications are clustered by individual. All models have individual level fixed effects. Significance levels are indicated by $*$ $<.1$, ** $<.05$,  *** $<.01$.   \end{minipage}" )
*/	


	
	
	
*when looking at the effect on the exam I flip the attempts to be back to zero-1 for 2SLS
	replace attempted=attempted/100

	
*Rescaling correct so results are in percentage points
	replace correct=correct*100
	
*Table IV

*Setting up some scalars to include in table	
	
	eststo clear
	areg correct grade i.problem, cluster(anom_id) absorb(anom_id)
	scalar students=e(N_clust)
    sum correct if grade==0
	scalar mean1=r(mean)
	areg correct grade i.problem if problem<=6, cluster(anom_id) absorb(anom_id)
	scalar students2=e(N_clust)
    sum correct if grade==0& problem<=6
	scalar mean2=r(mean)
	areg correct grade i.problem if problem>=7, cluster(anom_id) absorb(anom_id)
	scalar students3=e(N_clust)
    sum correct if grade==0 & problem>=7
	scalar mean3=r(mean)

*Setting up panel variable for xtivreg	
xtset anom_id problem


*Estimations for Table 4	
	eststo: areg correct grade i.problem, absorb(anom_id) cluster(anom_id)
	estadd scalar Mean = scalar(mean1)
	estadd local Students = scalar(students)
	
	eststo: areg correct grade i.problem if problem<=6, absorb(anom_id) cluster(anom_id)
	estadd scalar Mean = scalar(mean2)
	estadd local Students = scalar(students2)
	
	eststo: areg correct grade i.problem if problem>=7, absorb(anom_id) cluster(anom_id)
	estadd scalar Mean = scalar(mean3)
	estadd local Students = scalar(students3)

	eststo: xtivreg correct (attempted=grade) i.problem, fe  vce(cluster anom_id)
	estadd scalar Mean = scalar(mean1)
	estadd local Students = scalar(students)
	
	eststo: xtivreg correct (attempted=grade) i.problem if problem<=6, fe  vce(cluster anom_id)
	estadd scalar Mean = scalar(mean2)
	estadd local Students = scalar(students2)
	
	eststo: xtivreg correct (attempted=grade) i.problem if problem>=7,  fe  vce(cluster anom_id)
	estadd scalar Mean = scalar(mean3)
	estadd local Students = scalar(students3)
	
	local linewidth = 1.25
/*	
esttab  using "$dbroot/ECO1001_SP18\4work\reducedform2sls_IB_a.tex", keep(grade attempted) style(tex) ///
 stats(Mean N Students, labels("Mean of Ungraded" "\# of Obs" "\# of students") fmt(%9.1f %9.0fc %9.0fc %9.1f)) ///
 label replace ///
star( * .1 ** .05 *** .01) b(%12.1f) se(%12.1f) nogaps  ///
mtitles("\makecell{(1)} All" "\makecell{(2)} Midterm" "\makecell{(3)} Final" "\makecell{(4)} All" "\makecell{(5)} Midterm" "\makecell{(6)} Final") ///
mgroups("Reduced Form" "2SLS", pattern(1 0 0 1 0 0) span prefix(\multicolumn{@span}{c}{)  suffix(}) erepeat(\cmidrule(lr){@span}) ) ///
nonotes nonum addnotes("\begin{minipage}{`linewidth'\linewidth} \footnotesize \smallskip  \textbf{Note:} Coefficients are estimated via OLS. All specifications are clustered by individual. All models have individual level fixed effects. Significance levels are indicated by $*$ $<.1$, ** $<.05$,  *** $<.01$.   \end{minipage}" )
*/




*Table V
xtset anom_id problem

*Better for table presentation
replace attempted=attempted*100
	
	
*Estimates for table and setting up scalars for inclusion in table	
	eststo: xtreg attempted grade nudge gradenudge i.week  if asian==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum attempted if asian==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg attempted grade nudge gradenudge i.week  if white==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum attempted if white==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg attempted grade nudge gradenudge i.week  if asianwhite==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum attempted if asianwhite==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo:  xtreg attempted grade nudge gradenudge i.week if sat_math_med==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum attempted if sat_math_med==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo:  xtreg attempted grade nudge gradenudge i.week if sat_math_med==2, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum attempted if sat_math_med==2 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo:  xtreg attempted grade nudge gradenudge i.week if sat_math_med==., fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum attempted if sat_math_med==. & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg attempted grade nudge gradenudge i.week  if female==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum attempted if female==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg attempted grade nudge gradenudge i.week  if female==0, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum attempted if female==0 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg attempted grade nudge gradenudge i.week  if final_gpa<=3.24, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum attempted if final_gpa<=3.24 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg attempted grade nudge gradenudge i.week  if final_gpa>3.24 & final_gpa~=., fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum attempted if final_gpa>3.24 & final_gpa~=. & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
/*	
local linewidth = 2	
esttab  using "$dbroot/ECO1001_SP18\4work\firststage_het_IB_a.tex", keep(grade nudge gradenudge) style(tex) ///
 stats(Mean N Students  , labels("Untreated Mean" "\# of students" "\# of Obs" ) fmt(%9.1f %9.0fc %9.0fc %9.1f)) ///
 label replace ///
star( * .1 ** .05 *** .01) b(%12.1f) se(%12.1f) nogaps  ///
mtitles("Asian" "White" "Other" "Below Med" "Above Med" "Miss" "Female" "Male" "Below GPA" "Above GPA") ///
mgroups("Race" "Math SAT Score" "Gender" "Transfer Status", pattern(1 0 0 1 0 0 1 0 1 0) span prefix(\multicolumn{@span}{c}{)  suffix(}) erepeat(\cmidrule(lr){@span}) ) ///
nonotes nonum addnotes("\begin{minipage}{`linewidth'\linewidth} \footnotesize \smallskip  \textbf{Note:} Coefficients are estimated via OLS. All specifications are clustered by individual. All models have individual level fixed effects. Significance levels are indicated by $*$ $<.1$, ** $<.05$,  *** $<.01$.   \end{minipage}" )
*/	
	
eststo clear

*Table VI	
	replace attempted=attempted/100
	
	eststo clear

	xtset anom_id problem
	
	eststo: xtivreg correct (attempted=grade) i.problem  if asian==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum correct if asian==1 & grade==0
	estadd scalar Mean=r(mean)
	
	eststo: xtivreg correct (attempted=grade) i.problem  if white==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum correct if white==1 & grade==0
	estadd scalar Mean=r(mean)
	
	eststo: xtivreg correct (attempted=grade) i.problem  if asianwhite==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum correct if asianwhite==1 & grade==0
	estadd scalar Mean=r(mean)
	
	eststo:  xtivreg correct (attempted=grade) i.problem if sat_math_med==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum correct if sat_math_med==1 & grade==0
	estadd scalar Mean=r(mean)
	
	eststo:  xtivreg correct (attempted=grade) i.problem if sat_math_med==2, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum correct if sat_math_med==2 & grade==0
	estadd scalar Mean=r(mean)
	
	eststo:  xtivreg correct (attempted=grade) i.problem if sat_math_med==., fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum correct if sat_math_med==. & grade==0
	estadd scalar Mean=r(mean)
	
	eststo: xtivreg correct (attempted=grade) i.problem  if female==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum correct if female==1 & grade==0
	estadd scalar Mean=r(mean)
	
	eststo: xtivreg correct (attempted=grade) i.problem  if female==0, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum correct if female==0 & grade==0
	estadd scalar Mean=r(mean)
	
	eststo: xtivreg correct (attempted=grade) i.problem if final_gpa<=3.24, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum correct if final_gpa<=3.24 & grade==0
	sum correct if final_gpa<=3.24 & grade==1

	estadd scalar Mean=r(mean)
	
	eststo: xtivreg correct (attempted=grade) i.problem  if final_gpa>3.24 & final_gpa~=., fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum correct if final_gpa>3.24 & final_gpa~=. & grade==0
	sum correct if final_gpa>3.24 & final_gpa~=. & grade==1

	estadd scalar Mean=r(mean)
	

*Figure II
forvalues x=1(1)12 {
sum correct if problem==`x' & grade==0
scalar mean`x'=r(mean)
}

forvalues x=1(1)12 {
	eststo: ivregress 2sls correct (attempted=grade) if problem==`x'
	estadd scalar Mean = scalar(mean`x')

}



	local linewidth = 2.5
	
*If you want to look at Figure II in table form instead.	
/*
esttab  using "$dbroot/ECO1001_SP18\4work\2sls_IB_a.tex", keep(attempted) style(tex) ///
 stats(Mean N, labels("Mean of Ungraded" "\# of students") fmt(%9.3f %9.0fc %9.0fc %9.3f)) ///
 label replace ///
star( * .1 ** .05 *** .01) b(%12.3f) se(%12.3f) nogaps  ///
mtitles("\makecell{(1)}" "\makecell{(2)}" "\makecell{(3)}" "\makecell{(4)}" "\makecell{(5)}" "\makecell{(6)}" "\makecell{(7)}" "\makecell{(8)}" "\makecell{(9)}" "\makecell{(10)}" "\makecell{(11)}" "\makecell{(12)}") ///
mgroups("2SLS", pattern(1 0 0 0 0 0 0 0 0 0 0 0) span prefix(\multicolumn{@span}{c}{)  suffix(}) erepeat(\cmidrule(lr){@span}) ) ///
nonotes nonum addnotes("\begin{minipage}{`linewidth'\linewidth} \footnotesize \smallskip  \textbf{Note:} Coefficients are estimated via OLS. All specifications are clustered by individual. All models have individual level fixed effects. Significance levels are indicated by $*$ $<.1$, ** $<.05$,  *** $<.01$.   \end{minipage}" )
*/	
eststo clear

*Figure III
forvalues x=1(1)12 {
sum correct if problem==`x' & grade==0 & final_gpa<=3.24
sum correct if problem==`x' & grade==0 & final_gpa>3.24 & final_gpa~=.

}

forvalues x=1(1)12 {
	ivregress 2sls correct (attempted=grade) if problem==`x' & final_gpa<=3.24
	ivregress 2sls correct (attempted=grade) if problem==`x' & final_gpa>3.24 & final_gpa~=.

}

*Appendix table A1
	eststo clear
	replace attempted=attempted*100
	areg attempted grade nudge gradenudge i.week, cluster(anom_id) absorb(anom_id)
	scalar students=e(N_clust)
	areg attemptsx i.grade##i.nudge i.week, cluster(anom_id) absorb(anom_id)
	scalar students2=e(N_clust)
	areg time_on_attempts_1 i.grade##i.nudge i.week, cluster(anom_id) absorb(anom_id)
	scalar students3=e(N_clust)
	areg time_total i.grade##i.nudge i.week, cluster(anom_id) absorb(anom_id)
	scalar students4=e(N_clust)
	areg score i.grade##i.nudge i.week, cluster(anom_id) absorb(anom_id)
	scalar students5=e(N_clust)
	areg correct i.grade##i.nudge i.week, cluster(anom_id) absorb(anom_id)
	scalar students6=e(N_clust)
	

	sum attempted if grade==0 & nudge==0
	scalar mean1=r(mean)
	sum attemptsx if grade==0 & nudge==0 & attemptsx>0
	scalar mean2=r(mean)
	sum time_on_attempts_1 if grade==0 & nudge==0
	scalar mean3=r(mean)
	sum time_total if grade==0 & nudge==0
	scalar mean4=r(mean)
	sum score if grade==0 & nudge==0
	scalar mean5=r(mean)
	sum correct if grade==0 & nudge==0
	scalar mean6=r(mean)
	
	
	
	eststo: reg attempted assignb i.problem, vce(cluster anom_id)
	estadd scalar Mean = scalar(mean1)
	estadd local Students = scalar(students)
	
	eststo: reg attemptsx assignb i.problem if attemptsx>0, cluster(anom_id)
	estadd scalar Mean = scalar(mean2)
	estadd local Students = scalar(students2)
	
	eststo: reg time_on_attempts_1 assignb i.problem, cluster(anom_id)
	estadd scalar Mean = scalar(mean3)
	estadd local Students = scalar(students3)
	
	eststo: reg time_total assignb i.problem, cluster(anom_id)
	estadd scalar Mean = scalar(mean4)
	estadd local Students = scalar(students4)
	
	eststo: reg score assignb i.problem, cluster(anom_id)
	estadd scalar Mean = scalar(mean5)
	estadd local Students = scalar(students5)
	

	eststo: reg correct assignb i.problem, cluster(anom_id)
	estadd scalar Mean = scalar(mean6)
	estadd local Students = scalar(students6)
	
replace attempted=attempted/100	

*Table A4
	
eststo clear

xtset anom_id problem
	
	eststo: xtreg time_total grade nudge gradenudge i.week  if asian==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum time_total if asian==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg time_total grade nudge gradenudge i.week  if white==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum time_total if white==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg time_total grade nudge gradenudge i.week  if asianwhite==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum time_total if asianwhite==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo:  xtreg time_total grade nudge gradenudge i.week if sat_math_med==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum time_total if sat_math_med==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo:  xtreg time_total grade nudge gradenudge i.week if sat_math_med==2, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum time_total if sat_math_med==2 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo:  xtreg time_total grade nudge gradenudge i.week if sat_math_med==., fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum time_total if sat_math_med==. & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg time_total grade nudge gradenudge i.week  if female==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum time_total if female==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg time_total grade nudge gradenudge i.week  if female==0, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum time_total if female==0 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg time_total grade nudge gradenudge i.week  if final_gpa<=3.24, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum time_total if final_gpa<=3.24 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg time_total grade nudge gradenudge i.week  if final_gpa>3.24 & final_gpa~=., fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum time_total if final_gpa>3.24 & final_gpa~=. & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
/*
local linewidth = 2	
esttab  using "$dbroot/ECO1001_SP18\4work\firststage_het_IB_appendixc.tex", keep(grade nudge gradenudge) style(tex) ///
 stats(Mean N Students  , labels("Untreated Mean" "\# of Obs"  "\# of students" ) fmt(%9.1f %9.0fc %9.0fc %9.1f)) ///
 label replace ///
star( * .1 ** .05 *** .01) b(%12.1f) se(%12.1f) nogaps  ///
mtitles("Asian" "White" "Other" "Below Med" "Above Med" "Miss" "Female" "Male" "Below GPA" "Above GPA") ///
mgroups("Race" "Math SAT Score" "Gender" "Transfer Status", pattern(1 0 0 1 0 0 1 0 1 0) span prefix(\multicolumn{@span}{c}{)  suffix(}) erepeat(\cmidrule(lr){@span}) ) ///
nonotes nonum addnotes("\begin{minipage}{`linewidth'\linewidth} \footnotesize \smallskip  \textbf{Note:} Coefficients are estimated via OLS. All specifications are clustered by individual. All models have individual level fixed effects. Significance levels are indicated by $*$ $<.1$, ** $<.05$,  *** $<.01$.   \end{minipage}" )
	*/

	
	

	
*Table A5 
eststo clear

xtset anom_id problem
	
	eststo: xtreg score grade nudge gradenudge i.week  if asian==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum score if asian==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg score grade nudge gradenudge i.week  if white==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum score if white==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg score grade nudge gradenudge i.week  if asianwhite==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum score if asianwhite==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo:  xtreg score grade nudge gradenudge i.week if sat_math_med==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum score if sat_math_med==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo:  xtreg score grade nudge gradenudge i.week if sat_math_med==2, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum score if sat_math_med==2 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo:  xtreg score grade nudge gradenudge i.week if sat_math_med==., fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum score if sat_math_med==. & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg score grade nudge gradenudge i.week  if female==1, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum score if female==1 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg score grade nudge gradenudge i.week  if female==0, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum score if female==0 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg score grade nudge gradenudge i.week  if final_gpa<=3.24, fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum score if final_gpa<=3.24 & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	eststo: xtreg score grade nudge gradenudge i.week  if final_gpa>3.24 & final_gpa~=., fe  vce(cluster anom_id)
	estadd scalar Students=e(N_clust)
	sum score if final_gpa>3.24 & final_gpa~=. & grade==0 & nudge==0
	estadd scalar Mean=r(mean)
	
	/*
local linewidth = 2	
esttab  using "$dbroot/ECO1001_SP18\4work\firststage_het_IB_appendixd.tex", keep(grade nudge gradenudge) style(tex) ///
 stats(Mean N Students  , labels("Untreated Mean" "\# of Obs"  "\# of students" ) fmt(%9.1f %9.0fc %9.0fc %9.1f)) ///
 label replace ///
star( * .1 ** .05 *** .01) b(%12.1f) se(%12.1f) nogaps  ///
mtitles("Asian" "White" "Other" "Below Med" "Above Med" "Miss" "Female" "Male" "Below GPA" "Above GPA") ///
mgroups("Race" "Math SAT Score" "Gender" "Transfer Status", pattern(1 0 0 1 0 0 1 0 1 0) span prefix(\multicolumn{@span}{c}{)  suffix(}) erepeat(\cmidrule(lr){@span}) ) ///
nonotes nonum addnotes("\begin{minipage}{`linewidth'\linewidth} \footnotesize \smallskip  \textbf{Note:} Coefficients are estimated via OLS. All specifications are clustered by individual. All models have individual level fixed effects. Significance levels are indicated by $*$ $<.1$, ** $<.05$,  *** $<.01$.   \end{minipage}" )
	*/
	


	log close
	exit, clear
	
	eststo: areg time_total grade_1 grade_2 grade_3 nudge i.week, cluster(anom_id) absorb(anom_id)
	estadd scalar Mean = scalar(mean2)
	estadd local Students = scalar(students2)
	


	eststo: areg score grade nudge gradenudge i.week, cluster(anom_id) absorb(anom_id)
	estadd scalar Mean = scalar(mean2)
	estadd local Students = scalar(students2)

	eststo: areg score grade_1 grade_2 grade_3 nudge i.week, cluster(anom_id) absorb(anom_id)
	estadd scalar Mean = scalar(mean2)
	estadd local Students = scalar(students2)
	
	esttab using "$dbroot\ECO1001_SP18\08results\paper_results_table2.rtf", b(%12.2f) se(%12.2f) keep(assignb grade grade_1 grade_2 grade_3 nudge gradenudge) label nomtitles mgroups("Attempted Problem" "Score on Problem Given Attempting", pattern(1 0 0 0 1 0 0 0))    stats(Mean Students N  r2, labels("Mean of Ungraded/Unudged Group" "\# of Obs" "\# of students" "R2") fmt(%9.2f %9.0fc %9.0fc %9.2f)) title("Table 2" "First Stage") order(assignb grade grade_1 grade_2 grade_3 nudge gradenudge)  compress se replace
