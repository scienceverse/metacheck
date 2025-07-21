recode exploratory_1 3=0
recode exploratory_2 3=0
recode exploratory_3 3=0
recode exploratory_4 3=0
recode exploratory_5 3=0
recode exploratory_6 3=0
recode exploratory_7 3=0
recode exploratory_8 3=0
recode exploratory_9 3=0
recode exploratory_10 3=0
recode exploratory_11 3=0
recode exploratory_12 3=0
recode exploratory_1 2=1
recode exploratory_2 2=1
recode exploratory_3 2=1
recode exploratory_4 2=1
recode exploratory_5 2=1
recode exploratory_6 2=1
recode exploratory_7 2=1
recode exploratory_8 2=1
recode exploratory_9 2=1
recode exploratory_10 2=1
recode exploratory_11 2=1
recode exploratory_12 2=1
recode confirm_final_1 3=0
recode confirm_final_2 3=0
recode confirm_final_3 3=0
recode confirm_final_4 3=0
recode confirm_final_5 3=0
recode confirm_final_6 3=0
recode confirm_final_7 3=0
recode confirm_final_8 3=0
recode confirm_final_9 3=0
recode confirm_final_10 3=0
recode confirm_final_11 3=0
recode confirm_final_12 3=0
recode confirm_final_1 2=1
recode confirm_final_2 2=1
recode confirm_final_3 2=1
recode confirm_final_4 2=1
recode confirm_final_5 2=1
recode confirm_final_6 2=1
recode confirm_final_7 2=1
recode confirm_final_8 2=1
recode confirm_final_9 2=1
recode confirm_final_10 2=1
recode confirm_final_11 2=1
recode confirm_final_12 2=1
egen related_factors = rowtotal(confirm_final_1 confirm_final_2 confirm_final_3 confirm_final_4 confirm_final_5 confirm_final_6 confirm_final_7 confirm_final_8 confirm_final_9 confirm_final_10 confirm_final_11 confirm_final_12 exploratory_1 exploratory_2 exploratory_3 exploratory_4 exploratory_5 exploratory_6 exploratory_7 exploratory_8 exploratory_9 exploratory_10 exploratory_11 exploratory_12)
summ related_factors
save "/Users/michaelodonnell/Dropbox/Research/UVA Wave 3 Predicting/UVA_Wave_3_Predicting_to_analyze_with_ac_drops.dta"
preserve
keep if wave1==1
ttest related_factors, by(condition)
restore
preserve
keep if wave1==0
ttest related_factors, by(condition)
restore
ttest related_factors, by(condition)
esize twosample related_factors, by(condition) all
