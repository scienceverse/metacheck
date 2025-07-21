keep if att_check ==1
tabu v10
egen condition_better = rowtotal(better)
tabu condition_better
egen b_total = rowtotal(better_b worse_b)
tabu b_total
preserve
keep if second_750 ==1
ttest better_b == worse_b, unpaired
. esize twosample b_total, by(condition_better) all
restore
preserve
keep if second_750 ==0
ttest better_b == worse_b, unpaired
. esize twosample b_total, by(condition_better) all
restore
ttest better_b == worse_b, unpaired
. esize twosample b_total, by(condition_better) all
