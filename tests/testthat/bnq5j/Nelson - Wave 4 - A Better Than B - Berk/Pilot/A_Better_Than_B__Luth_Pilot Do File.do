tabu v10
drop if v10==0
gen condition_worse =.
replace condition=1 if missing(better_b)
replace condition = 0 if missing(condition)
tabu condition
egen rating_b = rowtotal (better_b worse_b)
tabu b_top10
keep if b_top10==1
ttest better_b== worse_b, unpaired
esize twosample rating_b, by(condition) all
