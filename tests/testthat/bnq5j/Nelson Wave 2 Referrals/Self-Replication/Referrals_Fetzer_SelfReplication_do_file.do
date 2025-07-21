tabu condition q21
keep if cond==0 & q21==2 | cond==1 & q21==1
egen acceptable_1 = rowtotal(accept_ref_free accept_receive_free)
egen acceptable_2 = rowtotal(accept_ref_low accept_receive_low)
egen acceptable_2 = rowtotal(accept_refer_low accept_receive_low)
egen quality_1 =rowtotal(qual_refer_free qual_receive_free)
egen quality_2 =rowtotal(qual_refer_low receive_qual_low)
gen id=_n
reshape long acceptable_ quality_, i(id) j(incentive)
preserve
keep if wave1==0
xtmixed quality_ condition##incentive|| id:, reml
margins condition##incentive
contrast incentive@condition, effects 
restore
preserve
keep if wave1==1
xtmixed quality_ condition##incentive|| id:, reml
margins condition##incentive
contrast incentive@condition, effects
restore
xtmixed quality_ condition##incentive|| id:, reml
margins condition##incentive
contrast incentive@condition, effects 
