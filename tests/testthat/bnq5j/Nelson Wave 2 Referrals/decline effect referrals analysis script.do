preserve
keep if second_750_true ==1
xtmixed accept_ condition##time || id:,
contrast time@condition, effects 
xtmixed qual_ condition##time || id:, 
contrast time@condition, effects 
restore
preserve
keep if second_750_true ==0
xtmixed accept_ condition##time || id:,
contrast time@condition, effects 
xtmixed qual_ condition##time || id:, 
contrast time@condition, effects 
restore
xtmixed accept_ condition##time || id:,
contrast time@condition, effects 
xtmixed qual_ condition##time || id:, 
contrast time@condition, effects 
