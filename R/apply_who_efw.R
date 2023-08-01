# Build a function to apply the WHO-EFW table
apply_who_efw=function(x){
  ifelse(x<20,500,ifelse(x>36,2500,filter(who_efw,ga==x)$newborn_weight))
}