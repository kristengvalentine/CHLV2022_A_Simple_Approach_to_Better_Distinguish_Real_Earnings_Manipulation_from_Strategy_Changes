*PCA analysis;
proc factor data = x1a3  method =PRINCIPAL  rotate=VARIMAX nfactors=1 out = x1a4;
var discfo_pred disprod_pred disexp_0miss_resid_pred; 
run; 


data x1a5;
drop factor1 factor2;
set x1a4; 
realem1_pred = factor1;   
realem2_pred = factor2;   
run; 
