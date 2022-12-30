/*************************************************************************************************************************************************
**This program creates the REMComp measure from Christensen, T. E., Huffman, A. A., Lewis-Western, M. F., & Valentine, K. (2022). ****************
A simple approach to better distinguish real earnings manipulation from strategy changes. Forthcoming at Contemporary Accounting Research.********

This code only produces the REMComp measure and requires the researcher to use outside data and macros, referenced in the comments below.
The researcher should tailor this code for the specific sample under investigation and note that factor loadings can differ across samples. 
See for example Table 2 Panel B versus Table 8 Panel A in Christensen, Huffman, Lewis-Western and Valentine (2022)
**************************************************************************************************************************************************/

%let wrds = wrds.wharton.upenn.edu 4016; 
options comamid=TCP remote=WRDS;
signon username=_prompt_;
Libname rwork slibref=work server=wrds;

*****************************1 - PULL DATA FROM WRDS************************************************************************;
rsubmit;
data comp; set comp.funda
(keep = gvkey tic conm cik datadate cusip indfmt datafmt popsrc consol curcd fyear sich
		at sale OANCF XIDOC xrd xsga dltt dlc PSTK COGS INVT CHE ceq mibt);
where	(year(datadate) ge 1985)
		and (indfmt='INDL') and (datafmt='STD') and (popsrc='D') and (consol='C') and at>0;
drop indfmt datafmt popsrc consol;
run;
proc sort data=comp out=comp nodupkey; by gvkey fyear;run;


/**NOTE: Jennings, J. N., Kim, J. M., Lee, J. A., & Taylor, D. J. (2020). Measurement error and bias in causal models in accounting research. Available at SSRN 3731197.
find that using the Compustat header file state variable can lead to incorrect inferences due to Compustat backfilling.
In Christensen, Huffman, Lewis-Western and Valentine (2022), we use the firm's true state as listed on the firm's annual 10-K filing in Edgar using data obtained from
Josh Lee's website: https://joshualeeacct.wixsite.com/joshualee/data

The Compustat header file state data is used in this SAS code only as a placeholder. Please replace with Josh Lee's most updated data as indicated in the comments in the code below.
*/
proc sql; create table comp2 as select distinct
		a.*, b.state as state_temp
	from comp as a left join comp.company as b
	on a.gvkey = b.gvkey;
quit;
endrsubmit;

rsubmit;
proc download data=comp2 out=comp;run;
endrsubmit;


*create lagged variables;
proc sort data = comp; by gvkey fyear;run;
data comp; set comp;
lagAT=lag(AT);
if gvkey ne lag(gvkey)  or fyear-1 ne lag(fyear) then do
lagAT=.;
end;

lagsale=lag(sale);
if gvkey ne lag(gvkey)  or fyear-1 ne lag(fyear) then do
lagsale=.;
end;

laginvt=lag(invt);
if gvkey ne lag(gvkey)  or fyear-1 ne lag(fyear) then do
laginvt=.;
end;

run;

data comp; set comp;
avgAT=(at+lagAT)/2;
cfocf = OANCF - XIDOC;
cfocf_s = cfocf/avgAT;
int=1/avgAT;
sale_s = sale/avgAT;
sale_chg = (sale-lagsale)/avgAT;
invt_chg=invt-laginvt;
prod = cogs + invt_chg;
prod_s=prod/avgAT;

xrd_0=xrd;
if xrd_0=. then xrd_0=0;
if xrd_0=0 then xrd_0=0;
rd = xrd_0/avgAT;

if xsga = . then xsga = 0;
xsga_s = xsga/avgAT;
disexp = rd + XSGA_s;

run;


*create lagged variables;
proc sort data = comp; by gvkey fyear;run;
data comp; set comp;
lagsale_chg=lag(sale_chg);
if gvkey ne lag(gvkey)  or fyear-1 ne lag(fyear) then do
lagsale_chg=.;
end;

lagsale_s=lag(sale_s);
if gvkey ne lag(gvkey)  or fyear-1 ne lag(fyear) then do
lagsale_s=.;
end;
run;

*create FF industry classifications;
***see macro code at https://github.com/ed-dehaan/FamaFrenchIndustries; 
%ind_ff48 (dset=comp, outp=comp, sic=sich, bin_var=ind, ind_code=ffinds );



/**
APPLY SAMPLE SELECTION PROCEDURES AND WINSORIZE
**NOTE: In Christensen, Huffman, Lewis-Western and Valentine (2022), we apply sample selection procedures and winsorize variables prior to running the regressions below;
**See Cohen, Pandit, Wasley and Zach (2020) for a discussion of sample composition and winsorization: https://doi.org/10.1111/1911-3846.12553
**/

****Second, run regressions;
proc sort data=comp out=comp;BY ffinds FYEAR ;run;
PROC REG DATA = comp NOPRINT;
model cfocf_s  = int sale_s  sale_chg; BY ffinds FYEAR ;
OUTPUT OUT = out2 R=discfo p=discfo_pred; run;

PROC REG DATA = out2 NOPRINT;
model PROD_S  = int sale_s  sale_chg lagsale_chg; BY ffinds FYEAR ;
OUTPUT OUT = out3 R=disprod p=disprod_pred; run;

PROC REG DATA = out3 NOPRINT;
model disexp  = int lagsale_s; BY ffinds FYEAR ;
OUTPUT OUT = out4 R=disexp_resid p=disexp_resid_pred; run; 

***multiply the cash flow and disexp rem metrics by -1 so that higher values correspond to greater earnings management;
data out4; set out4;
discfo = discfo*-1;
disexp_resid=disexp_resid*-1;
run;


***create litrisk variable;
data out4; set out4;
if sich ge 2833 and sich le 2836 then litrisk=1;
if sich ge 8731 and sich le 8734 then litrisk=1;
if sich ge 3570 and sich le 3577 then litrisk=1;
if sich ge 7370 and sich le 7374 then litrisk=1;
if sich ge 3600 and sich le 3674 then litrisk=1;
if sich ge 5200 and sich le 5961 then litrisk=1;
if litrisk=. then litrisk=0; 
if sich=. then litrisk=.;
run;



***create NOA variable;
data out4; set out4;
if mibt=. then mibt=0;
op_assets= at-che;
op_liabilities=at-dlc-dltt-mibt-pstk-ceq;
netopat=(op_assets-op_liabilities)/lagsale;
run;

*calculate industry-year median of netopat;
proc sql; create table noa_med as select distinct
	ffinds, fyear, median(netopat) as netopat_med
	from out4 group by ffinds, fyear
	having ffinds ne .
	order by ffinds, fyear;
	quit;run;
proc sql;
create table out5 as
	select a.*, b.netopat_med
	from out4 a left join noa_med b
	on a.ffinds=b.ffinds
	and a.fyear = b.fyear;
quit;
proc sort data=out5 nodupkey; by gvkey fyear; run;
data out5; set out5;
if netopat>netopat_med then past_am=1; else past_am=0;
if netopat=. or netopat_med=. then past_am=.;
run;


**create district circuit litigation variable;
*bring in Josh Lee's state of incorporation data obtained from https://joshualeeacct.wixsite.com/joshualee/data;
/*
For a description of how the headquarters data was collected, see:
?Jennings, J., J. Lee, and D. Matsumoto. 2017. “The Effect of Industry Co-location on Analysts’ Information Acquisition Costs,” The Accounting Review, 92 (6), 103-127.
?
For a comparison of this headquarters data to other headquarters data sources (e.g., Compustat, SEC EDGAR Filing Headers, etc.), see:
?Jennings, J., J. Kim, J. Lee, and D. Taylor. 2020. “Measurement Error and Bias in Causal Models in Accounting Research,” Working Paper
*/





/**NOTE: This code uses the Compustat header file state information only as a placeholder;

In Christensen, Huffman, Lewis-Western and Valentine (2022), we use the firm's true state as listed on the firm's annual 10-K filing in Edgar using data obtained from
Josh Lee's website: https://joshualeeacct.wixsite.com/joshualee/data

The Compustat header file state data is used in this SAS code only as a placeholder. Please replace with Josh Lee's most updated data.
*/
*To link zip code to state, obtained data from https://simplemaps.com/data/us-zips;
data out5; set out5;
ninth = 0;
if state_temp in ('AK' , 'AZ' , 'CA' , 'HI' , 'ID' , 'MT' , 'NV' , 'OR' , 'WA') then ninth = 1;
if state_temp="" then ninth=.;
run;

data out6; set out5; 
if ninth ne . then high_litrisk_9=0;
if fyear<=1999 and ninth=1 then high_litrisk_9 = 1;
run;

*PCA analysis - varimax rotation;
proc factor data = out6  method =PRINCIPAL  rotate=VARIMAX nfactors=1 out = x1;
var discfo disprod disexp_resid litrisk high_litrisk_9 past_am; 
run; 


data x1a;
drop factor1;
set x1; 
realem1 = factor1;   
run; 

