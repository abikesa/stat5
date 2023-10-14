**** Lab 4 document ****

* Read in the dataset
import delimited C:\Users\Elizabeth\Dropbox\MLM2019\Labs\Lab42019\guatemala.csv, clear

** Identify the number of clusters(communities), moms and kids
codebook cluster mom kid

** Compute the number of mothers evaluated in each community
bys cluster mom: gen kid_counter = _n
bys cluster: egen junk = count(mom) if kid_counter==1
bys cluster: egen num_moms = min(junk)
drop junk
bys cluster: gen cluster_counter = _n
summ num_moms if cluster_counter==1

** Compute the number of kids per cluster
bys cluster: egen cluster_kids = count(cluster)
summ cluster_kids if cluster_counter==1

** Compute the number of kids per mom
tab num_kids if kid_counter==1

** What is the prevalence of completing the full course of 
** immunizations
summ immun

** How does the prevalence of completing the full course of
** immunizations differ with and without the campaign
bys kid2p: summ immun


********************************************************
********************************************************
** Fit the random intercept only model
********************************************************
********************************************************
meqrlogit immun || cluster: || mom: , intp(12)
meqrlogit, eform


********************************************************
********************************************************
** Fit the random intercept + kid2p model
********************************************************
********************************************************
meqrlogit immun kid2p || cluster: || mom: , intp(12)
meqrlogit, or

********************************************************
********************************************************
** Fit the random intercept + random slope kid2p model
********************************************************
********************************************************
meqrlogit immun kid2p || cluster: kid2p , cov(uns) ///
|| mom: , intp(12)
meqrlogit, or

********************************************************
********************************************************
** Fit the random intercept + random slope kid2p model
********************************************************
********************************************************
gen kid2p_rural = kid2p*rural
gen kid2p_pcind = kid2p*pcind

meqrlogit immun kid2p rural pcind kid2p_rural kid2p_pcind ///
|| cluster: kid2p , cov(uns) || mom: , intp(12)
meqrlogit, or
