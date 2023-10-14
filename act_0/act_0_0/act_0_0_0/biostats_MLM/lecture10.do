* Set the working directory

cd "C:\Users\ejohnson\Documents\MLM2010"

* Set the plotting scheme

set scheme s1mono

* Open the dataset

use "fife.dta", clear

* Explore the crossed structure of the data

egen pick_combo = tag(pid sid)

* Count the number of unique values of sid
* within each pid using egen total() function

egen numsid = total(pick_combo), by(pid)
sort pid sid

* List the sid values within the first 8 pids

list pid sid numsid if pick_combo & pid<9, sepby(pid) noobs

* How many secondary schools per primary school

egen pick_pid = tag(pid)
tab numsid if pick_pid

* How many primary schools per secondary school

egen numpid = total(pick_combo), by(sid)
egen pick_sid = tag(sid)
tab numpid if pick_sid

* Fit the model using xtmixed

xtmixed attain || _all: R.sid || pid:
estimates store model1
predict u* , reffects
predict seu*, reses
predict fit, fitted

list pid sid u2 u1 fit if pick_combo & pid<9, sepby(pid) noobs

* Generate a graph of the primary school effects with 95% confidence bounds

gen lower = u2-2*seu2
gen upper = u2+2*seu2

twoway (rcap lower upper pid, legend(off) ytitle(Primary school effect) xtitle(Primary school ID) ylab(-4(2)4, angle(horizontal))) (scatter u2 pid, ms(s) mc(black))

* Generate a graph of the secondary school effects with 95% confidence bounds

gen lower_sec = u1 - 2*seu1
gen upper_sec = u1 + 2*seu1

twoway (rcap lower_sec upper_sec sid, legend(off) ytitle(Secondary school effect) xtitle(Secondary school ID) ylab(-4(2)4, angle(horizontal))) (scatter u1 sid, ms(s) mc(black))

* Fitting the interaction model

xtmixed attain || _all: R.sid || pid: || sid: 
estimates store model2
predict ui*, reffects
predict seui*, reses
predict fiti, fitted

list pid sid ui2 ui1 ui3 fiti if pick_combo & pid<9, sepby(pid) noobs

lrtest model1 model2
