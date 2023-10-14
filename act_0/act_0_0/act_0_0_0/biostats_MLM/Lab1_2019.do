** Change the working directory
cd "C:\Users\Elizabeth\Documents\MLM2019"

** Open the dataset
import delimited "hsb_data.csv", clear

** 2) Data visualization and model fit
* Keep just the first 25 schools
keep if newid <= 25
* Create a figure decomposing the within and between information
set scheme s1mono
graph box mathach, over(newid)
* Fit the two-stage normal-normal model
mixed mathach || newid:

** 4a) Fix theta, y-bar, tau^2 and sigma^2, change n
clear
set obs 70
gen n = _n
gen b_1 = 11.1 / (11.1 + 38.4 / n) * (7.6 - 13)
gen b_2 = 11.1 / (11.1 + 38.4 / n) * (16.3 - 13)
gen b_3 = 11.1 / (11.1 + 38.4 / n) * (11.1 - 13)
twoway (line b_1 n, yline(0) lc(black) ytitle("Estimated b_i") ///
xtitle("School sample size") xlab(0(10)70) ylab(-6(2)6, angle(horizontal)) ///
legend(pos(11) ring(0) col(1) label(1 "newid = 3") ///
label(2 "newid = 4") label(3 "newid = 22") region(lstyle(none)))) /// 
(line b_2 n, lc(red)) (line b_3 n, lc(blue))
 
 ** 4b) Fix theta, y-bar, n, sigma^2, change tau^2
 clear
 set obs 76
 gen tausquared = _n - 1
 gen b_1 = tausquared / (tausquared + 38.4 / 48) * (7.6 - 13)
gen b_2 = tausquared / (tausquared + 38.4 / 20) * (16.3 - 13)
gen b_3 = tausquared / (tausquared + 38.4 / 67) * (11.1 - 13)
twoway (line b_1 tausquared, yline(0) lc(black) ytitle("Estimated b_i") ///
xtitle("Variance between School-mean MA scores") xlab(0(15)75) ylab(-6(2)6, angle(horizontal)) ///
legend(pos(11) ring(0) col(1) label(1 "newid = 3") ///
label(2 "newid = 4") label(3 "newid = 22") region(lstyle(none)))) /// 
(line b_2 tausquared, lc(red)) (line b_3 tausquared, lc(blue))

 
 ** 4c) Fix theta, y-bar, n, tau^2, change sigma^2
 clear
 set obs 1600
 gen sigmasquared = _n - 1
 gen b_1 = 11.1 / (11.1 + sigmasquared / 48) * (7.6 - 13)
gen b_2 = 11.1 / (11.1 + sigmasquared / 20) * (16.3 - 13)
gen b_3 = 11.1 / (11.1 + sigmasquared / 67) * (11.1 - 13)
twoway (line b_1 sigmasquared, yline(0) lc(black) ytitle("Estimated b_i") ///
xtitle("Variance in MA scores among students from the same school") xlab(0(200)1600) ylab(-6(2)6, angle(horizontal)) ///
legend(pos(11) ring(0) col(1) label(1 "newid = 3") ///
label(2 "newid = 4") label(3 "newid = 22") region(lstyle(none)))) /// 
(line b_2 sigmasquared, lc(red)) (line b_3 sigmasquared, lc(blue))

** 4d) Fix theta, y-bar, n, total variance (50) and change ICC
 clear
 set obs 101
 gen ICC = (_n - 1) / 100
 gen b_1 = ICC*50 / (ICC*50 + (1-ICC)*50 / 48) * (7.6 - 13)
gen b_2 = ICC*50 / (ICC*50 + (1-ICC)*50 / 20) * (16.3 - 13)
gen b_3 = ICC*50 / (ICC*50 + (1-ICC)*50 / 67) * (11.1 - 13)
twoway (line b_1 ICC, yline(0) lc(black) ytitle("Estimated b_i") ///
xtitle("Intraclass correlation coefficient") xlab(0(0.2)1) ylab(-6(2)6, angle(horizontal)) ///
legend(pos(11) ring(0) col(1) label(1 "newid = 3") ///
label(2 "newid = 4") label(3 "newid = 22") region(lstyle(none)))) /// 
(line b_2 ICC, lc(red)) (line b_3 ICC, lc(blue))


