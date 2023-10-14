	 clear
input n r id
47 0 1
148 18 2
119 8 3
810 46 4
211 8 5
196 13 6
148 9 7
215 31 8
207 14 9
97 8 10
256 29 11
360 24 12
end

gllamm r ,i(id) family(binomial) denom(n) adapt
gllapred b0, u
gllapred p, mu
