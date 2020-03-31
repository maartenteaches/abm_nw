clear all
mata
mata clear

x = runiform(1000,1)
y = AssociativeArray()
y.reinit("real")
for(i=1; i<=rows(x); i++) {
	y.put(i,x[i])
}

timer_clear()
for(i=1; i<=1000000; i++){
	timer_on(1)
	foo = x[400]
	timer_off(1)
	timer_on(2)
	blup = x[|400,1 \ 400,1|]
	timer_off(2)
	timer_on(3)
	bar = y.get(400)
	timer_off(3)
}
timer()
end