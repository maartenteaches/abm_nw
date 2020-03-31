cscript
cd H:\abm\abm_nw
do dfs.mata
mata:
	mata set matastrict on
	
	
	foo = dfs()
	foo.n(10)
	list = J(10,1,NULL)
	list[1] = &((2,4))
	list[2] = &((1,4))
	list[3] = &((9))
	list[4] = &((1,2,6))
	list[5] = &((6))
	list[6] = &((4,5,7,8))
	list[7] = &((6))
	list[8] = &((6))
	list[9] = &((3,10))
	list[10] = &((9))
	foo.adjlist(list)
	foo.run()
	foo.count
	foo.components
end