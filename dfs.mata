cscript
mata
mata set matastrict on
class dfs {
	real                   scalar    n
	real                   scalar    start_node
	pointer(real vector)   vector    adjlist
	real                   vector    visited
	real                   vector    components
	real                   scalar    count
	
	void                            is_posint()
	void                            is_valid_id()
	transmorphic                    n()
	transmorphic                    adjlist()
	transmorphic                    start_node()
	real                   vector   visited()                    
	void                            setup()
	void                            run()
	void                            dfs()
	real                   scalar   count()
	real                   vector   components()
}

void dfs::is_posint(real scalar val) 
{
	string scalar errmsg
	
	if (val <= 0 | mod(val,1)!= 0 ){
		errmsg = "scalar must be a positive integer"
		_error(3300, errmsg)
	}
}

void dfs::is_valid_id(real scalar val)
{
	is_posint(val)
	if (val > n) {
		_error("a node id cannot be larger than n")
	}
}

transmorphic dfs::n(| real scalar val)
{
	if (args()==1) {
		is_posint(val)
		n=val
	}
	else {
		return(n)
	}
}

transmorphic dfs::adjlist(| pointer(real vector) vector list)
{
	if (args() == 1) {
		adjlist = list
	}
	else {
		return(adjlist)
	}
}

transmorphic dfs::start_node(| real scalar val)
{
	if (args() == 1) {
		start_node = val
	}
	else {
		return(start_node)
	}
}


void dfs::setup()
{
	real scalar i, j
	real vector list
	
	if (n == .) {
		_error("number of nodes need to be specified first")
	}
	if (adjlist == J(1,0,NULL)) {
		_error("the adjacency list need to be specified first")
	}
	if (length(adjlist)!=n) {
		_error("length of adjacency list needs to be equal to n")
	}
	for(i=1; i<=n; i++) {
		if (adjlist[i]==NULL) {
			_error(3010, "the adjacency for at least one node is not set")
		} 
		list = *adjlist[i]
		for(j=1; j<=length(list); j++) {
			is_valid_id(list[j])
		}
	}
	if (start_node == .) start_node = 1
	is_valid_id(start_node)
	visited = J(1,n,0)
	count = 0
	components = J(1,n,.)
}

void dfs::dfs(real scalar at)
{
	real scalar i
	real vector neighbours
	visited[at] = 1
	components[at] = count
	
	neighbours = *adjlist[at]
	for(i=1; i<=length(neighbours);i++) {
		if (!visited[i]) dfs(neighbours[i])
	}
}

void dfs::run()
{
	real scalar i
	setup()
	for(i=1; i<=n; i++) {
		count
		if (!visited[i]) {
			count++
			dfs(i)
		}
	}
	dfs(start_node)
	
}
end