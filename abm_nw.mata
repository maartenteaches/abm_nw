version 16.1

clear all
mata:

mata set matastrict on

class abm_nw
{
	protected: 
		real                   vector    N_edges
		real                   vector    N_nodes
		real                   scalar    N_edges0
		real                   scalar    N_nodes0		
		real                   scalar    maxnodes
		real                   scalar    tdim
		real                   scalar    directed
		real                   scalar    weighted
		real                   scalar    nodes_set
		real                   scalar    nw_set
		real                   scalar    setup
		real                   scalar    randomit
		real                   vector    frozen
		
		class AssociativeArray scalar    network
		pointer(real vector)   vector    adjlist0
		pointer(real vector)   matrix    adjlist
		real                   vector    nodes0
		pointer(real vector)   vector    nodes
		pointer(real vector)   vector    dropped_nodes
		real                   vector    dropped_nodes0
		
		void                             is_valid_id()
		void                             is_valid_time()
		void                             is_frozen()
		void                             is_setup()
		void                             is_nodesset()
		void                             is_posint()
		void                             is_bool()
		void                             is_pr()
		void                             is_symmetric()
		
		void                             copy_adjlist()
		void                             copy_nodes()
		void                             new()
	
	public:
		void                             from_edgelist()
		void                             from_adjlist()
		void                             from_adjmatrix()
		void                             random()
		void                             sw()                                       //small world
		transmorphic                     N_nodes()
		real                   scalar    N_edges()
		
		transmorphic                     tdim()
		transmorphic                     directed()
		transmorphic                     randomit()
		transmorphic                     weighted()
		void                             setup()
		
		void                             copy_nw()                                // copy network from t-1 to t, also calls copy_adjlist() and copy_nodes()
		void                             add_edge()
		void                             add_node()
		void                             return_node()
		void                             remove_edge()
		void                             change_weight()
		void                             remove_node()
		void                             rewire()
        void                             clear()
		
		real                   rowvector neighbours()
		real                   scalar    weight()
		void                             no_edge()
		real                   scalar    edge_exists()
		real                   vector    schedule()
		
		real                   matrix    export_adjmat()
		real                   matrix    export_edgelist()
}


real scalar abm_nw::edge_exists(real scalar ego, real scalar alter, | real scalar t, string scalar fast)
{
	real scalar res
	
	if (args()==2) t=0
	if (args()<4) {
		is_valid_id(ego, t)
		is_valid_id(alter, t)
	}
	if (t==0) {
		res = anyof(*adjlist0[ego], alter)
	}
	else {
		res = anyof(*adjlist[ego,t], alter)
	}
	return(res)
}

void abm_nw::is_setup()
{
	if(setup == 0) _error(3000,"setup is required")
}

void abm_nw::is_nodesset()
{
	if (nodes_set==0) _error(3000, "setting number of nodes is required")
}

real rowvector abm_nw::neighbours(real scalar ego, | real scalar t, string scalar dropped_ok, string scalar fast)
{
	if (t==.) t=0
	if (args() < 4) {
		is_valid_id(ego, t, dropped_ok)
		is_nodesset()
	}
	if (t==0) {
		return(*adjlist0[ego])
	} 
	else {
		return(*adjlist[ego,t])
	}
	
} 

void abm_nw::is_symmetric(| real scalar t)
{
	real scalar i, j, stop
	real vector cols
	
	is_nodesset()
	is_valid_time(t)
	if (t==.) t=0
	
	
	stop = 0

	for(i=1; i <= maxnodes;i++) {
		cols = neighbours(i,t, "dropped_ok")
		for(j=1; j<=length(cols);j++){
			if (weight(i,cols[j],t) != weight(cols[j],i,t)) {
				stop = 1
				_error(3000,"network is not symmetric")
				break
			}
		}
		if (stop) break
	}

}

void abm_nw::is_valid_time(real scalar time)
{
	if (time != .) {
		if (tdim == 0 & time > 0) {
			_error(3000,"time is specified while there is no time dimension")
		}
		if (tdim==. & time > 0) {
			_error(3000, "time is specified without specifying tdim")
		}
		is_posint(time, "zero_ok")
		if(time > tdim) {
			_error(3000,"specified time exceeds tdim")
		}
	}
}

void abm_nw::is_valid_id(real scalar id, | real scalar time, string scalar dropped_ok)
{
	is_nodesset()
    is_valid_time(time)
	if (dropped_ok == "") {
		if (args()==1 | time == 0) {
			if(!anyof(nodes0,id)) {
				_error(3000,"invalid id")
			}
		}
		else{
			if(!anyof(*nodes[time],id)) {
				_error(3000,"invalid id")
			}
		}	
	}
	else {
		if (id < 0 | id > maxnodes | mod(id,1)!=0 ) {
			_error(3000, "invalid id")
		}
	}
}

void abm_nw::is_frozen(| real scalar t)
{
	if (t==.) t=0
	is_valid_time(t)
	if (t==0 & setup == 1) {
		_error("network has been frozen")
	}
	if(t>0 ) {
		if (frozen[t]==1) _error("network has been frozen")
	}
}

void abm_nw::is_bool(real scalar val)
{
	string scalar errmsg
	
	if (val != 1 & val != 0) {
		errmsg = "scalar can only be 0 or 1"
		_error(3300, errmsg)
	}
}

void abm_nw::is_posint(real scalar val, | string scalar zero_ok) 
{
	string scalar errmsg
	
	if (args() == 1) {
		if (val <= 0 | mod(val,1)!= 0 ){
			errmsg = "scalar must be a positive integer"
			_error(3300, errmsg)
		}
	}
	else {
		if (val < 0 | mod(val,1)!= 0 ){
			errmsg = "scalar must be zero or a positive integer"
			_error(3300, errmsg)
		}
	}
}

void abm_nw::is_pr(real scalar val)
{
	if (val < 0 | val > 1) {
		_error(3300,"scalar is not a valid probability")
	}
}

void abm_nw::new(){
    nodes_set = 0
	nw_set    = 0
	setup     = 0
	N_edges0   = 0
	network.reinit("real",3)
	network.notfound(0)
}
transmorphic abm_nw::N_nodes( real scalar t, | real scalar N){
		real scalar i
		is_valid_time(t)
		if (t==.) t=0
		
		if (args()==2){
			if (t!=0) _error("number of nodes can only be set for t=0")
		    is_posint(N)
			is_frozen()
			N_nodes0 = N
			adjlist0 = J(N_nodes0,1,NULL)
			for(i=1; i<=N_nodes0; i++) {
				adjlist0[i] = &(J(1,0,.))
			}
			nodes0 = 1..N_nodes0
			maxnodes = N_nodes0
			dropped_nodes0 = J(1,0,.)
			nodes_set=1
		}
		else {
			if (t==0) {
				return(N_nodes0)
			}
		    else {
				return(N_nodes[t])
			}
		}
}

transmorphic abm_nw::randomit(| real scalar bool)
{
    if(args()==1) {
	    is_bool(bool)
		is_frozen()
		randomit = bool
	}
	else {
	    return(randomit)
	}
}



real scalar abm_nw::N_edges(| real scalar t)
{
	is_valid_time(t)
	if (t==.) t = 0
	if (t==0) {
		return(N_edges0)
	}
	else {
		return(N_edges[t])
	}
}
	
void abm_nw::no_edge(real scalar t, real scalar orig, real scalar dest, | string scalar fast) 
{
	if (args()<4) {
		is_valid_id(orig,t)
		is_valid_id(dest,t)
	}
	if(edge_exists(orig, dest, t, "fast")) {
		_error(3000,"edge already defined")
	}
	if (directed == 0) {
		if(edge_exists(dest,orig,t, "fast")) {
			_error(3000,"edge already defined")
		}		
	}
}

transmorphic abm_nw::tdim(| real scalar t)
{
    real scalar i,j
	
	if(args()==1) {
	    is_frozen()
	    is_posint(t, "zero_ok")
	    is_nodesset()
		tdim = t
		if (tdim > 0) {
			adjlist = J(maxnodes, tdim, NULL)
			nodes = J(tdim,1,NULL)
			N_nodes = J(tdim,1,.)
			N_edges = J(tdim,1,0)
			dropped_nodes = J(tdim,1,NULL)
			frozen = J(tdim,1,0)
			for(i=1;i<=tdim; i++) {
				dropped_nodes[i] = &(J(1,0,.))
			}
			
			for(i=1; i<= maxnodes; i++) {
				for(j=1; j<=tdim; j++) {
					adjlist[i,j] = &(J(1,0,.))
				}
			}
		}
	}
	else{
	    return(tdim)
	}
}

transmorphic abm_nw::directed(| real scalar bool)
{
    if (args()==1)  {
	    is_bool(bool)
		is_frozen()
		directed = bool
	}
	else{
	    return(directed)
	}
}

transmorphic abm_nw::weighted(| real scalar bool)
{
	if(args()==1) {
		is_bool(bool)
		is_frozen()
		weighted = bool
	}
	else {
		return(weighted)
	}
}

void abm_nw::setup(| string scalar fast)
{
	if (tdim==.) {
	    tdim = 0
	}
	if (directed == .) {
	    directed = 1
	}
	if (directed == 0 & args() == 0) {
		is_symmetric()
	}
	if (randomit == .) {
	    randomit = 0
	}
	if (weighted == .) weighted = 1
	setup = 1
}

void abm_nw::from_adjlist(real matrix adj)
{
	real scalar i,j, orig
	
	if (nw_set==1) {
		_error(3000,"initial network already set")
	}
	if (dropped_nodes0!=J(1,0,.)) _error("nodes have been dropped")
	if (directed==.) _error("directed needs to be set")
	if (cols(adj) < 1) _error("number of cols needs to be 1 or higher")
	for (i=1 ; i<=rows(adj) ; i++) {
		orig = adj[i,1]
		for (j=2 ; j<= cols(adj) ; j++) {
			if (adj[i,j] != .) {
				add_edge(0,orig,adj[i,j])
			}
		}
	}
	nw_set = 1
}


void abm_nw::from_edgelist(real matrix edges)
{
	real scalar    i

	if (nw_set==1) {
		_error(3000,"initial network already set")
	}
	if (dropped_nodes0!=J(1,0,.)) _error("nodes have been dropped")
	if(cols(edges) < 2) {
		_error(3200, "edges must have at least 2 columns")
	}
	if(cols(edges) > 3) {
	    _error(3200, "edges cannot have more than 3 columns")
	}
	
	if (cols(edges)==3 & weighted == 0) _error("can't add weights to unweighted network'")
	if (directed==.) _error("directed needs to be set")
	if (cols(edges)==2) {
	    edges = edges , J(rows(edges),1,1)
	}
	for (i=1; i <= rows(edges); i++) {
		add_edge(0,edges[i,1], edges[i,2], edges[i,3])
	}
	nw_set = 1
}

void abm_nw::from_adjmatrix(real matrix adjmat)
{
	real scalar i, j, max

	if (nw_set==1) {
		_error(3000, "initial network already set")
	}
	if (dropped_nodes0!=J(1,0,.)) _error("nodes have been dropped")
	if (rows(adjmat) != N_nodes0 | cols(adjmat) != N_nodes0) {
		_error(3000, "matrix must have N_nodes0 rows and columns")
	}
	if (directed==0 & issymmetric(adjmat) == 0) {
		_error(3000, "matrix must be symmetric for non-directed network")
	}
	if (weighted == 0 & !all((adjmat:==0):|(adjmat:==1))) {
	    _error("can't add weights to an unweighted matrix'")
	}
	for(i=1; i<=N_nodes0; i++) {
		max = (directed == 0 ? i: N_nodes0)
		for(j=1; j<=max; j++) {
			if (adjmat[i,j]!=0 & i!=j) {
				add_edge(0,i,j,adjmat[i,j])
			}
		}
	}
	nw_set = 1
}

void abm_nw::random(real scalar pr) {                                         
	
	real scalar i, j, max
	
	if (nw_set==1) {
		_error(3000,"initial network already set")
	}
	is_nodesset()
	if (dropped_nodes0!=J(1,0,.)) _error("nodes have been dropped")
	if (weighted == .) weighted = 0
	if (weighted == 1) _error("random makes unweighted an network")
	is_pr(pr)
	for(i=1; i<=N_nodes0;i++) {
		max = ( directed == 0 ? i : N_nodes0 )
		for(j=1; j<=max; j++) {
			if (i!=j & runiform(1,1)<=pr) {
				add_edge(0,i,j)
			}
		}
	}
	nw_set = 1
}

void abm_nw::sw(real scalar degree, real scalar pr)
{
	real scalar left, right, i, j, alt_dest
	real vector basedest, dest

	if (nw_set==1) {
		_error(3000,"initial network already set")
	}
	is_nodesset()
	if (dropped_nodes0!=J(1,0,.)) _error("nodes have been dropped")
	if (weighted == .) weighted = 0
	if (weighted == 1) _error("sw makes unweighted an network")
	is_posint(degree)
	if (directed==0 & mod(degree,2)!= 0) {
		_error(3000, "in an unidirected network the degree has to be even")
	} 	
	if (degree>N_nodes0) {
		_error(3000, "degree has be less than the number of nodes")
	}
	is_pr(pr)
	
	left = -floor(degree/2)
	right = ceil(degree/2)
	
	if (directed == 0) {
		basedest = 1..right
		for(i=1; i <= N_nodes0 ; i++) {
			dest = basedest :+ i
			dest = mod(dest:-1, N_nodes0):+ 1
			for(j=1; j <= right; j++) {
				if (runiform(1,1)>pr) {
					add_edge(0,i,dest[j],1,"replace", "fast")
				}
				else {
					alt_dest = ceil(runiform(1,1)*(N_nodes0-1))
					alt_dest = alt_dest + (alt_dest>=i)
					add_edge(0,i,alt_dest,1,"replace", "fast")
				}
			}
		}
	}
	else {
		if (left != 0) {
			basedest = (left..-1)
		}
		else {
			basedest = J(1,0,.)
		}
		basedest = basedest , (1..right)
		for(i=1; i<= N_nodes0; i++){
			dest = basedest :+ i
			dest = mod(dest :-1, N_nodes0) :+ 1
			for(j=1; j<=degree;j++){
				if (runiform(1,1)>pr) {
					add_edge(0,i,dest[j],1,"replace", "fast")
				}
				else {
					alt_dest = ceil(runiform(1,1)*(N_nodes0-1))
					alt_dest = alt_dest + (alt_dest>=i)
					add_edge(0,i,alt_dest,1,"replace", "fast")
				}
			}
		}
	}
	nw_set=1
}

void abm_nw::add_edge(real scalar t, real scalar orig, real scalar dest,| real scalar weight, string scalar replace, string scalar fast)
{
	real rowvector key
	real scalar change
	
	if (weight==0) return
	
	if (args()!=6) {
		is_valid_id(orig,t)  // also checks if t is valid
		is_valid_id(dest,t)
		is_frozen(t)
		if (weighted == 0 & (weight != . & weight != 0 & weight != 1)){
			_error("can't set weights for unweighted network'")
		} 
		if (args() <= 4) no_edge(t,orig, dest)
		if (args() == 3) weight = 1
	}
	change = edge_exists(orig,dest,t, "fast")
	if (weighted) {
		key = t, orig, dest
		network.put(key, weight)
	}
	if (change==0) {
		if (t == 0) {
			adjlist0[orig] = &(*adjlist0[orig], dest)
			N_edges0 = N_edges0 + 1
		}
		else {
			adjlist[orig,t] = &(*adjlist[orig,t], dest)
			N_edges[t] = N_edges[t] + 1
			
		}
	}	
	if (directed == 0) {
		change = edge_exists(dest,orig,t, "fast")
		if (weighted) {
			key = t, dest, orig
			network.put(key, weight)
		}
		if (change==0) {
			if (t == 0) {
				adjlist0[dest] = &(*adjlist0[dest], orig)
				N_edges0 = N_edges0 + 1
			}
			else {
				adjlist[dest,t] = &(*adjlist[dest,t], orig)
				N_edges[t] = N_edges[t] + 1
			}		
		}
	}
}

void abm_nw::remove_edge(real scalar t, real scalar orig, real scalar dest)
{
    real rowvector key, adj, adj2
	
    is_valid_id(orig,t)
	is_valid_id(dest,t)
	is_frozen(t)
    if (!edge_exists(orig,dest,t, "fast")) {
	    _error(3000, "no edge to remove")
	}
	
	if (weighted) {
		key = t, orig, dest
		network.remove(key)
	}
	if (t==0) {
	    adj = *adjlist0[orig]
		adj = select(adj, adj:!=dest)
		adjlist0[orig] = &adj
		N_edges0 = N_edges0 - 1
	}
	else {
	    adj = *adjlist[orig, t]
		adj = select(adj, adj:!=dest)
		adjlist[orig,t] = &adj
		N_edges[t] = N_edges[t] - 1
	}
	if (directed == 0) {
		if (weighted) {
			key = t, dest, orig
			network.remove(key)
		}
		if (t==0) {
			adj2 = *adjlist0[dest]
			adj2 = select(adj2, adj2:!=orig)
			adjlist0[dest] = &adj2
			N_edges0 = N_edges0 - 1
		}
		else {
			adj2 = *adjlist[dest, t]
			adj2 = select(adj2, adj2:!=orig)
			adjlist[dest,t] = &adj2
			N_edges[t] = N_edges[t] - 1
		}	    
	}
}

void abm_nw::change_weight(real scalar t, real scalar orig, real scalar dest, real scalar val)
{
	is_valid_id(orig,t)
	is_valid_id(dest,t)
	is_frozen(t)
	if (weighted==0) _error("no weight to change")
	
    if (!edge_exists(orig,dest,t, "fast")) {
	    _error(3000, "no edge to change")
	}
	add_edge(t, orig, dest, val, "replace", "fast")
}

void abm_nw::rewire(real scalar t, real scalar orig0, real scalar dest0,
    real scalar orig1, real scalar dest1)
{
    real scalar val
	
    is_valid_id(orig0,t)
	is_valid_id(orig1,t)
	is_valid_id(dest0,t)
	is_valid_id(dest1,t)
	is_frozen(t)
	if(!edge_exists(orig0,dest0,t, "fast")) {
	    _error("no edge to rewire")
	}
	
	val = weight(orig0,dest0,t)
	remove_edge(t,orig0, dest0)
	add_edge(t, orig1, dest1, val, "replace")
}

void abm_nw::remove_node(real scalar t, real scalar id)
{
	real scalar i, j
	real vector cols
	
	is_valid_id(id,t)
	is_frozen(t)
	if (t==0) {
		dropped_nodes0 = dropped_nodes0, id
		N_nodes0 = N_nodes0 - 1
		nodes0 = select(nodes0, nodes0:!=id)
		if (weighted){
			for(i=1; i<=maxnodes; i++) {
				cols = *adjlist0[i]
				for(j=1; j<= length(cols); j++) {
					if (i==id | cols[j] == id) {
						network.remove((t,i,cols[j]))
					}
				}
			}
		}
		adjlist0[id] = &(J(1,0,.))
		for(i=1;i<=maxnodes;i++) {
			adjlist0[i] = &(select(*adjlist0[i],*adjlist0[i]:!=id))
			if (*adjlist0[i] == J(0,0,.)) adjlist0 = &(J(1,0,.))
		}
	}
	else {
		dropped_nodes[t] = &(*dropped_nodes[t] , id)
		N_nodes[t] = N_nodes[t] - 1
		nodes[t] = &(select(*nodes[t],*nodes[t]:!=id))
		if (weighted) {
			for(i=1; i<=maxnodes; i++) {
				cols = *adjlist[i,t]
				for(j=1; j<= length(cols); j++) {
					if (i==id | cols[j] == id) {
						network.remove((t,i,cols[j]))
					}
				}
			}
		}
		adjlist[id,t] = &J(1,0,.)
		for(i=1;i<=maxnodes;i++) {
			adjlist[i,t] = &(select(*adjlist[i,t],*adjlist[i,t]:!=id))
			if (*adjlist[i,t] == J(0,0,.)) adjlist[i,t] = &(J(1,0,.))
		}
	}
}

void abm_nw::return_node(real scalar t, real scalar id) 
{
    is_valid_time(t)
	is_frozen(t)
    if (t==0) {
	    if (!anyof(dropped_nodes0,id)) {
		    _error("a node can only be returned if it was previously dropped")
		}
	}
	else {
	    if (!anyof(*dropped_nodes[t], id)){
		    _error("a node can only be returned if it was previously dropped")
		}
	}
	if (t==0) {
	    dropped_nodes0 = select(dropped_nodes0, dropped_nodes0:!=id)
		if ( dropped_nodes0 == J(0,0,.) ) dropped_nodes0 = J(1,0,.)
		nodes0 = nodes0 , id
		N_nodes0 = N_nodes0 + 1
	}
	else {
	    dropped_nodes[t] = &(select(*dropped_nodes[t], *dropped_nodes[t] :!= id))
		if (*dropped_nodes[t] == J(0,0,.)) dropped_nodes[t] = &J(1,0,.)
		nodes[t] = &(*nodes[t], id)
		N_nodes[t] = N_nodes[t] + 1
	}
	
}

void abm_nw::add_node(real scalar t)
{
    real scalar i
	pointer (real vector) vector toadd
	
    is_valid_time(t)
	is_frozen(t)
	maxnodes = maxnodes +1
	
	if (t== 0) {
	    nodes0 = nodes0 , maxnodes
		N_nodes0 = N_nodes0 + 1
	}
	else {
	    nodes[t] = &(*nodes[t], maxnodes)
		dropped_nodes0 = dropped_nodes0, maxnodes
		for(i=1; i<t; i++) {
		    dropped_nodes[i] = &(*dropped_nodes[i], maxnodes)
		}
		N_nodes[t] = N_nodes[t] + 1
	}
	adjlist0 = adjlist0 \ &J(1,0,.)
	if (tdim>0) {
	    toadd = J(1,tdim,NULL)
		for(i=1; i<=tdim; i++) {
		    toadd[i] = &J(1,0,.)
		}
		adjlist = adjlist \ toadd
	}
}

void abm_nw::copy_nodes(real scalar t0, real scalar t1)
{
	real vector orig, dropped
	real scalar n
	
	is_valid_time(t0)
	is_valid_time(t1)
	if (nodes[t1] != NULL) _error("nodes for t1 already set")
	if (t0==0) {
		orig    = nodes0
		n       = N_nodes0
		dropped = dropped_nodes0
	}
	else {
		orig    = *nodes[t0]
		n       = N_nodes[t0]
		dropped = *dropped_nodes[t0]
	}
	nodes[t1]         = &orig
	N_nodes[t1]       = n
	dropped_nodes[t1] = &dropped
}

void abm_nw::copy_adjlist(real scalar t0, real scalar t1)
{
	real scalar i
	
	is_valid_time(t0)
	is_valid_time(t1)

	if(t0==0){
		for(i=1;i<=rows(adjlist0);i++) {
			adjlist[i,t1] = &(*adjlist0[i])
		}
	}
	else {
		for(i=1;i<=rows(adjlist);i++) {
			adjlist[i,t1] = &(*adjlist[i, t0])
		}		
	}
}
void abm_nw::copy_nw(real scalar t0, real scalar t1)
{
	real scalar i, j, val
	real vector cols, key0, key1
	
	is_valid_time(t0)
	is_valid_time(t1)
	is_frozen(t1)
	is_setup()

	copy_nodes(t0, t1)
	copy_adjlist(t0, t1)
	if (t0==0) {
		val = N_edges0
	}
	else{
		val = N_edges[t0]
		frozen[t0] = 1
	}
	N_edges[t1] = val
	if (weighted) {
		for(i=1; i<=maxnodes; i++) {
			cols = *adjlist[i,t1]
			for(j=1; j <= length(cols); j++) {
				key0 = t0, i, cols[j]
				key1 = t1, i, cols[j]
				network.put(key1, network.get(key0))
			}
		}
	}
}

real matrix abm_nw::export_adjmat(|real scalar t) 
{
	real matrix res
	real vector dropped, cols
	real scalar i, j

	is_valid_time(t)
	if (t==.) t=0
	if (t== 0) {
		dropped = dropped_nodes0 
	}
	else {
		dropped = *dropped_nodes[t]
	}
	res=J(maxnodes, maxnodes, 0)
	for(i=1; i<=length(dropped); i++) {
	    res[|dropped[i],1\dropped[i],maxnodes|] = J(1,maxnodes,.)
		res[|1,dropped[i]\maxnodes, dropped[i]|] = J(maxnodes,1,.)
	}
	for(i=1; i<=maxnodes; i++) {
	    if(t==0) {
		    cols = *adjlist0[i]
		}
		else {
		    cols = *adjlist[i,t]
		}
		for(j=1; j<=length(cols);j++){
		    res[i,cols[j]] = weight(i,cols[j],t)
		}
	}
	return(res)
}

real matrix abm_nw::export_edgelist(| real scalar t, string scalar ego_all)
{
    real matrix res, temp
	real vector orig, cols, all_nodes, sel
	real scalar i, j, k
	
	is_valid_time(t)
	if (t==.) t=0
	if (t==0) {
		orig = nodes0
	}
	else {
		orig = *nodes[t]
	}
	res = J(0,3,.)
	for(i=1; i<= length(orig); i++) {
		cols = neighbours(orig[i], t)
		temp = J(length(cols),3, .)
		for(j=1; j<=length(cols); j++) {
			temp[j,.] = (orig[i],cols[j],weight(orig[i],cols[j],t))
		}
		res = res \ temp
	}
	if (directed == 0) {
	    res = select(res,res[.,1]:<res[.,2])
	}
	if (args() == 2) {
		all_nodes = schedule(t)
		sel=J(1,cols(all_nodes),1)
		for(i=1; i <= rows(res); i++) {
			k = selectindex(all_nodes:==res[i,1])
			if(cols(k) ==1){
				sel[k] = 0
			}
		}
		temp = select(all_nodes,sel)'
		temp = temp, J(rows(temp),2,.)
		res = res \ temp
	}

	
	_sort(res,(1,2))
	return(res)
}

real scalar abm_nw::weight(real scalar ego, real scalar alter, | real scalar t, string scalar fast)
{
	if (args() < 4) {
		is_valid_id(ego)
		is_valid_id(alter)
	}
	if (t==.) t=0
	if (weighted) {
		return(network.get((t,ego,alter)))
	}
	else {
		if(t==0) {
			return(anyof(*adjlist0[ego], alter))
		}
		else{
			return(anyof(*adjlist[ego,t], alter))
		}
	}
}

real vector abm_nw::schedule(| real scalar t)
{
	real vector res
	
    if(args()==0) t = 0
	is_valid_time(t)
	if (t==0) {
		res = nodes0
	}
	else {
	    res = *nodes[t]
	}
	if (randomit) res = jumble(res')'
	return(res)
}

void abm_nw::clear()
{
	real scalar i,j

	adjlist0 = J(N_nodes0,1,NULL)
	for(i=1; i<=N_nodes0; i++) {
		adjlist0[i] = &(J(1,0,.))
	}
	nodes0 = 1..N_nodes0
	maxnodes = N_nodes0

	dropped_nodes0 = J(1,0,.)
	
	if (tdim > 0 & tdim < .) {
		adjlist = J(maxnodes, tdim, NULL)
		nodes = J(tdim,1,NULL)
		N_nodes = J(tdim,1,.)
		N_edges = J(tdim,1,0)
		dropped_nodes = J(tdim,1,NULL)
		frozen = J(tdim,1,0)
		for(i=1;i<=tdim; i++) {
			dropped_nodes[i] = &(J(1,0,.))
		}
		
		for(i=1; i<= maxnodes; i++) {
			for(j=1; j<=tdim; j++) {
				adjlist[i,j] = &(J(1,0,.))
			}
		}
	}
	if (weighted==1) network.clear()
	nw_set    = 0
	setup     = 0

}

end

