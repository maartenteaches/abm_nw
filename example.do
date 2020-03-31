    cd "c:\mijn documenten\projecten\stata\abm\abm_nw" 
    
    set seed 12345678
    clear all
	set rmsg on
    
    // ----------------------------------- class definitions
    // import abm_nw class definition
    run abm_nw.mata
    
    mata:
    mata set matastrict on
    
    // the main agent based model
    class spread 
    {
        class abm_nw          scalar   nw
        real                  matrix   infected
        real                  scalar   degree
        real                  scalar   pr
        real                  scalar   N
        real                  scalar   tdim
        
        transmorphic                   degree()
        transmorphic                   pr()
        transmorphic                   N()
        transmorphic                   tdim()
          
        void                           setup()
        void                           run()
        void                           step()
    
        void                           toStata()
    }
    
    // ------------ -----------------------setup the network
    transmorphic spread::N(| real scalar val)
    {
        if (args()==1) {
            nw.N_nodes(0,val)
            N = val
        }
        else {
            return(N)
        }
    }
    
    transmorphic spread::tdim(| real scalar val)
    {
        if (args()==1) {
            tdim = val
        }
        else {
            return(tdim)
        }
    }
    
    transmorphic spread::degree(| real scalar val) 
    {
        if (args()==1) {
            degree = val
        }
        else {
            return(degree)
        }
    }
    
    transmorphic spread::pr(| real scalar val)
    {
        if (args() == 1){
            pr = val
        }
        else {
            return(pr)
        }
    }
    
    void spread::setup()
    {
        if (tdim==.) tdim = 10
        if (N==.) {
            N=30
            nw.N_nodes(0,30)
        }
        if (degree==.) degree = 4
        if (pr==.) pr=.1
        
        nw.directed(0)
        nw.weighted(0)
        nw.tdim(0)
        nw.sw(degree, pr)
        nw.setup()
        
        infected = J(N,tdim,0)
        infected[1,1] = 1
    }
    
    // --------------------------------------- run the model
    void spread::step(real scalar t)
    {
        real scalar i, j
        real vector neigh
        
        infected[.,t+1] = infected[.,t]
    
        for(i=1; i <= N; i++) {
            if(infected[i,t]==1) {  
                neigh = nw.neighbours(i)
                for(j=1; j<= length(neigh); j++) {
                    infected[neigh[j],t+1] = 1
                }
            }
        }   
    }
    
    void spread::run()
    {
        real scalar i
        setup()
        for(i=1; i<tdim; i++) {
            step(i)
        }
    }
    
    // ------------------------- export the results to Stata
    void spread::toStata()
    {
        real vector xid
        real matrix result
        real scalar i, k, n
        string vector varnames
        
        // collect what we want to export in a matrix
        result = nw.export_edgelist(0, "ego_all") 
        result = result, J(rows(result),tdim,.)
        for (i=1; i<=rows(result);i++) {
            result[|i,4 \ i, .|] = infected[result[i,1],.]
        }
    
        // move matrix to Stata
        k = cols(result)
        varnames = J(1,k,"")
        varnames[1..3] = "ego", "alter","weight"
        for(i = 4 ; i <= k; i++) {
            varnames[i] = "inf" + strofreal(i-3)
        }
        xid = st_addvar("long", varnames)
        n = st_nobs()
        n = rows(result) - n
        if (n > 0) {
            st_addobs(n)
        }
        st_store(.,xid, result)
        st_local("n", strofreal(N))
    }
    
    
    end
    
    // --------------------------------------- run the model
    mata:
    model = spread()
    model.N(40)
    model.degree(4)
    model.pr(.1)
    model.tdim(10)
    model.run()
    model.toStata()
    end
    
    // A nice display of this type of network can be dreated
    // by putting the nodes in order on a circle 
    // remember your trigonometry? (if not, Wikipedia helps)
    gen x_ego = cos((ego-1)/`n'*2*_pi)*(1+mod(ego,2)*.2)
    gen y_ego = sin((ego-1)/`n'*2*_pi)*(1+mod(ego,2)*.2)
    gen x_alter = cos((alter-1)/`n'*2*_pi)*(1+mod(alter,2)*.2)
    gen y_alter = sin((alter-1)/`n'*2*_pi)*(1+mod(alter,2)*.2)
    
    // each node can appear multiple times in the data
    // once for each edge, 
    // but we want to display each node only once
    bys ego (alter) : gen first = _n == 1
    
    // the graphs
    forvalues t = 1/10{
        twoway pcspike y_ego x_ego y_alter x_alter,                ///
                      lcolor(gs8) scheme(s1mono) ||                ///
            scatter y_ego x_ego if first & inf`t'==1 ,             ///
                       mcolor(red) msymbol(O) ||                   ///
            scatter y_ego x_ego if first & inf`t'==0,              ///
                       mcolor(black) msymbol(Oh)                   ///
                       aspect(1) xscale(off) yscale(off)           ///
                       legend(order(1 "infected" 2 "susceptible")) ///
                       name(t`t', replace) title(Time `t')      ///   
            
    }