# abm_nw: A Mata class for managing a network for Agent Based Models

## Description

abm_nw is a [Mata](https://www.stata.com/features/overview/introduction-to-mata/)
class intended to help manage a network for Agent Based models. An Agent Based Model 
is a simulation in which agents, that each follow simple rules, interact with one 
another and thus produce a often surprising outcome at the macro level. The purpose 
of an ABM is to explore mechanisms through which actions of the individual agents 
add up to a macro outcome, by varying the rules that agents have to follow or 
varying with whom the agent can interact (i.e. varying the network). 

Which agents can interact with one another is thus an important part of an ABM.  The 
abm_nw class contains a set of functions that allows one to set up such a network, 
find neighbourhoods of node, and change the network.


## Requirements and use

This requies [Stata](https://www.stata.com) version 15. The class itself is 
defined in the abm_nw.mata file. The class can be imported and used in an Agent
Based Model by adding the line `do abm_nw.mata` to the .do file that defines the
ABM. There is a complete example in the help files, start with `help abm_nw`.
The file cert.do is the certification script, it documents for what potential 
bugs I have tested the code.
