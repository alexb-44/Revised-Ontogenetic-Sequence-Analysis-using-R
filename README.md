# Revised Ontogenetic Sequence Analysis (ROSA)
## ROSA, written in R, reconstructs sequences of developmental changes, providing parsimony-based estimates of developmental variation within populations

ROSA is a bioinformatics tool for determining the relative order in which phenotypic transformations occur during development. 
It takes a matrix of character states observed across sampled conspecific individuals, analogous to the evolutionary character matrices used in phylogenetic analyses, 
and outputs reconstructed ontogenies consisting of snapshots of phenotypic development (called “semaphoronts”) separated by developmental events. 
It is hoped that ROSA will be useful to anyone interested in developmental biology and intraspecific variation, including both neontologists and paleontologists. 


ROSA is inspired by Ontogenetic Sequence Analysis (OSA) (Colbert, 1999; Colbert and Rowe, 2008), which repurposes cladistic parsimony analysis for reconstructing developmental sequences. 
In ROSA, the principle of parsimony is applied in reconstructing the fewest necessary complete sequences such that all observed semaphoronts are involved in at least one 
sequence and all sequences proceed from the least mature semaphoront to the most mature semaphoront. The output of ROSA is an “ontogenetic/developmental sequence graph/network” 
describing the ontogeny best represented by the sampled population as well as all alternative ontogenies reconstructed under this concept of parsimony. 
