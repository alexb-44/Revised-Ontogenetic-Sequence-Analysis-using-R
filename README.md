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


# Known areas for improvement
## You can read more in-depth elaborations on these issues in the Algorithm fixes document
* Better/additional methods for handing missing data: Instead of permuting incomplete semaphoronts (those with NA character states) into a least and most mature variant, mimicking the functionality of OSA, it is probably better to use all other semaphoronts in the working matrix to calculate the best permutation(s) using summed pairwise distances. There may be other ways to handle missing data that can be added as options for the user in the future. 
* Removing the need to binarize the input data: Binarization of the input dataset is currently implemented in order to simplify calculations and potentially track developmental events. However, there is no perfect way to faithfully binarize multistate characters and the above fix to the problem of missing data will require normal multistate character data in order to avoid assigning impossible character state combinations to incomplete semaphoronts prior to analysis. 
* Fix to issue with clean_up() subfunction: The job of this subfunction is to remove edges that do not participate in any of the most parsimonious graphs following the nearest_neighbors() step. However, it currently only identifies singleton redundant edges and fails to find pairs of such edges. A better algorithm for capturing both singleton and paired redundant edges is outlined in the Algorithm fixes doc. 
* Incorporating multiple target phenotypes when analyzing polymorphic populations: There is currently no way to assign >1 target semaphoronts in ROSA. This can be achieved with a simple fix to the nearest_neighbors() subfunction. The fix is outlined in the Algorithm fixes doc, but briefly, it would involve pruning edges extending from vertices designated by the user as alternative adult semaphoronts. This effectively ensures that target vertices act strictly as endpoints in the final graph(s). 
* Decomposing the output graph into all most parsimonious subgraphs: Currently, ROSA does not output the set of all most parsimonious ontogenetic sequence graphs. Instead, it outputs something like the graph formed by superimposing all most parsimonious graphs. Since we are interested in the fewest ontogenetic sequences necessary to explain all observed semaphoronts, the final step in the main analysis should be the decomposition of the graph currently output into all most parsimonious subgraphs. Though it may run into computational limits, an algorithm for doing this decomposition is explained in the Algorithm fixes doc. What’s more, this decomposition algorithm could be expanded to take over other steps in ROSA in the future, provided those computational limits don’t prove an impenetrable obstacle. 
