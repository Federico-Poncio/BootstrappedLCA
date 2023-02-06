# BootstrappedLCA
Bootstrap Latent Class Analysis with observation weights in R (for four classes)

The Sorter function is the key to this. In this case I'm using the values of certain questions to sort the randomly assigned class numbers into their respective order, so successive runs could be comparable. For other datasets, the sorter function will have to be modified to sort the classess found in it. 

The dataset consists of ten dependent variables for which we want to perfom LCA, starting with "Y", and five explanatory variables to help distinguish the classess. The Ys are likert type data while the Xs are likert and categorical. 

The extension to more than four classess is simple and analogous to the code presented here, as well as the reduction to less. There is no significant change in procedure with the number of classes.
