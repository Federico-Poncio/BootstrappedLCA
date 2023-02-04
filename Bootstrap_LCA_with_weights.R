# Load libraries
library(poLCA)
library(ggplot2)

# Load dataset
dataset <- read.csv("dataset_LCA.csv", row.names=1)
# CAMBIAR DATASET POR OTRO DUMMY

#####
# Define useful stuff
dat.variables <- dataset[,1:15] #subset of dataset with variables of interest
dat.weights <- dataset$weights  #vector of observation weights
Formula <- as.formula(cbind(Y01,Y02,Y03,Y04,Y05,Y06,Y07,Y08,Y09,Y10)~ 
                        X1+X2+X3+X4+X5) # define formula for LCA

#####
# Initialize first LCA
sampled_rows <- sample(1:NROW(dat.variables), 
                       size = 600, 
                       prob = dataset$weights, 
                       replace = T) # sample rows with weights and replacement

sampled.df <- dat.variables[sampled_rows,] # keep sampled rows

lca.polca4 <- poLCA(Formula,nclass=4, data=sampled.df, 
                    nrep=1,na.rm=F,
                    graphs=T, 
                    maxiter = 10000,
                    verbose = F) # run LCA with one rep, graph T for analyzing
#####
# Define sorting function

sorter <- function(MODEL){
  ord2 <-MODEL$probs$Y02[,1]+MODEL$probs$Y03[,1]-
    (MODEL$probs$Y02[,5]+MODEL$probs$Y03[,5])
  
  mx <- unname(which(ord2==max(ord2)))
  mn <- unname(which(ord2==min(ord2)))
  
  ord3 <-MODEL$probs$Y08[,1]+MODEL$probs$Y09[,1]-
    (MODEL$probs$Y08[,5]+MODEL$probs$Y09[,5])
  left <- ord3[-c(mx,mn)]
  
  left <- ord3[-c(mx,mn)]
  left.mn <- unname(which(left ==min(left)))
  left.mx <- unname(which(left ==max(left)))
  
  mn2 <- unname(which(ord3 == left[left.mn]))
  mx2 <- unname(which(ord3 == left[left.mx]))
  
  return(c(mx, mx2 , mn2 , mn))
}

#####
# Sort LCA according to class probabilities and run again
probs.init <- poLCA.reorder(lca.polca4$probs,
                            sorter(lca.polca4))

lca.polca4 <- poLCA(Formula,nclass=4, data=sampled.df, nrep=1,
                    na.rm=F,graphs=T, maxiter = 10000,verbose = F,
                    probs.start = probs.init)

#####
# initialize counters
dataset$counter <- 0

dataset$posterior1<-0
dataset$posterior2<-0
dataset$posterior3<-0
dataset$posterior4<-0
post <- c("posterior1", "posterior2", "posterior3", "posterior4")
dataset[sampled_rows, post]<-as.data.frame(lca.polca4$posterior)

dataset$posterior1sq<-0
dataset$posterior2sq<-0
dataset$posterior3sq<-0
dataset$posterior4sq<-0
postsq <- c("posterior1sq", "posterior2sq", "posterior3sq", "posterior4sq")
dataset[sampled_rows, postsq]<-dataset[sampled_rows, post]**2

for(row in sampled_rows){
  dataset$counter[row] <- dataset$counter[row] +1 
}

dataset$counter[sampled_rows]<-
  rowSums(cbind(dataset[sampled_rows,"counter"],1), na.rm=T)

BIC <- c(lca.polca4$bic)

coeff <- lca.polca4$coeff # coeficientes de regresión
coeff.sq <- coeff**2
probs <- lca.polca4$probs # probabilidades de clase/pregunta
probs.sq <- probs
for(k in 1:length(probs)){
  probs.sq[[k]] <- probs[[k]]**2
}

#####
# For loop

grphs <- rep(c(rep(FALSE,30),TRUE),10) #boolean vector to plot BICs
nreps = 1#300 # repetitions of the for loop

for(i in 1:nreps){
  #sampling
  sampled_rows <- sample(1:NROW(dat.variables), size = 600, 
                         prob = dataset$weights, replace = T)
  #subsetting
  sampled.df <- dat.variables[sampled_rows,]
  if(grphs[i]){plot(BIC)}
  #model:
  lca.polca4 <- poLCA(Formula,nclass=4, data=sampled.df, nrep=1,
                      na.rm=F,graphs=grphs[i], maxiter = 10000,verbose = F)
  probs.init2 <- poLCA.reorder(lca.polca4$probs,
                               sorter(lca.polca4))
  lca.polca4 <- poLCA(Formula,nclass=4, data=sampled.df, nrep=1,
                      na.rm=F,graphs=grphs[i], maxiter = 10000,verbose = F,
                      probs.start = probs.init2)
  # regression coefficients
  coeff <- coeff + lca.polca4$coeff
  coeff.sq <- coeff.sq + coeff**2
  
  #posteriors by row
  for(row in 1:length(sampled_rows)){
    dataset[sampled_rows[row], post] <- 
      dataset[sampled_rows[row], post]+lca.polca4$posterior[row,1:4]
    dataset[sampled_rows[row], postsq] <- 
      dataset[sampled_rows[row], postsq]+lca.polca4$posterior[row,1:4]**2
    dataset$counter[sampled_rows[row]] <- 
      dataset$counter[sampled_rows[row]]+1
  }
  
  #prob by question and class
  Probs <- lca.polca4$probs
  for(k in 1:length(probs)){
    probs[[k]] <- Probs[[k]] + probs[[k]]
  }
  for(k in 1:length(probs)){
    probs.sq[[k]] <- probs.sq[[k]] + Probs[[k]]**2
  }
  #BIC of observations
  BIC <- c(BIC,lca.polca4$bic)
  
  ("rep:")
  print(i)
}

#####
# Compute results and save

coeff.mean <- coeff/(nreps+1)
coeff.sd <- sqrt(coeff.sq/nreps-(coeff/(nreps+1))**2)

posterior.mean<- dataset[, post]/dataset$counter
posterior.sd <- sqrt(dataset[, postsq]/(dataset$counter)-
                       (dataset[, post]/(dataset$counter))**2)

probs.mean <- probs
for(k in 1:length(probs)){
  probs.mean[[k]] <- probs[[k]]/(nreps+1)
}
probs.sd <- probs
for(k in 1:length(probs)){
  probs.sd[[k]] <- sqrt(probs.sq[[k]]/nreps-(probs[[k]]/(nreps+1))**2)
}

confidences <- c()
for (row in 1:NROW(posterior.mean)){
  srt <- sort(as.matrix(posterior.mean[row,]),decreasing = T)
  confidences <- c(confidences,
                   srt[1]-srt[2])
}

write.csv(coeff.mean,"coeff_mean_boots.csv")
write.csv(coeff.sd,"coeff_sd_boots.csv")
write.csv(posterior.mean,"posterior_mean_boots.csv")
write.csv(posterior.sd,"posterior_sd_boots.csv")
save(probs.mean, file = "probs_mean_boots.RData")
save(probs.sd, file = "probs_sd_boots.RData")
write.csv(confidences,"confidences_assignment_boots.csv")


hist(confidences)

boxplot(BIC)

posterior.mean$MAX <- names(posterior.mean)[max.col(abs(posterior.mean))]
posterior.mean$confidences <- confidences

ggplot(posterior.mean,aes(x=confidences))+
  geom_histogram()+facet_grid(~MAX)+theme_bw()


#####
# Plot probabilities

#load(file = "probs_mean_boots.RData")
lcmodel <- reshape2::melt(probs.mean, level=2)

zp1 <- ggplot(lcmodel,aes(x = value, y = L2, fill = forcats::fct_rev(Var2)))
zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
zp1 <- zp1 + facet_grid(cols=vars(Var1)) 
zp1 <- zp1 + scale_fill_brewer(type="seq", palette="RdBu") +theme_bw()
zp1 <- zp1 + labs(x = "Questions",
                  y="Expected probability",
                  fill = "")
zp1 <- zp1 + guides(fill = guide_legend(reverse=TRUE, nrow=3,
                                        byrow=TRUE))
zp1 <- zp1 + theme(legend.position="bottom",
                   axis.text.x=element_blank(),
                   legend.text = element_text(size = 8))+
  scale_y_discrete(position = "left", limits=rev) #reordenar el y
print(zp1)
