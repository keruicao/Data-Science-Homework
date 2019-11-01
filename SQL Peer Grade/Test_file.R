or = Con_All$Fecoccemp
ui = unique(or)

library(RecordLinkage)

m = length(ui)
cor = matrix(ncol = m,nrow = m)
for(i in 1:m){
  for(j in 1:m){
    cor[i,j] = levenshteinDist(ui[i],ui[j])
  }
}

for(i in 1:m){
  for(j in 1:m){
    cor[i,j] = ifelse(cor[i,j]<4,cor[i,j],0)
  }
}

m=100 # max number of clusters
n=10  # number of repeats experiments for each k value
tot = matrix(nrow = m,ncol  = n)
f = matrix(nrow = m,ncol  = n)
# Calculate the total withingroup errors
for(i in 1:m){
  for(j in 1:n){
    cl = kmeans(x = cor,centers = i,iter.max = 100)
    f[i,j] = cl$betweenss/cl$tot.withinss
    tot[i,j] = cl$tot.withinss
  }
}
# calculate average total within-group error
tot %<>% apply(MARGIN = 1,mean)
f %<>% apply(MARGIN = 1,mean)
# plot errors
tot = data.frame(cbind(tot,seq(1,m,1)))
f = data.frame(cbind(f,seq(1,m,1)))
ggplot(tot) + geom_line(aes(y=tot,x = V2),size = 2) + 
  scale_x_continuous("Number of Cluster",breaks = seq(0,100,5)) + 
  ylab("SSE")





##########################################################

