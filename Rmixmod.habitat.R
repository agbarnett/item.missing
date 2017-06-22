# Rmixmod.habitat.R
# using Rmixmod to find latent classes in Habitat missing data
# Feb 2017
library(Rmixmod)
library(doBy)

# load data of pure zeros and ones and information on question order
load('Missing.habitat.data.RData')  

# convert missing zero/one to factors
mm = missing.matrix
mm[mm==0] = '0'
mm[mm==1] = '1'
to.use = as.data.frame(mm)
### Run model
# run up to 20 clusters (this takes a while)
max.clust = 20
mres = mixmodCluster(to.use, nbCluster=2:max.clust, model=mixmodMultinomialModel(listModels=c('Binary_pk_Ekj')), criterion='BIC')

### BIC
# get BIC results, flag if any groups under 1%
BIC = NULL
for (i in 1:(max.clust-1)){
  tab = table(mres@results[[i]]@partition)
  less = any(100*tab/sum(tab) < 1) # any groups with less than 1% (too small)
  frame = data.frame(i=i, any.less=less, n.cluster=mres@results[[i]]@nbCluster, criterion=mres@results[[i]]@criterion, criterionV=mres@results[[i]]@criterionValue)
  BIC = rbind(BIC, frame)
}
bic.plot = ggplot(data=BIC, aes(x=n.cluster, y=criterionV))+
  geom_line()+
  geom_point(size=6, data=BIC, aes(pch=any.less))+
  scale_shape_manual('Any groups sizes\nunder 1%', values=c(15,19), labels=c('No','Yes'))+
  scale_x_continuous(breaks=2:20)+
  xlab('Maximum number of latent groups')+
  ylab('BIC')+
  theme_bw()+
  theme(legend.position=c(0.8, 0.8), text=element_text(size=20), panel.grid.minor = element_blank())
bic.plot 

### Plot estimated probabilities by group
# examine groups for smallest BIC
bindex = which(BIC$criterionV == min(BIC$criterionV))
best = mres@results[[bindex]] # 
props = best@parameters@proportions # proportions in each group
n.cluster = best@nbCluster
preds = best@partition # predicted group for each person
# arrange into plot
to.plot = NULL
for (k in 1:n.cluster){
  r = best@parameters@scatter[[k]][,1]
  frame = data.frame(cluster=k, i=1:length(r), prob=r)
  to.plot = rbind(to.plot, frame)
}
# merge with question order
to.plot = merge(to.plot, qorder, by='i')
# numbers per group
n.group = as.numeric(table(best@partition))
cmeans = summaryBy(prob ~ cluster, data=to.plot)
cmeans$n.group = n.group
cmeans$percent = round(100*cmeans$n.group / sum(cmeans$n.group))
cmeans = cmeans[order(cmeans$prob.mean),] # arrange smallest to largest
cmeans$clustero = 1:nrow(cmeans)
cmeans

# re-order latent groups by missing probability
to.ploto = merge(to.plot, cmeans, by='cluster') # add new label
labels = paste(1:n.cluster, ' (', cmeans$n.group, ')', sep='') # use numbers
to.ploto$flabel = factor(to.ploto$clustero, levels=1:n.cluster, labels=labels)
# plot
gplot = ggplot(data=to.ploto, aes(x=order, y=prob))+
  geom_point()+
  xlab('Question order')+
  ylab('Probability of missing')+
  scale_x_continuous(limits=c(1, ncol(mm)), breaks=c(1,50,100,150,200,250))+
  theme_bw()+
  theme(legend.position='none', text=element_text(size=15), panel.grid.minor = element_blank())+
  facet_wrap(~flabel, as.table=T)
gplot

