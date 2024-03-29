##EDA
dat_1 = read.csv('./../data/3ST.csv')
dat_2 = read.csv('./../data/4ST.csv')
dat_3 = read.csv('./../data/4SG.csv')

# May the Lightning effect the PDO, B+C or K+A numbers (taking PDO as example)?

## For each typle of intersection, group by lightning.
threest_lig = dat_1[dat_1$LIGHTING==1,]
threest_nolig = dat_1[dat_1$LIGHTING==0,]

data_comb <- data.frame(
  value = c(data.matrix(threest_lig$PDO),data.matrix(threest_nolig$PDO)), 
  label = c(rep('With lightning',length(data.matrix(threest_lig$PDO))),
            rep('Without lightning',length(data.matrix(threest_nolig$PDO))))
)
library(ggplot2)
library(plyr)
mu <- ddply(data_comb, "label", summarise, grp.mean=mean(value))
p_over <- ggplot(data_comb, aes(x=value, color=label, fill=label)) + geom_histogram(
  fill="white", alpha=0.2, position="identity")+ 
  labs(title="Overlaid plot of each 3ST cross",x="PDO number", y = "Count")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=label),linetype="dashed")
p_over


## I dont think this comparison make sense as the total numbers are different.
## So one possible solution is to randomly choose cases in group with large sample size.
threest_lig = threest_lig[sample(nrow(threest_lig), nrow(threest_nolig)), ]
data_comb <- data.frame(
  value = c(data.matrix(threest_lig$PDO),data.matrix(threest_nolig$PDO)), 
  label = c(rep('With lightning',length(data.matrix(threest_lig$PDO))),
            rep('Without lightning',length(data.matrix(threest_nolig$PDO))))
)
library(ggplot2)
library(plyr)
mu <- ddply(data_comb, "label", summarise, grp.mean=mean(value))
p_over <- ggplot(data_comb, aes(x=value, color=label, fill=label)) + geom_histogram(
  fill="white", alpha=0.2, position="identity")+ 
  labs(title="Overlaid plot of 3ST group",x="PDO number", y = "Count")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=label),linetype="dashed")
p_over
## Then the comparison seems to be more rational. Then for the rest two types of crosses, we have:

fourst_lig = dat_2[dat_2$LIGHTING==1,]
fourst_nolig = dat_2[dat_2$LIGHTING==0,]
fourst_lig = fourst_lig[sample(nrow(fourst_lig), nrow(fourst_nolig)), ]
data_comb <- data.frame(
  value = c(data.matrix(fourst_lig$PDO),data.matrix(fourst_nolig$PDO)), 
  label = c(rep('With lightning',length(data.matrix(fourst_lig$PDO))),
            rep('Without lightning',length(data.matrix(fourst_nolig$PDO))))
)
library(ggplot2)
library(plyr)
mu <- ddply(data_comb, "label", summarise, grp.mean=mean(value))
p_over <- ggplot(data_comb, aes(x=value, color=label, fill=label)) + geom_histogram(
  fill="white", alpha=0.2, position="identity")+ 
  labs(title="Overlaid plot of 4ST group",x="PDO number", y = "Count")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=label),linetype="dashed")
p_over

foursg_lig = dat_3[dat_3$LIGHTING==1,]
foursg_nolig = dat_3[dat_3$LIGHTING==0,]
foursg_lig = foursg_lig[sample(nrow(foursg_lig), nrow(foursg_nolig)), ]
data_comb <- data.frame(
  value = c(data.matrix(foursg_lig$PDO),data.matrix(foursg_nolig$PDO)), 
  label = c(rep('With lightning',length(data.matrix(foursg_lig$PDO))),
            rep('Without lightning',length(data.matrix(foursg_nolig$PDO))))
)
library(ggplot2)
library(plyr)
mu <- ddply(data_comb, "label", summarise, grp.mean=mean(value))
p_over <- ggplot(data_comb, aes(x=value, color=label, fill=label)) + geom_histogram(
  fill="white", alpha=0.2, position="identity")+ 
  labs(title="Overlaid plot of 4SG group",x="PDO number", y = "Count")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=label),linetype="dashed")
p_over

# May the cross type influence the PDO, B+C or K+A numbers (taking PDO as example)?

data_root = "./../data"
data_files = list.files(data_root)
pd_output = c()
for (i in data_files){
  PD = read.csv(paste(data_root,'/',i,sep=''))
  if (startsWith(i,'3ST')){
    type_ctl = 0
  } else if (startsWith(i,'4SG')){
    type_ctl = 1
  } else {
    type_ctl = 2
  }
  TYPE = rep(type_ctl,nrow(PD)) 
  PD_type = cbind(PD, TYPE)
  if (is.null(pd_output)){
    pd_output = PD_type
  } else{
    pd_output = rbind(pd_output,PD_type)
  }
}

pd_3st = pd_output[pd_output$TYPE==0,]
pd_4sg = pd_output[pd_output$TYPE==1,]
pd_4st = pd_output[pd_output$TYPE==2,]

data_comb <- data.frame(
  value = c(data.matrix(pd_3st$PDO),data.matrix(pd_4st$PDO),data.matrix(pd_4sg$PDO)), 
  label = c(rep('3ST',length(data.matrix(pd_3st$PDO))),
            rep('4ST',length(data.matrix(pd_4st$PDO))),
            rep('4SG',length(data.matrix(pd_4sg$PDO))))
)
library(ggplot2)
library(plyr)
mu <- ddply(data_comb, "label", summarise, grp.mean=mean(value))
p_over <- ggplot(data_comb, aes(x=value, color=label, fill=label)) + geom_histogram(
  fill="white", alpha=0.2, position="identity")+ 
  labs(title="Overlaid plot of PDO number for different crosses",x="PDO number", y = "Count")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=label),linetype="dashed")
p_over
