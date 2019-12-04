##Data import 
dat_1 = read.csv('./../data/3ST.csv')
dat_2 = read.csv('./../data/4ST.csv')
dat_3 = read.csv('./../data/4SG.csv')

### Question 1: Simplest question, do these different types of intersection holds different number of crashes? (Measured by mean or median.)

### About PDO

st3_pdo = dat_1$PDO
st4_pdo = dat_2$PDO
sg4_pdo = dat_3$PDO

### Check same dispersion assumption for Mann Whitney test: only st4 vs sg4 group satisfied

ansari.test(st3_pdo,st4_pdo)
ansari.test(st4_pdo,sg4_pdo)
ansari.test(st3_pdo,sg4_pdo)

### Check st3_pdo with st4_pdo:

library(magrittr)
X <- data.frame(value = data.matrix(st3_pdo), label = "st3") 
Y <- data.frame(value = data.matrix(st4_pdo), label = "st4") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)
W <- data_XY[data_XY$label=='st4',] %>% dplyr::select("rank") %>% sum
n = length(st4_pdo)
m = length(st3_pdo)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(191*(191^2-1)+78*(78^2-1)+61*(61^2-1)+32*(32^2-1)+25*(25^2-1)+17*(17^2-1)+10*(10^2-1)+7*(7^2-1)+9*(9^2-1)+6*(6^2-1)+2*(2^2-1)+3*(3^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### Check by 95% boostrap CI. 95% confident CI does not contain 0 and both bounds are negative
bootci <- function(x,y){
  n1 = length(x)
  n2 = length(y)
  dif = NULL
  for(i in 1:1000){
    x1 = base::sample(x,n1,replace=TRUE)
    x2 = base::sample(y,n2,replace=TRUE)
    dif = c(dif, mean(x1)-mean(x2))
  }
  return(quantile(dif,prob=c(0.025,0.975)))
}
set.seed(1)
bootci(st3_pdo,st4_pdo)

### It returns true, so we think there is difference. 

### Check st4_pdo with sg4_pdo:

X <- data.frame(value = data.matrix(st4_pdo), label = "st4") 
Y <- data.frame(value = data.matrix(sg4_pdo), label = "sg4") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)
W <- data_XY[data_XY$label=='sg4',] %>% dplyr::select("rank") %>% sum
n = length(sg4_pdo)
m = length(st4_pdo)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(36*(36^2-1)+13*(13^2-1)+12*(12^2-1)+16*(16^2-1)+10*(10^2-1)+8*(8^2-1)+13*(13^2-1)+10*(10^2-1)+9*(9^2-1)+2*(2^2-1)+3*(3^2-1)+2*(2^2-1)+5*(5^2-1)+2*3*(3^2-1)+2*(2^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### Also return true, so we think there is difference.

### Check st3_pdo with sg4_pdo:

X <- data.frame(value = data.matrix(st3_pdo), label = "st3") 
Y <- data.frame(value = data.matrix(sg4_pdo), label = "sg4") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)
W <- data_XY[data_XY$label=='sg4',] %>% dplyr::select("rank") %>% sum
n = length(sg4_pdo)
m = length(st3_pdo)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(191*(191^2-1)+75*(75^2-1)+53*(53^2-1)+30*(30^2-1)+27*(27^2-1)+23*(23^2-1)+19*(19^2-1)+2*13*(13^2-1)+9*(9^2-1)+3*2*(2^2-1)+4*(4^2-1)+5*(5^2-1)+6*(6^2-1)+2*3*(3^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### Also return true, so we think there is difference.

### Check by 95% boostrap CI. 95% confident CI does not contain 0 and both bounds are negative
set.seed(1)
bootci(st3_pdo,sg4_pdo)


### next, we use the categorical analysis perspective to verify the independence holds false.

st3_crash = c(nrow(dat_1[dat_1$PDO==0,]),nrow(dat_1[dat_1$PDO>0,]),nrow(dat_1[dat_1$PDO==0,])+nrow(dat_1[dat_1$PDO>0,]))
st4_crash = c(nrow(dat_2[dat_2$PDO==0,]),nrow(dat_2[dat_2$PDO>0,]),nrow(dat_2[dat_2$PDO==0,])+nrow(dat_2[dat_2$PDO>0,]))
sg4_crash = c(nrow(dat_3[dat_3$PDO==0,]),nrow(dat_3[dat_3$PDO>0,]),nrow(dat_3[dat_3$PDO==0,])+nrow(dat_3[dat_3$PDO>0,]))
total = st3_crash + st4_crash + sg4_crash
tbl <- data.frame(st3_crash,st4_crash,sg4_crash,total)
rownames(tbl) <- c('Safe',"Crash","total")
tbl
Q = 0
for (i in 1:2){
  for (j in 1:3){
    Q = Q + (tbl[i,j]-(tbl[i,4]*tbl[3,j]/tbl[3,4]))^2/(tbl[i,4]*tbl[3,j]/tbl[3,4])
  }
}
Q >= qchisq(0.95,df=(2*1))

### We get the same result: intersection type and DPO crash number are relative.

### About KA

st3_ka = dat_1$KA
st4_ka = dat_2$KA
sg4_ka = dat_3$KA

### Check same dispersion assumption for Mann Whitney test

ansari.test(st3_ka,st4_ka)
ansari.test(st4_ka,sg4_ka)
ansari.test(st3_ka,sg4_ka)

### Check st3_ka with st4_ka:

X <- data.frame(value = data.matrix(st3_ka), label = "st3") 
Y <- data.frame(value = data.matrix(st4_ka), label = "st4") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)
W <- data_XY[data_XY$label=='st4',] %>% dplyr::select("rank") %>% sum
n = length(st4_ka)
m = length(st3_ka)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(425*(425^2-1)+19*(19^2-1)+2*(2^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### It returns true, so we think there is difference.

### Check by 95% boostrap CI. 95% confident CI does contain
set.seed(1)
bootci(st3_ka,st4_ka)

### Check st4_ka with sg4_ka:

X <- data.frame(value = data.matrix(st4_ka), label = "st4") 
Y <- data.frame(value = data.matrix(sg4_ka), label = "sg4") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)
W <- data_XY[data_XY$label=='sg4',] %>% dplyr::select("rank") %>% sum
n = length(sg4_ka)
m = length(st4_ka)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(138*(138^2-1)+22*(22^2-1)+3*(3^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### Then, this time, we found that the st4 and sg4 doesn't pose a strong effect on the ka crash number.

### Check by 95% boostrap CI. 95% confident CI does contain 0.
set.seed(1)
bootci(st4_ka,sg4_ka)

### Check st3_pdo with sg4_pdo:

X <- data.frame(value = data.matrix(st3_ka), label = "st3") 
Y <- data.frame(value = data.matrix(sg4_ka), label = "sg4") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)
W <- data_XY[data_XY$label=='sg4',] %>% dplyr::select("rank") %>% sum
n = length(sg4_ka)
m = length(st3_ka)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(453*(453^2-1)+31*(31^2-1)+3*(3^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### Returns true, so we think there is difference.

### Check by 95% boostrap CI. 95% confident CI does not contain 0.
set.seed(1)
bootci(st3_ka,sg4_ka)

### next, we use the categorical analysis perspective to verify the independence holds false.

st3_crash = c(nrow(dat_1[dat_1$KA==0,]),nrow(dat_1[dat_1$KA>0,]),nrow(dat_1[dat_1$KA==0,])+nrow(dat_1[dat_1$KA>0,]))
st4_crash = c(nrow(dat_2[dat_2$KA==0,]),nrow(dat_2[dat_2$KA>0,]),nrow(dat_2[dat_2$KA==0,])+nrow(dat_2[dat_2$KA>0,]))
sg4_crash = c(nrow(dat_3[dat_3$KA==0,]),nrow(dat_3[dat_3$KA>0,]),nrow(dat_3[dat_3$KA==0,])+nrow(dat_3[dat_3$KA>0,]))
total = st3_crash + st4_crash + sg4_crash
tbl <- data.frame(st3_crash,st4_crash,sg4_crash,total)
rownames(tbl) <- c('Safe',"Crash","total")
tbl
Q = 0
for (i in 1:2){
  for (j in 1:3){
    Q = Q + (tbl[i,j]-(tbl[i,4]*tbl[3,j]/tbl[3,4]))^2/(tbl[i,4]*tbl[3,j]/tbl[3,4])
  }
}
Q >= qchisq(0.95,df=(2*1))


### About BC


st3_bc = dat_1$BC
st4_bc = dat_2$BC
sg4_bc = dat_3$BC

### Check same dispersion assumption:
ansari.test(st3_bc,st4_bc)
ansari.test(st4_bc,sg4_bc)
ansari.test(st3_bc,sg4_bc)

### Check st3_bc with st4_bc:

X <- data.frame(value = data.matrix(st3_bc), label = "st3") 
Y <- data.frame(value = data.matrix(st4_bc), label = "st4") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)
W <- data_XY[data_XY$label=='st4',] %>% dplyr::select("rank") %>% sum
n = length(st4_bc)
m = length(st3_bc)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(295*(295^2-1)+78*(78^2-1)+34*(34^2-1)+23*(23^2-1)+6*(6^2-1)+4*(4^2-1)+3*(3^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### It returns true, so we think there is difference.

### Check by 95% boostrap CI. 95% confident CI does not contain 0.
set.seed(1)
bootci(st3_bc,st4_bc)

### Check st4_bc with sg4_bc:

X <- data.frame(value = data.matrix(st4_bc), label = "st4") 
Y <- data.frame(value = data.matrix(sg4_bc), label = "sg4") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)
W <- data_XY[data_XY$label=='sg4',] %>% dplyr::select("rank") %>% sum
n = length(sg4_bc)
m = length(st4_bc)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(61*(61^2-1)+33*(33^2-1)+20*(20^2-1)+14*(14^2-1)+11*(11^2-1)+10*(10^2-1)+5*(5^2-1)+4*(4^2-1)+3*(3^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### Also return true, so we think there is difference.
### Check by 95% boostrap CI. 95% confident CI does not contain 0.
set.seed(1)
bootci(st4_bc,sg4_bc)

### Check st3_bc with sg4_bc:

X <- data.frame(value = data.matrix(st3_bc), label = "st3") 
Y <- data.frame(value = data.matrix(sg4_bc), label = "sg4") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)
W <- data_XY[data_XY$label=='sg4',] %>% dplyr::select("rank") %>% sum
n = length(sg4_bc)
m = length(st3_bc)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(297*(297^2-1)+77*(77^2-1)+44*(44^2-1)+25*(25^2-1)+15*(15^2-1)+14*(14^2-1)+5*(5^2-1)+7*(7^2-1)+2*2*(2^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### Returns true, so we think there is difference.

### Check by 95% boostrap CI. 95% confident CI does not contain 0.
set.seed(1)
bootci(st3_bc,sg4_bc)


### next, we use the categorical analysis perspective to verify the independence holds false.

st3_crash = c(nrow(dat_1[dat_1$BC==0,]),nrow(dat_1[dat_1$BC>0,]),nrow(dat_1[dat_1$BC==0,])+nrow(dat_1[dat_1$BC>0,]))
st4_crash = c(nrow(dat_2[dat_2$BC==0,]),nrow(dat_2[dat_2$BC>0,]),nrow(dat_2[dat_2$BC==0,])+nrow(dat_2[dat_2$BC>0,]))
sg4_crash = c(nrow(dat_3[dat_3$BC==0,]),nrow(dat_3[dat_3$BC>0,]),nrow(dat_3[dat_3$BC==0,])+nrow(dat_3[dat_3$BC>0,]))
total = st3_crash + st4_crash + sg4_crash
tbl <- data.frame(st3_crash,st4_crash,sg4_crash,total)
rownames(tbl) <- c('Safe',"Crash","total")
tbl
Q = 0
for (i in 1:2){
  for (j in 1:3){
    Q = Q + (tbl[i,j]-(tbl[i,4]*tbl[3,j]/tbl[3,4]))^2/(tbl[i,4]*tbl[3,j]/tbl[3,4])
  }
}
Q >= qchisq(0.95,df=(2*1))
###########################################################################################################################################################################################

### Question 2: We have already known the crossing type affects the crash number, so we now explore whether cross type + another property may effects the crash number.

## Cross type + discrete skewed angle 
############################################################################################################################################################################################################

## Firstly, we make skewed angle discrete:

dat_1$SKEW_ANGLE = cut(dat_1$SKEW_ANGLE, breaks=c(-1,31,61,91), labels=c(1,2,3))
dat_2$SKEW_ANGLE = cut(dat_2$SKEW_ANGLE, breaks=c(-1,31,61,91), labels=c(1,2,3))
dat_3$SKEW_ANGLE = cut(dat_3$SKEW_ANGLE, breaks=c(-1,31,61,91), labels=c(1,2,3))

## Forming the table (PDO):

st3_1_crash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$SKEW_ANGLE==1),]),
                nrow(dat_1[(dat_1$PDO>0) & (dat_1$SKEW_ANGLE==1),]), 
                nrow(dat_1[(dat_1$PDO==0) & (dat_1$SKEW_ANGLE==1),]) + nrow(dat_1[(dat_1$PDO>0) & (dat_1$SKEW_ANGLE==1),]))
st3_2_crash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$SKEW_ANGLE==2),]),
                nrow(dat_1[(dat_1$PDO>0) & (dat_1$SKEW_ANGLE==2),]), 
                nrow(dat_1[(dat_1$PDO==0) & (dat_1$SKEW_ANGLE==2),]) + nrow(dat_1[(dat_1$PDO>0) & (dat_1$SKEW_ANGLE==2),]))
st3_3_crash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$SKEW_ANGLE==3),]),
                nrow(dat_1[(dat_1$PDO>0) & (dat_1$SKEW_ANGLE==3),]), 
                nrow(dat_1[(dat_1$PDO==0) & (dat_1$SKEW_ANGLE==3),]) + nrow(dat_1[(dat_1$PDO>0) & (dat_1$SKEW_ANGLE==3),]))
st4_1_crash = c(nrow(dat_2[(dat_2$PDO==0) & (dat_2$SKEW_ANGLE==1),]),
                nrow(dat_2[(dat_2$PDO>0) & (dat_2$SKEW_ANGLE==1),]), 
                nrow(dat_2[(dat_2$PDO==0) & (dat_2$SKEW_ANGLE==1),]) + nrow(dat_2[(dat_2$PDO>0) & (dat_2$SKEW_ANGLE==1),]))
st4_2_crash = c(nrow(dat_2[(dat_2$PDO==0) & (dat_2$SKEW_ANGLE==2),]),
                nrow(dat_2[(dat_2$PDO>0) & (dat_2$SKEW_ANGLE==2),]), 
                nrow(dat_2[(dat_2$PDO==0) & (dat_2$SKEW_ANGLE==2),]) + nrow(dat_2[(dat_2$PDO>0) & (dat_2$SKEW_ANGLE==2),]))
st4_3_crash = c(nrow(dat_2[(dat_2$PDO==0) & (dat_2$SKEW_ANGLE==3),]),
                nrow(dat_2[(dat_2$PDO>0) & (dat_2$SKEW_ANGLE==3),]), 
                nrow(dat_2[(dat_2$PDO==0) & (dat_2$SKEW_ANGLE==3),]) + nrow(dat_2[(dat_2$PDO>0) & (dat_2$SKEW_ANGLE==3),]))
sg4_1_crash = c(nrow(dat_3[(dat_3$PDO==0) & (dat_3$SKEW_ANGLE==1),]),
                nrow(dat_3[(dat_3$PDO>0) & (dat_3$SKEW_ANGLE==1),]), 
                nrow(dat_3[(dat_3$PDO==0) & (dat_3$SKEW_ANGLE==1),]) + nrow(dat_3[(dat_3$PDO>0) & (dat_3$SKEW_ANGLE==1),]))
sg4_2_crash = c(nrow(dat_3[(dat_3$PDO==0) & (dat_3$SKEW_ANGLE==2),]),
                nrow(dat_3[(dat_3$PDO>0) & (dat_3$SKEW_ANGLE==2),]), 
                nrow(dat_3[(dat_3$PDO==0) & (dat_3$SKEW_ANGLE==2),]) + nrow(dat_3[(dat_3$PDO>0) & (dat_3$SKEW_ANGLE==2),]))
sg4_3_crash = c(nrow(dat_3[(dat_3$PDO==0) & (dat_3$SKEW_ANGLE==3),]),
                nrow(dat_3[(dat_3$PDO>0) & (dat_3$SKEW_ANGLE==3),]), 
                nrow(dat_3[(dat_3$PDO==0) & (dat_3$SKEW_ANGLE==3),]) + nrow(dat_3[(dat_3$PDO>0) & (dat_3$SKEW_ANGLE==3),]))
total = st3_1_crash + st3_2_crash + st3_3_crash + st4_1_crash + st4_2_crash + st4_3_crash + sg4_1_crash + sg4_2_crash + sg4_3_crash 
tbl <- data.frame(st3_1_crash, st3_2_crash, st3_3_crash, st4_1_crash, st4_2_crash, st4_3_crash, sg4_1_crash,
                  sg4_2_crash, total)
# we've drop sg4_3_crash, as its size = 0.
rownames(tbl) <- c('Safe',"Crash","total")
tbl

### Conduct chi-square test:

Q = 0
for (i in 1:2){
  for (j in 1:8){
    Q = Q + (tbl[i,j]-(tbl[i,9]*tbl[3,j]/tbl[3,9]))^2/(tbl[i,9]*tbl[3,j]/tbl[3,9])
  }
}
Q >= qchisq(0.95,df=(2*1))

### It returns true, so there exist correlation.


## Cross type + light (PDO)
#########################################################################################################################################################

st3_0_crash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$LIGHTING==0),]),
                nrow(dat_1[(dat_1$PDO>0) & (dat_1$LIGHTING==0),]), 
                nrow(dat_1[(dat_1$PDO==0) & (dat_1$LIGHTING==0),]) + nrow(dat_1[(dat_1$PDO>0) & (dat_1$LIGHTING==0),]))
st3_1_crash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$LIGHTING==1),]),
                nrow(dat_1[(dat_1$PDO>0) & (dat_1$LIGHTING==1),]), 
                nrow(dat_1[(dat_1$PDO==0) & (dat_1$LIGHTING==1),]) + nrow(dat_1[(dat_1$PDO>0) & (dat_1$LIGHTING==1),]))
st4_0_crash = c(nrow(dat_2[(dat_2$PDO==0) & (dat_2$LIGHTING==0),]),
                nrow(dat_2[(dat_2$PDO>0) & (dat_2$LIGHTING==0),]), 
                nrow(dat_2[(dat_2$PDO==0) & (dat_2$LIGHTING==0),]) + nrow(dat_2[(dat_2$PDO>0) & (dat_2$LIGHTING==0),]))
st4_1_crash = c(nrow(dat_2[(dat_2$PDO==0) & (dat_2$LIGHTING==1),]),
                nrow(dat_2[(dat_2$PDO>0) & (dat_2$LIGHTING==1),]), 
                nrow(dat_2[(dat_2$PDO==0) & (dat_2$LIGHTING==1),]) + nrow(dat_2[(dat_2$PDO>0) & (dat_2$LIGHTING==1),]))
sg4_0_crash = c(nrow(dat_3[(dat_3$PDO==0) & (dat_3$LIGHTING==0),]),
                nrow(dat_3[(dat_3$PDO>0) & (dat_3$LIGHTING==0),]), 
                nrow(dat_3[(dat_3$PDO==0) & (dat_3$LIGHTING==0),]) + nrow(dat_3[(dat_3$PDO>0) & (dat_3$LIGHTING==0),]))
sg4_1_crash = c(nrow(dat_3[(dat_3$PDO==0) & (dat_3$LIGHTING==1),]),
                nrow(dat_3[(dat_3$PDO>0) & (dat_3$LIGHTING==1),]), 
                nrow(dat_3[(dat_3$PDO==0) & (dat_3$LIGHTING==1),]) + nrow(dat_3[(dat_3$PDO>0) & (dat_3$LIGHTING==1),]))
total = st3_0_crash + st3_1_crash + st4_0_crash + st4_1_crash + sg4_0_crash + sg4_1_crash 
tbl <- data.frame(st3_0_crash, st3_1_crash, st4_0_crash, st4_1_crash, sg4_0_crash, sg4_1_crash, total)
rownames(tbl) <- c('Safe',"Crash","total")
tbl
Q = 0
for (i in 1:2){
  for (j in 1:6){
    Q = Q + (tbl[i,j]-(tbl[i,7]*tbl[3,j]/tbl[3,7]))^2/(tbl[i,7]*tbl[3,j]/tbl[3,7])
  }
}
Q >= qchisq(0.95,df=(2*1))


### Cross type + turn 
#######################################################################################################################

st3_00_crash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$APPROACH_LEFTTURN==0) & (dat_1$APPROACH_RIGHTTURN==0),]),
                nrow(dat_1[(dat_1$PDO>0) & (dat_1$APPROACH_LEFTTURN==0) & (dat_1$APPROACH_RIGHTTURN==0),]), 
                nrow(dat_1[(dat_1$PDO==0) & (dat_1$APPROACH_LEFTTURN==0) & (dat_1$APPROACH_RIGHTTURN==0),]) + nrow(dat_1[(dat_1$PDO>0) & (dat_1$APPROACH_LEFTTURN==0) & (dat_1$APPROACH_RIGHTTURN==0),]))
st3_10_crash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$APPROACH_LEFTTURN==1) & (dat_1$APPROACH_RIGHTTURN==0),]),
                 nrow(dat_1[(dat_1$PDO>0) & (dat_1$APPROACH_LEFTTURN==1) & (dat_1$APPROACH_RIGHTTURN==0),]), 
                 nrow(dat_1[(dat_1$PDO==0) & (dat_1$APPROACH_LEFTTURN==1) & (dat_1$APPROACH_RIGHTTURN==0),]) + nrow(dat_1[(dat_1$PDO>0) & (dat_1$APPROACH_LEFTTURN==1) & (dat_1$APPROACH_RIGHTTURN==0),]))
st3_01_crash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$APPROACH_LEFTTURN==0) & (dat_1$APPROACH_RIGHTTURN==1),]),
                 nrow(dat_1[(dat_1$PDO>0) & (dat_1$APPROACH_LEFTTURN==0) & (dat_1$APPROACH_RIGHTTURN==1),]), 
                 nrow(dat_1[(dat_1$PDO==0) & (dat_1$APPROACH_LEFTTURN==0) & (dat_1$APPROACH_RIGHTTURN==1),]) + nrow(dat_1[(dat_1$PDO>0) & (dat_1$APPROACH_LEFTTURN==0) & (dat_1$APPROACH_RIGHTTURN==1),]))
st3_11_crash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$APPROACH_LEFTTURN==1) & (dat_1$APPROACH_RIGHTTURN==1),]),
                 nrow(dat_1[(dat_1$PDO>0) & (dat_1$APPROACH_LEFTTURN==1) & (dat_1$APPROACH_RIGHTTURN==1),]), 
                 nrow(dat_1[(dat_1$PDO==0) & (dat_1$APPROACH_LEFTTURN==1) & (dat_1$APPROACH_RIGHTTURN==1),]) + nrow(dat_1[(dat_1$PDO>0) & (dat_1$APPROACH_LEFTTURN==1) & (dat_1$APPROACH_RIGHTTURN==1),]))

st4_00_crash = c(nrow(dat_2[(dat_2$PDO==0) & (dat_2$APPROACH_LEFTTURN==0) & (dat_2$APPROACH_RIGHTTURN==0),]),
                 nrow(dat_2[(dat_2$PDO>0) & (dat_2$APPROACH_LEFTTURN==0) & (dat_2$APPROACH_RIGHTTURN==0),]), 
                 nrow(dat_2[(dat_2$PDO==0) & (dat_2$APPROACH_LEFTTURN==0) & (dat_2$APPROACH_RIGHTTURN==0),]) + nrow(dat_2[(dat_2$PDO>0) & (dat_2$APPROACH_LEFTTURN==0) & (dat_2$APPROACH_RIGHTTURN==0),]))
st4_10_crash = c(nrow(dat_2[(dat_2$PDO==0) & (dat_2$APPROACH_LEFTTURN==1) & (dat_2$APPROACH_RIGHTTURN==0),]),
                 nrow(dat_2[(dat_2$PDO>0) & (dat_2$APPROACH_LEFTTURN==1) & (dat_2$APPROACH_RIGHTTURN==0),]), 
                 nrow(dat_2[(dat_2$PDO==0) & (dat_2$APPROACH_LEFTTURN==1) & (dat_2$APPROACH_RIGHTTURN==0),]) + nrow(dat_2[(dat_2$PDO>0) & (dat_2$APPROACH_LEFTTURN==1) & (dat_2$APPROACH_RIGHTTURN==0),]))
st4_01_crash = c(nrow(dat_2[(dat_2$PDO==0) & (dat_2$APPROACH_LEFTTURN==0) & (dat_2$APPROACH_RIGHTTURN==1),]),
                 nrow(dat_2[(dat_2$PDO>0) & (dat_2$APPROACH_LEFTTURN==0) & (dat_2$APPROACH_RIGHTTURN==1),]), 
                 nrow(dat_2[(dat_2$PDO==0) & (dat_2$APPROACH_LEFTTURN==0) & (dat_2$APPROACH_RIGHTTURN==1),]) + nrow(dat_2[(dat_2$PDO>0) & (dat_2$APPROACH_LEFTTURN==0) & (dat_2$APPROACH_RIGHTTURN==1),]))
st4_11_crash = c(nrow(dat_2[(dat_2$PDO==0) & (dat_2$APPROACH_LEFTTURN==1) & (dat_2$APPROACH_RIGHTTURN==1),]),
                 nrow(dat_2[(dat_2$PDO>0) & (dat_2$APPROACH_LEFTTURN==1) & (dat_2$APPROACH_RIGHTTURN==1),]), 
                 nrow(dat_2[(dat_2$PDO==0) & (dat_2$APPROACH_LEFTTURN==1) & (dat_2$APPROACH_RIGHTTURN==1),]) + nrow(dat_2[(dat_2$PDO>0) & (dat_2$APPROACH_LEFTTURN==1) & (dat_2$APPROACH_RIGHTTURN==1),]))

sg4_00_crash = c(nrow(dat_3[(dat_3$PDO==0) & (dat_3$APPROACH_LEFTTURN==0) & (dat_3$APPROACH_RIGHTTURN==0),]),
                 nrow(dat_3[(dat_3$PDO>0) & (dat_3$APPROACH_LEFTTURN==0) & (dat_3$APPROACH_RIGHTTURN==0),]), 
                 nrow(dat_3[(dat_3$PDO==0) & (dat_3$APPROACH_LEFTTURN==0) & (dat_3$APPROACH_RIGHTTURN==0),]) + nrow(dat_3[(dat_3$PDO>0) & (dat_3$APPROACH_LEFTTURN==0) & (dat_3$APPROACH_RIGHTTURN==0),]))
sg4_10_crash = c(nrow(dat_3[(dat_3$PDO==0) & (dat_3$APPROACH_LEFTTURN==1) & (dat_3$APPROACH_RIGHTTURN==0),]),
                 nrow(dat_3[(dat_3$PDO>0) & (dat_3$APPROACH_LEFTTURN==1) & (dat_3$APPROACH_RIGHTTURN==0),]), 
                 nrow(dat_3[(dat_3$PDO==0) & (dat_3$APPROACH_LEFTTURN==1) & (dat_3$APPROACH_RIGHTTURN==0),]) + nrow(dat_3[(dat_3$PDO>0) & (dat_3$APPROACH_LEFTTURN==1) & (dat_3$APPROACH_RIGHTTURN==0),]))
sg4_01_crash = c(nrow(dat_3[(dat_3$PDO==0) & (dat_3$APPROACH_LEFTTURN==0) & (dat_3$APPROACH_RIGHTTURN==1),]),
                 nrow(dat_3[(dat_3$PDO>0) & (dat_3$APPROACH_LEFTTURN==0) & (dat_3$APPROACH_RIGHTTURN==1),]), 
                 nrow(dat_3[(dat_3$PDO==0) & (dat_3$APPROACH_LEFTTURN==0) & (dat_3$APPROACH_RIGHTTURN==1),]) + nrow(dat_3[(dat_3$PDO>0) & (dat_3$APPROACH_LEFTTURN==0) & (dat_3$APPROACH_RIGHTTURN==1),]))
sg4_11_crash = c(nrow(dat_3[(dat_3$PDO==0) & (dat_3$APPROACH_LEFTTURN==1) & (dat_3$APPROACH_RIGHTTURN==1),]),
                 nrow(dat_3[(dat_3$PDO>0) & (dat_3$APPROACH_LEFTTURN==1) & (dat_3$APPROACH_RIGHTTURN==1),]), 
                 nrow(dat_3[(dat_3$PDO==0) & (dat_3$APPROACH_LEFTTURN==1) & (dat_3$APPROACH_RIGHTTURN==1),]) + nrow(dat_3[(dat_3$PDO>0) & (dat_3$APPROACH_LEFTTURN==1) & (dat_3$APPROACH_RIGHTTURN==1),]))

tbl <- data.frame(st3_00_crash,st3_01_crash,st3_10_crash,st3_11_crash,st4_00_crash,st4_10_crash,st4_11_crash,sg4_00_crash,sg4_01_crash,sg4_10_crash,sg4_11_crash,total)
rownames(tbl) <- c('Safe',"Crash","total")
tbl
Q = 0
for (i in 1:2){
  for (j in 1:11){
    Q = Q + (tbl[i,j]-(tbl[i,12]*tbl[3,j]/tbl[3,12]))^2/(tbl[i,12]*tbl[3,j]/tbl[3,12])
  }
}
Q >= qchisq(0.95,df=(2*1))

### Cross type + AADT major
##PDO########################################################################################################################################

st3_aadt_m_pdocrash = dat_1[dat_1$PDO>0,c("AADT_MAJOR")]
st3_aadt_m_pdonocrash = dat_1[dat_1$PDO==0,c("AADT_MAJOR")]
st4_aadt_m_pdocrash = dat_2[dat_2$PDO>0,c("AADT_MAJOR")]
st4_aadt_m_pdonocrash = dat_2[dat_2$PDO==0,c("AADT_MAJOR")]
sg4_aadt_m_pdocrash = dat_3[dat_3$PDO>0,c("AADT_MAJOR")]
sg4_aadt_m_pdonocrash = dat_3[dat_3$PDO==0,c("AADT_MAJOR")]

### Comparing st3:
shapiro.test(st3_aadt_m_pdocrash)
shapiro.test(st3_aadt_m_pdonocrash)
shapiro.test(st4_aadt_m_pdocrash)
shapiro.test(st4_aadt_m_pdonocrash)
shapiro.test(sg4_aadt_m_pdocrash)
shapiro.test(sg4_aadt_m_pdonocrash)

### Only sg4_aadt_m_pdonocrash passes the normality test, while their dispersions are similar, so we decide to conduct w-rank test.

### Check st3 (crash v.s. non crash)

X <- data.frame(value = data.matrix(st3_aadt_m_pdonocrash), label = "nocrash") 
Y <- data.frame(value = data.matrix(st3_aadt_m_pdocrash), label = "crash") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)[table(data_XY$rank)!= 1]
W <- data_XY[data_XY$label=='crash',] %>% dplyr::select("rank") %>% sum
n = length(st3_aadt_m_pdocrash)
m = length(st3_aadt_m_pdonocrash)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(35*2*(2^2-1)+18*3*(3^2-1)+9*4*(4^2-1)+10*5*(5^2-1)+6*6*(6^2-1)+3*7*(7^2-1)+2*9*(9^2-1)+4*10*(10^2-1)+12*(12^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### Yes, controlling crosstype, the major addt affects pdo crashing number.

### Chech st4 (crash v.s. non crash)

X <- data.frame(value = data.matrix(st4_aadt_m_pdonocrash), label = "nocrash") 
Y <- data.frame(value = data.matrix(st4_aadt_m_pdocrash), label = "crash") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)[table(data_XY$rank)!= 1]
W <- data_XY[data_XY$label=='crash',] %>% dplyr::select("rank") %>% sum
n = length(st4_aadt_m_pdocrash)
m = length(st4_aadt_m_pdonocrash)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(13*2*(2^2-1)+2*3*(3^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### This time, we think major addt doesn't affects pdo crashing number in st4 crossing.

### Check sg4 (crash v.s. non crash)

X <- data.frame(value = data.matrix(sg4_aadt_m_pdonocrash), label = "nocrash") 
Y <- data.frame(value = data.matrix(sg4_aadt_m_pdocrash), label = "crash") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)[table(data_XY$rank)!= 1]
W <- data_XY[data_XY$label=='crash',] %>% dplyr::select("rank") %>% sum
n = length(sg4_aadt_m_pdocrash)
m = length(sg4_aadt_m_pdonocrash)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(21*2*(2^2-1)+3*(3^2-1)+4*(4^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### And also, we don't have sufficent clue to show that major addt affects pdo crashing number in sg4 crossing.


### Cross type + AADT minor
########################################################################################################################################

st3_aadt_m_pdocrash = dat_1[dat_1$PDO>0,c("AADT_MINOR")]
st3_aadt_m_pdonocrash = dat_1[dat_1$PDO==0,c("AADT_MINOR")]
st4_aadt_m_pdocrash = dat_2[dat_2$PDO>0,c("AADT_MINOR")]
st4_aadt_m_pdonocrash = dat_2[dat_2$PDO==0,c("AADT_MINOR")]
sg4_aadt_m_pdocrash = dat_3[dat_3$PDO>0,c("AADT_MINOR")]
sg4_aadt_m_pdonocrash = dat_3[dat_3$PDO==0,c("AADT_MINOR")]

### Comparing st3:
shapiro.test(st3_aadt_m_pdocrash)
shapiro.test(st3_aadt_m_pdonocrash)
shapiro.test(st4_aadt_m_pdocrash)
shapiro.test(st4_aadt_m_pdonocrash)
shapiro.test(sg4_aadt_m_pdocrash)
shapiro.test(sg4_aadt_m_pdonocrash)

### Only sg4_aadt_m_pdonocrash passes the normality test, while their dispersions are similar, so we decide to conduct w-rank test.

### Check st3 (crash v.s. non crash)

X <- data.frame(value = data.matrix(st3_aadt_m_pdonocrash), label = "nocrash") 
Y <- data.frame(value = data.matrix(st3_aadt_m_pdocrash), label = "crash") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)[table(data_XY$rank)!= 1]
W <- data_XY[data_XY$label=='crash',] %>% dplyr::select("rank") %>% sum
n = length(st3_aadt_m_pdocrash)
m = length(st3_aadt_m_pdonocrash)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(6*2*(2^2-1)+9*3*(3^2-1)+8*4*(4^2-1)+4*5*(5^2-1)+4*6*(6^2-1)+4*7*(7^2-1)+5*8*(8^2-1)+3*9*(9^2-1)+2*10*(10^2-1)+2*11*(11^2-1)+2*12*(12^2-1)+13*(13^2-1)+3*14*(14^2-1)+15*(15^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### Yes, controlling crosstype, the major addt affects pdo crashing number.

### Chech st4 (crash v.s. non crash)

X <- data.frame(value = data.matrix(st4_aadt_m_pdonocrash), label = "nocrash") 
Y <- data.frame(value = data.matrix(st4_aadt_m_pdocrash), label = "crash") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)[table(data_XY$rank)!= 1]
W <- data_XY[data_XY$label=='crash',] %>% dplyr::select("rank") %>% sum
n = length(st4_aadt_m_pdocrash)
m = length(st4_aadt_m_pdonocrash)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(8*2*(2^2-1)+6*3*(3^2-1)+2*4*(4^2-1)+5*(5^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### This time, we think major addt doesn't affects pdo crashing number in st4 crossing.

### Check sg4 (crash v.s. non crash)

X <- data.frame(value = data.matrix(sg4_aadt_m_pdonocrash), label = "nocrash") 
Y <- data.frame(value = data.matrix(sg4_aadt_m_pdocrash), label = "crash") 
data_XY <- rbind(X,Y)[order(rbind(X,Y)$value),]
data_XY$rank = rank(data_XY$value, ties.method = "average")
table(data_XY$rank)[table(data_XY$rank)!= 1]
W <- data_XY[data_XY$label=='crash',] %>% dplyr::select("rank") %>% sum
n = length(sg4_aadt_m_pdocrash)
m = length(sg4_aadt_m_pdonocrash)
E_0_W <- n *(m+n+1)/2
var_0_W <- (m*n/12)*((m+n+1)-1/((m+n)*(m+n-1))*(16*2*(2^2-1)+8*(3^2-1)+3*(4^2-1)))
T_ <- abs((W-E_0_W)/sqrt(var_0_W))
Z_crit <- qnorm(0.025, 0, 1, lower.tail = F)
abs(T_) > Z_crit

### For BC and KA with AADT, just applie the codes above.

#####################################################################################
### We now conduct set of 2*2 tables test for turns and lighting.

## For lighting, we forming the tables:

#st3:

st3_noncrash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$KA==0) & (dat_1$BC==0) & (dat_1$LIGHTING==0),]),
                nrow(dat_1[(dat_1$PDO==0) & (dat_1$KA==0) & (dat_1$BC==0) & (dat_1$LIGHTING==1),]), 
                nrow(dat_1[(dat_1$PDO==0) & (dat_1$KA==0) & (dat_1$BC==0) & (dat_1$LIGHTING==0),]) + nrow(dat_1[(dat_1$PDO==0) & (dat_1$KA==0) & (dat_1$BC==0) & (dat_1$LIGHTING==1),]))

total = c(nrow(dat_1[ (dat_1$LIGHTING==0),]),
          nrow(dat_1[ (dat_1$LIGHTING==1),]),
          nrow(dat_1))

st3_crash = total - st3_noncrash

tbl_st3 <- data.frame(st3_noncrash,st3_crash,total)
rownames(tbl_st3) <- c('Light',"No light","total")
tbl_st3

#st4:
st4_noncrash = c(nrow(dat_2[(dat_2$PDO==0) & (dat_2$KA==0) & (dat_2$BC==0) & (dat_2$LIGHTING==0),]),
                 nrow(dat_2[(dat_2$PDO==0) & (dat_2$KA==0) & (dat_2$BC==0) & (dat_2$LIGHTING==1),]), 
                 nrow(dat_2[(dat_2$PDO==0) & (dat_2$KA==0) & (dat_2$BC==0) & (dat_2$LIGHTING==0),]) + nrow(dat_2[(dat_2$PDO==0) & (dat_2$KA==0) & (dat_2$BC==0) & (dat_2$LIGHTING==1),]))

total = c(nrow(dat_2[ (dat_2$LIGHTING==0),]),
          nrow(dat_2[ (dat_2$LIGHTING==1),]),
          nrow(dat_2))

st4_crash = total - st4_noncrash

tbl_st4 <- data.frame(st4_noncrash,st4_crash,total)
rownames(tbl_st4) <- c('Light',"No light","total")
tbl_st4

#sg4:
sg4_noncrash = c(nrow(dat_3[(dat_3$PDO==0) & (dat_3$KA==0) & (dat_3$BC==0) & (dat_3$LIGHTING==0),]),
                 nrow(dat_3[(dat_3$PDO==0) & (dat_3$KA==0) & (dat_3$BC==0) & (dat_3$LIGHTING==1),]), 
                 nrow(dat_3[(dat_3$PDO==0) & (dat_3$KA==0) & (dat_3$BC==0) & (dat_3$LIGHTING==0),]) + nrow(dat_3[(dat_3$PDO==0) & (dat_3$KA==0) & (dat_3$BC==0) & (dat_3$LIGHTING==1),]))

total = c(nrow(dat_3[ (dat_3$LIGHTING==0),]),
          nrow(dat_3[ (dat_3$LIGHTING==1),]),
          nrow(dat_3))

sg4_crash = total - sg4_noncrash

tbl_sg4 <- data.frame(sg4_noncrash,sg4_crash,total)
rownames(tbl_sg4) <- c('Light',"No light","total")
tbl_sg4

#QMH
nume = 0
deno = 0
df.list <- list(tbl_st3,tbl_st4,tbl_sg4)
for (tbl in df.list ){
  nume = nume + (tbl[1,1]-(tbl[1,3]*tbl[3,1])/tbl[3,3])
  deno = deno + (tbl[1,3]*tbl[2,3]*tbl[3,1]*tbl[3,2])/(tbl[3,3]*tbl[3,3]*(tbl[3,3]-1))
}
QMH = (nume^2)/deno
QMH >= qchisq(1-0.05, df=1)

## It shows that we have no reason to refuse null hypothesis of MH test.



#####################################################################################################
### Now the MH test for turns.

st3_noncrash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$KA==0) & (dat_1$BC==0) & (dat_1$APPROACH_LEFTTURN==0) & (dat_1$APPROACH_RIGHTTURN==0) ,]),
                 nrow(dat_1[(dat_1$PDO==0) & (dat_1$KA==0) & (dat_1$BC==0) & ((dat_1$APPROACH_LEFTTURN==1) | (dat_1$APPROACH_RIGHTTURN==1)),]), 
                 nrow(dat_1[(dat_1$PDO==0) & (dat_1$KA==0) & (dat_1$BC==0) & (dat_1$APPROACH_LEFTTURN==0) & (dat_1$APPROACH_RIGHTTURN==0),]) + nrow(dat_1[(dat_1$PDO==0) & (dat_1$KA==0) & (dat_1$BC==0) & ((dat_1$APPROACH_LEFTTURN==1) | (dat_1$APPROACH_RIGHTTURN==1)),]))

total = c(nrow(dat_1[(dat_1$APPROACH_LEFTTURN==0) & (dat_1$APPROACH_RIGHTTURN==0),]),
          nrow(dat_1[((dat_1$APPROACH_LEFTTURN==1) | (dat_1$APPROACH_RIGHTTURN==1)),]),
          nrow(dat_1))

st3_crash = total - st3_noncrash

tbl_st3 <- data.frame(st3_noncrash,st3_crash,total)
rownames(tbl_st3) <- c('No turns',"Turns","total")
tbl_st3

#st4:
st4_noncrash = c(nrow(dat_2[(dat_2$PDO==0) & (dat_2$KA==0) & (dat_2$BC==0) & (dat_2$APPROACH_LEFTTURN==0) & (dat_2$APPROACH_RIGHTTURN==0),]),
                 nrow(dat_2[(dat_2$PDO==0) & (dat_2$KA==0) & (dat_2$BC==0) & ((dat_2$APPROACH_LEFTTURN==1) | (dat_2$APPROACH_RIGHTTURN==1)),]), 
                 nrow(dat_2[(dat_2$PDO==0) & (dat_2$KA==0) & (dat_2$BC==0) & (dat_2$APPROACH_LEFTTURN==0) & (dat_2$APPROACH_RIGHTTURN==0),]) + nrow(dat_2[(dat_2$PDO==0) & (dat_2$KA==0) & (dat_2$BC==0) & ((dat_2$APPROACH_LEFTTURN==1) | (dat_2$APPROACH_RIGHTTURN==1)),]))

total = c(nrow(dat_2[ (dat_2$APPROACH_LEFTTURN==0) & (dat_2$APPROACH_RIGHTTURN==0),]),
          nrow(dat_2[ ((dat_2$APPROACH_LEFTTURN==1) | (dat_2$APPROACH_RIGHTTURN==1)),]),
          nrow(dat_2))

st4_crash = total - st4_noncrash

tbl_st4 <- data.frame(st4_noncrash,st4_crash,total)
rownames(tbl_st4) <- c('No turns',"Turns","total")
tbl_st4

#sg4
sg4_noncrash = c(nrow(dat_3[(dat_3$PDO==0) & (dat_3$KA==0) & (dat_3$BC==0) & (dat_3$APPROACH_LEFTTURN==0) & (dat_3$APPROACH_RIGHTTURN==0),]),
                 nrow(dat_3[(dat_3$PDO==0) & (dat_3$KA==0) & (dat_3$BC==0) & ((dat_3$APPROACH_LEFTTURN==1) | (dat_3$APPROACH_RIGHTTURN==1)),]), 
                 nrow(dat_3[(dat_3$PDO==0) & (dat_3$KA==0) & (dat_3$BC==0) & (dat_3$APPROACH_LEFTTURN==0) & (dat_3$APPROACH_RIGHTTURN==0),]) + nrow(dat_3[(dat_3$PDO==0) & (dat_3$KA==0) & (dat_3$BC==0) & ((dat_3$APPROACH_LEFTTURN==1) | (dat_3$APPROACH_RIGHTTURN==1)),]))

total = c(nrow(dat_3[ (dat_3$APPROACH_LEFTTURN==0) & (dat_3$APPROACH_RIGHTTURN==0),]),
          nrow(dat_3[ ((dat_3$APPROACH_LEFTTURN==1) | (dat_3$APPROACH_RIGHTTURN==1)),]),
          nrow(dat_3))

sg4_crash = total - sg4_noncrash

tbl_sg4 <- data.frame(sg4_noncrash,sg4_crash,total)
rownames(tbl_sg4) <- c('No turns',"Turns","total")
tbl_sg4

#QMH
nume = 0
deno = 0
df.list <- list(tbl_st3,tbl_st4,tbl_sg4)
for (tbl in df.list ){
  nume = nume + (tbl[1,1]-(tbl[1,3]*tbl[3,1])/tbl[3,3])
  deno = deno + (tbl[1,3]*tbl[2,3]*tbl[3,1]*tbl[3,2])/(tbl[3,3]*tbl[3,3]*(tbl[3,3]-1))
}
QMH = (nume^2)/deno
QMH >= qchisq(1-0.05, df=1)

## It also shows that we have no reason to refuse null hypothesis of MH test.

### Conduct chi-square test on individual table
chisq.test(tbl_sg4[1:2,1:2],correct = FALSE)

### Conduct Fisher-exact test on individual table
fisher.test(tbl_st4[1:2,1:2], alternative = "two.sided", conf.int=FALSE)
