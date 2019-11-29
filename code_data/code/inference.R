##Data import 
dat_1 = read.csv('./../data/3ST.csv')
dat_2 = read.csv('./../data/4ST.csv')
dat_3 = read.csv('./../data/4SG.csv')

### Question 1: Simplest question, do these different types of intersection holds different number of crashes? (Measured by mean or median.)

### About PDO

st3_pdo = dat_1$PDO
st4_pdo = dat_2$PDO
sg4_pdo = dat_3$PDO

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

### Question 2: We have already known the crossing type affects the crash number, so we now explore whether cross type + another property may effects the crash number.

## Cross type + discrete skewed angle 

## Firstly, we make skewed angle discrete:

dat_1$SKEW_ANGLE = cut(dat_1$SKEW_ANGLE, breaks=c(-1,31,61,91), labels=c(1,2,3))
dat_2$SKEW_ANGLE = cut(dat_2$SKEW_ANGLE, breaks=c(-1,31,61,91), labels=c(1,2,3))
dat_3$SKEW_ANGLE = cut(dat_3$SKEW_ANGLE, breaks=c(-1,31,61,91), labels=c(1,2,3))

## Forming the table:

st3_1_crash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$SKEW_ANGLE==1),]),
                nrow(dat_1[(dat_1$PDO>0) & (dat_1$SKEW_ANGLE==1),]), 
                nrow(dat_1[(dat_1$PDO==0) & (dat_1$SKEW_ANGLE==1),]) + nrow(dat_1[(dat_1$PDO>0) & (dat_1$SKEW_ANGLE==1),]))
st3_2_crash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$SKEW_ANGLE==2),]),
                nrow(dat_1[(dat_1$PDO>0) & (dat_1$SKEW_ANGLE==2),]), 
                nrow(dat_1[(dat_1$PDO==0) & (dat_1$SKEW_ANGLE==2),]) + nrow(dat_1[(dat_1$PDO>0) & (dat_1$SKEW_ANGLE==2),]))
st3_3_crash = c(nrow(dat_1[(dat_1$PDO==0) & (dat_1$SKEW_ANGLE==3),]),
                nrow(dat_1[(dat_1$PDO>0) & (dat_1$SKEW_ANGLE==3),]), 
                nrow(dat_1[(dat_1$BC==0) & (dat_1$SKEW_ANGLE==3),]) + nrow(dat_1[(dat_1$PDO>0) & (dat_1$SKEW_ANGLE==3),]))
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
                  sg4_2_crash, sg4_3_crash, total)
rownames(tbl) <- c('Safe',"Crash","total")
tbl



