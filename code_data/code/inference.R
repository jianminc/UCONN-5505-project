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


### About KA

### About BC