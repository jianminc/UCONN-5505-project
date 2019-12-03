import numpy as np
import pandas as pd
from inp import PSVM
from scipy.linalg import eigh

st3_tbl = pd.read_csv('../data/3ST.csv')
st4_tbl = pd.read_csv('../data/4ST.csv')
sg4_tbl = pd.read_csv('../data/4SG.csv')

tbl_total = pd.concat([pd.concat([st3_tbl, st4_tbl]),sg4_tbl])
X = tbl_total[['AADT_MAJOR','AADT_MINOR','LIGHTING','APPROACH_LEFTTURN', 'APPROACH_RIGHTTURN','SKEW_ANGLE']].values
Y_PDO = tbl_total['PDO'].values
Y_BC = tbl_total['BC'].values
Y_KA = tbl_total['KA'].values

M_pdo = PSVM(X, Y_PDO, h=4,Lambda =1,printf=False)
lam,ve = eigh(M_pdo)

def select_d(lam):
    jump = []
    for i in range(len(lam)-1):
        jump.append(lam[-(i+1)]/lam[-(i+2)])
    return jump.index(max(jump))+1

d = select_d(lam)
Ve_pdo = ve[:,-d:]

M_bc = PSVM(X, Y_BC, h=4,Lambda =1,printf=False)
lam,ve = eigh(M_bc)

def select_d(lam):
    jump = []
    for i in range(len(lam)-1):
        jump.append(lam[-(i+1)]/lam[-(i+2)])
    return jump.index(max(jump))+1

d = select_d(lam)
Ve_bc = ve[:,-d:]

M_ka = PSVM(X, Y_KA, h=4,Lambda =1,printf=False)
lam,ve = eigh(M_ka)

def select_d(lam):
    jump = []
    for i in range(len(lam)-1):
        jump.append(lam[-(i+1)]/lam[-(i+2)])
    return jump.index(max(jump))+1

d = select_d(lam)
Ve_ka = ve[:,-d:]