import numpy as np

def sign2indict(x):
    x[np.where(x == 0)] = -1
    return x

def weight(y,scale):
    z = np.zeros((len(y),1))
    for i in range(len(y)):
        if y[i]==1:
            z[i]=float(1-scale)
        else:
            z[i]=float(scale)
    return z

def PSVM(X,Y,h,Lambda,printf):
    p = X.shape[1]
    n = X.shape[0]
    tile = np.percentile(Y, np.linspace(0, 100, h + 2)[1:-1].tolist())
    for i in range(h):
        locals()['Y' + str(i + 1)] = sign2indict(np.sign(Y - tile[i]))
    Sigma = np.cov(X.T)
    Lambda = Lambda
    def minussqrtm(M):
        (U,S,VT) = np.linalg.svd(M)
        D = np.diag(1/np.sqrt(S))
        return np.dot(np.dot(U,D),VT)
    Z = np.dot(X-X.mean(axis=0),minussqrtm(Sigma))
    from sklearn.svm import SVC
    for l in range(h):
        svc = SVC(C=Lambda/(2*n),kernel='linear')
        svc.fit(Z,locals()['Y'+ str(l+1)].ravel())
        locals()['beta' + str(l+1)] = np.dot(minussqrtm(Sigma),svc.coef_.reshape(p,1))
        if printf == True:
            print(np.concatenate([svc.intercept_.ravel(),locals()['beta' + str(l+1)].ravel()]))
    M = 0
    for i in range(h):
        M += np.dot(locals()['beta' + str(i + 1)].reshape(p, 1), locals()['beta' + str(i + 1)].reshape(1, p))
    return M
