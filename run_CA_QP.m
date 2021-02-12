format('long')
options=odeset('RelTol',1e-14);
istate=[2.0 log(0.0001)];
lensim=[0 10000];
[t,xy]=ode15s(@CA_Forced_QuasiPer,lensim,istate,options);
