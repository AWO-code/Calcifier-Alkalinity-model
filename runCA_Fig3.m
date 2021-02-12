format('long')
options=odeset('RelTol',1e-14);
istate=[2.0 log(0.002)];
lensim=[0 10000];
[t,xy]=ode15s(@CA_Forced,lensim,istate,options);
