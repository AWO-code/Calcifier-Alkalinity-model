format('long')
options=odeset('RelTol',1e-14);
istate=[1.9841131 log(0.0001)];
lensim=[0 10000];
[t,xy]=ode15s(@CAmodelTransForced,lensim,istate,options);
