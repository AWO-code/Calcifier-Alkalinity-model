format('long')
options=odeset('RelTol',1e-14);
istate=[2.0 log(0.0003)];
lensim=[0 10000];
[u,ab]=ode15s(@CAmodelTransForced,lensim,istate,options);
