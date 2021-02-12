function dxy = diffxy(t, xy)
%
%split xy into variables in our equations
%
A = xy(1);
P = xy(2);
%
% define the derivatives of these variables from equations
%
%time t is in kyr
om=0.31416;
k=50;
S=100;
I=0.004;
beta=0.006;
Adot = I-k*(1+beta*cos(om*t))*A*exp(P);
Pdot = k*(1+beta*cos(om*t))*A-S;
%i
%return the derivatives in dxy in the right order
%
dxy = [Adot; Pdot];

