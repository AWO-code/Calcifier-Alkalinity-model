      implicit none
      real*8 A,P,t,u,v,w,mu
      real*8 om,dt,Pi,I,k1,k2,alpha
      integer j,k,l,q,y
      I=0.004
      dt=0.00001
      Pi=3.14159
      om=2*Pi/20
      t=0
      A=2.0
      P=log(0.0001)
      k1=50
      k2=100
      alpha=0.004
      do l=1,1000000000
       q=mod(l,10000)
       if (q == 0) then
       write(60,"(e13.5,e13.5)") t, A
       write(70,"(e13.5,e13.5)") t, dexp(P)
       end if
       y=mod(l,100000)
       if (y == 0) then
         call random_number(u)
         call random_number(v)
         w=sqrt(-2*log(u))*cos(2*pi*v)
       write(80,"(e13.5,e13.5)") t, w
       end if 
        mu=k1*(1.0+0.005*w+alpha*dcos(om*t))*A
        A=A+dt*(I-mu*dexp(P))
	P=P+dt*(mu-k2)
        t=t+dt
      end do
      end
        
      
