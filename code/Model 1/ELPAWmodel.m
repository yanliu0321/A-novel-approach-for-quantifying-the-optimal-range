function dy=ELPAWmodel(i,y,par1,z1,z2,z3)
t=round(i);
 global R T P
dy=zeros(7,1);
b0=par1(1);Emax=par1(2);Emean=par1(3);Evar=par1(4);
mu01=par1(5);T01=par1(6);v1=par1(7);mu02=par1(8);T02=par1(9);v2=par1(10);
mu03=par1(11);T03=par1(12);v3=par1(13);delta=par1(14);
a=par1(15);b=par1(16);alpha=par1(17);lamda=par1(18);k_var=par1(19);k_fix=par1(20);

v= b0+Emax./(1+exp((Emean-y(6))./Evar));me=0.05;
if T(t)>10
    de=(T(t)-10)./110;
else
    de=0;
end
dl=-0.0007.*T(t)^2+0.0392.*T(t)-0.3911;dp=0.0008.*T(t)^2-0.0051.*T(t)+0.0319;
ml= 1-mu01.*exp(-(T(t)-T01).^2./v1^2);
mp= 1-mu02.*exp(-(T(t)-T02).^2./v2^2);
ma= 1-mu03.*exp(-(T(t)-T03).^2./v3^2);
k=k_var.*(Ga(P(t),alpha,lamda))+k_fix;
ef=exp(-0.1.*(1+y(4)./k));
dy=[(1-z1)*z3*v*y(5)-me*y(1)-de*y(1);
    z1*z3*v*y(5)-z2*a*de*y(2)-b*me*y(2);
    de*y(1)+z2*a*de*y(2)-ml*y(3)-dl*y(3)-(ml/k)*y(3)^2;
    dl*y(3)-mp*y(4)-dp*y(4);
    ef*dp*y(4)-ma*y(5);
    R(t)-delta*y(6);
    ef*dp*y(4)
    ];