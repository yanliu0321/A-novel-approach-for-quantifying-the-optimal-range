function [c,ceq]=constraintfile(par1)
global T 
mu01=par1(5);T01=par1(6);v1=par1(7);mu02=par1(8);T02=par1(9);v2=par1(10);
mu03=par1(11);T03=par1(12);v3=par1(13);alpha=par1(17);belta=par1(18);
ml= 1-mu01.*exp(-(T-T01).^2./v1^2);
mp= 1-mu02.*exp(-(T-T02).^2./v2^2);
ma= 1-mu03.*exp(-(T-T03).^2./v3^2);
c=[
   ml-1;
   mp-1;
   ma-1;
   1-(alpha-1)/belta];

ceq=[];


