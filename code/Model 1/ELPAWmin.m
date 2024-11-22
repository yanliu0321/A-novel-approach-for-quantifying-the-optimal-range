function d=ELPAWmin(par1,MOI,BI,T151,T152,T211,T212,year,month)%
[x1,tt]=jfc(T151,T152,T211,T212,par1);
par1
data=table(year,month,x1(1:1826,:));
statarray = grpstats(data,{'year','month'},'sum');
semimonth=table2array(statarray(:,4));
L_semimonth=semimonth(:,3);
A_semimonth=semimonth(:,5);
L_semimonth(isnan(BI))=[];
A_semimonth(isnan(MOI))=[];
L_semimonth=L_semimonth./sum(L_semimonth);
A_semimonth=A_semimonth./sum(A_semimonth);
MOI(isnan(MOI))=[];
BI(isnan(BI))=[];
BI=BI./sum(BI);MOI=MOI./sum(MOI);

d=dist(A_semimonth',MOI)+dist(L_semimonth',BI);
