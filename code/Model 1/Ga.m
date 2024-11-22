function p=Ga(x,alpha,lamda)
p=lamda.^alpha./gamma(alpha).*x.^(alpha-1).*exp(-lamda.*x);