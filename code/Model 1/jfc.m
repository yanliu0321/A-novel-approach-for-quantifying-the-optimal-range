function [x1,tt]=jfc(T151,T152,T211,T212,par1)
X0=[0 1000000 0 0 0 1 0];
options2=odeset('RelTol',1e-5,'AbsTol',1e-5,'Events',@event);
x1=[1,9,1,1,1,1,1];
tt=0:4;

for i=1:4
    t1=T151(i);
    t2=T211(i);
    t3=T212(i);
    t4=T152(i);
    t0=tt(i)+1;
    if t1==0||(t1-t0)<2
       t1=t0+2;
    end
    if x1(end,2)/(x1(end,1)+x1(end,2))>=0.9
    [t,x]=ode45(@ELPAWmodel,t0:t1,X0,[],par1,1,0,0);
    X0=x(end,:);x1=[x1;x];
    else
    [t,x]=ode45(@(i,y) ELPAWmodel(i,y,par1,1,0,1),t0:t1,X0,options2);
    X0=x(end,:);x1=[x1; x];tt(i)=t(end);t0=tt(i)+1;
    if round(t0)<(t1-2)
    [t,x]=ode45(@ELPAWmodel,t0:t1,X0,[],par1,1,0,0);
    X0=x(end,:);x1=[x1;x];  
    else
    t1=t(end);tt(i)=0;      
    end
    end  
    if (t2-t1)<3
        t2=t1+3;
    end
    [t,x]=ode45(@ELPAWmodel,(t1+1):t2,X0,[],par1,1,1,1);
    X0=x(end,:);x1=[x1;x];
    if (t3-t2)<3
        t3=t2+3;
    end
    [t,x]=ode45(@ELPAWmodel,(t2+1):t3,X0,[],par1,0,1,1);
    X0=x(end,:);x1=[x1;x];
    if t4==0||(t4-t3)<3
       t4=t3+3; 
    end    
    [t,x]=ode45(@ELPAWmodel,(t3+1):t4,X0,[],par1,1,1,1);%[T212,T152],温度高于15℃，低于21℃，无滞育期，成蚊产滞育卵,滞育卵孵化
    X0=x(end,:);x1=[x1;x];tt(i+1)=t(end);
end
    i=5;
    t1=T151(i);
    t2=T211(i);
    t3=T212(i);
    t4=T152(i);
    t0=tt(i)+1;
    if t1==0||(t1-t0)<2
       t1=t0+2; 
    end
    if x1(end,2)/(x1(end,1)+x1(end,2))>=0.9
    [t,x]=ode45(@ELPAWmodel,t0:t1,X0,[],par1,1,0,0);
    X0=x(end,:);x1=[x1;x];
    else
    [t,x]=ode45(@(i,y) ELPAWmodel(i,y,par1,1,0,1),t0:t1,X0,options2);
    X0=x(end,:);x1=[x1; x];tt(i)=t(end);t0=tt(i)+1;
    if round(t0)<(t1-2)
    [t,x]=ode45(@ELPAWmodel,t0:t1,X0,[],par1,1,0,0);
    X0=x(end,:);x1=[x1;x];  
    else
    t1=t(end);tt(i)=0;    
    end
    end 
    if (t2-t1)<3
        t2=t1+3;
    end
    [t,x]=ode45(@ELPAWmodel,(t1+1):t2,X0,[],par1,1,1,1);
    X0=x(end,:);x1=[x1;x];
    if (t3-t2)<3
        t3=t2+3;
    end
    [t,x]=ode45(@ELPAWmodel,(t2+1):t3,X0,[],par1,0,1,1);
    X0=x(end,:);x1=[x1;x];

[t,x]=ode45(@ELPAWmodel,(t3+1):1826,X0,[],par1,1,1,1); x1=[x1;x];
x1(1,:)=[];
