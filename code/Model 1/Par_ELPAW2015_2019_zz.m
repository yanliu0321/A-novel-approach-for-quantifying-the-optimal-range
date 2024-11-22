%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  All Code was Written by Yan Liu
%%%
%%%  Last updated: 2024.11.22
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


clear all
clc

load work_history.mat


%% 
% Solve the optimal parameters
%    b0 Emax Emean Evar   MUl  Tl Vl      MUp  Tp Vp     MUa  Ta Va      a   b  delta    alpha  beta   kR   kbasis 
lb=[0  10   -5    0     0.1  27 5       0.1  29 5      0.1  20 5      0.1 0.01 0.01    50     0.1    0    290];%
ub=[5  20    5    2     0.96 29 20     0.86  31 20     0.76 21 20      1   1    1      inf     1    10^6 34632];
% Step 1:
% options1=optimoptions('ga','Display','final','Generations',2000,'NonlinearConstraintAlgorithm','auglag');
% [par1,fval]=ga(@(par1) ELPAWmin(par1,MOI,BI,T151,T152,T211,T212,year,month),20,[],[],[],[],lb,ub,@constraintfile,options1);%根据数据使用遗传算法进行参数估计
% canshu=par1;


par1=[4.84 	19.61 	0.84 	0.1 	0.68 	28.60 	19.84 	0.84 	30.71 	19.83 	0.75 	20.03 	5.08 	0.68 	0.99 	0.02 	66.14 	0.39 	951558.95 	32643.64 
];

[x1,tt]=jfc(T151,T152,T211,T212,par1);


%%
% reault
data=table(year,month,x1);
statarray = grpstats(data,{'year','month'},'sum');
statarray_A = grpstats(data,{'year'},'sum');
semimonth=table2array(statarray(:,4));



figure
% define color
color_fill = [0.1, 0.5, 0.1];    
color_moi_bi = [203/255, 173/255, 56/255];   
color_model0 = [0.5, 0.2, 0.7];     

subplot(221)
x = 1:96;
y_lower = zeros(1, 96);  
fill([x fliplr(x)], [semimonth(13:end,5)' ./ 7.2^3 fliplr(y_lower)], color_fill, 'FaceAlpha', 0.5, 'EdgeColor', 'none') % 更深的填充颜色，透明度稍高
hold on
stem(1:96, A_GZ2016_2021yl(1:96)./2^1, 'Color', color_model0, 'LineWidth', 1, 'Marker', 'none') % 深紫色线条，增大线条宽度
plot(1:96, MOI(13:end), 'o', 'MarkerSize', 5, 'MarkerEdgeColor', color_moi_bi, 'MarkerFaceColor', color_moi_bi, 'LineWidth', 1.2) % 加粗 MOI/BI 的线条
hold off
ylabel('\fontsize{15}Adult mosquitoes'); 
xlabel('\fontsize{15}2016.01-2019.12');
xlim([0, 97]); ylim([0 1000])
title('\fontsize{15}(a)')

subplot(223) 
fill([x fliplr(x)], [semimonth(13:end,3)'./15.5^3 fliplr(y_lower)], color_fill, 'FaceAlpha', 0.5, 'EdgeColor', 'none')
hold on
stem(1:96, L_GZ2016_2021yl(1:96)./2, 'x-', 'Color', color_model0, 'LineWidth', 1,'Marker','none')
plot(1:96, BI(13:end), 'o', 'MarkerSize', 5, 'MarkerEdgeColor', color_moi_bi, 'MarkerFaceColor', color_moi_bi, 'LineWidth', 1.2) % 加粗 MOI/BI 的线条
hold off
legend({'Model 1', 'Model 0', 'MOI/BI'}, 'Location', 'northwest')
ylabel('\fontsize{15}Larvae'); 
xlabel('\fontsize{15}2016.01-2019.12');
xlim([0,97]); ylim([0 1000])
title('\fontsize{15}(b)')



%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%
alpha=par1(17);lamda=par1(18);k_var=par1(19);k_fix=par1(20);
aa=alpha;bb=1/lamda;
p1=gaminv(0.025,aa,bb)% 131.1987
p2=gaminv(0.975,aa,bb)%212.8327
k=k_var.*(Ga(P,alpha,lamda))+k_fix;
[spk,indexpk]=sortrows([P,k],1);

subplot(2,2,[2,4])
f1 = [p1, p1, p2, p2];
f2 = [3.2, 5.2, 5.2, 3.2] * 10^4;
fill(f1, f2, [221,227,215]./255, 'FaceAlpha', 1, 'EdgeColor', 'none')
hold on
plot(spk(:,1), spk(:,2), 'Color', [53,128,69,255]./255, 'LineWidth', 2); 
xlim([0,450]); ylim([3.2 5.2] * 10^4)
title('\fontsize{15}(c)'); set(gca, 'FontSize', 16);
xlabel('\fontsize{15}Total weekly rainfall \it R_w(t) \rm /mm'); 
ylabel('\fontsize{15}Environmental carrying capacity \it k(t)')

text(p2-10, 50000, '\leftarrow  95% probability interval', 'Color', [0.5, 0, 0.5], 'FontSize', 16) 
text(p2+10, 49200, '(Optimum rainfall range)', 'Color', [0.5, 0, 0.5], 'FontSize', 15) 
text(p1, k_var * (Ga(p1, alpha, lamda)) + k_fix, '(131.2, 35813) ', 'Color', 'black', 'FontSize', 15, 'HorizontalAlignment', 'right')
text(p2, k_var * (Ga(p2, alpha, lamda)) + k_fix, ' (212.8, 34938)', 'Color', 'black', 'FontSize', 15)
text(-5, k_fix + 200, '\it k_{basis}', 'Color', [53,128,69,255]./255, 'FontSize', 13, 'HorizontalAlignment', 'right')
plot([p1 p2], [k_var * (Ga(p1, alpha, lamda)) + k_fix, k_var * (Ga(p2, alpha, lamda)) + k_fix], 'r*', 'MarkerSize', 7)
hold off
set(gcf, 'Position', [100, 100, 1500, 650])
