function [value,isterminal,direction]=event(t,y)
value=y(2)/(y(1)+y(2))-0.9;
isterminal=1;
direction=1;