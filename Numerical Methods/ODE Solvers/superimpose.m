function graph = superimpose(a,b,h,yINI)
% Will Kramlinger; 4/17/14
% The function plots the solutions of a differential equation, in this
% case the equation HW7, using previously written forward Euler,
% modified Euler, and RK4 functions.
% Input variables:
% a = The first value of x.
% b = The last value of x.
% h = Step size.
% yINI = The value of the solution y at the first point (initial value).
% Output variables:
% graph = instructs user that this variable is irrelevant and to look
% at the graph
[xFE,yFE] = odeEULER(@HW7,a,b,h,yINI);
plot(xFE,yFE,'--');
hold on
[xMod,yMod] = odeModEuler(@HW7,a,b,h,yINI);
plot(xMod,yMod,'g:s');
[xRK4,yRK4] = odeRK4(@HW7,a,b,h,yINI);
plot(xRK4,yRK4,'r:p');
title('Comparison of ODE Solving Methods');
xlabel('t'); ylabel('N');
legend('ForwEuler','ModEuler','RK4','Location','best');
graph = 'Look at the graph, doggy.';
hold off
end