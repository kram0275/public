function [x,y] = odeRK4(ODE,a,b,h,yINI)
% Will Kramlinger; 4/17/14
% Code appropriated from textbook.
% odeRK4 solves a first order initial value ODE using Runge-Kutta fourth
% order method.
% Input variables:
  % ODE Name for the function that calculates dy/dx.
% a The first value of x.
% b The last value of x.
% h Step size.
% yINI The value of the solution y at the first point (initial value).
% Output variables:
  % x A vector with the x coordinate of the solution points.
% y A vector with the y coordinate of the solution points.
x(1) = a; y(1) = yINI;
N = (b - a)/h;
for i = 1:N
x(i+1) = x(i) + h;
K1 = ODE(x(i),y(i));
xhalf = x(i) + h/2;
yK1 = y(i) + K1*h/2;
K2 = ODE(xhalf,yK1);
yK2 = y(i) + K2*h/2;
K3 = ODE(xhalf,yK2);
yK3 = y(i) + K3*h;
K4 = ODE(x(i+1),yK3);
y(i+1) = y(i) + (K1 + 2*K2 + 2*K3 + K4)*h/6;
end
% plot(x,y);
end