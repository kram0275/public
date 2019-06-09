function [r,T] = heat_equation(a,b,Ta,Tb,n,C)
% Will Kramlinger; 5/8/14
% The function solves the heat equation ODE for a hollow tube:
%               r*T''(r) + T'(r) = C  [Eqn. 1]
% using the finite difference formula.  

% Input Variables:
% a = 1st value of r, radius at interior wall of tube
% b = last value of r, radius at outer wall of tube
% Ta = temperature at point a
% Tb = temperature at point b
% n = # of intervals ===> NOTE:(n+1) = # of data points
% C = a constant [see Eqn. 1]
% Output Variables:
% r = an array of discretization points
% T = temperature at each discretization point
clf

h = (b-a)/n;
r = linspace(a,b,n+1)';
T = zeros(n+1,1);

T(1) = Ta;
T(n+1) = Tb;

f = zeros(n-1,n-1);
g = C*(2*h^2).*ones(n-1,1);
g(1) = g(1) - (2*r(2) - h) * Ta;
g(n-1) = g(n-1) - (2*r(n) + h) * Tb;
for k = 2:n
    f(k-1,k-1) = -4 * r(k);
    f(k-1,k) = 2*r(k) + h;
    f(k,k-1) = 2*r(k+1) - h;
end
f(n,:) = []; % f matrix grew too big on iterations.
f(:,n) = []; % Pragmatic way of dealing with it.
T_interior = f\g;
T(2:n) = T_interior;
% Begin plotting section
plot(r,T); 
title('Temperature profile, T(r)'); 
xlabel('r [cm]'); ylabel('T [°C]');
% End plotting section

% ACTUAL SOLUTION for C = -500, via Wolfram Alpha
x = linspace(a,b,100);
y = -500*x + 538.809*log(x) + 1100;
hold on
plot(x,y,'g'); hold off
legend('Numerical Solution','Actual Solution');
end