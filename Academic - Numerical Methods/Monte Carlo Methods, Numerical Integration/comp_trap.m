function I = comp_trap(x,y)
% Will Kramlinger; 4/6/14
% The function takes x and y arrays, representing a single variable
% function, and calculates an integral using the composite trapezoidal
% method.
% Input Variables:
% x = an array representing the independent variable
% y = an array representing the dependent variable
% Output Variables:
% I = the resulting integral (scalar value)

if length(x) ~= length(y)
    error('x and y must the same length, ya goon!')
else
    n = length(x) - 1; % n = number of intervals
end

table = zeros(n,1);
for k = 1:n
    table(k) = .5 * (y(k) + y(k+1))*(x(k+1) - x(k));
end
I = sum(table);
end