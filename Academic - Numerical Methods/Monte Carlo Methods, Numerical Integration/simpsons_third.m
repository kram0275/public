function I = simpsons_third(x,y)
% Will Kramlinger; 4/6/14
% The function takes x and y arrays, representing a single variable
% function, and calculates an integral using the composite Simpson's 1/3
% Method.
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

if mod(n,2) ~= 0
    error('You do not have an even number of equally spaced subintervals, 
	   ya idiot!')
else
    h = (x(end) - x(1)) / n; % h = step size between consecutive x values
end
% Beginning of integral calculations
sum1 = zeros(length(x), 1);
for k = 2:2:n
   sum1(k) = y(k);
end
sum1 = 4*sum(sum1);

sum2 = zeros(length(x),1);
for l = 3:2:(n-1)
    sum2(l) = y(l);
end
sum2 = 2*sum(sum2);

I = h/3 * (y(1) + sum1 + sum2 + y(end));
end