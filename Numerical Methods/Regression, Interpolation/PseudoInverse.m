function [a1,a0] = PseudoInverse(x,y)
% Will Kramlinger; 2/28/14
% The function calculates the coefficients a1 and a0 of the linear equation
% y = a1*x + a0 that best fits n data points.
% Input variables:
% x = A vector with the coordinates x of the data points.
% y = A vector with the coordinates y of the data points.
% Output variables:
% a1 = The coefficient a1.
% a0 = The coefficient a0.

nx = length(x);
ny = length(y);
if nx ~= ny
    error('The number of elements in x must be the as in y.')
    a1 = 'Error';
    a0 = 'Error';
else
    xmod = ones(nx,2);
    xmod(1:nx,1) = x';
    coeff = (xmod'*xmod)\xmod'*y';
    a1 = coeff(1,1);
    a0 = coeff(2,1);
    fprintf('The fit equation is y = %4.4fx + %4.4f.',a1,a0)
        plot(x,y,'green*'); hold on
        plot(x,a1*x+a0,'r');
        xlabel('x'); ylabel('y');           
        legend('Data','Fit Line', 'Location', 'best'); hold off 
end
