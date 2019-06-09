function coeff = PolyFit(x,y,m)
% Will Kramlinger; 2/28/14
% PolyFit expands on Linear Regression and provides a fitting curve of the 
% general form y = (a_m)(x^m) + (a_m-1)(x^(m-1)) + ... + (a_1)(x) + a_0
% where n = number of data points & m < n-1.
% Input variables:
% x = A vector with the coordinates x of the data points.
% y = A vector with the coordinates y of the data points.
% m = The desired order of the fitting curve.
% Output variables:
% coeff = An array with the coefficients of the polynomial.
%         coeff(1) = a_0, coeff(2) = a_1, ...

if length(x) ~= length(y)
    disp('The number of elements in x must be the as in y.')
    coeff = 'DOES NOT COMPUTE, DOGGY!';
end
if m >= length(x)
    disp('M must be less than length(x) - 1')
    coeff = 'DOES NOT COMPUTE, DOGGY!';
end

V = zeros(m+1,m+1); 
for i = 1 :(m+1)
    for j = 1: (m+1)
        V(i,j) = sum(x.^(i + j - 2));
    end
end

S = zeros(m+1,1);
for k = 1: (m+1)
    S(k,1) = sum(x.^(k-1).*y);
end

coeff = (V\S);
disp('NOTE TO SELF: coeff(1) = a_0, coeff(2) = a_1, and so on.')

% Plotting Section
xfit = linspace(min(x),max(x),100);
yfit = zeros(1,100);
total = zeros(1,m+1);
for a = 1:100
    for b = 1: (m+1)
        total(b) = coeff((m+1) - b + 1) * xfit(a).^((m+1) - b);
    end
    yfit(a) = sum(total);    
end
plot(x,y,'green*'); hold on
plot(xfit,yfit,'r');
xlabel('x'); ylabel('y');
legend('Data','Fitting Curve')
hold off
% /Plotting Section
end

    
