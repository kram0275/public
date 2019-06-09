function coeff = Interpolate(x,y)
% Will Kramlinger; 2/28/14
% Interpolate performs standard interpolation and provides a fitting curve 
% of the general form y = (a_m)(x^m) + (a_m-1)(x^(m-1)) + ... 
%                          + (a_1)(x) + a_0
% where n = number of data points & m < n-1.
% Input variables:
% x = A row vector with the coordinates x of the data points.
% y = A row vector with the coordinates y of the data points.
% % Output variables:
% coeff = An array with the coefficients of the polynomial.
%         coeff(1) = a_0, coeff(2) = a_1, ...

if length(x) ~= length(y)
    disp('The number of elements in x must be the as in y.')
    coeff = 'DOES NOT COMPUTE, DOGGY!';
else
    n = length(x);
end

vdm = zeros(n,n);
for k = 1:n
    vdm(1:n,k) = x'.^(k-1);
end

coeff = vdm\y';
disp('NOTE TO SELF: coeff(1) = a_0, coeff(2) = a_1, and so on.')

% Plotting Section
xfit = linspace(min(x),max(x),100);
yfit = zeros(1,100);
total = zeros(1,n);
for a = 1:100
    for b = 1:n
        total(b) = coeff(n - b + 1) * xfit(a).^(n - b);
    end
    yfit(a) = sum(total);    
end
plot(x,y,'green*'); hold on
plot(xfit,yfit,'r');
xlabel('x'); ylabel('y');
legend('Data','Interpolation')
hold off
% /Plotting Section
end