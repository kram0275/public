function [r,area] = monte_carlo_area(xpoly,ypoly,xscale,yscale,n)
% Will Kramlinger; 4/6/14
% The function uses the Monte Carlo area approximation method on a given
% user-supplied polygon.
% Input Variables:
% xpoly = array of x values for the polygon
% ypoly = array of y values for the polygon
% xscale = scaling factor for the x-axis
% yscale = scaling factor for the y-axis
% n = the desired number of random points used to check in/outside the 
%     area
% Output Variables:
% r = (number of points in polygon) / (total number of points)
% area = area of polygon [r * size of rectangle]

x = xscale .* rand(n,1);
y = yscale .* rand(n,1);
in = inpolygon(x,y,xpoly,ypoly);
plot(xpoly,ypoly,x(in),y(in),'r+',x(~in),y(~in),'bo');
xlabel('x [mi]'); ylabel('y [mi]'); 
title('Monte Carlo Area Calculation');

r = sum(in) / n;
area = r * xscale * yscale;

end