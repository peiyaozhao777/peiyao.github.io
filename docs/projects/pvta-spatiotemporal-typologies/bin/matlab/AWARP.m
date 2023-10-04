function [d, D] = AWARP(x,y)
%x and y are run-length encoded series where negative integers represent
%length of runs of zeros

n = length(x);
m = length(y);
x(n+1) = 1;
y(m+1) = 1;
D(1:n+1,1:m+1) = inf;
D(1,1) = 0;



for i = 1:n
    for j = 1:m
        
        a1 = D(i,j) + (x(i)-y(j))^2;
        if( i > 1 && j > 1 )
        
            if( x(i) > 0 && y(j) > 0 )
                a1 = D(i,j) + (x(i)-y(j))^2;
            elseif ( x(i) < 0 && y(j) < 0 )
                a1 = D(i,j);
            elseif ( x(i) > 0 && y(j) < 0 )
                a1 = D(i,j) + x(i)^2 * (-y(j)); %Unocmment to make it an upperbound and work exactly for binary strings with runs of ones
               % a1 = D(i,j) + x(i)^2; %Unocmment to make it a lowerbound
            elseif ( x(i) < 0 && y(j) > 0 )
                a1 = D(i,j) + y(j)^2  * (-x(i));   %uncomment to make it an upperbound and work exactly for binary strings with runs of ones
               % a1 = D(i,j) + y(j)^2;   %uncomment to make it a lowerbound
            end
        end

        
            a2 = inf;
            if( x(i) > 0 && y(j) > 0 )
                a2 = D(i+1,j) + (x(i)-y(j))^2;
            elseif ( x(i) < 0 && y(j) < 0 )
                a2 = D(i+1,j);
            elseif ( x(i) < 0 && y(j) > 0 )
                a2 = D(i+1,j) + y(j)^2;
            elseif ( x(i) > 0 && y(j) < 0 )
                a2 = D(i+1,j) + x(i)^2 * (-y(j));
            end


        
            a3 = inf;
            if( x(i) > 0 && y(j) > 0 )
                a3 = D(i,j+1) + (x(i)-y(j))^2;
            elseif ( x(i) < 0 && y(j) < 0 )
                a3 = D(i,j+1);
            elseif ( x(i) > 0 && y(j) < 0 )
                a3 = D(i,j+1) + x(i)^2;
            elseif ( x(i) < 0 && y(j) > 0 )
                a3 = D(i,j+1) + y(j)^2 * (-x(i));
            end
                      
    
        D(i+1,j+1) = min([a1 a2 a3]);
        
   end
end


d = sqrt(D(n+1,m+1));
