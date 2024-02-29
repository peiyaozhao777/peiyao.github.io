function X = RealtoSparseTS(x)

c = 0;
k = 1;

X = [];
while x(k) ~= 0
    X(k) = x(k);
    k = k+1;
end

for i = k:length(x)
    if x(i) ~= 0
        
        if ( c > 0 )

                        
            X(k) = 0;
            k = k+1;

            X(k) = c;
            k = k+1;
        end
        
        X(k) = x(i);
        k = k+1;
        c = 0;
    else
      c = c + 1;  
    end
    
end

        if ( c > 0 )
            
                        
            X(k) = 0;
            k = k+1;

            
            X(k) = -c;
            k = k+1;
        end
        
end