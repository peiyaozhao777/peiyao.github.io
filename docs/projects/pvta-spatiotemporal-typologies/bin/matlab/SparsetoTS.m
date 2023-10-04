function v = SparsetoTS(x)

v = [];
for i = 1:length(x)
    if ( x(i) > 0 )
        v = [v x(i)];
    else
        v = [v zeros(1,-floor(x(i)/1000))];
    end
end