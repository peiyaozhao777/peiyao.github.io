function y = SequenceToSparse(x)
%x is a sequence of timestamps, no problem if trailing by zeros

i = 2;
y(1) = 1;
j = 2;
while i < length(x) && x(i) ~= 0
    y(j) = - (x(i)-x(i-1)-1); %Negative to represent a run of zeros
    y(j+1) = 1;
    j = j + 2;
    i = i+1;
end

