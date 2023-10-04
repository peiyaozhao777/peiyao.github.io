dd(1:100) = 0;
rr(1:100) = 0;



w = 200;    %length of the signals in timestamps

window = 40; %window of constraint

for i = 1:100%0
    
    
    %generate two random signals
    %First take all zeros
    x = zeros(1,w);
    y = zeros(1,w);
    
    %Choose some indices
    xi = ceil(rand(1,ceil(rand(1,1)*w/2))*w);
    yi = ceil(rand(1,ceil(rand(1,1)*w/2))*w);
    
    %insert some positive values in those indices
    x(xi) = ceil(rand(1,length(xi))*100); % Change the 100 to 1 to generate binary sequences. 
    y(yi) = ceil(rand(1,length(yi))*100);
    
    %a requirement of the algorithm. First and last values must be
    %positive. This is a benign requirement.
    x(1) = 1; x(w) = 1; 
    y(1) = 1; y(w) = 1; 
    
   

    %r = dtw(x,y,window);
    
    
    
    %Use mex to compile the dtw.cpp. You can use any matlab script for DTW
    r = dtw(x,y);

    %This step converts the signal to encode runs of zeros in one number
    X = toSparseTS(x);
    Y = toSparseTS(y);
    

    %Check for a rare empty sequence
    if isempty(X) || isempty(Y)
        continue;
    end
    
    
    d = AWARP(X,Y); %Look inside the code to change the upper or lower bounding properties
    
    
    %u = ConstrainedDiTW(X,Y,window);
    %l = DiTW(X,Y)
    %uu(i) = u;
    %ll(i) = l;

    

    dd(i) = d;
    rr(i) = r(1);
    
 end

[junk ind] = sort(rr);
plot(rr(ind));
hold on
plot(dd(ind),'r');