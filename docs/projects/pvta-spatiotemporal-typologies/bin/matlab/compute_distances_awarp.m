% testing the user time series data (temporal analysis)
fptest1 = "../../results/pattern-analysis/temporal/one-week-hourly-aggregated.csv"; 
fptest2 = "../../results/pattern-analysis/temporal/one-week-1-min-aggregated.csv";
fptest3 = "../../results/pattern-analysis/temporal/one-week-5-mins-aggregated.csv";

% full data to use
fp1hr = "../../results/pattern-analysis/temporal/full-bytemark-dummies-hourly-aggregated.csv";

% testing the stop time series data (spatial analysis)
fptest4 = "../../results/pattern-analysis/spatial/full-apc-stop-alighting-dummies-hourly-aggregated.csv" % Boardings
fptest5 = "../../results/pattern-analysis/spatial/full-apc-stop-boarding-dummies-hourly-aggregated.csv" % Alightings

% Spatiotemporal analysis
% LATITUDE DATA
lat = "../../results/pattern-analysis/spatial/aggregated-time-series/full-bytemark-timeseries-table-Latitude-full.csv"
%LONGITUDE DATA 
lon = "../../results/pattern-analysis/spatial/aggregated-time-series/full-bytemark-timeseries-table-Longitude-full.csv"


tqdmss = 0

disp(tqdmss)

% tic
% computeDM(fptest1, 0, 1440);
% toc
% tic
% computeDM(fptest2, 0, 1440);
% toc
tic % looks like it computes elapsed time
computeDM(lat, 1, 144); % Run Awarp
toc

tic
computeDM(lon, 1, 144); % Run Awar
toc


function computeDM(filepath, constrained, window)
    disp('begin')
    tablename = split(filepath,'/');
    tablename = tablename(end);
    df = readmatrix(filepath);
    disp('read matrix ended')
    len = size(df, 1) - 1; % first row contains labels
    obs = size(df, 2); % first row contains labels

    dd = zeros(obs, obs);
    dd(1,:) = df(1,:); % set labels to first row
    dd(:,1) = dd(1,:);
    
    disp('end')
    tqdm = 0
    for i = 2:obs
        tqdm = tqdm + 1
        %disp(tqdm)
        x = df(:,i);
        x = x(2:end);
        notnanindexx = ~isnan(x);
        x = x(notnanindexx);
        x(1) = 1; x(len) = 1; 
        X = toSparseTS(x);    
        for j = 2:obs
            if j > i
                y = df(2:end,j);         
                notnanindexy = ~isnan(y);
                y = y(notnanindexy);
                %a requirement of the algorithm. First and last values must be
                %positive. This is a benign requirement.            
                y(1) = 1; y(len) = 1; 

                %This step converts the signal to encode runs of zeros in one number
                Y = toSparseTS(y);

                %Check for a rare empty sequence
                if isempty(X) || isempty(Y)
                    continue;
                end
                if constrained==1
                    d = AWARP(X,Y); %upper bounded
                    prefix = append('awarp-constrained-w', string(window),'-distance-matrix-');
                else
                    d = ConstrainedAWARP(X,Y,window);
                    prefix = 'awarp-distance-matrix-';
                end
                dd(i,j) = d;            
            end
        end
    end    
    distmatrixname = append('../../results/pattern-analysis/spatial/DTW-matrices/', prefix, tablename);
    writematrix(dd, distmatrixname)
end
