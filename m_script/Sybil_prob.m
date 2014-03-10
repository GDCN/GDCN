%
% Created by HalfLeif 2014-03-08
%
% Model for sybil attack on replication with "anonymous authentication"
% for each task.
% Calculates probability of JobOwner receiving R replica results that are
% both Identical and Untrue (ie incomplete or partly false in any respect).
% That is, what is the probability of JobOwner not realizing something has
% gone very wrong with the execution of one Task.
%
% Assumes identification takes fixed amount of time, d.
% Assumes sybil nodes are perfectly synchronized
%  

clear all

N = 996;   % Total number of nodes
B = 4;     % Total number of cooperative Sybil nodes on different cores;
            % Assumes all B run in parallel
k = 1;      % Avgerage time to truthfully complete one task
d = 0.01;   % Exact time for going through identification process
R = 3;      % Number of replicas

b = B/N;    % Ratio of Sybil nodes
p = d/k;    % Ratio identification/task execution time

% OBS! Måste använda ceiling:
% ex R = 3; B = 1000
% ska bli 1 cykel, inte 0.003 cykler
mceil = @(x) ceil(x);
%mceil = @(x) (x);

% OBS! Handlar inte om först till kvarn att ladda upp resultat
% utan först till kvarn att vinna JobOwners respekt

TimeForRFalse = d*mceil(R/B);
Lr = (N-B)/(d+k);

% How many truthful requests are sent in average per time interval
% of length TimeForRFalse
AverageTruthfulRequestsInInterval = Lr * TimeForRFalse

% This gives the same results as below and are more intuitive,
% the expression can be developed as below to get more general conclusions.
% 
% Calculated with Poisson distribution: f(0; Lr'), Lr' = Lr * TimeForRFalse
% 
%LogAllFalse1 = - Lr * TimeForRFalse
%AllFalse1 = exp(LogAllFalse1)

Freq = N*(1-b)*p/(p+1);
C = mceil(R/(N*b));

LogAllFalse2 = - Freq * C;
ProbAllFalse = exp(LogAllFalse2)

% Time wasted doing authentication
ComputerWasteTimePerTask = R*d + (R-1)*k;
ComputerWasteTimeComparedToBOINC = R*d;
% Actual efficiency
ComputationalEfficiency = k / (k + ComputerWasteTimePerTask)
ComputationalEfficiencyvsBOINC = k / (k + ComputerWasteTimeComparedToBOINC)