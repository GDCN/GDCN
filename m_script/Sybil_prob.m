
clear all

N = 2000;
B = 1000;
k = 1;
d = 0.15;
R = 3;

b = B/N;
p = d/k;

% Extremt känslig för Ceiling!!! 0.67 -> 2.2e-57 ...
%mceil = @(x) ceil(x);
mceil = @(x) (x);

TimeForRFalse = d*mceil(R/B);
Lr = (N-B)/(d+k);

%LogAllFalse1 = - Lr * TimeForRFalse
%AllFalse1 = exp(LogAllFalse1)

Freq = N*(1-b)*p/(p+1);
C = mceil(R/(N*b));

LogAllFalse2 = - Freq * C
AllFalse2 = exp(LogAllFalse2)