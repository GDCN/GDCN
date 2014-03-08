
clear all

N = 2000;
B = 1990;
k = 1;
d = 0.15;
R = 3;

b = B/N;
p = d/k;

% OBS! Måste använda ceiling:
% ex R = 3; B = 1000
% ska bli 1 cykel, inte 0.003 cykler
mceil = @(x) ceil(x);
%mceil = @(x) (x);

% OBS! Handlar inte om först till kvarn att ladda upp resultat
% utan först till kvarn att vinna JobOwners respekt

TimeForRFalse = d*mceil(R/B);
Lr = (N-B)/(d+k);

%LogAllFalse1 = - Lr * TimeForRFalse
%AllFalse1 = exp(LogAllFalse1)

Freq = N*(1-b)*p/(p+1);
C = mceil(R/(N*b));

LogAllFalse2 = - Freq * C
AllFalse2 = exp(LogAllFalse2)