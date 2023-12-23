% WILCDIST   Distribution of the Wilcoxon signed-rank statistic
%
% The statistic is W = sum(Ri) = R1+R2+...+Rn,
% where n = sample size, and Ri represents the i'th nearest sample;
% Ri = i if that sample has positive signed rank (i.e. xi > m0)
% Ri = 0 otherwise (i.e. xi <= m0).
% Each possibility has probability 0.5 under the null hypothesis.
%
% We can use a recursive convolution to find the distribution.
% Let Wn be the Wilcoxon statistic from n data points (1...n).
% Then W1 has a simple distribution (0 or 1 with equal probability).
% W(n+1) is the convolution of Wn, and a variable that takes values
% 0 and n with equal probabilities.
%
% Returns:
%   p = probabilities of the values 0...max
%   w = the possible values 0...max (for convenience)
%
function [p,w] = wilcdist(n)

if n==0
    % No data points, always zero.
    p = [1];
    w = [0];
elseif n==1
    % One data point, either bigger or smaller than median.
    p = [0.5 0.5];
    w = [0 1];
else
    [pprev,wprev] = wilcdist(n-1);
    wmax       = n*(n+1)/2;
    w          = 0:wmax;
    p          = zeros(1, wmax+1);
    % Now Rn is either zero, so we get the previous distribution...
    p(1+wprev) = pprev * 0.5;
    % or Rn is n, so we get the previous distribution shifted by n.
    p(1+wprev+n) = p(1+wprev+n) + pprev * 0.5;
end
