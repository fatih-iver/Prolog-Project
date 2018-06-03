%2016400264

% Calculates sum of elements in a list either integer or real
sum([], 0). % Base Condition
sum([H|T], S):- sum(T, SS), S is H + SS. % Recursive Part

% Check whether an element is in a list
contains([H|_], H). % Base Conditon
contains([_|T], X):- contains(T, X). % Recursive Part

% Check whether list L excludes X
excludes(L, X):- \+ contains(L, X).

% Concatenate two lists
append([],X,X). % Base Condition                         
append([X|Y],Z,[X|W]) :- append(Y,Z,W). % Recursive Condition

% Find size of a list 
size([],0). % Base Condition
size([_|T], S):- size(T, SR), S is SR + 1. % Recursive Condition

% Find all teams by using built-in function findall
findallteam(L):- findall(Team,team(Team,_),L).

% Get number of teams in the league by calculating size of result of findall
numberofteams(N):- findallteam(L), size(L, N).

% Handmake implementation to find all teams in the league without using built in findall
allTeams(L, N):- getallteams(L, [], N).
% allteams(L, N):- findall(Team,team(Team,_),L), length(L, N).

% Get all teams in the league without using built-in findall
getallteams([H|T], I, N):- team(H, _), excludes(I, H), append([H], I, Z), getallteams(T, Z, NS), N is NS + 1.
getallteams([], I, 0):- numberofteams(N), size(I, S), N =:= S.

% Get total score of a team by calculating its goals in the home and away matches
scored(T, W, S):- scoredHome(T, W, SH), scoredAway(T, W, SA), S is SH + SA.
scoredHome(T, W, SH):- findall(S, (match(UW,T,S,_,_), W>=UW), SL), sum(SL, SH).
scoredAway(T, W, SA):- findall(S, (match(UW,_,_,T,S), W>=UW), SL), sum(SL, SA).

% Get total concedes of a team by calculating its concedes in the home and away matches
conceded(T, W, C):- concededHome(T, W, CH), concededAway(T, W, CA), C is CH + CA.
concededHome(T, W, CH):- findall(C, (match(UW,T,_,_,C), W>=UW), CL), sum(CL, CH).
concededAway(T, W, CA):- findall(C, (match(UW,_,C,T,_), W>=UW), CL), sum(CL, CA).

% Get average of a team by substracting scores from concedes
average(T, W, A):- scored(T, W, S), conceded(T, W, C), A is S - C.

% Get average for the given team, by using previous average function
getAverages([], _, []).
getAverages([H|T], W , [AH|AT]):- average(H, W, AH), getAverages(T, W, AT).

% Get all teams and find their corresponding averages for sorting purposes
getAllAverages(L, W, A):- findallteam(L), getAverages(L, W, A).

% Get list of defeated teams by a team W, by looking its matches in home and away 
wins(T, W, L, N):- winsHome(T, W, WH, NH), winsAway(T, W, WA, NA), append(WH, WA, L), N is NH + NA.
winsHome(T, W, L, N):- findall(AT, (match(UW,T,S,AT,C), W>=UW, S>C), L), size(L, N).
winsAway(T, W, L, N):- findall(HT, (match(UW,HT,C,T,S), W>=UW, S>C), L), size(L, N).

% Get list of teams that defeat a team W, by looking its matches in home and away 
losses(T, W, L, N):- lossesHome(T, W, LH, NH), lossesAway(T, W, LA, NA), append(LH, LA, L), N is NH + NA.
lossesHome(T, W, L, N):- findall(AT, (match(UW,T,S,AT,C), W>=UW, C>S), L), size(L, N).
lossesAway(T, W, L, N):- findall(HT, (match(UW,HT,C,T,S), W>=UW, C>S), L), size(L, N).

% Get list of teams that draws with a team W, by looking its average
draws(T, W, L, N):- drawsHome(T, W, DH, NH), drawsAway(T, W, DA, NA), append(DH, DA, L), N is NH + NA.
drawsHome(T, W, L, N):- findall(AT, (match(UW,T,S,AT,C), W>=UW, S=:=C), L), size(L, N).
drawsAway(T, W, L, N):- findall(HT, (match(UW,HT,C,T,S), W>=UW, C=:=S), L), size(L, N).

% Get all averages sorted by using built-in sort and reverse functions
getAveragesSorted(W, AS):- getAllAverages(_, W, A), sort(A, ASR), reverse(ASR, AS).

% Make pairs teams with their averages for sorting purposes
teamAveragePairs(W, P):- getAllAverages(L, W, A), pairs_keys_values(P, A, L).

% Sort pairs by using built-in function keysort and reverse, and pairs_values
order(L, W):- teamAveragePairs(W, P), keysort(P, RL), reverse(RL, IL), pairs_values(IL, L).

% Get top three teams after sorting
topthree([F, S, T], W):- order([F, S, T | _], W).