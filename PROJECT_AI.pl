
% --------------------
% Destination facts grouped by interest
% destination(City, "Display Name", InterestCategory, LocalDistanceKmFromCityCenter)
% --------------------

% ---- Lifestyle ----

destination(colombo, lotus_tower, lifestyle, 2).
destination(colombo, one_galle_face_mall, lifestyle, 6).
destination(galle, galle_fort, lifestyle, 1).
destination(nuwara_eliya, gregory_lake, lifestyle, 2).
destination(nuwara_eliya, victoria_park,lifestyle, 3).
destination(kandy, kandy_lake ,lifestyle, 2).


% ---- Religious ----
destination(colombo, gangaramaya_temple, religious, 3).
destination(kandy, temple_of_the_tooth, religious, 2).
destination(anuradhapura, sri_maha_bodhi, religious, 3).
destination(anuradhapura, ruwanwelisaya, religious, 4).
destination(dambulla, dambulla_golden_temple, religious, 5).
destination(jaffna, nallur_kandaswamy_temple, religious, 3).
destination(polonnaruwa, gal_vihara, religious, 4).
destination(trincomalee, koneswaram_temple, religious, 3).


% ---- Nature ----
destination(nuwara_eliya, horton_plains, nature, 32).
destination(ella, little_adams_peak, nature, 3).
destination(sigiriya, pidurangala_rock, nature, 4).
destination(polonnaruwa, parakrama_samudraya, nature, 6).
destination(riverston, riverston_peak,nature, 15).
destination(nuwara_eliya, hakgala_botanical_garden,nature, 8).
destination(kandy, peradeniya_botanical_garden,nature, 7).
destination(pinnawala, pinnawala_elephant_orphanage, nature, 85).


% ---- Beach ----
destination(galle, unawatuna_beach, beach, 6).
destination(matara, mirissa_beach, beach, 4).
destination(trincomalee, nilaveli_beach, beach, 12).
destination(batticaloa, pasikudah_beach, beach, 10).
destination(negombo, negombo_beach , beach, 3).
destination(arugam_bay, arugam_bay_beach,beach, 5).


% ---- Food ----
destination(colombo, pettah_Market, food, 4).
destination(kandy, kandy_central_market, food, 2).
destination(jaffna, nallur_street_food,food, 3).
destination(galle, galle_fish_market, food, 2).

% --------------------
% Roads (inter-city distances in km)
% --------------------
road(colombo, negombo, 38).
road(colombo, bentota, 85).
road(colombo, kandy, 115).
road(colombo, galle, 126).
road(negombo, kandy, 100).
road(negombo, anuradhapura, 165).
road(kandy, sigiriya, 90).
road(kandy, nuwara_eliya, 75).
road(kandy, polonnaruwa, 137).
road(galle, bentota, 70).
road(anuradhapura, jaffna, 200).
road(anuradhapura, sigiriya, 75).
road(anuradhapura, polonnaruwa, 105).
road(anuradhapura, trincomalee, 140).
road(sigiriya, polonnaruwa, 60).
road(sigiriya, trincomalee, 90).
road(polonnaruwa, trincomalee, 110).
road(nuwara_eliya, ella, 55).
road(ella, galle, 205).
road(ella, colombo, 205).
road(galle, matara, 35).
road(matara, mirissa, 12).
road(batticaloa, trincomalee, 160).


% ----------------Destination Distances------------------
% colombo 
road(colombo,lotus_tower,3.7).
road(lotus_tower, gangaramaya_temple,4).
road(colombo,gangaramaya_temple,2).
road(lotus_tower, galle_face_green,3).
road(lotus_tower,one_galle_face_mall,3).
road(one_galle_face_mall,colombo,2).
road(lotus_tower,one_galle_face_mall,3).
road(colombo,pettah_market,4).
road(colombo,one_galle_face_mall,6).
road(negombo,negombo_beach,4).
road(negombo_beach,colombo,40.3).

% kandy
road(kandy,kandy_lake,2).
road(kandy,peradeniya_botanical_garden,6).
road(kandy,temple_of_the_tooth,1).
road(kandy,kandy_central_market,4).

% ella
road(ella,little_adams_peak,5).

% anuradhapura
road(anuradhapura,sri_maha_bodhi,2).
road(anuradhapura,ruwanwelisaya,3).

% polonnaruwa
road(polonnaruwa,gal_vihara,6).
road(polonnaruwa,parakrama_samudraya,8).

% trincomalee
road(trincomalee,koneswaram_temple,5).
road(trincomalee,nilaveli_beach,15).

% sigiriya
road(sigiriya,pidurangala_rock,6).

% batticaloa
road(batticaloa,pasikudah_beach,15).

% jaffna
road(jaffna,nallur_kandaswamy_temple,6).
road(jaffna,nallur_street_food,8).

% matara
road(matara,mirissa_beach,8).

% arugam_bay
road(arugam_bay,arugam_bay_beach,8).

% pinnawala
road(pinnawala,pinnawala_elephant_orphanage,115).

% dambulla
road(dambulla,dambulla_golden_temple,10).

% riverston
road(riverston,riverston_peak,18).

% galle
road(galle,gall_fort,3).
road(galle_fort,unawatuna_beach,6).
road(galle,galle_fish_market,2).
road(unawatuna_beach,galle_fish_market,4).

% nuwara_eliya
road(nuwara_eliya,gregory_lake,3).
road(nuwara_eliya,victoria_park,4).
road(nuwara_eliya,hakgala_botanical_garden,10).
road(nuwara_eliya,horton_plains,25).



% Travel modes and average speeds (km/h)
mode_speed(car, 45).
mode_speed(bus, 35).
mode_speed(train, 50).
mode_speed(tuk, 30).
mode_speed(walk, 5).


% make bidirectional
connected(A,B,W) :- road(A,B,W).
connected(A,B,W) :- road(B,A,W).

% --- Heuristic values (straight-line estimates to goal g) ---
h(colombo, 59).
h(negombo, 79).
h(bentota, 69).
h(kandy, 81).
h(galle, 23).
h(kandy, 105).
h(anuradhapura, 150).
h(sigiriya, 140).
h(nuwara_eliya, 100).
h(polonnaruwa, 170).
h(jaffna, 400).
h(trincomalee, 335).
h(ella, 170).
h(matara, 30).
h(kakirawa, 50).
h(mirissa, 9).

%heuristic for destination
h(kandy_lake,10). h(peradeniya_botanical_garden,6). h(temple_of_the_tooth,20). h(kandy_central_market,0).
h(lotus_tower,3.7). h(gangaramaya_temple,4). h(gangaramaya_temple,2). h(galle_face_green,3).
h(one_galle_face_mall,3). h(pettah_market,5). h(negombo_beach,4). h(little_adams_peak,5). 
h(sri_maha_bodhi,2). h(ruwanwelisaya,3). h(gal_vihara,6). h(parakrama_samudraya,8). 
h(koneswaram_temple,5). h(nilaveli_beach,15). h(pidurangala_rock,6). h(pasikudah_beach,15).
h(nallur_kandaswamy_temple,6). h(nallur_street_food,8). h(mirissa_beach,8). h(arugam_bay_beach,8).
h(arugam_bay_beach,8). h(pinnawala_elephant_orphanage,115). h(dambulla_golden_temple,10).
h(riverston_peak,18). h(gall_fort,3). h(unawatuna_beach,6). h(galle_fish_market,2).
h(gregory_lake,3). h(victoria_park,4). h(hakgala_botanical_garden,10). h(horton_plains,25).

% --- A* Search ---
astar(Start, Goal, Path, Cost) :-
    h(Start, H0),
    astar_search([node(Start, [Start], 0, H0)], Goal, RevPath, Cost),
    reverse(RevPath, Path).

% If we reach the goal, succeed and cut (stop searching further)
astar_search([node(State, Path, G, _)|_], State, Path, G) :- !.

astar_search([node(State, Path, G, _)|Rest], Goal, FinalPath, Cost) :-
    findall(node(Next, [Next|Path], G1, F1),
            ( connected(State, Next, StepCost),
              \+ member(Next, Path),
              G1 is G + StepCost,
              h(Next, H),
              F1 is G1 + H ),
            Children),
    append(Rest, Children, OpenList),
    sort(4, @=<, OpenList, Sorted),   % sort by F value
    astar_search(Sorted, Goal, FinalPath, Cost).

% shortest no of edges (bfs)
bfs(Start, Goal, Path) :-
    bfs_queue([[Start]], Goal, Path).

bfs_queue([[Goal|Rest]|_], Goal, Path) :-
    reverse([Goal|Rest], Path).
bfs_queue([[Current|Rest]|Other], Goal, Path) :-
    findall([Next,Current|Rest],
            (connected(Current, Next, _),
             \+ member(Next, [Current|Rest])),
            NewPaths),
    append(Other, NewPaths, Updated),
    bfs_queue(Updated, Goal, Path).

% dfs
dfs(Start, Goal, Path) :-
    dfs_stack([[Start]], Goal, Path).

dfs_stack([[Goal|Rest]|_], Goal, Path) :-
    reverse([Goal|Rest], Path).
dfs_stack([[Current|Rest]|Other], Goal, Path) :-
    findall([Next,Current|Rest],
            (connected(Current, Next, _),
             \+ member(Next, [Current|Rest])),
            NewPaths),
    % push to FRONT to behave like a stack (LIFO)
    append(NewPaths, Other, Updated),
    dfs_stack(Updated, Goal, Path).

% sum distances along a path (path cost)
path_cost([_], 0).
path_cost([A,B|T], Cost) :-
    connected(A,B,W),
    path_cost([B|T], Rest),
    Cost is W + Rest.

% collect all paths
all_bfs_paths(Start, Goal, Paths) :-
    findall(Path, bfs(Start, Goal, Path), Paths).

all_dfs_paths(Start, Goal, Paths) :-
    findall(Path, dfs(Start, Goal, Path), Paths).

% find shortest path by distance
shortest_path(Paths, ShortestPath, MinCost) :-
    map_list_to_pairs(path_cost, Paths, Pairs),   % [(Cost,Path),...]
    keysort(Pairs, [MinCost-ShortestPath|_]).     % take the smallest cost

% convenience predicates
shortest_bfs_path(Start, Goal, Path, Cost) :-
    all_bfs_paths(Start, Goal, Paths),
    shortest_path(Paths, Path, Cost),
	write('Path :'), write(Path), nl,
	write('Cost :'), write(Cost),nl.

shortest_dfs_path(Start, Goal, Path, Cost) :-
    all_dfs_paths(Start, Goal, Paths),
    shortest_path(Paths, Path, Cost).
	
recommend(Intrest) :-
    destination(City, Destination, Intrest, Distance),
    format(' City: ~w~n Destination: ~w~n Distance: ~w km~n~n', 
           [City, Destination, Distance]),
    fail.
recommend(_).
	
start:-
    nl,
    write('============================================================='), nl,
    write('        SMART TOURIST ASSISTANT - Mini Expert System         '), nl,
    write('============================================================='), nl, nl,

    write(' We will help you find the perfect travel destination!'), nl, nl,

    write(' Please answer the following questions: '), nl, nl,
	
	write(' What is your intrest? (enter one from these)'),nl,
	write(' lifestyle | religious | nature | beach | food'),nl,nl,
	read(Intrest), nl, nl,
	
	write(' ---- Recommending The perfect City To You! ----'), nl,nl,
	recommend(Intrest), nl,

	
	write('Enter the DESTINATION PLACE NAME YOU NEED TO VISIT...'),nl,nl,
	read(Goal),nl,
	
	write('Enter your current city from the given list:'),nl,nl,
	write('colombo    | negombo    | kandy       | galle | anuradhapura | sigiriya | batticaloa'), nl,
	write('polonnaruwa| trincomalee| nuwara_eliya| ella  | matara       | mirissa'), nl, nl,

	read(Start),nl,
	
	write('Shortest path to your destination '), write(Goal), write(' is :'),nl,
	shortest_bfs_path(Start,Goal,_,_), nl,

	
    write(' _______BEST ROUTE FOUND !______ '), nl,
    (   astar(Start, Goal, Path, Cost) ->  
    write('Best  Path     : '), write(Path), nl,
    write('Total distance : '), write(Cost), write(' km '), nl ;
    write('sorry , a path could not be found between '), write(Start), write(' and '), write(Goal), nl).
	