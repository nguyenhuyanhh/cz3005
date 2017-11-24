% Load models
:- ['subway.pl'].

% Help message
help :- write('Enter start. to start the process.').

% Query for each type
query_bread :- 
    options(breads),nl,
    write('Please input your bread: '),nl,
    read(X),
    selected(X, breads),nl,
    assert(bread(X)).
query_main :-
    options(mains),nl,
    write('Please input your main: '),nl,
    read(X),
    selected(X, mains),nl,
    assert(main(X)).
query_cheese :-
    options(cheeses),nl,
    write('Please input your cheese: '),nl,
    read(X),
    selected(X, cheeses),nl,
    assert(cheese(X)).
query_veg_loop :-
    read(X),
    ( not(X == 0) ->
    selected(X, vegs),nl,
    assert(veg(X)),
    query_veg_loop ;
    true ).
query_veg :-
    options(vegs),nl,
    write('Please input your vegs (0 to end): '),nl,
    query_veg_loop.
query_sauce_loop :-
    read(X),
    ( not(X == 0) ->
    selected(X, sauces),nl,
    assert(sauce(X)),
    query_sauce_loop ;
    true ).
query_sauce :-
    options(sauces),nl,
    write('Please input your sauces (0 to end): '),nl,
    query_sauce_loop.
query_cookie_loop :-
    read(X),
    ( not(X == 0) ->
    selected(X, cookies),nl,
    assert(cookie(X)),
    query_cookie_loop ;
    true ).
query_cookie :-
    options(cookies),nl,
    write('Please input your cookies (0 to end): '),nl,
    query_cookie_loop.
query_addon_loop :-
    read(X),
    ( not(X == 0) ->
    selected(X, addons),nl,
    assert(addon(X)),
    query_addon_loop ;
    true ).
query_addon :-
    options(addons),nl,
    write('Please input your addons (0 to end): '),nl,
    query_addon_loop.

% Declare dynamic predicates to store results
:- dynamic bread/1, main/1, cheese/1, veg/1, sauce/1, cookie/1, addon/1.

% Display options.
choices(Bread, Main, Cheese, Vegs, Sauces, Cookies, Addons) :-
    bread(Bread),
    main(Main),
    cheese(Cheese),
    findall(X, veg(X), Vegs),
    findall(X, sauce(X), Sauces),
    findall(X, cookie(X), Cookies),
    findall(X, addon(X), Addons).
display :-
    choices(Bread, Main, Cheese, Vegs, Sauces, Cookies, Addons),
    write('Your choices:'),nl,
    write('Bread: '), write(Bread), nl,
    write('Main: '), write(Main), nl,
    write('Cheese: '), write(Cheese), nl,
    atomic_list_concat(Vegs, ',', Veg),
    write('Vegs: '), write(Veg), nl,
    atomic_list_concat(Sauces, ',', Sauce),
    write('Sauces: '), write(Sauce), nl,
    atomic_list_concat(Cookies, ',', Cookie),
    write('Cookies: '), write(Cookie), nl,
    atomic_list_concat(Addons, ',', Addon),
    write('Addons: '), write(Addon), nl.
    
% Start a run
start :-
    write('----------------------------------------------'),nl,
    write('--------------------SUBWAY--------------------'),nl,
    write('----------------------------------------------'),nl,
    exec,
    end.

% Meal options
meal_normal :-
    query_bread, query_main, query_cheese, query_veg,
    query_sauce, query_cookie, query_addon.
meal_veggie :-
    query_bread, query_veg,
    query_sauce, query_cookie, query_addon.
meal_vegan :-
    query_bread, query_veg,
    query_sauce, query_addon.
meal_value :-
    query_bread, query_main, query_cheese, query_veg,
    query_sauce.

% Execute a run
exec:-
    write('Please choose your meal type (one of normal, veggie, vegan, value)?'),nl,
    read(Meal),
    ( (Meal == veggie) -> meal_veggie ;
        (Meal == vegan) -> meal_vegan ;
            (Meal == value) -> meal_value ;
                meal_normal), % normal by default
    write('----------------------------------------------'),nl,
    write('----------------------------------------------'),nl,
    display.    

% Flush all choices from memory
flush :- retract(bread(_)), fail.
flush :- retract(main(_)), fail.
flush :- retract(cheese(_)), fail.
flush :- retract(veg(_)), fail.
flush :- retract(sauce(_)), fail.
flush :- retract(cookie(_)), fail.
flush :- retract(addon(_)), fail.

% End a run
end :-
    write('----------------------------------------------'),nl,
    write('------------------END-SUBWAY------------------'),nl,
    write('----------------------------------------------'),
    flush. % reset the system
