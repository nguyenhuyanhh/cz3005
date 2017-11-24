% Load model
:- ['subway.pl'].

% Help message
help :- write('Enter start. to start the process.').

% Query looping for each type, error-checking and handle multiple values
query_bread_loop :-
    read(X),
    ( selected(X, breads) -> % if true then proceed
        nl, assert(bread(X)) ;
        write('Invalid option, try again!'),nl, % else try again 
        query_bread_loop ).
query_main_loop :-
    read(X),
    ( selected(X, mains) -> % if true then proceed
        nl, assert(main(X)) ;
        write('Invalid option, try again!'),nl, % else try again
        query_main_loop ).
query_cheese_loop :-
    read(X),
    ( selected(X, cheeses) -> % if true then proceed
        nl, assert(cheese(X)) ;
        write('Invalid option, try again!'),nl, % else try again 
        query_cheese_loop ).
query_veg_loop :-
    read(X),
    ( not(X == 0) -> % if input is 0, proceed to next list
        ( selected(X, vegs) -> % if true then proceed
            nl, assert(veg(X)) ;
            write('Invalid option, try again!'),nl ), % else try again         
        query_veg_loop ;
        true ).
query_sauce_loop :-
    read(X),
    ( not(X == 0) -> % if input is 0, proceed to next list
        ( selected(X, sauces) -> % if true then proceed
            nl, assert(sauce(X)) ;
            write('Invalid option, try again!'),nl ), % else try again
        query_sauce_loop ;
        true ).
query_cookie_loop :-
    read(X),
    ( not(X == 0) -> % if input is 0, proceed to next list
        ( selected(X, cookies) -> % if true then proceed
            nl, assert(cookie(X)) ;
            write('Invalid option, try again!'),nl ), % else try again
        query_cookie_loop ;
        true ).
query_addon_loop :-
    read(X),
    ( not(X == 0) -> % if input is 0, proceed to next list
        ( selected(X, addons) -> % if true then proceed
            nl, assert(addon(X)) ;
            write('Invalid option, try again!'),nl ), % else try again            
        query_addon_loop ;
        true ).

% Query for each type
query_bread :- options(breads),nl,write('Please input your bread: '),nl,
    query_bread_loop.
query_main :- options(mains),nl,write('Please input your main: '),nl,
    query_main_loop.
query_cheese :- options(cheeses),nl,write('Please input your cheese: '),nl,
    query_cheese_loop.
query_veg :- options(vegs),nl,write('Please input your vegs (0 to end): '),nl,
    query_veg_loop.
query_sauce :- options(sauces),nl,write('Please input your sauces (0 to end): '),nl,
    query_sauce_loop.
query_cookie :- options(cookies),nl,write('Please input your cookies (0 to end): '),nl,
    query_cookie_loop.
query_addon :- options(addons),nl,write('Please input your addons (0 to end): '),nl,
    query_addon_loop.

% Declare dynamic predicates to store results
:- dynamic meal/1, bread/1, main/1, cheese/1, veg/1, sauce/1, cookie/1, addon/1.

% Display selected options.
choices(Meal, Bread, Main, Cheese, Vegs, Sauces, Cookies, Addons) :-
    meal(Meal),
    bread(Bread),
    main(Main),
    cheese(Cheese),
    findall(X, veg(X), Vegs),
    findall(X, sauce(X), Sauces),
    findall(X, cookie(X), Cookies),
    findall(X, addon(X), Addons).
display :-
    choices(Meal, Bread, Main, Cheese, Vegs, Sauces, Cookies, Addons),
    write('Your meal type: '),write(Meal), nl,
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
    ( (Meal == veggie) -> meal_veggie, assert(meal(veggie)) ;
        (Meal == vegan) -> meal_vegan, assert(meal(vegan)) ;
            (Meal == value) -> meal_value, assert(meal(value)) ;
                meal_normal, assert(meal(normal)) ), % normal by default
    write('----------------------------------------------'),nl,
    write('----------------------------------------------'),nl,
    display.    

% Flush all choices from memory
flush :- retract(meal(_)), fail.
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
