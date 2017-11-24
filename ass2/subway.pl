% Display all options in a list
options_list([]). % empty list
options_list([H]) :- write(H), write('.'). % final list item
options_list([H|T]) :- write(H), write(', '), options_list(T), !. % normal list
options(breads):- breads(L), write('breads = '), options_list(L).
options(mains):- mains(L), write('mains = '), options_list(L).
options(cheeses):- cheeses(L), write('cheeses = '), options_list(L).
options(vegs):- vegs(L), write('vegs = '), options_list(L).
options(sauces):- sauces(L), write('sauces = '), options_list(L).
options(cookies):- cookies(L), write('cookies = '), options_list(L).
options(addons):- addons(L), write('addons = '), options_list(L).

% Select an option
selected(X, breads) :- breads(L), member(X, L), write('bread = '), write(X), !.
selected(X, mains) :- mains(L), member(X, L), write('main = '), write(X), !.
selected(X, cheeses) :- cheeses(L), member(X, L), write('cheese = '), write(X), !.
selected(X, vegs) :- vegs(L), member(X, L), write('veg = '), write(X), !.
selected(X, sauces) :- sauces(L), member(X, L), write('sauce = '), write(X), !.
selected(X, cookies) :- cookies(L), member(X, L), write('cookie = '), write(X), !.
selected(X, addons) :- addons(L), member(X, L), write('addon = '), write(X), !.

% Lists of options
breads([italian, heartyitalian, wheat, honeyoat, wholegrain, parmesan, flatbread, wrap, salad]).
mains([ham, turkey, coldcut, bbqchicken, tuna, eggmayo, meatball]).
cheeses([american, monterrey, none]).
vegs([lettuce, tomato, cucumber, capsicum, onion, jalapeno, pickle, avocado]).
sauces([bbq, honeymustard, mustard, sweetonion, redwine, mayonnaise, chipottle, ranch, vinegar]).
cookies([chocchip, doublechoc, walnut, macnut, raspberry, brownie]).
addons([drink, soup, chips]).