% :- module(csse403nlp,[parse/2,translate/2]).
% we should use a module,really, but it wasn't obvious how to
% make it so the that the operator => displays correctly when it
% is in a module.

% this creates a new => operator to have the same priority as +
% seems to make it behave as you would expect.
:- op(200, xfx, =>).
=>(_,_).

% your code goes below
% the tests are in another file

parse(X,statement(NT,VT)).

translate(statement(NT,VT), Output).







