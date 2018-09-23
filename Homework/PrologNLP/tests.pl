% run this with run_test().

    % FOR NEXT YEAR ADD
    % [some,boy,that,runs,likes,some,girl

    
% parse and translate
pandt(Input,Output) :- parse(Input,Parsed),translate(Parsed,Output).

:- begin_tests(parse).
test(parse1) :-
	parse([some,boy,runs],statement(some(noun(boy)), verb(runs))).

test(parse1fail,[fail]) :-
	parse([some,boy],_).

test(parse2fail,[fail]) :-
	parse([runs],_).

test(parse3fail,[fail]) :-
	parse([some,runs,boy],_).

test(parse2) :-
	parse([some,girl,dances],statement(some(noun(girl)), verb(dances))).

test(parse3) :-
	parse([all,apples,dance],statement(all(noun(apples)), verb(dance))).

test(parse4) :-
	parse([some,boys,run],statement(some(noun(boys)), verb(run))).

test(plural1fail,[fail]) :-
	parse([some,boys,runs],_).

test(plural2fail,[fail]) :-
	parse([some,boy,run],_).

test(plural3fail,[fail]) :-
	parse([all,boy,runs],_).

test(parse5) :-
	parse([some,boy,likes,some,apple],statement(some(noun(boy)), verb(likes, some(noun(apple))))).

test(parse6) :-
	parse([all,girls,like,all,apples],statement(all(noun(girls)), verb(like, all(noun(apples))))).

test(parse7) :-
	parse([all,girls,that,like,all,apples,dance],statement(all(relcl(noun(girls), verb(like, all(noun(apples))))), verb(dance))).

test(parse8) :-
	parse([some,apples,that,like,some,boys,like,some,girls],statement(some(relcl(noun(apples), verb(like, some(noun(boys))))), verb(like, some(noun(girls))))).

test(parse9) :-
	parse([some,apples,that,dance,dance], statement(some(relcl(noun(apples), verb(dance))), verb(dance))).

test(parse4fail,[fail]) :-
	parse([some,apples,that,dance,dance,dance],_).

test(parse5fail,[fail]) :-
	parse([some,apples,that,dance,that,dance],_).

test(parse10) :-
	parse([some,apples,that,all,girls,respect,dance],statement(some(relcl(noun(apples), all(noun(girls)), verb(respect))), verb(dance))).

test(parse11) :-
	parse([some,apple,that,some,girl,respects,dances],statement(some(relcl(noun(apple), some(noun(girl)), verb(respects))), verb(dances))).

test(parse12) :-
	parse([all,boys,like,all,girls,that,like,all,boys],statement(all(noun(boys)), verb(like, all(relcl(noun(girls), verb(like, all(noun(boys)))))))).

test(parse13) :-
	parse([some,boy,likes,all,apples,that,some,boy,likes],statement(some(noun(boy)), verb(likes, all(relcl(noun(apples), some(noun(boy)), verb(likes)))))).


test(translate1) :-
	pandt([some,boy,runs],exists(1, boy(1)+run(1))).

test(translate2) :-
	pandt([all,boys,run],all(1, boy(1)=>run(1))).

test(translate3) :-
	pandt([some,girls,dance],exists(1, girl(1)+dance(1))).

test(translate4) :-
	pandt([all,boys,like,all,apples],all(1, boy(1)=>all(2, apple(2)=>like(1, 2)))).

test(translate5) :-
	pandt([some,boys,like,all,apples],exists(1, boy(1)+all(2, apple(2)=>like(1, 2)))).

test(translate6) :-
	pandt([all,boys,like,some,apples],all(1, boy(1)=>exists(2, apple(2)+like(1, 2)))).

test(translate7) :-
	pandt([some,girl,that,hates,some,apple,runs],exists(1, girl(1)+exists(2, apple(2)+hate(1, 2))+run(1))).

test(translate8) :-
	pandt([some,apple,that,respects,some,girl,likes,some,apple],exists(1, apple(1)+exists(2, girl(2)+respect(1, 2))+exists(2, apple(2)+like(1, 2)))).

test(translate9) :-
	pandt([all,apples,that,some,boy,likes,dance],all(1, (apple(1)+exists(2, boy(2)+like(2, 1)))=>dance(1))).

test(translate10) :-
	pandt([some,boy,likes,all,apples,that,some,boy,likes], exists(1, boy(1)+all(2, (apple(2)+exists(3, boy(3)+like(3, 2)))=>like(1, 2)))).

% thanks to Yuqi Zhou for this test case
       
test(translate11) :-
	pandt([some,boy,that,runs,likes,some,girl], exists(1,boy(1)+run(1)+exists(2,girl(2)+like(1,2)))).

       

:- end_tests(parse).


