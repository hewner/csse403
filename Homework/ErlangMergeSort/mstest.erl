-module(mstest).
-export([]).
-include_lib("eunit/include/eunit.hrl").


t1_test() -> [1,2,3,4,5,6] = mergesort:sort([1,4,6,2,5,3],mergesort:spawn_workers(4),1).
t2_test() -> [1,2,3,4,5,6] = mergesort:sort([1,4,6,2,5,3],mergesort:spawn_workers(4),2).
t3_test() -> [1,2,3,4,5,6] = mergesort:sort([1,4,6,2,5,3],mergesort:spawn_workers(4),4).
t6_test() -> [1,4] = mergesort:sort([1,4],mergesort:spawn_workers(5),1).
t4_test() -> [1,2,3,4,5,6] = mergesort:sort([1,4,6,2,5,3],mergesort:spawn_workers(4),20).
t5_test() -> [1,2,3,4,5,6,7] = mergesort:sort([1,4,6,2,5,3,7],mergesort:spawn_workers(1),3).
