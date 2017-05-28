-module(tree_tests).
-include_lib("eunit/include/eunit.hrl").
-include("tree.hrl").

create_new_tree_test() ->
    Expected = #tree{key = "yes, this is dog.",
		     value = "ring ring",
		     left = undefined,
		     right = undefined},
    Result = tree:init("yes, this is dog.", "ring ring"),
    ?assertEqual(Expected, Result).

insert_smaller_value_test() ->
    New_node = #tree{key = 50,
		     value = "some other value"},
    Expected = #tree{key = 100,
		     value = "some value",
		     right = undefined,
		     left = New_node},
    Root = tree:init(100, "some value"),
    Result = tree:insert(Root, 50, "some other value"),
    ?assertEqual(Expected, Result).

insert_greater_value_test() ->
    New_node = #tree{key = 200,
		     value = bye},
    Expected = #tree{key = 100,
		     value = hi,
		     right = New_node},
    Root = tree:init(100, hi),
    Result = tree:insert(Root, 200, bye),
    ?assertEqual(Expected, Result).

insert_equal_key_test() ->
    New_node = #tree{key = 100,
		     value = another_hundo},
    Expected = #tree{key = 100,
		     value = one_hundo,
		     left = New_node},
    Root = tree:init(100, one_hundo),
    Result = tree:insert(Root, 100, another_hundo),
    ?assertEqual(Expected, Result).

insert_a_smaller_and_an_even_smaller_key_test() ->
    Root = tree:init(3, "DREI"),
    Second_node = #tree{key = 1,
			value = "EINS"},
    First_node = #tree{key = 2,
		       value = "ZWEI",
		       left = Second_node},
    Expected = #tree{key = 3,
		     value = "DREI",
		     left = First_node},
    Pre_result = tree:insert(Root, 2, "ZWEI"),
    Result = tree:insert(Pre_result, 1, "EINS"),
    ?assertEqual(Expected, Result).
    
insert_multiple_values_test() ->
    Root = tree:init(0, 0),
    Values = [{1, 1}, {2, 2}, {3, 3}],
    Three_node = #tree{key = 3,
		       value = 3},
    Two_node = #tree{key = 2,
		     value = 2,
		     right = Three_node},
    One_node = #tree{key = 1,
		     value = 1,
		     right = Two_node},
    Expected = Root#tree{right = One_node},
    Result = tree:insert_multiple(Root, Values),
    ?assertEqual(Expected, Result).

balance_test() ->
    Root = tree:init(0, noll),
    Expected = #tree{key = 1,
		     value = ett,
		     left = #tree{key = 0, value = noll},
		     right = #tree{key = 2, value = tvao}},
    Unbalanced = tree:insert_multiple(Root, [{1, ett}, {2, tvao}]),
    Result = tree:balance(Unbalanced),
    ?assertEqual(Expected, Result).

to_list_test() ->
    Root = tree:init(3, san),
    Tree = tree:insert_multiple(Root, [{1, yi}, {4, si}]),
    Expected = [{3, san}, {1, yi}, {4, si}],
    Result = tree:to_list(Tree),
    ?assertEqual(Expected, Result).

balance_bigger_tree_test() ->
    Root = tree:init(1, jat),
    Unbalanced = tree:insert_multiple(Root, [{2, ji},
					     {3, san},
					     {4, sei},
					     {5, ng},
					     {6, luk}]),
    Expected = #tree{key = 4,
		     value = sei,
		     left = #tree{key = 2,
				  value = ji,
				  left = #tree{key = 1, value = jat},
				  right = #tree{key = 3, value = san}},
		     right = #tree{key = 6,
				   value = luk,
				   right = undefined,
				   left = #tree{key = 5, value = ng}}
		    },
    Result = tree:balance(Unbalanced),
    ?assertEqual(Expected, Result).

from_list_test() ->
    Input = [{1, ichi}, {2, ni}, {3, san}, {4, yon}, {5, go}, {6, roku}],
    Expected = #tree{key = 4,
		     value = yon,
		     left = #tree{key = 2,
				  value = ni,
				  left = #tree{key = 1, value = ichi},
				  right = #tree{key = 3, value = san}},
		     right = #tree{key = 6,
				   value = roku,
				   right = undefined,
				   left = #tree{key = 5, value = go}}
		    },
    Result = tree:from_list(Input),
    ?assertEqual(Expected, Result).
    
to_list_breadth_first_test() ->
    Tree = tree:from_list([{1, $a}, {2, $b},
			   {3, $c}, {4, $d},
			   {5, $e}, {6, $f},
			   {7, $g}]),
    Expected = [{4, $d}, {2, $b}, {6, $f}, {1, $a}, {3, $c}, {5, $e}, {7, $g}],
    Result = tree:to_list_breadth_first(Tree),
    ?assertEqual(Expected, Result).
    
get_test() ->
    Tree = tree:from_list([{a, "alpaca"},
			   {b, "boar"},
			   {c, "camel"},
			   {d, "dromedary"}]),
    Result = tree:get_value(d, Tree),
    Expected = "dromedary",
    ?assertEqual(Expected, Result).

get_when_undefined_test() ->
    Tree = tree:from_list([{a, "antimony"},
			   {b, "bohrium"}]),
    Result = tree:get_value(c, Tree),
    Expected = {error, not_defined},
    ?assertEqual(Expected, Result).
