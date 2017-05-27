-module(tree_tests).
-include_lib("eunit/include/eunit.hrl").
-include("tree.hrl").

create_new_tree_test() ->
    Expected = #tree{value = "yes, this is dog.",
		     left = undefined,
		     right = undefined},
    Result = tree:init("yes, this is dog."),
    ?assertEqual(Expected, Result).

insert_smaller_value_test() ->
    New_node = #tree{value = 50},
    Expected = #tree{value = 100,
		     right = undefined,
		     left = New_node},
    Root = tree:init(100),
    Result = tree:insert(Root, 50),
    ?assertEqual(Expected, Result).

insert_greater_value_test() ->
    New_node = #tree{value = 200},
    Expected = #tree{value = 100,
		     right = New_node},
    Root = tree:init(100),
    Result = tree:insert(Root, 200),
    ?assertEqual(Expected, Result).

insert_equal_value_test() ->
    New_node = #tree{value = 100},
    Expected = #tree{value = 100,
		     left = New_node},
    Root = tree:init(100),
    Result = tree:insert(Root, 100),
    ?assertEqual(Expected, Result).

insert_a_smaller_and_an_even_smaller_value_test() ->
    Root = tree:init(3),
    Second_node = #tree{value = 1},
    First_node = #tree{value = 2,
		       left = Second_node},
    Expected = #tree{value = 3,
		     left = First_node},
    Pre_result = tree:insert(Root, 2),
    Result = tree:insert(Pre_result, 1),
    ?assertEqual(Expected, Result).
    
insert_multiple_values_test() ->
    Root = tree:init(0),
    Values = [1, 2, 3],
    Three_node = #tree{value = 3},
    Two_node = #tree{value = 2,
		     right = Three_node},
    One_node = #tree{value = 1,
		     right = Two_node},
    Expected = Root#tree{right = One_node},
    Result = tree:insert_multiple(Root, Values),
    ?assertEqual(Expected, Result).

balance_test() ->
    Root = tree:init(0),
    Expected = #tree{value = 1, left = #tree{value = 0}, right = #tree{value = 2}},
    Unbalanced = tree:insert_multiple(Root, [1, 2]),
    Result = tree:balance(Unbalanced),
    ?assertEqual(Expected, Result).

to_list_test() ->
    Root = tree:init(3),
    Tree = tree:insert_multiple(Root, [1, 4]),
    Expected = [3, 1, 4],
    Result = tree:to_list(Tree),
    ?assertEqual(Expected, Result).

balance_bigger_tree_test() ->
    Root = tree:init(1),
    Unbalanced = tree:insert_multiple(Root, [2, 3, 4, 5, 6]),
    Expected = #tree{value = 4,
		     left = #tree{value = 2,
				  left = #tree{value = 1},
				  right = #tree{value = 3}},
		     right = #tree{value = 6,
				   right = undefined,
				   left = #tree{value = 5}}
		    },
    Result = tree:balance(Unbalanced),
    ?assertEqual(Expected, Result).

from_list_test() ->
    Input = [1, 2, 3, 4, 5, 6],
    Expected = #tree{value = 4,
		     left = #tree{value = 2,
				  left = #tree{value = 1},
				  right = #tree{value = 3}},
		     right = #tree{value = 6,
				   right = undefined,
				   left = #tree{value = 5}}
		    },
    Result = tree:from_list(Input),
    ?assertEqual(Expected, Result).
    
to_list_breadth_first_test() ->
    Tree = tree:from_list([1, 2, 3, 4, 5, 6, 7]),
    Expected = [4, 2, 6, 1, 3, 5, 7],
    Result = tree:to_list_breadth_first(Tree),
    ?assertEqual(Expected, Result).
    
