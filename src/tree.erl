-module(tree).
-include("tree.hrl").
-export([init/2,
	 insert/3,
	 insert_multiple/2,
	 to_list/1,
	 to_list_breadth_first/1,
	 balance/1,
	 from_list/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

init(Key, Value) ->
    #tree{key = Key,
	  value = Value}.

init({Key, Value}) ->
    init(Key, Value).

insert(undefined, Key, Value) ->
    #tree{key = Key,
	  value = Value};
insert(Tree, Key, Value) ->
    do_insert(Tree, Key, Value).

do_insert(Tree, Key, Value) ->
    case Tree#tree.key of
	Key ->
	    Tree#tree{value = Value};
	X when (X > Key) ->
	    Tree#tree{left = insert(Tree#tree.left, Key, Value)};
	_ ->
	    Tree#tree{right = insert(Tree#tree.right, Key, Value)}
    end.

insert_multiple(Tree, []) ->
    Tree;
insert_multiple(Tree, [{Key, Value} | T]) ->
    insert_multiple(insert(Tree, Key, Value), T).

to_list(undefined) ->
    [];
to_list(Tree) ->
    Key = Tree#tree.key,
    Value = Tree#tree.value,
    Left = Tree#tree.left,
    Right = Tree#tree.right,
    [{Key, Value}] ++ to_list(Left) ++ to_list(Right).

to_list_breadth_first(Tree) ->
    tlbf([Tree]).

tlbf([]) ->
    [];
tlbf(Trees) ->
    Child_trees = lists:flatten([get_children(Tree) || Tree <- Trees]),
    [{Tree#tree.key, Tree#tree.value} || Tree <- Trees] ++ tlbf(Child_trees).

get_children(Tree) ->
    get_unless_undefined(Tree#tree.left) ++ get_unless_undefined(Tree#tree.right).

get_unless_undefined(undefined) ->
    [];
get_unless_undefined(X) ->
    [X].

balance(Tree) ->
    List = to_list(Tree),
    from_list(List).

balance_list([]) ->
    [];
balance_list(Sorted_list) ->
    Middle_index = 1 + length(Sorted_list) div 2,
    Middle = lists:nth(Middle_index, Sorted_list),
    Shorter_list = remove_nth(Middle_index, Sorted_list),
    Smaller = [X || X <- Shorter_list, X =< Middle],
    Greater = [X || X <- Shorter_list, X > Middle],
    [Middle | balance_list(Smaller) ++ balance_list(Greater)].

from_list(List) ->
    BL = balance_list(qs(List)),
    insert_multiple(init(hd(BL)), tl(BL)).

qs([]) ->
    [];
qs([Pivot | T]) ->
    qs([X || X <- T, X =< Pivot]) ++ [Pivot] ++ qs([X || X <- T, X > Pivot]).

remove_nth(N, List) ->
    remove_nth(N, List, 1).

remove_nth(N, [_|T], N) ->
    T;
remove_nth(N, [H|T], Current) ->
    [H | remove_nth(N, T, Current + 1)].

get_value(_, undefined) ->
    {error, not_defined};
get_value(Key, Tree) ->
    case Tree#tree.key of
	Key ->
	    Tree#tree.value;
	_ ->
	    case Key =< Tree#tree.key of
		true ->
		    get_value(Key, Tree#tree.left);
		false ->
		    get_value(Key, Tree#tree.right)
	    end
    end.
