-module(tree).
-include("tree.hrl").
-export([init/1,
	 insert/2,
	 insert_multiple/2,
	 to_list/1,
	 to_list_breadth_first/1,
	 balance/1,
	 from_list/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

init(Value) ->
    #tree{value = Value}.

insert(undefined, Value) ->
    #tree{value = Value};
insert(Tree, Value) ->
    do_insert(Tree, Value).

do_insert(Tree, Value) ->
    case Value =< Tree#tree.value of
	true ->
	    Tree#tree{left = insert(Tree#tree.left, Value)};
	false ->
	    Tree#tree{right = insert(Tree#tree.right, Value)}
    end.

insert_multiple(Tree, []) ->
    Tree;
insert_multiple(Tree, [Value | T]) ->
    insert_multiple(insert(Tree, Value), T).

to_list(undefined) ->
    [];
to_list(Tree) ->
    Value = Tree#tree.value,
    Left = Tree#tree.left,
    Right = Tree#tree.right,
    [Value] ++ to_list(Left) ++ to_list(Right).

to_list_breadth_first(Tree) ->
    tlbf([Tree]).

tlbf([]) ->
    [];
tlbf(Trees) ->
    Child_trees = lists:flatten([get_children(Tree) || Tree <- Trees]),
    [Tree#tree.value || Tree <- Trees] ++ tlbf(Child_trees).

get_children(Tree) ->
    get_unless_undefined(Tree#tree.left) ++ get_unless_undefined(Tree#tree.right).

get_unless_undefined(undefined) ->
    [];
get_unless_undefined(X) ->
    [X].

balance(Tree) ->
    List = to_list(Tree),
    Sorted = qs(List),
    BL = balance_list(Sorted),
    insert_multiple(init(hd(BL)), tl(BL)).

balance_list([]) ->
    [];
balance_list(List) ->
    Middle_index = 1 + length(List) div 2,
    Middle = lists:nth(Middle_index, List),
    Shorter_list = remove_nth(Middle_index, List),
    Smaller = [X || X <- Shorter_list, X =< Middle],
    Greater = [X || X <- Shorter_list, X > Middle],
    [Middle | balance_list(Smaller) ++ balance_list(Greater)].

from_list(List) ->
    Balanced = balance_list(List),
    insert_multiple(init(hd(Balanced)), tl(Balanced)).

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


