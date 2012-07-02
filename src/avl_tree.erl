-module(avl_tree).
 % Map implemented with an AVL tree. Strictly worse than gb_trees. Unused.
-export([new/1, find/2, insert/3, remove/2, tree_to_list/1]).

-record(avl, {
    key,
    val,
    left,
    right,
    height
  }).

new([]) ->
  undefined;
new(Lst) when is_list(Lst) -> % Takes a proplist. Not enforced.
  lists:foldl(fun ({K, V}, T) -> insert(T, K, V) end, undefined, Lst).

new(K, V, L, R) ->
  balance(
    #avl{key=K,
	 val=V,
	 left=L,
	 right=R,
	 height=(max(height(L), height(R)) + 1)
	}
   ).

insert(T = #avl{key=K, val=V, left=L, right=R}, Key, Val) ->
  if
    Key > K ->
      new(K, V, L, insert(R, Key, Val));
    Key < K ->
      new(K, V, insert(L, Key, Val), R);
    Key == K ->
      T#avl{val=Val}
  end;
insert(undefined, Key, Val) ->
  new(Key, Val, undefined, undefined).

remove(#avl{key=K, left=L, right=R}, Key) ->
  if
    Key > K ->
      remove(R, Key);
    Key < K ->
      remove(L, Key);
    Key == K ->
      case R of
	undefined ->
	  L;
	#avl{} ->
	  #avl{key=NewKey, val=NewVal} = min(R),
	  new(NewKey, NewVal, L, remove(R, NewKey))
      end
  end.

find(#avl{val={K, V}, left=L, right=R}, Key) ->
  if
    Key > K ->
      find(R, Key);
    Key < K ->
      find(L, Key);
    Key == K ->
      {ok, V}
  end;
find(undefined, _Key) ->
  undefined.

height(undefined) -> 0;
height(#avl{height=H}) -> H.

min(#avl{left = L = #avl{}}) -> min(L);
min(T=#avl{left = undefined}) -> T.

balance(undefined) ->
  undefined;
balance(T=#avl{key=K, val=V, left=L, right=R}) ->
  Diff = height(L) - height(R),
  if
    Diff > 1 ->
      new(L#avl.key, L#avl.val, L#avl.left, new(K, V, L#avl.right, R));
    Diff < -1 ->
      new(R#avl.key, R#avl.val, new(K, V, L, R#avl.left), R#avl.right);
    true ->
      T
  end.

tree_to_list(#avl{key = K, val=V, left=L, right=R}) ->
  tree_to_list(L) ++ [{K, V} | tree_to_list(R)];
tree_to_list(undefined) ->
  [].
