-module(t).

-export([
   a/2, b/2
]).

a(X,   '_') ->
   stream:build([{X, 1},{X, 2},{X, 3}]).   

b(X,   '_') ->
   stream:build([{X, X + 1},{X, X + 2},{X, X + 3}]).   

