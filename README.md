# datalog

The library implements a simplified version of query engine using _logic programming_ paradigm. The logic program consists of finite set of facts (knowledge base) and rules. The rules are used to deduce new facts from other facts. The knowledge base is abstracted using streams to retrieve ground facts and feed them to logic engine.  

The Horn clauses formally defines rules

```
   L0 :- L1, ..., Ln
```

L0 is a rule head, it is a producer of new facts deducted by body expression. The body is a conjunction of statement, built-in functions and filters. Each Li consist of predicate symbol and terms sucn as p(t1, ..., tk). A term is either a constant or a variable. The library reflect each predicate to another horn clause or stream of facts.

Each datalog program has a goal that defines a subset of required relation.

Example of datalog program
```
?- id(X, "Ridley Scott").
id(X, Y) :- like(X, name, Y). 
```

## syntax

The library supports two notations for datalog: 
* the original as ```string()```, it is compliant with @todo
* Erlang native format as ```term()```

### native format

```
-type(datalog() :: {atom(), bind(), [horn()]}).
-type(horn()    :: {atom(), bind(), [pred()]}).
-type(pred()    :: {atom(), bind()}).
-type(bind()    :: [any()]).
```

datalog 
#{ id => [ [x,y], {like, [x, name, y]}]} }

goal
#{'?' => id, x => '_', y => <<"Ridley Scott">>}

compiled program (by make) 
#{ id => [ [x,y], #{id => like, t => [x, name, y], s => ? }]} }




Example of Erlang native datalog
```
{id, [x, <<"Ridley Scott">>], 
   [
      {id, [x,y], [ {like, [x, name, y]} ]}
   ]
}.
```

### datalog to predicate

#### match all
```
?- id(X, Y). 
id(X, Y) :- like(X, Y).

like('_', '_')
```

#### pattern match
```
?- id("A", Y). 
id(X, Y) :- like(X, Y).

like(<<"A">>, '_')
```

#### pattern match with clauses
```
?- id("A", Y). 
id(X, Y) :- like(X, Y), Y > 10.

like(<<"A">>, [{'>', 10}])
```

```
?- id("A", Y). 
id(X, Y) :- like(X, Y), Y > 10, Y < 20.

like(<<"A">>, [{'>', 10}, {'<', 20}])
```



## relation algebra vs datalog

### intersection

```algebra
   a(X,Y) ⋀ b(X,Y)
```

```datalog 
   h(X,Y) :- a(X,Y), b(X,Y) 
```

### union

```algebra
   a(X,Y) ⋁ b(X,Y)
```

```datalog
   h(X,Y) :- a(X,Y)
   h(X,Y) :- b(X,Y)
```

# references

1. http://ion.uwinnipeg.ca/~ychen2/journalpapers/StratifiedDB.pdf
1. http://www.cs.toronto.edu/~drosu/csc343-l7-handout6.pdf
