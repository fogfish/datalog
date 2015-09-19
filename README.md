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

# syntax

The library supports two notations for datalog: 
* the original as ```string()```, it is compliant with @todo
* Erlang native format as ```term()```

## native format

```
-type(datalog() :: {atom(), bind(), [horn()]}).
-type(horn()    :: {atom(), bind(), [pred()]}).
-type(pred()    :: {atom(), bind()}).
-type(bind()    :: [any()]).
```

Example of Erlang native datalog
```
{id, [x, <<"Ridley Scott">>], 
   [
      {id, [x,y], [ {like, [x, name, y]} ]}
   ]
}.
```




# background

head :- body
 
* head is a "collector" of answers to the query expressed by the body
* body is a conjunction of predicates / "relations" (also build-in predicates and filters)
* variables are bound to value is the sets

rule context (intent) is tuple, each element is variable

predicate produce stream - (tuple bound to variables)
(input + predicate) (mapping to indexes) -> output


predicate:
p(X,Y,...) -> erlang:apply(Mod, p, [X, Y, ...])
{p, [1,2,3]}

rule:
{i, [1,2,3], [{...}, {...}]}

goal (?)

emitter - input to evaluator:
 {atom(), X} either match, or built-in filter, ... 


# term
  ingress - {1, in} | {1, {'>', 10}} 
  egress  - {1, eg}



# relation algebra

## intersection

```algebra
   a(X,Y) ⋀ b(X,Y)
```

```datalog 
   h(X,Y) :- a(X,Y), b(X,Y) 
```

## union

```algebra
   a(X,Y) ⋁ b(X,Y)
```

```datalog
   h(X,Y) :- a(X,Y)
   h(X,Y) :- b(X,Y)
```


# references

1.

1. http://ion.uwinnipeg.ca/~ychen2/journalpapers/StratifiedDB.pdf
1. http://www.cs.toronto.edu/~drosu/csc343-l7-handout6.pdf
