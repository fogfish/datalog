# datalog

datalog evaluator library

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

1. http://ion.uwinnipeg.ca/~ychen2/journalpapers/StratifiedDB.pdf
1. http://www.cs.toronto.edu/~drosu/csc343-l7-handout6.pdf