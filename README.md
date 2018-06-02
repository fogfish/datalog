# datalog

The library is designed to formalize relation of n-ary streams. It implements an ad-hoc query engine using simplified version of general _logic programming_ paradigm.

[![Build Status](https://secure.travis-ci.org/fogfish/datalog.svg?branch=master)](http://travis-ci.org/fogfish/datalog)

## Key features

* top-down, breath-first evaluation algorithm of logical program
* Erlang native interface to describe relation of streams
* Datalog query engine to formalize streams relation using human readable language

### Background

The logic program consists of finite set of rules and large volume of ground facts -- knowledge. The rules are used to deduce new facts from other facts (built new relations). The [Horn clauses](https://en.wikipedia.org/wiki/Horn_clause) formally defines rules (first-order formula)

```
L0 :- L1, ..., Ln
```

`L0` is a rule head, it is a producer of new relation (facts). The body is a conjunction of existed relations, built-in functions and filters. Each `Li` is a predicate expression consist of predicate symbol and terms such as `p(t1, ..., tk)`, terms are either a literal constant or a variable. The predicate expression refers to relation of arbitrary arity - stream of tuples; terms range over the stream of tuples. Body predicates refers either to derived relations or ground facts. The predicates with common variables give rise to join. Ground facts are physically stored in external memory and accesses using streams i/o technique.

A head is a new derived relation, deducted through the _logical program_ (body of horn clause) and ground facts. It is not explicitly persisted anywhere and corresponds the relation view (projection). The materialization of these view is the main task of this library.

A naive example, a new relation `about` is deducted from two relations `category` and `article`.
```
about(title, subject) :- category(x, subject), article(title, x).
```

### σ function

The library uses a "functional" interpretation of predicates, any predicate is a function -- sigma expression. It associates some of its bound terms to the remaining ones, returning the lazy set of tuples corresponding to materialized predicate. For example if p is binary predicate, its σ function is denoted as

```
σ(S) -> { y | x ∈ S ^ p(x, y) }
```

The library translate goals of rules into algebraic queries with an objective to access the minimum of ground facts needed in order to determine the answer. Rules are a compiled to composition of σ functions (sub-queries). They are recursively expanded and the evaluation of the current sub-query is postponed until the new sub-query has been completely solved. 

The defined sigma expression formalism translates purely declarative semantic into operational semantic, i.e. specify of query must be executed. The lazy set ensures simplicity of one-tuple-at-a-time evaluation strategy while preserving efficiency of set-oriented methods used by high-level query languages. 

The sigma function is the formalism to relate logic program to ground facts persisted by external storage (most common query languages, access methodologies, i/o interfaces). The library uses sigma algebra to evaluate logic program but it requires developers to implement corresponding access protocols supported by external storage. This is an abstraction interface to retrieve ground facts _matching_ predicate. The library hides the concerns of logical program evaluation but provides hooks to implement access protocols.



## Getting started









The type of σ function is defined as following. The `datalog:pattern()` and `datalog:heap()` carries the values of bound arguments so that sigma function return all tuples matching the pattern.

```
-spec sigma( datalog:pattern() ) -> fun( (datalog:heap()) -> fun( (_) -> datum:stream() ) ).
```

The library uses `map()` as data structure for tuples. It allows efficiently bind deducted values to head variabl. Thus, each sigma function return stream (lazy list) of maps.



## datalog expression

The logical program is a collection of horn clauses. The library expresses a datalog query using map as container type, see [datalog.erl](src/datalog.erl). It is called _native format_

```
-type q()       :: #{ name() => [head() | body()] }.
-type head()    :: [atom()].
-type body()    :: [{name(), pattern()}].
-type name()    :: atom().
-type pattern() :: #{'_' => head(), _ => match()}.
-type match()   :: _ | [bif()].
```

Each key/val pair of the map is horn clause -- the key is unique name, the value is the head and the body of the horn clause. The head defines variables which are deducted by body expression and lifted to new fact (relation). The body is a conjunction of statement, built-in functions and filters. The statement contains the name of predicate and pattern used to match group facts. 

E.g. query `h(x,y) :- p(x,y).` is parsed to map `#{ h => [ [x,y], {p,#{'_' => [x,y]}} ] }`

The _native format_ of datalog query is compiled to evaluator function. The evaluator function takes reference to external storage and returns lazy set of deducted facts. The compiler performs composition of sigma function to expression that addresses the query goal. 

```
fun( (_) -> datum:stream() ).
```

The typical usage scenario **parse**, **compile** and **evaluate**.

```erlang
%% parse query
Q = datalog:p("h(x,y) :- list(x,y), y > 1.").

%% compile query
E = datalog:c(Q).

%% evaluate query
datalog:q(E, [{a, 1}, {b, 2}, {c, 3}]).
```

The library allows to by-pass parsing and use direct interface to **declare** and **evaluate** query. In the following example, `datalog:horn` declares a horn clause that evaluates conjunction of statement; `datalog:list` declares a sigma function that translates a `datalog:pattern` into list pattern match primitives using notation of streams (lazy lists).

```erlang
%% define query using native format
E = datalog:horn([x,y], [
      datalog:list(#{ '_' => [x, y], y => [{'>', 1}] })
]).

%% evaluate query
datalog:q(E, [{a, 1}, {b, 2}, {c, 3}]).
```




## Reference

1. [What You Always Wanted to Know About Datalog (And Never Dared to Ask)](https://pdfs.semanticscholar.org/9374/f0da312f3ba77fa840071d68935a28cba364.pdf)
1. [Theory of Relational Databases](http://www.cs.nott.ac.uk/~psznza/G53RDB07/rdb14.pdf)



<!--








Example of horn clause with `X`, `Y`, `Z` variables and `year`, `title`, `1987` constants:
```
   title(Z) :- list(X, year, Y), list(X, title, Z), Y = 1987.
```



The library reflect each predicate to another horn clause or stream of facts.

Each datalog program has a goal that defines a subset of required relation.


The knowledge base is abstracted using streams to retrieve ground facts and feed them to logic engine.

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

-->

