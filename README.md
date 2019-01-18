# datalog

Datalog is a query language based on the logic programming paradigm. The library is designed to formalize relation of n-ary streams. It implements an ad-hoc query engine using simplified version of general _logic programming_ paradigm. The library facilitates development of data integration, information exchange and semantic web applications.

[![Build Status](https://secure.travis-ci.org/fogfish/datalog.svg?branch=master)](http://travis-ci.org/fogfish/datalog)
[![Coverage Status](https://coveralls.io/repos/github/fogfish/datalog/badge.svg?branch=master)](https://coveralls.io/github/fogfish/datalog?branch=master)
[![Hex.pm](https://img.shields.io/hexpm/v/datalog.svg)](https://hex.pm/packages/datalog) 
[![Hex Downloads](https://img.shields.io/hexpm/dt/datalog.svg)](https://hex.pm/packages/datalog)

## Key features

* Top-down, sub-query, breath-first evaluation algorithm of logical program
* Erlang native interface to describe relation of streams
* Formalizes streams relation using human readable language
* Supports conjunctions, unions and recursion

### Background

The logic program consists of finite set of rules and large volume of ground facts -- knowledge. The rules are used to deduce new facts from other facts (built new relations). The [Horn clauses](https://en.wikipedia.org/wiki/Horn_clause) formally defines rules (first-order formula)

```
p₀(ẋ₀) :- p₁(ẋ₁) ^ ... ^ pₙ(ẋₙ).
```

`p₀` is a rule head, it is a producer of new relation (facts). The body is a conjunction of predicates. Each `pᵢ` is a predicate expression consist of predicate symbol and terms such as `p(t₁ , ... , tₙ)`, terms are either a literal constant or a variable. The predicate expression refers to relation of arbitrary arity - stream of tuples; terms range over this stream of tuples. Body predicates refers either to derived relations or ground facts. The predicates with common variables give rise to join. Ground facts are physically stored in external memory and accesses using streams abstractions.

A head is a new derived relation, deducted through the _logical program_ (body of horn clause) and ground facts. It is not explicitly persisted anywhere and corresponds the relation view (projection). The materialization of these view is the main task of this library.

A naive example, a new relation `about` is deducted from two relations `category` and `article`.
```
about(title, subject) :- category(x, subject), article(title, x).
```

### σ function

The library uses a "functional" interpretation of predicates, any predicate is a function -- sigma expression. It associates some of its bound terms to the remaining ones, returning the lazy set of tuples corresponding to materialized predicate. For example if p is binary predicate, its σ function is denoted as

```
σ(S) -> { ẋ ∈ S ^ p(ẋ) }
```

The library translate goals of rules into algebraic queries with an objective to access the minimum of ground facts needed in order to determine the answer. Rules are compiled to composition of σ functions (sub-queries). They are recursively expanded and the evaluation of the current sub-query is postponed until the new sub-query has been completely solved. 

The defined sigma expression formalism translates purely declarative semantic into operational semantic, i.e. specify of query must be executed. The lazy set ensures simplicity of one-tuple-at-a-time evaluation strategy while preserving efficiency of set-oriented methods used by high-level query languages. 

The sigma function is the formalism to relate logic program to ground facts persisted by external storage (most common query languages, access methodologies, i/o interfaces). The library uses sigma algebra to evaluate logic program but it requires developers to implement corresponding access protocols supported by external storage. This is an abstraction interface to retrieve ground facts _matching_ predicate. The library hides the concerns of logical program evaluation but provides hooks to implement access protocols.


## Getting started

The latest version of the library is available at its `master` branch. All development, including new features and bug fixes, take place on the `master` branch using forking and pull requests as described in contribution guidelines.

### Installation

The stable library release is available via hex packages, add the library as dependency to `rebar.config`

```erlang
{deps, [{datalog}]}.
``` 

### Usage

The library requires implementation of streaming interface to fetch a ground facts from external storage. It provides a [reference implementation](src/datalog_lists.erl) to deal with lists.

Let's consider usage example of library using [movies dataset](priv/imdb.config) and human readable [datalog](doc/syntax.md).

Build library and run the development console

```bash
make && make run
```

The typical usage scenario **parse**, **compile** and **evaluate**.

```erlang
%% parse query
Q = datalog:p("?- h(_, _). f(x,y). h(x,y) :- f(x,y), y > 1.").

%% compile query
E = datalog:c(datalog_list, Q).

%% evaluate query
S = datalog:q(E, [{a, 1}, {b, 2}, {c, 3}]).

%% [ 
%%   [b,2], [c,3] 
%% ]
stream:list(S).
```

Let's consider a complex scenarios with a reference dataset about movies.

```erlang
{ok, Imdb} = file:consult("./priv/imdb.config").
```

#### Basic queries

Match a person from dataset using query goals

```erlang
%%
%% define a query goal to match a person with `name` equal to `Ridley Scott`.
%% An identity rule is used to produce stream of tuples 
Q = "?- h(_, \"name\", \"Ridley Scott\"). h(s, p, o) :- f(s, p, o).".

%%
%% parse and compile a query into executable function
F = datalog:c(datalog_list, datalog:p(Q)).

%%
%% apply the function to dataset and materialize a stream of tuple, it returns
%% [
%%    [<<"urn:person:137">>,<<"name">>,<<"Ridley Scott">>]
%% ]
stream:list(F(Imdb)).
```

#### Data patterns

Match a person from dataset using patterns: literals and guards

```erlang
Q = "
   ?- h(_, _). 

   f(s, p, o). 

   h(s, o) :- 
      f(s, \"name\", o), o = \"Ridley Scott\".
".

%%
%% [
%%    [<<"urn:person:137">>,<<"Ridley Scott">>]
%% ]
F = datalog:c(datalog_list, datalog:p(Q)).
stream:list(F(Imdb)).
```

Discover all movies produces in 1987

```erlang
Q = "
   ?- h(_, _). 

   f(s, p, o). 

   h(s, title) :- 
      f(s, \"year\", 1987), 
      f(s, \"title\", title).
".

%%
%% [
%%    [<<"urn:movie:202">>,<<"Predator">>],
%%    [<<"urn:movie:203">>,<<"Lethal Weapon">>],
%%    [<<"urn:movie:204">>,<<"RoboCop">>]
%% ]
F = datalog:c(datalog_list, datalog:p(Q)).
stream:list(F(Imdb)).
```

Discover all actors of "Lethal Weapon" movie.

```erlang
Q = "
   ?- h(_). 

   f(s, p, o). 

   h(name) :- 
      f(m, \"title\", \"Lethal Weapon\"), 
      f(m, \"cast\", p), 
      f(p, \"name\", name).
".

%%
%% [
%%    [<<"Mel Gibson">>],
%%    [<<"Danny Glover">>],
%%    [<<"Gary Busey">>]
%% ]
F = datalog:c(datalog_list, datalog:p(Q)).
stream:list(F(Imdb)).
```

#### Predicates

Discover all movies produced before 1984

```erlang
Q = "
   ?- h(_, _). 

   f(s, p, o).

   h(title, year) :- 
      f(s, \"year\", year), 
      f(s, \"title\", title), 
      year < 1984.
".

%%
%% [
%%    [<<"First Blood">>,1982],
%%    [<<"Alien">>,1979],
%%    [<<"Mad Max">>,1979],
%%    [<<"Mad Max 2">>,1981]
%% ]
F = datalog:c(datalog_list, datalog:p(Q)).
stream:list(F(Imdb)).
```


### Design custom σ function

σ function is a partial application takes terms and side-effect environment, it returns a stream on tuples matching the terms pattern.

As an example, the following sigma function takes two terms (2-arity) and returns corresponding stream of tuples. The library uses list `[_]` as data structure for tuples. It allows efficiently bind deducted values to term variable. Thus, each sigma function return stream (lazy list) of lists.

```erlang
f(_, _, [X, Y]) ->
   fun(Env) ->
      stream:build(...)
   end.
```

Each term takes one of the following types `undefined | [filter()] | literal()`:
* `undefined` terms position corresponds to free variable, streams output at this 
position will be bound to corresponding variable at datalog expression.
* `[filter()]` defines acceptable range of term values
* `literal()` defines exact matching of term at stream


## Reference

1. [What You Always Wanted to Know About Datalog (And Never Dared to Ask)](https://pdfs.semanticscholar.org/9374/f0da312f3ba77fa840071d68935a28cba364.pdf)
1. [Theory of Relational Databases](http://www.cs.nott.ac.uk/~psznza/G53RDB07/rdb14.pdf)
1. [Foundation of Database](https://wiki.epfl.ch/provenance2011/documents/foundations%20of%20databases-abiteboul-1995.pdf)
1. [Recursive Programming in Datalog](http://infolab.stanford.edu/~ullman/dscb/dl-old.pdf)
1. [Datalog and Recursive Query Processing](http://blogs.evergreen.edu/sosw/files/2014/04/Green-Vol5-DBS-017.pdf)
1. http://ion.uwinnipeg.ca/~ychen2/journalpapers/StratifiedDB.pdf
1. http://www.cs.toronto.edu/~drosu/csc343-l7-handout6.pdf


## License

Copyright 2014 Dmitry Kolesnikov

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
