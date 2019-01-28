# datalog syntax

> Datalog is a declarative logic programming language that syntactically is a subset of Prolog. It is often used as a query language for deductive databases... Datalog is not Turing complete, and is thus used as a domain-specific language. Unlike in Prolog, Datalog queries on finite sets are guaranteed to terminate.
> https://en.wikipedia.org/wiki/Datalog

This document depicts a datalog syntax supported by the library and it extensions that implements enhancement for Semantic Web and bindings with Erlang runtime.

The datalog program consist of finite set of rules and references to ground facts, which are stored in external memory. The rules are used to deduce new facts from other facts. It is important to understand: 
* there are no functions symbols in datalog, each predicate symbol refers to relation of arbitrary arity.
* datalog has a purely declarative semantics - the order of clauses is irrelevant.

## Rules

The [Horn clauses](https://en.wikipedia.org/wiki/Horn_clause) formally defines datalog rules (first-order formula)

```
p₀(ẋ₀) :- p₁(ẋ₁), ... , pₙ(ẋₙ).
```

`p₀` is a rule head, which represents a newly derived relation, deducted through horn clauses evaluation. The body is a conjunction of predicates `p₁ , ... , pₙ`. Each predicate refers to either derived or ground relation of arity equals to `|ẋ|`. The language support primitive predicates using infix notation, e.g. equality predicate `x = 1`. 

`ẋ₀ , ... , ẋₙ` are vector of variables and constants. Every variable of `ẋ₀` must occur in `ẋ₁ , ... , ẋₙ` so that rule is range restricted. The predicates with common variables give rise to join of relations. Vector `|ẋ|` facilitates predicate to map a relation from domain Dⁿ to boolean values so that program is able to holds in each relation that obeys rules.

A datalog program is a finite set of rules.

Example of rules

```
h(x, z) :- a(x, y), b(y, z).
```

## Constants

Constants are predicate terms that do not change its value during the evaluation of program. Constants facilitate in pattern matching of ground relations while evaluating a programs. The library uses [semantic types](https://github.com/fogfish/semantic/blob/master/doc/datatype.md) to express constants. The usage of semantic types improves ambiguous translations of string literals to type system used at external memory. 

Data type | Syntax | Example
--- | --- | ---
xsd:anyURI |  `<.+>` | `<http://example.com/a>`
|| `[a-zA-Z_@]+:[a-zA-Z_@]+` | `example:a`
xsd:string | `".*"` | `"example text"`
|| `".*"@en` | `"example text"@en`
xsd:integer | `-?\d+` | `123`
|| `"-?\d+"^^xsd:integer` | `"123"^^xsd:integer`
xsd:decimal | `-?\d+.\d+` | `12.3`
|| `"-?\d+.\d+"^^xsd:decimal` | `"12.3"^^xsd:decimal`
xsd:boolean | `true\|false` | `true`
xsd:datetime | ISO8601 | `2007-04-05T14:30:00Z`
xsd:date | ISO8601 | `2007-04-05`
xsd:time | ISO8601 | `14:30:00Z`
xsd:yearmonth | ISO8601 | `2007-04`
xsd:monthdate | ISO8601 | `--04-05`

The library also implements container data types (tuples, maps, lists) usable as constants

Data type | Syntax | Example
--- | --- | ---
rdf:seq |  `( ... )` | `(154, 7623, 23)`
rdf:map |  `{ ... }` | `{type:"Point", coordinates:[21.3, 60.2]}`
rdf:list|  `[ ... ]` | `[1, "a", 3.2]`


Example of constants

```
h(x) :- a("example", x), b(x, 10.0).
h(x) :- a(<http://example.com/a>, x), b(x, y), x = example:a.
```

## Variables

Variable consists of all finite alphanumeric characters and digits beginning with an upper case letter, says datalog syntax. The library relaxes an upper case requirement for variables due to internal AST representation where variable becomes an atom. Variable are used to lift positional values of relation to a new one, and so on. The library support JSON-LD flavored syntax for variable name e.g. `@id`, `@type`.

Sometimes, you need to skip or ignore value of relation, use a black symbol (\_) to mark unused positions. 

Example of variables

```
h(z) :- a(_, y), b(y, z).
h(@id,z) :- a(@id, @type), b(@type, z).
```

## Predicates

> In a logic programming view, the term “predicate” is used as synonym for “relation (name)”.

Let's take an example `p₁(x₁ , ... , xₙ)`. `p₁` the name of relation, which is either correspond to ground truths one maintained by external storage or logical one derived while evaluating a program. `x₁ , ... , xₙ` terms either variable or constants.

```
actors(id, name).
movies(id, title, year, cast).

casting(title, name) :- movies(_, title, year, cast), actors(cast, name), year < 1984.
```

Ground truths relation `actors` consists of tuples `(id, name)`, another ground truth relation `movies` is `(id, title, year, cast)`. The derived relation `casting` joins `movies` and `actors` relations and restricts `movies` to instance where `year` less then 1984. Values of `title` and `name` is lifted to derived relation.

**Ground-truths** predicate provides reference to external memory and mapping of external tuples.  

```
p( ... ).
```

**Derived relation**

```
p( ... ) :- ... .
```

**Semantic Web** predicates are compact IRIs, its just annotates relations as an instance of a class (rdf:type), nothing more the syntax sugar. 

```
schema:thing( ... ) :- foaf:person( ... foaf:name ), foaf:name > "A", foaf:name < "B".
```

**Native** predicates refers to ground truths relations implemented by Erlang functions. The syntax uses dot (.) to separate references to native modules and functions. 

```
module.function( ... ).
.function( ... ).
``` 

**Built-in** are native predicates where module definition is by passed, they are implemented by the evaluator. 

```
.unique( ... ) // a predicate ensures unique term(s) within the stream
.flat( ... )   // a predicate flatmap identity of term(s) over stream
.eq( ... )     // a predicate is a boolean predicates over stream terms
.ne( ... )
.lt( ... )
.gt( ... )
.le( ... )
.ge( ... )
``` 

**Infix** predicates, also know as guards, restricts terms of other relations. They always takes left argument a variable and right argument as constant.

```
x = 10
x > 10
x < 10
x >= 10
x =< 10
x != 10
x in (10, 20, 30)
``` 

## Union

datalog supports union `a(X,Y) ⋁ b(X,Y)` by

```
h(X,Y) :- a(X,Y)
h(X,Y) :- b(X,Y)
```

## Recursion

The following example computes the transitive closure of a graph.

```
t(x,y) :- g(x,y) 
t(x,y) :- g(x,z), t(z,y).
```

Consider the graph

```
g : (1, 2), (2, 3), (3, 4), (4, 5). 
```

Then we have

```
0 -> t : (1, 2), (2, 3), (3, 4), (4, 5)
1 -> t : (1, 3), (2, 4), (3, 5)
2 -> t : (1, 4), (2, 5)
3 -> t : (1, 5)
```

An other example

```
g : (1, 2), (2, 3), (3, 2).

0 -> t : (1, 2), (2, 3), (3, 2)
1 -> t : (1, 3), (2, 2), (3, 3)
```

## Aggregations

Many applications require the computation of various kinds of summary information over the data. Formally, an aggregate function is a mapping `ƒ` from bags (multi-sets) of domain values to domain values. The library offloads implementation of aggregations to sigma functions (external storage) but it provides a syntax to declare aggregation intent at ground truth predicates:

```
p(id, count "links", category 10 "subject").
```

## Goal

Goal defines a first rule.

```
?- h(_, _).
?- h("example", _).
?- h/2.
```

## Semantic Web compatibility

Semantic Web uses heavily IRIs as concept identifiers. This library implements datalog syntax enhancement  that support absolute and compact IRIs within the query. An absolute IRI is only used as literals but compact IRIs can be used as variables and literals simultaneously.

The following example shows usage of absolute IRI. You can use them as in-line literals within any predicates.

```
p(x, <http://example.com/1>).

h(x) :- p(x, <http://example.com/1>).
```

The following example shows usage of compact IRI, they are only allowed at ground truth or infix predicates.

```
p(x, foaf:name).

h(x) :- p(x, y), y = foaf:name.
```

Any compact IRI within predicate is used as variable 

```
h(rdf:id, foaf:name) :- p(rdf:id, foaf:name).
```


