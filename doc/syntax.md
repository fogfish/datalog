# datalog syntax

> Datalog is a declarative logic programming language that syntactically is a subset of Prolog. It is often used as a query language for deductive databases... Datalog is not Turing complete, and is thus used as a domain-specific language. Unlike in Prolog, Datalog queries on finite sets are guaranteed to terminate.
> https://en.wikipedia.org/wiki/Datalog

This document depicts a datalog syntax supported by this library and its extensions that supports Semantic Web and Erlang runtime.

The datalog program consist of finite set of rules and reference to ground fact, which are stored in external memory. The rules are used to deduce new facts from other facts. It is important to understand: 
* there are no functions symbols in datalog, each predicate symbol refers relation of arbitrary arity.
* datalog has a purely declarative semantics - the order of clauses is irrelevant.

## Rules

The [Horn clauses](https://en.wikipedia.org/wiki/Horn_clause) formally defines datalog rules (first-order formula)

```
p₀(ẋ₀) :- p₁(ẋ₁), ... , pₙ(ẋₙ).
```

`p₀` is a rule head, which is a newly derived relation, deducted through horn clauses evaluation. The body is a conjunction of predicates `p₁ , ... , pₙ`. Each predicate refers to either derived or ground relation of arity equals to `|ẋ|`. The language support primitive predicates using infix notation, e.g. equality predicate `x = 1`. 

`ẋ₀ , ... , ẋₙ` are vector of variables and constants. Every variable of `ẋ₀` must occur in `ẋ₁ , ... , ẋₙ` so that rule is range restricted. The predicates with common variables give rise to join of relations. Vector `|ẋ|` facilitates predicate to map a relation from domain Dⁿ to boolean values so that evaluator is able to holds in each relation that obeys rules.

A datalog program is a finite set of rules.

Example of rules

```
h(x, z) :- a(x, y), b(y, z).
```

## Constants

Constants are predicate terms that do not change its value during the evaluation of program. Constants facilitate in pattern matching of ground relations while evaluating a programs. The library uses [semantic types](https://github.com/fogfish/semantic/blob/master/doc/datatype.md) to express constants

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
xsd:boolean | `true | false` | `true`
xsd:datetime | `\(\d+,\d+,\d+\)` | `(154, 7623, 23)`
|| ISO8601 | `2007-04-05T14:30:00Z`
xsd:date | ISO8601 | `2007-04-05`
xsd:time | ISO8601 | `14:30:00Z`
xsd:yearmonth | ISO8601 | `2007-04`
xsd:monthdate | ISO8601 | `--04-05`


## Variables


## Predicates

> In a logic programming view, the term “predicate” is used as synonym for “relation (name)”.


## horn clause

## inline literal
```
h(z) :- a("x", y), b(y, z). 
h(z) :- a(100, y), b(y, z). 
h(z) :- a(1.0, y), b(y, z). 
```

## match anything
```
h(z) :- a(_, y), b(y, z).
```

## guards
```
h(x,z) :- a(x, y), b(y, z), x = "x".
h(x,z) :- a(x, y), b(y, z), x = ("x", "y", "z").
h(x,z) :- a(x, y), b(y, z), x > 100.
h(x,z) :- a(x, y), b(y, z), x < 1.0.
h(x,z) :- a(x, y), b(y, z), x >= 1.0, x =< 100.
```

## bif
```
h(x,z) :- .a(x, y), .b(y, z).
h(x,z) :- mod.a(x, y), mod.b(y, z).
```

## semantic web
```
foaf:person(rdf:id, foaf:name) :- foaf:person(rdf:id, foaf:name), foaf:name > "A", foaf:name < "B".
```

## json-ld
```
h(@id,z) :- a(@id, @type), b(@type, z).
```
