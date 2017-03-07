# datalog syntax

## horn clause
```
h(x,z) :- a(x, y), b(y, z).
```

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
