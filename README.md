# hypergraph

tool & library to visualize and edit sourced labelled hypergraphs

# build instructions

opam install dune cairo2-gtk vg 
make

maybe [opam install otfm] ?


# syntax

-- terms:
0            empty graph
u | v        parallel composition
fu           forget u
lu           lift u
pu           permute p u
a            edge labeled a
-            unlabelled edge
\foo         edge labeled foo (f/l/s being reserved for forget/lift/star)
iu           u located according to injection i
s(u1,...,uk) series composition (without the forget)
*(u1,...,uk) star operation
u.v          binary sequential composition
u'           swap last two sources
#i u         force arity i

-- permutations
[231]         explicit permutation of size <=9
(234)         cyclic permutation (here, [1342]), of size <=9
[1,..,8,10,9] explicit permutation of possibly greater size
(9,10)        cyclic permutation of possible greater size
note: the identity permutations cannot be written

-- injections
{264}         injection mapping 1 to 2, 2 to 6, 3 to 4 
{2,6,11}      injection with potentially bigger values
{011}         injection mapping 1 to 11

-- decorations
forgets, edges, stars, and dots can be decored with key value lists:
 f<key=val;...>u
 a<key=val;...>
 *<key=val;...>(u,...,u)
 u.<key=val;...>v

interpreted keys:
 pos=x,y     : give an explicit position
 shift=x,y   : give an explicit shift (for edges only)
 radius=x    : give an explicit radius
 scale=x     : give an explicit scale
 label=x     : give an explicit label


# modules

Misc:        miscellaneous utilities

Set:         finite (multi)sets
Id:          identifiers and maps indexed by identifiers
Seq:         finite sequences, index starting at 1
Perm:        finite support permutations
Inj:         finite support injections
ISeq:        increasing sequences

Info:        informations about vertices & edges

Common:      type aliases, basic functions, algebra types

Raw:         raw terms, where arity is inferred
Term:        plain terms, as in the paper
NTerm:       normalised terms (full prime decomposed)

Lexer:       lexer
Parser:      parser (produces raw terms)

Graph:       graphs and associated functions

Conversions: conversion functions between graphs and various kinds of terms

Constants:   constants for drawing graphs
Geometry:    geometric utilities to draw edges
Drawable:    drawing graphs

Place:       placing graphs

Sanity:      sanity checks

Main:        tests in text mode
Ui:          user interface
Geometry-ui: user interface to debug edge drawing
