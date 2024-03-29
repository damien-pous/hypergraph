# hypergraph

tool & library to visualize and edit sourced labelled hypergraphs

# build instructions

need graphviz to place graphs, then

opam install dune cairo2-gtk vg 
make

maybe [opam install otfm] ?


# syntax

-- terms (u,v)
0            empty graph
a            a-labeled edge
u | v        parallel composition
fu           forget u
lu           lift u
pu           permute p u

iu           u located according to injection i
s(u1,...,uk) series composition (without the forget)
*(u1,...,uk) star operation
u.v          binary sequential composition
u'           swap last two sources

-- sourced terms (at top level only)
#k u                              set arity to k
#<key=val;...><key=val;...>... u  set arity and source decorations

-- labels (a)
s             when s does not contain f/l/s, which are reserved for forgets/lifts/series
-s            when s does, or when s is empty

-- permutations (p)
[231]         explicit permutation of size <=9
(234)         cyclic permutation (here, [1342]), of size <=9
[1,..,8,10,9] explicit permutation of possibly greater size
(9,10)        cyclic permutation of possible greater size
note: the identity permutations cannot be written

-- injections (i)
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
 shift=x,y   : give an explicit shift (for edges)
 radius=x    : give an explicit radius
 label=x     : give an explicit label


# modules

## lib
Misc:        miscellaneous utilities

Set:         finite (multi)sets
Id:          identifiers and maps indexed by identifiers (no longer used)
Seq:         finite sequences, index starting at 1
Perm:        finite support permutations
Inj:         finite support injections
ISeq:        increasing sequences

Types:       shared class & module types (no implementation)

Constants:   constants for drawing graphs
Info:        informations about vertices & edges

Functor:     functors to generate derived operations & source-decorated operations

Raw:         raw terms, where arity is inferred
Term:        plain terms, as in the paper
NTerm:       normalised terms (full prime decomposed)

Lexer:       lexer
Parser:      parser (produces raw terms)

Picture:     canvas for drawing & virtual arenas
Geometry:    geometric utilities to draw edges

Graph:       graphs and associated functions

Conversions: conversion functions between graphs and various kinds of terms

Place:       placing graphs

GArena:      GTK arenas

## bin
Hg:          text mode program
Gui:         GTK program

## tests
Sanity:      sanity checks
Geo:         GTK program to debug edge drawing
