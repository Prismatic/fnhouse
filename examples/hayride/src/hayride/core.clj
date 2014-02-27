(ns hayride.core
  (:use plumbing.core

        )
  (:require [plumbing.graph :as graph])
  )


#_
"
input coercion
output coercion
custom input/output coercion
doc generation
non-trivial resources
nesting of subgraphs
   (instance)
   (maybe use Leon's )
   maybe release other stuff (in plumbing or separate)
URI args
query params
bodies in/out

include nice exception printing (e.g. for schema errors)


possible example applications:
- todos
- twitter clone
- somehting fnhouse themed

to find:
- middleware for json stuff
"

(def service
  (graph/graph
   :tasks (fnk [] (atom []))))
