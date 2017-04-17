![fnhouse!](https://raw.github.com/wiki/plumatic/fnhouse/images/fnhouse.jpg)

Leiningen dependency (Clojars): 

[![Clojars Project](http://clojars.org/prismatic/fnhouse/latest-version.svg)](http://clojars.org/prismatic/fnhouse)

**This is an alpha release. The API and organizational structure are
subject to change. Comments and contributions are much appreciated.**

[![Build Status](https://travis-ci.org/Prismatic/fnhouse.svg?branch=master)](https://travis-ci.org/Prismatic/fnhouse)
[![Dependencies Status](https://jarkeeper.com/Prismatic/fnhouse/status.svg)](https://jarkeeper.com/Prismatic/fnhouse)

fnhouse
=======

Fnhouse is a library that builds on top of [ring](https://github.com/ring-clojure/ring)
to provide a concise way to write web handlers safely, readably, without global variables,
without repeating yourself, and with hooks to do much more.

Here is an example fnhouse handler:

```clojure
(defnk $entries$POST
  "Add a new entry to the guestbook"
  {:responses {200 schemas/ClientEntry}}
  [[:request body :- schemas/EntryData]
   [:resources guestbook index]]
  (let [entry-id (swap! index inc)
        indexed-entry (assoc body :index entry-id)]
    (swap! guestbook assoc entry-id indexed-entry)
    {:body indexed-entry}))
```

This handler is implemented in terms of a keyword function (`defnk`) introduced in
  the [Plumbing](https://github.com/plumatic/plumbing) library.
  Using [Schema](https://github.com/plumatic/schema), fnhouse
  handlers can specify the type of their input arguments: in the example above, `body` is declared to be of type `EntryData`.
The handler explicitly declares the resources on which it depends: `guestbook`.
The Schema annotations on the arguments to the handler allow for
 schema checking to validate that the inputs have the right structure.



The entirety of fnhouse consists of a small number of files
 that play nicely together, but can be easily substituted, if need be.
To see an example use of fnhouse, check out guesthouse, the guestbook example.

### schemas.clj

Defines Schemas for `Handlers` and `HandlerInfo`.
The `HandlerInfo`
is a Schema for information about a specific HTTP handler, like the one above.
It specifies:

- the path and the HTTP method of the handler (which are used in routing)
- the declared Schemas of the following components of the request: the body, URI-args, and query-params
- the responses, a map from status code to the Schema of the body field of the response that the handler returns
- the resources upon which the handler depends
- source map information specifying the source code location where the handler is defined
- optionally it can also specify additional, arbitrary, user-defined annotations

### handlers.clj

Defines `nss->handlers-fn`, which
takes a map from path prefix string to namespace symbol.
   Sucks up all the fnhouse handlers in each namespace, and prefixes each handler's
   path with the corresponding path prefix.
   Finally, curries the resulting set of handlers to produce a function from a map
   of the union of all resources required by the handlers, to an API description
   (which is just a sequence of `schemas/AnnotatedHandlers` -- a handler function paired with `HandlerInfo` that describes it).


### middleware.clj

Defines the `coercion-middleware` function, which takes an `AnnotatedHandler`, an
input-coercer, and an output coercer.  These coercers are functions from schema
to functions of request and data; the inner functions return the transformed
data (or nil, if coercion is not necessary).  It wraps the handlers in a
ring-style middleware that coerces and validates inputs and outputs.  The
middleware uses walkers to simultaneously coerce and validate inputs in a
generous way (i.e., 1.0 in body will be cast to 1 in order to validate against
a long schema), and outputs will be clientized to match the output schemas as
specified by the output-coercer.  If custom coercion is not needed, (constantly
nil) can be passed as a no-op coercer.

### routes.clj

Defines the `root-handler` function, which takes a seq of `AnnotatedHandler`s, and
returns a single handler that routes the request to the appropriate handler
while binding `:uri-args` in the request to the appropriate path segments.

### docs.clj

A proof of concept, ultra-primitive HTML doc generation from a fnhouse API spec,
   including schemas and per-namespace handler doc pages.
The `docs.clj` file defines the `all-docs` function, which
generates HTML for handlers and schemas from a sequence of `HandlerInfo` descriptions.
It also defines a handler `$:page$GET` that can serve up the HTML
 generated by the `all-docs` function.

For much richer, stylish and functional documentation (and much more) via swagger, check out [fnhouse-swagger](https://github.com/metosin/fnhouse-swagger). 

## Example: Guesthouse

The [guesthouse example](examples/guesthouse),
  demonstrates how each of the components of fnhouse can be used in order to generate a
  simple, yet fully featured, web service.
This example project provides a simple guestbook service
  in which clients can add, remove, modify, and search for entries in an online guestbook.

The handlers for the guestbook are all defined in
[guestbook.clj](examples/guesthouse/src/guesthouse/guestbook.clj).
To see how the components of fnhouse can be tied together in order to make a
webserver look at the `start-api` method of
[core.clj](examples/guesthouse/src/guesthouse/core.clj).
To see `start-api` in action, check out the corresponding
[core_test.clj](examples/guesthouse/test/guesthouse/core_test.clj).

# Community

Please feel free to join the Plumbing [mailing list](https://groups.google.com/forum/#!forum/prismatic-plumbing) to ask questions or discuss how you're using Schema.

We welcome contributions in the form of bug reports and pull requests; please see `CONTRIBUTING.md` in the repo root for guidelines.

# Supported Clojure versions

Fnhouse is currently supported on Clojure 1.5.1 and 1.6.x.

# License

Distributed under the Eclipse Public License, the same as Clojure.
