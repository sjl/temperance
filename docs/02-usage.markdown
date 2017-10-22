Usage
=====

This guide assumes you know the basics of logic programming.  If you've never
done any logic programming before, you should probably start with an
introduction to that before trying to read this.

[TOC]

## Hello, Temperance

Temperance is a pretty big system and we'll need to look at a few different
pieces one by one before you'll have a good picture of how it all fits
together, but let's at least get something on the screen.  First we'll set up
a logic database with some basic facts:

    :::lisp
    (in-package :cl-user)
    (use-package :temperance)

    (push-logic-frame t)

    (facts t
      (likes steve cats)
      (likes steve beer)
      (likes kim cats)
      (likes kim tea)
      (likes sally cats)
      (likes sally beer))

    (rule t (likes sally ?who)
      (likes ?who cats))

    (finalize-logic-frame t)

Ignore the stuff about logic frames for now.  We've added some facts saying:

* Steve and Sally like cats and beer
* Kim likes cats and tea
* Sally also likes anybody who likes cats

And now we can ask some questions:

    :::lisp
    (query-all t (likes ?who beer))
    ; =>
    ; ((?WHO STEVE)
    ;  (?WHO SALLY))

    (query-all t (likes sally ?what))
    ; =>
    ; ((?WHAT CATS)
    ;  (?WHAT BEER)
    ;  (?WHAT STEVE)
    ;  (?WHAT KIM)
    ;  (?WHAT SALLY))

    (query-all t (likes ?who ?what))
    ; =>
    ; ((?WHAT CATS ?WHO STEVE)
    ;  (?WHAT BEER ?WHO STEVE)
    ;  (?WHAT CATS ?WHO KIM)
    ;  (?WHAT TEA ?WHO KIM)
    ;  (?WHAT CATS ?WHO SALLY)
    ;  (?WHAT BEER ?WHO SALLY)
    ;  (?WHAT STEVE ?WHO SALLY)
    ;  (?WHAT KIM ?WHO SALLY)
    ;  (?WHAT SALLY ?WHO SALLY))

## Databases

The main data structure of Temperance is a database.  Most functions in
Temperance's API take this as their first argument.

You can create a database with `make-database`:

    :::lisp
    (defparameter *db* (make-database))
    (defparameter *db2* (make-database))

You can then use it in other functions:

    :::lisp
    (push-logic-frame *db*)

    (fact *db* (drink coffee))
    (fact *db* (drink tea))
    (fact *db* (drink water))

    (finalize-logic-frame *db*)

    (query-all *db* (drink ?what))
    ; =>
    ; ((?WHAT COFFEE)
    ;  (?WHAT TEA)
    ;  (?WHAT WATER))

    (query-all *db2* (drink ?what))
    ; =>
    ; NIL

Temperance also creates a database by default and stores it in
`*standard-database*`.  You can pass `t` to functions instead of a database
object to operate on this standard database if you don't want to bother creating
one of your own.

Databases are not (currently) thread-safe.  The consequences are undefined if
you access the same database object concurrently.  Thread-safety is on the
**TODO** list.

## Logic Frames

Temperance supports adding/retracting rules and facts to/from a database at
runtime, but it has a special interface for doing this in the interest of speed.

(Note: from here on out we'll say "rules" to mean "rules and facts", because
facts are just rules with empty bodies.)

### Assertion / Pushing

Each Temperance database has a "logic stack" consisting of zero or more "logic
frames".  To add some rules to the database you push a logic frame onto the
stack, add the rules, and then finalize the logic frame to compile the rules
into Prolog bytecode:

    :::lisp
    (defparameter *db* (make-database))

    ; Nothing's in the database yet.
    (query-all *db* (drink ?what))
    ; => NIL

    ; Push a logic frame.
    (push-logic-frame *db*)

    ; Still nothing in the DB.
    (query-all *db* (drink ?what))
    ; => NIL

    ; Add some facts.
    (fact *db* (drink coffee))
    (fact *db* (drink tea))
    (fact *db* (drink water))

    ; There's STILL nothing in the DB, because we haven't
    ; finalized the frame yet!
    (query-all *db* (drink ?what))
    ; => NIL

    ; Finalize the frame.  This compiles `drink`'s rules
    ; into bytecode.
    (finalize-logic-frame *db*)

    ; And now we can finally see some results.
    (query-all *db* (drink ?what))
    ; => ((?WHAT COFFEE) (?WHAT TEA) (?WHAT WATER))

### Retraction / Popping

You can pop a logic frame off the stack to retract everything inside it:

    :::lisp
    (query-all *db* (drink ?what))
    ; => ((?WHAT COFFEE) (?WHAT TEA) (?WHAT WATER))

    (pop-logic-frame *db*)

    (query-all *db* (drink ?what))
    ; => NIL

### Convenience

Most of the time you'll want to push a logic frame, add some stuff, and then
immediately finalize it.  Temperance provides a `push-logic-frame-with` macro to
make this easier:

    :::lisp
    (push-logic-frame-with *db*
      (princ "Adding some facts...")
      (fact *db* (sound cat meow))
      (fact *db* (sound dog woof))
      (fact *db* (sound cow moo)))
    ; => Adding some facts...

    (query-all *db* (sound cat ?what))
    ; => ((?WHAT MEOW))

Note how the body of `push-logic-frame-with` takes arbitrary code, not just
rules and facts.

### Restrictions

There main limitation introduced by Temperance's logic stack is that any
particular predicate must exist entirely in a single logic frame.  You *cannot*
spread the definition of a single predicate across multiple frames:

    :::lisp
    ; This is fine
    (push-logic-frame-with *db*
      (fact *db* (sound cat meow)))

    ; ERROR!  `sound/2` was already defined in a previous frame!
    (push-logic-frame-with *db*
      (fact *db* (sound dog woof)))

### Why?

Why does Temperance bother with this stack interface?  Why not just allow
arbitrary assertion and retraction of facts?

Arbitrary assertion and retraction would be a friendlier interface, but would be
slower.  Temperance was made with General Game Playing in mind, where speed is
important.  Asserting and retracting facts with a stack like this allows several
speed improvements:

* Predicates only need to be compiled once (when their frame is finalized),
  instead of being recompiled every time a new clause is asserted.
* Retraction of a frame is lighting fast — it's just changing a single integer
  and zeroing out a contiguous block of memory.
* The VM's code store no longer needs to deal with memory fragmentation or
  garbage collection.

## Rules

Once you've got an open logic frame you can add some rules to the database.

### Basic Rule/Fact Macros

Temperance offers a number of macros for adding things to a logic frame.  `(fact
database fact)` is the simplest, and will just add a single fact to the
database:

    :::lisp
    (fact *db* (cat scruffy))

If you want to add more than one fact you can use `(facts database fact...)`:

    :::lisp
    (facts *db*
      (cat fluffy)
      (dog rover)
      (dog lassie)
      (dog spot))

Adding rules can be done with `(rule database head body...)`:

    :::lisp
    (rule *db* (pet ?who)
      (cat ?who))

    (rule *db* (pet ?who)
      (dog ?who))

Logic variables in Temperance are any symbols that start with the `?` character.

### Dynamic Rules

The `fact`, `facts`, and `rule` macros quote their arguments.  If you need to
build rules at runtime you'll need to use the `invoke-...` variants and manage
the quoting yourself:

    :::lisp
    (defun add-cat-lover (database person)
      (invoke-rule database
          `(likes ,person ?who)
        '(cat ?who)))

The author is aware that `invoke-...` is an awful name and welcomes suggestions
for something better.

## Queries

Once you've got some logic frames finalized and ready to go you can query the
database with the `query...` macros.

### query

`(query database body...)` will return two values:

* A plist of the substitutions necessary to make `body` true (or `nil` if none
  is possible).
* `t` if the query was successful, `nil` otherwise.

The second value lets you distinguish a successful query that required no
bindings from an unsuccessful query.  For example:

    :::lisp
    (defparameter *db* (make-database))

    (push-logic-frame-with *db*
      (facts *db*
        (cat scruffy)
        (cat fluffy)))

    (query *db* (cat fluffy))
    ; =>
    ; NIL
    ; T        successful, no bindings needed

    (query *db* (cat chad))
    ; =>
    ; NIL
    ; NIL      unsuccessful

    (query *db* (cat ?who))
    ; =>
    ; (?WHO SCRUFFY)
    ; T

`query` only returns the first set of substitutions it finds.  If you want to see *all* results you'll need to use `query-all`.

### query-all

### query-map

### query-for

### query-do

### query-find

### prove

### Anonymous Variables

Each bare `?` symbol is treated as a separate anonymous variable:

### Dynamic Queries

## Lists
## Cut
## Call
## Built-In Predicates
