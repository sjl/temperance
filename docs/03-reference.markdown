# API Reference

The following is a list of all user-facing parts of Temperance.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `TEMPERANCE`

### `*STANDARD-DATABASE*` (variable)

The standard database used when `t` is supplied as a database designator.

### `FACT` (macro)

    (FACT DATABASE FACT)

Add a logical fact to `database`.

  `fact` will be wrapped in `(quote ...)`.  If you need to dynamically construct
  facts at runtime, see `invoke-fact`.

  Examples:

    (fact t (likes kim cats))
    (fact t (likes sjl cats))

  

### `FACTS` (macro)

    (FACTS DATABASE
      &BODY
      FACTS)

Add zero or more logical facts to `database`.

  Each fact in `facts` will be wrapped in `(quote ...)`.  If you need to
  dynamically construct facts at runtime, see `invoke-facts`.

  Examples:

    (facts t
      (successor 0 1)
      (successor 1 2)
      (successor 2 3))

  

### `FINALIZE-LOGIC-FRAME` (function)

    (FINALIZE-LOGIC-FRAME DATABASE)

Finalize the top logic frame of `database`'s logic stack.

  An error will be signaled if the logic stack is empty or the top frame is
  already finalized.

  

### `INVOKE-FACT` (function)

    (INVOKE-FACT DATABASE FACT)

Add a logical fact to `database`.

  The `fact` macro is a nicer interface, but this function can be useful if you
  need to build rules dynamically at runtime.

  Examples:

    (invoke-fact t '(successor 0 1))

    (defun add-cat-lover (name)
      (invoke-fact t `(likes ,name cats)))

  

### `INVOKE-FACTS` (function)

    (INVOKE-FACTS DATABASE &REST FACTS)

Add zero or more logical facts to `database`.

  The `facts` macro is a nicer interface, but this function can be useful if you
  need to build rules dynamically at runtime.

  Examples:

    (invoke-facts t
                  '(successor 0 1)
                  '(successor 1 2)
                  '(successor 2 3))

  

### `INVOKE-PROVE` (function)

    (INVOKE-PROVE DATABASE &REST TERMS)

### `INVOKE-QUERY` (function)

    (INVOKE-QUERY DATABASE &REST TERMS)

### `INVOKE-QUERY-ALL` (function)

    (INVOKE-QUERY-ALL DATABASE &REST TERMS)

### `INVOKE-QUERY-DO` (function)

    (INVOKE-QUERY-DO DATABASE FUNCTION &REST TERMS)

### `INVOKE-QUERY-FIND` (function)

    (INVOKE-QUERY-FIND DATABASE PREDICATE &REST TERMS)

### `INVOKE-QUERY-FOR` (function)

    (INVOKE-QUERY-FOR DATABASE VARIABLE &REST TERMS)

### `INVOKE-QUERY-MAP` (function)

    (INVOKE-QUERY-MAP DATABASE FUNCTION &REST TERMS)

### `INVOKE-RULE` (function)

    (INVOKE-RULE DATABASE HEAD &REST BODY)

Add a logical rule to `database` with the given `head` and `body`.

  The `rule` macro is a nicer interface, but this function can be useful if you
  need to build rules dynamically at runtime.

  Example:

    ; Sally like anyone who likes cats
    (invoke-rule t '(likes sally ?who)
      '(likes ?who cats))

  

### `MAKE-DATABASE` (function)

    (MAKE-DATABASE)

Create and return a fresh database.

### `POP-LOGIC-FRAME` (function)

    (POP-LOGIC-FRAME DATABASE)

Pop off the top logic frame of `database`'s logic stack.

  An error will be signaled if the logic stack is empty or the top frame is
  unfinalized.

  

### `PROVE` (macro)

    (PROVE DATABASE &REST TERMS)

### `PUSH-LOGIC-FRAME` (function)

    (PUSH-LOGIC-FRAME DATABASE)

Push a new, open logic frame onto `database`.

  An error will be signaled if there is already an unfinalized logic frame on
  the top of the stack.

  

### `PUSH-LOGIC-FRAME-WITH` (macro)

    (PUSH-LOGIC-FRAME-WITH DATABASE
      &BODY
      BODY)

Push a new logic frame onto `database`, run `body`, and finalize it.

  This is a convenience macro for the common process of pushing a logic frame,
  adding some stuff to it, and finalizing it right away.

  Example:

    (push-logic-frame-with t
      (rule t (likes sally ?who)
        (likes ?who cats))
      (facts t
        (likes kim cats)
        (likes sjl cats)
        (likes bob dogs)))

    (query-all t (likes sally ?who))
    ; =>
    ((?who kim) (?who sjl))

  

### `QUERY` (macro)

    (QUERY DATABASE &REST TERMS)

### `QUERY-ALL` (macro)

    (QUERY-ALL DATABASE &REST TERMS)

### `QUERY-DO` (macro)

    (QUERY-DO DATABASE FUNCTION &REST TERMS)

### `QUERY-FIND` (macro)

    (QUERY-FIND DATABASE PREDICATE &REST TERMS)

### `QUERY-FOR` (macro)

    (QUERY-FOR DATABASE VARIABLE &REST TERMS)

### `QUERY-MAP` (macro)

    (QUERY-MAP DATABASE FUNCTION &REST TERMS)

### `RESET-STANDARD-DATABASE` (function)

    (RESET-STANDARD-DATABASE)

Reset `*standard-database*` to a new, fresh database.

### `RULE` (macro)

    (RULE DATABASE
        HEAD
      &BODY
      BODY)

Add a logical rule to `database` with the given `head` and `body`.

  `head` and `body` will be wrapped in `(quote ...)`.  If you need to
  dynamically construct rules at runtime, see `invoke-rule`.

  Example:

    ; Sally like anyone who likes cats
    (rule t (likes sally ?who)
      (likes ?who cats))

  

### `WITH-DATABASE` (macro)

    (WITH-DATABASE DATABASE
      &BODY
      BODY)

Execute `body` with `*standard-database*` bound to `database`.

### `WITH-FRESH-DATABASE` (macro)

    (WITH-FRESH-DATABASE
      &BODY
      BODY)

Execute `body` with `*standard-database*` bound to a new, fresh database.

