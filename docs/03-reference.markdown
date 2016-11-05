# API Reference

The following is a list of all user-facing parts of Temperance.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `TEMPERANCE`

### `FACT` (macro)

    (FACT DATABASE FACT)

### `FACTS` (macro)

    (FACTS DATABASE
      &BODY
      FACTS)

### `FINALIZE-LOGIC-FRAME` (function)

    (FINALIZE-LOGIC-FRAME DATABASE)

### `INVOKE-FACT` (function)

    (INVOKE-FACT DATABASE FACT)

### `INVOKE-FACTS` (function)

    (INVOKE-FACTS DATABASE &REST FACTS)

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

### `MAKE-DATABASE` (function)

    (MAKE-DATABASE)

### `POP-LOGIC-FRAME` (function)

    (POP-LOGIC-FRAME DATABASE)

### `PROVE` (macro)

    (PROVE DATABASE &REST TERMS)

### `PUSH-LOGIC-FRAME` (function)

    (PUSH-LOGIC-FRAME DATABASE)

### `PUSH-LOGIC-FRAME-WITH` (macro)

    (PUSH-LOGIC-FRAME-WITH DATABASE
      &BODY
      BODY)

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

### `RULE` (macro)

    (RULE DATABASE
        HEAD
      &BODY
      BODY)

### `WITH-FRESH-DATABASE` (macro)

    (WITH-FRESH-DATABASE
      &BODY
      BODY)

