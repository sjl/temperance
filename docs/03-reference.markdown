# API Reference

The following is a list of all user-facing parts of Temperance.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `TEMPERANCE.WAM`

### `FACT` (macro)

    (FACT FACT)

### `FACTS` (macro)

    (FACTS
      &BODY
      FACTS)

### `FINALIZE-LOGIC-FRAME` (function)

    (FINALIZE-LOGIC-FRAME)

### `INVOKE-FACT` (function)

    (INVOKE-FACT FACT)

### `INVOKE-FACTS` (function)

    (INVOKE-FACTS &REST FACTS)

### `INVOKE-PROVE` (function)

    (INVOKE-PROVE &REST TERMS438)

### `INVOKE-QUERY` (function)

    (INVOKE-QUERY &REST TERMS262)

### `INVOKE-QUERY-ALL` (function)

    (INVOKE-QUERY-ALL &REST TERMS296)

### `INVOKE-QUERY-DO` (function)

    (INVOKE-QUERY-DO FUNCTION &REST TERMS366)

### `INVOKE-QUERY-FIND` (function)

    (INVOKE-QUERY-FIND PREDICATE &REST TERMS402)

### `INVOKE-QUERY-MAP` (function)

    (INVOKE-QUERY-MAP FUNCTION &REST TERMS330)

### `INVOKE-RULE` (function)

    (INVOKE-RULE HEAD &REST BODY)

### `MAKE-DATABASE` (function)

    (MAKE-DATABASE)

### `POP-LOGIC-FRAME` (function)

    (POP-LOGIC-FRAME)

### `PROVE` (macro)

    (PROVE &REST TERMS)

### `PUSH-LOGIC-FRAME` (function)

    (PUSH-LOGIC-FRAME)

### `PUSH-LOGIC-FRAME-WITH` (macro)

    (PUSH-LOGIC-FRAME-WITH
      &BODY
      BODY)

### `QUERY` (macro)

    (QUERY &REST TERMS)

### `QUERY-ALL` (macro)

    (QUERY-ALL &REST TERMS)

### `QUERY-DO` (macro)

    (QUERY-DO FUNCTION &REST TERMS)

### `QUERY-FIND` (macro)

    (QUERY-FIND PREDICATE &REST TERMS)

### `QUERY-MAP` (macro)

    (QUERY-MAP FUNCTION &REST TERMS)

### `RESET-DATABASE` (function)

    (RESET-DATABASE)

### `RULE` (macro)

    (RULE HEAD
      &BODY
      BODY)

### `WITH-DATABASE` (macro)

    (WITH-DATABASE DATABASE
      &BODY
      BODY)

### `WITH-FRESH-DATABASE` (macro)

    (WITH-FRESH-DATABASE
      &BODY
      BODY)

