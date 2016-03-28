# API Reference

The following is a list of all user-facing parts of Bones.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `BONES.PAIP`

Test?

### `*CHECK-OCCURS*` (variable)

Whether to perform an occurs check.

### `CLEAR-DB` (function)

    (CLEAR-DB)

### `FACT` (macro)

    (FACT &REST BODY)

### `FAIL` (variable)

Failure to unify

### `NO-BINDINGS` (variable)

A succesful unification, with no bindings.

### `QUERY` (macro)

    (QUERY &REST GOALS)

Perform the query interactively.

### `QUERY-ALL` (macro)

    (QUERY-ALL &REST GOALS)

Perform the query and automatically show all results.

### `QUERY-ONE` (macro)

    (QUERY-ONE &REST GOALS)

Perform the query and just show the first result.

### `RETURN-ALL` (macro)

    (RETURN-ALL &REST GOALS)

### `RETURN-ONE` (macro)

    (RETURN-ONE &REST GOALS)

### `RULE` (macro)

    (RULE &REST CLAUSE)

### `UNIFY` (function)

    (UNIFY X Y &OPTIONAL (BINDINGS NO-BINDINGS))

Unify the two terms and return bindings necessary to do so (or FAIL).

