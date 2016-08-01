
<!-- README.md is generated from README.Rmd. Please edit that file -->
ds4ciMisc Package
=================

Includes various functions Jim has found useful over the years. This is, at the moment, is a work in progress as they are gathered from Jim's project files and include here.

SQL Tools
---------

The initial set of functions are focused on making it easier to use SQL DBMS based data sets in R. In particular, to support data profiling of SQL based data sets in the dProf package.

-   `sql_list()` Transforms a vector into a SQL list suitable for use in a WHERE clause.
-   `sql_collapse()` Collapses a multi-line string, such as what `readLines()` returns when reading a SQL script file, into a single line. SQL line comments, "--", are also removed.
-   `sql_varsub()` Replaces dbVisualizer variable tags, "\({...}\)", with supplied variable values.
