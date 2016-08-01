-- qVariablesInQueryExample1.sql
-- used in demos and tests of the sql_xxxx() funcitons.
SELECT COUNT(DISTINCT ${column||product_id}$) AS number_unique_${column}$
  FROM ${table}$
;
