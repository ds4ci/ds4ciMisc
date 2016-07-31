-- qVariablesInQueryExample1.sql

SELECT COUNT(DISTINCT ${column||product_id}$) AS number_unique_${column}$
  FROM ${table}$
;
