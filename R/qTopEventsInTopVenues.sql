--qTopEventsInTopVenues.sql
  WITH 
top_venues AS (
    SELECT v.venueid,
           v.venuename,
           SUM(s.pricepaid) AS total_venue
      FROM venue AS v
      JOIN event AS e
        ON e.venueid = v.venueid
      JOIN sales AS s
        ON s.eventid = e.eventid 
     GROUP BY 1, 2
     ORDER BY total_venue DESC
     LIMIT 10
     ),
top_events AS (
    SELECT e.eventid,
           e.eventname,
           e.venueid,
           SUM(s.pricepaid) AS total_event
      FROM event AS e
      JOIN sales AS s
        ON s.eventid = e.eventid 
     GROUP BY 1, 2, 3
     ORDER BY total_event DESC
     LIMIT 30
     )    
SELECT venuename, total_venue, eventname, total_event
  FROM top_venues AS tv
  LEFT JOIN top_events AS te
    ON te.venueid = tv.venueid 
 ORDER BY total_venue DESC
;       