//test comment
START a = node(*)
MATCH (a)-[:ACTED_IN]->(m)<-[:DIRECTED]-(d)
RETURN a.name, m.title, d.name;

START a = node(*)
MATCH (a)-[:ACTED_IN]->(m)<-[:DIRECTED]-(d)
WITH d,m,count(a) as Actors
WHERE Actors > 4
RETURN d.name as Director,m.title as Movie, Actors ORDER BY Actors;

START a=node(*)
MATCH p=(a)-[:ACTED_IN]->(m)<-[:DIRECTED]-(d)
return p;

START a = node(*)
MATCH p1=(a)-[:ACTED_IN]->(m), p2=d-[:DIRECTED]->(m)
WHERE m.title="The Matrix"
RETURN p1, p2;

START a = node(*)
MATCH (a)-[:ACTED_IN]->(m)<-[:DIRECTED]-(d)
WHERE a=d
RETURN a.name;

START a = node(*)
MATCH (a)-[:ACTED_IN]->(m)<-[:DIRECTED]-(d)
WHERE a=d
RETURN a.name;

START a=node(*)
MATCH (a)-[:ACTED_IN]->(m)<-[:DIRECTED]-(d)
RETURN a.name, d.name, count(*) as Movies,collect(m.title) as Titles
ORDER BY (Movies) DESC
LIMIT 5;

START keanu=node:node_auto_index(name="Keanu Reeves")
RETURN keanu;

START keanu=node:node_auto_index(name="Keanu Reeves")
MATCH (keanu)-[:ACTED_IN]->(movie)
RETURN movie.title;

START keanu=node:node_auto_index(name="Keanu Reeves")
MATCH (keanu)-[r:ACTED_IN]->(movie)
WHERE "Neo" in r.roles
RETURN DISTINCT movie.title;

START keanu=node:node_auto_index(name="Keanu Reeves")
MATCH (keanu)-[:ACTED_IN]->()<-[:DIRECTED]-(director)
RETURN director.name;

START keanu=node:node_auto_index(name="Keanu Reeves")
MATCH (keanu)-[:ACTED_IN]->(movie)<-[:ACTED_IN]-(n)
WHERE n.born < keanu.born
RETURN DISTINCT n.name, keanu.born ,n.born;

START keanu=node:node_auto_index(name="Keanu Reeves"),
      hugo=node:node_auto_index(name="Hugo Weaving")
MATCH (keanu)-[:ACTED_IN]->(movie)
WHERE NOT((hugo)-[:ACTED_IN]->(movie))
RETURN DISTINCT movie.title;

START a = node(*)
MATCH (a)-[:ACTED_IN]->(m)
WITH a,count(m) as Movies
RETURN a.name as Actor, Movies ORDER BY Movies;

START keanu=node:node_auto_index(name="Keanu Reeves"),actor
MATCH past=(keanu)-[:ACTED_IN]->()<-[:ACTED_IN]-(),
      actors=(actor)-[:ACTED_IN]->()
WHERE hasnt=actors NOT IN past
RETURN hasnt;

START keanu=node:node_auto_index(name="Keanu Reeves")
MATCH (keanu)-[:ACTED_IN]->()<-[:ACTED_IN]-(c),
      (c)-[:ACTED_IN]->()<-[:ACTED_IN]-(coc)
WHERE NOT((keanu)-[:ACTED_IN]->()<-[:ACTED_IN]-(coc))
AND coc > keanu
RETURN coc.name, count(coc)
ORDER BY count(coc) DESC
LIMIT 3;

START kevin=node:node_auto_index(name="Kevin Bacon"),
      movie=node:node_auto_index(name="Mystic River")
MATCH (kevin)-[:ACTED_IN]->(movie)
RETURN DISTINCT movie.title;

CREATE (n 
	{
       title:"Mystic River",
       released:1993,
       tagline:"We bury our sins here, Dave. We wash them clean."
       }
	) RETURN n;


START movie=node:node_auto_index(title="Mystic River")
SET movie.released = 2003
RETURN movie;

start emil=node:node_auto_index(name="Emil Eifrem") MATCH emil-[r]->(n) DELETE r, emil;

START a=node(*)
MATCH (a)-[:ACTED_IN]->()<-[:ACTED_IN]-(b)
CREATE UNIQUE (a)-[:KNOWS]->(b);

START keanu=node:node_auto_index(name="Keanu Reeves")
MATCH (keanu)-[:KNOWS*2]->(fof) 
WHERE keanu <> fof
RETURN distinct fof.name;

START charlize=node:node_auto_index(name="Charlize Theron"),
      bacon=node:node_auto_index(name="Kevin Bacon")
MATCH p=shortestPath((charlize)-[:KNOWS*]->(bacon))
RETURN extract(n in nodes(p) | n.name)[1];

START actors=node:

MATCH (alice)-[:`REALLY LIKES`]->(bob)
MATCH (alice)-[:`REALLY ``LIKES```]->(bob)
myFancyIdentifier.`(weird property name)`
"string\t\n\b\f\\\''\""
