package main

import (
	_ "github.com/bmizerany/pq"
	"database/sql"
	"fmt"
	"time"
)

func main() {
	db, openErr := sql.Open("postgres", "dbname=gobyexample sslmode=disable")
	if openErr != nil { panic(openErr) }
	defer db.Close()
	fmt.Println(db)

	createRep, createErr := db.Exec("CREATE TABLE items (a int8, b float8, c boolean, d text, e timestamp with time zone)")
	if createErr != nil { panic(createErr) }
	fmt.Println(createRep)

	insertRep, insertErr := db.Exec("INSERT INTO items VALUES (1, 2.0, false, 'string', '2000-01-01T01:02:03Z')")
	if insertErr != nil { panic(insertErr) }
	fmt.Println(insertRep)

	t1, _ := time.Parse(time.RFC3339, "2000-04-08T03:02:01Z")
	t2, _ := time.Parse(time.RFC3339, "2007-03-02T10:15:45Z")
	minsertRep, minsertErr := db.Exec("Insert INTO items VALUES ($1, $2, $3, $4, $5), ($6, $7, $8, $9, $10)",
									  3, 7.0, true,  "more", t1,
									  5, 1.0, false, "less", t2)
	if minsertErr != nil { panic(minsertErr) }
	fmt.Println(minsertRep)

	dropRep, dropErr := db.Exec("DROP TABLE items")
	if dropErr != nil { panic(dropErr) }
	fmt.Println(dropRep)
}

// # start postgres
// $ createdb gobyexample
// $ cd xx-postgres
// $ go get
// $ ./xx-postgres
