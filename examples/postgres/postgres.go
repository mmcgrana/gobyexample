package main

import _ "github.com/bmizerany/pq"
import "database/sql"
import "time"
import "fmt"

func main() {
    conf := "dbname=gobyexample sslmode=disable"
    db, openErr := sql.Open("postgres", conf)
    if openErr != nil {
        panic(openErr)
    }
    defer db.Close()
    fmt.Println(db)

    createRep, createErr := db.Exec(
        `CREATE TABLE items
         (a int, b float, c boolean,
          d text, e timestamp with time zone)`)
    if createErr != nil {
        panic(createErr)
    }
    fmt.Println(createRep)

    insertRep, insertErr := db.Exec(
        `INSERT INTO items VALUES
         (1, 2.0, false,
          'string', '2000-01-01T01:02:03Z')`)
    if insertErr != nil {
        panic(insertErr)
    }
    fmt.Println(insertRep)

    timeFmt := time.RFC3339
    t1, _ := time.Parse(timeFmt, "2000-04-08T03:02:01Z")
    t2, _ := time.Parse(timeFmt, "2007-03-02T10:15:45Z")
    minsertRep, minsertErr := db.Exec(
        `Insert INTO items VALUES
         ($1, $2, $3, $4, $5),
         ($6, $7, $8, $9, $10)`,
        3, 7.0, true, "more", t1,
        5, 1.0, false, "less", t2)
    if minsertErr != nil {
        panic(minsertErr)
    }
    num, _ := minsertRep.RowsAffected()
    fmt.Println(num)

    rows, selectErr := db.Query("SELECT * FROM items")
    if selectErr != nil {
        panic(selectErr)
    }
    defer rows.Close()
    for rows.Next() {
        var r1 int
        var r2 float64
        var r3 bool
        var r4 string
        var r5 time.Time
        rows.Scan(&r1, &r2, &r3, &r4, &r5)
        fmt.Println(r1, r2, r3, r4, r5)
    }
    rowsErr := rows.Err()
    if rowsErr != nil {
        panic(rowsErr)
    }

    dropRep, dropErr := db.Exec("DROP TABLE items")
    if dropErr != nil {
        panic(dropErr)
    }
    fmt.Println(dropRep)
}

// todo: connection pooling & concurrency
// todo: re-connection
// todo: errors
// todo: database_url
