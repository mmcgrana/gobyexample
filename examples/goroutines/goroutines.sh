# Quando si esegue questo programma si può notare prima
# l'output delle chiamate bloccanti. Successivamente si
# nota come le due goroutine lanciate producano
# dell'output "disordinato". Questo disordine riflette
# l'esecuzione concorrente delle goroutine da parte del
# runtime di Go.
$ go run goroutines.go
direct : 0
direct : 1
direct : 2
goroutine : 0
going
goroutine : 1
goroutine : 2
<enter>
done

# Adesso siamo pronti per analizzare la struttura
# complementare alle goroutine in Go: i channel.
