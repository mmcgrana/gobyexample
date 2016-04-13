// Possiamo usare i channel per sincronizzare l'esecuzione
// attraverso delle goroutine. Di seguito un esempio dove
// usiamo un ricevimento bloccante per aspettare che una
// goroutine termini il suo lavoro.

package main

import "fmt"
import "time"

// Questa è la funzione che eseguiremo in una goroutine.
// Il canale `fatto` verrà usato per dire a un'altra
// goroutine che ha finito il suo lavoro.
func worker(fatto chan bool) {
	fmt.Print("sto lavorando...")
	time.Sleep(time.Second)
	fmt.Println("fatto")

	// Inviamo un valore per dire che abbiamo finito il
	// nostro lavoro.
	fatto <- true
}

func main() {

	// Iniziamo una goroutine lavoratrice, dandole un
	// canale da notificare una volta che ha svolto
	// il suo lavoro.
	done := make(chan bool, 1)
	go worker(done)

	// Blocchiamo l'esecuzione finché non abbiamo ricevuto
	// una notifica sul canale dalla goroutine lavoratrice.
	<-done
}
