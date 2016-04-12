// Spesso ci troviamo nella situazione di voler eseguire
// del codice Go in un certo istante nel futuro, oppure
// ripeterne l'esecuzione ad intervalli fissati. Le funzioni
// built-in di Go _timer_ e _ticker_ risolvono questi
// problemi facilmente. Vedremo prima i timer e a seguire
// i [ticker](tickers).

package main

import "time"
import "fmt"

func main() {

	// I timer rappresentano un singolo evento nel futuro.
	// Si indica al timer quanto si vuole attendere e
	// questo ci fornisce un channel sul quale riceveremo
	// una notifica dopo il tempo desiderato. In questo caso
	// si aspettano due secondi.
	timer1 := time.NewTimer(time.Second * 2)

	// Il comando `<-timer1.C` permette di bloccarsi sul
	// channel del timer `C` fino a quando non riceve un
	// valore, nel momento in cui il timer scatta.
	<-timer1.C
	fmt.Println("Timer 1 scaduto")

	// Se si vuole semplicemente aspettare del tempo è
	// possibile utilizzare la funzione `time.Sleep`.
	// Un buon caso d'uso per cui i timer risultano utili
	// è rappresentato dalla necessità di fermare il timer
	// prima che sia scattato. Qui vediamo un esempio di
	// questo use caso d'uso.
	timer2 := time.NewTimer(time.Second)
	go func() {
		<-timer2.C
		fmt.Println("Timer 2 scaduto")
	}()
	stop2 := timer2.Stop()
	if stop2 {
		fmt.Println("Timer 2 fermato")
	}
}
