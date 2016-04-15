// Il pacchetto `math/rand` di Go offre funzionalità per la
// generazione di [numeri pseudo-casuali](https://it.wikipedia.org/wiki/Numeri_pseudo-casuali)

package main

import "time"
import "fmt"
import "math/rand"

func main() {

	// Ad esempio, `rand.Intn(100)` ritorna un `int` `n` tale
	// che `0 <= n < 100`.
	fmt.Print(rand.Intn(100), ",")
	fmt.Print(rand.Intn(100))
	fmt.Println()

	// `rand.Float64` ritorna un valore di tipo `float64` `f`
	// tale che `0.0 <= f < 1.0`.
	fmt.Println(rand.Float64())

	// Questa espressione può essere utilizzata
	// per genere float random in un altro intervallo,
	// In questo caso`5.0 <= f' < 10.0`.
	fmt.Print((rand.Float64()*5)+5, ",")
	fmt.Print((rand.Float64() * 5) + 5)
	fmt.Println()

	// Il generatore di numeri casuli è deterministico,
	// produrrà quindi la stessa sequenza di numeri ad ogni
	// esecuzione. Per generare sequenze casuali ad ogni
	// esecuzione, dobbiamo settare un seed variabile.
	// Tieni in considerazione che non è sicuro utilizzare
	// questo metodo per generare numeri che devono essere
	// segreti, è consigliabile usare il pacchetto `crypto/rand`.
	s1 := rand.NewSource(time.Now().UnixNano())
	r1 := rand.New(s1)

	// È quindi possibile invocare `rand.Rand` come
	// la funzione `rand` del pacchetto.
	fmt.Print(r1.Intn(100), ",")
	fmt.Print(r1.Intn(100))
	fmt.Println()

	// Se imposti come seed lo stesso numero, vedrai
	// che verrà generata la stessa sequenza di numeri
	s2 := rand.NewSource(42)
	r2 := rand.New(s2)
	fmt.Print(r2.Intn(100), ",")
	fmt.Print(r2.Intn(100))
	fmt.Println()
	s3 := rand.NewSource(42)
	r3 := rand.New(s3)
	fmt.Print(r3.Intn(100), ",")
	fmt.Print(r3.Intn(100))
}
