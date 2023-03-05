// Em Go, um _array_ é uma sequência numerada de elementos
// de um tamanho específico. Tipicamente, [slices](slices) são
// muito mais comuns; arrays são úteis em alguns cenários
// específicos.

package main

import "fmt"

func main() {

	// Aqui é criado um array `a` com capacidade de armazenar
	// exatamente 5 inteiros. O tipo de elemento que ele irá
	// armazenar (int) e seu tamanho (5) são partes do tipo do array.
	// Neste caso, o array tem valores padrão zero.
	var a [5]int
	fmt.Println("vazio:", a)

	// É possível alterar o valor de um índice do array
	// utilizando a sintaxe `array[índice] = valor`,
	// bem como selecionar um valor com `array[índice]`.
	a[4] = 100
	fmt.Println("índice 4 alterado:", a)
	fmt.Println("valor índice 4:", a[4])

	// A função nativa `len` retorna o tamanho de um array.
	fmt.Println("len:", len(a))

	// Para declarar a inicializar um array em uma linha,
	// é possível usar esta sintaxe.
	b := [5]int{1, 2, 3, 4, 5}
	fmt.Println("array inicializado:", b)

	// Arrays, via de regra são unidimensionais, mas é possível
	// compor tipos para formar arrays multidimensionais.
	var twoD [2][3]int
	for i := 0; i < 2; i++ {
		for j := 0; j < 3; j++ {
			twoD[i][j] = i + j
		}
	}
	fmt.Println("bi-dimensional: ", twoD)
}
