// _Slices_ é um importante tipo de dado em Go,
// oferecendo uma interface mais completa do que
// arrays para lidar com sequências.

package main

import "fmt"

func main() {

	// Diferente de arrays, slices são tipados apenas com
	// tipo dos elementos que armazenará (sem um tamanho).
	// Para criar um slice vazio, com tamanho não zero,
	// deve-se usar o comando nativo `make`. Aqui é feito
	// um slice de `string`, com tamanho 3
	// (inicialmente com valor padrão zero).
	s := make([]string, 3)
	fmt.Println("vazio:", s)

	// Para alterar os valores de um slice e seleciná-los,
	// faz-se da mesma forma que com array.
	s[0] = "a"
	s[1] = "b"
	s[2] = "c"
	fmt.Println("exibe slice:", s)
	fmt.Println("valor índice 2:", s[2])

	// `len` retorna o tamanho de slices, da mesma forma
	// que com arrays.
	fmt.Println("len:", len(s))

	// Em adição a estas operações básicas, slices
	// suportam muitas outras que as fazem mais úteis do que
	// arrays. Uma delas é a função nativa `append`, que
	// retorna a slice contendo um ou mais novos valores.
	// Note que é preciso aceitar o valor retornado da função
	// `append` para ter a slice atualizada.
	s = append(s, "d")
	s = append(s, "e", "f")
	fmt.Println("slice com acréscimo:", s)

	// Slices também podem ser copiadas com `copy`. Aqui
	// é criado uma slice vazia `c` do mesmo tamanho da
	// slice `s`. Então, a slice `s` é copiada para `c`.
	c := make([]string, len(s))
	copy(c, s)
	fmt.Println("slice copiada:", c)

	// Slices suportam um operador "slice" com a sintaxe
	// `slice[índiceBaixo:índiceAlto]`. Por exemplo, o
	// comando a seguir seleciona os elementos da slice
	// de índices 2, 3 e 4; ou `s[2]`, `s[3]`, e `s[4]`.
	l := s[2:5]
	fmt.Println("slice 1:", l)

	// Já este, "fatia" o slice `s` até o
	// índice 5 (não incluso) ou `s[5]`.
	l = s[:5]
	fmt.Println("slice 2:", l)

	// E este, "fatia" o slices `s` a partir do
	// índice 2 (incluso) ou `s[2]`.
	l = s[2:]
	fmt.Println("slice 3:", l)

	// Também é possível declarar e inicializar um
	// slice em apenas uma linha.
	t := []string{"g", "h", "i"}
	fmt.Println("slice inicializada:", t)

	// Slices podem ser compsotas em estruturas
	// multi-dimensionais. O tamanho das slices internas
	// pode variar, diferente de arrays multi-dimensionais.
	twoD := make([][]int, 3)
	for i := 0; i < 3; i++ {
		innerLen := i + 1
		twoD[i] = make([]int, innerLen)
		for j := 0; j < innerLen; j++ {
			twoD[i][j] = i + j
		}
	}
	fmt.Println("bi-dimensional: ", twoD)
}
