// _range_ itera sobre elementos de uma variedade
// de estrutura de dados. Aqui será demonstrado como
// utilizá-lo com algumas das estruturas de dados já
// apresentadas.

package main

import "fmt"

func main() {

	// Aqui é utilizado o `range` para somar os números
	// de um slice. Funciona da mesma forma em arrays.
	nums := []int{2, 3, 4}
	sum := 0
	for _, num := range nums {
		sum += num
	}
	fmt.Println("soma:", sum)

	// `range` tanto em arrays quanto em slices fornece
	// chave e valor; ou índice e valor para cada entrada.
	// No exemplo acima não foi necessário o índice, então
	// foi ignorado com identificador vazio `_`.
	// Algumas vezes, entretanto, os índices serão necessários.
	for i, num := range nums {
		if num == 3 {
			fmt.Println("índice:", i)
		}
	}

	// `range` em mapas itera sobre os pares de chave/valor.
	kvs := map[string]string{"a": "apple", "b": "banana"}
	for k, v := range kvs {
		fmt.Printf("%s -> %s\n", k, v)
	}

	// `range` pode iterar apenas sobre as chaves de um mapa.
	for k := range kvs {
		fmt.Println("key:", k)
	}

	// `range` em strings itera sobre pontos de código Unicode.
	// O primeiro valor é o byte de índice de início da `rune`,
	// e o segundo, da própria `rune`.
	// Veja a seção [Strings and Runes](strings-and-runes)
	// para mais detalhes.
	for i, rune := range "go" {
		fmt.Println(i, rune)
	}
}
