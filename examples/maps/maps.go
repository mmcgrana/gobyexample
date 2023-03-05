// _Maps_ é o [vetor associativo](https://pt.wikipedia.org/wiki/Vetor_associativo)
// nativo de Go.
// (também chamado de _hashes_ ou _dicts_ em outras linguagens).

package main

import "fmt"

func main() {

	// Para criar um map vazio, utilize o comando nativo `make`:
	// `make(map[tipoDaChave]tipoDoValor)`.
	m := make(map[string]int)

	// É possível alterar ou criar pares de chave/valor
	// usando a sintaxe `nomeDoMap[chave] = valor`.
	m["k1"] = 7
	m["k2"] = 13

	// Ao imprimir um map com `fmt.Println`, por exemplo,
	// serão exibidos todos os pares chave/valor.
	fmt.Println("mapa:", m)

	// Para selecionar o valor de determinada chave,
	// usa-se o comando `nomeDoMap[chave]`.
	v1 := m["k1"]
	fmt.Println("valor 1: ", v1)

	// O comando nativo `len`, recebendo um mapa como
	//  argumento, retorna o número de pares chave/valor.
	fmt.Println("len:", len(m))

	// O comando nativo `delete` remove um determinado
	// par de chave/valor do mapa.
	delete(m, "k2")
	fmt.Println("mapa:", m)

	// Ao selecionar um determinado valor em um mapa,
	// existe um segundo retorno opcional, do tipo booleano,
	// que indica a presença ou ausência de um determinado
	// par no map. Isto pode ser utilizado para desambiguação
	// entre chaves ausentes e chaves com valor zero, como
	// `0` or `""`. Onde o valor correspondente à chave não
	// for necessário, é possível ignorar com um identificador
	// vazio `_`.
	_, prs := m["k2"]
	fmt.Println("presença da chave:", prs)

	// Também é possível declarar e inicializar um
	// novo mapa na mesma linha com a sintaxe a seguir.
	n := map[string]int{"foo": 1, "bar": 2}
	fmt.Println("mapa:", n)
}
