// Go suporta [_funções anônimas_](https://en.wikipedia.org/wiki/Anonymous_function),
// as quais podem formar
// <a href="https://pt.wikipedia.org/wiki/Clausura_(ci%C3%AAncia_da_computa%C3%A7%C3%A3o)"><em>closures</em></a>.
// Funções anônimas são úteis quando se pretende
// definir a função em linha sem ser necessário nomeá-la.

package main

import "fmt"

// Esta função `intSeq` retorna outra função, que é
// definida anonimamente no corpo de `intSeq`. A função
// retornada _fecha sobre_ (_closes over_) a variavel
// `i` para formar um fechamento (closure).
func intSeq() func() int {
	i := 0
	return func() int {
		i++
		return i
	}
}

func main() {

	// Aqui, a execução da função `intSeq` (que retorna outra
	// função) é atribuída à variável `nextInt`.
	// Esta função captura o próprio valor de `i`, que
	// será atualizado a cada vez que é chamada `nextInt`.
	nextInt := intSeq()

	// Veja o efeito do closure chamando `nextInt`
	// algumas vezes.
	fmt.Println(nextInt())
	fmt.Println(nextInt())
	fmt.Println(nextInt())

	// Para confirmar que o estado é único àquela variável
	// específica, ao criar outra e testar, o resultado é diverso.
	newInts := intSeq()
	fmt.Println(newInts())
}
