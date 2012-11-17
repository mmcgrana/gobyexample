package beer

import static extension beer.BottleSupport.*
import org.junit.Test

class BottleSong {

	@Test
	def void singIt() {
		println(singTheSong(99))
	}
	
	def singTheSong(int all) '''
		«FOR i : all .. 1»
			«i.Bottles» of beer on the wall, «i.bottles» of beer.
			Take one down and pass it around, «(i - 1).bottles» of beer on the wall.
			
		«ENDFOR»
		No more bottles of beer on the wall, no more bottles of beer.
		Go to the store and buy some more, «all.bottles» of beer on the wall.
	'''

	def private java.lang.String bottles(int i) {
		switch i {
			case 0 : 'no more bottles'
			case 1 : 'one bottle'
			default : '''«i» bottles'''
		}.toString
	}	
	
	def String Bottles(int i) {
		bottles(i).toFirstUpper
	}
}