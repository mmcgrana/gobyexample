;--------------------------------------
; Lychrel numbers.
; 
; :author: Marc 'BlackJack' Rintsch
; :date: 2008-03-07
; :version: 0.1
; 
; Prints all `Lychrel numbers`_ between 1 and 100000.
; 
; The numbers are stored as array of "digits" in little endian
; order.  Each digit is a byte with a value between 0 and 9.
; 
; Runtime on C64: 00:21:01
; 
; .. _Lychrel numbers: http://en.wikipedia.org/wiki/Lychrel_number
; 
; .. cl65 -l -tnone -C simple.cfg lychrel.s -o lychrel.prg
;--------------------------------------

;--------------------------------------
; External addresses.
;--------------------------------------
	chrout	= $ffd2

;--------------------------------------
; Constants.
;--------------------------------------
	TO		= 100000
	TO_DIGITS	= 10
	ITERATIONS	= 100
	MAX_DIGITS	= TO_DIGITS + ITERATIONS

;--------------------------------------
; Global variables.
;--------------------------------------
.zeropage
; 
; Length of the currently tested `n` in digits.
; 
n_length:
	.res 1
; 
; Length of the number(s) `xa` and `xb` while testing.
; 
length:
	.res 1

.bss
; 
; Number to be tested as digits i.e. bytes with values between
; 0 and 9.  The length is stored in `n_length`.
; 
n:
	.res TO_DIGITS
; 
; Space for calculating the reversed and added values.
; In the `main` code the current number is copied into `xa`
; and then repeatedly `reverse_add`\ed to itself with the
; result of that adding stored in `xb`.
; 
xa:
	.res MAX_DIGITS
xb:
	.res MAX_DIGITS

;--------------------------------------
; BASIC header.
;--------------------------------------
.code
	.word 0800h		; Load address.
	.byte 0
	.word @line_end
	.word 2008		; Line number.
	.byte $9e		; SYS token.
	.byte "2080 "		; SYS argument.
	.byte "LYCHREL NUMBERS/BJ"
@line_end:
	.byte 0, 0, 0		; Line and program end marker.

;--------------------------------------
; Main program.
;--------------------------------------
.proc main

.zeropage
; 
; Three byte counter for `TO` iterations (100000 = $0186a0).
; 
i:
	.res 3

.code
; 
; Clear and set `n` and `i` to 1.
; 
	lda #0		; n := 0; n := 1; i := 1
	sta i+1
	sta i+2
	ldx #TO_DIGITS
clear_n:
	sta n-1,x
	dex
	bne clear_n
	inx
	stx i
	stx n
	stx n_length
	
mainloop:
	jsr is_lychrel
	bcc no_lychrel
	jsr print_n
no_lychrel:
	jsr increase_n
	
	inc i		; INC(i)
	bne skip
	inc i+1
	bne skip
	inc i+2
skip:
	lda i
	cmp #<TO
	bne mainloop
	lda i+1
	cmp #>TO
	bne mainloop
	lda i+2
	cmp #^TO
	bne mainloop
	
	rts
.endproc

;--------------------------------------
; Print `n` and a trailing newline.
;
; :in: `n_length`, `n`
;--------------------------------------
.proc print_n
	ldy n_length
L1:
	lda n-1,y
	ora #%110000    ; = '0'
	jsr chrout
	dey
	bne L1
	
	lda #13
	jmp chrout
.endproc

;--------------------------------------
; Increase `n` by one.
; 
; This procedure expects n[n_length] == 0 in case the number gets
; one digit longer.
; 
; :in: `n`, `n_length`
; :out: `n`, `n_length`
;--------------------------------------
.proc increase_n
	ldx #0
L1:
	inc n,x		; Increase digit.
	lda n,x
	cmp #10		; If "carry", store 0 and go to next digit.
	bne return
	lda #0
	sta n,x
	inx
	bne L1
return:
	cpx n_length	; If "carry" after last digit, increase length.
	bcc skip
	inc n_length
skip:
	rts
.endproc

;--------------------------------------
; Tests if `n` is a Lychrel number.
; 
; :in: `n`, `n_length`
; :out: C is set if yes, cleared otherwise.
; :uses: `length`, `xa`, `xb`
;--------------------------------------
.proc is_lychrel
.zeropage
i:
	.res 1

.code
	ldx n_length		; xa := n; length := n_length
	stx length
L1:
	lda n-1,x
	sta xa-1,x
	dex
	bne L1
	
	lda #ITERATIONS		; i := ITERATIONS
	sta i
L2:
	jsr reverse_add
	jsr is_palindrome
	bne no_palindrome
	clc
	rts
no_palindrome:
	ldx length		; a := b
L3:
	lda xb-1,x
	sta xa-1,x
	dex
	bne L3
	
	dec i			; Loop body end.
	bne L2
	
	sec
	rts
.endproc

;--------------------------------------
; Add the reverse to `xa` to itself and store the result in `xb`.
; 
; :in: `length`, `xa`
; :out: `length`, `xb`
;--------------------------------------
.proc reverse_add
.code
	ldx #0
	ldy length
	clc
L1:
	lda xa,x
	adc xa-1,y
	
	cmp #10
	bcc no_adjust
	sbc #10
no_adjust:
	sta xb,x
	
	dey
	inx
	txa		; ``eor`` instead of ``cpx`` to keep the carry flag
	eor length	; of the addition above.
	bne L1
	
	bcc no_carry
	lda #1
	sta xb,x
	inc length
no_carry:
	rts
.endproc

;--------------------------------------
; Checks if `xb` is a palindrome.
; 
; :in: `length`, `xb`
; :out: Z flag set if `xb` is a palindrome, cleared otherwise.
;--------------------------------------
.proc is_palindrome
.code
	ldx #0
	lda length
	tay
	lsr
	sta L1+1	; Self modifying code!
L1:
	cpx #0		; <<< 0 replaced by (`length` / 2).
	beq return
	lda xb,x
	cmp xb-1,y
	bne return
	dey
	inx
	bne L1
return:
	rts
.endproc
