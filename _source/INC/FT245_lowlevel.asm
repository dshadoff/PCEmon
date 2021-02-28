FT245_DATA	equ	$1C00
FT245_STATUS	equ	$1C01
FT245_IDENT	equ	$A0	; top 4 bits of the status byte should be this, if it exists
FT245_RDRDY	equ	$1	; RXF bit  - when high, do not read data from FT245 FIFO
FT245_WRFUL	equ	$2	; TXE# bit - when high, do not write data to FT245 FIFO


;======================================

Check_FT245_Connected:
	stz	ft245_present
	lda	FT245_STATUS
	and	#$F0
	cmp	#FT245_IDENT	; if Z flag set, port exists
	bne	.exit
	lda	#1
	sta	ft245_present	; store a 1 when port is installed
.exit:
	rts

;======================================
; Waits for and reads byte from FT245.
; Out: A = byte received
;Y reg gets totally destroyed!

read_FT245_with_exit:
	phx
	ldy	#$50			;# of loops to check for timeout
	sty	<TIMEOUT+1
	lda	#FT245_RDRDY
.rdwt:	iny
	bne	.rd_nx1
	dec	<TIMEOUT+1		;Dec this value every 256 loops.
	bne	.rd_nx1

	plx
	sec
	rts
;---
.rd_nx1:
	bit	FT245_STATUS
	bne	.rdwt
	bra	ft245rd_nx3

;======================================

FT245_In:
	phx
	lda	#FT245_RDRDY
.rd_nx1:
	bit	FT245_STATUS
	bne	.rd_nx1

ft245rd_nx3:
	lda	FT245_DATA
	plx
	clc
	rts

;======================================

FT245_Out:
	pha
	lda	#FT245_WRFUL
.wr_nx1:
	bit	FT245_STATUS
	bne	.wr_nx1
	pla
	sta	FT245_DATA
	rts

