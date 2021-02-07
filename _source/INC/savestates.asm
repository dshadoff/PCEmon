
;savestate stuff -- maybe it will actually work?


;SOFT_TIA	equ SNAP_RAM	;$2140	;hide away in stack!
;RLE_PREV_BYTE	equ SOFT_TIA+1	;temp storage for RLE encoded byte
;RLE_COUNTER     equ SOFT_TIA+2	;"		"
;RLE_DEST        equ SOFT_TIA+3  ;(2) location with greatest length
;TIA_LEN		equ SOFT_TIA+5	;location for length!
;FOUND_LEN	equ TIA_LEN	;found length
;RLE_LEN		equ TIA_LEN+1	;needed length

Uncompress_ZP:
;;;	lda	#$DD
;;;	sta	<$02
;;;	tii	$2002,$2003,253		;fuck up ZP just to make sure!

	stw     RLE_DEST,<$00	;read pointer
	ldx	#$02		;start writing from ZP $02
	cly
.uncomp_lp:
	lda     [$00],Y
	cmp	#$BB		;magic number?
	bne	.simple_write
;---------- okay, read RLE info
	iny
	lda     [$00],Y		;get RLE length
	sta	RLE_COUNTER
	iny
	lda     [$00],Y		;get data to write!
.rleloop:
	sta	<$00,X
	inx
	dec     RLE_COUNTER
	bne	.rleloop
	bra	.check_data_end
;------------------
.simple_write:
	sta	<$00,X
	inx
.check_data_end:
	iny
	cpy     RLE_LEN		;Reached end of RLE data yet?
	bne     .uncomp_lp

	;Now clear the RLE data from our RAM area.
	stw     RLE_DEST,<$00	;read pointer
	cly
	cla
.clr_rle_lp:
	sta     [$00],Y
	iny
	cpy     RLE_LEN		;Reached end of RLE data yet?
	bne     .clr_rle_lp

	rts




Compress_ZP:
	jsr	Find_RLE_Space	;RLE_DEST has location and FOUND_LEN has max bytes avail.
	jsr	RLE_Zero_Page
	lda     FOUND_LEN
	cmp     RLE_LEN		;if Available length is less than RLE length, *RED ALERT!*
	bcc	.redalert
;;        stw	#$100,$0402	;Sprite 0 colour
;;	stw	#$1C0,$0404	;Green
	bra	.compress_end
.redalert:
        stw	#$100,$0402	;Sprite 0 colour
	stw	#$38,$0404   ;Red
.compress_end:
	rts


RLE_Zero_Page: ;(RLE_DEST now has location and FOUND_LEN has max bytes avail.)
	stw	RLE_DEST,<$00	;Destination!
	ldx	#$02	;start at ZP $02
	cly		;pointer into Destination!
	lda	<$02	;get byte here
	dec	a
	sta     RLE_PREV_BYTE	;make sure previous is different to start!
	stz	RLE_COUNTER	;count of Zero!
	stz     RLE_LEN		;ideally less than $100!
;------- now start search!
.search_loop:
	lda	<$00,X
	cmp     RLE_PREV_BYTE
	beq	.same_byte
.new_zp_byte:
	bsr	_finalize_ZP_run
	inx
	bne     .search_loop
 	bra     .totally_done_RLE
;	bne	.new_zp_byte
.same_byte:
	inc     RLE_COUNTER
.search_cont:
	inx
	bne     .search_loop
	;done all ZP from $02 to $FF
	inc	a			;just make it different
.totally_done_RLE:
	bsr	_finalize_ZP_run	;do a final clearing of the RLE buffer
	sty	RLE_LEN			;this becomes the official length!
	rts


_finalize_ZP_run:	;A contains new value
	pha     ;Store current
	lda	RLE_COUNTER
	beq	.no_previous
	lda     RLE_PREV_BYTE
	cmp	#$BB		;this is our "magic byte"!
				;it'll have to be encoded as 3 bytes no matter what...
	beq	.write_rle_bytes	;oh, well!
	lda     RLE_COUNTER
	cmp	#1		;is it only 1?
	beq	.write_single_byte
	cmp	#2              ;is it only 2?
	beq	.write_two_bytes
;--------
.write_rle_bytes:
	lda	#$BB		;First write RLE marker!
	sta	[$00],Y
	iny
	lda	RLE_COUNTER	;then the counter
	sta	[$00],Y
	iny
	lda	RLE_PREV_BYTE	;finally byte to repeat
	sta	[$00],Y
	iny
	bra	.no_previous
;------------------
.write_two_bytes:
	lda	RLE_PREV_BYTE
	sta	[$00],Y
	iny
.write_single_byte:
	lda	RLE_PREV_BYTE
	sta	[$00],Y
	iny
;----
.no_previous:
	pla	;Get back current
	sta     RLE_PREV_BYTE
	lda	#1
	sta     RLE_COUNTER	;minimum will be 1

	rts




Find_RLE_Space:	;first find size of available RAM
	stwz    TIA_LEN
	stw	#SNAP_RAM+$18,<$00	;save pointer to start of RAM
.zero_search:
	clx		;X will be our # contiguous
.search_loop:
;--
	lda	<$01		;have we found NOTHING?
	cmp	#$40
	beq	.end_search
;--
	lda	[$00]
	beq	.is_zero
	;not zero here....
	cpx	#0	;before THIS wasn't a zero either!
	bne	.run_found
	incw	<$00	;try next spot
	bra     .zero_search
;------------
.is_zero:
	inx
	incw	<$00	;try next spot
	cpx	#$FF	;hit the jackpot?
	beq     .end_search
	bra	.search_loop
;-----------
.run_found:
	bsr     _terminate_run
	incw	<$00	;try next spot
	bra     .zero_search
;-----------
.end_search:	;just trickle on down!

_terminate_run:		;saves X to length if LONGER!
	cpx	#$0
	beq     .shorter_run
	cpx	FOUND_LEN	;compare current run with Longest so far
	beq	.shorter_run	;not long enough
	bcs	.longer_run
.shorter_run:		;do nothing
	rts
.longer_run:
	stx     FOUND_LEN
	lda	<$00		;subtract length from pointer
	sub     FOUND_LEN
	sta     RLE_DEST
	lda	<$01
	sbc	#0
	sta	RLE_DEST+1	;Current best address stored here.
	rts




;=================================

Restore_Savestate:	;This jumps off into Syscard ROM again!
        stw	#$100,$0402	;Sprite 0 colour
	stw	#$1C0,$0404	;Green
;-------- Give some seconds to switch joypads
	lda	#8
.wait_joyswitch:
;;;	pha
	jsr	Wait_A_While
;;;	pla
	dec	a
	bne     .wait_joyswitch
        stw	#$100,$0402	;Sprite 0 colour
	stw	#$49,$0404	;Grey
	jsr	Wait_A_While
;----------------------------
	lda	#$FF
	sta   psg_mainvol	; Restore main volume

	ldy	SNAP_Y  ;get back Y reg

;;	cla
;;	sta	$2228,Y		;clear Controller Press just read!!!
;;	sta	$222D,Y		;clear Controller Press just read!!!

	ldx	SNAP_SP
	txs		;get back game's Stack Pointer
	ldx	SNAP_X	;get back X reg

	lda	SNAP_MPR6
	tam	#6
	lda	SNAP_MPR5
	tam	#5
	lda	SNAP_MPR4
	tam	#4
	lda	SNAP_MPR3
	tam	#3
	lda	SNAP_MPR2
	tam	#2

	lda	SNAP_ZP0
	sta	<$00
	lda	SNAP_ZP1
	sta	<$01

	tii	Restore_RAM,SAVE_FINAL_RAM,32
	jmp     SAVE_FINAL_RAM	;;;$2110
Restore_RAM:
	cla       	;syscard
	tam	#7	;at $E000
        SCREEN_ON	;VSync interrupt too!
	lda	SNAP_SR
	pha
	lda	SNAP_INTMASK
	sta	$1402
	lda	SNAP_A	;finally get back A reg
	plp		;get back Processor Status
;;;	cli	;may be unwanted?
	jmp     $E503	;return to END OF joypad reading...

;========================================




Uncompress_to_Serial:	;Uncompresses the RLE code blindly to the serial port.
	lda	SNAP_ZP0
	jsr	_ex_Serial_Out
	lda	SNAP_ZP1
	jsr	_ex_Serial_Out

	stw     RLE_DEST,<Start_Address	;read pointer
	cly
.uncomp_lp:
	lda     [Start_Address],Y
	cmp	#$BB		;magic number?
	bne	.simple_write
;---------- okay, read RLE info
	iny
	lda     [Start_Address],Y		;get RLE length
	tax
	iny
	lda     [Start_Address],Y		;get data to write!
.rleloop:
	jsr	_ex_Serial_Out
	dex
	bne	.rleloop
	bra	.check_data_end
;------------------
.simple_write:
	jsr	_ex_Serial_Out
.check_data_end:
	iny
	cpy     RLE_LEN		;Reached end of RLE data yet?
	bne     .uncomp_lp

	rts
