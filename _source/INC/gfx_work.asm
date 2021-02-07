
setup_video:
	SCREEN_OFF
	jsr	xres_512
	jsr	yres_240
	jsr	BAT_6432

	jsr	Load_My_Chars	;Try to map & load in my CHR bank
	bcs	.no_chars
;---

	stw	#$100,$0402	;Colour Table register
	stwz	$0404           ;Clear Sprite 0 colour
	lda	#0
	sta	<palnum		;set one of 8 palettes
	jsr	change_pal
	jsr	Clear_Screen
.no_chars:
	st0	#7	;Clear scrolling?
	stwz	video_data
	rts


Load_My_Chars:	;Map in Character graphics bank
		;OK, we'll allow it to be THIS BANK+1 or 2
	tma	#2	;$4000
	pha	;Store bank
	tma	#7	;Get THIS bank
	inc	a
	tam	#2
	jsr	Chk_MyChar
	bcc     .map_success
	tma	#2
	inc	a
	tam	#2
	jsr     Chk_MyChar
	bcs     .map_fail
.map_success:
	vload   #TEXT_VRAM,Sc1_CHR,#$1000
	pla
	tam	#2	;Restore bank in $4000
	clc
	rts
.map_fail:
	rmb2	<echo_flags	;TURN OFF SCREEN PRINTING!
	pla
	tam	#2	;Restore bank in $4000
	sec
	rts

Chk_MyChar:
	lda	$5002	;Hmm.. I gotta be really careful here!
	cmp	#$00
	bne	.mychar_fail
	lda	$5003
	cmp	#$FF
	bne	.mychar_fail
	lda	$5004
	cmp	#$38
	bne	.mychar_fail
	lda	$5006
	cmp	#$28
	bne	.mychar_fail
	clc
	rts
.mychar_fail:
	sec
	rts


change_pal:	;Select a Palette based on contents of A reg
	phx
	phy
	and	#7	;I'll take care of limiting to 8 pals here.
	asl	a	;x2
	asl	a	;x4
	asl	a	;x8! (Cuz words)
	tax
	cly
	stwz	$0402	;Colour Table register
.wrtpallp:
	lda	Pals,X
	sta	$0404
	inx
	lda	Pals,X
	sta	$0405
	inx
	iny
	cpy	#4	;Done 4 entries?
	bne     .wrtpallp
	ply
	plx
	rts


;--------------------------------
NewlineBoth:	;Outputs a newline & manages screen line no.
	bbr1	<echo_flags,.no_rs232_newline
	pha
	lda	#$0D
	jsr	Serial_Out
	lda	#$0A
	jsr	Serial_Out
	pla
.no_rs232_newline:
NewlineScrOnly:
;;	jmp	manage_screen_newlines
;; These two routines are connected for speed.
manage_screen_newlines:
	bbr2	<echo_flags,.no_wrap_screen	;If no screen writing, do NADA
	pha
	addw	#$40,<curline_ptr
	lda     <curline_ptr+1
	and	#$07		;Limit to $000-$7FF
	sta     <curline_ptr+1

	cmp	#$07
	bne	.no_scroll_flag_on
	smb0    <scroll_flag	;Start at write to $600 in VRAM!
.no_scroll_flag_on:

	bbr0	<scroll_flag,.noscroll_erase_screen
	jsr	Erase_Line
	addw	#$40,<erase_ptr
	lda     <erase_ptr+1
	and	#$07		;Limit to $000-$7FF
	sta     <erase_ptr+1
	lda	<scrollval
	add	#8
	sta	<scrollval
	jsr	Set_Scroll	;Sliiiide screen up!
.noscroll_erase_screen:

	jsr   	write_curlineptr 	; set VRAM write address

	pla
.no_wrap_screen:
	rts



Set_Scroll:
	vidreg  #$08
	lda     <scrollval
	sta	video_data
	stz   video_data+1
	rts




spcspc:		;Prints 2 spaces to both screen & RS-232
	pha
	lda	#$20
	jsr     PrintBoth
	jsr     PrintBoth
	pla
	rts

spc:		;Prints a space to both screen & RS-232
	pha
	lda	#$20
	jsr     PrintBoth
	pla
	rts



Erase_Line:	;Erases 1 line of text from screen
		;Guaranteed to have the SCREEN ON, right?
	phy
	vidreg  #$00
	lda   <erase_ptr
	sta   video_data
	lda   <erase_ptr+1
	sta   video_data+1
	vidreg  #$02
	ldy	#64
.eraselinelp:
	st1	#$20
	st2	#FONT_BANK
	dey
	bne	.eraselinelp
	ply
	rts


write_curlineptr:
       stw	<curline_ptr,<screen_ptr	;Reset line!
write_scrnptr:	;Writes the screen pointer to VDC.
	bbr2	<echo_flags,.write_scrnptr_end 	;Skip screen pointer stuff
	vidreg  #$00
	lda   <screen_ptr
	sta   video_data
	lda   <screen_ptr+1
	sta   video_data+1
	vidreg  #$02
.write_scrnptr_end:
	rts
;----------------------
; No "inc_scrnptr" because VDC auto-increments it.
;--------------------------------
dec_scrnptr:	;Goes back one Char, and writes to VDC.
		;Quick and DIRTY, because we don't change the HIGH BYTE!
	bbr2	<echo_flags,.dec_scrnptr_end
	vidreg  #$00
	lda   <screen_ptr
	dec   a
	sta   <screen_ptr
	sta   video_data
	lda   <screen_ptr+1
	sta   video_data+1
	vidreg  #$02
.dec_scrnptr_end
	rts
;----------------------
Print_Text_Scr_Only:       ;A bit of a bad hack... I'm starting already.
	lda	<echo_flags
	pha
	rmb1    <echo_flags	;Disable RS-232 writing
	jsr	Print_Text_Both
	pla
	sta	<echo_flags
	rts
;----------------

PrintBoth:	;Outputs a Char to either/both/neither Screen & RS-232
	bbr2	<echo_flags,.no_screen_print
	cmp	#$0D
	beq     .no_screen_print	;(Ignore?)
	cmp	#$0A
	bne	.no_print_newline
	jsr	NewlineScrOnly		;Manage the screen here too?
	bra	.no_screen_print
.no_print_newline:
        pha
	ora	<font_hilite
	sta	video_data
	lda	#FONT_BANK
	sta	video_data+1
	pla
.no_screen_print:
	bbr1	<echo_flags,.no_rs232_print
	jsr	Serial_Out
.no_rs232_print:
	rts



Print_Text_Both:	;Prints a string from <_si, zero-terminated
	phy
	cly
.prtbothlp:
	lda	[_si],Y
	beq	.prtboth_end
	cmp	#$FF		;is it "Alt Font ON"?
	beq	.alt_on
	cmp	#$FE		;is it "Alt Font OFF"?
	beq	.alt_off
	jsr	PrintBoth	;Send to screen + RS-232
	bra	.no_hilite
.alt_on:
	smb7	<font_hilite
	bra	.no_hilite
.alt_off:
	rmb7	<font_hilite
.no_hilite:
	iny
	bne	.prtbothlp
	inc	<_si+1
	bra	.prtbothlp
.prtboth_end:
	ply
	rts

Print_Byte_Both:	;Prints a byte to the screen.
	pha			; preserve
	lsr   a 		; use top nybble
	lsr   a
	lsr   a
	lsr   a
	ora   #$30
	cmp	#$3A	;Is it ABCDEF?
	bcc	.noadj1
	add	#7	;Make ASCII ABCDEF
.noadj1:
	jsr	PrintBoth
	pla			; restore byte
Print_Nybble_Both:
	pha
	and   #$0F		; use bottom nybble
	ora   #$30
	cmp	#$3A	;Is it ABCDEF?
	bcc	.noadj1
	add	#7	;Make ASCII ABCDEF
.noadj1:
	jsr	PrintBoth
	pla
	rts

Print_Binary_Both:	;Prints byte to screen as Binary
	pha
	sta	<tempjmp	;Temp storage
	phx
	ldx	#8
.bina_lp:
	asl	<tempjmp
	bcs	.bin_one
	lda	#'0'	;Zero
	bra	.bin_cont
.bin_one:
	lda	#'1'	;One
.bin_cont:
	jsr	PrintBoth
	dex
	bne	.bina_lp
	plx
	pla
	rts



;;Print_Binary:	;Prints byte to screen as Binary
;;	pha
;;	phx
;;	ldx	#8
;;.bina_lp:
;;	asl	a
;;	bcs	.bin_one
;;	st1	#$10	;Zero
;;	bra	.bin_cont
;;.bin_one:
;;	st1	#$11	;One
;;.bin_cont:
;;	st2	#FONT_BANK	;Tile $2xx
;;	dex
;;	bne	.bina_lp
;;	plx
;;	pla
;;	rts


;;Print_Byte:	;Prints a byte to the screen.
;;	pha			; preserve
;;	lsr   a 		; use top nybble
;;	lsr   a
;;	lsr   a
;;	lsr   a
;;	ora   #$10
;;	sta   video_data	; store char # (0-F) (LSB of VRAM word)
;;	lda   #FONT_BANK		;VRAM $2000
;;	sta   video_data+1
;;	pla			; restore byte
;;Print_Nybble:
;;	pha
;;	and   #$0F		; use bottom nybble
;;	ora   #$10
;;	sta   video_data	; store char # (0-F) (LSB of VRAM word)
;;	lda   #FONT_BANK
;;	sta   video_data+1
;;	pla
;;	rts


Conv_Char_to_Nybble:	;Let's preserve A on fail?
	phx
	pha
	;input: A: a text char (should be 0..9 or A..F)
	sub	#$30	;0..9 range
	cmp	#10	;higher than 9?
	bcc	.finished
	cmp	#'A'-$30 ;lower than A..F?
	bcc	.bad_char
	cmp	#'G'-$30 ;lower than G?
	bcc	.sub_from_AF
.bad_char:
	pla
	plx	;Good.  Both A and X restored.
	sec
	rts
.sub_from_AF:	;Good character, but we need to convert down to HEX $A-$F
	sub	#7	;$11-$16 down to $A-$F
.finished: 	;We can use the number as-is
	plx     ;This WAS A, but we don't need it now.
	plx	;This was X.
	clc
	rts

Print_Safe_Char:	;Changes "unprintable" characters to '.'
	cmp	#$20	;Less than $20?
	bcc     .unsafe_char
	cmp	#$80	;more than $80?
	bcc     .safe_char
.unsafe_char:
	lda	#'.'
.safe_char:	jmp	PrintBoth

;*******************************************************************************

Clear_Screen:
	phx
	phy
	stw	#$0000,<_di
	jsr   	set_write 	; set VRAM write address for text screen
	cly
	ldx	#8		;$800 words to clear
.clrlp:
	st1	#$20
	st2	#FONT_BANK
	iny
	bne	.clrlp
	dex
	bne     .clrlp
	ply
	plx
	rts


;BAT_3264:			;Set virtual width to 32, height to 64
;	st0	#9
;	st1	#%01000000
;	st2	#0
;	lda	#%01000000
;	jsr	Store_BAT_Size
;	rts

BAT_6432:			;Set virtual width to 64, height to 32
	st0	#9
	st1	#%00010000
	st2	#0
	lda	#%00010000
;;	jsr	Store_BAT_Size
;;	rts
;-- drop down!
Store_BAT_Size:
	pha
	;Slide our BAT vars down to 0..7
	lsr	a
	lsr	a
	lsr	a
	lsr	a
	and	#$07	;clear 'CG' bit
	sta	<BAT_Size
	pla
	rts

;;yres_224:
;;	st0 #$0C
;;	stw #$1702,video_data
;;	st0 #$0D
;;	stw #$00DF,video_data
;;	rts

yres_240:
	st0 #$0C
	stw #$0D01,video_data
	st0 #$0D
	stw #$00EF,video_data	;$EF=240
	st0 #$0E
	stw #$0003,video_data
	rts

xres_256:
	vreg	#$0A
	stw	#$0202,video_data
	vreg	#$0B
	stw	#$041F,video_data
	lda	#%00000100
	sta	$0400
	rts

xres_320:
	vreg	#$0A
	stw	#$0303,video_data
	vreg	#$0B
	stw	#$062B,video_data
	lda	#%00000101
	sta	$0400
	rts

xres_512:
	vidreg	#$0A
	stw	#$0B02,video_data  	;Sets up a 528-pixel screen!
	vidreg	#$0B
	stw	#$043F,video_data
	lda	#$06			;hi-res with artifact reduction
	sta	$0400
	rts

Pals: 	.incpal "INC/GunstarASCII.pcx",0,2
