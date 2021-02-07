
; ======== ASM FILE to HACK IN a CHEAT MENU to the SYSCARD ============

	.nomlist
	.list

JOYTRG		equ $222D
CHEATNUM	equ $2100
CURSPOS		equ $2101	;0..5 for ADDRESS/BYTE
TEMP1		equ $2102
TEMP2		equ $2103
CHEAT_RAM	equ $2104	;used for cheats/trainers (16 bytes)
	; $2100-$2103 should be OK as temporary storage

	.zp
	.bss
;--- CODE area ----------
	.code
	.bank 0
	.incbin "INC\cdsel-before.bin"
;-------------------------

;============ SYSCARD HACKING STARTS HERE ================

	.bank 0		;E000-FFFF
	.org $E4F3	;Turn "Select" into "RUN+Select" activates PCEmon
	.db $C9,$0C,$D0,$07

    	.bank 3		;$C000-DFFF

	.org $D386
	jsr	Screen_Drawing	;Reroute the screen drawing. (1st entry)
	.org $d94b
	jsr	Screen_Drawing	;Reroute the screen drawing. (BRAM format exit)
	.org $da3e
	jsr	Screen_Drawing	;Reroute the screen drawing. (BRAM format exit2)
	.org $d975
	jmp     Screen_Drawing	;Reroute the screen drawing. (BRAM delete exit)

	.org $D6F5
	cmp #$05	;Bounds-checking in BRAM menu

	.org $D870	;Where is the table for Cursor POS?
	.dw COORD_TBL

    	.org $D8D0
	.dw $DA50	;Change jump table source in System Card BRAM menu


;============ SYSCARD HACKING ENDS HERE ================




;------------------------- My code and other stuff here! ------------
	.org $DA50	;JUMP TABLE HERE!
	.dw $D8D8	;BRAM Delete
	.dw $D8EE	;BRAM Format
	.dw Cheat_Exit_Activate		;$D902	;END
	.dw Do_Cheats
	.dw Do_Cheats
	.dw Do_Cheats
COORD_TBL:
	.db $46,$54,$46,$6C,$46,$84,$30,$A8,$30,$B8,$30,$C8
;-----------------------------------


; Cheats:  0   1 2  3  4   5   6 7  8  9   A   B C  D  E   F
;         'T' LLHH DD 'C' 'H' LLHH DD 'C' 'H' LLHH DD 'C' 'H'

Do_Cheats:
	phx
;;	stz	JOYTRG		;Nothing until next trigger!!!
	lda	$2836	;get cursor value (3..5)
	sec
	sbc	#$03
	sta	CHEATNUM	;$2100
	asl	a
	asl	a
	ora	CHEATNUM	;Multiply cursor (0..2) by 5
	tax		;keep in X
	stz	CURSPOS		;On entry to each cheat, cursor at 0
	bra	.Sprite_Moved_End
;----------------
.check_joy_lp:		;X always contains 0,5,10
	lda	JOYTRG
	bit	#$02	;button II?
	bne     Cheats_Exit
	bit	#$01	;button I
	bne	.I_pressed
	bit	#$20
	bne	.R_pressed
	bit	#$80
	bne	.L_pressed
	bit	#$10
	bne	.U_pressed
	bit	#$40
	bne	.D_pressed	;I'm depressed... ;-|

	bra	.check_joy_lp

;IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
.I_pressed:		;Turn on/off cheats
        jsr	_is_cheat_on
	bcs	.reactivate_cheat	;was off, so turn on
	;was on, so turn off
	stz	CHEAT_RAM+4,X
	stz	CHEAT_RAM+5,X
	bra	.Update_Screen_End
.reactivate_cheat:
	lda	#'C'
	sta    CHEAT_RAM+4,X	;Write "CHeat" signature!
	lda	#'H'
	sta    CHEAT_RAM+5,X
	bra	.Update_Screen_End
;IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII

;RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
.R_pressed:
	lda	CURSPOS
	inc	a
	cmp	#6
	bne	.no_reset_curspos
	cla
.no_reset_curspos:
	sta     CURSPOS
	bra	.Sprite_Moved_End
;RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR

;LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
.L_pressed:
	lda	CURSPOS
	dec	a
	bpl	.no_set_curspos
	lda	#5
.no_set_curspos:
	sta     CURSPOS
	bra	.Sprite_Moved_End
;LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL

;UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU
.U_pressed:
	stz	TEMP1		;Meaning "ADD"
	jsr	Change_Add_or_Data
	bra     .Update_Screen_End
;UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU

;DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
.D_pressed:
	lda	#1
	sta	TEMP1		;Meaning "SUB"
	jsr	Change_Add_or_Data
	bra     .Update_Screen_End
;DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD


.Update_Screen_End:
	jsr	Print_Cheat_Values
	bra	.Button_Pressed_End
.Sprite_Moved_End:
	bsr	Position_Sprite
	;flow down...
.Button_Pressed_End:
	stz	JOYTRG
	bra	.check_joy_lp
;--------- will loop until II pressed ----
Cheats_Exit:
	plx
	rts





Position_Sprite:	;Sets up my sprite Gfx and positions the sucker!
	phx
	ldx	CURSPOS	;get which nybble we're on
	lda	Crsr_Pos_Tbl,X
	plx
	clc
	adc	#$6F
	sta     $2219

	lda	#$82	;Make "SQUARE" sprite
	sta	$221B

	lda	#$0f    ;Palette $F, $8X is "overlaid"
	sta	$221D
	stz	$2216	;Sprite Zero?
	jsr	$e0a5   ;Position sprite
	lda	$2219
	clc
	adc	#8
	sta	$2219
	lda	#$01    ;Palette 1 (BLUE)
	sta	$221D
	lda	#$01    ;$8X is "overlaid"
	sta	$2216	;Sprite One?
       	jsr	$e0a5   ;Position sprite
	jsr	$e09f	;VRAM-SATB transfer start

	rts





Change_Add_or_Data:
	lda	TEMP1
	bne	.decrement_routine
.increment_routine:
	lda	CURSPOS
	beq     .addr_x000_i
	cmp	#1
	beq	.addr_0x00_i
	cmp	#2
	beq	.addr_00x0_i
	cmp	#3
	beq	.addr_000x_i
	cmp	#4
	beq	.data_x0_i
;-------------
.data_0x_i:
	lda    CHEAT_RAM+3,X
	and	#$F0
	sta	TEMP2
	lda    CHEAT_RAM+3,X
	inc	a
	and	#$0F
	ora	TEMP2
	sta	CHEAT_RAM+3,X
	rts
.data_x0_i:
	lda    CHEAT_RAM+3,X
	clc
	adc	#$10
	sta	CHEAT_RAM+3,X
	rts
.addr_x000_i:
	lda    CHEAT_RAM+2,X
	clc
	adc	#$10
	sta	CHEAT_RAM+2,X
	rts
.addr_0x00_i:
	lda    CHEAT_RAM+2,X
	and	#$F0
	sta	TEMP2
	lda    CHEAT_RAM+2,X
	inc	a
	and	#$0F
	ora	TEMP2
	sta	CHEAT_RAM+2,X
	rts
.addr_00x0_i:
	lda    CHEAT_RAM+1,X
	clc
	adc	#$10
	sta	CHEAT_RAM+1,X
	rts
.addr_000x_i:
	lda    CHEAT_RAM+1,X
	and	#$F0
	sta	TEMP2
	lda    CHEAT_RAM+1,X
	inc	a
	and	#$0F
	ora	TEMP2
	sta	CHEAT_RAM+1,X
	rts

.decrement_routine:
	lda	CURSPOS
	beq     .addr_x000_d
	cmp	#1
	beq	.addr_0x00_d
	cmp	#2
	beq	.addr_00x0_d
	cmp	#3
	beq	.addr_000x_d
	cmp	#4
	beq	.data_x0_d
;-------------
.data_0x_d:
	lda    CHEAT_RAM+3,X
	and	#$F0
	sta	TEMP2
	lda    CHEAT_RAM+3,X
	dec	a
	and	#$0F
	ora	TEMP2
	sta	CHEAT_RAM+3,X
	rts
.data_x0_d:
	lda    CHEAT_RAM+3,X
	sec
	sbc	#$10
	sta	CHEAT_RAM+3,X
	rts
.addr_x000_d:
	lda    CHEAT_RAM+2,X
	sec
	sbc	#$10
	sta	CHEAT_RAM+2,X
	rts
.addr_0x00_d:
	lda    CHEAT_RAM+2,X
	and	#$F0
	sta	TEMP2
	lda    CHEAT_RAM+2,X
	dec	a
	and	#$0F
	ora	TEMP2
	sta	CHEAT_RAM+2,X
	rts
.addr_00x0_d:
	lda    CHEAT_RAM+1,X
	sec
	sbc	#$10
	sta	CHEAT_RAM+1,X
	rts
.addr_000x_d:
	lda    CHEAT_RAM+1,X
	and	#$F0
	sta	TEMP2
	lda    CHEAT_RAM+1,X
	dec	a
	and	#$0F
	ora	TEMP2
	sta	CHEAT_RAM+1,X
	rts




Screen_Drawing:
	stz     CHEAT_RAM	;Turn OFF cheats inside this menu!
	jsr	$D5C6		;Clear screen, write BRAM Text

	stz	$0402	;Colour Table register
	lda	#1
	sta	$0403
	stz	$0404           ;Clear Sprite 0 colour
	stz	$0405
;--
	lda	#$ff
	sta	$0402	;Colour Table register
	lda	#1
	sta	$0403
	stz	$0404           ;Clear Sprite 0 colour
	stz	$0405
Print_Cheat_Header:
	;will flow down to next one
	ldy	#$02
	lda	#$68	;$BAT in VRAM
	bsr     set_write
	ldy	#$02	;CHARMAP #2, Palette 0!
	phx
	clx
.txtloop:
	lda	Cht_Txt,X
	beq	.end_text
	sta	$0002
	sty	$0003
	inx
	bra     .txtloop
.end_text:
	plx


Print_Cheat_Values:
	phx
	phy
	ldy	#$02    ;This should be used as BAT high byte, maybe
	lda	#$A9	;$BAT in VRAM
	bsr     set_write
	clx
	bsr	Print_One_Cheat
	lda	#$E9	;$BAT in VRAM
	bsr     set_write
	ldx	#5
	bsr	Print_One_Cheat
	ldy	#$03
	lda	#$29	;$BAT in VRAM
	bsr     set_write
	dey
	ldx	#10
	bsr	Print_One_Cheat
	ply
	plx
	rts

;--------------------------------
set_write:	;A has low byte, Y has high.
	st0  #$00
	sta  $0002
	sty  $0003
	st0  #$02
	rts

Cht_Txt: .db "- CHEAT CODES -",0

Print_One_Cheat:
	bsr	_is_cheat_on
	bcs	.print_x
	lda	#$0E		;O symbol
	bra	.wrt_onoff
.print_x:	lda #$7F	;X symbol
.wrt_onoff:
	sta	$0002
	sty	$0003

	st1	#$20	;SPACE!
	st2	#$02
	st1	#'$'
	st2	#$02

	lda    CHEAT_RAM+2,X
	jsr	Print_Byte
	lda    CHEAT_RAM+1,X
	jsr	Print_Byte

	st1	#$20	;SPACE!
	st2	#$02
	st1	#':'
	st2	#$02
	st1	#$20	;SPACE!
	st2	#$02

	lda    CHEAT_RAM+3,X
	jsr	Print_Byte
	rts





Print_Byte:		;Prints a byte to the screen.
	pha			; preserve
	lsr   a 		; use top nybble
	lsr   a
	lsr   a
	lsr   a
	ora   #$30
	cmp	#$3A	;Is it ABCDEF?
	bcc	.noadj1
	adc	#6	;Make ASCII ABCDEF (CARRY was ALREADY SET!)
.noadj1:
	sta	$0002
	sty	$0003
	pla			; restore byte
Print_Nybble:
	pha
	and   #$0F		; use bottom nybble
	ora   #$30
	cmp	#$3A	;Is it ABCDEF?
	bcc	.noadj1
	adc	#6	;Make ASCII ABCDEF
.noadj1:
	sta	$0002
	sty	$0003
	pla
	rts





Cheat_Exit_Activate:	;Activate cheats if any of the 3 slots is "Correct"
	phx
	clx
	bsr	_is_cheat_on
	bcc     .cheats_turn_on
	ldx	#5
	bsr	_is_cheat_on
	bcc     .cheats_turn_on
	ldx	#10
	bsr	_is_cheat_on
	bcc     .cheats_turn_on
.cheats_turn_off:
	plx
	stz     CHEAT_RAM
	bra     .to_end
;----------
.cheats_turn_on:
	plx
	lda	#'T'
	sta     CHEAT_RAM
;;;	bra     .to_end
.to_end:
	jmp	$D902	;END


_is_cheat_on:	;X points to 0,5,10 cheat offset
	lda    CHEAT_RAM+4,X	;Check for "CHeat" signature!
	cmp	#'C'
	bne	.dead_cheat
	lda    CHEAT_RAM+5,X
	cmp	#'H'
	bne	.dead_cheat
	clc
	rts
.dead_cheat:
	sec	;CARRY means FAIL
	rts

Crsr_Pos_Tbl:	.db 0,8,$10,$18,$38,$40