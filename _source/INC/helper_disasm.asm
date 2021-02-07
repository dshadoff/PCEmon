;--------------------------------------------------------------------
;  Helper files for PCEmon.  They get mapped in at $4000 when needed
;--------------------------------------------------------------------
;;;x	.org $2200
		;Helper magic signature is below. ($4000-$4003)
 .db "HLPR"
		;Then the command name            ($4004-     )
 .db 'D'
		;And a description / help string.
 .db " <AAAA>[BBBB] - Disassemble address range",0

	.org $4040	;Leave space for the string.
Helper_D_Start:
	jsr	_ex_Get_Line_Next_Word	;Get a full word somehow!
	bcc	.no_Param_Error
	jsr	_ex_spcspc
	lda	#'#'
	jsr	_ex_PrintBoth
        stw	#$4004,<_si 		;My message
	jsr	_ex_Print_Text_Both
	jmp	_ex_NewlineBoth
.no_Param_Error:
	;start address now in <_ax
	stw	<_ax,<Start_Address

	jsr	_ex_Get_Line_Next_Word	;Get end address in <_AX!
	bcs     .empty_end_address
 	stw	<_ax,<Stop_Address
	bra	.good_end_address
.empty_end_address:
	stw	#$ffff,<Stop_Address
.good_end_address:
	stz	<Pause_LoopNum
;;;	stw	<Start_Address,<ORIGSTART	;back up starting address


.dis_loop:
	lda	[Start_Address]		;get opcode
	jsr	Print_Cur_Address
	jsr	Copy_Opcode_Operands	;Copy next xxx bytes to storage
	jsr	Disasm_Opcode           ;This advances <Start_Address automatically

;----------------
	IF 0=0
	lda	<Pause_Setting  ;Compare # of lines printed to our Max!
	beq     .no_pause_now	;no pause if Zero
	cmp	<Pause_LoopNum	;This is our max lines
	beq	.do_pause
	bcs	.no_pause_now
.do_pause:
	;ok... here, print a message (that gets cleared)
	stw	#PausedDumpTxt,<_si
	jsr     _ex_Print_Text_Both
;---
	; fake "carriage return"
	jsr	_ex_write_curlineptr	;Go back to start of line?
	lda	#$0D
	jsr	_ex_PrintBoth
;---
	;wait for key press.  If Space, continue, else BAIL!
	jsr	_ex_Serial_In
	cmp	#$20
	bne	.disasm_end
	stz     <Pause_LoopNum	;reset looper
.no_pause_now:
	ENDIF
;----------------

	lda	<Start_Address+1
	beq     .disasm_end		;Look, $0000 is just garbage, OK?

	lda	<Stop_Address+1		;is stop address less than start?
	cmp 	<Start_Address+1
	bcc     .disasm_end
	bne	.dis_loop		;If MSBs not equal, Start < Stop still
	lda	<Stop_Address		;is stop address LSB less than start?
	cmp 	<Start_Address
	bcs	.dis_loop

.disasm_end:
	jsr	_ex_NewlineBoth
	rts



;============= DON'T LOOK BEHIND THE CURTAIN!!! ==================

 ;;;ORIGSTART =    LineBuffer+$0A		;2 bytes - original start add.
SPACECTR =     LineBuffer+$0C		;how many spaces to print to align columns?
OPLENCTR =     LineBuffer+$0D		;how many operand bytes left?
OPCODETYPE =   LineBuffer+$0E		;1 byte
OPCODELEN =    LineBuffer+$0F		;1 byte
OPCODEBUF =    LineBuffer+$10   	;7 bytes max


Print_Cur_Address:
	pha
	jsr	_ex_spcspc
	lda	<Start_Address+1
	jsr	_ex_Print_Byte_Both	;print MSB
	lda	<Start_Address
	jsr	_ex_Print_Byte_Both	;print MSB
	lda	#':'
	jsr	_ex_PrintBoth
	jsr	_ex_spcspc
	pla
	rts


Copy_Opcode_Operands:
	pha
	phx
	phy

	sta	<OPCODEBUF
	tax

	incw	<Start_Address		;Opcode read, now point "PC" after it!

	lda     DIS_Opcode_Type_Length,X        ;Get length, type
	lsr	a				;Isolate Type
	lsr	a
	lsr	a
	lsr	a
	sta     <OPCODETYPE
	lda     DIS_Opcode_Type_Length,X
	and	#$0F				;Isolate length
	sta     <OPCODELEN
	beq	.no_copy_operands		;no more to copy
	tay	;Y will be countdown loop
	clx	;X will be writing index
.copy_operands:
        lda	[Start_Address]
	sta	<OPCODEBUF+1,X
	incw	<Start_Address
	inx
	dey
	bne	.copy_operands

.no_copy_operands:
	ply
	plx
	pla
	rts




Disasm_Opcode:	;Opcode should now be in the buffer, Length, Type as well!
	pha
	phx
	phy

	stz	<SPACECTR	;0 characters printed so far.
	jsr	_print_opcode_name
	jsr	_print_operands
	jsr	_print_hexvals
	jsr	_ex_NewlineBoth
	inc	<Pause_LoopNum	;pause at bottom of screen

	lda	<OPCODEBUF	;get opcode
	cmp	#$40	;RTI?
	beq	.add_newline
	cmp	#$60
	bne	.noadd_newline
.add_newline:
	inc     <Pause_LoopNum
	jsr     _ex_NewlineBoth

.noadd_newline:

	ply
	plx
	pla
	rts




_print_opcode_name:
	lda	<OPCODEBUF	;Get opcode
	sta	<_si		;store temporarily
	stz	<_si+1
	aslw	<_si
	aslw	<_si		;x4
	addw    #DIS_Opcode_Strings,<_si
	cly
.printmnem:
	lda	[_si],Y		;Print opcode mnemonic
	jsr	_ex_PrintBoth
	iny
	cpy	#4
	bne	.printmnem
	jmp	_ex_spc




_print_operands:
	lda	<OPCODELEN      ;how many operands
	beq	.oper_end	;nothing to do!
	sta	<OPLENCTR	;a countdown variable
	clx			;index into operands
	lda     <OPCODETYPE
	asl	a		;Word pointer
	tay
	lda	DIS_String_Tbl,Y
	sta	<_si
	lda	DIS_String_Tbl+1,Y
	sta	<_si+1		;_si now points to operand string!
	cly
;-------------- read operand string!
.rdstringlp:
	lda	[_si],Y
	beq     .oper_end	;All finished if Zero!
	cmp	#$FF
	bne	.not_add_type
	bsr     _get_operand_address
	bra	.rdstring_inc
.not_add_type:
	cmp	#$FE
	bne	.not_rel_type
	bsr     _get_operand_relative
	bra	.rdstring_inc
.not_rel_type:
	cmp	#$FD
	bne	.not_byt_type
	bsr     _get_operand_byte
	bra	.rdstring_inc
.not_byt_type:
	cmp	#$FC
	bne	.not_word_type
	bsr     _get_operand_word
	bra	.rdstring_inc
.not_word_type:		;Print regular text!
	jsr	_ex_PrintBoth
	inc	<SPACECTR	;1 char printed

.rdstring_inc:
	iny
	bra	.rdstringlp
.oper_end:
	rts

		;X still points into operands!
_get_operand_address:
	lda	<OPLENCTR
	cmp	#1	;is there only one left?
	beq	_get_operand_byte
_get_operand_word:
	lda	#'$'
	jsr	_ex_PrintBoth
	inc	<SPACECTR	;1 char printed
	inx	;get word
	lda	<OPCODEBUF+1,X
	jsr	_ex_Print_Byte_Both	;print MSB
	dex
	lda	<OPCODEBUF+1,X
	jsr	_ex_Print_Byte_Both	;print LSB
	lda	<SPACECTR	;4 chars printed
	add	#4
	sta	<SPACECTR
	inx
	inx
	dec     <OPLENCTR
	dec	<OPLENCTR
	rts
_get_operand_byte:
	lda	#'$'
	jsr	_ex_PrintBoth
	inc	<SPACECTR	;1 char printed
	lda	<OPCODEBUF+1,X
	jsr	_ex_Print_Byte_Both	;print LSB
	inc	<SPACECTR	;2 chars printed
	inc	<SPACECTR
	inx
	dec     <OPLENCTR
	rts


_get_operand_relative:
	lda	#'$'
	jsr	_ex_PrintBoth
	inc	<SPACECTR	;1 char printed
	stw	<Start_Address,<_ax	;store into temp.
	stz	<_bh
	lda	<OPCODEBUF+1,X		;Get relative offset
	bpl	.no_adjust_hibyte	;if $80-FF, NEGATIVE!
	dec	<_bh			;now $FF
.no_adjust_hibyte:
	sta	<_bl
	addw	<_bx,<_ax		;result in <_ax!
	lda     <_ah
	jsr	_ex_Print_Byte_Both	;print MSB
	lda     <_al
	jsr	_ex_Print_Byte_Both	;print LSB
	lda	<SPACECTR	;4 chars printed
	add	#4
	sta	<SPACECTR
	inx
	dec     <OPLENCTR

	rts


_print_hexvals: 		;prints the hex values of current opcode
	pha
	phy
	phx

	lda	#$19		;where does COLUMN start?
	sec
	sbc	<SPACECTR       ;$14 - however much we DIDN'T PRINT
	tax
.tab_next_column:
	jsr	_ex_spc
	dex
	bne	.tab_next_column

	lda	#$3B	;a "semicolon"
	jsr	_ex_PrintBoth

	ldy	<OPCODELEN
	iny
	clx
.hexvallp:
	lda     <OPCODEBUF,X
	jsr     _ex_Print_Byte_Both
	jsr	_ex_spc
	inx
	dey
	bne     .hexvallp

	plx
	ply
	pla
	rts





DIS_Opcode_Strings:	;all 256 opcodes, 4 chars each.
 ;    0000111122223333444455556666777788889999aaaabbbbccccddddeeeeffff
 .db "BRK ORA SXY ST0 TSB ORA ASL RMB0PHP ORA ASL ----TSB ORA ASL BBR0"
 .db "BPL ORA ORA ST1 TRB ORA ASL RMB1CLC ORA INC ----TRB ORA ASL BBR1"
 .db "JSR AND SAX ST2 BIT AND ROL RMB2PLP AND ROL ----BIT AND ROL BBR2"
 .db "BMI AND AND ----BIT AND ROL RMB3SEC AND DEC ----BIT AND ROL BBR3"
 ;4X-
 .db "RTI EOR SAY TMA BSR EOR LSR RMB4PHA EOR LSR ----JMP EOR LSR BBR4"
 .db "BVC EOR EOR TAM CSL EOR LSR RMB5CLI EOR PHY --------EOR LSR BBR5"
 .db "RTS ADC CLA ----STZ ADC ROR RMB6PLA ADC ROR ----JMP ADC ROR BBR6"
 .db "BVS ADC ADC TII STZ ADC ROR RMB7SEI ADC PLY ----JMP ADC ROR BBR7"
 ;8X-
 ;    0000111122223333444455556666777788889999aaaabbbbccccddddeeeeffff
 .db "BRA STA CLX TST STY STA STX SMB0DEY BIT TXA ----STY STA STX BBS0"
 .db "BCC STA STA TST STY STA STX SMB1TYA STA TXS ----STZ STA STZ BBS1"
 .db "LDY LDA LDX TST LDY LDA LDX SMB2TAY LDA TAX ----LDY LDA LDX BBS2"
 .db "BCS LDA LDA TST LDY LDA LDX SMB3CLV LDA TSX ----LDY LDA LDX BBS3"
 ;CX-
 .db "CPY CMP CLY TDD CPY CMP DEC SMB4INY CMP DEX ----CPY CMP DEC BBS4"
 .db "BNE CMP CMP TIN CSH CMP DEC SMB5CLD CMP PHX --------CMP DEC BBS5"
 .db "CPX SBC ----TIA CPX SBC INC SMB6INX SBC NOP ----CPX SBC INC BBS6"
 .db "BEQ SBC SBC TAI SET SBC INC SMB7SED SBC PLX --------SBC INC BBS7"

DIS_Opcode_Type_Length:	;"type" (Hnyb) as well as length of operands (Lnyb)
 .db  $11,$71,$00,$11,$21,$21,$21,$21,$00,$11,$00,$00,$22,$22,$22,$92
 .db  $31,$81,$61,$11,$21,$41,$41,$21,$00,$52,$00,$00,$22,$42,$42,$92
 .db  $22,$71,$00,$11,$21,$21,$21,$21,$00,$11,$00,$00,$22,$22,$22,$92
 .db  $31,$81,$61,$00,$41,$41,$41,$21,$00,$52,$00,$00,$42,$42,$42,$92
 ;4X-
 .db  $00,$71,$00,$11,$31,$21,$21,$21,$00,$11,$00,$00,$22,$22,$22,$92
 .db  $31,$81,$61,$11,$00,$41,$41,$21,$00,$52,$00,$00,$00,$42,$42,$92
 .db  $00,$71,$00,$00,$21,$21,$21,$21,$00,$11,$00,$00,$62,$22,$22,$92
 .db  $31,$81,$61,$C6,$41,$41,$41,$21,$00,$52,$00,$00,$72,$42,$42,$92
 ;8X-
 .db  $31,$71,$00,$A2,$21,$21,$21,$21,$00,$11,$00,$00,$22,$22,$22,$92
 .db  $31,$81,$61,$A3,$41,$41,$51,$21,$00,$52,$00,$00,$22,$42,$42,$92
 .db  $11,$71,$11,$B2,$21,$21,$21,$21,$00,$11,$00,$00,$22,$22,$22,$92
 .db  $31,$81,$61,$B3,$41,$41,$51,$21,$00,$52,$00,$00,$42,$42,$52,$92
 ;CX-
 .db  $11,$71,$00,$C6,$21,$21,$21,$21,$00,$11,$00,$00,$22,$22,$22,$92
 .db  $31,$81,$61,$C6,$00,$41,$41,$21,$00,$52,$00,$00,$00,$42,$42,$92
 .db  $11,$71,$00,$C6,$21,$21,$21,$21,$00,$11,$00,$00,$22,$22,$22,$92
 .db  $31,$81,$61,$C6,$00,$41,$41,$21,$00,$52,$00,$00,$00,$42,$42,$92





DIS_String_Tbl:
 .dw DIS_Str0,DIS_Str1,DIS_Str2,DIS_Str3,DIS_Str4,DIS_Str5,DIS_Str6
 .dw DIS_Str7,DIS_Str8,DIS_Str9,DIS_StrA,DIS_StrB,DIS_StrC

;FF = number (byte or word), FE = relative to PC, FD = byte only, FC = word always

DIS_Str0: .db 0
DIS_Str1: .db "#",$FF,0
DIS_Str2: .db $FF,0
DIS_Str3: .db $FE,0
DIS_Str4: .db $FF,",X",0
DIS_Str5: .db $FF,",Y",0
DIS_Str6: .db "[",$FF,"]",0
DIS_Str7: .db "[",$FF,",X]",0
DIS_Str8: .db "[",$FF,"],Y",0
DIS_Str9: .db $FD,",",$FE,0
DIS_StrA: .db "#",$FD,",",$FF,0
DIS_StrB: .db "#",$FD,",",$FF,",X",0
DIS_StrC: .db $FC,",",$FC,",#",$FC,0