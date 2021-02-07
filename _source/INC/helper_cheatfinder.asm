;--------------------------------------------------------------------
;  Helper files for PCEmon.  They get mapped in at $4000 when needed
;--------------------------------------------------------------------
;;;;x	.org $2200
		;Helper magic signature is below. ($4000-$4003)
 .db "HLPR"
		;Then the command name            ($4004-     )
 .db 'T'
		;And a description / help string.
 .db " - interactive Trainer/Cheat finder. Uses CD RAM 80:86.",0

	.org $4040	;Leave space for the string.
Helper_T_Start:
	tma	#3	;Backup MPR at $6000
	pha
	tma	#4	;Backup MPR at $8000
	pha
	tma	#5	;Backup MPR at $A000
	pha
	stwz	<VAL_HINYB	;Clear our Search Value
;------------
Trainer_Restart:
	lda	#$86	;CD-RAM!
	tam	#5	;this will be our reference at $A000
	lda	#$FF
	sta	$A000
	tii	$A000,$A001,$1FFF	;set all RAM to $FFs
	stz	<ITERATIONS
	lda	#$80
	sta	<FIRSTBANK
	sta     <SECONDBANK	;Both 1st/2nd will start at CD bank $80.
	stwz	<NUMRESULTS	;no "results" on startup.



Trainer_MainLoop:
	jsr	Trainer_Srch_Prompt
_trn_wait_keypress:
	jsr	_ex_Serial_In	;Get byte from serial port
	cmp	#'X'
	beq     Helper_T_Exit
	cmp	#'x'
	beq     Helper_T_Exit
.chk_inputu1:
	cmp	#'U'
	bne     .chk_inputu2
	jmp	Helper_T_Upload
.chk_inputu2:
	cmp	#'u'
	bne     .chk_inputr1
	jmp	Helper_T_Upload
.chk_inputr1:
	cmp	#'R'
	bne     .chk_inputr2
	jmp	Helper_T_Restart
.chk_inputr2:
	cmp	#'r'
	bne     .chk_inputv1
	jmp	Helper_T_Restart
.chk_inputv1:
	cmp	#'V'
	bne     .chk_inputv2
	jmp	Helper_T_Value
.chk_inputv2:
	cmp	#'v'
	bne     .chk_inpute1
	jmp	Helper_T_Value
.chk_inpute1:
	cmp	#'E'
	bne     .chk_inpute2
	jmp	Helper_T_Equal
.chk_inpute2:
	cmp	#'e'
	bne     .chk_inputn1
	jmp	Helper_T_Equal
.chk_inputn1:
	cmp	#'N'
	bne     .chk_inputn2
	jmp	Helper_T_NotEqual
.chk_inputn2:
	cmp	#'n'
	bne     .chk_inputl1
	jmp	Helper_T_NotEqual
.chk_inputl1:
	cmp	#'L'
	bne     .chk_inputl2
	jmp	Helper_T_Less
.chk_inputl2:
	cmp	#'l'
	bne     .chk_inputg1
	jmp	Helper_T_Less
.chk_inputg1:
	cmp	#'G'
	bne     .chk_inputg2
	jmp	Helper_T_Greater
.chk_inputg2:
	cmp	#'g'
	bne     .chk_inputs1
	jmp	Helper_T_Greater
.chk_inputs1:
	cmp	#'S'
	bne     .chk_inputs2
	jmp	Helper_T_Show
.chk_inputs2:
	cmp	#'s'
	bne     .chk_inputend
	jmp	Helper_T_Show
.chk_inputend:
	jmp	_trn_wait_keypress
;---------



Helper_T_Exit:
	pla
	tam	#5	;Restore MPRs
	pla
	tam	#4
	pla
	tam	#3
	jsr	_ex_NewlineBoth
	rts



;============= DON'T LOOK BEHIND THE CURTAIN!!! ==================
VAL_HINYB =     LineBuffer+$06
VAL_LOWNYB =     LineBuffer+$07
ITERATIONS =     LineBuffer+$08		;How many times have we done a search?
FIRSTBANK  =     LineBuffer+$09		;Which bank to compare to
SECONDBANK =     LineBuffer+$0A		;Which bank to read from
COMPAREVAL =     LineBuffer+$0B		;fixed value to compare to
FIRSTPTR   =	 LineBuffer+$0C		;(2)
SECONDPTR  =	 LineBuffer+$0E		;(2)
REFPTR     =	 LineBuffer+$10		;(2)
NUMRESULTS =	 LineBuffer+$12		;(2) - how many found for each comparison




Compare_Everything:	;Input: X is compare type
			;Output: Nothing, but report # of matches
;----------
; first, set up all RAM for comparisons
	stw	#$6000,<FIRSTPTR	;Read from this bank, save in "COMPAREVAL"
	stw	#$8000,<SECONDPTR	;Read from this bank
	stw	#$A000,<REFPTR		;Check with this bank
	stwz	<NUMRESULTS		;no "results" on startup.
	lda	<ITERATIONS		;will be 1...6
	add	#$7E                    ;two banks LESS
	tam	#3
	inc	a
	tam	#4			;so we map in $7F,$80 (1 iter)
					;then $80,$81         (2 iters)
					;then $81,$82	      (3 iters)
					;.... $84,$85         (6 iters)
.compare_ev_loop:
	lda	[REFPTR]	;Get "MATCHED" check
	beq	.no_match_so_skip	;if Zero, no need to check.
;--------------- okay, need to compare first with second
	lda	[FIRSTPTR]		;Get first value
	cpx	#0	;is it search value?
	beq	.skip_save_compare
	sta	<COMPAREVAL		;Compare to the address ONLY if ITER > 1
.skip_save_compare:
	lda	[SECONDPTR]		;Get second value
	jsr	Comparison_Logic	;CLC if comparison satisfied, SEC if not
	bcs	.not_match_so_zero	;Zero out reference
;--- match made!
	incw    <NUMRESULTS
	bra     .no_match_so_skip	;that's all!
.not_match_so_zero:
	cla
	sta     [REFPTR]		;our reference pointer now Zero
;-------------------
.no_match_so_skip:
	incw	<FIRSTPTR
	incw    <SECONDPTR
	incw	<REFPTR
	lda	<REFPTR+1		;Get high byte
	cmp	#$C0			;reached end of search area?
	bne     .compare_ev_loop

;	print results here!
	jsr	_ex_NewlineBoth
	jsr	_ex_spcspc
	lda     <NUMRESULTS+1
	beq	.skip_high_byt
	jsr	_ex_Print_Byte_Both
.skip_high_byt:
	lda     <NUMRESULTS
	jsr	_ex_Print_Byte_Both
	stw	#Trn_Found_Txt0,<_si
	jsr	_ex_Print_Text_Both

	rts




;====  HMMmmm, maybe make X-register hold the search type?
; X = 00: search value
; X = 01: search equal
; X = 02: search not equal
; X = 03: search less
; X = 04: search greater
;...

Comparison_Logic:	;Inputs: A, compare to <COMPAREVAL, X = comparison type

	cpx	#0
	beq	.equal_compare
	cpx	#1
	bne	.not_equal_compare
;~~~~~~~~~~ EQUAL HERE ~~~~~~~
.equal_compare:
	cmp	<COMPAREVAL	;compare NEW with OLD
	beq	.compare_success
	bra     .compare_fail
.not_equal_compare:

	cpx	#2
	bne	.not_unequal_compare
;~~~~~~~~~~ UNEQUAL HERE ~~~~~~~
	cmp	<COMPAREVAL	;compare NEW with OLD
	bne	.compare_success
	bra     .compare_fail
.not_unequal_compare:

	cpx	#3
	bne	.not_less_compare
;~~~~~~~~~~ LESS THAN HERE ~~~~~~~
	cmp	<COMPAREVAL	;compare NEW with OLD
	bcc	.compare_success	;if NEW < OLD, carry clear
	bra     .compare_fail
.not_less_compare:


	cpx	#4
	bne	.not_greater_compare
;~~~~~~~~~~ GREATER THAN HERE ~~~~~~~
	cmp	<COMPAREVAL	;compare NEW with OLD
	beq     .compare_fail
	bcs	.compare_success	;if NEW > OLD, carry success
	bra     .compare_fail
.not_greater_compare:


;	cpx	#4
;	bne	.not_greater_compare
;- - - - - - - - - - - - - -
.compare_success:
	clc
	rts
.compare_fail:
	sec
	rts



;==========================================






Helper_T_Upload:
	lda     <ITERATIONS
	cmp	#6		;done 6 uploads?
	beq     .no_more_upload
;------ Upload 8K!
	stz	<Dump_Src_Type	;;xBxxxxMV	;B (temp) buffer, M = MPR (bank), V=VRAM
	stw     #$6000,<Start_Address	;Set up our buffer start
	lda	<ITERATIONS	;Get which iteration 0..5
	add	#$80		;make it bank $80-$85
	tam	#3		;MPR at $6000-
	jsr	_normal_add_upload
	inc     <ITERATIONS
	jmp	Trainer_MainLoop
.no_more_upload:
	jmp	_trn_wait_keypress
;========================================



Helper_T_Restart:
	lda     <ITERATIONS
	beq	.no_need_restart
	jmp	Trainer_Restart		;This will Zero out iterations.
.no_need_restart:
	jmp	_trn_wait_keypress
;=========================================


Helper_T_Value:
	lda     <ITERATIONS
	beq	.cant_compare
	clx
	jsr	Prompt_Compare_Value	;get value from the keyboard!
	jsr	Compare_Everything	;Do the big compare routine!
	jmp	Trainer_MainLoop
.cant_compare:
	jmp	_trn_wait_keypress
;=========================================


Helper_T_Equal:
	lda     <ITERATIONS
	beq	.cant_compare
	cmp	#1		;is iterations # 1?
	beq     .cant_compare
	ldx	#1
	jsr	Compare_Everything	;Do the big compare routine!
	jmp	Trainer_MainLoop
.cant_compare:
	jmp	_trn_wait_keypress
;=========================================


Helper_T_NotEqual:
	lda     <ITERATIONS
	beq	.cant_compare
	cmp	#1		;is iterations # 1?
	beq     .cant_compare
	ldx	#2
	jsr	Compare_Everything	;Do the big compare routine!
	jmp	Trainer_MainLoop
.cant_compare:
	jmp	_trn_wait_keypress
;=========================================


Helper_T_Less:
	lda     <ITERATIONS
	beq	.cant_compare
	cmp	#1		;is iterations # 1?
	beq     .cant_compare
	ldx	#3
	jsr	Compare_Everything	;Do the big compare routine!
	jmp	Trainer_MainLoop
.cant_compare:
	jmp	_trn_wait_keypress
;=========================================

Helper_T_Greater:
	lda     <ITERATIONS
	beq	.cant_compare
	cmp	#1		;is iterations # 1?
	beq     .cant_compare
	ldx	#4
	jsr	Compare_Everything	;Do the big compare routine!
	jmp	Trainer_MainLoop
.cant_compare:
	jmp	_trn_wait_keypress
;=========================================




Helper_T_Show:
	lda     <ITERATIONS
	bne	.show_results
.cant_compare:
	jmp	_trn_wait_keypress
.show_results:
	stw	#$6000,<FIRSTPTR	;Read from this bank
	stw	#$A000,<REFPTR		;Check with this bank
	stz	<Pause_LoopNum		;clear our loops
;----
.show_resu_loop:
	lda	[REFPTR]
	beq     .not_match		;if reference is not #$FF, not a match
	jsr     Print_Found_Address	;Print address for each bank!!!
	inc	<Pause_LoopNum
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
	bne	.print_match_end
	stz     <Pause_LoopNum	;reset looper

	stw	#Trn_Blank_Line_Txt,<_si	;This will print a blank line
	jsr     _ex_Print_Text_Both
	; fake "carriage return"
	jsr	_ex_write_curlineptr	;Go back to start of line?
	lda	#$0D
	jsr	_ex_PrintBoth

.no_pause_now:
	ENDIF
;----------------

.not_match:
	incw	<REFPTR
	incw    <FIRSTPTR
	lda	<REFPTR+1		;Get high byte
	cmp	#$C0			;reached end of search area?
	bne     .show_resu_loop

	stw	#Trn_End_Txt,<_si	;Pause at end of code list!
	jsr     _ex_Print_Text_Both
	jsr	_ex_Serial_In		;Wait for keypress
;--------
.print_match_end:
	jmp	Trainer_MainLoop
;=========================================




Print_Found_Address:	;Prints the address and all matched values
	pha
	jsr	_ex_spcspc
	lda	<FIRSTPTR+1
	and	#$1F			;limit address!
	ora	#$20			;...to $2000-$3FFF
	jsr	_ex_Print_Byte_Both	;print MSB
	lda	<FIRSTPTR
	jsr	_ex_Print_Byte_Both	;print MSB
	lda	#':'
	jsr	_ex_PrintBoth
	jsr	_ex_spcspc
;----------
	clx	;up to iterations
.next_found_byte:
	txa
	add	#$80
	tam	#3	;map to $6000
	lda	[FIRSTPTR]	;Get byte at found address
	jsr	_ex_Print_Byte_Both
	jsr	_ex_spcspc
	inx
	cpx     <ITERATIONS	;finished yet?
	bne	.next_found_byte
;----------
	jsr	_ex_NewlineBoth
	pla
	rts


;=====================================

Prompt_Compare_Value:
	phx
	stw	#Trn_Val_Prompt_Txt,<_si	;Print "header"
	jsr	_ex_Print_Text_Both
	lda	<VAL_HINYB                      ;Print our value
	jsr	_ex_Print_Nybble_Both
	lda	<VAL_LOWNYB
	jsr	_ex_Print_Nybble_Both
	addw	#(Trn_Val_Prompt_Txt_End-Trn_Val_Prompt_Txt),<screen_ptr
					;Position over BYTES to write!

.reset_input_values:
	bsr	_trn_backspaces			;Go back to those nybbles
	clx	;will be our cursor position
.getval_loop:
	jsr	_ex_Serial_In
	cmp	#$0D
	beq	.handle_endline
	cmp	#$0A
	beq	.handle_endline 	;Exit!

	bsr	_trn_toupper		;Convert string to upper case
	jsr	_ex_Conv_Char_to_Nybble
	bcs     .getval_loop		;Refuse it if not $0-$F!
	sta     <VAL_HINYB,X		;Save in position 0 or 1 (H,L)
	jsr     _ex_Print_Nybble_Both	;Print it!!!
	inx
	cpx	#2
	bne	.getval_loop		;Get more values....
	bra     .reset_input_values
;===================
.handle_endline:
	lda	<VAL_HINYB
	asl	a
	asl	a
	asl	a
	asl	a	;Put into $F0 position
	ora     <VAL_LOWNYB
	sta     <COMPAREVAL
	;send out endline (this will take care of more serial bytes coming in...)
	jsr	_ex_NewlineBoth
	plx
	rts


;~~~~


_trn_backspaces:
	jsr	_ex_write_scrnptr	;Go back over these characters
	bbr0	<echo_flags,.no_backspc_trn
	lda	#8			;BACKSPACE CHAR
	jsr	_ex_Serial_Out		;Backspace Out!
	jsr	_ex_Serial_Out
.no_backspc_trn:
	rts


_trn_toupper:		;Converts string to upper case
	cmp	#'a'
	bcc	.lessthan_lower_a
	cmp	#'z'
	beq	.is_lower_z
	bcs	.greater_than_lower_z
.is_lower_z:
	and	#$DF	;remove #$20
.greater_than_lower_z:
.lessthan_lower_a:
	rts


;=====================================

Trainer_Srch_Prompt:
	stw	#Trn_Prompt_Txt0,<_si	;Print "header"
	jsr	_ex_Print_Text_Both
	lda	<ITERATIONS
	jsr	_ex_Print_Nybble_Both	;Print which iteration
	stw	#Trn_Prompt_Txt1,<_si	;Print "header"
	jsr	_ex_Print_Text_Both
	lda     <ITERATIONS
	cmp	#6		;done 6 uploads?
	beq     .no_upload
	stw	#Trn_Prompt_TxtU,<_si
	jsr	_ex_Print_Text_Both
.no_upload:
	lda     <ITERATIONS
	beq	.no_more_prompts	;If Zero, no searching yet!
	stw	#Trn_Prompt_TxtR,<_si
	jsr	_ex_Print_Text_Both
	stw	#Trn_Prompt_TxtV,<_si
	jsr	_ex_Print_Text_Both
	lda     <ITERATIONS
	cmp	#1		;1st upload can't do comparisons
	beq     .no_compare_prompts
	stw	#Trn_Prompt_Txt2,<_si
	jsr	_ex_Print_Text_Both
.no_compare_prompts:
	stw	#Trn_Prompt_TxtS,<_si
	jsr	_ex_Print_Text_Both
.no_more_prompts:
	stw	#Trn_Line_Txt,<_si
	jsr	_ex_Print_Text_Both
	rts

Trn_Prompt_Txt0:	.db $d,$a,$d,$a
			.db " +---------------------------------------------------------+",$d,$a
			.db " | RAM Cheat Search Tool  --  Step ",0
Trn_Prompt_Txt1:	.db "  --  Press a key:     |",$d,$a
			.db " |   X - exit to PCEmon                                    |",$d,$a,0
Trn_Prompt_TxtU:	.db " |   U - upload RAM image                                  |",$d,$a,0
Trn_Prompt_TxtR:	.db " |   R - restart search                                    |",$d,$a,0
Trn_Prompt_TxtV:	.db " |                                                         |",$d,$a
			.db " | -- Comparison Options --                                |",$d,$a
			.db " |  V - equal to numerical value                           |",$d,$a,0
Trn_Prompt_Txt2:	.db " |  E - equal to previous                                  |",$d,$a
			.db " |  N - not equal to previous                              |",$d,$a
			.db " |  L - less than previous                                 |",$d,$a
			.db " |  G - greater than previous                              |",$d,$a,0
Trn_Prompt_TxtS:	.db " |                                                         |",$d,$a
			.db " |  S - show search results                                |",$d,$a,0
Trn_Line_Txt:		.db " +---------------------------------------------------------+",$d,$a,0
Trn_Found_Txt0:		.db " matches found.",$d,$a,0
Trn_Blank_Line_Txt:	.db "                                       ",0
Trn_End_Txt:		.db "  -- end -- PRESS A KEY --",0
Trn_Val_Prompt_Txt:	.db "  Enter Compare Value: "
Trn_Val_Prompt_Txt_End: .db 0		;Needed!


