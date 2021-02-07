PausedDumpTxt:		.db "   (Space to continue, otherwise exit.)",0
RegisterText: 	.db "   PC   A  X  Y  NVTBDIZC  SP  MPR 0  1  2  3  4  5  6  7 (J7)",$D,$A,0
CommandErrorText:	.db "  NOT A VALID COMMAND.",$D,$A,0
ParamErrorText:		.db "  MISSING / BAD PARAMETER(S).",$D,$A,0
;;;;;;;;;UnimplementedText:	.db "  UNIMPLEMENTED COMMAND.",$D,$A,0
BaudSoonText:		.db "  Baud Rate:",0
BufSizeTxt:		.db "  BufSize:$",0
OutFlagsTxt:		.db "  Output Device(s): ",0
LongCommandTxt:		.db $D,$A,"  You've found the command that prints all command strings!",$D,$A,0
PauseSetTxt:		.db "  Hexdump Pause now $",0
PauseSet2Txt:		.db " line(s).",$d,$a,0
SendBinTxt:		.db "  Okay, send your binary!",$d,$a,0
BufEmptyTxt:		.db "  Buffer is empty.",$d,$a,0
BufBRAMErrTxt:		.db "  BRAM in buffer must be $800 bytes.",$d,$a,0
BufFullTxt:		.db "  The buffer can't hold more than $1E00 bytes!",$d,$a,0
IntWarningTxt:		.db $d,$a,"  BRK / Interrupt!  ",0
Recvd1Txt:		.db "  Received $",0
Recvd2Txt:		.db " bytes, saved from $",0
Recvd3Txt:		.db " to $",0
TrainerTxt:		.db "  Trainer (Cheats): ",0
NoTrainerTxt:		.db "  No Trainers Active!",$d,$a,0



Evaluate_Line:	;Now we finally have a line of certain length!
;;;;	stz	<start_state	;clear our BAD state
	ldx	<line_len
	beq	.eval_nothing
	;Now... go through our line and change to uppercase?
	jsr	LineBuf_to_Uppercase
	jsr	Get_Line_Command	;Search through line buffer for command (byte)
	bcs     .eval_nothing           ;If an empty string, ignore!
	;Okay, command is now in A reg.
	sub	#$20	;Make range from !"# to Z[\]^_
	cmp	#$40	;is is past this range?
	bcs     Command_Error
	;Now A is $00-$3F, 40 possible commands.
	sta	<last_command	;Save it!
	jmp	Command_Matrix
.eval_nothing:
	rts


Param_Error:  		;If an error with parameters, say so, then give HELP!
	stw	#ParamErrorText,<_si
	jsr	Print_Text_Both
	lda     <last_command
	jmp     Print_Long_Description	;HELP!

Command_Error:
	stw	#CommandErrorText,<_si
prt_mytx:
	jmp     Print_Text_Both
;---- end ---



Cmd_Mtrx_Tbl:	;64 places to jump to a routine!
	; !"#$%& to '
	.dw 0,Secret_Funcs,0,List_Helpers,0,0,0,0
	;() to /
	.dw Copy_Pal_to_Buf,Copy_Buf_to_Pal,0,0,0,0,0,Print_Short_Descriptions
	;0 to 7
	.dw Set_MPR,Set_MPR,Set_MPR,Set_MPR,Set_MPR,Set_MPR,Set_MPR,Set_MPR
	;8 to ?
	.dw Set_JmpMPR,0,0,0,Copy_BRAM_to_Buf,0,Copy_Buf_to_BRAM,Full_Command_Listing
	;@A to H
	.dw 0,A_Reg_Set,Change_Baud_Rate,Check_Buf,Bin_Dump_Range,0,Fill_Mem,Save_State,Hex_Dump_Range
	;I to O
	.dw 0,Jump_Addr,0,Load_Buffer,Write_Mem,Next_Palette,Toggle_Output_Device
	;P to W
	.dw Pause_Val_Set,0,Dump_Registers,Save_Buffer_Bin,Trainer_Func,Upload_RAW,Write_VDC_Reg,Write_buf_to_RAM
	;X to _
	.dw X_Reg_Set,Y_Reg_Set,0,0,0,0,0,Print_All_Descriptions
;-------------

Command_Matrix:         ;Now A is $00-$3F, 40 possible commands.
	phx             ;Keep X for later
	asl	a	;double A
	tax
	lda	Cmd_Mtrx_Tbl+1,X	;Get high byte of jump table
	beq     .unimplemented
	sta	<tempjmp+1
	lda	Cmd_Mtrx_Tbl,X		;Get low byte of jump table
	sta	<tempjmp
	plx
	jmp	[tempjmp]
.unimplemented: 	;Prints an error if unimplemented routine
	plx
	stw	#CommandErrorText,<_si
	jmp	prt_mytx

Count_Commands:	;Counts how many valid routines
	phx
	phy
	clx	;Loop counter
	cly	;# valid
.cnt_loop:
	lda	Cmd_Mtrx_Tbl+1,X
	beq	.invalid
	iny
.invalid:
	inx
	inx
	cpx	#128	;done all 64 WORD entries?
	bne	.cnt_loop
	tya
	ply
	plx
	rts

;========= COMMANDS IN MATRIX ARE BELOW! ===========

;#######  #######
;#######  #######


;####### ! Secret functions #######
Secret_Funcs:
	jsr	Get_Line_Next 	;Is there another parameter? Carry Clear if Yes.
	bcc	.secret_command
.secret_end:
	rts
.secret_command:
	;Okay, command is now in A reg.
	cmp	#'L' ;low-res
	bne	.notlowres
	jsr	xres_256
	jmp	BAT_6432
.notlowres:
	cmp	#'H' ;high-res
	bne	.nothires
	jsr	xres_512
	jmp	BAT_6432
.nothires:
	cmp	#'M' ;medium-res
	bne	.notmidres
	jsr	xres_320
	jmp	BAT_6432
.notmidres
	cmp	#'<'
	bne	.notadpcm_read
	jmp	Read_ADPCM
.notadpcm_read:
	cmp	#'>'
	bne	.notadpcm_write
	jmp	Write_ADPCM
.notadpcm_write:
	cmp	#'P' ;Play ADPCM?
	bne	.notadpcm_play
	jmp	ADPCM_Play
.notadpcm_play:
	rts



;####### #[C] list/execute helper Command #######
List_Helpers:

;;;	lda	#'T'
;;;	bra     .helper_command


	jsr	Get_Line_Next 	;Is there another parameter? Carry Clear if Yes.
	bcc	.helper_command
	;if no parameter, $FF = search for all
	lda	#$FF
.helper_command:
	sta	<_dl		;Command to search for in <_dl

;---BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
;--- first search BUFFER!
	stw	#Hlpr_String,<_si	;look for "HLPR"
	stw	#BUFFER_LOC,<_di	;usually $2200
	jsr	Compare_String
	bcs     .fail_buf_hlpr		;failed if this function sets Carry.

	;Found one!  Check it!
	lda	<_dl
	cmp	#$FF
	beq	.list_buf_helpers
	;okay, if not list all, compare current with desired helper
	cmp	BUFFER_LOC+$4
	bne     .fail_buf_hlpr
	;FOUND the helper, now JUMP to it!
	jmp	BUFFER_LOC+$40
	;rts
.list_buf_helpers:
	;Print it!
	jsr 	spc
	lda	#'B'
	jsr	PrintBoth
	lda	#'u'
	jsr	PrintBoth
	lda	#'f'
	jsr	PrintBoth
	jsr 	spc

	stw	#BUFFER_LOC+$4,<_si	;Start of command and string
	jsr	Print_Text_Both
	jsr	NewlineBoth
.fail_buf_hlpr:
;----BBBBBBBBBBBBBBBBBBBBBBBBBBBB


	tma	#2	;Backup MPR at $4000
	pha     	;lv 1
;---
	cla
	sec		;This means fail for now.
.srch_list_loop:
	tam	#2	;start at bank 0!
	pha             ;lv 2

	stw	#Hlpr_String,<_si	;look for "HLPR"
	stw	#$4000,<_di
	jsr	Compare_String
	bcs     .fail_this_bank		;failed if this function sets Carry.

	;Found one!  Check it!
	lda	<_dl
	cmp	#$FF
	beq	.list_all_helpers
	;okay, if not list all, compare current with desired helper
	cmp	$4004
	bne     .fail_this_bank
	;FOUND the helper, now JUMP to it!
	jsr	$4040
	pla	;take stored A off the top  ;lv 2
	bra     .helper_end

.list_all_helpers:
	;Print it!
	jsr 	spc
	tma	#2
	jsr	Print_Byte_Both
	jsr 	spcspc

	stw	#$4004,<_si	;Start of command and string
	jsr	Print_Text_Both
	jsr	NewlineBoth
	clc			;Clear carry once found!

;----
.fail_this_bank:

	pla		;lv 2
	inc	a
	cmp	#$88	;Top limit!
	bne     .srch_list_loop
;----
.helper_end:
	pla		;lv 1
	tam	#2
	rts



;####### ( - copy all VCE palettes to buffer #######
Copy_Pal_to_Buf:
	stwz	$0402		;Colour table #0
	tai	$0404,BUFFER_LOC,$0400		;Copy whole VCE to buffer!
	stw	#$0400,<Buffer_Size		;The size of all the palettes
	rts

;####### ) [AAA] - copy buffer to VCE palette index [AAA] or 0 #######
Copy_Buf_to_Pal:
	lda	<Buffer_Size
	ora	<Buffer_Size+1
	beq	.buf_zero_err
	jsr	Get_Line_Next_Word	;Gets index
	bcc	.paladd_set
	stwz	<_ax			;Clear index then
.paladd_set:
	tii	Hard_TIA,SOFT_TIA,8	;copy to soft TIA
	stw	<_ax,$0402		;Point into VCE
	stw	<Buffer_Size,TIA_LEN
	jmp	SOFT_TIA		;quick copy to VCE.
.buf_zero_err:
	stw	#BufEmptyTxt,<_si
	jmp	Print_Text_Both



;####### / - Short command listing and monitor status #######
Print_Short_Descriptions:
	stw	#BufSizeTxt,<_si
	jsr	Print_Text_Both
	lda	<Buffer_Size+1
	jsr	Print_Byte_Both
	lda	<Buffer_Size
	jsr	Print_Byte_Both

	stw	#BaudSoonText,<_si
	jsr	Print_Text_Both
	jsr	print_baud_vars

;;;	lda	<start_state	;Get starting state
;;;	beq     .no_int_start   ;If empty, no interrupts.
;;;	stw	#IntWarningTxt,<_si
;;;	jsr	Print_Text_Both
;;;	lda	<start_state
;;;	jsr	Print_Byte_Both
;;;	jsr 	NewlineBoth
;;;.no_int_start:

	stw	#ShortCommandsTxt,<_si
	jsr	Print_Text_Both

	rts


;####### Sets MPR0..7! #######
Set_MPR:
	jsr	Get_Line_Next_Byte	;Gets 1 hex BYTE
	bcs	.to_Param_Error         ;Byte still in _al

	lda	<last_command
	sub	#$10		;makes it 0..x
	and	#$07		;0..7
	cmp     #7
	beq	.smpr7
	cmp     #6
	beq	.smpr6
	cmp     #5
	beq	.smpr5
	cmp     #4
	beq	.smpr4
	cmp     #3
	beq	.smpr3
	cmp     #2
	beq	.smpr2
	cmp     #1
	beq	.smpr1
	cmp     #0
	beq	.smpr0
	bra	.to_Param_Error
.smpr0:
	lda	<_al
	tam	#0
	bra	.mpr_now_set
.smpr1:
	lda	<_al
	tam	#1
	bra	.mpr_now_set
.smpr2:
	lda	<_al
	tam	#2
	bra	.mpr_now_set
.smpr3:
	lda	<_al
	tam	#3
	bra	.mpr_now_set
.smpr4:
	lda	<_al
	tam	#4
	bra	.mpr_now_set
.smpr5:
	lda	<_al
	tam	#5
	bra	.mpr_now_set
.smpr6:
	lda	<_al
	tam	#6
	bra	.mpr_now_set
.smpr7:
	lda	<_al
	tam	#7
;;;	bra	.mpr_now_set
.mpr_now_set:
;;	jmp     Dump_Registers
	rts
.to_Param_Error:
	jmp     Param_Error


;####### 8 Sets MPR7 on jump! #######
Set_JmpMPR:
	jsr	Get_Line_Next_Byte	;Gets 1 hex BYTE
	bcs	.to_Param_Error         ;Byte still in _al
	sta	<MPR_reg
	rts
.to_Param_Error:
	jmp     Param_Error




;####### < - copy BRAM to buffer #######
Copy_BRAM_to_Buf:
	tma	#2	;Save MPR reg
	pha
	lda	#BRAM_BANK	;(F7)
	tam	#2		;Now at $4000
	csl	;Set LOW CPU speed!
	lda	#$48	;Unlock BRAM!
	sta	$1807
	lda	#$75
	sta	$1807
	lda	#$80
	sta	$1807
	tii	$4000,BUFFER_LOC,$0800	;copy BRAM to RAM
	lda	$1803	;Lock BRAM!
	csh	;Set HIGH CPU speed!
	stw	#$800,<Buffer_Size	;Length of BRAM
	pla
	tam	#2
	rts


;####### > - write buffer to BRAM  *CAREFUL!* #######
Copy_Buf_to_BRAM:
	cmpw	#$800,<Buffer_Size	;Is it the length of BRAM?
	bne	.stored_BRAM_err
	tma	#2	;Save MPR reg
	pha
	lda	#BRAM_BANK	;(F7)
	tam	#2		;Now at $4000
	csl	;Set LOW CPU speed!
	lda	#$48	;Unlock BRAM!
	sta	$1807
	lda	#$75
	sta	$1807
	lda	#$80
	sta	$1807
	tii	BUFFER_LOC,$4000,$0800	;copy RAM to BRAM
	lda	$1803	;Lock BRAM!
	csh	;Set HIGH CPU speed!
	pla
	tam	#2
	rts
.stored_BRAM_err:
	stw	#BufBRAMErrTxt,<_si
	jmp	prt_mytx


;####### ? [C] - Full command listing / [List single commmand] #######
Full_Command_Listing:
	jsr	Get_Line_Next 	;Is there another parameter? Carry Clear if Yes.
	bcc	.to_single_command_list
	jsr     Print_Working_Descriptions
	rts
.to_single_command_list:
	;Okay, command is now in A reg.
	sub	#$20	;Make range from !"# to Z[\]^_
	cmp	#$40	;is is past this range?
	bcs     .to_Command_Error
	jsr	_check_command_exists_to_X
	cpx	#0
	beq     .to_Command_Error
        jmp	Print_Long_Description
.to_Command_Error:
	jmp	Command_Error



;####### A <DD> - set A register #######
A_Reg_Set:
	jsr	Get_Line_Next_Byte	;Gets 1 hex BYTE
	bcs	.to_Param_Error
	sta	<A_reg	;Yay! Save to our storage
	jmp     Dump_Registers
.to_Param_Error:
	jmp     Param_Error

;####### B <0..2> - change Baud rate to 9600,19200,57600 #######
Change_Baud_Rate:
	jsr	Get_Line_Next
	bcs	.to_Param_Error
	sub     #$30			;ASCII to Hex
	cmp	#BAUD_LIMIT		;Too high
	bcs     .to_Param_Error
	jsr	setup_baud_vars		;Input: A: Baud rate 0,1,2...
	stw	#BaudSoonText,<_si
	jsr	Print_Text_Scr_Only
	lda	<echo_flags
	pha
	rmb1    <echo_flags	;turn off RS232 temporarily
	jsr	print_baud_vars
	jsr	NewlineBoth	;JMP here to end!
	pla
	sta	<echo_flags
	rts
.to_Param_Error:
	jmp     Param_Error

;####### C - Check buffer (a hexdump) #######
Check_Buf:
	lda	<Buffer_Size
	ora	<Buffer_Size+1
	beq	.buf_zero_err
	stz	<Dump_Src_Type
	stw	#BUFFER_LOC,<Start_Address
	stw	#BUFFER_LOC,<Stop_Address
	addw	<Buffer_Size,<Stop_Address	;eg $2200+$800=$2A00
	jmp	Hex_Dump_Mid_Entry_Point
.buf_zero_err:
	stw	#BufEmptyTxt,<_si
	jmp	Print_Text_Both



;####### D[V] <AAAA>[BBBB] - binDump / D <MM>:[MM] MPR banks #######
Bin_Dump_Range:
	jmp	Hex_Dump_Range	;We will JUMP here!
				;...and then jump back here!
return_to_bindump:
	;The hex dump stuff set up [V]RAM Start_Address, Stop_Address,
	;MPR start and stop, ...
	;Now just needs to DUMP, maybe from VRAM,
	;possibly INC the MPR, and do it again...

	smb7    <echo_flags	;Suppress Prompt!

.restart_bindump:

	jsr	DumpBin		;Go from Start_Address to Stop_Address

	bbr1    <Dump_Src_Type,.finish_bindump
	;now we are doing MPR dumping.  Check if finished
	stw	#$4000,<Start_Address
	stw	#$6000,<Stop_Address
        lda	<Start_Bank
	cmp	<Stop_Bank
	beq     .mpr_cleanup
	inc	a
	TAM	#2         	;Map in next bank!
	sta	<Start_Bank
	bra     .restart_bindump
;;;	bbs1    <Dump_Src_Type,.mpr_cleanup
.mpr_cleanup:
	lda	<LineBuffer+LINE_MAX-1	;Get back stored MPR
	tam	#2			;Store this!
.finish_bindump:
	stz	<Dump_Src_Type
	rts


;####### F <AAAA> <BBBB> <DD> - Fill range with byte #######
Fill_Mem:
	stz	<Dump_Src_Type
	jsr	Get_Line_Next_Word	;Get a full word somehow!
	bcs	.to_Param_Error
	;Start now in <_ax
	stw	<_ax,<Start_Address
	jsr	Get_Line_Next_Word
	bcs	.to_Param_Error
	;Destination now in <_ax
	stw	<_ax,<Stop_Address

	jsr	Get_Line_Next_Byte
	bcs	.to_Param_Error
	;Fill Byte now in <_ax, and A
	sta	[Start_Address]		;Store our fill byte in Start location
	stw	<Start_Address,<_ax	;Start is, eg. $2300
	incw	<_ax			;Destination is then $2301
	jsr	CopyMem		;Simple copy from RAM (Start-Stop) RAM (_ax)
	rts
.to_Param_Error:
	jmp     Param_Error




;####### G[S/L] - Go to saved game / [Save/upLoad save state] #######
Save_State:
	jsr	Get_Line_Next	;Get next Char/Byte?
	bcs	.to_infinity_and_beyond		;um, jump to save state
	cmp	#'S'
	bne	.not_state_save
;----
	smb7    <echo_flags	;Suppress Prompt!
	stw	#SNAP_RAM,<Start_Address
	stw	#$4000,<Stop_Address	;end of RAM
	stz	<Dump_Src_Type			;Clear special source type
	jmp	DumpBin
;-----
.not_state_save:
	cmp	#'L'
	bne	.not_state_load
;------ Upload SaveState!
	stz	<Dump_Src_Type	;;xBxxxxMV	;B (temp) buffer, M = MPR (bank), V=VRAM
	stw     #SNAP_RAM,<Start_Address	;Set up our buffer start
	stwz	<Buffer_Size		;Our current buffer size
	jmp	_normal_add_upload
.not_state_load:
	cmp	#'R'
	bne	.not_state_raw
;----- Dump Uncompressed RAM!
	jsr	Uncompress_to_Serial
	stw	#$2100,<Start_Address	;start at stack and...
	stw	RLE_DEST,<Stop_Address	;end at start of RLE buffer
	smb7    <echo_flags	;Suppress Prompt!
	stz	<Dump_Src_Type			;Clear special source type
	jsr	DumpBin
;------- send Zeroes instead of RLE data!
	ldx	RLE_LEN		;loop counter to skip the RLE data
	beq	.why_zero	;for some reason, it's zero.  Skip!
	cla
.clr_rle_lp:
	jsr	Serial_Out
	dex
	bne     .clr_rle_lp
.why_zero:
;----------------
	lda     RLE_DEST
	add     RLE_LEN
	sta     <Start_Address		;Point to AFTER RLE data
	lda     RLE_DEST+1
	adc	#0
	sta     <Start_Address+1
	smb7    <echo_flags	;Suppress Prompt!
	stw	#$4000,<Stop_Address	;end of RAM
	stz	<Dump_Src_Type			;Clear special source type
	jmp	DumpBin
.not_state_raw:
	rts


.to_infinity_and_beyond:
;;	jsr	xres_256	;Restore lores
;;	jsr	BAT_3264
	jsr	Uncompress_ZP
	jmp	Restore_Savestate


;####### H[V] <AAAA>[BBBB] - Hexdump add. / H <MM>:[MM] MPR #######
;This will be a long one!
;Don't need linebuffer during hexdump, maybe use it here?
Hex_Dump_Range:

	stz	<Dump_Src_Type	;;xxxxxxMV	;M = MPR (bank), V=VRAM

	jsr	Get_Line_Next	;Get next Char/Byte?
	bcs	.to_Param_Error	;empty line!
	dex	;Back to start of Char
	cmp	#'V'
	beq	.vram_dumping
	bra	.no_vram_hexdump
.to_Param_Error:
	jmp     Param_Error

.vram_dumping: 	;THIS IS WORKING FOR NOW, SET SOMETHING HERE AND CONTINUE
	lda	#1	;VRAM and ONLY VRAM
	sta	<Dump_Src_Type
	inx	;GO PAST 'V'
.no_vram_hexdump:
	jsr	Get_Line_Next_Word	;Get a full word somehow!
	bcs	.to_Param_Error

	stw	<_ax,<Start_Address	;Will be returned in <_ax

	;NOW PEEK AT "Colon"?
	jsr	Get_Line_Next		;Seek to next thing on line
	bcs     .empty_stop_add		;.to_Param_Error ;if nothing, treat as endless dump...?
	cmp	#':'
	beq	MPR_hexdump
	dex				;If not colon, go back and try to read.

	jsr	Get_Line_Next_Word	;Get NEXT word????!??
	bcc	.fine_stop_add     ;.to_Param_Error
.empty_stop_add:
	;Hmm.. no stop address... set it to $FFFF if RAM and $7FFF if VRAM?
	bbs0	<Dump_Src_Type,.vram_stopadd
        stwz	<_ax	;stw	#$FFFF,<_ax	;whole 64k 6502 range!
	bra     .fine_stop_add
.vram_stopadd:
        stw	#$8000,<_ax	;whole 32kWord VDC range! (was 7FFF)
.fine_stop_add:
	stw	<_ax,<Stop_Address

Hex_Dump_Mid_Entry_Point:
	lda	<last_command	;Are we D or H?
	cmp     #'D'-$20
	bne	.hex_continue_mid_entry
	jmp     return_to_bindump
;~~~~~~
.hex_continue_mid_entry:
	lda	<Start_Address
	and	#$F0		;Make sure starts at #X0
	sta     <Start_Address

	lda	<Stop_Address	;*TINY* bug here if the STOP address is 0000.
	and	#$0F		;Did they type in $xx00?
	beq	.good_stop_address
				;nope, bad, so...
	lda	<Stop_Address
	ora	#$0F		;raise lowest nybble
	sta     <Stop_Address
	incw	<Stop_Address
.good_stop_address:
	bra	Start_hexdump


MPR_hexdump:	;THIS IS WORKING FOR NOW, SET SOMETHING HERE AND CONTINUE
	bbr0    <Dump_Src_Type,.continue_mpr
	jmp	Param_Error 		;Can't have both VRAM and MPRs!
.continue_mpr:
	lda	#2		;MPR and ONLY MPR!
	sta	<Dump_Src_Type
	lda	<_al
	sta     <Start_Bank
        jsr	Get_Line_Next_Byte	;Assume it's a BANK
	bcc     .next_bank_byte         ;if found something, print it!
					;otherwise, set a default.
	lda	<Start_Bank		;which is its starting bank
.next_bank_byte:
	sta	<Stop_Bank
.start_mpr_hexdump:
	stw	#$4000,<Start_Address	;Will load into MPR #2
	stw	#$6000,<Stop_Address
	tma	#2			;Store this!
	sta	<LineBuffer+LINE_MAX-1	;Store at top of line buffer... Hmm.
	lda	<Start_Bank
	TAM	#2         	;Map in first bank!

;-- BIN Dumping!
	lda	<last_command	;Are we D or H?
	cmp     #'D'-$20
	bne	Start_hexdump
	jmp     return_to_bindump
;---------


Start_hexdump:

	stz	<Pause_LoopNum	;Reset loops

;================ Check at the start if we need to Stop Dumping! ======
.top_of_hex_loop:
	cmpw    <Start_Address,<Stop_Address
	beq     .check_next_mpr_or_exit_hexdump
	bra	.continue_hexdump
	rts
.bail_dumping:  ;Exit, but print newline first
	jsr	NewlineBoth
	bbs1    <Dump_Src_Type,.mpr_cleanup
	stz	<Dump_Src_Type
	rts
.check_next_mpr_or_exit_hexdump:
	bbr1    <Dump_Src_Type,.finish_hexdump
	;now we are doing MPR dumping.  Check if finished
	stw	#$4000,<Start_Address
	stw	#$6000,<Stop_Address
        lda	<Start_Bank
	cmp	<Stop_Bank
	beq     .mpr_cleanup
	inc	a
	TAM	#2         	;Map in next bank!
	sta	<Start_Bank
	bra     .continue_hexdump
.mpr_cleanup:
	lda	<LineBuffer+LINE_MAX-1	;Get back stored MPR
	tam	#2			;Store this!
.finish_hexdump:	;We are DONE!
	stz	<Dump_Src_Type
	rts
;================

.continue_hexdump:
	lda	<Pause_Setting  ;Compare # of lines printed to our Max!
	beq     .no_pause_now	;no pause if Zero
	cmp	<Pause_LoopNum	;This is our max lines
	bne	.no_pause_now
	;ok... here, print a message (that gets cleared)
	stw	#PausedDumpTxt,<_si
	jsr     Print_Text_Both
;---
	; fake "carriage return"
	jsr	write_curlineptr	;Go back to start of line?
	lda	#$0D
	jsr	PrintBoth
;---
	;wait for key press.  If Space, continue, else BAIL!
	jsr	Serial_In
	cmp	#$20
	bne	.bail_dumping
	stz     <Pause_LoopNum	;reset looper
.no_pause_now:

	;The actual reading, dumping part:
;vvvvvvvvvvvvvvvvvvvvv
	bbr0	<Dump_Src_Type,.no_vaddr_set	;VRAM reading?
	stw	<Start_Address,<_di     ;Start address will be VRAM WORDs!
	jsr	set_read		;Read from VRAM!
.no_vaddr_set:
;vvvvvvvvvvvvvvvvvvvvv

	cly
.read_line_loop:
;vvvvvvvvvvvvvvvvvvvvv
	bbr0	<Dump_Src_Type,.src_is_ram	;Skip this if not VRAM!
	lda	video_data
	sta     LineBuffer,Y
	iny
	lda	video_data+1
	bra	.write_temp_linebuf
;vvvvvvvvvvvvvvvvvvvvvvv
.src_is_ram:
	lda	[Start_Address],Y
.write_temp_linebuf:
	sta     LineBuffer,Y	;needs to be Absolute here, oh well.
	iny
	cpy	#$10
	bne     .read_line_loop	;OK, buffer now has 16 stable bytes

	jsr	spc
;--- change here if printing MPRs
	bbs1    <Dump_Src_Type,.mpr_print_bank
	jsr	spc
	jsr	spcspc
        lda	Start_Address+1 	;Don't forget to print the address!
	bra	.non_mpr_print_add
;-----------
.mpr_print_bank:
	lda	<Start_Bank
	jsr     Print_Byte_Both
	lda     #':'
	jsr     PrintBoth	;Print Char
        lda	Start_Address+1 	;Don't forget to print the address!
	and	#$1F		;Remove $4xxx or $5xxx
;----------------------
.non_mpr_print_add:
	jsr     Print_Byte_Both
        lda	Start_Address
	jsr     Print_Byte_Both

	lda     #':'
	jsr     PrintBoth	;Print Char
	jsr	spc


	clx
.print_hexline_loop:
	cpx	#4         ;Print a separator space every 4 Bytes
	beq	.hex_spacer
	cpx	#8
	beq	.hex_spacer
	cpx	#12
	bne	.hex_nospacer
.hex_spacer:
	jsr	spc
.hex_nospacer:
	lda	<LineBuffer,X
	jsr     Print_Byte_Both
	inx
	cpx	#$10
	bne     .print_hexline_loop

	jsr	spc
	lda     #'|'
	jsr     PrintBoth	;Print Char

 	clx
.print_ascline_loop:
	lda	<LineBuffer,X
	jsr     Print_Safe_Char		;Make it a printable character!
	inx
	cpx	#$10
	bne     .print_ascline_loop

	jsr	NewlineBoth

	inc	<Pause_LoopNum
	bbs0	<Dump_Src_Type,.vram_inc_addr	;Only increase VRAM by 8!
	addw	#$0008,<Start_Address
.vram_inc_addr:
	addw	#$0008,<Start_Address
	jmp	.top_of_hex_loop
;###### LOONG HEXDUMP END #######


;####### J <AAAA> - Jump to (execute) address #######
Jump_Addr:
	jsr	Get_Line_Next_Word	;Gets a Word length
	bcs	.to_Param_Error
	;Word is in <_ax, so we'll jump there

	ldx	<X_reg
	ldy	<Y_reg
	lda	<A_reg	;Load up all our registers

	bsr	.jumpoff_point

	stx	<X_reg  ;Restore all our registers
	sty	<Y_reg
	sta	<A_reg
	tsx
	dex
	dex
	txs
	pla     ;Get return address
	sta	<PC_reg		;Store it as the PC
	pla
	sta	<PC_reg+1

	php
	pla
	sta	<P_reg

	jmp	Dump_Registers
.to_Param_Error:
	jmp     Param_Error
.jumpoff_point:	
	tii     HardRAM_Jumpoff,LineBuffer,60	;copy code to RAM
	jsr	LineBuffer	;RAM_Jumpoff
	rts


HardRAM_Jumpoff:	;This will be copied to RAM and executed
	pha             ;push A reg
	tma	#7	;store MPR and push it!
	pha
	lda	<MPR_reg
	tam	#7	;Map in our JUMP MPR!
	pla             ;pop old MPR
	sta	<MPR_reg ;save that!
	pla             ;pop A reg
	bsr	.soft_landing	;subroutine there, returns here!
	pha	;push A reg
	tma	#7	;get Jump MPR and push it!
	pha
	lda	<MPR_reg ;get old MPR
	tam	#7	;restore to MPR!
	pla		;pop Jump MPR
	sta	<MPR_reg ;save it again.

	pla	;pop A reg
	rts

.soft_landing:
	jmp	[_ax]
	;bye-bye!!!


;####### L <size> - Load buffer up to $1E00 bytes #######
Load_Buffer:
	jsr	Get_Line_Next_Word	;Gets a Word length
	bcs	.to_Param_Error
	lda	<_al
	ora	<_ah
	beq     .to_Param_Error		;no good if 0 bytes
	cmpw	#BUFFER_MAX,<_ax	;no good if too big
	beq	.ok_size
	bcs     .to_Param_Error
.ok_size:
	stw	#SendBinTxt,<_si
	jsr	prt_mytx
	jsr	NewlineBoth	;JMP here to end!

	stw     #BUFFER_LOC,<Start_Address	;Set up our buffer start
	stw	#BUFFER_LOC,<Stop_Address
	addw	<_ax,<Stop_Address              ;And finish in our buffer.
	stw	<_ax,<Buffer_Size		;Our current buffer size
	jsr	ReadBin	;Read binary from serial
	rts
.to_Param_Error:
	jmp     Param_Error

;####### M <AAAA> <DD> [DD] [DD] [DD]... - write hex to Memory #######
Write_Mem:
	jsr	Get_Line_Next_Word	;Gets a Word length
	bcs	.to_Param_Error
	stw     <_ax,<_di		;save destination here

	jsr	Get_Line_Next_Byte 	;Get at least one data byte...
	bcs	.to_Param_Error
	phy
	cly
.write_mem_lp:
;;;;	lda	<_al			;Data word here...
	sta	[_di],Y                 ;Write it!
	incw	<_di
	jsr	Get_Line_Next_Byte 	;Get next data byte(s)
	bcc	.write_mem_lp           ;if no error, loop and write!
;-- exit!
	ply
	rts
.to_Param_Error:
	jmp     Param_Error


;####### N - Next screen palette #######
Next_Palette:
	inc	<palnum
	lda	<palnum		;set one of 8 palettes
	jsr	change_pal
	rts


;####### O [E/R/S] - Print [toggle] Output: Echo/Screen/RS-232 #######
Toggle_Output_Device:
	jsr	Get_Line_Next	;(Now A has that command, X points to next. Carry Clear)
	bcs	.cont_toggle_output	;if empty line, now print Echo!  ;.to_Param_Error
	cmp	#'E'		;bit 0, "Echo"
	beq	.toggle_echo
	cmp	#'R'
	beq	.toggle_rs232_msgs
	cmp	#'S'
	beq	.toggle_screen_msgs
	bra     .to_Param_Error	;It's none of these, It's a BAD line!
.toggle_screen_msgs:
	;This is toggle Screen
	lda	#%00000100
	eor	<echo_flags
	sta	<echo_flags
	bra	Toggle_Output_Device	;.cont_toggle_output
.toggle_rs232_msgs:
	;This is toggle RS232 messages
	lda	#%00000010
	eor	<echo_flags
	sta	<echo_flags
	bra	Toggle_Output_Device	;.cont_toggle_output
.toggle_echo:
	;This is toggle RS232 Echo
	lda	#%00000001
	eor	<echo_flags
	sta	<echo_flags
	bra	Toggle_Output_Device
.cont_toggle_output:
	stw	#OutFlagsTxt,<_si
	jsr	prt_mytx
	jsr	Print_Output_Flags
	jmp	NewlineBoth	;JMP here to end!
.to_Param_Error:
	jmp     Param_Error

;####### P <DD> - set # of hexdump lines before Pause (00 = no pause) #######
Pause_Val_Set:
	jsr	Get_Line_Next_Byte	;Gets 1 hex BYTE
	bcs	.to_Param_Error
	sta	<Pause_Setting	;Yay! Save to our storage
	stw	#PauseSetTxt,<_si
	jsr	prt_mytx
	lda	<Pause_Setting
	jsr	Print_Byte_Both
	stw	#PauseSet2Txt,<_si
	jmp	prt_mytx
.to_Param_Error:
	jmp     Param_Error



;#######  R - display CPU Registers #######
Dump_Registers:
	stw	#RegisterText,<_si
	jsr     prt_mytx
	jsr	spcspc
	lda	<PC_reg+1	;Program Counter
	jsr	Print_Byte_Both
	lda	<PC_reg
	jsr	Print_Byte_Both
	jsr	spc
	lda	<A_reg          ;Print registers
	jsr	Print_Byte_Both
	jsr	spc
	lda	<X_reg
	jsr	Print_Byte_Both
	jsr	spc
	lda	<Y_reg
	jsr	Print_Byte_Both
	jsr	spcspc
	lda	<P_reg		;Processor status
	jsr	Print_Binary_Both
	jsr	spcspc
	lda	<SP_reg		;Stack pointer
	jsr	Print_Byte_Both
	jsr	spcspc
	jsr	spcspc
	jsr	spc
;------- now print MPRs!
	tma	#0
	jsr	Print_Byte_Both
	jsr	spc
	tma	#1
	jsr	Print_Byte_Both
	jsr	spc
	tma	#2
	jsr	Print_Byte_Both
	jsr	spc
	tma	#3
	jsr	Print_Byte_Both
	jsr	spc
	tma	#4
	jsr	Print_Byte_Both
	jsr	spc
	tma	#5
	jsr	Print_Byte_Both
	jsr	spc
	tma	#6
	jsr	Print_Byte_Both
	jsr	spc
	tma	#7
	jsr	Print_Byte_Both
	jsr	spcspc
	lda	<MPR_reg	;get stored MPR7!
	jsr     Print_Byte_Both
	jmp	NewlineBoth	;JMP here to end!

;####### S - Save buffer to binary #######
Save_Buffer_Bin:
	smb7    <echo_flags	;Suppress Prompt!
	stw	#BUFFER_LOC,<Start_Address
	stw	#BUFFER_LOC,<Stop_Address
	addw	<Buffer_Size,<Stop_Address	;eg $2200+$800=$2A00
	stz	<Dump_Src_Type			;Clear special source type
	jmp	DumpBin




;#######  T [0/1/2 [AAAA DD]] - view (clear/set add. for) cheat Trainer  #######
Trainer_Func:
	jsr	Get_Line_Next_Byte	;Get 0 1 or 2?
	bcs     .list_trainers          ;if no option, print trainer list
	cmp     #2		;is it 0-2?
	beq	.set_trainernum
	bcs	.to_Param_Error
.set_trainernum:
        sta	<Stop_Address+1	;Trainer number here
	asl	a
	asl	a
	ora     <Stop_Address+1	;multiply 0-2 by 5!
	sta     <Stop_Address+1
        jsr	Get_Line_Next_Word	;get the address for the cheat!
        bcs	.remove_trainer		;no parameter means CLEAR the cheat!

	stw	<_ax,<Start_Address	;put cheat address here
        jsr	Get_Line_Next_Byte
	bcs	.to_Param_Error
	sta     <Stop_Address	;store cheat value here!
;------- now everything OK!
	lda	#'T'
	sta     CHEAT_RAM	;Trainers ACTIVE!
	phx
	ldx     <Stop_Address+1		;index into cheats
;=======
	lda	#'C'		;Write "CHeat" signature!
	sta    CHEAT_RAM+4,X
	lda	#'H'
	sta    CHEAT_RAM+5,X
	lda	<Start_Address	;get cheat add LO
	sta    CHEAT_RAM+1,X
	lda	<Start_Address+1 ;get cheat add HI
	sta    CHEAT_RAM+2,X
	lda	<Stop_Address	;get cheat DATA
	sta    CHEAT_RAM+3,X
;=======
	plx
	;rts
	bra	.list_trainers
;-----
.remove_trainer:
	phx
	ldx     <Stop_Address+1		;index into cheats
	stz     CHEAT_RAM+5,X
	stz	CHEAT_RAM+4,X
	plx
	bra	.list_trainers
;----------
.to_Param_Error:
	jmp     Param_Error
.list_trainers:
	lda	CHEAT_RAM
	cmp	#'T'	;for trainer
	bne	.no_cheats

	stw	#TrainerTxt,<_si
	jsr	Print_Text_Both
	phx
	clx
	bsr	_list_cheat
	bsr	_list_cheat
	bsr	_list_cheat
	plx
	jmp	NewlineBoth	;JMP here to end!
;---
.no_cheats:
	stw	#NoTrainerTxt,<_si
	jmp	Print_Text_Both
;---
_list_cheat:
	lda    CHEAT_RAM+4,X	;Check for "CHeat" signature!
	cmp	#'C'
	bne	.dead_cheat
	lda    CHEAT_RAM+5,X
	cmp	#'H'
	bne	.dead_cheat
	;cheat seems valid, so list it!
	lda	#'$'
	jsr	PrintBoth
	lda    CHEAT_RAM+2,X
	jsr	Print_Byte_Both
	lda    CHEAT_RAM+1,X
	jsr	Print_Byte_Both
	lda	#':'
	jsr	PrintBoth
	lda    CHEAT_RAM+3,X
	jsr	Print_Byte_Both
	bra	.done_cheat
.dead_cheat:
	lda	#'x'
	jsr	PrintBoth
.done_cheat:
	jsr	spcspc
	txa
	add	#5
	tax
	rts

; Cheats:  0   1 2  3  4   5   6 7  8  9   A   B C  D  E   F
;         'T' LLHH DD 'C' 'H' LLHH DD 'C' 'H' LLHH DD 'C' 'H'



;####### U (pload directly to buffer/VRAM/RAM BANK! #######
Upload_RAW:
	stz	<Dump_Src_Type	;;xBxxxxMV	;B (temp) buffer, M = MPR (bank), V=VRAM

	jsr	Get_Line_Next	;Get next Char/Byte?
	bcc	.try_vram	;empty line = upload to buffer

	;This MEANS UPLOAD TO BUFFER!
	lda	#%01000000
	sta     <Dump_Src_Type	;Set "BUFFER"!
	stw     #BUFFER_LOC,<Start_Address	;Set up our buffer start
	stwz	<Buffer_Size		;Our current buffer size
	bra	_normal_add_upload
.try_vram:
	dex	;Back to start of Char
	cmp	#'V'
	beq	.vram_upload
	bra	.no_vram_upload
.to_Param_Error:
	jmp     Param_Error

.vram_upload: 	;THIS IS WORKING FOR NOW, SET SOMETHING HERE AND CONTINUE
	lda	#1	;VRAM and ONLY VRAM
	sta	<Dump_Src_Type
	inx	;GO PAST 'V'
.no_vram_upload:
	jsr	Get_Line_Next_Word	;Get a full word somehow!
	bcs	.to_Param_Error

	stw	<_ax,<Start_Address	;Will be returned in <_ax

	;NOW PEEK AT "Colon"?
	jsr	Get_Line_Next		;Seek to next thing on line
	bcs     _normal_add_upload	;If nothing after, it's a normal address
	cmp	#':'
	bne	_normal_add_upload	;If not colon, treat as normal


.mpr_upload:
	bbr0    <Dump_Src_Type,.continue_mpr
	jmp	Param_Error 		;Can't have both VRAM and MPRs!
.continue_mpr:
	lda	#2		;MPR and ONLY MPR!
	sta	<Dump_Src_Type
	lda	<_al
	sta     <Start_Bank

.start_mpr_upload:
	stw	#$4000,<Start_Address	;Will load into MPR #2
;;;;;;	stw	#$6000,<Stop_Address
	tma	#2			;Store this!
	sta	<LineBuffer+LINE_MAX-1	;Store at top of line buffer... Hmm.
	lda	<Start_Bank
	TAM	#2         	;Map in first bank!


_normal_add_upload:
	stw	<Start_Address,<Stop_Address	;store temporarily
	stw	#SendBinTxt,<_si
	jsr	prt_mytx
	jsr	NewlineBoth	;JMP here to end!



	;Execute one of 3 (hopefully fast) upload types
	bbs1    <Dump_Src_Type,.mpr_upload_start
	bbs0    <Dump_Src_Type,.vram_upload_start
	;oh, so it's a regular one in RAM/Buffer
	jsr	Upload_Regular
	bbs6    <Dump_Src_Type,.bufread_cleanup
.regular_dump_exit:
	stz	<Dump_Src_Type
	stw     <Start_Address,<_ax	;Ending add here
	subw	<Stop_Address,<_ax	;End-Start = size
	jmp	upload_print_size	;Jump below a bit.
;-------
.vram_upload_start:
	stw	<Start_Address,<_di
	jsr	set_write		;set VRAM address!
	jsr	Upload_Irregular_VRAM
	stz	<Dump_Src_Type
	rts
.mpr_upload_start:
	jsr	Upload_Irregular_MPR
	lda	<LineBuffer+LINE_MAX-1	;Get back stored MPR
	tam	#2			;Store this!
	stz	<Dump_Src_Type
	jmp	Upload_Report_MPR

.bufread_cleanup:
;Buffer read here:
	stz	<Dump_Src_Type
	;check if oversize
	subw	#BUFFER_LOC,<Start_Address
	;Compare result! ---
	lda	#LOW(BUFFER_MAX)
	sec
	sbc	<Start_Address
	lda	#HIGH(BUFFER_MAX)
	sbc	<Start_Address+1
	;-----
	bcc	oversize_buf
	stw     <Start_Address,<Buffer_Size
	stw     <Buffer_Size,<_ax		;Temp storage for printing...
        addw	#BUFFER_LOC,<Start_Address	;This is the final address!
upload_print_size:
	decw    <Start_Address			;add back original location

	stw	#Recvd1Txt,<_si 		;Print buffer size!
	jsr	Print_Text_Both
	lda	<_ax+1
	jsr	Print_Byte_Both
	lda	<_ax
	jsr	Print_Byte_Both

	stw	#Recvd2Txt,<_si			;Print starting address
	jsr	Print_Text_Both
	lda	<Stop_Address+1
	jsr	Print_Byte_Both
	lda	<Stop_Address
	jsr	Print_Byte_Both

	stw	#Recvd3Txt,<_si			;Print ending address
	jsr	Print_Text_Both
	lda	<Start_Address+1
	jsr	Print_Byte_Both
	lda	<Start_Address
	jsr	Print_Byte_Both
	jsr	NewlineBoth	;JMP here to end!

	rts
oversize_buf:
	stw     #BUFFER_MAX,<Buffer_Size	;Set buffer to max
	stw	#BufFullTxt,<_si
	jmp	Print_Text_Both

Upload_Report_MPR:	;<Start_Address right now contains the end add ($4000...$5FFF)
			;<Start_Bank and <Stop_Bank contain MPR values
	cmpw	#$4000,<Start_Address	;Did we do exactly 8192/16384... bytes?
	bne	.no_dec_mpr
	dec     <Stop_Bank              ;Go BACK one bank
	stw	#$6000,<Start_Address	;Stop address is here...
.no_dec_mpr:
	subw	#$4000,<Start_Address	;now $0001-$2000
	stw     <Start_Address,<_ax	;file size here...
	decw	<Start_Address          ;now $0000-$1FFF

	;gotta add # of banks to total file size
	lda	<Stop_Bank
	sub     <Start_Bank	;eg $82-$80 = 2
	sta	<_bh		;high byte
	stz	<_bl
	lsrw	<_bx
	lsrw	<_bx
	lsrw	<_bx		;divide by 8!
	lda     <_ah		;each bank is worth $2000 (in _ah)
	add     <_bl
	sta	<_ah
	lda	<_bh
	adc	#0
	sta	<_bh

	stw	#Recvd1Txt,<_si 		;Print file size!
	jsr	Print_Text_Both
	lda	<_bh
	jsr	Print_Byte_Both
	lda	<_ah
	jsr	Print_Byte_Both
	lda	<_al
	jsr	Print_Byte_Both

	stw	#Recvd2Txt,<_si			;Print starting address
	jsr	Print_Text_Both
	lda	<Start_Bank			;print starting bank
	jsr	Print_Byte_Both
	lda	#':'
	jsr	PrintBoth
	cla
	jsr	Print_Byte_Both
	cla					;Starting at $0000!
	jsr	Print_Byte_Both

	stw	#Recvd3Txt,<_si			;Print ending address
	jsr	Print_Text_Both
	lda	<Stop_Bank			;print ending bank
	jsr	Print_Byte_Both
	lda	#':'
	jsr	PrintBoth
	lda	<Start_Address+1
	jsr	Print_Byte_Both
	lda	<Start_Address
	jsr	Print_Byte_Both
	jsr	NewlineBoth	;JMP here to end!
	rts





;####### V <RR> <DDDD> - write data to VDC register #######
Write_VDC_Reg:
	jsr	Get_Line_Next_Byte	;Gets 1 hex BYTE
	bcs	.to_Param_Error
	pha
	jsr	Get_Line_Next_Word	;Gets 1 hex WORD
	pla
	bcs	.to_Param_Error
	;A contains register for now... write it!
	sta	$0000			;VDC Address register
	cmp	#9		;is it BAT size?
	bne	.not_batsize	;If it is, save the VAR for later
	lda     <_al		;the reg with the VDC data.
	jsr	Store_BAT_Size
.not_batsize:
	stw	<_ax,$0002		;VDC Data register
	rts
.to_Param_Error:
	jmp     Param_Error




;####### W[V] <AAAA> - Write buffer to [V]RAM address #######
Write_buf_to_RAM:
	lda	<Buffer_Size
	ora	<Buffer_Size+1
	beq	.buf_zero_err
	stz	<Dump_Src_Type
	stw	#BUFFER_LOC,<Start_Address
	stw	#BUFFER_LOC,<Stop_Address
	addw	<Buffer_Size,<Stop_Address	;eg $2200+$800=$2A00

	jsr	Get_Line_Next	;Get next Char/Byte?
	bcs	.to_Param_Error	;empty line!
	dex	;Back to start of Char
	cmp	#'V'
	beq	.vram_copy
	bra	.no_vram_copy
.to_Param_Error:
	jmp     Param_Error

.vram_copy:
	lda	#1	;VRAM and ONLY VRAM
	sta	<Dump_Src_Type
	inx	;GO PAST 'V'
	bbr0	<Buffer_Size,.no_vram_copy	;is the buffer an ODD size?
	incw	<Stop_Address
.no_vram_copy:
	jsr	Get_Line_Next_Word	;Get a full word somehow!
	bcs	.to_Param_Error

	;Destination now in <_ax
	jsr	CopyMem
	stz	<Dump_Src_Type
	rts

.buf_zero_err:
	stw	#BufEmptyTxt,<_si
	jmp	Print_Text_Both

;####### X <DD> - set X register #######
X_Reg_Set:
	jsr	Get_Line_Next_Byte	;Gets 1 hex BYTE
	bcs	.to_Param_Error
	sta	<X_reg	;Yay! Save to our storage
	jmp     Dump_Registers
.to_Param_Error:
	jmp     Param_Error

;####### Y <DD> - set Y register #######
Y_Reg_Set:
	jsr	Get_Line_Next_Byte	;Gets 1 hex BYTE
	bcs	.to_Param_Error
	sta	<Y_reg	;Yay! Save to our storage
	jmp     Dump_Registers
.to_Param_Error:
	jmp     Param_Error






;#######  #######
;#######  #######




Read_ADPCM:
	lda	#$80
	sta	$180D
	stz	$180D	;reset read/write address
.garb_wait:
	bit	$180C	;wait for ADPCM driver to be free
	bmi	.garb_wait
	lda	$180A	;discard Garbage Read!

	lda	#2		;MPR and ONLY MPR!
	sta	<Dump_Src_Type
	lda	#$80
	sta     <Start_Bank
	stw	#$4000,<Start_Address	;Will load into MPR #2
	tma	#2			;Store this!
	sta	<LineBuffer+LINE_MAX-1	;Store at top of line buffer... Hmm.
	lda	<Start_Bank
	TAM	#2         	;Map in first bank!

Copy_ADPCM_to_RAM:
.ad_ready_wait:
	bit	$180C	;wait for ADPCM driver to be free
	bmi	.ad_ready_wait
	lda	$180A	;Read ADPCM byte!
;---
	sta	[Start_Address]		;Read data
	incw    <Start_Address
	lda	<Start_Address+1
	cmp	#$60			;Reached the end?
	bne     Copy_ADPCM_to_RAM
;------
	lda	#$40
	sta	<Start_Address+1	;WRAP
	tma	#2
	inc	a
	tam	#2			;Next MPR.  Will this be too slow?
	cmp	#$88			;done MPRs $80-$87
	bne	Copy_ADPCM_to_RAM

	lda	<LineBuffer+LINE_MAX-1	;Get back stored MPR
	tam	#2			;Store this!
	stz	<Dump_Src_Type
	stw	#AD_Copied_txt,<_si
	jsr     Print_Text_Both

	rts


Write_ADPCM:
	lda	#$80
	sta	$180D
	stz	$180D	;reset read/write address

	lda	#2		;MPR and ONLY MPR!
	sta	<Dump_Src_Type
	lda	#$80
	sta     <Start_Bank
	stw	#$4000,<Start_Address	;Will load into MPR #2
	tma	#2			;Store this!
	sta	<LineBuffer+LINE_MAX-1	;Store at top of line buffer... Hmm.
	lda	<Start_Bank
	TAM	#2         	;Map in first bank!

Copy_RAM_to_ADPCM:
	lda	[Start_Address]		;Read data
	sta	$180A			;Write ADPCM byte!
	incw    <Start_Address
	lda	<Start_Address+1
	cmp	#$60			;Reached the end?
	bne     Copy_RAM_to_ADPCM
;------
	lda	#$40
	sta	<Start_Address+1	;WRAP
	tma	#2
	inc	a
	tam	#2			;Next MPR.  Will this be too slow?
	cmp	#$88			;done MPRs $80-$87
	bne	Copy_RAM_to_ADPCM

	lda	<LineBuffer+LINE_MAX-1	;Get back stored MPR
	tam	#2			;Store this!
	stz	<Dump_Src_Type
	stw	#AD_Written_txt,<_si
	jsr     Print_Text_Both
	rts

AD_Copied_txt:
	.db "  ADPCM copied to Banks 80:87.",$d,$a,0
AD_Written_txt:
	.db "  Banks 80:87 copied to ADPCM RAM.",$d,$a,0


ADPCM_Play:
	lda	#$80
	sta	$180D	;RESET ADPCM HARDWARE!
	jsr	Get_Line_Next_Byte
	bcs     .no_play_option		;if no option, STOP playing!
	sta	$180E	;Change ADPCM playback speed
	lda	#$20
	sta	$180D	;Done!
.no_play_option:
	rts




;==============================================

Upload_Irregular_VRAM:
	phx
	clx
	jsr	Serial_In              ;Get ONE byte from Serial
.read_start:
	sta	video_data,X		;Write VRAM
	txa		;alternate betw. $0002/$0003
	eor	#1
	tax
	jsr	read_serial_with_exit
	bcc	.read_start		;If FAIL, end
.read_finish:
	plx
	rts

Upload_Irregular_MPR:
	jsr	Serial_In              ;Get ONE byte from Serial
.read_start:
	sta	[Start_Address]		;Read data
	incw    <Start_Address
	lda	<Start_Address+1
	cmp	#$60			;Reached the end?
	bne     .no_next_bank
	lda	#$40
	sta	<Start_Address+1	;WRAP
	tma	#2
	inc	a
	tam	#2			;Next MPR.  Will this be too slow?
.no_next_bank:
	jsr	read_serial_with_exit
	bcc     .read_start		;If FAIL, end
.read_finish:
	tma	#2
	sta	<Stop_Bank		;Final bank here
	rts


Upload_Regular:
	jsr	Serial_In              ;Get ONE byte from Serial
.read_start:
	sta	[Start_Address]		;Read data
	incw    <Start_Address
	jsr	read_serial_with_exit
	bcc     .read_start		;If FAIL, end
.read_finish:
	rts



CopyMem:	;Simple copy from RAM (Start-Stop) to [V]RAM (_ax)
	phy
	cly
;vvvvvvvvvvvvvvvvvvvvv
	bbr0	<Dump_Src_Type,.no_vaddr_set	;VRAM writing?
	stw	<_ax,<_di     ;Start address will be VRAM WORDs!
	jsr	set_write		;Write to VRAM!
.no_vaddr_set:
;vvvvvvvvvvvvvvvvvvvvv

.copy_start:
	cmpw    <Start_Address,<Stop_Address
	beq     .copy_finish
	lda	[Start_Address],Y	;Read data
;vvvvvvvvvvvvvvvvvvvvv
	bbr0	<Dump_Src_Type,.dest_is_ram	;Skip this if not VRAM!
	sta	video_data              ;Send to VRAM
	incw    <Start_Address
	lda	[Start_Address],Y	;Read data
	sta	video_data+1		;write a WORD for each address.
	bra	.dump_dest_byte
;vvvvvvvvvvvvvvvvvvvvvvv
.dest_is_ram:
	sta	[_ax],Y
.dump_dest_byte:
	incw    <Start_Address		;Inc source pointer
	incw    <_ax                    ;Inc dest pointer
	bra     .copy_start
.copy_finish:
	ply
	rts


ReadBin:	;Reads in a binary range from Serial
	phy
	cly
.read_start:
	cmpw    <Start_Address,<Stop_Address
	beq     .read_finish
	jsr	Serial_In              ;Get from Serial
	sta	[Start_Address],Y	;Read data
	incw    <Start_Address
	bra     .read_start
.read_finish:
	ply
	rts



DumpBin:	;Dumps binary range
	phy
	cly

;vvvvvvvvvvvvvvvvvvvvv
	bbr0	<Dump_Src_Type,.no_vaddr_set	;VRAM reading?
	stw	<Start_Address,<_di     ;Start address will be VRAM WORDs!
	jsr	set_read		;Read from VRAM!
.no_vaddr_set:
;vvvvvvvvvvvvvvvvvvvvv

	cly
.dump_start:
	cmpw    <Start_Address,<Stop_Address
	beq     .dump_finish
;vvvvvvvvvvvvvvvvvvvvv
	bbr0	<Dump_Src_Type,.src_is_ram	;Skip this if not VRAM!
	lda	video_data
	jsr	Serial_Out
	lda	video_data+1			;read a WORD for each address.
	bra	.dump_src_byte
;vvvvvvvvvvvvvvvvvvvvvvv
.src_is_ram:
	lda	[Start_Address],Y	;Read data
.dump_src_byte:
	jsr	Serial_Out              ;Write to serial
	incw    <Start_Address
	bra     .dump_start
.dump_finish:
	ply
	rts



;#*#*#*#*#*#*#*#*# PRINT ALL COMMAND DESCRIPTIONS #*#*#*#*#*#*#
Print_Working_Descriptions:	;Prints only active ones
	phx
	cla	;Start from <space>!"#...
.printall:
	jsr	_check_command_exists_to_X
	cpx	#0
	beq     .no_print_this
	jsr	Print_Long_Description
.no_print_this:
	inc	a
	cmp	#64	;Done?
	bne	.printall
	plx
	rts

Print_All_Descriptions:
        stw	#LongCommandTxt,<_si
	jsr     Print_Text_Both
	cla
.printall:
	jsr	Print_Long_Description
	inc	a
	cmp	#64	;Done?
	bne	.printall
	rts

Print_Long_Description:	;takes command in A, prints its description, CRLF.
	pha             ;Our text strings are long (4k!) so keep in its own bank
	phy		;And we need to point to them from Command byte.
	tay
	tma	#6	;Store current mapped $C000-$DFFF!
	pha
;------------------------
;;	map	Text_Strings
	tma	#7		;Get this bank
	inc	a               ;
	tam	#6		;And map in next one at $C000-
	lda	$C000
	cmp	#'Y'
	bne	.fail_string
	lda	$C001
	cmp	#'E'
	bne	.fail_string
	lda	$C002
	cmp	#'S'
	bne	.fail_string	;Magic word is 'YES'.
;-------------------------

	sty	<_si+1	;Store command in high byte
	stz	<_si
	lsrw	<_si
	lsrw	<_si	;Com * 256 / 4 = Com * 64
	addw	#Long_Command_Descriptions,<_si
	jsr     Print_Text_Both
	jsr	NewlineBoth	;JMP here to end!
	bra	.success_string
.fail_string:
	tya	;command back in A
	add	#$20		;Back to legible text
	jsr	PrintBoth       ;just repeat command
	jsr	spc
.success_string:
	pla
	tam	#6
	ply
	pla
	rts

_check_command_exists_to_X: 	;A helper function
	asl	a	;Word-indexed
	tax
	lda     Cmd_Mtrx_Tbl+1,X	;Check if working
	sax     ;"Working" in X, Index in A
	lsr	a	;Command back in A
	rts

;--------------------------------------------------------------------
Get_Line_Command:	;Will start at beginning of line buffer...
			;A will hold first command byte found,
			;X will point to byte after that.
			;Carry set if nothing found (eg, all SPACES)
	clx
Get_Line_Next:
	cpx	<line_len
	beq	.reached_end
	lda	<LineBuffer,X
	inx			;Point to next byte
	cmp	#$20            ;Was it a space character?
	beq	.reject
	;accepted!
	clc	;Now A has that command, X points to next. Carry Clear
	rts
.reject:
	bra	Get_Line_Next
.reached_end:	;A fruitless search.  Bail!  Set Carry
	sec
	rts



Get_Line_Immediate:	;Does above, but gets THE NEXT CHAR!
			;No skipping spaces to next character!
	cpx	<line_len
	beq	.reached_end
	lda	<LineBuffer,X
	inx			;Point to next byte
	clc	;Now A has that command, X points to next. Carry Clear
	rts
.reached_end:	;A fruitless search.  Bail!  Set Carry
	sec
	rts


;An IMPROVED(?) "Get byte/word from line buffer" routine
; IN: X is some pointer into LineBuffer
; Out: Byte/Word returned in <_al/<_ax, carry clear/set for success/failure
;      X should point right after Byte/Word, A should contain <_al

Get_Line_Next_Byte:	;Get a byte of data, hopefully
	lda	#2      ;2 nybbles worth!
	bra	Get_Line_Next_Something
Get_Line_Next_Word:	;Get a Word's worth of DATA or fail miserably.
	lda	#4	;4 nybbles worth!
Get_Line_Next_Something:
	sta	<_bh	;Temporary loop maximum!
	sty	<_bl	;Store Y reg here for now
	stwz	<_ax	;clear our temp storage

	jsr	Get_Line_Next	;get 1 char, X points to next
        bcs	.total_fail	;This means line was empty after that.
	;Now we've found something, so go Back to what we found
	dex

	cly	;do a loop, how about that?
.get_words_nybble:
	jsr	Get_Line_Immediate ;get 1 char, X next, no skipping spaces!
	bcs     .partial_fail	;Fail if found nothing (ie, end of Line!)
	jsr	Conv_Char_to_Nybble	;Convert ASCII to HEX nybble
	bcc     .nybble_good
	;NYBBLE NOT GOOOOD!  FIRE BAAAD!
				;Fail if, eg. not 0..F
				;eg, it could be a space, colon, etc.
				;BUT... X points AFTER this bad char!
	dex			;Gotta go back!
	bra	.partial_fail
.nybble_good:
	;Success...?
	;Roll nybble into 16-bit storage
	aslw	<_ax
	aslw	<_ax
	aslw	<_ax
	aslw	<_ax
	ora	<_al
	sta	<_al
	;It's now 4 nybbles IN
	iny
	cpy	<_bh	;Do 2 or 4 times?
	bne     .get_words_nybble
	bra	.some_success
;--
.partial_fail:
	cpy	#0		;0 successes?
	beq     .total_fail	;Then failed to read anything
.some_success:
	ldy	<_bl
	lda	<_al		;Heck, put a byte into A
	clc                     ;Well, we got something, at least?
	rts
.total_fail:
	ldy	<_bl
	sec
	rts



;---------------------------------------------------------------------

Compare_String:		;usual "strcomp" command (limited to 256 characters)
		;in: <_si string to compare, <_di location to search
		;out: SEC if failed, CLC if fully found
	pha
	phy
	cly
.cont_search:
	lda	[_si],Y
	beq	.srch_success	;if 0 shows up, search is over
	cmp	[_di],Y
	bne	.failed_search	;if strings differ, fail!
	iny
	bne     .cont_search
	;if string is a bit too long, we'll call it a fail!
.failed_search:
	sec
	bra	.srch_end
.srch_success:
	clc			;"succeeded"
.srch_end:
	ply
	pla
	rts

Hlpr_String: .db "HLPR",0

LineBuf_to_Uppercase:
	phx
	clx
.toupper:
	lda	<LineBuffer,X
	cmp	#'a'
	bcc	.lessthan_lower_a
	cmp	#'z'
	beq	.is_lower_z
	bcs	.greater_than_lower_z
.is_lower_z:
	and	#$DF	;remove #$20
	sta	<LineBuffer,X 	;Save back in line buffer
.greater_than_lower_z:
.lessthan_lower_a:
	inx
	cpx	<line_len
	bcc	.toupper
	plx
	rts


Clear_Mon_Buffers:
	lda	#$20	;"SPACE"
	sta	<LineBuffer+63
	TDD     LineBuffer+63,LineBuffer+62,63	;Fill whole line buffer.
	stz	<line_len
	jsr   	write_curlineptr 	;Reset line! & set VRAM write address
	jsr	Print_Prompt

	clx	;X WILL BE OUR POINTER INTO THE LINE BUFFER!
	rts




Print_Prompt:
	bbr2	<echo_flags,.no_sc_prompt
	st1	#$20	;Space
	st2	#FONT_BANK
	st1	#'>'
	st2	#FONT_BANK
	addw	#2,<screen_ptr	;Advance our pointer
.no_sc_prompt:
	bbr0	<echo_flags,.no_RS_Prompt	;No prompt if Echo OFF
	bbs7    <echo_flags,.no_RS_Prompt	;No prompt if printing BINARY
	lda	#$20
	jsr	Serial_Out
	lda	#'>'
	jsr	Serial_Out
.no_RS_Prompt:
	rts



Print_No_Serial:	;Print an error and connection diagram as help.

	tma	#6	;Store current mapped $C000-$DFFF!
	pha
	tma	#7		;Get this bank
	inc	a               ;
	tam	#6		;And map in next one at $C000-
	lda	$C000
	cmp	#'Y'
	bne	.fail_string	;Magic word is 'YES'.

;-- actual function start
	lda     <echo_flags
	pha
	lda	#%00000100	;NO SERIAL
	sta	<echo_flags
	stw	#PortErrorText,<_si
	jsr     Print_Text_Both

	pla
	sta	<echo_flags
.done_port_error:
;-- actual function end
.fail_string:
	pla
	tam	#6	;Restore $C000

	BG_ON_NOINTS	;Screen ON, but all INTs off!
	rts




print_baud_vars:
	lda	<baud_setting
	;Baud Setting will now point to some text
	asl	a
	asl	a
	asl	a ;x8
	tax
	ldy	#8
.prtbaud:
	lda     baud_vartext,X
	phy
	jsr	PrintBoth
	ply
	inx
	dey
	bne	.prtbaud
.no_print_baud:
	rts

baud_vartext:
 .db "9600 bps"
 .db "19200bps"
 .db "57600bps"


Print_Output_Flags:	;Print the settings for Echo
	bbr2	<echo_flags,.noprt_scflag
	stw	#Flag_Sc_Txt,<_si
	jsr	Print_Text_Both
.noprt_scflag:
	bbr1	<echo_flags,.noprt_rsflag
	stw	#Flag_Rs_Txt,<_si
	jsr	Print_Text_Both
.noprt_rsflag:
	bbr0	<echo_flags,.noprt_EcOnflag
	stw	#Flag_EchoOn_Txt,<_si
	jmp	Print_Text_Both
.noprt_EcOnflag:
	stw	#Flag_EchoOff_Txt,<_si
	jmp	Print_Text_Both

Flag_Sc_Txt:	.db "SCREEN ",0
Flag_Rs_Txt:	.db "RS-232 ",0
Flag_EchoOn_Txt: .db "[ECHO ON]",0
Flag_EchoOff_Txt: .db "[ECHO OFF]",0

Print_Startup_Panel:
	jsr	NewlineBoth
	stw	#Dashed_Line_Txt,<_si
	jsr	Print_Text_Both
	stw	#Startup_Screen_Txt,<_si
	jsr	Print_Text_Both
	stw	#Dashed_Line_Txt,<_si
	jsr	Print_Text_Both
	stw	#Startup_Screen_Txt2a,<_si
	jsr	Print_Text_Both
	jsr	Count_Commands		;# of valid Commands in A
	jsr	Print_Byte_Both
	stw	#Startup_Screen_Txt2b,<_si
	jsr	Print_Text_Both
;;	jsr	NewlineBoth
	jsr	Print_Short_Descriptions
	rts


Startup_Screen_Txt:
;TTL/SERIAL
	IF (TTL_LOGIC)	;TTL style logic
   .db "| PCEmon 5vTTL MONITOR BY CHRIS COVELL <chris_covell@yahoo.ca> |",$d,$a,0
	ELSE		;RS-232 negative logic
   .db "| PCEmon RS232 MONITOR BY CHRIS COVELL <chris_covell@yahoo.ca> |",$d,$a,0
	ENDIF
;TTL/SERIAL
;-------12345678911234567892123456789312x4567894123456789512345678961234-----
Startup_Screen_Txt2a:
   .db " Version 1.",0
Startup_Screen_Txt2b:
   .db ".4 (10/25/2015)",0
;   .db "                                                                ",0
Dashed_Line_Txt:
   .db "+--------------------------------------------------------------+",$d,$a,0

; Control codes: $FF turns on Alt font (+$80); $FE turns it off.
;  ",$fe,"  ",$ff,"
ShortCommandsTxt:
;------12345678911234567892123456789312x4567894123456789512345678961234-----
  .db $d,$a,$d,$a,$ff," General Commands",$fe,$d,$a
  .db $ff," /",$fe," status. ",$ff,"?[c]",$fe," (cmd) help. "
  .db $ff,"!<L/M/H>",$fe," screen res. ",$ff,"N",$fe," next palette.",$d,$a
  .db $ff," O [e/r/s]",$fe," toggle output/echo.  ",$ff,"B <0..2>",$fe," 9600/19200/57600 baud.",$d,$a
  .db $ff," P<dd>",$fe," hexdump lines before pause. ",$ff,"# [c]",$fe," list / execute helpers.",$d,$a
  .db $ff," T [0/1/2 [aaaa dd]]",$fe," view (clear/set address for) cheat trainer.",$d,$a
  .db $d,$a
  .db $ff," Memory Commands",$fe,$d,$a
  .db $ff," H[v]<aaaa>[bbbb]",$fe," hexdump (v)add.  ",$ff,"D[v]<aaaa>[bbbb]",$fe," dump binary.",$d,$a
  .db $ff," H<mm>:[mm]",$fe," hexdump mpr banks.     ",$ff,"D<mm>:[mm]",$fe," bindump mpr banks.",$d,$a
  .db $ff," M<aaaa><dd>[...dd]",$fe," data to mem.   ",$ff,"F <aaaa><bbbb><dd>",$fe," fill mem.",$d,$a
  .db $ff," U[v][aaaa]",$fe," upload data to buffer (no option) or (v)ram address.",$d,$a
  .db $ff," U[mm]:",$fe," upload data to MPR banks.  ",$ff,"J <aaaa>",$fe," jump to address.",$d,$a
  .db $ff," G[L/S/R]",$fe," Go to saved game / (upLoad/Save state (/ Raw RAM).",$d,$a
  .db $d,$a
  .db $ff," Buffer Commands",$fe,$d,$a
  .db $ff," L<size>",$fe," load buffer from ser.  ",$ff,"S",$fe," save buffer to ser.  ",$ff,"C",$fe," check.",$d,$a
  .db $ff," W[v]<aaaa>",$fe," copy buf to (v)ram. ",$ff,"(",$fe," vce pals to buf. ",$ff,")",$fe," buf to vce.",$d,$a
  .db $ff," <",$fe," bram to buf.  ",$ff,">",$fe," buf to bram. "
  .db $ff,"!<",$fe," / ",$ff,"!>",$fe," ADPCM to/from RAM 80:87.",$d,$a,$d,$a
  .db $ff," Register Commands",$fe,$d,$a
  .db $ff," A/X/Y<dd>",$fe," set register value.  ",$ff,"0..7 <dd>",$fe," set mpr bank value.",$d,$a
  .db $ff," V<rr><dddd>",$fe," data to vdc reg.   ",$ff,"R",$fe," see cpu regs. ",$ff,"8",$fe," set jump mpr7.",$d,$a,0
;------12345678911234567892123456789312x4567894123456789512345678961234-----