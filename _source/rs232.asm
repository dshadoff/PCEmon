;+--------------------------------------------------------------+
;| PCEmon RS232 MONITOR BY CHRIS COVELL <chris_covell@yahoo.ca> |
;+--------------------------------------------------------------+
; Version 1.2X.X  (10/6/2015)
; This is the source code, yeah!
; Let me know if anything really needs improvement.

;Current bugs: turning screen echo off then changing resolutions causes
; BAT corruption when echo turned back on?
; VRAM dumped only 65534 bytes(?) if doing DV0... *fixed* in Hexdump Rt?

        .include "INC/My_startup.asm"
	.nomlist
	.list
; ----
;Following 2 defines create 2 additional versions of my program:
; HEADLESS doesn't touch VRAM or write to the screen
; NOHELP doesn't include the HELP (?) strings.
; SHORT combines both so that my program is 8k and only sends to RS-232!

TEXT_VRAM	equ $1000         ;characters start at $01xx
FONT_BANK	equ TEXT_VRAM/4096
LINE_MAX        equ 60		;Maximum length of command
BAUD_LIMIT	equ 3		;0,1,2 only
CHEAT_RAM	equ $2104	;used for cheats/trainers (16 bytes)
SAVE_FINAL_RAM  equ $2114	;start 16 bytes later due to cheats!
BUFFER_LOC	equ $2200	;storage Buffer location in RAM
BUFFER_MAX	equ $4000-(BUFFER_LOC)	;7680 bytes, normally
BRAM_BANK	equ $F7	;put this in one of the MPRs

;---------
SNAP_RAM	equ $2140	;RAM to store snapshots!
SNAP_A		equ SNAP_RAM+$08
SNAP_X		equ SNAP_RAM+$09
SNAP_Y		equ SNAP_RAM+$0A
SNAP_SP		equ SNAP_RAM+$0B
SNAP_SR		equ SNAP_RAM+$0C
SNAP_PC		equ SNAP_RAM+$0D
SNAP_MPR7	equ SNAP_RAM+$0F
SNAP_ZP0	equ SNAP_RAM+$10
SNAP_ZP1	equ SNAP_RAM+$11
SNAP_INTMASK	equ SNAP_RAM+$12	;what gets restored to $1402 ?
SNAP_MPR2	equ SNAP_RAM+$13
SNAP_MPR3	equ SNAP_RAM+$14
SNAP_MPR4	equ SNAP_RAM+$15
SNAP_MPR5	equ SNAP_RAM+$16
SNAP_MPR6	equ SNAP_RAM+$17

SOFT_TIA	equ SNAP_RAM	;$2140	;hide away in stack!
RLE_PREV_BYTE	equ SOFT_TIA+1	;temp storage for RLE encoded byte
RLE_COUNTER     equ SOFT_TIA+2	;"		"
RLE_DEST        equ SOFT_TIA+3  ;(2) location with greatest length
TIA_LEN		equ SOFT_TIA+5	;location for length!
FOUND_LEN	equ TIA_LEN	;found length
RLE_LEN		equ TIA_LEN+1	;needed length
;-----


; Zero-page variables
;Also, MagicKit vars from $EE-$FF
	.zp
nmi_count:	ds 2	;Counter for NMIs...
nmi_pass:	ds 1
VCE_copy:	ds 1
palnum:		ds 1
joyportread:	ds 1
selclr:		ds 1
baud_wrt_delay1:	ds 1
baud_wrt_gap:		ds 1
baud_read_st_delay:	ds 1
baud_read_rd_delay:	ds 1
TIMEOUT		ds 2		;Timeout for serial receive
baud_setting:	ds 1		;0,1,2 for different BPS
;--- monitor stuff
Start_Address	ds 2	;Address to start from (will increment)
Stop_Address	ds 2	;Address to stop at ($xFFF or $x000 ????)
Start_Bank	ds 1	;from...
Stop_Bank	ds 1    ;to.... ROM (RAM?) banks to dump
Dump_Src_Type	ds 1	;xxxxxxMV	;M = MPR (bank), V=VRAM
Buffer_Size	ds 2	;Size 0..7680 of buffer
Pause_Setting	ds 1	;How many lines to pause after?
Pause_LoopNum	ds 1    ;internal loop counter
LineBuffer:	ds 64
line_len:	ds 1	;Length of a line (excl. 0D,0A): 00..64
screen_ptr:	ds 2	;Pointer for writing to screen.
erase_ptr:	ds 2	;Pointer for line to erase...
scroll_flag	ds 1	;1 = scroll for each newline.
scrollval	ds 1	;current scroll value.  Will be 0,8,16,..
curline_ptr:	ds 2	;Current line pointer
echo_flags:	ds 1	;PxxxxSRE: P=1=NoPrompt! S=Screen Printing, R=RS-232 Printing, E=RS-232 *Echo*
tempjmp:	ds 2	;A temporary jump table
last_command	ds 1	;The command that was executed (0..$3F)
font_hilite	ds 1	;#$80 if we highlight the font
BAT_Size	ds 1	;setting for BAT size (0...7)
	;...

;--- CODE area ----------

	.code
	.bank START_BANK

main:
	jsr	clear_vars
	jsr	setup_video
	lda	#1	;19200 baud
	jsr	setup_baud_vars
	IF HEADLESS=0
	jsr   	write_curlineptr
	jsr	Check_Port_Connected
	jsr	Print_Startup_Panel
	ELSE
	NOP3	;for the above 3 JSRs!
	NOP3
	NOP3
	ENDIF



	BG_ON_NOINTS	;Screen ON, but all INTs off!
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Monitor_First_Start:
	jsr   	write_curlineptr
;;	jsr	List_Helpers
;;;	jsr	Print_Short_Descriptions
Monitor_Clear:
	jsr	Clear_Mon_Buffers
My_Monitor_Loop:		;X is the line buffer pointer from now!

	jsr	Serial_In
	cmp	#$08		;Is it a backspace?
	beq	.handle_backspace
	cmp	#$0D
	beq	.handle_CR
	cmp	#$0A
	beq	.handle_LF
 	;--- print byte now
	cpx	#LINE_MAX		;Are we AT the max line?
	beq	My_Monitor_Loop	;...If so, X stays where it is
					;Otherwise...
	sta	<LineBuffer,X	;Store in our buffer

	bbr0	<echo_flags,.no_echo_keypress
	jsr	Serial_Out
.no_echo_keypress:
	bbr2	<echo_flags,.no_echo_screen	;Skip video writing, maybe
	sta	video_data
	ldy	#FONT_BANK
	sty	video_data+1
	;--- advance screen pointer, etc
	inc     <screen_ptr	;DIRTY, as HIGH byte not touched!
.no_echo_screen:
	inx			;X will store into next buffer slot
	bra     My_Monitor_Loop

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.handle_backspace:
	;Right now, the screen pointer is pointing at the next char to be filled.
	;If it's at the start of the line, do nothing!
	cpx	#0
	beq     My_Monitor_Loop	;Go back, do nothing...?
	cpx	#LINE_MAX	;Are we AT the max line?
	bne	.backspace_not_max_line
	;if at EOL, clear *2* chars!

;;	bbr0	<echo_flags,.no_backspc1
;;	jsr	Serial_Out		;Backspace Out!
;;.no_backspc1:
	;Above removed because it's handled in my Char typing code?


	bbr2	<echo_flags,.backspc_no_nothing	;If no screen writing, do NADA
	;if at EOL, clear *2* chars!
	st1	#$20
	st2	#FONT_BANK
	;will that be enough... rely on screenpointer reset below...
.backspace_not_max_line:
	bbr0	<echo_flags,.no_backspc2
	jsr	Serial_Out		;Backspace Out!
.no_backspc2:
	bbr2	<echo_flags,.backspc_no_nothing
	;otherwise, decrement the screen pointer
	jsr	dec_scrnptr
	;and CLEAR the current CHAR.
	st1	#$20
	st2	#FONT_BANK
	;will that be enough...?
	;Nope, <screen_ptr stays where it is, and we rewrite it!
        jsr	write_scrnptr
.backspc_no_nothing:
	;Also, decrement X
	dex
	bra	My_Monitor_Loop

.handle_CR:
	rmb7    <echo_flags	;Reenable Prompt!
;;	bbr0	<echo_flags,.no_RS_CR
;;	jsr	Serial_Out		;CR Out!
;;.no_RS_CR:
	lda	<curline_ptr
	add	#2              ;Move over 2 spaces past prompt.
	sta	<screen_ptr	;Reset line! (DIRTY! Low Byte only)
	jsr   	write_scrnptr 	; set VRAM write address
	stx	<line_len	;Pointer becomes max line length
	clx	;Clear POINTER INTO THE LINE BUFFER!
	bra	My_Monitor_Loop

.handle_LF:
	rmb7    <echo_flags	;Reenable Prompt!
	bbr0	<echo_flags,.no_RS_LF
	lda	#$0D
	jsr	Serial_Out
	lda	#$0A
	jsr	Serial_Out		;LF Out!
.no_RS_LF:
	;at this point, we're going to the next line
	;but first interpret the current line!
	cpx	#0
	beq	.CR_already_done
	stx	<line_len	;This is for LF-only systems?
.CR_already_done:
	jsr	manage_screen_newlines
	jsr	Evaluate_Line
	jmp	Monitor_Clear



;±±±[ USER DATA ]±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±


Hard_TIA:
	tia	BUFFER_LOC,$0404,$0400	;Copy from buffer to VCE
	rts


;=======================================================================





clear_vars:
	lda	#$AA
	sta	$18C0
	lda	#$55
	sta	$18C0	;Unlocks Super CD-RAM!

	lda	#%00000111
	sta	<echo_flags	;PxxxxSRE: S=Screen, R=RS, E=RS *Echo*
	IF HEADLESS=1		;NO SCREEN WRITING!
	rmb2    <echo_flags
	ELSE
	nop
	nop
	ENDIF
	stz	<BAT_Size
	stwz	<nmi_count
	stz	<selclr
	stz	<nmi_pass
	stw	#$80,<curline_ptr
	stw	#$780,<erase_ptr	;Pointer for line to erase...
	stz	<scroll_flag		;1 = scroll for each newline.
	lda	#$FF
	sta	<scrollval		;current scroll value.  Will be 0,8,16,..
	stz	<last_command
	stz	<font_hilite

	lda	#$18		;Default lines for hexdump
	sta	<Pause_Setting

	stwz	<Buffer_Size

	rts
;========================================


;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	.include "INC/monitor_functions.asm"
	.include "INC/serial_lowlevel.asm"
	.include "INC/gfx_work.asm"
	.include "INC/savestates.asm"
	.include "INC/includes.asm"	;This goes last!
;**************************************************

     	.bank	START_BANK

	IF 1=0
	.org	$FF00

	;---- $E4A1 gets hacked to JSR to here!
	sta	$1000		;Needed right here!!
;;        stw	#$100,$0402	;Sprite 0 colour
;;	stw	#$49,$0404	;Grey
	lda	CHEAT_RAM
	cmp	#'T'	;for trainer
	bne	.no_cheats
;------------
	phx
	lda	<$00
	pha
	lda	<$01
	pha
;--------------
;;        stw	#$100,$0402	;Sprite 0 colour
;;	stw	#$1C0,$0404	;Green
	stz	$0402
	lda	#1
	sta	$0403 ;\
	sta	$0404 ; |- make Border Colour a dark Blue!
	stz	$0405 ;/
;---------
	clx	;go to first cheat!
	bsr	_do_cheat
	bsr	_do_cheat
	bsr	_do_cheat
;------------
	pla
	sta	<$01
	pla
	sta	<$00
	plx
;------------
.no_cheats:
	rts
	;EXIT to controller-reading routine!

_do_cheat:
	lda    CHEAT_RAM+4,X	;Check for "CHeat" signature!
	cmp	#'C'
	bne	.dead_cheat
	lda    CHEAT_RAM+5,X
	cmp	#'H'
	bne	.dead_cheat
	;cheat seems valid, so do it!
	lda    CHEAT_RAM+1,X
	sta	<$00
	lda    CHEAT_RAM+2,X
	sta	<$01
	lda    CHEAT_RAM+3,X
	sta	[$00]
.dead_cheat:
	txa
	add	#5
	tax
	rts

; Cheats:  0   1 2  3  4   5   6 7  8  9   A   B C  D  E   F
;         'T' LLHH DD 'C' 'H' LLHH DD 'C' 'H' LLHH DD 'C' 'H'


	ENDIF


	.org	$FF80
;JJJJJJJJJJJJJJJJJJJJJJJJJJ  Jump Tables!  JJJJJJJJJJJJJJJJJJJJJJ
_ex_Get_Line_Next:	jmp Get_Line_Next
_ex_Get_Line_Next_Word:	jmp Get_Line_Next_Word
_ex_Get_Line_Next_Byte: jmp Get_Line_Next_Byte
_ex_spc:		jmp spc
_ex_spcspc:		jmp spcspc
_ex_PrintBoth:		jmp PrintBoth
_ex_Print_Byte_Both:	jmp Print_Byte_Both
_ex_Print_Text_Both:	jmp Print_Text_Both
_ex_NewlineBoth:	jmp NewlineBoth
_ex_write_curlineptr:	jmp write_curlineptr
_ex_set_read:		jmp set_read
_ex_Serial_In:		jmp Serial_In
_ex_Serial_Out:		jmp Serial_Out
_ex_DumpBin:		jmp DumpBin
_ex_Print_Nybble_Both:  jmp Print_Nybble_Both
_ex_Conv_Char_to_Nybble: jmp Conv_Char_to_Nybble
_ex_write_scrnptr:	jmp write_scrnptr
;JJJJJJJJJJJJJJJJJJJJJJ For external functions JJJJJJJJJJJJJJJJJJ


;Code for hacking ROMs/Syscards with:
; $4f3 In ROM (when the game/syscard reset is in E000...):
;MPR_reg = $20E7	;Temporary MPR storage
; ea ea ea ea ea ea ea 48 4C D0 FF  <- this will Map in my Bank at $E000-

;at syscard $FFD0:
; A9 15 53 80 ea ea (more if you wish)

	.org	$FFD0
	lda	#$15	;<syscard's "free" bank>
	TAM	#7
	nop
	nop
	pla
	jmp	_reset
