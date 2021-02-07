;
; STARTUP.ASM  -  MagicKit standard startup code
;

	.include "INC/my_standard.inc"

; ----
; setup flexible boundaries for startup code
; and user program's "main".

START_BANK	.equ	0
MAIN_BANK	.equ	START_BANK+1


; ----
; system variables


;北盵 STARTUP CODE ]北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
NOP3	.macro
	nop
	nop
	nop
	.endm

SCREEN_OFF	.macro
	st0  #5
	st1   #$00
	st2   #$00
		.endm
SCREEN_ON	.macro
	st0  #5
	st1   #$C8
	st2   #$00
		.endm

BG_ON_NOINTS .macro
	st0  #5
	st1   #$80
	st2   #$00
		.endm

BG_ON	.macro
	st0  #5
	st1   #$88
	st2   #$00
		.endm

vidreg	.macro			; set up video register
	st0	\1
	.endm

; ----
; interrupt vectors

	.code
	.bank START_BANK
	.org  $FFF6

	.dw _cd_irq	;_irq2 (CD-ROM/BRK)
	.dw _short_vsync ;_irq1 (VDC int)
	.dw _timer	;_timer
	.dw _rti	;_nmi
	.dw _reset

	.org $E000

; ----
; reset
; ----
; things start here
; ----

_reset:
	sei			; disable interrupts
	sta	SNAP_A		;at least at syscard "startup", A is restored.
				;Values in MPR6 more important than A value.
	lda	#$F8		; and the RAM bank in the second page
	tam	#1              ;MAP RAM!
;;;;	stz	<start_state	;start from regular reset.

	php		;Not good, but get P reg.
_reset_cont:
	csh			; select the 7.16 MHz clock
	cld			; clear the decimal flag

	lda   #$FF		; map the I/O bank in the first page
	tam   #0
	lda   #%00000111
	sta   $1402		; disable interrupts --- causes Savestate failure
	stz   $1403

	stx	SNAP_X
	plx		;Not good, but need to get Processor flags
	stx	SNAP_SR
	tsx
	stx	SNAP_SP
	sty	SNAP_Y

	ldx   #$3F		; initialize the stack pointer
	txs

	st0   #5
	st1   #$00	;***  NO Interrupts
	st2   #$00

	stz	SNAP_INTMASK	;this gets written to $1402 at the end
	lda	<$00
	sta	SNAP_ZP0
	lda	<$01
	sta	SNAP_ZP1

	tma	#7
	sta	SNAP_MPR7
	tma	#6
	sta	SNAP_MPR6
	tma	#5
	sta	SNAP_MPR5
	tma	#4
	sta	SNAP_MPR4
	tma	#3
	sta	SNAP_MPR3
	tma	#2
	sta	SNAP_MPR2

	tii	Hard_TII,SOFT_TIA,8	;copy to soft TIA

	jsr	Compress_ZP		;this will try to fit ZP somewhere in RAM
;	jsr	Uncompress_ZP
;;.boob	bra	.boob
;	jmp	Restore_Savestate

; ----
; initialize the hardware

	stz   timer_ctrl	; init timer
	stz   psg_mainvol	; main volume to zero
	jsr   init_vdc		; init video - DO NOT clear VRAM.
;;;	cli
	;Interrupts still disabled!??!

; ----
; jump to main routine
	tii	SNAP_A,A_reg,8	;copy snap registers to monitor regs.
	jmp   main



;==================================

Hard_TII:
	tii	$0000,$0000,$0000	;Zero a number of bytes
	rts

;北盵 INTERRUPT CODE ]北北北北北北北北北北北北北北北北北北北北北北北北北北北北北

_rts:
	rts
_rti:
;	rti
_nmi:
	rti

_timer:
	pha
	lda   #%00000111
	sta   $1402		; disable interrupts
	pla
	stz   $1403
;;;;	smb2	<start_state	;Bit 2 is TIMER
	rti

_cd_irq:
	pha
	lda   #%00000111
	sta   $1402		; disable interrupts
	pla
;;;	sta	<A_reg
	stz   $1403
;;;;	smb0	<start_state	;Bit 0 is CD-ROM / BRK /IRQ2
;;;	rti		;BRK is now finished!!!

_reset_clone:
	sei			; disable interrupts
	pla	;P register was on stack!
	sta	SNAP_SR	;<P_reg
	pla     ;PC register LO was on stack
	sta	SNAP_PC	;<PC_reg
	pla     ;PC register HI was on stack
	sta	SNAP_PC+1	;<PC_reg+1
	lda	SNAP_SR	;<P_reg
	pha	;push this on stack
	lda     SNAP_A	<A_reg
	jmp	_reset_cont


; vsync interrupt, now timing-critical!
; Anything here is ALL that happens at VBlank!
_short_vsync:
	pha
	lda   #%00000111
	sta   $1402		; disable interrupts
	pla
	stz   $1403
;;;;	smb1	<start_state    ;Bit 1 is VDC/IRQ1
	rti


;北[ LIBRARY ]北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
	.include "INC/my_library.asm"

	.nomlist
	.list

	;...

