;Write: $74 = 9600 baud, $37 = 19200 baud, $D/$E = 57600 baud
;Read: SB6/R7E = 9600 baud, S47/R3E = 19200 baud, S13/R10 = 57600 baud
setup_baud_vars:	;Input: A: Baud rate 0,1,2...
	phx
	sta	<baud_setting		;Save as our current setting
	tax
	lda	baud_rds_tbl,X
	sta	<baud_read_st_delay
	lda	baud_rdr_tbl,X
	sta	<baud_read_rd_delay
	lda	baud_wrt_tbl,X
	sta	<baud_wrt_delay1
	lda	#$20
	sta	<baud_wrt_gap
	plx
	rts

baud_rds_tbl: .db $B6,$47,$13
baud_rdr_tbl: .db $7E,$3E,$10
baud_wrt_tbl: .db $74,$37,$0D

;===========================================================================

Check_Port_Connected:	;If port is "high" continuously, then no serial port connected!
	jsr	Check_FT245_Connected
	tst	#1, ft245_present
	bne	.portok			; running on ft245

	stz	<LineBuffer
	jsr	Serial_Space_Out	;Initialize serial line with 0/1 (TTL)
.here:
	ldx	#$40	;64 loops!
.chkport:
	lda	$1000 ;
	and	#$08	;Check bit 3.  If Zero, not a problem!
;TTL/SERIAL
	IF (TTL_LOGIC)
	bne	.portok
	ELSE
	beq	.portok
	ENDIF
;TTL/SERIAL
	dex
	bne     .chkport
	;Oh, no... always 1 means no serial port!
	bbs5	<LineBuffer,.skip_this_print
	jsr	Print_No_Serial	;Print an error and connection diagram as help.
	smb5	<LineBuffer
.skip_this_print:
	jsr	Wait_A_While
	jmp	.here
.portok:
;Port is OK!
	rts

;------------------------------------------

Wait_A_While:
	pha
	phy
	phx
	clx
	cla
	ldy	#10
.loopme:
	dec	a
	bne	.loopme
	dex
	bne	.loopme
	dey
	bne	.loopme
	plx
	ply
	pla
	rts




; Waits for and reads byte from serial. Max 22 cycles between
; JSRs to serial_read, or data corruption will occur.
; Out: A = byte received
;Y reg gets totally destroyed!

read_serial_with_exit:
	tst	#1, ft245_present
	beq	.serial
	jmp	read_FT245_with_exit
.serial:
	phx
	ldy #$50			;# of loops to check for timeout
	sty	<TIMEOUT+1
	lda #8          ; RUN (pin 5, bit 3)
.rdwt   iny
	bne	.rd_nx1
	dec	<TIMEOUT+1		;Dec this value every 256 loops.
	bne	.rd_nx1

	plx
	sec
	rts
;---
.rd_nx1:
	bit	$1000
;TTL/SERIAL
	IF (TTL_LOGIC)
	bne	.rdwt
	ELSE
	beq	.rdwt
	ENDIF
;TTL/SERIAL
	bra	rd_nx3


;======================================

Serial_In:	;Timing-efficient serial reading but CAN'T EXIT!
	tst	#1, ft245_present
	beq	.serial
	jmp	FT245_In
.serial:
	phx	;Gotta Store X at least
	lda #8          ; RUN (pin 5, bit 3)
.rd_nx1:
	bit	$1000 ;
;TTL/SERIAL
	IF (TTL_LOGIC)	;TTL style logic
	bne	.rd_nx1
	ELSE		;RS-232 negative logic
	beq	.rd_nx1
	ENDIF
;TTL/SERIAL
rd_nx3:
;------------------------------
Delay_Baud_Start:                    ;Delay loop.  Total 6,11,16,21... cycles
	ldy	<baud_read_st_delay	;
.cont_baudloop:
	dey
	bne	.cont_baudloop
.bdstlp_finish:
	bsr rd_first      ; bit 0
	bsr rd_bit        ; bit 1
	bsr rd_bit        ; bit 2
	bsr rd_bit        ; bit 3
	bsr rd_bit        ; bit 4
	bsr rd_bit        ; bit 5
	bsr rd_bit        ; bit 6
	bsr rd_bit        ; bit 7

;TTL/SERIAL
	IF (TTL_LOGIC)	;TTL style logic
	eor	#0	;waste time
	ELSE		;RS-232 negative logic
	eor	#$FF		;Invert data in!
	ENDIF
;TTL/SERIAL

	plx
	clc		;Clear Carry means fine exit!
	rts

;--------
rd_bit:                   ; 6 JSR       ;My code (sans delay) is 26 cycles.
Delay_Baud:                    ;Delay loop.
	ldy	<baud_read_rd_delay
.rd_baudloop:
	dey
	bne	.rd_baudloop
rd_first:
	tax
	lda $1000	;Data comes in bit 3
	lsr a           ;to bit 2
	lsr a           ;to bit 1
	lsr a           ;to bit 0
	lsr a		;-> Carry Bit
	txa
	ror a
	rts
;================================


;======================================

Serial_Out:
	tst	#1, ft245_present
	beq	.serial
	jmp	FT245_Out
.serial:
	pha

;TTL/SERIAL
	IF (TTL_LOGIC)	;TTL style logic
	eor	#0	;waste time
	ELSE		;RS-232 negative logic
	eor	#$FF		;Invert data in!
	ENDIF
;TTL/SERIAL

	;Now our Word needs to be framed S_76543210_M
	;Shift out 10 bits!
	bsr	Serial_Mark_Out		; "Mark"
	bsr	Serial_Shift_Out	; b0
	bsr	Serial_Shift_Out	; b1
	bsr	Serial_Shift_Out	; b2
	bsr	Serial_Shift_Out	; b3
	bsr	Serial_Shift_Out	; b4
	bsr	Serial_Shift_Out	; b5
	bsr	Serial_Shift_Out	; b6
	bsr	Serial_Shift_Out	; b7
	bsr	Serial_Space_Out	; "Space"

	bsr	Serial_Gap_Wait		;Wait a gap between chars...

	pla
	rts

Serial_Shift_Out:	;Shift one bit out the port.
	pha
	and	#$01	;Isolate 1 bits
	sta	$1000	;Save out to port
	pla
	lsr	a	;Shift bits down
	jmp	Baud_Bit_Delay_Out

Serial_Mark_Out: 	;Send a ONE (ish)?
	pha
;TTL/SERIAL
	IF (TTL_LOGIC)	;TTL style logic
	lda	#$00
	ELSE		;RS-232 negative logic
	lda	#$01
	ENDIF
;TTL/SERIAL
	sta	$1000
	lsr	a	;waste the same amount of time...
	pla
        jmp	Baud_Bit_Delay_Out

Serial_Space_Out:	;Send a ZERO?? (well...)
	pha
;TTL/SERIAL
	IF (TTL_LOGIC)	;TTL style logic
	lda	#$01
	ELSE		;RS-232 negative logic
	lda	#$00
	ENDIF
;TTL/SERIAL
	sta	$1000
	lsr	a	;waste the same amount of time...
	pla
        jmp	Baud_Bit_Delay_Out

Baud_Bit_Delay_Out:	;Delay between bits sent
	phx
	ldx	<baud_wrt_delay1
out_del_lp:
	dex
	bne	out_del_lp		;Just a simple, dumb loop.
	plx
	rts

Serial_Gap_Wait:
	phx
	ldx	<baud_wrt_gap
	bra	out_del_lp
;***************************************************


;;TTL/SERIAL
;	IF (TTL_LOGIC)	;TTL style logic
;
;	ELSE		;RS-232 negative logic
;
;	ENDIF
;;TTL/SERIAL
