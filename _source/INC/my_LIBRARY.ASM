;
; LIBRARY.ASM  -  MagicKit Standard Library
;
;

; ----
; load_vram
; ----
; copy a block of memory to the VRAM
; ----
; IN :  _DI = VRAM location
;       _SI = memory location
;       _CX = number of words to copy
; ----

load_vram:
	jsr   set_write
	cly
.loop:	lda   [_si],Y
	sta   video_data
	iny
	lda   [_si],Y
	sta   video_data+1
	iny
	bne   .next
	inc   <_si+1
.next:	decw  <_cx
	lda   <_cl
	ora   <_ch
	bne   .loop
	rts


; ----
; set_read
; ----
; set the VDC VRAM read address
; ----
; IN :  _DI = VRAM location
; ----

set_read:
	vidreg  #$01
	lda   <_di
	sta   video_data
	lda   <_di+1
	sta   video_data+1
	vidreg  #$02
	rts 


; ----
; set_write
; ----
; set the VDC VRAM write address
; ----
; IN :  _DI = VRAM location
; ----

set_write:
	vidreg  #$00
	lda   <_di 
	sta   video_data
	lda   <_di+1
	sta   video_data+1
	vidreg  #$02
	rts 


; ----
; HSR(xres)
; ----
; macros to calculate the value of the HSR VDC register
; ----
; IN :  xres, horizontal screen resolution
; ----

HSR	.macro
	 .if (\1 <= 272)
	  ; low res
	  .db $02
	  .db (17 - (((\1 / 8) - 1) / 2))
;	  .db (18 - (((\1 / 8) - 1) / 2)) ;OLD VALUE
	 .else
	  ; high res
	  .db $03
	  .db (24 - (((\1 / 8) - 1) / 2))
	 .endif
	.endm


; ----
; HDR(xres)
; ----
; macros to calculate the value of the HDR VDC register
; ----
; IN :  xres, horizontal screen resolution
; ----

HDR	.macro
	 .db ((\1 / 8) - 1)
	 .if (\1 <= 272)
	  ; low res
	  .db (38 - ((18 - (((\1 / 8) - 1) / 2)) + (\1 / 8)))
	 .else
	  ; high res
	  .db (54 - ((24 - (((\1 / 8) - 1) / 2)) + (\1 / 8)))
	 .endif
	.endm


; ----
; init_vdc
; ----
; initialize the video controller
;  * screen mode set to 256x240
;  * display and sprites off
;  * interrupts disabled
;  * virtual screen size set to 512x256
;  * SATB set to $7F00
;  * clear the video RAM
; ----

init_vdc:
	; default screen resolution

	.ifndef _xres
_xres    .equ 256
	.endif

	; initialize the VDC registers

	stw   #.table,<_si 	; register table address in '_si'
	cly
.l1:	lda   [_si],Y		; select the VDC register
	iny
;;;;	sta   <_vreg
	sta   video_reg
	lda   [_si],Y		; send the 16-bit data
	iny
	sta   video_data
	lda   [_si],Y
	iny 
	sta   video_data+1
	cpy   #36		; loop if not at the end of the
	bne   .l1		; table

	; set the pixel clock frequency

	.if (_xres <= 272)
	 lda  #4
	 sta  color_ctrl
	.else
	 lda  #5
	 sta  color_ctrl
	.endif

	; clear the video RAM

	;Let's NOT do this.

;;	st0	#0
;;	st1	#0
;;	st2	#0

;;	ldx	#128
;;.l2:	cly
;;.l3:	st1	#0
;;	st2	#0
;;	dey
;;	bne	.l3
;;	dex
;;	bne	.l2

	rts

	; VDC register data

.table:	.db $05,$00,$00		; CR    control register
	.db $06,$00,$00		; RCR   scanline interrupt counter
	.db $07,$00,$00		; BXR   background horizontal scroll offset
	.db $08,$00,$00		; BYR        "     vertical     "      "
	.db $09,$10,$00		; MWR   size of the virtual screen
	.db $0A			; HSR +
	 HSR _xres		;     |                 [$02,$02]
	.db $0B			; HDR | display size
	 HDR _xres		;     | and synchro     [$1F,$04]
	.db $0C,$02,$0F		; VPR |
	.db $0D,$EF,$00		; VDW |
	.db $0E,$04,$00		; VCR +
	.db $0F,$10,$00		; DCR   DMA control register
	.db $13,$00,$7F		; SATB  address of the SATB


; ----
; init_psg
; ----
; initialize the sound generator.
; ----

;;init_psg:
;;	stz   psg_mainvol	; main volume to zero
;;	stz   psg_lfoctrl	; disable the LFO
;;
;;	lda   #5		; set volume to zero for each channel
;;.clear:	sta   psg_ch            ; and disable them
;;	stz   psg_ctrl
;;	stz   psg_pan
;;	dec   A
;;	bpl   .clear
;;
;;	lda   #4		; disable noise for channel 5 & 6
;;	sta   psg_ch
;;	stz   psg_noise
;;	lda   #5
;;	sta   psg_ch
;;	stz   psg_noise
;;	rts

