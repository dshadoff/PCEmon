;--------------------------------------------------------------------
;  Helper files for PCEmon.  They get mapped in at $4000 when needed
;--------------------------------------------------------------------
;;;x	.org $2200
		;Helper magic signature is below. ($4000-$4003)
 .db "HLPR"
		;Then the command name            ($4004-     )
 .db 'X'
		;And a description / help string.
 .db " [B/W/S/G [pal]] Save Screen (BG/Wide/Spr/Grouped) as PCX",0

	.org $4040	;Leave space for the string.
Helper_X_Start:
	jsr	_ex_Get_Line_Next	;Get next Char/Byte?
	bcs	.normal_screen_dump
	cmp	#'B'
	beq     .bg_dump
	cmp	#'S'
	beq	.spr_dump
	cmp	#'G'
	beq	.grpspr_dump
	cmp	#'W'
	beq	.widebg_dump
	rts	; stop it if it's invalid

.normal_screen_dump:
	jsr	Setup_Dimensions
	jsr	Send_Out_PCX_Header
	jsr	Send_Out_PCX_Data
	jsr	Send_Out_PCX_Palette

	rts

.widebg_dump:
	lda	#1
	bra	.setwidth
.bg_dump:
	cla
.setwidth: 	;A reg used for TileDump_Dimensions...
	jsr	__get_pal_param
	jsr	TileDump_Dimensions
	jsr	Send_Out_PCX_Header
	jsr	Send_Out_Tiles_PCX
	jsr	Send_Out_PCX_Palette
	rts

.grpspr_dump:
	lda	#$80	;grouped sprite layout
	bra     .sprdum_cont
.spr_dump:
	cla	;regular sprite layout
.sprdum_cont:
	jsr	__get_pal_param
	sta	<LEFTORRIGHT
	cla	;used for next function
	jsr	TileDump_Dimensions
	jsr	Send_Out_PCX_Header
	jsr	Send_Out_Sprites_PCX
	jsr	Send_Out_PCX_Spr_Palette
	rts




LEFTORRIGHT = LineBuffer+$0F	;1 byte, 1=Right side of SPR tile. ($8X = grouped sprites)
TILEBUF =    LineBuffer+$10   	;4 bytes temp planar storage
PCXTILEBUF = LineBuffer+$14     ;8 bytes temp chunky storage
PCXCURLINE = LineBuffer+$1C	;2 bytes which scanline are we doing?
CURBAT	=    LineBuffer+$1E	;2 bytes address to read from BAT
CURTILE =    LineBuffer+$20	;2 bytes last read tile num
CURPAL	=    LineBuffer+$22	;1 byte  last read palette

PCXLINEWIDTH = LineBuffer+$24	;2 bytes # of bytes per line
PCXHEIGHT    = LineBuffer+$26   ;2 bytes
PCXCURCHAR   = LineBuffer+$28	;how far along in a line
CURBATLINE   = LineBuffer+$2A	;each scanline, we have to go back here
LASTVALUE    = LineBuffer+$2C	;last byte in storage when writing RLE
RUNCOUNT     = LineBuffer+$2D	;count can be 0 (empty???) to 63

;==================================
; For each tile:
; Tile num 000-$7FF are VRAM add $0000-$7FF0
; row 0..7 are at VRAM address 0..7
; Bitplane 0 is at vram 0..7 LSB.
; Bitplane 1 is at VRAM 0..7 MSB.
; Bitplane 2 is at vram 8..F LSB.
; Bitplane 3 is at vram 8..F MSB.
;
; so...
; Get tilemap, read tile palette, store it *$10
; read tile #, store it *$10 as address
; Get what PCX line we want 0..7, add to address
; Read 2 bytes from address, add 8 to address, read 2 more bytes.
; Done for 8 horizontal pixels!


Send_Out_PCX_Data:	;All that remains...  hrm...



	stwz	<PCXCURLINE		;start at scanline 0
	stwz	<CURBATLINE             ;actually an address
	stz	<RUNCOUNT		;clear RLE buffer


.each_line_loop:
	stwz	<PCXCURCHAR		;Go back to 1st char in a scanline
        stw	<CURBATLINE,<CURBAT	;go back to start in BAT
.each_char_loop:
	;Read a BAT entry, read a Tile sliver, Write it!
	stw     <CURBAT,<_di
	jsr	_ex_set_read		;read a tile/palette from BAT
	lda	video_data		;get tile LSB
	sta     <CURTILE
	lda	video_data+1		;get tile MSB/Pal
	pha
	and	#$0F			;isolate tile MSB
	sta	<CURTILE+1
	pla
	and	#$F0 			;isolate Palette #
	sta	<CURPAL
	incw    <CURBAT			;Go to next BAT value next time
	jsr	Convert_Tile_At_Line
	jsr	Send_8_PCX_Pixels	;RLE too?
	addw	#8,<PCXCURCHAR
	cmpw    <PCXLINEWIDTH,<PCXCURCHAR	;Reached # of bytes per line?
	bne	.each_char_loop
	;ok, go back to start of line, do next scanline
	jsr	Purge_RLE_Buffer	;purge the buffer of the rest of the bytes
	incw    <PCXCURLINE
	lda     <PCXCURLINE
	and	#7			;has it done 8 scanlines?
	bne	.each_line_loop
	;ok, go to next row in BAT
	stw	<CURBAT,<CURBATLINE	;CURBAT was pointing to NEXT line in BAT!!!
	cmpw    <PCXHEIGHT,<PCXCURLINE	;have we done maximum # of scanlines?
	bne     .each_line_loop		;if not, go back to start (as the BAT increases)


	rts

;==============================
; read tile #, store it *$10 as address
; Get what PCX line we want 0..7, add to address
; Read 2 bytes from address, add 8 to address, read 2 more bytes.
; Done for 8 horizontal pixels!
Send_Out_Tiles_PCX:

	stwz	<PCXCURLINE		;start at scanline 0
	stwz	<CURBATLINE             ;actually a BAT value
	stz	<RUNCOUNT		;clear RLE buffer

.each_line_loop:
	stwz	<PCXCURCHAR		;Go back to 1st char in a scanline
        stw	<CURBATLINE,<CURBAT	;go back to start in BAT

.each_char_loop:
	;Read a BAT value, read a Tile sliver, Write it!
	stw     <CURBAT,<CURTILE	;BAT will be tile!
	incw    <CURBAT			;Go to next BAT value next time
	jsr	Convert_Tile_At_Line
	jsr	Send_8_PCX_Pixels	;RLE too?
	addw	#8,<PCXCURCHAR
	cmpw    <PCXLINEWIDTH,<PCXCURCHAR	;Reached # of bytes per line?
	bne	.each_char_loop
	;ok, go back to start of line, do next scanline
	jsr	Purge_RLE_Buffer	;purge the buffer of the rest of the bytes
	incw    <PCXCURLINE
	lda     <PCXCURLINE
	and	#7			;has it done 8 scanlines?
	bne	.each_line_loop
	;ok, go to next row in BAT
	stw	<CURBAT,<CURBATLINE	;CURBAT was pointing to NEXT line in BAT!!!
	cmpw    <PCXHEIGHT,<PCXCURLINE	;have we done maximum # of scanlines?
	bne     .each_line_loop		;if not, go back to start (as the BAT increases)
	rts

;==================================
; For each Sprite tile:
; Tile num 000-$7FF are VRAM add $0000-$7FF0
; row 0..F are at VRAM address 0..F
; Bitplane 0 is at vram 00..0F MSB,LSB (left 8 pixels,right 8 pixels)
; Bitplane 1 is at VRAM 10..1F MSB,LSB
; Bitplane 2 is at vram 20..2F MSB,LSB
; Bitplane 3 is at vram 30..3F MSB,LSB
;
; so...
; read tile #, store it *$40 as address
; Get what PCX line we want 0..F, add to address
; Read 2 bytes from address, add $10 to address, repeat 3 more times.
; Done for 16 horizontal pixels!

; Sprite tile arrangements (when "grouped"):
; 0 1 8 9 10 11 18 19
; 2 3 A B 12 13 1A 1B
; 4 5 C D 14 15 1C 1D
; 6 7 E F 16 17 1E 1F

Send_Out_Sprites_PCX:
	stwz	<PCXCURLINE		;start at scanline 0
	stwz	<CURBATLINE             ;actually a BAT value
	stz	<RUNCOUNT		;clear RLE buffer

.each_line_loop:
	stwz	<PCXCURCHAR		;Go back to 1st char in a scanline
        stw	<CURBATLINE,<CURBAT	;go back to start in BAT

.each_char_loop:
	;Read a BAT value, read a Tile sliver, Write it!
	;;;stw     <CURBAT,<CURTILE	;BAT will be tile!
	jsr	Regroup_Spr_TileNum
	incw    <CURBAT			;Go to next BAT value next time

	jsr	Convert_Send_Sprite_Tiles

	addw	#16,<PCXCURCHAR
	cmpw    <PCXLINEWIDTH,<PCXCURCHAR	;Reached # of bytes per line?
	bne	.each_char_loop
	;ok, go back to start of line, do next scanline
	jsr	Purge_RLE_Buffer	;purge the buffer of the rest of the bytes
	incw    <PCXCURLINE
	lda     <PCXCURLINE
	and	#$0F			;has it done 16 scanlines?
	bne	.each_line_loop
	;ok, go to next row in BAT
	stw	<CURBAT,<CURBATLINE	;CURBAT was pointing to NEXT line in BAT!!!
	cmpw    <PCXHEIGHT,<PCXCURLINE	;have we done maximum # of scanlines?
	bne     .each_line_loop		;if not, go back to start (as the BAT increases)
	rts

;==============================






Purge_RLE_Buffer:	;Here, we send out RLE pixels if there are any left.
	phx
	ldx	<RUNCOUNT
	beq	.no_purge	;if buffer empty, do nothing
	jsr	Send_RLE_Pair   ;if not empty, send what we have.
	stz     <RUNCOUNT	;buffer empty again
.no_purge:
	plx
	rts

Send_RLE_Pixel:		;Here we take a new pixel, and send out old if different
	phx
	ldx	<RUNCOUNT
	beq	.no_compare	;buffer is empty!
;-----------
	cpx	#63		;is buffer full?
	beq	.send_immediately
;-----------
	cmp     <LASTVALUE	;is our new value the same as the last?
	beq	.no_compare	;if the same, well, just INC the counter...
        ;if not the same, send old.
.send_immediately:
	jsr	Send_RLE_Pair
	stz     <RUNCOUNT	;buffer empty again
.no_compare:	;Send nothing, but store our current value
	inc	<RUNCOUNT
	sta     <LASTVALUE
	plx
	rts

Send_RLE_Pair:	;Send what's in our buffer
	pha	;A still might contain our NEW pixel, save it!
;---
	lda     <RUNCOUNT
	cmp	#1	;if it's 1, maybe send a single byte?
	bne	.send_2_bytes
	;if value is #$C0 or more, have to send run count anyway.
        lda	<LASTVALUE
	cmp	#$C0		;is it an "INVALID" colour?
	bcc	.colour_ok
	;if not OK, have to write #$C1 in file first.
	lda     <RUNCOUNT
.send_2_bytes:
	ora	#$C0	   ;send RLE indicator
	jsr	_ex_Serial_Out
	lda	<LASTVALUE
.colour_ok:
	jsr	_ex_Serial_Out
;---
	pla
	rts



Send_8_PCX_Pixels:
	phx
	phy

	clx
.send8pixlp:
	lda	<PCXTILEBUF,X	;Get stored 4bpp pixel
	ora     <CURPAL		;Add palette in top nyb.
	jsr	Send_RLE_Pixel
	inx
	cpx	#8
	bne     .send8pixlp

	ply
	plx
	rts


Convert_Tile_At_Line:	;This will take a tile number, palette, and scanline,
			;and return 8 chunky bytes in a buffer
	phx
	phy

	stwz	<PCXTILEBUF  	;Clear buffers just in case
	stwz	<PCXTILEBUF+2
	stwz	<PCXTILEBUF+4
	stwz	<PCXTILEBUF+6

	stw	<CURTILE,<_di
	aslw    <_di
	aslw    <_di
	aslw    <_di
	aslw    <_di	;x 16!
	lda	<PCXCURLINE
	and	#7
	ora	<_di
	sta	<_di	;Tile # plus which line IN tile!
	jsr	_ex_set_read
	lda	video_data	;tile bitplane 0
	sta	<TILEBUF
	lda	video_data+1	;bitplane 1
	sta	<TILEBUF+1
	addw	#8,<_di
	jsr	_ex_set_read
	lda	video_data	;bitplane 2
	sta	<TILEBUF+2
	lda	video_data+1	;bitplane 3
	sta	<TILEBUF+3

	clx
.roll_in_chunky:
	asl     <TILEBUF+3
	rol     <PCXTILEBUF,X
	asl     <TILEBUF+2
	rol     <PCXTILEBUF,X
	asl     <TILEBUF+1
	rol     <PCXTILEBUF,X
	asl     <TILEBUF
	rol     <PCXTILEBUF,X
	inx
	cpx	#8
	bne	.roll_in_chunky

	ply
	plx
	rts



_spr_group_tbl:
 .db 0, 1,  8,  9, $10, $11, $18, $19
 .db 2, 3, $A, $B, $12, $13, $1A, $1B
 .db 4, 5, $C, $D, $14, $15, $1C, $1D
 .db 6, 7, $E, $F, $16, $17, $1E, $1F

Regroup_Spr_TileNum:	;Rearrange sprite layout into groups, as above
	phx
	lda	<CURBAT+1
	sta     <CURTILE+1

	lda     <CURBAT
	bbr7    <LEFTORRIGHT,.no_rearrange	;"Grouping" marker here.
;----
	and	#$E0	;clear out 00-1F
	sta     <CURTILE
	lda	<CURBAT
	and	#$1F	;isolate 00-1F
	tax
	lda	_spr_group_tbl,X
	ora     <CURTILE
;----
.no_rearrange:
	sta     <CURTILE
	plx
	rts


Convert_Send_Sprite_Tiles: 		;This converts & sends 16 pixels of Spr Tiles
	rmb0	<LEFTORRIGHT		;"LEFT"
	jsr	Convert_Spr_Tiles
	jsr	Send_8_PCX_Pixels	;RLE too?
	smb0	<LEFTORRIGHT		;"RIGHT"
	jsr	Convert_Spr_Tiles
	jsr	Send_8_PCX_Pixels	;RLE too?
	rts


; read tile #, store it *$40 as address
; Get what PCX line we want 0..F, add to address
; Read 2 bytes from address, add $10 to address, repeat 3 more times.
; Done for 16 horizontal pixels!

Convert_Spr_Tiles:	;This will take a tile number, palette, and scanline,
			;and return 8 chunky bytes in a buffer
	phx
	phy

	stwz	<PCXTILEBUF  	;Clear buffers just in case
	stwz	<PCXTILEBUF+2
	stwz	<PCXTILEBUF+4
	stwz	<PCXTILEBUF+6

	stw	<CURTILE,<_di
	aslw    <_di
	aslw    <_di
	aslw    <_di
	aslw    <_di	;x $10!
	aslw    <_di
	aslw    <_di	;x $40!
	lda	<PCXCURLINE
	and	#$0F
	ora	<_di
	sta	<_di	;Tile # plus which line IN tile!
	jsr	_ex_set_read

	lda	video_data	;tile RIGHT side, bpl0
	sta	<TILEBUF        ;saving right side won't hurt
	lda	video_data+1	;tile LEFT side, bpl0
	bbs0    <LEFTORRIGHT,.noleftbp0
	sta	<TILEBUF	;save left side instead.
.noleftbp0:
	addw	#$10,<_di
	jsr	_ex_set_read

	lda	video_data	;tile RIGHT side, bpl1
	sta	<TILEBUF+1        ;saving right side won't hurt
	lda	video_data+1	;tile LEFT side, bpl1
	bbs0    <LEFTORRIGHT,.noleftbp1
	sta	<TILEBUF+1	;save left side instead.
.noleftbp1:
	addw	#$10,<_di
	jsr	_ex_set_read

	lda	video_data	;tile RIGHT side, bpl2
	sta	<TILEBUF+2        ;saving right side won't hurt
	lda	video_data+1	;tile LEFT side, bpl2
	bbs0    <LEFTORRIGHT,.noleftbp2
	sta	<TILEBUF+2	;save left side instead.
.noleftbp2:
	addw	#$10,<_di
	jsr	_ex_set_read

	lda	video_data	;tile RIGHT side, bpl3
	sta	<TILEBUF+3        ;saving right side won't hurt
	lda	video_data+1	;tile LEFT side, bpl3
	bbs0    <LEFTORRIGHT,.noleftbp3
	sta	<TILEBUF+3	;save left side instead.
.noleftbp3:

	clx
.roll_in_chunky:
	asl     <TILEBUF+3
	rol     <PCXTILEBUF,X
	asl     <TILEBUF+2
	rol     <PCXTILEBUF,X
	asl     <TILEBUF+1
	rol     <PCXTILEBUF,X
	asl     <TILEBUF
	rol     <PCXTILEBUF,X
	inx
	cpx	#8
	bne	.roll_in_chunky

	ply
	plx
	rts




Send_Out_PCX_Spr_Palette:
	stw	#$0100,$0402		;Colour table #256
	bra	_pcx_pal_cont
;-------
Send_Out_PCX_Palette:
	stwz	$0402		;Colour table #0
_pcx_pal_cont:
	lda	#$0C
	jsr	_ex_Serial_Out	;Start of Colour Table Array
	clx			;256 palette entries
;---
.pal_sendout_lp:
	lda	$0404
	sta	<_cl
	lda	$0405
	sta	<_ch		;Get a palette entry
	;Red comes first
	lda	<_cl            ;5..3
	asl	a
	asl	a		;7..5
	jsr	Send_VGA_Pal_Byte	;copy lower bits and send byte!
	;Green next
	lda	<_cl		;7,6
	lsr	<_ch		;green bit 8 in carry
	ror	a		;7..5
	jsr	Send_VGA_Pal_Byte
	;finally blue
	lda	<_cl		;2..0
	ror	a               ;1..C
	ror	a               ;0..C..7
	ror	a               ;C..6
	ror	a               ;7..5
	jsr	Send_VGA_Pal_Byte
;---
	inx
	bne	.pal_sendout_lp

	rts

Send_VGA_Pal_Byte:
	and	#$E0		;isolate top bits
	sta	<_dl            ;7..5
	lsr	a
	lsr	a
	lsr	a		;now bits 4..2
	ora	<_dl		;now bits 7..2
	sta	<_dl
	lsr	a
	lsr	a
	lsr	a               ;now bits 4..0
	and	#$03
	ora     <_dl		;now bits 7..0
	jsr	_ex_Serial_Out
	rts


TileDump_Dimensions:	;Set up 128x1024 or 256x512 bitmap file
	cmp	#0
	beq	.narrow_screen
	stw	#256,<PCXLINEWIDTH
	stw	#512,<PCXHEIGHT
	rts
.narrow_screen:
	stw	#128,<PCXLINEWIDTH
	stw	#1024,<PCXHEIGHT
	rts


Setup_Dimensions:	;Sets up bitmap file from our BAT size
	phx

	stz	<PCXLINEWIDTH
	ldx	<BAT_Size
	lda     PCX_XStop_Tbl,X ;Get width Hi Byte
	inc	a		;($0100,200,400...)
	sta	<PCXLINEWIDTH+1

	stz	<PCXHEIGHT
	ldx	<BAT_Size
	lda     PCX_YStop_Tbl,X ;Get height Hi Byte
	inc	a
	sta	<PCXHEIGHT+1

	plx
	rts


__get_pal_param:
	pha
	jsr	_ex_Get_Line_Next_Byte
	bcc	.has_spr_param
	cla     ;palette #0
.has_spr_param:
	asl	a
	asl	a
	asl	a
	asl	a	;set to $F0
	sta	<CURPAL
	pla
	rts


Send_Out_PCX_Header:
	smb7    <echo_flags	;Suppress Prompt!
	stw	#PCX_Header,<Start_Address
	stw	#PCX_XY_Stop,<Stop_Address
	stz	<Dump_Src_Type			;Clear special source type
	jsr	_ex_DumpBin


	decw	<PCXLINEWIDTH               	
	lda	<PCXLINEWIDTH               	;Send lo byte of width (xx00)
	jsr	_ex_Serial_Out
	lda     <PCXLINEWIDTH+1                 ;Send width Hi Byte
	jsr	_ex_Serial_Out
	incw	<PCXLINEWIDTH

	decw	<PCXHEIGHT
	lda	<PCXHEIGHT	;Send height Lo Byte
	jsr	_ex_Serial_Out
	lda	<PCXHEIGHT+1
	jsr	_ex_Serial_Out
	incw	<PCXHEIGHT

	stw	#PCX_Header2,<Start_Address
	stw	#PCX_X_Width,<Stop_Address
	jsr	_ex_DumpBin

	lda	<PCXLINEWIDTH               	;Send lo byte of width (xx00)
	jsr	_ex_Serial_Out
	lda     <PCXLINEWIDTH+1			 ;Send width Hi Byte ($0100,200,400...)
	jsr	_ex_Serial_Out



	stw	#PCX_Header3,<Start_Address
	stw	#PCX_Header_End,<Stop_Address
	jsr	_ex_DumpBin
	;================== okay, that's the HEADER sent! ========
	rts


	;PCX_Width_Tbl_l:  .db $FF,$FF,$FF,$FF ... all the SAME!
	;PCX_Height_Tbl_l:   ;always FFs!
PCX_XStop_Tbl: .db $00,$01,$03,$03,$00,$01,$03,$03
PCX_YStop_Tbl: .db 0,0,0,0,1,1,1,1


;;;	.org $5F00	;128-byte header
PCX_Header:
	.db $0A,$05,$01,$08	;PCX ident, 8 bitplanes...
	.dw $0000,$0000		;X,Y-start
PCX_XY_Stop:			;The following is the total pixel w,h minus 1.
	.dw $00FF,$00FF		;X,Y-stop (0FF,1FF,3FF...)
PCX_Header2:	;continuing...
	.dw $0000,$0000		;"Resolution" whatever that means.
	.dw 0,0,0,0,0,0,0,0     ;\
	.dw 0,0,0,0,0,0,0,0	; palette (unused)
	.dw 0,0,0,0,0,0,0,0     ;/
	.db 0
	.db 1			;# of planes
PCX_X_Width:
	.dw $0100		;Total pixel W. for one line.
PCX_Header3:
	.db $01,0		;Colour (not greyscale)
	.dw 0,0,0,0,0     	; pad with 58 more bytes.
	.dw 0,0,0,0,0,0,0,0	;
	.dw 0,0,0,0,0,0,0,0     ;
	.dw 0,0,0,0,0,0,0,0     ;
PCX_Header_End: