;
; VDC.INC  -  MagicKit PC-Engine video MACRO definitions
;

 
;
; VREG - set up video register to be read/written
;

.macro vreg
	lda	\1
	sta	<_vreg
.if (\?1 = ARG_IMMED)
	st0	\1
.else
	sta	video_reg
.endif
.endm 

