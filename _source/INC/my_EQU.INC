;
; EQU.INC  -  MagicKit Standard Equates
;
;

;
; This block defines standard parameter-passing
; areas (in zero-page) for subroutines
;
;_si	= $20EE	; source address
;_di	= $20F0	; destination address
;
;_irq_m	= $20F5	; interrupt control flags
;_vsr	= $20F6	; the VDC status register
;_vreg	= $20F7	; the current selected VDC register
;
;_ax	= $20F8
;_al	= $20F8
;_ah	= $20F9
;_bx	= $20FA
;_bl	= $20FA
;_bh	= $20FB
;_cx	= $20FC
;_cl	= $20FC
;_ch	= $20FD
;_dx	= $20FE
;_dl	= $20FE
;_dh	= $20FF


A_reg	= $20E0
X_reg	= $20E1
Y_reg	= $20E2
SP_reg	= $20E3		;stack pointer
P_reg	= $20E4		;processor status
PC_reg	= $20E5		;(2) Program counter
MPR_reg = $20E7		;Temporary MPR storage
start_state = $20E8	;Did an interrupt happen, or sth?



_si	= $20EE	; source address
_di	= $20F0	; destination address

_irq_m	= $20F5	; interrupt control flags
_vsr	= $20F6	; the VDC status register
_vreg	= $20F7	; the current selected VDC register

_ax	= $20F8
_al	= $20F8
_ah	= $20F9
_bx	= $20FA
_bl	= $20FA
_bh	= $20FB
_cx	= $20FC
_cl	= $20FC
_ch	= $20FD
_dx	= $20FE
_dl	= $20FE
_dh	= $20FF


;
; This block defines names for standard
; devices and equipment on the PC-Engine
; (They should be self-explanatory...)
;

videoport    .equ $0000

video_reg    .equ videoport
video_reg_l  .equ video_reg
video_reg_h  .equ video_reg+1

video_data   .equ videoport+2
video_data_l .equ video_data
video_data_h .equ video_data+1


colorport    .equ $0400
color_ctrl   .equ colorport

color_reg    .equ colorport+2
color_reg_l  .equ color_reg
color_reg_h  .equ color_reg+1

color_data   .equ colorport+4
color_data_l .equ color_data
color_data_h .equ color_data+1


psgport      .equ $0800
psg_ch       .equ psgport
psg_mainvol  .equ psgport+1
psg_freqlo   .equ psgport+2
psg_freqhi   .equ psgport+3
psg_ctrl     .equ psgport+4
psg_pan      .equ psgport+5
psg_wave     .equ psgport+6
psg_noise    .equ psgport+7
psg_lfofreq  .equ psgport+8
psg_lfoctrl  .equ psgport+9


timerport    .equ $0C00
timer_cnt    .equ timerport
timer_ctrl   .equ timerport+1        


joyport      .equ $1000


irqport      .equ $1400
irq_disable  .equ irqport+2
irq_status   .equ irqport+3


;
; This block defines names for macro
; argument types (\?x)
;

ARG_NONE	.equ 0
ARG_REG		.equ 1
ARG_IMMED	.equ 2
ARG_ABSOLUTE	.equ 3
ARG_INDIRECT	.equ 4
ARG_STRING	.equ 5

