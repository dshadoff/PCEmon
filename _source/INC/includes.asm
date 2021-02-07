;**************************************************
	IF NOHELP=0
       	.bank	MAIN_BANK	;This will be text strings!
	.org	$C000
	ENDIF
	IF NOHELP=1
	.bank 0
	ENDIF
	.include "INC\text_strings.asm"
;**************************************************

;**************************************************
	IF HEADLESS=1
	.bank	0
	ENDIF
	IF HEADLESS=0
	  IF NOHELP=0
	   .bank	MAIN_BANK+1
	   .org	$4000
	  ENDIF
	  IF NOHELP=1
	   .bank	MAIN_BANK
	   .org	$4000
	  ENDIF
	ENDIF
Sc1_CHR: .vram TEXT_VRAM
	IF HEADLESS=0
	.incchr "INC\GunstarASCII.pcx"
	ENDIF
;**************************************************
	IF HEADLESS=0

	.bank MAIN_BANK+4	;spare space
	.org    $4000
	.include "INC\helper_cheatfinder.asm"

	.bank MAIN_BANK+5	;spare space
	.org    $4000
	.include "INC\helper_disasm.asm"

	.bank MAIN_BANK+6	;spare space
	.org    $4000
	.include "INC\helper_pcxsave.asm"
	ENDIF