; **** Text Strings ****
Text_Strings:
	IF NOHELP=0
	 .db "YES",0	;Indicates the start of this bank!
	ENDIF
Long_Command_Descriptions:
	IF NOHELP=0
   .db "                     CHRIS COVELL DID THIS!                    ",0
   .db " !+ dev cmnd: L/M/H-res, </> RAM 80:87<>ADPCM, P[n] ADPCM Play ",0
   .db ' ','"',"                                                             ",0
   .db " # [C] - list helper plugins / [execute helper Command]        ",0
   .db " $                                                             ",0
   .db " %                                                             ",0
   .db " &                                                             ",0
   .db " '                                                             ",0
   .db " ( - copy all VCE palettes to buffer                           ",0
   .db " ) [iii] - copy buffer to VCE palette index [iii] or 0         ",0
   .db " *                                                             ",0
   .db " +                                                             ",0
   .db " ,                                                             ",0
   .db " -                                                             ",0
   .db " .                                                             ",0
   .db " / - Short command listing and monitor status                  ",0
   .db " 0 <DD> - set MPR0 mapper reg *NOT RECOMMENDED!*               ",0
   .db " 1 <DD> - set MPR1 mapper reg *NOT RECOMMENDED!*               ",0
   .db " 2 <DD> - set MPR2 mapper reg                                  ",0
   .db " 3 <DD> - set MPR3 mapper reg                                  ",0
   .db " 4 <DD> - set MPR4 mapper reg                                  ",0
   .db " 5 <DD> - set MPR5 mapper reg                                  ",0
   .db " 6 <DD> - set MPR6 mapper reg                                  ",0
   .db " 7 <DD> - set MPR7 mapper reg *NOT RECOMMENDED!*               ",0
   .db " 8 <DD> - set MPR7 value used when jumping to address          ",0
   .db " 9                                                             ",0
   .db " :                                                             ",0
   .db " ;                                                             ",0
   .db " < - copy BRAM to buffer                                       ",0
   .db " =                                                             ",0
   .db " > - write buffer to BRAM  *CAREFUL!*                          ",0
   .db " ? [C] - Full command listing / [List single commmand]         ",0
   .db " @                                                             ",0
   .db " A <DD> - set A register                                       ",0
   .db " B <0..2> - change Baud rate to 9600,19200,57600               ",0
   .db " C - Check buffer (a hexdump)                                  ",0
   .db " D[V] <AAAA>[BBBB] - binDump add.[Vram] / D <MM>:[MM] MPR banks",0
   .db " E                                                             ",0
   .db " F <AAAA> <BBBB> <DD> - Fill range with byte                   ",0
   .db " G [L/S/R] - Go to saved game / [upLoad / Save state / Raw RAM]",0
   .db " H[V] <AAAA>[BBBB] - Hexdump add.[Vram] / H <MM>:[MM] MPR banks",0
   .db " I                                                             ",0
   .db " J <AAAA> - Jump to (execute) address                          ",0
   .db " K                                                             ",0
   .db " L <size> - Load buffer up to $1E00 bytes over serial port     ",0
   .db " M <AAAA> <DD> [DD] [DD] [DD]... - write hex to Memory         ",0
   .db " N - Next screen palette                                       ",0
   .db " O [E/R/S] - [toggle] Output device: serial Echo/RS-232/Screen ",0
   .db " P <DD> - set # of hexdump lines before Pause (00 = no pause)  ",0
   .db " Q                                                             ",0
   .db " R - display CPU Registers                                     ",0
   .db " S - Save buffer as binary over serial port                    ",0
   .db " T [0/1/2 [AAAA DD]] - view (clear/set add. for) cheat Trainer ",0
   .db " U[V] [AAAA] - Upload direct to buf/(V)RAM / U [MM:] MPR bank+ ",0
   .db " V <RR> <DDDD> - write data to VDC register                    ",0
   .db " W[V] <AAAA> - Write buffer to (V)RAM address                  ",0
   .db " X <DD> - set X register                                       ",0
   .db " Y <DD> - set Y register                                       ",0
   .db " Z                                                             ",0
   .db " [                                                             ",0
   .db ' ','\',"                                                             ",0
   .db " ]                                                             ",0
   .db " ^                                                             ",0
   .db "                                                               ",0
 ; 64 possible commands, haha the last one is hidden "_"
	ENDIF

;",$ff,"",$fe,"
PortErrorText:
	IF NOHELP=0
 .db $ff,"  **ERROR**  DISCONNECT JOYPAD AND CONNECT SOME SERIAL LINES.",$fe,$d,$a
 .db $d,$a," Simple serial connection via the PCE controller port:",$d,$a,$d,$a
 .db "     /--------\\         RS-232 DSub-9 (for example)",$d,$a
 .db "    / ",$ff,"G",$fe,"  o  ",$ff,"S",$fe,"  \\             /---------------\\",$d,$a
 .db "    | ",$ff,"R",$fe,"    o o |  -->        \\ 1  ",$ff,"S  R",$fe,"  4  ",$ff,"G",$fe," /",$d,$a
 .db "    \\  o   o   /              \\  6  7  8  9 /",$d,$a
 .db "     \\--------/                \\-----------/",$d,$a,$d,$a
 .db "     /--------\\         TTL FTDI Molex (for example)",$d,$a
 .db "    / ",$ff,"G",$fe,"  o  ",$ff,"S",$fe,"  \\           |------------------|",$d,$a
 .db "    | ",$ff,"R",$fe,"    o o |  -->      | 6  ",$ff,"S  R",$fe,"  3  2  ",$ff,"G",$fe," |",$d,$a
 .db "    \\  o   o   /           |------------------|",$d,$a
 .db "     \\--------/",$d,$a,$d,$a
 .db "  Connect G to G, S to S, R to R, as labeled.",$d,$a
 .db "  S is the PCE SEL line, but you can think of it as 'Send'.",$d,$a
 .db "  R is the PCE Run/L button input, but it can mean 'Receive'.",$d,$a
 .db "  G is a common ground.",$d,$a,$d,$a
 .db "  Sorry, but a MultiTap won't work with this protocol.",$d,$a,$d,$a
 .db "  Don't use a high-voltage (+-12V) serial port; any damage you",$d,$a
 .db "  cause to your PCE or city block is your responsibility."
 .db 0
	ENDIF