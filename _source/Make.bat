set PCE_INCLUDE=d:\ASM\include
del *.pce
del *.lst
REM pceas -S -raw rs232.asm >debug.txt
copy /b INC\_opt_reg.asm+rs232.asm RS232_reg.asm
pceas -S -raw RS232_reg.asm >debug.txt
ren rs232_reg.pce RS232.pce
ren rs232_reg.lst rs232.lst
del rs232_reg.asm

copy /b INC\_opt_short.asm+rs232.asm RS232_SHORT.asm
pceas -S -raw RS232_SHORT.asm >debug_short.txt
del a.txt
del RS232_SHORT.asm
del RS232_SHORT.lst

copy /b INC\_opt_headless.asm+rs232.asm RS232_HDLS.asm
pceas -S -raw RS232_HDLS.asm >a.txt
del a.txt
del RS232_HDLS.asm
del RS232_HDLS.lst


copy /b INC\_opt_ttl_reg.asm+rs232.asm TTL_reg.asm
pceas -S -raw TTL_reg.asm >a.txt
ren TTL_reg.pce TTL.pce
ren TTL_reg.lst TTL.lst
del TTL_reg.asm
del TTL.lst

copy /b INC\_opt_ttl_short.asm+rs232.asm TTL_SHORT.asm
pceas -S -raw TTL_SHORT.asm >a.txt
del a.txt
del TTL_SHORT.asm
del TTL_SHORT.lst

copy /b INC\_opt_ttl_headless.asm+rs232.asm TTL_HDLS.asm
pceas -S -raw TTL_HDLS.asm >a.txt
del a.txt
del TTL_HDLS.asm
del TTL_HDLS.lst
del *.sym