all: RS232_reg.pce RS232_SHORT.pce RS232_HDLS.pce TTL_reg.pce TTL_SHORT.pce TTL_HDLS.pce

# SRC = rs232.ASM

CFLAGS =-S -raw

%.pce: %.asm
	pceas $(CFLAGS) $(SRC) $(LIBS) $< >$@.map
	rm $<

RS232_reg.asm: INC/_opt_reg.asm rs232.asm
	cat INC/_opt_reg.asm rs232.asm >$@

RS232_SHORT.asm: INC/_opt_short.asm rs232.asm
	cat INC/_opt_short.asm rs232.asm >$@

RS232_HDLS.asm: INC/_opt_headless.asm rs232.asm
	cat INC/_opt_headless.asm rs232.asm >$@

TTL_reg.asm: INC/_opt_ttl_reg.asm rs232.asm
	cat INC/_opt_ttl_reg.asm rs232.asm >$@

TTL_SHORT.asm: INC/_opt_ttl_short.asm rs232.asm
	cat INC/_opt_ttl_short.asm rs232.asm >$@

TTL_HDLS.asm: INC/_opt_ttl_headless.asm rs232.asm
	cat INC/_opt_ttl_headless.asm rs232.asm >$@

clean:
	rm -f *.pce *.lst *.sym *.s *.iso *.map RS232_reg.asm RS232_SHORT.asm RS232_HDLS.asm TTL_reg.asm TTL_SHORT.asm TTL_HDLS.asm
