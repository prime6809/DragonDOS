#
# Makefile for Dragondos rom.
#
# 21/11/2004, P.Harvey-Smith.
#

AS=lwasm
ASFLAGS=-9 -r -I defs

ZIP=pkzip
RM=rm

all: ddos10 ddos12 ddos2f

# Dragon Dos 1.00, should be identical to ROM supplied by Dragon Data 		
ddos10: ddos12.asm
		$(AS) $(ASFLAGS) -DDragon -oroms/ddos10.rom -llist/ddos10.lst ddos12.asm 

# Dragon Dos 1.20, P.G.Scott's fixes applied 		
ddos12:	 ddos12.asm
		$(AS) $(ASFLAGS) -DDragon -DPGSFix -oroms/ddos12.rom -llist/ddos12.lst ddos12.asm 

#Dragon Alpha DragonDos 2.f
ddos2f: ddos2f.asm
		$(AS) $(ASFLAGS) -DDragon -oroms/ddos2f.rom -llist/ddos2f.lst ddos2f.asm 

#Dragon Alpha DragonDos 2.f with fixes
ddos2ffix: ddos2f.asm
		$(AS) $(ASFLAGS) -DDragon -DPGSFix -oroms/ddos2f.rom -llist/ddos2f.lst ddos2f.asm 
	
clean:
		$(RM) -f roms/*.rom
		$(RM) -f list/*.lst
		
check:
		cmp -l roms/ddos10.rom ../ROMS/ddos10.rom
		cmp -l roms/ddos2f.rom ../ROMS/ddos2f.bin
		
