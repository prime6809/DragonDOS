#
# Makefile for Dragondos rom.
#
# 21/11/2004, P.Harvey-Smith.
#
# 2026-05-28, added defs for rom and list directories and code to 
# create them if not existing, and remove them when cleaned.
#

AS=lwasm
ASFLAGS=-9 -r -I defs

ZIP=pkzip
RM=rm
MKDIR=mkdir

LISTDIR=list
ROMDIR=roms

all: prep ddos10 ddos12 ddos2f

# Dragon Dos 1.00, should be identical to ROM supplied by Dragon Data 		
ddos10: ddos12.asm
		$(AS) $(ASFLAGS) -DDragon -o$(ROMDIR)/ddos10.rom -l$(LISTDIR)/ddos10.lst ddos12.asm 

# Dragon Dos 1.20, P.G.Scott's fixes applied 		
ddos12:	 ddos12.asm
		$(AS) $(ASFLAGS) -DDragon -DPGSFix -o$(ROMDIR)/ddos12.rom -l$(LISTDIR)/ddos12.lst ddos12.asm 

#Dragon Alpha DragonDos 2.f
ddos2f: ddos2f.asm
		$(AS) $(ASFLAGS) -DDragon -o$(ROMDIR)/ddos2f.rom -l$(LISTDIR)/ddos2f.lst ddos2f.asm 

#Dragon Alpha DragonDos 2.f with fixes
ddos2ffix: ddos2f.asm
		$(AS) $(ASFLAGS) -DDragon -DPGSFix -o$(ROMDIR)/ddos2f.rom -l$(LISTDIR)/ddos2f.lst ddos2f.asm 
	
prep:
		$(MKDIR) -p ./$(LISTDIR)
		$(MKDIR) -p ./$(ROMDIR)
clean:
		$(RM) -rf ./$(ROMDIR)
		$(RM) -rf ./$(LISTDIR)

distclean:	clean
		$(RM) *~
				
check:
		cmp -l roms/ddos10.rom ../ROMS/ddos10.rom
		cmp -l roms/ddos2f.rom ../ROMS/ddos2f.bin
		
