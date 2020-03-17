Dragon DOS source code.

Included here is the assemble-able source code for Dragon DOS version 1.0 which was originally disassembled in 2004 so that it could be ported to run on the Dragon Alpha / Professional.

It has over the years received more commenting and tidying up as needed, such as for it’s 
inclusion in the dos emulation section of DragonMMC. 

I was later able to obtain a genuine copy of the Alpha / Professional Dragon DOS 2.f this has been disassembled and is also presented here. 

In March 2020 I was able to obtain a scan of the original DOS for the Alpha / Professional, and with the aid of that was able to finish commenting both the Dos V1 and V2f source files. 

To build these you will need the lwasm assembler, and a version of make that understands Unix / Linux type Makefiles.

The Makefile will build 3 files in the roms directory :

ddos10.rom 	is a strict binary identical to the ROM supplied on the original DOS cartridge.
ddos12.rom 	is the same as the above, with the fixes from May 1985 Dragonuser applied.
ddos2f.rom	is a binary identical copy of the Alpha / Professional DOS.

At some point in the future I may add the facility to build ddos10 / ddos12 for the CoCo floppy controller for use with the Tano Dragon (as CoCo controllers are more easily available in the USA). I may also port Dragon Dos itself to run on the CoCo with Extended Colour basic, assuming equivalent entry point into the ECB ROM can be found.

– Phill Harvey-Smith 2020-03-17.

