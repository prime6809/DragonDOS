;
; DragonDos 1.0, Copyright (C) 1982, Dragon Data Ltd.
;
; Disassembled 2020-03-09, P.Harvey-Smith.
;
		use	cpudefs.asm
		use	dgndefs.asm
		use	dosdefs.asm
		use	romdefs.asm
		use	basictokens.asm
		use 	basicdefs.asm
		use	samdefs.asm
		use	wddefs.asm
				
* Dragon Alpha has a third PIA at FF24, this is used for
* Drive select / motor control, and provides FIRQ from the
* disk controler.

DPPIADA		EQU	DPPIA2DA
DPPIACRA	EQU	DPPIA2CRA		
DPPIADB		EQU	DPPIA2DB		
DPPIACRB	EQU	DPPIA2CRB

PIADA		EQU	DPPIADA+IO	; Side A Data/DDR
PIACRA		EQU	DPPIACRA+IO	; Side A Control.
PIADB		EQU	DPPIADB+IO	; Side A Data/DDR
PIACRB		EQU	DPPIACRB+IO	; Side A Control.

;WD2797 Floppy disk controler, used in Alpha Note registers in reverse order !
DPCmdReg	EQU	DPCmdRegA	; command/status			
DPTrkReg	EQU	DPTrkRegA	; Track register
DPSecReg	EQU	DPSecRegA	; Sector register
DPDataReg	EQU	DPDataRegA	; Data register

CmdReg		EQU	DPCmdReg+IO	; command/status			
TrkReg		EQU	DPTrkReg+IO	; Track register
SecReg		EQU	DPSecReg+IO	; Sector register
DataReg		EQU	DPDataReg+IO	; Data register

NextResJump	EQU	BasStub2+StubResJumpOfs		; Jump to reserved word handler of user table
NextFuncsJump	EQU	BasStub2+StubFuncsJumpOfs	; Jump to functions handler of user table

        org     $C000

; Disk controler ID, if a cartrage starts with the chars 'DK', then the basic rom routines
; will do a JMP to $C002 to init the cartrage.

DosBegin
	FCC     /DK/		; Disk cartrage auto-init identifier.

LC002   BRA     DosInit

; Indirect jump table, these are probably the official DragonData entry
; points to be called with jsr [entry]

        FDB     DOSLowLevel		; Low Level disk IO routine
        FDB     DosCommand		; Address of data table for low level command
        FDB     DOSValidFilename	; Validate filename & copy to disk block.
        FDB     DOSOpenFile		; Open A file.
        FDB     DOSCreateFile		; Create file (make backup)
        FDB     DOSGetFLen		; Get file length
        FDB     DOSCloseAll		; Close all open files
        FDB     DOSCloseFile		; Close file
        FDB     DOSFRead		; Read data from file
        FDB     DOSFWrite		; Write data to file
        FDB     DOSGetFree		; Get free space on a disk
        FDB     DOSDeleteFile		; Delete a file

	ifdef PGSFix
	FDB	DOSProtectMC		; Protect/unprotect file
	FDB	DOSRenameMC		; Rename a file 
	else
        FDB     DOSProtectMC		; Protect/unprotect file
        FDB     DOSRename		; Rename a file 
        endc
	
	FDB     DOSGetDirEntry		; Get a directory entry
        FDB     DOSFindAndRead		; Find free buffer and read sector
        FDB     DOSSyncDir		; Copy updated sectors from track 20 to 16 (sync direcories)
        FDB     DOSReadAbsSector	; Read absolute sector
        FDB     DOSWriteAbsSector	; Write absolute sector (no verify)

;
; Init Dos
; 

DosInit LDX     #DosAreaStart		; Point to bottom of dos vars area	
        TFR     X,Y		
LC02F   CLR     ,X+			; Clear a byte, increment X
        LEAY    -1,Y			; decrement counter
        BNE     LC02F			; loop again if more to do
	
; X now points to the top of the dos variable area

        TFR     X,D
        TFR     A,B
        ADDB    #$18
        STB     <BasStartProg		; Setup new begining of basic
        JSR     >BasLocateScreen
        LDA     <GrDisplayStartAddr	; Adjust graphics ram pages
        ADDA    #$06
        STA     <GrLastDisplayAddr

;
; Init various low ram stuff, interrupt vectors, basic stub etc
; 

        LDX     #NewVectorTable		; Point to rom copy of data to copy
LC049   LDB     ,X+			; Get byte count byte
        BEQ     LC054			; zero= end of table, exit copy
        LDU     ,X++			; Get destination address
        JSR     >UtilCopyBXtoU		; do copy
        BRA     LC049			; do next


LC054   LDB     #$00			; Set hardware control mask
					; Was $20 for DDv1
        STB     DosHWMaskFF48
       	COM     DosVerifyFlag		; function unknown 
       
	LDX     #DosNewUSRTable		; Adjust usr vector base
        STX     <BasUSRTableAddr	
        LDU     #BasFCError		; Setup the 10 usr vectors to point to BasFCError
        LDB     #$0A			; do 10

LC066   STU     ,X++			; setup vector
        DECB				; decrement count
        BNE     LC066			; loop again if more to do
	
        INC     DosDefDriveNo	
        BSR     DosReset
	
        LDX     #VectBase		; Point to ram hooks
        LDY     #RamHookTable		; Point to ram hook table in rom
        LDD     #$137E			; load A with number of vectors B with opcode for JMP
LC07A   STB     ,X+			; setup jump
        LDU     ,Y++			; setup vector
        STU     ,X++
        DECA				; decrement counter
        BNE     LC07A			; Loop again if more to do
        
	LDX     #$4549			; Look for DK at $E000
        CMPX    $E000
        LBEQ    $E002			; Found it, call it's init routine
	
EIEND   LDX     #ResetVector		; Setup new reset vector
        STX     <IndVecReset
        LDA     #$55
        STA     <WarmStartFlag
	andcc	#~(FlagIRQ+FlagFIRQ)	; reenable inturrupts
	
        LDX     #BasSignonMess		; Print staandard Basic signon message
        JSR     >TextOutString
        INC     <DosDriveNo
	
        LDX     #$8000			; delay counter
        CLRA				
LC0A4   MUL				; take some time....
        LEAX    -1,X			; decrement count
        BNE     LC0A4			; loop again....
	
        JSR     >DosDoRestore
        JSR     >TextCls		; clear screen
	
        LDX     #DosSignonMess-1	; Print dos signon message
        JSR     >TextOutString	
        JMP     >BasCmdMode		; Jump to normal basic command mode

DosReset
	LDA     #WDCmdForceInt		; Force WD2797 to interrupt & reset
        STA     CmdReg
	
	ifdef	PGSFix
        LDX     #DosD0Online		; Clear drive online flags 
	else
        LDX     #DosD0Online-1		; Clear drive online flags 
	endc
	
        CLR     ,X			; initialize drive info tables
        CLR     1,X
        CLR     2,X
        CLR     3,X
        
	LDX     #Drv0Details+DrvDetUseCnt	; Clear drive in use counts
        CLR     ,X
        CLR     6,X
        CLR     12,X
        CLR     $12,X
	
        CLR     <DosIOInProgress 	; Flag to check for timeout
	
        LDX     #DosDirSecStatus 	; Clear Dirctory status, FCBs etc
LC0D9   CLR     ,X+
        CMPX    #DosFCBEnd
        BNE     LC0D9
	
        LDB     #$04			; Count of buffers to process
        LDX     #Buff1Details		; Setup disk buffer initial values
        LDU     #$0800			; addr of buffer
LC0E8   CLR     2,X
        STB     4,X
        STU     5,X
        LEAU    $0100,U			; Increment addr for next buffer
        LEAX    7,X			; do next buffer
        DECB				; done all ?
        BNE     LC0E8			; no : do next
        RTS

;
; The following code is quite clever, there are actually multiple code paths.
; This involves having a 2 byte instruction encoded as the oprand to a 3
; byte instruction eg :-
;
; L00FA	CMPX	#$C605
;
; CMPX is one byte long, so a jump to L00DB, will execute the oprand to the
; CMPX, which decodes as LDB #$05 this saves a few bytes by not having LDB #xx,
; BRA label.
;
; There are several examples of this in the Dos ROM !
;
	
DosDoReadAddr	
	LDB	#DosFnReadAddr		; Read address
	FCB	Skip2	

DosDoWriteTrack   
	LDB	#DosFnWriteTrack	; Write track
	FCB	Skip2	

DosDoWriteSecN   
	LDB	#DosFnWriteSecN 	; Write sector no verify 
	FCB	Skip2		

DosDoWriteSecV   
	LDB	#DosFnWriteSecV		; Write sector verify
	FCB	Skip2		

DosDoReadSec   
	LDB	#DosFnReadSec		; Read sector

DosDoFuncinB   
		PSHS    A
        LEAS    -2,S			; Make room on stack
        CLR     Thrash			; clear work byte		
LC10D   LDA     #$03  			; retry count ?
        STD     ,S				; save on stack

LC111   BSR     DosDoSeek		; Try to seek to track
        BCS     LC11E			; Error ?
	
        LDB     1,S			; No error, get dos op code
        STB     <DosCommand		; Save operation to perform
        JSR     >DOSLowLevel
        BCC     LC15A			; No error
	
LC11E   CMPB    #ErrWP		; Write protect ?, no point in retrying !
        BEQ     LC149
        
	DEC     ,S			; Dec retry count
        BEQ     LC13B			; Any retries left ?
        LDB     ,S			; get retry count
        LSRB				; gety lsbit
        BCC     LC133			; on even numbered retry, do recal
        INC     <DskTrackNo		; step out a track
        BSR     DosDoSeek
        DEC     <DskTrackNo		; step back in when retrying 
	BRA     LC111

LC133   LDA     <DskTrackNo		; Save Track no whilst doing restore
        BSR     DosDoRestore		; Restore & Recalibrate
        STA     <DskTrackNo		; Put track no back
        BRA     LC111			; Try again

; We come here when all reties exhausted
LC13B   CMPB    #ErrNR		; Not ready ?
        BNE     LC157

        TST     DosAreaStart
        BNE     LC157

        COM     DosAreaStart
        BRA     LC153

; Something to do with dir track ? Make sure same op done to both copies of Dir
LC149   LDA     <DskTrackNo		; Get track number
        CMPA    #DirPrimary		; Track 20 ?
        BNE     LC157			; no : error
        LDA     #DirBackup		; set track to 16
        STA     <DskTrackNo
LC153   LDB     1,S			; Get Dos op code
        BRA     LC10D			; So same to track 16

LC157   ORCC    #FlagCarry		; Flag error
        FCB	Skip1			; opcocode for BRN
LC15A   CLRB				; Flag no error
        LEAS    2,S			; Drop bytes from stack
        PULS    A,PC			; restore & return

;
; Another LDB, enbeded in CMPX sequence....
; 

DosDoReadSec2
	LDB     #DosFnReadSec2		; Read sector 2 (just first 2 chars)
	FCB	Skip2		

DosDoSeek   	
	LDB	#DosFnSeek
	FCB	Skip2			; Seek to a track

DosDoRestore   
        LDB	#DosFnRestore		; Restore to track 0
        STB     <DosCommand		; save in hardware byte

;
; Disk I/O.
;
; This is the main Disk I/O entry point. 
; All registers are preserved except the condition codes and B on the
; return. (error code).
;
; Call :
; 	DosDriveNo 	= drive number
;	DskSectorNo	= sector number
; 	DskTrackNo	= track number
;	DiskBuffPtr	= buffer pointer
;	DosCommand	= Dos function to execute (see below)
;
;		DosFnRestore	($00), Restore to track 0
;		DosFnSeek	($01), Seek to a track
;		DosFnReadSec	($02), Read a sector
;		DosFnWriteSecV	($03), Write a sector with verify
;		DosFnWriteSecN	($04), Write a sector no verify
;		DosFnWriteTrack	($05), Write (format) track
;		DosFnReadAddr	($06), Read address mark
;		DosFnReadSec2	($07), Read first two bytes of a sector
;
; Exit conditions :
;	
;	DosDiskError 	= WD2797 status register, or zero if no error.			
;	B 		= error code for basic or zero if no error.
;	CC.carry	= 0 if no error, 1 if error.
;

DOSLowLevel
	PSHS    CC,A,DP,X,Y,U
        ORCC    #(FlagIRQ+FlagFIRQ)	; Disable interrupts	
        ROR     ,S
        CLR     <DosDiskError		; Flag no disk error
        LDA     <DosCommand		; get HW byte
        CMPA    #$07			; Valid op
        
	BLS     LC179			; yes : do it !
        BRA     LC181			; No: error

LC179   JSR     >ResetAndStartDrive
        BCC     LC18C			; Error ?
        LDB     #$FD			; yes flag error

        FCB     Skip2			; Andothe CMPX saving.....

LC181   LDB     #$FC			; Set error code
        ORCC    #FlagCarry		; and carry bit
        ROL     ,S
        STB     <DosDiskError		; Flag disk error
        LBRA    LC230

LC18C   LDX     #PIA0CRA		; Point to PIA CR A		

; roughly equivilent to SetPIA in DDv1
; gets the control registers of PIA0 and PIA1, saves them on the stack
; and then disables interrupts from those PIAs, to prevent them interfereing
; with the disk system.
 
SavePIAs   
	LDA     ,X			; get PIA control register
        PSHS    A			; save it on the stack 
        ANDA    #$FC			; disable interrupts from PIAxA
        STA     ,X++			; save it, move to CRB
        LDA     ,X			; get CRB
        PSHS    A			; save on stack
        ANDA    #$FC			; disable interrupts from PIAxB
        STA     ,X			; save it
        LEAX    30,X			; move to next PIA
        CMPX    #$FF23			; reached PIA2 yet?
        BLO     SavePIAs		; nope, keep going

        LDA     PIA2CRB			; get control register B of PIA2 
        ORA     #$03			; enable interrupt, rishing edge
        STA     PIA2CRB			; save back to PIA


	LDA     #$03
        PSHS    A
        
        LDA     <DskSectorNo		; Get disk sector no
        CMPA    #SectorsPerTrack	; >$12, therefore on second side
        BLS     LC1B5			; no: don't adjust
	
        SUBA    #SectorsPerTrack	; Calculate sector number on second side

        LDB     #$02			; Flag as side 2 (Dragon WD2797 only)
        ORB     DosHWMaskFF40	
        STB     DosHWMaskFF40

LC1B5   STA     SecReg
        STA     <DosSectorSeek

LC1B8   LDY     <DBZero
        LDX     <DiskBuffPtr		; Point to buffer
        LDB     <DosCommand		; Get hardware byte (function code)
        LSLB				; Calculate offset in table
        
        LDU     #DosFunctionTable	; Point to function dispatch table
        LDA     <DosSectorSeek		; get sector seeking to

        CMPA    SecRegA			; Compare to sector register
        BNE     LC1B5			; not same write again			

        LDA     #$FF			; Set DP=$FF, to make IO quicker
        TFR     A,DP
	JSR     [B,U]			; Jump to function handler
	
        STA     <DosDiskError		; Save error code
        BCC     LC1E6			; No error : check verify
        BRA     LC214

LC1E6	ANDA    DosErrorMask		; Mask out error bits we are not interested in
        STA     <DosDiskError		; save errors for later use
        BEQ     LC1FB			

        LDA     <DosCommand		; Get operation code
        CMPA    #DosFnReadSec2		; ReadSec2 command ?		
        BEQ     LC1F7			; yes 

        DEC     ,S			; Dec retry count
        BNE     LC1B8			; retry, if tries remaining

LC1F7   ORCC    #FlagCarry		; Flag error
        BRA     LC214

LC1FB	TST     DosErrorMask		; is this write sector ?
        BPL     LC214			; no : jump ahead

        LDA     <DskTrackNo		; Get track number
        CMPA    #DirPrimary		; Primary Dir track ?
        BEQ     LC211			; yep 

        CMPA    #DirBackup		; Secondary dir track ?
        BEQ     LC211

        TST     DosVerifyFlag		; should we verify?
        ANDCC   #~(FlagCarry)		; Flag no carry
        BPL     LC214			; no, skip ahead

LC211	LBSR    DosDoReadSec2		; verify the write
LC214	LEAS    1,S
        ROL     4,S
        LDX     #DosD0Track-1		; Point to drive track table
        LDB     <DosDriveNo		; Get last used drive
        ABX				; get pointer to track for current drive
        LDA     TrkReg			; Get current track number from WD
        CMPA    <DskTrackNo		; Same as current track no ?
        BEQ     LC236

        LDB     DosErrorMask		; is this a seek ?
        CMPB    #$19
        BNE     LC236

        LDB     #ErrSFF		; Error code : no error
        STB     <DosDiskError
        ROR     4,S
        ORCC    #FlagCarry		; flag error
        ROL     4,S

LC236	STA     ,X			; update track table

; Restore PIA control registers saved in SavePIAs above.

        LDX     #$FF23			; point at PIA1 CRB
LC23B   PULS    A			; get CRB value
        STA     ,X			; write it to PIA
        PULS    A			; get CRA value
        STA     ,--X			; write it to PIA, move pointer
        LEAX    -30,X			; point to previous PIA
        CMPX    #$FF01			; done all ?
        BHI     LC23B			; nope loop again

        CLR     DosHWMaskFF40		; Clear hardware mask
LC24E
LC230	CLRB				; Flag no error
        PULS    CC,A,DP,X,Y,U		; Restore regs
        BHS     LC28A			; no error, exit

        LDB     <DosDiskError		; get last error code
        BEQ     LC28A			; no error, exit

        BPL     LC279
;
; Work out the dragon error code that coresponds to the hardware
; error reported by WD.
;        

        CMPB    #$FC
        BNE     LC261

	LDB     #ErrPR		; parameter
        BRA     LC288

LC261
        CMPB    #$FD
        BNE     LC269

        LDB     #BErrDN			; Device Number 
        BRA     LC288

LC269
        CMPB    #$FE
        BNE     LC271

        LDB     #ErrNR		; not ready
        BRA     LC288

LC271
        CMPB    #$FF
        BNE     LC286

        LDB     #ErrSK		; seek
        BRA     LC288

LC279   TFR     B,A
        LDB     #$82
        ROLA
LC27E   ADDB    #$02
        ROLA
        TSTA
        BLO     LC288
        BNE     LC27E

LC286	LDB     #ErrUD		; undefined
LC288	ORCC    #FlagCarry
LC28A   RTS

;
; Reset controler chip, and spin up drive.
;

ResetAndStartDrive   
	LDA     #WDCmdForceInt		; Force interrupt
        STA     CmdReg
        LDA     <DosDriveNo		; Get last active drive no
        BEQ     LC27C
	
        CMPA    #$04			; Drive valid ?
        BLS     LC29B
LC27C   ORCC    #FlagCarry		; Flag error
DosHookRetDevParam   
	RTS

; Generate Alpha drive mask from drive number, do drive no
; left shifts to get the correct mask

LC29B   LDB     #$01			; initialize mask at 1
LC29D   LSLB				; shift it left
        DECA				; decrement drive no
        BNE     LC29D			; keep going until zero

        LSRB				; compensate for one shift too many
        ORB     #MotorOnA		; turn on motor
        PSHS    B			; save it
        LDA     DosHWMaskFF48		; get saved hw mask
        ANDA    #~DriveMaskA		; mask drives off in HW mask	
        ORA     ,S+			; combine with drive we want
        STA     DosHWMaskFF48		; re-save hardware mask
        LBSR    AlphaDskCtl		; send it to AY-8912, turn on drives etc

        LDA     PIA2CRA			; get control register of PIA2 side A
        ORA     #$38			; set CA2 high	(enable NMI?)		
        STA     PIA2CRA			; 
        
	LDX     #DosD0Track-1		; Point to current track table
        LDA     <DosDriveNo		; Get active drive
        LDA     A,X			; Get drive current track
        STA     TrkReg			; Write to controler
        LDA     #$D2			; set timeout		
        STA     DosTimeout
	CLRB				; no error ?
        RTS

;
; Dos function 0 restore
;

DosFunctionRestore   
	CLRA				; zero track no
        STA     >DskTrackNo		; Save Track number
        BRA     LC2B9

;
; Dos function 1 seek
; 

DosFunctionSeek
	LDA     >DskTrackNo		; Get current track no
        CMPA    DPTrkReg		; Are we over it ?
        BNE     SeekTrackinA		; no : seek to it
        CLRA
        STA     DosErrorMask		; Turn off verify
        TFR     A,DP			; Reset DP
        RTS
;
; Seek to a track, routine will exit either with an error, or via NMI.
; On entry A contains track to seek to.
;

SeekTrackinA
	STA     <DPDataReg		; <Eval43 ;DataReg  		
        LDA     #WDCmdSeek		; Seek command
	
LC2B9   LDB     #$19
        STB     DosErrorMask
        LDX     #DosD0StepRate-1	; Point to step rate table
        LDB     >DosDriveNo		; Get active drive
        ORA     B,X			; Mask in that drive's step rate
        STA     DPCmdReg		; save in command reg
LC2C8   MUL				; burn up CPU time waiting....
        MUL				; NMI will exit this loop
        LEAY    -1,Y			; decrement timeout counter
        BNE     LC2C8			; count exhausted ? : no keep going
        BRA     DosHWError		; yes : error

;
; Dos function 6 : Read address mark (Dragon)
; 

DosFunctionReadAddr   
	LDA     #WDCmdReadAddr		; Read address mark
        FCB     Skip2			; CMPX again :)
;
; Dos function 2 : Read sector (Dragon/Cumana)
; 
DosFunctionReadSec
	LDA     #WDCmdReadSec		; Read a sector
        LDB     #WDDefErrMask		; set error mask
        STB     DosErrorMask

        LDB     #$05			; try 5x$FFFF for timeout
        ORA     DosHWMaskFF40		; Mask in side etc
        STA     DPCmdReg
LC2E1   
	LDA     DPPIACRB		; Check for INT from WD
        BMI     LC2F3			; yes : start reading bytes
        LEAY    -1,Y			; decrement timeout count
        BNE     LC2E1			; check for int again, if no timeout
	
        LDA     DPPIADB
	ifdef	PGSFix
	BRN     LC2F2
	else
	BMI     LC2F2
	endc
        DECB				; decrement retry wait count
        BNE     LC2E1			; count=0 ?, no continue waiting
        BRA     DosHWError		; yes : error and exit

;
; Read sector/address mark loop, exits with NMI.
;

LC2F2   SYNC
LC2F3   LDB     DPPIADB			; clear interrupt
	LDA     DPDataReg		; read byte from WD
        STA     ,X+			; save in buffer
        BRA     LC2F2			; do next byte

;
; Dos function 7 read first two bytes of a sector, used by BOOT command.
;

DosFunctionReadSec2   
	LDX     #$004F			
        LDA     #WDDefErrMask		; Set error mask
        STA     DosErrorMask

;
; Code to wait for DRQ when using Dragon/Cumana controlers.
;
        LDB     #$05			; Wait 5x$FFFF 
        LDA     #WDCmdReadSec		; Read sector command
        ORA     DosHWMaskFF40		; mask in heads etc
        STA     DPCmdReg		; write it to WD
LC30C   
	LDA     DPPIACRB		; Check for Int from WD
        BMI     LC327			; yes : start reading
        LEAY    -1,Y			; decrement timeout
        BNE     LC30C			; check again
        LDA     DPPIADB			; try clearing int
	
	ifdef	PGSFix
	BRN     LC327
	else
	BMI     LC327
	endc
	
        DECB				; decrement outer count
        BNE     LC30C

DosHWError   
	LDA     #WDCmdForceInt		; Force the WD to abort
        STA     DPCmdReg
        CLRA				; Reset DP to page 0
        TFR     A,DP
        LDA     #$FE			; return error
        ORCC    #FlagCarry
        RTS

;
; Read data from WD, as normal exited with NMI
;
; Read bytes code when using Dragon/Cumana controlers.
;

LC327   LDA     DPDataReg		; read byte from WD
        TST     DPPIADB			; clear interrupt
        STA     ,X+			; save in memory
        
	SYNC				; wait for next
	
	LDA     DPDataReg		; get next byte
        TST     DPPIADB			; clear interrupt
        STA     ,X			; save byte

LC334   SYNC
	TST     DPPIADB			; clear interrupt
        LDA     DPDataReg		; read byte
	BRA     LC334
	

;
; Dos function 4 Write sector no verify
;

DosFunctionWriteSec2
	LDA     #WDErrMask5F		; set error mask
        FCB     Skip2

;
; Dos function 3 Write sector with verify
;

DosFunctionWriteSec
	LDA     #WDErrMaskDF		; set error mask
        STA     DosErrorMask
        BSR     DosSetPrecomp		; Setup write precomp

;
; Write sector for Dragon/Cumana controlers.
;
	LDA     #WDCmdWriteSec		; Write sector
        ORA     DosHWMaskFF40		; Mask in side etc
        STA     DPCmdReg		; write to WD

        LDA     ,X+			; fetch first byte to write
LC34E   
	LDB     DPPIACRB		; Int from WD ?
        BMI     LC35B			; yes : start writing
        
	LEAY    -1,Y			; decrement timeout
        BNE     LC34E			; if not timed out continue waiting
        BRA     DosHWError			; timout, abort, and return error

; Write sector loop, NMI breaks out of here

LC358  	LDA     ,X+			; get byte to write
        SYNC				; wait for WD
	
LC35B   STA     DPDataReg		; write byte to WD
        LDA     DPPIADB			; clear interrupt
        BRA     LC358

;
; Dos function 5 write track.
;
; Will return WTErr if N.F.G.
;
; The format of the write buffer is somewhat strange for this routine.
; The reason for this is so that we can format a diskette without hosing 
; the poor user's basic program.
; The method of run length coding only requires $22B bytes instead of 
; the normale $0000 bytes.
;

DosFunctionWriteTrack   
	LDA     #WDErrMaskFormat	; set error mask	
        STA     DosErrorMask
        BSR     DosSetPrecomp		; Set write precomp

        LDA     #WDCmdWriteTrack	; Write (format) track			
        ORA     DosHWMaskFF40		; Mask in head etc
        STA     DPCmdReg
	
LC36F   LDD     ,X++			; Get bytes to write A=count, B=byte
LC371   SYNC				; Wait for WD

	CMPA    DPPIADB			; Clear interrupt
        STB     DPDataReg		; Write a byte on track
	DECA				; decrement byte count
        BNE     LC371			; continue until count=0

        LDA     ,X+			; get next 
        SYNC
	
        CMPA    DPPIADB			; Clear int
	STA     DPDataReg		; write to wd
	BRA     LC36F

;
; Set write precompensation based on track
;

DosSetPrecomp
	LDA     DPTrkReg		; Get track 
        CMPA    #TrackPrecomp		; track < 16
        BLS     LC38F			; no : no write precomp
	
	LDA     DosHWMaskFF48		; Enable precomp
        ORA     #WPCEnA
        BRA     LC394

LC38F   LDA     DosHWMaskFF48		; Turn off precomp
        ANDA    #~WPCEnA	;#$EF
LC394 
	LBSR	AlphaDskCtl		; Write control reg
	RTS
	ENDC

;
; BOOTDSK dispatch routine
; 
; Syntax is the same as for DSKINIT below.
;
	
CmdBootdsk
        PSHS    CC			; save flags
        DEC     <DosMakeSysDsk		; flag that we are making a system disk
        PULS    CC			; recover flags fall through to DSKINIT	
;
; Dskinit dispatch routine
;
; Syntax :
;	DSKINIT				(default drive,sides,tracks)
;	DSKINIT drive			(specified drive, default sides,tracks) 
;	DSKINIT drive,sides		(specified drive,sides default tracks) 
;	DSKINIT drive,sides,tracks	(specified drive,sides,tracks)
;
		
CmdDskInit   
	BEQ     LC3BC			; No parameters : use defaults
        JSR     >GetDriveNoInB		; Get drive no
        STB     <DosDriveNo		; save it
        JSR     >GetCommaThen8Bit	; Get comma, and then no of sides
        BEQ     LC3C1			; Error, use default sides & tracks
	
        DECB				; Convert sides to zero base
        CMPB    #$01			; > 1 sides specified : error & exit
        BHI     LC3B9			; Error : use default tracks
	
	STB     <DosRecLenFlag		; Save sides
        JSR     >GetCommaThen8Bit	; Get comman, then tracks
        BEQ     LC3C3			; Error : use default tracks
	
 	CMPB    #$28			; 40 tracks ?
        BEQ     LC3C5			; Yes skip on
	
        NEG     <DosRecLenFlag
        CMPB    #$50			; 80 tracks ?
        BEQ     LC3C5			; yes, skip on
LC3B9   JMP     >DosPRError

;
; Set defaults for format : disk=1,sides=1,tracks=40
;

LC3BC   LDB     DosDefDriveNo		; use default drive
        STB     <DosDriveNo		; save in last used
LC3C1   CLR     <DosRecLenFlag		
LC3C3   LDB     #FmtDefTracksA		; no of tracks to format by default
LC3C5   STB     <DosDSKINITraks
     
;
; <DosDSKINITraks = tracks to format
; <DosRecLenFlag  = sides-1 so singlesided = 0, double sided = 1
;
   
	JSR     >DosCloseAllFiles	; close all files error if can't
        LBNE    DosJmpToSysError
	
        LDX     #DosDiskBuffBase	; Point to the buffer base
        STX     <DiskBuffPtr
        JSR     >DosDoRestore		; Restore to track 0
        BNE    	DosJmpToSysError	; error : exit
	
        LDA     #$01			; start at sector 1
        STA     <DskSectorNo		
        JSR     >DosDoReadSec2		; try reading it
        CMPB    #$80			
        BEQ     DosJmpToSysError
	
LC3E5   CLR     <DosDSKINIHead		; do head 0
        CLR     <DskSectorNo		; start at sector 0
        
	JSR     >SetupTrackLayout	; setup track layout in ram
	JSR     >DosDoWriteTrack	; write the track
        BCS     DosJmpToSysError	; error : exit
        
	TST     <DosRecLenFlag		; is this a double sided disk ?	
        BEQ     LC404			; nope skip
        LDA     #$01			; do side 1
        STA     <DosDSKINIHead
        NEGA
        STA     <DskSectorNo

	JSR     >SetupTrackLayout	; setup track layout in ram
	JSR     >DosDoWriteTrack	; write the track
        BCS     DosJmpToSysError	; error : exit

LC404   INC     <DskTrackNo		; increment track 
        LDA     <DskTrackNo		
        CMPA    <DosDSKINITraks		; have we done all yet ?
        BCS     LC3E5			; nope do next track
	
        JSR     >DosDoRestore		; finished formatting, restore to track 0
        BCS     DosJmpToSysError
	
LC411   JSR     >DosDoSeek		; seek to track 
        BCS     DosJmpToSysError	; error : exit
	
        CLRA
        JSR     >CmdDskInitVerifyTrack	; verify current track
        INC     <DskTrackNo		; move to next track
        LDA     <DskTrackNo
        CMPA    <DosDSKINITraks		; done all ?	
        BCS     LC411			; nope : do next track
	
        LDX     <DiskBuffPtr		; point at disk buffer
        LDD     <DBZero	; d=0
LC426   STD     ,X++			; fill buffer with zeros
        CMPX    #DskInitBuffer+(3*256)	; end of buffer?
        BNE     LC426			; nope keep filling
	
        LDA     #$01			; sector 0 
        STA     <DskSectorNo
        LDA     #DirPrimary		; Directory track
        BSR     BuildAndWriteBAM	; build and write BAM 

        DEC     <DskSectorNo		; Point back at first BAM block 
        DEC     <DiskBuffPtr
        LDA     #DirBackup		; Directory backup track
        BSR     WriteBAM		; Write BAM sectors
        BRA     MakeBlankDir

BuildAndWriteBAM   
	PSHS    A
        BSR     BuildBAM		; Build BAM and geometry info
        PULS    A

WriteBAM
	STA     <DskTrackNo		; Write sector to track
        JSR     >DosDoWriteSecV
        BCS     DosJmpToSysError	; error : exit
	
        INC     <DskSectorNo		; do sector 2
        INC     <DiskBuffPtr
        JSR     >DosDoWriteSecV		; Write sector to track
        BCS     DosJmpToSysError	; error : exit
        RTS
	
;
; Exit with error, allow basic to handle it.
;

DosJmpToSysError           
	CLR     >DosMakeSysDsk
	JMP     >DosHookSysError	; Jump to basic error handler

; Fill in a blank directory sector, setting default attributes.
; Bug : this seems to only fill in the backup track, as the call 
; to Lc489 will never return !
MakeBlankDir   
	INC     <DiskBuffPtr		; Increment buff pointer by 1 page
        LDX     <DiskBuffPtr			
        LDD     #(AttrAtFormat*256)+DirEntPerSec	; Get default attribute & number of entries			
LC460   STA     ,X			; Fill in attributes
        LEAX    DirEntryLen,X		; move to next entry
        DECB				; any more : continue
        BNE     LC460			
	
        BSR     WriteNewDir		
        
        LDA     #DirPrimary		; Do primary dir track
        STA     <DskTrackNo
	BSR	WriteNewDir
	
        TST     <DosMakeSysDsk		; are we making system disk?
        BEQ     LC4D0			; nope exit

        JSR     DosDoRestore		; restore to track 0
        BLO     DosJmpToSysError	; error exit	

        LDX     #DosBegin		; point to beginning of dos
        LDB     #BootFirstSector	; first sector of boot area			
LC4B1
        STB     <DskSectorNo		; set sector
        STX     <DiskBuffPtr		; point at dos
        JSR     DosDoWriteSecV		; write the sector
        BLO     DosJmpToSysError	; error exit

        LEAX    256,X			; move up one sector's worth
        CMPX    #DosEnd			; beyond end of Dos
        BEQ     LC4D0			; yep, exit 

        LDB     <DskSectorNo		; increment sector number
        INCB
        CMPB    #BootLastSector+1	; done all sectors?
        BNE     LC4B1			; no loop again

        INC     <DskTrackNo		; increment track number
        LDB     #$01			; reset to sector 1
        BRA     LC4B1			; loop again

LC4D0
        CLR     >DosMakeSysDsk		; flag not making system disk
        RTS

WriteNewDir
	LDD     #$1003			; Process 16 sectors starting at sector 3
        STB     <DskSectorNo
LC473   JSR     >DosDoWriteSecV		; go write the sector
        BCS     DosJmpToSysError	; error : exit
	
        INC     <DskSectorNo		; do next sector
        DECA				; decrement count
        BNE     LC473			; keep going if more

        JMP     >DosReset		; reset dos

;
; Build the block availability bitmap in sectors 1 and 2 of directory track
; this does 3 things.
; 1) Marks all physical sectors as available, adjusting for DS/SS & 40/80 tracks
; 2) Marks all directory track sectors as being in use, ajusting for DS/SS disks.
; 3) Fills in disk geometry bytes in the first BAM sector.
;
BuildBAM   
	STA     <DskTrackNo
        LDA     #SectorsPerTrack
        LDB     #BAMEntries40SS		; used to calculate number of needed BAM entries
        TST     <DosRecLenFlag		; Double sided ?
	BEQ     LC48C
        
        LSLB				; Double number of sectors as DS
        LSLA				; Double number of BAM entries as DS
LC48C   STA     DskInitBuffer+DirSecPerTrk	; save sectors / track
        COMA
        STA     DskInitBuffer+DirSecPerTrk1s	; save complement for error check

        LDA     <DosDSKINITraks		; Get number of tracks
        STA     DskInitBuffer+DirTracks	; Save no of tracks

	TST     <DosRecLenFlag		; double sided ?	
        BNE     LC507			; no.....

        CMPA    #$50			; 80 track drive?
        BNE     LC507			; no.....

        LSLB				; double number of BAM entries as 80 track
LC507
        COMA				; Complement for error check
        STA     DskInitBuffer+DirTracks1s	; Save no of tracks check
        
	LDX     <DiskBuffPtr
        LDU     #DskInitBuffer+(256*1)	; 1 sector into buffer
        
	
	TST     <DosMakeSysDsk		; are we making a system disk?
        BEQ     LC522			; no 

        LDA     <DosDriveNo		; get last active drive
        CMPA    #$02			; external drive?
        BHI     LC522			; yes.....

        SUBB    #$05			; reserve second half of boot file
        LEAX    4,X			; in BAM
        LDA     #$FC
        STA     ,X+
LC522
	LDA     #$FF			; Mark a block of sectors free
LC4A3   STA     ,X+
        DECB				; Any more BAM groups ?
        BNE     LC4A3

        LDD     #(BAMOffDirBakSS*256)+BAMOffDirPriSS ; get offsets in BAM of dir sectors SS	
        TST     <DosRecLenFlag		; double sided ?
        BEQ     LC4BC
        BPL     LC4B9
	
        LDD     #$B4FF
LC4B4   STB     ,U+
        DECA
        BNE     LC4B4

LC4B9   LDD     #(BAMOffDirBakDS*256)+BAMOffDirPriDS ; get offsets in BAM of dir sectors DS 

LC4BC   LDU     <DBZero	; U=0
        PSHS    A			; Mark track 20 dir in use
        BSR     LC4C4
        PULS    B			; mark track 16 dir in use

LC4C4   LDX     <DiskBuffPtr		; get pointer to BAM
        ABX				; calculate offset
        LDA     #$FC			; mask for last 2 sectors
        STU     ,X++			; mark dir sectors in use
        STA     ,X
        RTS

CmdDskInitVerifyTrack   
	CLR     <DskSectorNo		; Sector 0
        TST     <DosRecLenFlag		; is it DS ?
        BEQ     LC4D6			; nope just do side 0
        BSR     LC4D6			; do side 0 then 1
	
LC4D6   LDA     #SectorsPerTrack	; Sector counter
LC4D8   INC     <DskSectorNo		; next sector
        JSR     >DosDoReadSec2		; go read it
        LBCS    DosJmpToSysError	; error : exit
        DECA				; decrement count
        BNE     LC4D8			; loop again if more
LC4E4   RTS

;
; Setup format block for write track
;
; The track format used by write track is made up of records of 3 bytes :
;
; byte 0	repeat count
; byte 1	data byte
; byte 2	terminator byte
; 
; The low level write track then takes the byte counts and outputs the repeated 
; bytes for us. This is slightly more complex than the systems used on other 
; systems (e.g. Nascom, Atom, BBC) where the entire track is laid out in memory
; raw. However this has the major advantage of needing much less memory, meaning
; the track layout can be kept within the disk buffers. This means that (unlike 
; the Atom and BBC DFS) formatting a disk does not corrupt any of the non DOS 
; related memory - neat.
;

SetupTrackLayout   
	LDU     <DiskBuffPtr		; Point to disk buffer
        LDX     #TrackHeaderTable
        LDY     #SectorIDTable		; Table of sector IDs with required interleave		
        
; Write track header
	LDB     #TrackHeaderSize	; Count
        BSR     DosUtilCopyBXtoU	; copy bytes
	
; Write sector ID 	
LC4F2   LDX     #SectorIDLayout		; first part of sector ID
        LDB     #SectorIDP1Len		; count 
        BSR     DosUtilCopyBXtoU	; copy bytes
        
; now fill in the C,H,R,N values for the sector	
	LDA     #$01			; repeat count 1 byte	
        LDB     <DskTrackNo		; get Cylinder number 
        STD     ,U++			; save in buffer
        LDB     <DosDSKINIHead		; get Head 
        STB     ,U+			; save in buffer (as term byte)
        LDB     ,Y+			; get sector (Record) number 
        STD     ,U++			; save count of 1 plus sector number
        STA     ,U+			; save sector size of 1 (N=256)
        
; Write end of sector ID + Sector data area
	LDB     #SectorIDP2Len		; count of bytes in rest of sector ID and sectro data
        BSR     DosUtilCopyBXtoU	; coopy bytes
	
        TST     ,Y			; done all sectors?
        BNE     LC4F2			; nope loop again

; Write track filler bytes
        LDB     #TrackFillerLen		; no of bytes at end of track

DosUtilCopyBXtoU   
	JMP     >UtilCopyBXtoU

;
; GetCommaThen8Bit, scan for comma, error if not found, then fetch 8 bit that follows (or error). 
;

GetCommaThen8Bit
	JSR     <BasChrGetCurr		; Get current basic char
        BEQ     LC4E4			; Any left no: return 
        JSR     >VarCKComma		; check for comma
        JMP     >Get8BitorError		; go get it

;
; Basic BACKUP command
;
; BACKUP src_drv TO dest_drv[,s][,t]
;	src_drv		= source drive no 1..4
;	dest_drv	= destination drive no 1..4
;	s		= number of sides 1 or 2
;	t		= number of tracks 40 or 80
;
; Stack frame
; 	$0015	Ram buffer size in sectors
;	$0014	Sectors / tracks
;
;  Destination Drive ---------------------------	
;
;	$0012	Ram pointer
;	$0010	String pointer
;	$0009	Sector
;	$0008	Track
;	$0007	Drive
;
;  Source drive --------------------------------
;	
;	$0005	Ram pointer
;	$0003	String pointer
;	$0002	Sector
;	$0001	Track
;	$0000	Drive
;  U->
;
; U is used as a pointer to the beginning of the stack frame. 
; Below this is the stack usage for the subroutines within the backup.
;
;


CmdBackup   
	LEAS    -BupStackFrame,S	; Make tempory space on stack
        TFR     S,U			; Point U at base of tempory space (Like OS-9 !)
        TFR     U,D		
        SUBD    #$0040			; reserve room for working stack
        SUBD    <BasVarEnd		; Check that we have suficient memory available
        LBMI    BasOMError		; NO: report ?OM error
	
        CMPA    #$01			; At least 1 sector's worth of ram (256 bytes) available
        LBLT    BasOMError		; NO: report ?OM error
        STA     BupAvailPages,U		; Store memory page count of avaiable RAM
        LDA     #SectorsPerTrack	; Sectors per track, initially 18 for SS disk
        STA     BupSecTrk,U
        LDD     <BasVarEnd		; Get end of RAM in use by basic
        STD     BupSrcBuff,U		; save in buffer pointers for source and dest
        STD     BupDestBuff,U

        LDD     #MessInsertSource-1	; Insert source and destination message pointers
        STD     BupSrcMess,U
        LDD     #MessInsertDest-1
        STD     BupDestMess,U
        
        LDD     #$0001			; Set source and dest track and sector to 0 & 1
        STD     BupSrcTrk,U
        STD     BupDestTrk,U
        LDA     DosDefDriveNo		; Get default drive no
        STA     ,U			; save in source drive
        STA     BupDestDrive,U		; and dest
        LDY     #(SectorsPerTrack*40)	; sector count 720 sectors=ss40 disk

	JSR     <BasChrGetCurr
        BEQ     DoCmdBackup		; No params backup from default drive to default 
        JSR     >Get8BitorError
        CMPB    #MaxDriveNo		; greater than Max drive (4)?
        LBHI    DosDNError

        STB     ,U			; Save source drive
        STB     BupDestDrive,U		; and default dest to same drive

	JSR     <BasChrGetCurr		; Get current character from basic
        BEQ     DoCmdBackup		; end of line : yes do backup

 	CMPA    #DTokTO			; is this the "TO" token ?
        BNE     CmdBackupErrorExit

 	JSR     <BasChrGet		; Get next char, skip over "TO"
        JSR     >Get8BitorError		; Get dest drive in B
        CMPB    #MaxDriveNo		; greater than Max drive (4)?
        LBHI    DosDNError

        STB     BupDestDrive,U		; Save in Dest driveno

	BSR     GetCommaThen8Bit	; Skip comma, and get next param
        BEQ     DoCmdBackup		; nothing : do backup

        CMPB    #$02			; 2 sided disk specified ?
        BEQ     BackupDS		; yes backup double sided
        CMPB    #$01			; 1 sided disk specified ?
        BEQ     BackupSS		; yes backup single sided


CmdBackupErrorExit   
	JMP     >BasSNError		; error : exit

BackupDS   
	TFR     Y,D			; Double sector count if double sided
        LEAY    D,Y
        ASL     BupSecTrk,U		; Set sectors per track for DS disk

BackupSS   
	JSR     >GetCommaThen8Bit	; Get next param (if any)
        BEQ     DoCmdBackup		; none: continue
        CMPB    #$50			; Do 80 tracks ?
        BEQ     Backup80
        CMPB    #$28			; Do 40 tracks ?
        BEQ     DoCmdBackup
        BRA     CmdBackupErrorExit	; neither : error

Backup80
	TFR     Y,D			; Double sector count if 80 track
        LEAY    D,Y

DoCmdBackup   
	CLRA
BupReadFromSrc   
	LEAY    1,Y			; Get sector count
        LEAX    ,U			; point to source drive on stack frame
        BSR     BupCheckPrompt		; Check if drives are same & prompt if so
	
LC5B2   LEAY    -1,Y			; decrement sector count
        BNE     LC5BC			; if more sectors, do next
        BSR     BupWriteToDest
        LEAS    BupStackFrame,U		; Clear stack frame
LC5BB   RTS

LC5BC   CMPA    BupAvailPages,U		; Filled all available RAM pages ?	
        BNE     LC5CE			; no : do next sector
        BSR     BupWriteToDest		; Yes : write to destination
        PSHS    D
        LDD     <BasVarEnd		; Get end of basic storage
        STD     BupDestBuff,U		; Save in source and dest buffer pointers
        STD     BupSrcBuff,U
        PULS    D
        BRA     BupReadFromSrc		; Do next sector

LC5CE   LDB     #DosFnReadSec		; Read the sectors
        BSR     LC5E4			; go do it
        INCA				
        BRA     LC5B2

BupWriteToDest   
	TSTA
        BEQ     LC5BB
        LEAX    BupDestDrive,U		; Point to dest drive vars
        BSR     BupCheckPrompt		; Check if drives are same & prompt if so
        LDB     #DosFnWriteSecV		; write sectors to destination
LC5DE   BSR     LC5E4			; go do it
        DECA				; decrement sector count
        BNE     LC5DE			; if more go again
LC5E3   RTS

LC5E4   PSHS    D
        LDA     ,X			; Get source drive
        STA     <DosDriveNo		; make source drive the current drive

        LDD     BupSrcBuff,X		; point to source buffer
        STD     <DiskBuffPtr
        LDD     BupSrcTrk,X		; get source track and sector
        STD     <DskTrackNo		; set them
        LDB     1,S			; get the function code read or write
        JSR     >DosDoFuncinB		; Ask dos to do it !
        BCC     LC60D			; no error, skip on
        STB     DosErrorCode		; save error code

        LDA     1,S			; Get function, read or write
        CMPA    #DosFnReadSec		; Read ?
	BNE     LC607
        PULS    D,X

	JSR     >BupWriteToDest
LC607   LDB     DosErrorCode		; Retrieve error code
        JMP     >DosHookSysError

LC60D   INC     BupSrcSec,X		; Move to next source sector
        LDA     BupSrcSec,X
        CMPA    BupSecTrk,U		; still sectors on this track to read ?
        BLS     LC61B			; yep : keep going
	
        LDA     #$01
        STA     BupSrcSec,X		; set source sec to 1
        INC     BupSrcTrk,X		; increment source track
LC61B   INC     BupSrcBuff,X		; move to next memorty page
        PULS    D,PC

;
; Check if source and dest drives are the same and if so prompt to swap disks.
;

BupCheckPrompt   
	LDB     ,U			; get source drive
        CMPB    7,U			; same as dest drive ?
        BNE     LC5E3			; no : continue
	
        PSHS    A,X,Y,U
        JSR     >TextCls		; clear screen
        LDX     1,S			; get message pointer
        LDX     3,X
        JSR     >TextOutString		; Print message (insert source/insert dest)
        LDX     #MessPressAnyKey-1
        JSR     >TextOutString		; Print press any key
        JSR     >TextWaitKeyCurs2	; Wait for a kepress
        JSR     >TextCls
        PULS    A,X,Y,U,PC

;
; Get8BitorError, get non zero 8 bit value in B, or generate error
;
Get8BitorError
	PSHS    Y,U
        JSR     >VarGet8Bit		; Get 8 bit value into B
        TSTB				; B non zero ?
        BNE     LC64A
        JMP     >BasFCError		; No : error
	
LC64A   PULS    Y,U,PC			; Restore and return
	
DosCmdDispatch   
	CMPA    #$FF			; Invalid token ?
        LBEQ    BasSNError
        SUBA    #DDTokFirstC		; Make token number zero based
        BPL     LC659			; valid token : yep continue
	
LC656   JMP     >BasSNError		; nope SNError

LC659   CMPA    #DDTokCountC		; check token in range
        BCC     LC663			; Nope, continue to next jump table
        LDX     #CommandDispatchTable	; Point to command address table
        JMP     >BasDoDispatch		; go do it !
        
LC663   JMP     [>NextResJump]		; Jump to user reserved word handler >$0137

DosFuncDispatch   
	SUBB    #$44
        BPL     LC66D			; Check token in range, skip if ok
        BRA     LC656			; else ?SN Error

LC66D   CMPB    #(DDTokCountF*2)	; check token in range
        BCC     LC679			; nope : skip to next handler
        LDX     #FunctionDipatchTable	; point to function table
        JSR     [B,X]			; jump to function
        JMP     >VarGetExprCC		; return value to basic

LC679   JMP     [>NextFuncsJump]	; Jump to user function handler >$013C

; test and flush all buffers

TestAndFlushAll   
	LDX     #Buff1Details		; Point to first buffer
LC680   JSR     >TestAndFlushBuffer	; Flush if needed
        BNE     DosHookSysError		; error : exit
	
        CLR     BuffFlag,X		; mark buffer free
        LEAX    BuffDetailSize,X	; move to next buffer
        CMPX    #(Buff1Details+(BuffCount*BuffDetailSize))	
	
        BCS     LC680			; done all : no loop again
LC68E   RTS

;
; Get drive no in B, returns drive no (from command) in B,
; or causes error if (drive < 0 ) or (drive > 4)
;

GetDriveNoInB   
	JSR     >VarGet8Bit		; Get 8 bit var
        TSTB
        BEQ     DosDNError
        CMPB    #$04			; Valid < 4 ?
        BLS     LC68E			; yes : skip
	 
DosDNError   
	LDB     #BErrDN			; Device no error
	FCB	Skip2		

DosPRError
	LDB 	#ErrPR		; Parameter error

DosHookSysError   
	STB     DosErrLast		; save last error code
        LDX     <BasCurrentLine		; Get current line no
        STX     DosErrLineNo		; save for ERR routine
        JSR     >BasResetStack		; reset basic stack
        CLR     <DosIOInProgress	; Flag no IO in progress
        JSR     >CasMotorOff		; turn off tape motor
        JSR     >SndDisable		; disable sound
        CLR     <TextDevN		; Set device no back to console
        
	TST     DosErrGotoFlag		; Do we have an error handler ?
        BPL     LC6C1			; Yes, handle errors
        LDX     <BasCurrentLine		; Get current line no
        LEAX    1,X
        BNE     LC6D2

        JSR     >TextOutCRLF		; output EOL

LC6C1   JSR     >TextOutQuestion 	; output '?'
        LDX     #BasErrorCodeTable	; Point to error code table $82A9
        LDB     DosErrLast		; Get last error code
	BPL     LC6CF
        
	LDX	#(DosErrorCodeTable-DDFirstError) 	; Get pointer to error table !
LC6CF   JMP     >SysErr2		; Jump to basic Error handler

LC6D2   LDX     #BasBRARun		; Go back to main interpreter loop $84DA
	PSHS    X			; push X as return address
        CLR     DosErrGotoFlag		; clear error goto flag
        LDD     DosErrDestLine		; get on error goto line
        STD     <BasTempLine		; save it in temp line
        JMP     >BasSkipLineNo		; jump to it.

;
; New reset vector
;

ResetVector
	NOP				; Main ROM checks for reset->NOP
        CLRA				; Reset DP=0
        TFR     A,DP		
        JSR     >DosReset		; Reset WD, and various Dos vars.
        CLR     DosErrorMask		; reset various flags
        CLR     DosTimeout
        CLR     DosAutoFlag
        LDA     #$35			; Re-enable NMI
        STA     PIA0CRB
        JMP     >WarmStart		; Jump back to Main ROM reset routine

;
; NMI vector, called to break out of read & write loops between 6809 & WD
; This allows the IO routines to handle sectors of all lengths.
;

NMISrv   
	LDA     DPCmdReg		; Read status register.
        CLRB				; Reset DP=0
        TFR     B,DP
        LEAS    12,S			; Drop registers from stack
        TSTA				; Setup CC

LC6FF   RTS

;
; New IRQ vector, used to count down and shut off drives.
;
; Note PGS fixes to IRQSrv could be re-arranged to avoid all the jumping around.
; but for now applying them unmodified.
IRQSrv  
	CLRA				; Make sure DP is set to 0
	TFR	A,DP
	TST	<DosIOInProgress 	; Doing IO ?
	
IRQSrv2
	BNE     LC71E			; Yes: don't time out
        LDA     DosTimeout		; Get timeout byte 
        BEQ     LC71E			; Already timed out : exit
 
        DECA				; Decrement timeout count
        STA     DosTimeout	

        BNE     LC71E			; not zero, don't timeout yet
        BSR     DOSSyncDir		; syncronsise directory
        BNE     LC721			; Error : report it
        LDA     DosHWMaskFF48		; turn off motor / deselect drives in hw byte
        ANDA    #0				
	STA     DosHWMaskFF48		; Just turn off drives on Dragon
  	LBSR	AlphaDskCtl		; Actually turn off motor
LC71E	
	JMP     >BasIRQVec		; Jump to BASIC IRQ

LC721   JMP     >DosHookSysError

;
; This code seems unused ! but checks for at least a free page of memory
; between the top of the basic vars and the bottom of the stack.
; 

Unknown TFR     S,D			; Get stack pointer in D
        SUBD    #$0100			; move down a page
        SUBD    <BasVarEnd		; Subtract end of bas vars			
        BMI     LC731			; -ve so out of memory
  	CLRB				; clear LSB of new stack
        TSTA				; At least a page free ?
	BNE     LC6FF			; yep : return
LC731   JMP     >BasOMError

;
; Copy updated track 20 to track 16
;
; Backup the directory to the alternate directory.
;
; No calling arguments.
;
; Stack frame.
;	$0008	Sector number
;	$0007	Sector number
;	$0006	Sector number
;	$0005	Sector number
;	$0004	Number of full buffers
;	$0003	Current sector number
;	$0002	User's drive number
;	$0001	Last error
;	$0000 	bitmask for 'DTYDIR'
;

DOSSyncDir   
	JSR     >TestAndFlushAll	; Flush all buffers if needed
        LEAS    -8,S			; Make room on stack
        LEAU    ,S			; Point U at stack frame
        LEAY    SyncSectors,U		; point Y at sync sector table
	
        LDX     #DosDiskBuffBase	; Point at tempory buffer area
        STX     <DiskBuffPtr
        CLR     SyncBufferNo,U		; clear buffer no counter
        LDB     <DosDriveNo		; Get last accessed drive
        STB     SyncDrive,U		; Save it
        LDB     #$01			; Drive counter shifted left to count
        STB     ,U
        CLR     <DosDriveNo
        LDX     #DosDirSecStatus-1	; $06AA
	
LC751   LDB     #SectorsPerTrack	; Sector count
        STB     SyncSecNo,U
        INC     <DosDriveNo

LC757   LDB     SyncSecNo,U		; get sector no
        LDA     B,X			; get it's status byte
        BITA    ,U			; test it
        BEQ     LC77D
	
        COMA
        ANDA    B,X
        STA     B,X
        INC     SyncBufferNo,U		; move to next buffer
        STB     <DskSectorNo
        STB     ,Y+
        LDB     #DirPrimary		; Track 20
        STB     <DskTrackNo		
        JSR     >DosDoReadSec		; Go read sector
        
	BNE     LC81D			; Error !
	
        INC     <DiskBuffPtr		; use next disk buffer
        LDB     SyncBufferNo,U		; Check to see if we have filled all buffers
        CMPB    #$04			
        BCS     LC77D			; nope keep going
        BSR     LC797			; flush them
	
LC77D   DEC     SyncSecNo,U		; decrement sector no
        BNE     LC757			; keep going if more sectors
	
        TST     SyncBufferNo,U		; any buffers in use ?, so sectors still waiting to be flushed ?
        BEQ     LC787			; no  skip
        BSR     LC797			; yes : flush them
	
LC787   ASL     ,U			; move to next drive			
        LDA     ,U				
        CMPA    #$08			; done all drives ?	
        BLS     LC751			; nope do next
	        
        LDA     SyncDrive,U		; Restore last used drive
        STA     <DosDriveNo
        
LC81A	LEAS    8,U			; Drop stack frame
	CLRB				; Flag no error
LC81D   RTS

LC797   LDA     #DirBackup		; Backup track no
        STA     <DskTrackNo
LC79B   DEC     <DiskBuffPtr		; Move to previous buffer
        LEAY    -1,Y
        LDA     ,Y
        STA     <DskSectorNo		; Pickup sector no
        JSR     >DosDoWriteSecV		; Go write it
        BEQ     LC7AB
        
	ifdef 	PGSFix
	LEAS    8,U
        else
	LEAS    2,S
	endc
        RTS

LC7AB   DEC     SyncBufferNo,U		; move to previous disk buffer
        BNE     LC79B
LC7AF   RTS


FIRQSrv
        TST     PIA2DB			; Clear interrupt conditions 
        TST     PIA2DA
        RTI				; and return

;
; Validate and open a file.
;
; Entry conditions are as for DOSValidFilename (below).
; Exit conditions are as for DOSOpenFile if carry clear, else
; error code in B if carry set.
;

DosValidateAndOpen
	BSR     DOSValidFilename	; Validate filename
        BNE     LC8B5			; Error : exit
        JMP     >DOSOpenFile		; Open file if valid

;
; Validate filename and copy to current drive block
;
;	On entry:
;	  X points to filename e.g. '3:FILENAME.EXT'
;	  B length of filename e.g. 0x0e
;	  Y points to default extension to use if none is given e.g. 'DAT'.
;           Use '   ' for no default extension
;	
;	If no drive given default drive (DosDriveNo) is used.			
;		
;	On Return:
;	  Filename appears at $0650-$065a (DosCurFilename)
;	  Current drive (DosCurDriveNo) is set
;	  CC.Z clear on error
;	  B contains error code
;	  U $065b always (SuperDosE6)
;
; Filenames can be of the following formats :
; 	"d:filename.ext"
;	"filename.ext:d"
;	"d:filename"
;	"filename:d"
;	"filename"
;	"filename.ext"
;

DOSValidFilename   
	ifdef	PGSFix
	JSR	LDFF3			; prevent null / invalid filenames
	else
	LDA     DosDefDriveNo
	endc
	
LC7C1   STA     DosCurDriveNo		; Set current drive number, default if non specified
        CLR     DosCurCount
        LDU     #DosCurDriveInfo	; Point at current drive info

        LDA     #$07			; Zero out first 8 bytes (filename)	
LC7CC   CLR     A,U
        DECA
        BPL     LC7CC
        
	LDA     2,Y			; Transfer extension into current details
        STA     DosCurExtension+2	; $065A
        LDA     1,Y
        STA     DosCurExtension+1	; $0659
        LDA     ,Y
        STA     DosCurExtension		; $0658

        CMPB    #MaxFilenameLen		; Filename too long ?
        BHI     LC829			; Yep : error
	
	TSTB				; null filename length?
	BEQ	LC829			; Yep : error
	
        CMPB    #$03			; Long enough to contain drive no ?
        BCS     LC811			; nope : skip on
	
; Because of the order of compare a drive letter at the END of the filename always
; takes presedence, this would only be siginificant if the filename where something like
; '1:2' which would access a file called '1' on drive 2, and NOT 2 on drive 1
	
        SUBB    #$02			; Look for drive no at end of filename
        LDA     B,X
        CMPA    #':'			; Seperator present ? $3A
        BNE     LC7F6			; No skip on
        INCB
        LDA     B,X			; Get drive no
        INCB
        BRA     LC800

LC7F6  	ADDB    #$02			; Check for drive at begining of path
        LDA     1,X
        CMPA    #':'			; Seperator present ? $3A
        BNE     LC811			; nope, use default drive
	
        LDA     ,X++			; Get ascii drive no
LC800   SUBA    #$30			; Work out drive number
	
	ifdef	PGSFix
	BLS	LC808
	CMPA    #MaxDriveNo		; Drive valid ?
        BLS	LC810
LC808	LDB	#$28
	RTS
	NOP				; nop for resync of assembled file
	else
        LBLS    DosDNError		; error if -ve
	
	CMPA    #MaxDriveNo		; Drive valid ?
        LBHI    DosDNError		; error if too big
	endc
	
	STA     DosCurDriveNo		; Set current drive if specified
        SUBB    #$02

; Parse filename looking for extension seperator
	
LC811   LDA     ,X+			; Get next char
LC810   DECB				; Decrement path count
        BMI     LC87A			; Reached end : yes skip
	
        CMPA    #'/'			; Check for slash $2F
        BEQ     LC81E

        CMPA    #'.'			; Check for period $2E
        BNE     LC83A

LC81E   CMPU    #DosCurDriveInfo	; $0650
        BEQ     LC829
        
        TST     DosCurCount		; First pass ?
        BEQ     LC82C			; yes : skip on

LC829   LDB     #ErrFS		; Error : bad filespec 
LC8B5   RTS

LC82C   INC     DosCurCount		; Mark second pass
        LDU     #DosCurExtension	; $0658
        CLR     ,U			; Zero out extension
        CLR     1,U
        CLR     2,U
        BRA     LC811

; Validate filename chars

LC83A   CMPA    #'A'			; $41
        BCS     LC84E			; Below, check if still valid
	
        CMPA    #'Z'			; $5A
        BLS     LC85A			; valid, skip on
	
        SUBA    #$20			; Convert to lower case if upper
        CMPA    #'A'			; $41
        BCS     LC829			; Invalid, return error
	
        CMPA    #'Z'			; $5A
        BLS     LC85A			; Valid: skip on
        BRA     LC829

LC84E   CMPA    #'-'			; $2D
        BEQ     LC85A			; Valid skip on
	
        CMPA    #'0'			; $30
        BCS     LC829			; Invalid : error
	
        CMPA    #'9'			; $39
        BHI     LC829			; Invalid : error
	
LC85A   STA     ,U+			; Save char in path
        CMPU    #DosCurDriveNo		; Path full ?
        BNE     LC867			; nope : skip
        
	TSTB				; Done all path chars ?
        BNE     LC829			; nope : error !
        BRA     LC87A

LC867   CMPU    #DosCurExtension	; Reached extension ? $0658
        BNE     LC811
	
        LDA     ,X+			; Get next 
        DECB				; Dec count
        BMI     LC87A			; Done, return
	
        CMPA    #'.'			; Check for seperator $2E
        BEQ     LC81E			; yep loop back
	
        CMPA    #'/'			; Check for seperator $2F
        BEQ     LC81E			; Yep loop back
	
LC87A   CLRB
        RTS

;
; Open a file and copy dir entry into FCB.
;
; The name in DosCurFilename is searched for in the FIBs, if found then
; the FIB number is returned in A. If not found a FIB is created and the
; disk is searched for the filaname. If not file is found ErrNE is returned
; in B.
;
;  On entry:
;	    Filename at DosCurFilename
;	    Drive no at DosCurDriveNo	
;	  Returns:
;	    CC.Z clear on error
;	    A FIB number (0-9)
;	    B contains error code
;
DOSOpenFile   
	LDX     #DosFCB0Addr		; Point to first FCB
        CLR     <DosCurrCtrlBlk
        LDD     DosCurDriveInfo		; Get first 2 bytes of current drive info
LC884  	CMPD    ,X			; Does this FCB point to it ?
        BNE     LC89F			; Nope : check next
	
; Found matching first 2 bytes of name in an FCB
	
        LDU     #DosCurDriveInfo+2	; Check bytes 2..end of filename
        LEAY    2,X			; Compare from byte 2 of FCB
        LDB     #$0A			; Do 10 bytes, rest of filename + ext
LC890   LDA     ,U+			; Get a byte from current
        CMPA    ,Y+			; compare to FCB
        BNE     LC89C			; Don't match : exit check
        DECB				; Decrement counter
        BNE     LC890			; Not at end : do next
        LBRA    LC947


; Move to check next FCB

LC89C   LDD     DosCurDriveInfo		; Re-get first 2 chars of current filaname
LC89F   LEAX    DosFCBLength,X		; Skip to next FCB
        INC     <DosCurrCtrlBlk		; Set current control block
        CMPX    #DosFCBEnd		; End of blocks ?
        BCS     LC884			; No, loop back and check this block
	
        CLR     <DosCurrCtrlBlk		; Set current block to zero
        LDX     #DosFCB0Addr		; Point at first FCB

LC8AE   TST     ,X			; FCB in use ?
        BEQ     LC8BF			; No : skip on
	
        LEAX    DosFCBLength,X		; Check next FCB
        INC     <DosCurrCtrlBlk		
        CMPX    #DosFCBEnd		; Done all FCBs
        BCS     LC8AE			; No : check next, yes error, can't open file, no free FCBS
        LDB     #ErrTF		; error : too many files open
LC8BE   RTS

LC8BF  	LDB     #$0C			; Copy 12 characters of filename
        TFR     X,Y			; Point Y at selected FCB
        LDU     #DosCurDriveInfo	; Point at current info
LC8C6   LDA     ,U+			; Copy filename
        STA     ,Y+
        DECB				; Dec count
        BNE     LC8C6			; if not all done : do next
	
        STA     <DosDriveNo		; Save current drive

; Note in disassembled superdos source, the following was LDU #$0616, which is part of the error line !
; This makes no sense, and is Drv0Details, in DragonDos source,	I think I just fixed a 20 year old
; bug !!!!!!

        LDU     #Drv0Details		; Get drive details	 
        LDB     #DrvDeatailLen		; 6 bytes/drive
        MUL
        LEAU    D,U			; Point at drive detail block
        INC     DrvDetUseCnt,U		; Increment usage/open file count       
	
	LDB     #$13			; Clear rest of FCB
LC8DB   CLR     ,Y+
        DECB				; Dec counter
        BNE     LC8DB			; Loop if more
	
        LDA     #AttrDeleted		; Flag file as deleted by default
        STA     FCBDirFlags,X
	
        CLR     DosTempFileNo		; start temp fileno at 0
        JSR     >DosGetDiskGeometry	; get disk geometry
	BNE     LC8BE			; error : exit
        
        LDY     ,X			; get LSN number from buffer			
        LEAY    2,Y			; add 2
        LDA     #DIRSecCount		; set number of sectors to scan
        STA     DosCurCount

LC8F6   STY     DosLSNCounter		; save LSN
        JSR     >DOSFindAndRead		; go read it
        BNE     LC8BE			; error
	
        LDX     BuffAddr,X		; get address of data buffer
        LEAU    DirLastByte,X		; point U at first byte after last entry
        STU     DosSaveBuffAddr		; save buff addr

LC908   LDA     ,X			; get first data byte (attribute)
        BITA    #(AttrDeleted+AttrIsCont) ; is this a deleted or continuation entry ?	
        BNE     LC928

        LDD     DosCurFilename		; get first 2 characters of filename
        CMPD    DirEntFilename,X	; compare to directory entry
        BNE     LC928			; not the same, skip to next

        LDU     #DosCurFilename+2	; check the rest of the name
        LEAY    DirEntFilename+2,X	; point to name[2] in entry		
        LDB     #$09			; check 9 characters, 6 left from name + 3 from extension
LC91D   LDA     ,U+			; get character from name
        CMPA    ,Y+			; compare to dir entry
        BNE     LC928			; not equal : give up
	
        DECB				; decrement character count
        BNE     LC91D			; keep going if more to compare
	
        BRA     LC954			; if we get here then we have a match

LC928   LDA     ,X			; get first data byte (attribute)
        BITA    #AttrEndOfDir		; end of directory ?
        BNE     LC944			; yep 
	
        INC     DosTempFileNo		; Move to next fileno
        LEAX    DirEntryLen,X		; move to next dir entry in sector
        CMPX    DosSaveBuffAddr		; beyond last entry ?
        BCS     LC908			; nope process next entry
	
        LDY     DosLSNCounter		; get current DIR LSN
        LEAY    1,Y			; increment it
        DEC     DosCurCount		; decrement directory sector count
        BNE     LC8F6			; if any left loop again
	
LC944   JSR     >DosFCBNoToAddr		; get FCB address
LC947   CLRB
        TST     FCBDirFlags,X		; check flags
        BPL     LC94E			; not a deleted file
	
        LDB     #ErrNE		; flag file does not exist
LC94E   LEAX    FCBFilePointer,X	; point to file pointer !!!
        LDA     <DosCurrCtrlBlk
        TSTB				; set CC.Z on error
        RTS

LC954   PSHS    X			; save dir entry pointer
        JSR     >DosFCBNoToAddr		; get FCB address
        PULS    Y			; recover dir entry pointer
        LDA     DosTempFileNo		; get file no
	
; Fill in FCB

        STA     FCBDiskFileNo,X		; fill in fileno
        LDA     ,Y
        STA     FCBDirFlags,X		; attribute
        
	LDD     DirEntFnBlock1,Y
	STD     FCBLSNExtent1,X		; LSN of extent 1
        
	LDA     DirEntFnBlock1+2,Y
        STA     FCBSecExtent1,X		; sector count of extent 1
        STA     FCBFSNExtent2+1,X
        
	CLR     FCBFSNExtent2,X
        CLR     FCBFSNExtent1,X		; Clear FSN 
        CLR     FCBFSNExtent1+1,X
        
	LDD     DirEntFnBlock2,Y	; LSN of extent 2
        STD     FCBLSNExtent2,X
        
	LDA     DirEntFnBlock2+2,Y	; sector count of extent 2
        STA     FCBSecExtent2,X
        
	LDD     #$FFFF			; set file len to -1
        STD     FCBFileLen,X		
        STA     FCBFileLen+2,X
        BRA     LC947

;
; Read from a file.
;
; Entry :
;	A = FCB no
;	X = pointer to buffer to receive data
;	Y = no of bytes to read
;	U = MSW of file offset (FSN sector no).
;	B = LSB of file offset (byte within sector).
;		U:B is effectivly the filepointer.
; Exit:
;	B = Error code
;	X = no of bytes *NOT* read if error = EOF
;
; Secondary entry points :
;	RWrite	called by DOSFWrite
;	Verify	called by DOSFWrite
;

DOSFRead   
	CLR     <DosRWFlag		; Flag this is a read
        STA     <DosCurrCtrlBlk		; set control block
        BRA     LC99E			; skip ahead

RWrite   
	LDA     #FileOpWrite		; Flag this is a write
	FCB     CSkip2

Verify   
	LDA     #FileOpVerify		; Flag this is a verify
        STA     <DosRWFlag
	
LC99E   STY     DosBytesRead		; save byte count
        LBEQ    DosFReadExit		; no bytes : exit
	
        STU     DosCurrSector		; save file pointer
        STB     DosSecOffset
        PSHS    X			; save buffer pointer
	
        JSR     >DosFCBNoToAddr		; convert FCB no to address in X
	
        LDA     FCBDrive,X		; get drive number
        STA     <DosDriveNo
        
	TST     <DosRWFlag		; Is this a read or write ?
        BNE     LC9C4			; if write skip on
	
        LDD     DosBytesRead		; get bytes read so far
        ADDD    FCBFilePointer+1,X	; add LSW of fileptr
        BCC     LC9C2			; any carry ?
	
        INC     FCBFilePointer,X	; carry to MSB
LC9C2   STD     FCBFilePointer+1,X	; update fileptr

LC9C4   PULS    X			; recover data buffer ptr	

        LDB     DosSecOffset		; get offset into last sector
LC9C9   CLRA		
        NEGB				; distance to end of sector		
        BNE     LC9CE			; not zero, skip
	
        INCA				; increment MSB
LC9CE   CMPD    DosBytesRead		; read all bytes yet ?
        BLS     LC9D7			; nope 
	
        LDD     DosBytesRead		; get bytes read
LC9D7   PSHS    D,X			; save 
        LDU     DosCurrSector		; get current FSN
        JSR     >FSNtoLSN		; convert to LSN
        BNE     DosFReadErrorExit	; error, tydy up stack and return
	
        TFR     D,Y			; transfer LSN to Y
	
        LDA     FCBDirFlags,X		; get directory flags from FIB
        BITA    #AttrWriteProt		; is the file protected?
        BEQ     LC9F2			; no skip
	
        TST     <DosRWFlag		; are we reading or writing?
        BEQ     LC9F2			; writing, and protected!
        LDB     #ErrPT			; protected error
DosFReadErrorExit   
	LEAS    4,S			; clean up stack
        RTS				; return

LC9F2   LDX     2,S			; get pointer to buffer			
        TST     1,S			; test MSB of length
        BNE     LCA22			; skip ahead if nonzero
	
        TST     <DosRWFlag		; are we reading or writing?
        BEQ     LCA03			; reading, do it
	
        BPL     LCA08			; writing, do it
        JSR     >DosVerifyAbsSector	; else go verify it
        BRA     LCA0B			; skip on

LCA03   JSR     >DOSReadAbsSector	; read the sector
        BRA     LCA0B			; skip on

LCA08   JSR     >DOSWriteAbsSector	; write the sector
LCA0B   BNE     DosFReadErrorExit	; error clean up stack and exit

        INC     2,S			; increment MSB of buffer
        LDX     DosCurrSector		; increment current sector
        LEAX    1,X
        STX     DosCurrSector		; and save it back
        DEC     DosBytesRead		; decrement MSB of length
        LDD     DosBytesRead		; get length
        PULS    D,X			; restore length & pointer (flags unchanged)
        BNE     LC9C9			; keep going if more to do	
        RTS				; otherwise return to caller

LCA22   TST     <DosRWFlag		; skip this part on verify
        BMI     RWNext			
	
        JSR     >DOSFindAndRead		; read next sector to buffer
        BNE     DosFReadErrorExit	; error, exit
	
        STX     DosPageBufAddr		; save page buffer pointer
        LDY     2,S			; get user buffer ptr
        LDB     DosSecOffset		; get offset into the sector 
        TST     <DosRWFlag		; reading or writing?
        BEQ     LCA60			; branch if reading

; This code is used if we are writing	
        LDA     #BuffDirty		; mark buffer dirty					
        STA     BuffFlag,X
        LDX     BuffAddr,X		; get address of data in buffer 
        ABX				; add offset within sector
	
LCA44   LDB     1,S			; get byte count
LCA46   LDA     ,Y+			; get data from user's buffer
        STA     ,X+			; put in disk read buffer
        DECB				; decrement count
        BNE     LCA46			; keep going until all done
	
        TFR     X,D			; get disk buffer pointer into d
        TSTB				; reached end of buffer?
        BNE     RWNext			; nope
	
        LDX     DosPageBufAddr		; get Page buffer address in X
        PSHS    Y			; save Y
        JSR     >TestAndFlushBuffer	; flush the buffer to disk
        PULS    Y			; restore Y
        BNE     DosFReadErrorExit	; error, exit
        BRA     RWNext			; keep going

; this code is used if we are reading
LCA60   LDX     BuffAddr,X		; get address of data in buffer 
        ABX				; add offset within buffer
        LDB     1,S			; lsb of length of data
LCA65   LDA     ,X+			; fetch a byte from disk buffer
        STA     ,Y+			; write a byte to user's buffer
        DECB				; decrement byte count
        BNE     LCA65			; keep going until all done

RWNext  LDX     DosCurrSector		; move to the next LSN
        LEAX    1,X
        STX     DosCurrSector		; save it back
        TFR     Y,X			; user's buffer to X	
        LDD     DosBytesRead		; get bytes read
        SUBD    ,S++			; subtract bytes transferred
        STD     DosBytesRead		; save it back
        LEAS    2,S			; drop pointer from stack
        BEQ     DosFReadExit		; branch if done all
        CLR     DosSecOffset		; clear sector offset, so we start 
        CLRB				; at beginning of next sector
        JMP     >LC9C9			; loop again

DosFReadExit   
	CLRB				; no error
        RTS

; 
; Convert a file sector number to a logical sector number.
;
; On entry 
;  U 			FSN that we want
;  DosCurrCtrlBlk	Current FIB no.
;
; Returns:
;  D			Logical sector number
;  X			FIB entry
;  U			FSN (preserved).

FSNtoLSN   
	JSR     >DosFCBNoToAddr		; get FCB addr in X
LCA8E   TFR     U,D			; get FSN in D
        SUBD    FCBFSNExtent1,X		; test to see where extent block is in file
        BCS     LCAA2			; if -ve then this block is after the one we want

; check to see if required sector is within this group of sectors

        TSTA				; If MSB is not 0 then it is not
        BNE     LCAA2			
	
        CMPB    FCBSecExtent1,X		
        BCC     LCAA2

        ADDD    FCBLSNExtent1,X
        BRA     LCAB4

; check extent 2

LCAA2   TFR     U,D			; get FSN in D
        SUBD    FCBFSNExtent2,X		; test to see where extent block is in file
  	BCS     LCAB7			; if -ve then this block is after the one we want
  
; check to see if required sector is within this group of sectors

        TSTA				; If MSB is not 0 then it is not
        BNE     LCAB7
  
	CMPB    FCBSecExtent2,X
        BCC     LCAB7
        
	ADDD    FCBLSNExtent2,X

LCAB4   ORCC    #FlagZero		; Found the block we want flag it
        RTS

; Wanted FSN not in extent 1 or 2

LCAB7   PSHS    U			; save FSN
        BSR     FindFSNinU		; scan extents
        BNE     LCAC8			; error exit
	
        LDD     DosTotalSFound		; how many scanned?
        CMPD    DosFSNToFind		; Have we found it?
        BHI     LCACA			; branch if so
	
        LDB     #ErrPE		; else past EOF error
LCAC8   PULS    U,PC			; restore & return

; At this point Y points to allocation extent block in directory entry.

LCACA   SUBB    AllocCount,Y		; length of extent
        SBCA    #$00			; propogate carry	
        STD     FCBFSNExtent1,X		; start of this extent
        
	LDA     AllocCount,Y		; get allocation sector count
        STA     FCBSecExtent1,X		; save it in extent
        LDD     ,Y			; get allocation LSN
        STD     FCBLSNExtent1,X		; save in extent
	
        TFR     Y,D			; transfer start LSN to D
        PSHS    U			; save FSN
        SUBD    ,S++			; subtract from LSN
        PULS    U			; restore FSN
        
	CMPB    #DirEntryLen-6		; end of entry?
        BCC     LCAF7			; yep, no more extents
	
        LDA     5,Y			; length of next extent
        STA     FCBSecExtent2,X
        LDD     3,Y			; LSN of next extent
        STD     $1A,X
        LDD     DosTotalSFound
        STD     FCBFSNExtent2,X
LCAF7   BRA     LCA8E			; loop again

;
; Scan extents
;
; Entry : 
;   	X	Address of a FCB
;   	U	FSN to find (File Sector Number)
;   	B	File number (on disk), also in $1d,X
;
; Returns with:
;   	DosTotalSFound	number of sectors scanned
;	DosFSNToFind	Original U 
;	Y		Extent entry in dir sector.
;

FindFSNinU   
	PSHS    X
        CLR     DosTotalSFound+1
        CLR     DosTotalSFound
        STU     DosFSNToFind		; set FSN to find 
        LDB     FCBDiskFileNo,X		; set current file to be FCB file
        STB     DosCurFileNo
        JSR     >DOSGetDirEntry		; Go get directory entry
        BNE     LCB56			; Error : exit
        
	TFR     X,U			; point u at dir entry
        PULS    X			; recover FCB pointer
	
        LEAY    DirEntFnBlock1,U	; point Y at first allocation block
        LDB     #$04			; entry counter
LCB17   LDA     ,U			; get attributes 
        ANDA    #AttrContinued		; extract continuation flag	
        BEQ     LCB20			; not a continuation block, skip
	
        LDA     DirEntFlag,U		; get dir entry flag

LCB20   PSHS    D
LCB22   LDD     DosTotalSFound		; get total sectors found so far
        ADDB    2,Y			; add sector count for this allocation block to total
        ADCA    #$00			; carry from b to a if neeeded
        STD     DosTotalSFound		; update total found
        CMPD    DosFSNToFind		; found the sector we need yet ?
        BHI     LCB55			; yep : exit
	
        LEAY    AllocEntrySize,Y	; move to next allocation entry			
        DEC     1,S			; decrement entry counter
        BNE     LCB22			; if more to do loop again

        LDB     ,S			; recover DirEntFlag or AttrContinued	
        BEQ     LCB53			; if it's 0
	
        LEAS    2,S			; drop stack frame
        STB     DosCurFileNo		

        PSHS    X			; save FCB pointer	
        JSR     >DOSGetDirEntry		; get next entry
        TFR     X,U			; make u point to dir entry
        PULS    X			; recover FCB pointer
        BNE     LCBDC

LCB4D   LEAY    DirEntCntBlock1,U	; point at first extension entry			
        LDB     #$07			; 7 entries in a continuation block
	BRA     LCB17

LCB53   LEAY    -3,Y
LCB55   CLRB				; flag no error

LCB56   LEAS    2,S			; Drop stack frame
LCBDC	RTS				; Return

;
; Write to a file.
;
; Entry :
;	A = FCB no
;	X = pointer to buffer to receive data
;	U = no of bytes to read
;	Y = MSW of file offset (FSN sector no).
;	B = LSB of file offset (byte within sector).
;		Y:B is effectivly the filepointer.
; Exit:
;	B = Error code
;

DOSFWrite   
	STA     <DosCurrCtrlBlk		; Set FCB to be current
        STX     DosFWBufPtr		; save params for later
        STU     DosFWByteCount
        STY     DosFWFPoint
        STB     DosFWFPoint+2
	
        JSR     >DosFCBNoToAddr		; Get address of FCB

        LDB     FCBDrive,X		; set drive from FCB
        STB     <DosDriveNo
	
LCB6F   JSR     >DOSGetFLen		; get the file length in U:A
        BEQ     LCB82			; no error : skip
	
        CMPB    #ErrFF		; File not found ?
        BEQ     LCB79			; yep : create the file
LCB78   RTS

LCB79   LDA     <DosCurrCtrlBlk		; Get current FCB
        JSR     >DOSCreateFile		; Create the file
        BNE     LCBDC			; error : exit
        BRA     LCB6F			; success : check file length

; If we come here we have successfuly got the file len

LCB82   CMPU    DosFWFPoint		; is file pointer beyond EOF ?	
        BHI     LCB92			; no : continue
        BCS     LCB8F			; yes : error
	
        CMPA    DosFWFPoint+2		; check LSB, if MSW equal
        BCC     LCB92			; not past end : continue
	
LCB8F   LDB     #ErrPE		; error : past end
LCB91   RTS

LCB92   PSHS    A			; save LSB of length	
        LDD     DosFWByteCount		; get no of bytes to write	
        ADDB    DosFWFPoint+2		; add current filepointer, 
        ADCA    DosFWFPoint+1
        PSHS    B			; save LSB of len+FP
        TFR     A,B			
        LDA     DosFWFPoint		; get MSB
        ADCA    #$00			; deal with carry from LSW to MSB
	
        PSHS    U			
        SUBD    ,S++			; Subtract no of sectors in file from MSW of new filesize 
        TFR     B,A
        PULS    B			; recover LSB of len+FP
        BHI     LCBB8			; new filesize higher extend file
        BCS     LCBC0			; new filesize lower do write
        
	SUBB    ,S			; MSW is equal check LSB
        BCC     LCBBC			; new filesize higher extend file
        BRA     LCBC0			; new filesize lower or equal do write

LCBB8   SUBB    ,S
        SBCA    #$00
LCBBC   BSR     ExtendFile		; extend the file ?
        BNE     LCBE9			; error :  return
	
LCBC0   LEAS    1,S			; drop saved byte
        LDB     DosFWFPoint+2		; recover file pointer LSB
        LDX     DosFWBufPtr		; and buffer
        LDY     DosFWByteCount		; and count
        LDU     DosFWFPoint		; and FP MSB
        
	JSR     >RWrite			; entry point for write
        BNE     LCB91			; error : exit
	
        TST     DosVerifyFlag		; verify written data ?
        BEQ     LCB91			; nope : return
        
	LDB     DosFWFPoint+2		; recover file pointer LSB
        LDX     DosFWBufPtr		; and buffer
        LDY     DosFWByteCount		; and count
        LDU     DosFWFPoint		; and FP MSB
        LBRA    Verify			; go verify  it	

LCBE9   LEAS    1,S			; clean up stack
        RTS				; return 

; 
; Extend a file, called by DOSFWrite
;
; Entry :
;	DosCurrCtrlBlk	= Fileno
;	DosDriveNo	= drive number
; 	D		= number of bytes to extend file
;
; Returns:
;	B		= error code
;
; The data on the disk is not preset / initialized.
;
; Procedure:
; The number of bytes that can be allocated to the last sector allready
; allocated to the file is detected.
; Get a free extent, if contiguous add it to the currently allocated 
; extent, otherwise make a new one.
; Decrease free extent by number of sectors needed (or all),
; If more sectors needed, loop.
; Figure number of bytes used in last sector and store in DIR.
;
; Update length of file and last sector.
;

; stack frame offsets
ExtBytes	EQU	0		; number of bytes to extend**
ExtExtentPtr	EQU	2		; Extent pointer
ExtEntry	EQU	4		; Entry pointer
ExtWant		EQU	6		; Best LSN block found
ExtDiskPtr	EQU	8		; pointer to drive table (DrvDet* consts)
ExtSaveExd	EQU	10		; ?

ExtendFrameSize	EQU	$0C		; stack frame size

; ** Since disk sectors are 256 bytes, and the 6809 stores the MSB in the 16 bit 
; registers pushed onto the stack with MSB at the lowest address (smallest offset 
; from S), ExBytes,S will access the MSB of ExBytes, which will be the number of 
; whole sectors requested.

ExtendFile   
	LEAS    -ExtendFrameSize,S	; make room on stack
        STD     ExtBytes,S		; save number of bytes
        STD     ExtSaveExd,S
	
        LDA     #$01			; Flag that we are working....
        STA     <DosIOInProgress
        JSR     >DosGetDiskGeometry	; get disk geometry
	LBNE    ExtendFileExit		; error exit
	
        STX     ExtDiskPtr,S		; save pointer to geometry buffer
        JSR     >DosFCBNoToAddr		; get address of FCB
        LDB     FCBDirNoLast,X		; get directory index of last file segment
        JSR     >DOSGetDirEntry		; go get it
        LBNE    ExtendFileExit		; error : exit
	
        STX     ExtEntry,S		; save directory pointer			
        LDU     DosCurDirBuff		; get def block for dir sector
        LDA     #-1			; flag it dirty
        STA     BuffFlag,U

; Work out the number of whole sectors we need to extend the file by.
; Round up if ammount to extend / sector size has a remainer.
; number of whole sectors will be in A and remainder in B

        CLRA				
        LDB     DirEntLastBytes,X	; no bytes in last sector
        BNE     LCC1C			; zero, skip
	
        INCA				; inc MSB
LCC1C   ADDD    ,S			; add to number of bytes to extend	
        TSTB				; remainder = 0
        BNE     LCC22			; no.....
        
	DECA				; decrement MSB
LCC22   STD     ,S			; resave amount to extend.	
        TSTA				; more than space in last sector?
        LBEQ    LCCCA			; nope, no allocation change needed
        
	LDB     #DirEntExts		; 4 allocation blocks in directory entry
        LEAY    DirEntFnBlock1,X	; point to first allocation block in dir
        LDA     DirEntAttr,X		; get flags of directory entry
        BITA    #AttrIsCont		; Continuation block?
        BEQ     LCC37			; no, read filename block extents 
        
	LEAY    DirEntCntBlock1,X	; point at first continuation block extent
        LDB     #DirEntCntExts		; get the extent count for continuation block (7)

LCC37   TST     AllocCount,Y		; any sectors allocated?
        BNE     LCC4F			; yes, skip, find last one
        
	LDD     #-1			; max possible allocation....
        STD     ExtWant,S		; save it on stack
        LEAY    -AllocLen,Y
        STY     ExtExtentPtr,S		; save extent pointer on stack
        LDX     ExtDiskPtr,S		; get drive table pointer
        TST     DrvDetFreeLen,X		; check to see if we have free space on disk
        LBNE    LCCE8
        BRA     LCC8D

; find last used allocation block in current directory / continuation block
LCC4F   TST     AllocCount,Y		; any sectors allocated?
        BEQ     LCC58			; no.....
        
	LEAY    AllocLen,Y		; move to next alloc block
        DECB				; decrement alloc block count
        BNE     LCC4F			; loop if more to do
	
LCC58   LEAY    -AllocLen,Y		; point at previous alloc block
        LDD     AllocLSN,Y		; Get LSN of block
        ADDB    AllocCount,Y		; add count to LSN
        ADCA    #$00			; propagate carry
        STD     ExtWant,S		; Save LSN of 1 block past this allocation
        STY     ExtExtentPtr,S		; save dir extent pointer
        LDX     ExtDiskPtr,S		; get disk pointer
        TST     DrvDetFreeLen,X		; Test disk free length
        BEQ     LCC8D					
        
	LDD     ExtWant,S		; get starting LSN for search				
        SUBD    DrvDetFreeEPtr,X	; get free extent pointer
        BEQ     LCC9D			; branch if same
        
	JSR     >GetFreeExtent		; else get a free extent
        BEQ     LCC7C			; branch if OK
        
	CMPB    #ErrDF		; Disk full?
        BNE     ExtendFileExit		; no extend the extent
        BRA     LCCE8			; skip ahead

; replace old extent
LCC7C   LDX     ExtDiskPtr,S		
        PSHS    A,U			; save regs
        LDB     DrvDetFreeLen,X		; get length of free block		
        LDX     DrvDetFreeEPtr,X	; get extent pointer for block
        JSR     >DeleteExtent		; go and delete it
        PULS    A,U			; restore regs
        BNE     ExtendFileExit		; error, exit
        BRA     LCC92

LCC8D   JSR     >GetFreeExtent		; go get free extent
        BNE     ExtendFileExit		; error exit

LCC92   LDX     ExtDiskPtr,S		; point to disk table
        STU     DrvDetFreeEPtr,X	; save LSN of free block
        STA     DrvDetFreeLen,X		; save no of sectors free
        CMPU    ExtWant,S		; compare free pointer to want pointer
        BNE     LCCE8			; skip if not same, new extent

; check if the available space, has enough sectors for the ammount requested.
; see note at begining of ExtendFile, about ExtBytes :) 
LCC9D   LDA     ExtBytes,S		; MSB of wanted bytes (number of whole sectors)		
        CMPA    DrvDetFreeLen,X		; free len
        BCS     LCCA5			; branch if free len lower

        LDA     DrvDetFreeLen,X		; number of sectors free
LCCA5   LDY     ExtExtentPtr,S		; get pointer to extent
        PSHS    A			; free sector count
        ADDA    AllocCount,Y		; add allocation count 
        BCC     LCCB2			; branch if no overflow....		

        PULS    A			; restore sector count
        BRA     LCCE8			; branch....

; Sector count in A still stacked
LCCB2   STA     AllocCount,Y		; update count	
        LDA     DrvDetFreeLen,X		; get free len
        SUBA    ,S			; subtract number of sectors allocated
        STA     DrvDetFreeLen,X		; resave it
        LDD     DrvDetFreeEPtr,X	; get LSN of beginning of free block
        ADDB    ,S			; and number of complete sectors
        ADCA    #$00			; propagate carry
        STD     DrvDetFreeEPtr,X	; resave it
	
        LDA     ExtBytes+1,S		; get number of sectors wanted
        SUBA    ,S+			; subtract no of sectors allocated
        STA     ,S			; resave
        BNE     LCC8D			; if not zero loop again, to allocate more!

LCCCA   LDX     ExtEntry,S		; get entry pointer
        TFR     X,U			; save in U
        JSR     >DosFCBNoToAddr		; get FCB address
        LDD     ExtSaveExd,S		; get original bytes requested
        ADDD    FCBFileLen+1,X		; add LSW of current file len
        BCC     LCCDB			; branch if no carry

        INC     FCBFileLen,X		; increment MSB if carry to poroagate it
LCCDB   STD     FCBFileLen+1,X		; save new file length
        STB     DirEntLastBytes,U	; update bytes in last sector
        CLRB

ExtendFileExit   
	LEAS    ExtendFrameSize,S	; drop stack frame
        CLR     <DosIOInProgress	; clear IO flag
        TSTB				; set error
        RTS

LCCE8   LDD     ExtExtentPtr,S		; get extent pointer
        TFR     D,Y			; save it in Y
        SUBD    ExtEntry,S		
        CMPB    #$13			; room for another entry in dir block
        BHS     NewDir			; no....make a new continuation block
	
        LEAY    AllocEntrySize,Y	; move to next allocation entry
        STY     ExtExtentPtr,S		; save allocation pointer
LCCF7   LDD     DrvDetFreeEPtr,X	; get pointer to free block of sectors
        STD     AllocLSN,Y		; save it in entry
        BRA     LCC9D			; loop to check all extending done

; add a new continuation block to the directory			
NewDir  LDA     #$01			; flag continuation type entry
        JSR     >FindEmptyDir		; find empty directory entry block
        BNE     ExtendFileExit		; error exit
	
        LDY     ExtEntry,S		; get extent pointer
        STA     DirEntFlag,Y		; flag continuation entry
        LDB     DirEntAttr,Y		; get entry attributes
        ORB     #AttrContinued		; flag it as a continuation block
        STB     DirEntAttr,Y		; save it back
	
        LDB     #BuffDirty		; mark disk buffer dirty
        STB     BuffFlag,X
	
        LDB     #AttrIsCont		; mark as continuation
        STB     DirEntAttr,U
        STU     ExtEntry,S
	
        JSR     >DosFCBNoToAddr		; get FCB address in X
        STA     FCBDirNoLast,X
        
	LEAY    DirEntryLen,U
        LDB     #DirEntryLen-1		; zero continuation block to initialize
LCD25   CLR     ,-Y			; clear a byte poiubter back
        DECB				; decrement count
        BNE     LCD25			; loop again if not all done
	
        STY     ExtExtentPtr,S		; save pointer to first extent in block
        LDX     ExtDiskPtr,S		; get disk pointer
        BRA     LCCF7			; loop to allocate 

;
; Get free extent
;
; This must be called by ExtendFile and allocates one extent from the
; free list.
;
; In order of prefference it will find a sector:
;	At the LSN specified. 
;	At the start of a pair of tracks.
;	Outside of the directory track.
;	At the start of the track outside of the directory.
;	At the start of the track inside of the directory.
; 	Any free sector at the start of a contiguous block.
;
; Entry:
;	ExtWant,S	= LSN preffered, of $FFFF.
;	DosDriveNo	= drive number
;
; Returns: 
;	U		= LSN
;	A		= length of extent.
;	B		= error code (if any).
;

GFTry		EQU	0
GFPointBuff	EQU	2	; buffer pointer?
GFlag		EQU	4	; flags?
TempLSN		EQU	5	; Temporary LSN
TempCount	EQU	7	; Tempory count
BTCount		EQU	8

GFFrameSize	EQU	9	; Stack frame size

GetFreeExtent   
	STS     Temps1			; save old stack pointer
        LEAS    -GFFrameSize,S		; make room for our stack frame
        CLR     GFlag,S			; zero flags			
        JSR     >DosGetDiskGeometry	; get disk geometry
        LBNE    GetFreeExtentExit	; error, exit
	
        LDY     DrvDetDirLSN,X		; get LSN of directory
        LDU     (ExtWant+GFFrameSize+2),S	; get ammount we want (note +2 for PC)
        STU     TempLSN,S		; save it in temp
        LEAX    1,U			; is requested LSN = $FFFF 
        BEQ     LCD5B			; yes, an use any block

; test to see if the supplied LSN is in use	
        BSR     GoGetBit		; go get bitmap for requested LSN (U)
        BNE     GoFoundFree		; branch if LSN not in use
	
        CMPU    #BAMLSNPerSec		; LSN < 1440
        BCS     LCD5B			; yes, try stepping in
	
        LDU     #1422			; Try this LSN (+ 1 track) next
        BRA     LCD7C			; else try stepping out

LCD5B   LEAU    ,Y			; U=LSN of directory
LCD5D   LDA     #$FF			; flag GFTry invalid (-ve)
        STA     GFTry,S

; Try the start of each track until we find a free sector on two consecutive
; tracks, or reach the beginning of the disk.
	
LCD61   LEAU    -SecTrkSS,U		; Decrement LSN by one complete track
        STU     -2,S			; TSTU :)
        BMI     LCD74			; branch if LSN < 0
		
        BSR     GoGetBit		; get bitmask for LSN
        BEQ     LCD5D			; branch if LSN in use, try another
	
        TST     GFTry,S			; is GFtry valid?
        BPL     GoFoundFree		; yep we found a valid block	
	
        STU     GFTry,S			; save current sector, as current try
        BRA     LCD61			; look on previous track

LCD74   LDU     GFTry,S
        STU     -2,S			; TSTU :)
        BPL     GoFoundFree		; LSN for try is +ve, test it
	
        LEAU    ,Y			; U=LSN of directory, try track after dir
	
LCD7C   LEAU    SecTrkSS,U		; Increment LSN by one complete track
        CMPU    #MaxLSN			; beyond max LSN?
        BHI     LCD8F			; yes try next test
	
        BSR     GoGetBit		; get bitmask for LSN
        BEQ     LCD7C			; branch if LSN in use, try next track out

GoFoundFree
	LBRA    FoundFree		; we've found a free block, go use it

GoGetBit
	LBRA    GetBit			; jump that alows us to short jump.....			

LCD8F   LDA     #$FF			; Temp  count = -1
        STA     TempCount,S
	
        LDA     GFlag,S			; get flag
LCD95   LDU     <DBZero			; U = zero
        LSRA				; bottom buffer?
        BCS     LCD9D			; yes.....
	
        LDU     #BAMLSNPerSec		; no go get other buffer
LCD9D   BSR     GoGetBit

        LDB     #BAMEntriesSec		; 180 BAM entries per sector....
LCDA1   LDA     ,X+			; get next byte from BAM 
        BNE     LCDAC			; branch if next 8 sectors all free
	
        LEAU    8,U			; increment LSN by 8 (8 LSNs per BAM entry)
        DECB				; decrement entry counter
        BNE     LCDA1			; look again if entry counter nonzero
        BRA     LCDED			; counter zero skip ahead

; look for the bigest block of free LSNs....

LCDAC   CLRB				; Zero BTCount
        STB     BTCount,S
	
        PSHS    U			; save LSN (stack offsets +2 because of push)
LCDB1   INCB				; increment counter
        LDA     ,X+			; get byte from BAM 
        INCA				; is it $FF?
        BEQ     LCDB1			; yes occupied loop again
	
        CMPB    BTCount+2,S		; biggest block found ?	
        BLS     LCDBF			; nope skip on
	
        STB     BTCount+2,S		; Yes save it
        STU     ,S			; update stacked LSN
	
LCDBF   LEAX    -1,X			; back one byte in BAM
        LDA     #SectorsPerBAM		; multiply BAM byte counter by entries 
        MUL				; per BAM byte
        LEAU    D,U			; point LSN at it
	
LCDC6   CLRB				; clear counter
        LDA     ,X+			; get next BAM byte
        BNE     LCDB1			; if not zero loop again
	
        LEAU    SectorsPerBAM,U		; add group of 8 to LSN
        TFR     X,D			; test for end of BAM 
        CMPB    #BAMEntriesSec
        BCS     LCDC6			; nope loop again
	
        PULS    U			; restore LSN of biggest block
        
	BSR     GetBit			; get bitmap
        LEAU    SectorsPerBAM,U		; move up 8 LSNs
        LDA     ,X			; get byte of LSN
	
        LDB     BTCount,S		; get count
        CMPB    #$01			; just 1 group of 8?
        BHI     LCDE6			; branch if not....
	
LCDE1   LEAU    -1,U			; Back up LSN.....			
        ASLA				; shift bitmap
        BCC     LCDE1			; till we get correct LSN for bit...
	
LCDE6   ASLA				; shift bitmap
        BCC     LCDED			; branch if beginning of bitmap byte?
        LEAU    -1,U			; back up LSN
        BRA     LCDE6			; loop again

LCDED   CMPB    #$02			; 2 blocks of 8?	
        BCC     FoundFree		; yep, found enough
	
        CMPB    TempCount,S		; bigger than count
        BHI     FoundFree		; yep, found enough
	
        LDA     TempCount,S		; temp count = $FF
        INCA
        BNE     LCE04			; no....
	
        STB     TempCount,S		; save temp count
        STU     TempLSN,S		; and temp LSN	
        LDA     GFlag,S			; get GFlag
        EORA    #$03			; flip bottom 2 bits
        BRA     LCD95			; go look again.....

LCE04   LDU     TempLSN,S		; get TempLSN
        BSR     GetBit			; go get it's bitmap entries
        BNE     FoundFree		; got some, allocate it	
	
        LDB     #ErrDF			; disk full 
        BRA     GetFreeExtentExit	; exit, cleaning up

FoundFree   
	BSR     GetBit			; Get bitmap
        CLRB				; sector count?
	
LCE11   INCB
        BEQ     LCE2B			; overflow?
	
; this clears the bit in the BAM, reserving the block for the file        
	PSHS    A			; save bitmask
        COMA				; compliment, to get turn off mask
        ANDA    ,X			; turn of bit
        STA     ,X			; resave it
        PULS    A			; restore bitmask
	
        ASLA				; shift bitmask left (try next block)
        BCC     LCE24			; branch if all bits done
        
	LDA     #$01			; re-initialize mask	
        LEAX    1,X			; get next group of blocks' bits from BAM
LCE24   BITA    ,X			; is sector in use ?
        BNE     LCE11			; no reserve it
        TFR     B,A			; yes, get sector count in A
	
	FCB	Skip2
LCE2B   LDA	#$FF			; 
        LDX     GFPointBuff,S		; get disk buffer entry pointer
        LDB     #BuffDirty		; make buffer dirty so it will be re-written
        STB     BuffFlag,X
        CLRB				; flag no error
	
GetFreeExtentExit   
	LDS     Temps1			; restore saved stack pointer
        TSTB				; test B, set CC.C on error
        RTS				; return to caller
;
; Get byte and bitmask for LSN
;
; The appropreate sector is read if not already in memory.
;
; On error returns to the calling program....
;
; Entry:
;	U	= LSN
;	Y	= Directory LSN
;
; Returns:
;	A	= bitmask
;	X	= byte within table
;	U,Y	= preserved.
;
; It is important to note that within the BAM a 1 bit represents a free
; sector and a 0 bit represents a used sector.
; The BAM is also split over the first two sectors of the directory track
; such that the first 1440 LSNs are in bytes 0..179 of sector 1
; and the second 1440 sectors are in bytes 0..179 of the second sector.
;

GetBit  PSHS    Y,U			; save regs
        LDA     #$01			; bit for 1st sector
        CMPU    #BAMLSNPerSec		; LSN in first table?
        BCS     LCE4B			; yes, skip ahead
	
        LEAY    1,Y			; point to next table
        LEAU    -BAMLSNPerSec,U		; decrement U to get offset in second table
        ASLA				; bit for second sector
	
LCE4B   PSHS    U			; save LSN required
        BITA    GFlag+8,S		; Already free?
        BNE     LCE5E			; yes.....
	
        STA     GFlag+8,S		; save it
        JSR     >DOSFindAndRead		; read LSN (Y), buffer entry ptr returned in X
        BNE     GetFreeExtentExit			; error exit
	
        STX     GFPointBuff+8,S		; store disk buffer entry pointer
        LDA     #BuffDirty		; mark buffer dirty
        STA     BuffFlag,X		

LCE5E   LDX     GFPointBuff+8,S		; get disk buffer entry pointer
        LDX     BuffAddr,X		; get address of data
        PULS    D			; recover required LSN
	
        LSRA				; D=D/8, get LSN bit and byte offset in BAM.
        RORB
        LSRA
        RORB
        LSRA
        RORB
        
	LDA     3,S			; get LSB of required LSN
        ANDA    #$07			; mask off bottom 3 bits
        LDU     #DPixMaskTable4Col-1	; convenient no->bitmask table....				
        NEGA				; negate A
        LDA     A,U			; get bit from table
        ABX				; lookup byte in BAM
        BITA    ,X			; extract it
        PULS    Y,U,PC			; restore and return

;
; Get a file length.
;
; Entry :
;	A 	= FCB no.
;
; Exit :
; 	U:A 	= length of file 
;	B	= Error code
;


DOSGetFLen   
	STA     <DosCurrCtrlBlk		; save FCB no
        BSR     DosFCBNoToAddr		; get address of FCB
        TST     FCBDirFlags,X		; get dir flags from FCB
        BPL     LCE84			; ok skip
	
        LDB     #ErrFF		; error : file not found
	
LCE83   RTS

LCE84   LDA     FCBFileLen+2,X		; number of extra bytes in last sector		
        LDU     FCBFileLen,X		; number of sectors
        LEAY    1,U			
        BNE     LCEAA

; Note U must still be $FFFF here so that FindFSNinU won't stop too soon.
        
	JSR     >FindFSNinU		; Go find File sector no in U
        BNE     LCE83
        
	LDB     DosCurFileNo		; get file number
        STB     FCBDirNoLast,X		; save in FCB
	
LCE99   LDA     DirEntLastBytes,U	; get bytes in last sector
        STA     FCBFileLen+2,X		; add to file length
        LDU     DosTotalSFound
        TSTA				; 0 bytes in last sector ?
        BEQ     LCEA7			; yep skip ahead
	
        LEAU    -1,U			; decrement total sectors found
LCEA7   STU     FCBFileLen,X		; set file len
LCEAA   CLRB				; no error
LCEAB   RTS

;
; Convert an FCB number to it's address.
;
; Entry :
;	A 	= FCB number 0..9
;
; Exit :
;	X 	= address of FCB
;	D,U,Y	= preserved
;

DosFCBNoToAddr   
	PSHS    D
        LDA     <DosCurrCtrlBlk		; get current FCB
        LDB     #DosFCBLength		; work out offset into table
        MUL
        TFR     D,X			; get offset into X
        LEAX    DosFCB0Addr,X		; work out address of FCB
        PULS    D,PC

;
; Close All open files and clean up buffers for one drive.
;
; This should be called four times when basic returns to the "OK" 
; prompt.
;
; Note: only closes files on the current disk (in DosDriveNo).
;
; Entry:
;
;	DosDriveNo	= drive no to close files on.
;
; Returns:
;	B		= Error code.
;

DOSCloseAll   
	LDA     #DosNumFCBs		; do all FCBs
        STA     <DosCurrCtrlBlk		; set current FCB no
	
LCEBF   BSR     DosFCBNoToAddr		; Get FCB address
        LDA     FCBDrive,X		; get drive from FCB 
        CMPA    <DosDriveNo		; same as current drive ?
        BNE     LCECB			; nope skip on
	
        BSR     DosCloseFile2		; close the file
        BNE     LCE83			; error : exit

LCECB   DEC     <DosCurrCtrlBlk		; do next block
        BPL     LCEBF			; if more loop again
LCECF   CLRB
        RTS

;
; Close a single file.
;
; Entry:
;	A 	= FCB no to close.
;
; Exit:
;	B	= Error code.
;
; If this was the last file open for that drive, it will call cleanup for all
; pages associated with that drive, and then invalidate them. The directory
; track for that drive is then backed up.
;

DOSCloseFile   
	STA     <DosCurrCtrlBlk		; set current FCB no

DosCloseFile2   
	BSR     DosFCBNoToAddr		; Get FCB address
        TST     ,X			; is first character of filename zero ?
        BEQ     LCECF			; yes : return
	
        LDA     <DosDriveNo		; save current drive on stack
        PSHS    A
        LDA     FCBDrive,X		; get drive from FCB 
        STA     <DosDriveNo		; make it current
        CLR     ,X			; zero out first char of filename 
	
        JSR     >DosGetDiskGeometry	; get disk geometry
        BNE     DOSCloseFileExit			; error : exit
	
        TST     DrvDetUseCnt,X		; Number of files in use on this drive
        BEQ     LCEF3			; branch if zero
	
        DEC     DrvDetUseCnt,X		; else decrement file in use count
        BEQ     LCEF3			; again test for zero
	
        CLRB				; flag no error
        BRA     DOSCloseFileExit	; and exit

; clean up the drive
LCEF3   LDB     DrvDetFreeLen,X		; get free length byte
        BEQ     LCF04			; branch if zero
	
        PSHS    X			; save buffer ptr
        LDX     DrvDetFreeEPtr,X	; get free extent pointer
        JSR     >DeleteExtent		; and delete it
	
        PULS    X			; recover buffer ptr
        BNE     DOSCloseFileExit	; branch if error
	
        CLR     DrvDetFreeLen,X		; free length byte
LCF04   JSR     >DOSSyncDir		; Syncronise directory tracks
        
; Invalidate drive table so user can swap disks now.	
	LDU     #DosD0Online-1		; point to drive online table
        LDA     <DosDriveNo		; mark FCB's drive as inactive
        CLR     A,U
	
DOSCloseFileExit   
	PULS    A			; recover acive drive at call
        STA     <DosDriveNo		; restore it
        TSTB				; set flags for error
LCF13   RTS

;
; Create a file.
;
; If a file exists with the supplied name, is is renamed with a ".BAK" extension.
; If there is already a ".BAK" file then it is first deleted.
; DOSRCreateFile is called for the real creation.
;
; Entry:	
;	A	= File number
;
; Exit:
;	B	= Error code
;

DOSCreateFile   
	STA     DosTempFCBNo		; save file number
        JSR     >DosFCBNoToAddr		; get file's address
	STX     DosSaveFCB		; save it's FCB address
	
        LDB     #8+3+1			; Length of name,extension,drive
        LDU     #DosCurDriveInfo	; point at current drive info
LCF22   LDA     ,X+			; get a byte from FCB
        STA     ,U+			; save in current drive info
        DECB				; decrement count
        BNE     LCF22			; loop if more to do
	
        LDD     -4,U			; get and save current extension
        STD     DosSaveExt
        LDA     -2,U
        STA     DosSaveExt+2
	
        LDD     #"BA"			; set current extension to "BAK"
        STD     -4,U
        LDA     #'K'
        STA     -2,U
        
	ifdef 	PGSFix
	TST     3,X
	else
	TST     FCBDirFlags,X		; test flags, this is incorrect as X no longer 
					; points at head of FCB!
	endc
	
        BMI     LCF69			; flags minus....brnach :)
	
        JSR     >DOSOpenFile		; Try opening 'FILENAME.BAK'
        CMPB    #ErrNE			; test if we got file doesn't exist error? 
	
        ifdef 	PGSFix
	BEQ     LCF51			; correct place to go
	else
	BEQ     LCF4F			; DDv1 goes here
        endc
	
	TSTB				; test for error
        BNE     LCF13			; if error return
	
        JSR     >DOSDeleteFile		; else delete the current BAK file
        BNE     LCF13			; if error return
        
	ifdef 	PGSFix
LCF4F	leax	12,x			; Move FCB pointer forward
LCF51	clr	-12,x			; clear first char of filename
	JSR	LDFCA
	BEQ	LCF69
	else
LCF4F   TST     [DosSaveFCB]		; test first character of filename
        BEQ     LCF69			; zero skip on
        LDA     DosTempFCBNo		; get original fileno
	endc
	
        JSR     >DOSRename		; rename original file to bak.
        BEQ     LCF61			; no error, branch on
	
        CMPB    #ErrFF			; is it ?? error !!!!
        BNE     LCF13			; no : exit

LCF61   LDA     DosTempFCBNo		; get temp fileno
	JSR     >DOSCloseFile		; close the file
        BNE     LCF13			; error, exit
	
LCF69   LDD     DosSaveExt		; restore original extension
        STD     DosCurExtension
        LDA     DosSaveExt+2
        STA     DosCurExtension+2
	
        JSR     >DOSOpenFile		; try opening file
        BEQ     DOSRawCreate		; no error, skip
	
        CMPB    #ErrNE			; is it file does not exist error?
        BNE     LCF13			; no, exit
	
;
; Raw create
;
; Entry:
;	A	= file no
;
; Exit:
;	B	= Error code
;
;	
	
DOSRawCreate   
	STA     <DosCurrCtrlBlk		; save fileno as current
        JSR     >DosFCBNoToAddr		; get it's FCB address
	
        TST     FCBDirFlags,X		; test FCB's flags
        BMI     LCF8A			; 
	 
        LDB     #ErrFE			; flag file exists.....
LCF89   RTS

LCF8A   CLRA				; clear, flag normal filename block wanted
        JSR     >FindEmptyDir		; find an empty filename block in directory 
        BNE     LCF89			; error, exit
	
        JSR     >DosFCBNoToAddr		; get address of FCB
        STA     FCBDiskFileNo,X		; save filenumber in FCB
        STA     FCBDirNoLast,X
	
;clear out FCB extents	
        LDB     #FCBSecExtent2		; point to end of second FCB extent
LCF9B   CLR     B,X			; clear a byte	
        DECB				; decrement offset
        CMPB    #FCBFilePointer		; reached FCB file pointer
        BCC     LCF9B			; nope, loop again
        
	LDB     #DirEntryLen-1		; clear bytes in DIR entry
LCFA4   CLR     B,U			; clear a byte

CmdRenameMC
        DECB				; decrement count	
        BPL     LCFA4			; keep going until all done
	
        LEAU    DirEntFilename,U	; point at dir entry filename
        LDB     #8+3			; length of filename + ext
LCFAD   LDA     ,X+			; get a byte from FCB
        STA     ,U+			; put in dir entry
        DECB				; decrement count
        BNE     LCFAD			; loop if not all done
	
        CLRB				; flag no error
LCFB5   RTS				; return

;
; Delete a file from disk.
;
; Entry:
;	A	= filenumber
;
; Exit:
;	B	= Error code
;

DOSDeleteFile   
	JSR     >DosCheckProtect	; check file protection
        BNE     LCF89			; error, return
	
        PSHS    X			; save FCB pointer
        LDB     FCBDiskFileNo,X		; get disk file  number
        JSR     >DOSGetDirEntry2	; Get dir entry 
	BNE     LD00C			; error, cleanup and exit
	
        TFR     X,U			; save dir entry pointer in U
        PULS    X			; restore FCB pointer
	
        LEAY    DirEntFnBlock1,U	; point Y to first allocation block	
        LDB     #$04
LCFCD   LDA     DirEntAttr,U		; get attributes from directory entry
        ANDA    #AttrContinued		; is this a continuation block?
        BEQ     LCFD6
	
        LDA     DirEntFlag,U		; get block no of continuation entries
LCFD6   PSHS    D			; save D
        LDB     #AttrAfterDel		; invalidate block
        STB     ,U
	
        PSHS    X			; save FCB pointer
	
LCFDE   LDX     ,Y++			; get allocation LSN
        LDB     ,Y+			; get allocation sector count
        BEQ     LCFEE			; branch if zero sectors, end of allocations
	
        PSHS    Y			; save allocation entry pointer
        BSR     DeleteExtent		; delete the extent
        PULS    Y			; restore allocation entry pointer
        LBNE    DosFReadErrorExit	; cleanup and exit if error
	
LCFEE   DEC     3,S			; decrement entry count
        BNE     LCFDE			; loop again if more entries in this dir block
	
        PULS    X			; restore FCB pointer
        LDB     ,S			; get more extents flag
        BEQ     LD00B			; exit if no more extents
	
        LEAS    2,S			; drop counter and flags
        PSHS    X			; save FCB pointer
        JSR     >DOSGetDirEntry2	; get next directory block	
        TFR     X,U			; put pointer to dir block in U
        PULS    X			; restore FCB pointer
        BNE     LCF89			; branch if errors in DOSGetDirEntry2
	
        LEAY    DirEntCntBlock1,U	; point to first allocation in continuation block
        LDB     #DirEntCntExts		; initilize number of allocations count
        BRA     LCFCD			; go delete them

LD00B   CLRB				; flag no error
LD00C   CLR     <DosIOInProgress	; flag no dos io in progress
        LEAS    2,S			; clen up stack
        TSTB				; set CC.C on error
        RTS

;
; Delete one extent.
;
; Called by DOSDeleteFile (amongst others). Marks one extent's worth of sectors
; free.
;
; Entry:
;	B	= Number of sectors.
;	X	= starting LSN.
;
; Exit:
;	B	= Error code.
;
;
; stack frame
;	4,s	0
;	2,s	LSN to delete
;	0,s	Sector count
;

DeleteExtent   
	CLRA				; clear A, makes D=sector count
        PSHS    A			; save A
        PSHS    D,X			; save sector count and LSN
	
        JSR     >DosGetDiskGeometry	; get the geometry of the disk
        BNE     DeleteExtentExit	; error, exit
	
        LDY     DrvDetDirLSN,X		; get LSN of directory	
        LDD     2,S			; get LSN of extent to delete
        SUBD    #BAMLSNPerSec		; subtract BAM entries in first BAM sector
        BCS     LD031			; branch if in first sector (-ve result)
        
	LEAY    1,Y			; move to next BAM sector
	
        STD     2,S			; save offset within BAM			
        ADDD    ,S			; add sector count
        SUBD    #BAMLSNPerSec		; subtract BAM entries in first BAM sector
        BCC     LD098			; error allocation overflows BAMs 
	
LD031   LDD     2,S			; get LSN of extent to delete
        ADDD    ,S			; add number of sectors to delete
        SUBD    #BAMLSNPerSec		; subtract BAM entries in first BAM sector
        BCS     LD041			; branch if in first BAM sector
	
        STB     4,S			; calc no of sectors in second BAM
        NEGB
        ADDB    1,S
        STB     1,S
	
LD041   JSR     >DOSFindAndRead		; read required BAM sector
        BNE     DeleteExtentExit	; error, exit
	
        LDA     #BuffDirty		; mark BAM sector buffer dirty
        STA     2,X
	
        LDD     2,S			; get LSN to delete from
        LSRA				; divide LSN by 8, to get the byte needed
        RORB
        ROR     ,S
        LSRA
        RORB
        ROR     ,S
        LSRA
        RORB
        ROR     ,S
	
; this leaves the byte at 0,s with the bottom 3 bits of D in the *TOP* 3 bits
; so in the loop below we need to decrement by $20 each loop, rather than 1.
; e.g.
; D=bbbb bbbb bbbb bbbb  0,S=0000 0000  before shift
; D=000b bbbb bbbb bbbb  0,S=bbb0 0000  after shift
;
	
        LDX     BuffAddr,X		; get address of BAM buffer
        ABX				; add calculated offset
        
	LDB     #$01			; get bitmap 
        LDA     ,S			; get bit number
        BEQ     LD066			; zero, exit loop
	
LD061   ASLB				; shift bit number left
        SUBA    #$20			; we take $20, because of shift above	
        BNE     LD061			; loop if not zero

LD066   STB     ,S			; 0,S now contains starting bit
        LDB     1,S			; get sector count
	
LD06A   LDA     ,S			; get bitmask 
        ORA     ,X			; or it with BAM, deallocate sector
        STA     ,X			; put back in BAM
        DECB				; decrement sector count
        BEQ     LD08B			; branch if no more sectors
	
        ASL     ,S			; shift mask left
        BCC     LD06A			; skip if mask not shifted into carry
	
        LDA     #$01			; re-initialize mask
        STA     ,S			
        LEAX    1,X			; move to next BAM byte
	
; check to see if we have more than 16 LSNs left to delete, if so do them quickly
; by marking groups of 16 sectors free.....
	
LD07D   CMPB    #$10			; more than 16 sectors left to flag?	
        BCS     LD06A			; no, continue doing a sector at a time
	
        LDA     #$FF			; mark a group of 8 sectors at once
        STA     ,X+			; mark next 8
        STA     ,X+			; and next 8
        SUBB    #$10			; decrement sector count by 16
        BNE     LD07D			; loop again
	
LD08B   LDX     #BAMLSNPerSec		; get BAM entries / sec
        LEAS    4,S			; clean up stack	
        LDB     ,S+			; get count
        BEQ	LD09C			; it's zero, exit

GoDeleteExtent   
	LBRA    DeleteExtent

LD098   LDB     #ErrIV			; Error invalid volume
DeleteExtentExit   
	LEAS    5,S			; clean up stack
LD09C   RTS			

;
; Set protection status.
;
; Entry:
;	A	= file number
;	B	= 1 protect, 0 unprotect
;
; Exit:
;	B	= error code
; 

DOSProtectMC
LD09D   STA     <DosCurrCtrlBlk		; set current file number
        JSR     >DosFCBNoToAddr		; get it's FCB address
        
	LDA     FCBDirFlags,X		; get flags from dir entry
        BMI     LD0CC			; error.....
	
        TSTB				; shoud we protect / unprotect?
        BEQ     LD0AC			; branch if unprotect
        
	ORA     #AttrWriteProt		; set protection bit

	FCB	Skip2			; skip 2 bytes
LD0AC   ANDA	#~AttrWriteProt		; reset protection bit
        STA     FCBDirFlags,X		; put back in dir entry
	
        LDB     FCBDiskFileNo,X		; get file number on disk
        JSR     >DOSGetDirEntry2	; go get it's dir enetry
        BNE     LD09C			; error, return
	
        STA     DirEntAttr,X		; put it in dir attributes.
        CLRB				; flag no error
LD0BB   RTS

DosCheckProtect   
	STA     <DosCurrCtrlBlk		; set current file number
        JSR     >DosFCBNoToAddr		; get it's FCB address
        
	LDA     FCBDirFlags,X		; get flags from dir entry
        BMI     LD0CC
	
        BITA    #AttrWriteProt		; check write protect attribute
        BEQ     LD0FB			; not protected....
        LDB     #ErrPT			; protection error
        RTS

LD0CC   LDB     #ErrFF			; return file not found
LD0CE   RTS
	
	;
; Rename a file.
;
; Entry:
;	DosCurFilename	= newname
;	A		= fileid
;
; Exit:
; 	B		= Error code
;

FNLength	EQU	8+3		; filename length

DOSRename
	JSR     >DosCheckProtect	; is the file protected?
        BNE     LD0FB			; yep, (or error) exit
	
; copy name into FCB	
        LDB     #FNLength		; byte count to copy			
        LDU     #DosCurFilename		; point at new filename
LD0D9   LDA     ,U+			; get a byte from new name
        STA     ,X+			; copy into FCB
        DECB				; dec count
        BNE     LD0D9			; loop if more to copy
	
        LEAX    -FNLength,X		; point back at beginning of FCB
        LDB     FCBDiskFileNo,X		; get file number on disk
        JSR     >DOSGetDirEntry2	; go get it's entry
        BNE     LD0FB			; error, exit
	
        LDU     #DosCurFilename		; point at new name
        LDB     #FNLength		; byte count to copy
        LEAX    1,X			; move past attribute byte
LD0F1   LDA     ,U+			; get a byte from new name
        STA     ,X+			; save in dir entry
        DECB				; decrement count
        BNE     LD0F1			; loop if more
	
        CLR     <DosIOInProgress	; clear IO in progress
        CLRB				; flag no error
LD0FB	RTS

;
; Find empty directory slot.
;
; Entry:
;	A	= 0, normal
;		= 1, continuation blocks, can't start in slot 0.
;
; Return:
;	A	= New dir entry number found
;	X	= Page table for buffer
;	U	= Entry within buffer
;	B	= Error code.
;
; Note that the buffer is always marked dirty.
;

FindEmptyDir   
	NEGA				; convert 0/1 to $00/$FF
        STA     DosTemp2		; save in temp
        JSR     >DosGetDiskGeometry	; get geometry of the disk
        BNE     LD0FB			; error return
	
        LDX     BuffLSN,X		; Get LSN from details
        PSHS    X			; save on stack
	
        LEAX    2,X			; add 2 to LSN
LD10B   STX     DosCurLSN		; make current
        TFR     X,Y			; save it in Y
        JSR     >DOSFindAndRead		; find LSN and read into buffer
        BNE     FindEmptyDirExit	; error, exit
	
        LDU     BuffAddr,X		; get buffer address
        LDB     #DirBlksPerSec		; 10 dir blocks per sector
        TST     DosTemp2		; are we looking for filename, or continuation?	
        BPL     LD123			; branch if filename
	
        NEG     DosTemp2		; turn $FF->1
        BRA     LD12A

; if we get here DosTemp = 0, so look at first directory block,
; however the above neg will turn $FF back to $1, so searches for 
; continuation blocks always beging at entry 1.
LD123   LDA     DirEntAttr,U		; get attributes from entry
        BMI     LD142			; entry deleted, can be re-used
	
        INC     DosTemp2		; increment entry counter
LD12A   LEAU    DirEntryLen,U		; move to next entry in sector
        DECB				; decrement counter
        BNE     LD123			; loop again if more to search
	
        LDX     DosCurLSN		; move to the next sector in the directory
        LEAX    1,X
        TFR     X,D			; transfer LSN to D
        SUBD    ,S			; subtract start LSN
        CMPB    #SectorsPerTrack	; got to end of dir track?
        BCS     LD10B			; no search next sector
	
        LEAS    2,S			; drop saved LSN
        LDB     #ErrFD			; return full directory error
        RTS

LD142   LDA     #BuffDirty			
        STA     BuffFlag,X
        LDA     DosTemp2		; get dir entry number
        CLRB				; flag no error
FindEmptyDirExit   
	LEAS    2,S			; clean up stack
        RTS				; return

;
; Get free space on a disk.
;
; Entry:
;	DosDriveNo	= drive to get info for
;
; Exit:	
;	X		= Free sectors
;	B		= Error code
;

DOSGetFree   
	BSR     DosGetDiskGeometry	; get the geometry for the disk
        BNE     LD161			; error, exit
	
        LDY     ,X			; get LSN in buffer...LSN of dir track			
        LDX     <DBZero			; X=0, initialize free block count
        BSR     LD162			; get free sectors in first BAM sector
        BNE     LD161			; error, exit
	
        LEAY    1,Y			; move to next BAM sector
        BSR     LD162			; get free sectors in second BAM sector
        BNE     LD161			; error, exit
        CLRB				; flag no errors
LD161   RTS

; count bits in 1 BAM sector
LD162   PSHS    X			; save X
        JSR     >DOSFindAndRead		; read LSN in Y
        BNE     FindEmptyDirExit	; error, exit
	
        LDU     BuffAddr,X		; get pointer to loaded sector
        PULS    X			; restore X
	
        LDB     #BAMEntriesSec		; count of bytes to process
LD16F   LDA     ,U+			; get a byte from buffer
LD171   LSRA				; shift bit 0 into carry
        BCC     LD176			; branch if bit clear, sector in use
	
        LEAX    1,X			; otherwise increment sector count
LD176   TSTA				; all bits processed?
        BNE     LD171			; nope, shift and test again
	
        DECB				; decrement byte count
        BNE     LD16F			; loop again if not all done
        RTS
	
;
; Get geometry for a disk and set the apropreate low memory vars.
; Read from disk if in memory copy not valid.
;
; Entry: 
;	DosDriveNo	= drive to get info for
;
; Exit: 
;	Drive vars setup in low ram, to be same as disk in drive.
;  	X		= Address of buffer detail entry for buffer to use
;

DosGetDiskGeometry
	LDX     #Drv0Details		; Point at drive details
        LDU     #DosD0Online-1		; Point at drive online table
        LDB     #DrvDeatailLen		; Get drive table entry len
        LDA     <DosDriveNo		; Get last used drive

        LEAU    A,U			; Point U at drive online flag
        DECA				; Make zero based
        MUL				; Calculate offset of drive we need
        LEAX    D,X
        TST     ,U			; Is drive online ?

        BNE     LD1CF			; Yes : exit
	
        LDY     #SectorsPerTrack*DirPrimary 	; First sector of DIR track ($0168)
        LDA     #SectorsPerTrack	; Set sectors per track for this drive
        STA     DosSecTrkTblOfs,U	; Set it

        PSHS    X			; Save drive detail pointer
        JSR     >DOSFindAndRead		; Find free buffer and read sector
        LBNE    LCB56			; Error : exit

; At this point X points to buffer details ???
	
        LDX     BuffAddr,X		; Get address of buffer data
        LDD     DirTracks1s,X		; Get complements of tracks/secs per track
        COMA				; Complemetn them for compare
        COMB
        CMPD    DirTracks,X		; compare them to validate the disk
        PULS    X			; restore drive detail pointer
        BNE     LD1D1			; Not the same, not valid disk.
	
        STB     DosSecTrkTblOfs,U	; Set Sectors/Track for this disk
        STA     DosTracksTblOfs,U	; Set tracks for this disk
        DEC     ,U			; Mark drive online
        CMPB    #SectorsPerTrack	; Disk single sided ?
        BEQ     LD1C0			; yes : skip on

        CLRB				; zero it
LD1C0   PSHS    B			; save it
        CLR     BuffFlag,X		; Clear buffer flag
        LDD     #SectorsPerTrack*DirPrimary 	; First sector of DIR track ($0168)
        TST     ,S+			; Do we need to double ? 
        BNE     LD1CD			; no : skip
	
        ASLB				; Multiply D by 2, as we have 2 sides
        ROLA
LD1CD   STD     ,X			; save it
LD1CF   CLRB				; No error
        RTS				; Return

LD1D1   LDB     #ErrIV		; Flag error, invalid Volume
LD1D3   RTS

;
; Get directory entry.
;
; Entry: 
;	DosDriveNo	= drive to process
;	B		= File number(on disk) to get entry for
;
; Exit: 
;	X		= Pointer to required Dir entry.
;	DosCurLSN	= LSN of directory entry.
;	DosCurDirBuff	= buffer pointer to LSN buffer
;	B		= error code
;

DOSGetDirEntry   
	LDA     #$FF			; Init sector counter
LD1D6   INCA				; increment sector counter
        SUBB    #DirEntPerSec		; Decrement file no, by a sectors worth of files
        BGE     LD1D6			; Done all ? no : continue looping
	
        ADDB    #DirEntPerSec		; Compensate for over loop
	
; At this point A contains the sector number within the directory that we are intereted in.
; and B contains the entry within that sector of the file's details.

        PSHS    D			; Save them
        BSR     DosGetDiskGeometry	; Setup disk geometry from disk in drive
        LBNE    LCB56			; Error : exit
	
        LDD     ,X			; Get LSN number from buffer
        ADDD    #$0002			; Advance past bitmap sectors
        ADDB    ,S+			; Add sector offset calculated above
        ADCA    #$00			; Deal with carry
        STD     DosCurLSN		; Save LSN
        TFR     D,Y			; Get LSN into Y
        JSR     DOSFindAndRead		; Find free buffer and read sector
;***    BSR     DOSFindAndRead		; Find free buffer and read sector
        PULS    A			; Retrieve entry number witin sector

        BNE     LD28A			; Error: exit
        TFR     X,U
        LDB     #DirEntryLen		; Length of dir entry
        MUL				; Calculate offset
        STX     DosCurDirBuff		; Saave block def pointer
        LDX     BuffAddr,X		; Get pointer to block data
        LEAX    D,X			; Get offset of DIR entry into X
        CLRB				; Flag no error
LD28A   RTS

;
; Get directory entry
;
; Entry:
;	B	= directory entry number
;
; Exit:
;	X	= directory entry pointer
;	Y	= buffer table pointer
;	D	= preserved
;

DOSGetDirEntry2
	PSHS    D
        BSR     DOSGetDirEntry		; Get directory entry we are interested in
        LBNE    LCB56			; Error :
        LDY     DosCurDirBuff		; Get Buffer def block for this entry
        LDA     #BuffDirtyExpire	; Set flag
        STA     BuffFlag,Y
        CLRB				; Flag no error
        PULS    D,PC			; Restore and return

;
; Find a free disk buffer.
; Find buffer with same sector
;
; Entry: 
;	Y		= LSN to find
;	DosDriveNo	= drive to process
;
; Exits:
;	X		= buffer table
;	U		= pointer to detail entry for free buffer (or 0).
;	B		= 0 if found $FF if not found
;

FindFreeBuffer   
        LDX     #Buff1Details		; Point at disk buffer detail table
        LDU     <DBZero			; U=0 
        LDB     #BuffCount		; 4 Disk buffers

LD222   LDA     BuffFlag,X		; Get buffer flag in A
        BEQ     LD23D			; Zero ?
	
        CMPA    #BuffInUse		; Is buffer in use ?
        BEQ     LD23F			; no, skip
	
        CMPY    BuffLSN,X		; is LSN the same?
        BNE     LD239			; no, skip
	
        LDA     <DosDriveNo		; Get last drive
        CMPA    BuffDrive,X		; Is this buffer using the same drive ?
        BNE     LD239			; nope, skip on
        BSR     MakeBuffYoungest	; Make this the youngest buffer
        CLRB				; Flag no error
        RTS

LD239   TST     BuffFlag,X		; Is buffer free ?
        BNE     LD23F			; nope, look at next
	
LD23D   TFR     X,U			; Select this buffer
LD23F   LEAX    BuffDetailSize,X	; move on to next buffer detail entry
        DECB				; Decrement counter
        BNE     LD222			; Any more to check ? : yes loop again

        LDB     #$FF			; Flag error
LD246   RTS
	
;
; Find a free buffer and read sector.
;
; Entry: 
;	Y 		= LSN to read
;	DosDriveNo	= drive to process
; Exit:
;	X 		= pointer to buffer entry 
;	U 		= preserved
;	B 		= error code 
;
; A buffer is allocated from the 4 buffer pool in the following order of
; preference:
;	1, Sector already in buffer
;	2, An empty buffer
;	3, Least recently used buffer, flushed first.
;

DOSFindAndRead   
	PSHS    U
        JSR     >FindFreeBuffer		; Find free buffer, pointer to details returned in U
        LBEQ    LCB56
	
        LEAX    ,U			; Make X point to details
        PULS    U
        BNE     LD25A
	
        BSR     FindFreeDiskBuffer	; Find buffer to read data into
        BNE     LD2F5 
	
LD25A   CLR     BuffFlag,X		; Make buffer free
        STY     ,X
        LDA     <DosDriveNo		; Get last drive
        STA     BuffDrive,X		; Set this drive's buffer
        PSHS    X			; Save buffer detail pointer
        LDX     BuffAddr,X		; Get address of buffer
        JSR     >DOSReadAbsSector	; Read the sector
        PULS    X			; Restore buff detail pointer
        BNE     LD2F5			; Error : exit
	
        LDA     #$01			; Set flag to 1
        STA     BuffFlag,X
        CLRB				; No error
LD2F5   RTS

;
; Find least recently used disk buffer, if none, and there is 
; a dirty buffer, then flush it and use that one.
;
; Exit : X=pointer to buffer info block.
;

FindFreeDiskBuffer   
	PSHS    D,Y,U
LD276   LDX     #Buff1Details		; Point to disk buffer table
        LDB     #$04			; Check 4 buffers
LD27B   LDA     BuffAge,X		; Get buffer age
        CMPA    #$01			; Oldest ?
        BEQ     LD286			; Yes go process it
	
        LEAX    7,X			; Do next bufffer
        DECB				; Decrement buffer count
        BNE     LD27B			; More : do next

LD286   BSR     MakeBuffYoungest	; Adjust ages of all other buffers
        LDA     BuffFlag,X		; Get buffer flag byte 
        CMPA    #$55			; In use ???
        BEQ     LD276			; yes, select another buffer
	
        INCA				; Check for Flag=$FF
        BNE     LD295			; no : skip on
        
	DEC     BuffFlag,X		; yes, select another buffer
	BRA     LD276

LD295   BSR     TestAndFlushBuffer	; Check for buffer flush needed ?
        
	ifdef	PGSFix
	BEQ	LD29C			; no error, skip
	STB	1,s			; save error code on stack for return
	else
	LBNE    DosFReadErrorExit	; No error: skip
	endc
	
        PULS    D,Y,U,PC		; restore and return

;
; Update the LRU counts of buffers.
;
; Entry:
;	X	= page table entry
;
; Exits:
;	Entry becomes most recently used.
;

MakeBuffYoungest   
	LDB     #BuffCount		; Process 4 buffers
LD29C	
        LDA     BuffAge,X		; Get current buffer Age
        LDU     #Buff1Details		; Point to disk buffer table
LD2A4   CMPA    BuffAge,U		; Compare to current buffer age
        BHI     LD2AA			; higher ? skip
        DEC     BuffAge,U		; Decrement Age byte (make older)
	
LD2AA  	LEAU    BuffDetailSize,U	; Do next buffer
        DECB				; Decrement count
        BNE     LD2A4			; More : do next
        LDA     #$04			; Mark this as youngest buffer
        STA     BuffAge,X
        RTS

;
; Clean up a buffer.
;
; If buffer is dirty, flush it to disk, and if verify is on, or 
; write is to directory tracks, then verify it.
;
; Maintain a list of directory sectors which need updating.
;
; Entry:
;	X	= buffer table
;
; Exit:
;	X	= preserved
;	B	= error code
;

TestAndFlushBuffer   
	TST     BuffFlag,X		; Buffer dirty ?
        BMI     FlushBuffer		; Yes, flush it !
        CLRB				; No error ?
        RTS

FlushBuffer
	LDA     <DosDriveNo		; Get last drive accessed
        PSHS    A			; save it on stack
        PSHS    X			; Save buffer pointer
        LDA     #$FF			; Flag Dos IO in progress
        STA     <DosIOInProgress
        CLR     BuffFlag,X		; Flag buffer no longer dirty
        LDA     BuffDrive,X		; Get drive this buffer refers to
        STA     <DosDriveNo		; Save in last accessed drive
        LDY     ,X			; get LSN ?
        LDX     BuffAddr,X		; Get buffer pointer
        JSR     >DOSWriteAbsSector	; Write it

        PULS    X			; Retrieve buffer pointer
        BNE     LD2F2			; no error : skip ahead

        LDA     <DskTrackNo		; Get current track no
        CMPA    #DirPrimary		; track 20 (directory) ?
        BNE     LD2ED			; no : skip ahead

;
; I do not have a clue why this code does this, it seems to take a byte from
; the basic rom do some stuff to it and update the Directory sector status table 
; with it !
; 
; Looking at $A673, the 8 bytes before it are $80,$40,$20,$10,$08,$04,$02,$01
; This is the 2 colour pixel mask table, but is a convenient table for mapping a bit
; number to the bit it represents.
;
	
	LDU     #PixMaskTable4Col	; This for some strange reason points U at basic rom !!!	
        LDA     <DosDriveNo		; get last drive
        NEGA
        LDA     A,U

        LDU     #DosDirSecStatus-1	; Point to directory status table
        LDB     <DskSectorNo		; get sector number
        ORA     B,U			; Put a byte in table
        STA     B,U

LD2ED   LDA     #$01			; Mark bufer as youngest
        STA     BuffFlag,X
        CLRB
	
LD2F2   PULS    A
        STA     <DosDriveNo		; Restore last drive
        CLR     <DosIOInProgress	; Mark no io in progress
        TSTB
        RTS
	
;
; Write absolute sector.
;
; Entry	:    
;	X		= Address to store data
;	Y		= LSN to read
;	DosDriveNo	= drive to process
;
; Exit:
;	X 		= preserved
;	B		= error code
;

DOSWriteAbsSector   
	BSR     CalcTrackFromLSN	; Setup disk vars in low ram with trackno
        JSR     >DosDoWriteSecN		; go write the sector
LD2FF   LDX     <DiskBuffPtr		; Restore buffer pointer 
        TSTB				; Test for Error
        RTS				; return to caller
;
; Verify absolute sector.
;
; Entry and Exit as for Write absolute sector.
;

DosVerifyAbsSector  	
	BSR     CalcTrackFromLSN	; Setup disk vars in low ram with trackno
	JSR     >DosDoReadSec2
	BRA     LD2FF
;
; Read absolute sector.
;
; Entry and Exit as for Write absolute sector.
;

DOSReadAbsSector   
	BSR     CalcTrackFromLSN	; Setup disk vars in low ram with trackno
        JSR     >DosDoReadSec		; Go read data
        BRA     LD2FF			; Return to caller

;
; Calculate track from Logical sector number.
; 
; Entry: 
;	X	= Buffer pointer
;	Y	= LSN to read/write
;
; Exit: 
;	A	= Track
;	B	= Sector	
;	Low ram vars DskTrackNo and DskSectorNo also set.
;

CalcTrackFromLSN   
	STX     <DiskBuffPtr		; Save in buffer pointer
        LDX     #DosD0SecTrack-1	; Point to Sec/Track table
        LDB     <DosDriveNo		; Get last drive
        LDB     B,X			; Get Sec/Track for that drive
        CLRA
        PSHS    D			; Save it
        CLR     ,-S			; Make room on stack
        TFR     Y,D			; Get LSN into D
	
; Calculate which track we need
	
LD321   INC     ,S			; Inc track counter
        SUBD    1,S			; Decrement sec/track from LSN
        BPL     LD321			; keep looping till it goes -ve
	
        ADDB    2,S			; Compensate for over-loop
        LDA     ,S			; Get track needed
        DECA				; Compensate track for over loop
        INCB
        LEAS    3,S			; Drop stack temps
        STD     <DskTrackNo		; Save track no
        RTS

;
; Copy command dispatch routine
;
; Syntax :
;	COPY filespec TO filespec	
;
; Stack setup as follows 
;
; offset	size 	purpose
; 0		1	Source FCB number
; 1		1	Destination FCB number
; 2		2	Buffer pointer ?
; 4		3	File pointer pos
;

CopySrcFCBNo	equ	0		; Source file FCB no
CopyDstFCBNo	equ	1		; Destination FCB no
CopyIOBufPtr	equ	2		; IO buffer pointer
CopySrcFP	equ	4		; Source filepointer

CmdCopy 
	LBSR    DOSCloseAll		; Close all files & devices
        BNE     LD36B			; Error : exit
        LEAS    -7,S			; Make room on stack
	
;
; Make a buffer for copying the file.
;

        TFR     S,D			; move to D
        SUBD    #$0100			; Make room for 1 sector
        SUBD    <BasVarEnd		; Will we overwrite basic ?		
        LBMI    BasOMError		; yes : error, exit
	
        CLRB
        TSTA				; At least 1 page of ram available
        LBEQ    BasOMError		; no : error, exit
        
	STD     CopyIOBufPtr,S		; IO buffer pointer
        LBSR    DosGetFilenameAndOpen
        BNE     LD36B			; Error : exit
        STA     CopySrcFCBNo,S		; save FCB no of source
        
	LBSR    DOSGetFLen		; Get file length
        BNE     LD36B
        JSR     <BasChrGetCurr		; scan current char from params
        
	CMPA    #DTokTO			; "TO" token (Dragon)
	LBNE    BasSNError		; no : Error
	
        JSR     <BasChrGet		; Get next character
        LBSR    DosGetFilenameAndOpen	; Get dest filename FCB number in A		
        BEQ     LD36E 			; No error : continue
	
	CMPB    #ErrNE		; File not exist ?
        BNE     LD36B			; does not exit, create it

LD36E   STA     CopyDstFCBNo,S		; Save destination FCB number
        LBSR    DOSCreateFile		; re-write destination file
        BEQ     LD375			; no error continue

LD36B   JMP     >DosHookSysError	; Error : exit
	
LD375   LDA     CopySrcFCBNo,S		; Get source FCB no
        STA     <DosCurrCtrlBlk		; Save current FCB
        LBSR    DosFCBNoToAddr		; Get FCB address
	
; Compare file pointer position with file length for source file

        LDD     FCBFilePointer,X	; MSW
        CMPD    FCBFileLen,X
        BCS     LD38B			; not there yet, continue copying

;***    LDD     FCBFilePointer+2,X	; LSB
;***    CMPD    FCBFileLen+2,X
        LDA     FCBFilePointer+2,X	; LSB
        CMPA    FCBFileLen+2,X
        BEQ     LD3DB			; reached EOF, copy done

LD38B   LDU     FCBFilePointer,X	; get source fileptr in U:A
        LDA     FCBFilePointer+2,X
	
        STA     CopySrcFP+2,S		; save source filepointer
        STU     CopySrcFP,S
	
        LDD     CopyIOBufPtr,S		; point to IO buff
        ADDD    CopySrcFP+1,S		; 
        STD     CopySrcFP+1,S
        BCC     LD39D
	
        INC     CopySrcFP,S
LD39D   LDA     CopySrcFP,S
        SUBA    FCBFileLen,X
        BCS     LD3B2
	
        LDD     CopySrcFP+1,S
        SUBD    FCBFileLen+1,X
        BLS     LD3B2
	
        LDD     FCBFileLen+1,X
        SUBD    FCBFilePointer+1,X
        STD     CopyIOBufPtr,S
	
LD3B2   LDA     CopySrcFCBNo,S		; retrieve source FCB no
        LDU     FCBFilePointer,X
        LDB     FCBFilePointer+2,X
        LDY     CopyIOBufPtr,S
        LDX     BasVarEnd
        LBSR    DOSFRead		; go read source file
        BNE     LD36B			; error : exit
	
        LDA     CopyDstFCBNo,S		; get destination FCB
        STA     <DosCurrCtrlBlk		; make current
        LBSR    DosFCBNoToAddr		; get FCB address
	
        LDY     FCBFileLen,X
        LDB     FCBFileLen+2,X
        LDU     CopyIOBufPtr,S
        LDX     BasVarEnd
        LBSR    DOSFWrite		; go write to destination
	
	BNE     LD36B			; error exit
        BRA     LD375

LD3DB   LBSR    DOSCloseAll		; close all files
        BNE     LD36B			; error exit
        LEAS    7,S			; cleanup stack
        RTS
;
; basic MERGE command
;
; Merges basic program in memory with a program from disk.
;
; MERGE "filename"
;

CmdMerge   
	LBSR    DosValidateAndOpenBAS	; open basic program file
        BNE     LD36B 			; error, exit
	
        BSR     ReadHeadGetTypeA	; read header, get filetype 		
        BNE     LD36B			; error, exit
	
        CMPA    #FTypeBas		; is the file basic?				
        BNE     GenFMError		; nope, FM error, exit
	
        LDU     <BasVarSimpleAddr	; point at end of basic program
        LDY     <BasStartProg		; point at beginning of basic program
        
	PSHS    Y,U			; save regs
        LEAU    1,U			; point 1 byte into vars 
        STU     <BasStartProg		; make this the new start of basic
        LBSR    DoBasLoad		; glo load program
	
        PULS    X,U			; recover old start and end prog addressses
        STU     <BasVarSimpleAddr	; and reset them
        STX     <BasStartProg
        LEAU    1,U			; point to loaded basic program
	
LD409   LDD     ,U++			; get pointer to next line
        BEQ     LD42B			; exit if zero, we have reached the end of loaded program
        
	LDD     ,U++			; get line number 
        STD     BasLinInpHead		; save it in line input buffer
        STD     <BasTempLine		; save it in tem line
        CLRB				; clear B
        LDX     #BasLinInpBuff		; point at line input buffer
LD418   INCB				; increment line char count
        LDA     ,U+			; get a loaded byte
        STA     ,X+			; put it in line input buffer
        BNE     LD418			; loop for next, if byte not zero (EOL)

        ADDB    #$04			;
        STB     <BasGenCount

        PSHS    U			; save loaded program pointer
        BSR     LD436			; put line in basic
        PULS    U			; recover loaded program pointer
        BRA     LD409			; loop again

LD42B   CLR     DosRunLoadFlag		; clear load run flag
        LBRA    LD4BD			; finish off load

GenFMError   
	JMP     BasFMError		; Jump to Basic's FM error handler

LD436   JSR     >BasFindLineNo		; find the line 
        BCS     LD44D			; branch if new line number
	
; line found with the same number, delete the current line and inser the new one.
        LDD     <BasFoundLineNo		; get address of found line 
        SUBD    ,X			; calculate length of founc line (-ve)
        ADDD    <BasVarSimpleAddr	; add base of loaded prog to line length 
        STD     <BasVarSimpleAddr	; set end address for copy
        LDU     ,X			; get address of next line
LD445   LDA     ,U+			; get a byte from next line
        STA     ,X+			; overwrite this line
        CMPX    <BasVarSimpleAddr	; done all?	
        BNE     LD445			; no,keep going

LD44D   LDD     <BasVarSimpleAddr	; get new base of variables
        STD     <Eval43			; save it
        ADDB    <BasGenCount		; add gencount to number of bytes moved
        ADCA    #$00			; process any carry B->A
        STD     <Eval41			; save it
        JSR     >BasChkArrSpaceMv	; check array space and move
	
        LDU     #$02D8			; point to input buffer
LD45D   LDA     ,U+			; get a byte from buffer	
        STA     ,X+			; put it in program
        CMPX    <Eval45			; end of line?
        BNE     LD45D			; no keep going
        LDX     <Eval41			; update end of basic program
        STX     <BasVarSimpleAddr
        JMP     >BasVect2		; re-link lines.	

;
; Read a header from the open file, into the DosCurDriveInfo
; Check that the header contacins the correct marker bytes at beginning and end.
; If valid return file type byte in A else generate and FM error.
;
; Entry:
;	DosCurrCtrlBlk	= file no to read
;
; Exit:
;	A		= filetype byte from header
;	X		= points to header block
;	B		= Error code (despit comments in DDv2c source).
;

ReadHeadGetTypeA   
	LDX     #DosCurDriveInfo	; point at current drive info
        LDY     #FileHeadLen		; 9 bytes, header size
        LDU     <DBZero			; U=0 filebointer at BOF
        CLRB				; B=0
        LDA     <DosCurrCtrlBlk		; get current control block
        LBSR    DOSFRead		; read file header	
        BNE	LD50C			; error, exit
	
LD47E   LDA     #MarkerHeadStart	; check for header marker bytes
        LDX     #DosCurDriveInfo	; point at file header in buffer
        CMPA    ,X			; first marker present?
        BNE     GenFMError		; generate FM error if not
	
        COMA				; check for second marker at end of header
        CMPA    HdrIDAA,X		; second marker present?
        BNE     GenFMError		; generate FM error if not
        LDA     HdrType,X		; get filetype in A		
        CLRB

LD50C   RTS

;
; Load a basic or binary file into memory.
;
; If basic, any basic program already in memory will be overwritten 
; and variables cleared.
;
; If binary, the program will be read into any location that is not ROM.
;
; RUN "filename"	: load and run 
; LOAD "filename"	: load but don't run 
;

; Clear various flags
DosHookRun   
	CLR     DosErrGotoFlag		; Clear on error goto flag
        CLR     DosErrLast		; clear last error code
        CLR     DosErrLineNo		; clear last error line
        CLR     DosErrLineNo+1
        CMPA    #$22			; quote
        BEQ     LD4A2
        TSTA
        RTS

LD4A2   LDB     #$01			; flag run

        FCB	Skip1
	
CmdLoad	CLRB				; flag load
        STB     DosRunLoadFlag		
        LBSR    DosValidateAndOpenBAS	; check and open the basic program file
        BNE     LD52F			; error, exit
	
        BSR     ReadHeadGetTypeA	; read header of basic program
        BEQ     LD532			; no error, continue

LD52F	JMP     DosHookSysError		; call error handler

LD532	CMPA    #FTypeBas		; is the file of type basic?
        BNE     LD504			; nope, go check for machine code
	
        BSR     DoBasLoad		; load the file into the program memory

LD4BD   CLR     <DosIOInProgress	; mark IO not in progress
        
	LDX     <BasStartProg		; point at start of program
        JSR     >BasSetProgPtrX		; asjust pointer
	
        LDX     <AddrFWareRamTop	; get the end of RAM
        STX     <BasVarStrTop		; setup string variables
        
	LDX     <BasVarSimpleAddr	; arrays top = simple vars top, 
        STX     <BasVarArrayAddr	; no arrays defined
	
	STX     BasVarEnd		; and no simple vars defined
LD549	
LD4CE   JSR     >CmdRestore		; perform a basic 'RESTORE' reset data pointers
        JSR     >BasResetStack		; reset basic stack
	
        TST     DosRunLoadFlag		; should we run the loaded code?
        BEQ     LD4DC			; nope, just return
	
        JMP     >BasRun			; go run it

LD4DC   CLRA				; flag no error
        JMP     >BasCmdMode		; return to command mode

;
; Load code of basic program
; On entry X points to file header
; 

DoBasLoad   
	LDD     HdrLen,X		; get file length
        TFR     D,Y			; get it into Y for FRead below
        ADDD    <BasStartProg		; add the start of basic, to get the new end
	
        STD     BasVarEnd		; save it 
        LDB     #$40			; Check we have 128 bytes free RAM
        JSR     >BasChkB2Free		; go check, OM error if insufficient

LD568	
LD4ED   LDA     <DosCurrCtrlBlk		; get current control block
        LDB     #FileHeadLen		; start reading at byte 9
        LDX     <BasStartProg		; into BasStartProg
        LDU     <DBZero	; U=0
        LBSR    DOSFRead		; go read it
	
        BNE    	LD52F			; error, try catching it
	
        JSR     >BasVect2		; tidy up after load
        LEAX    2,X			; point past end of program
        STX     <BasVarSimpleAddr	; save in basic vars
LD503   RTS				; return

LD504   CMPA    #FTypeBin		; is this file a machine code program?
        LBNE    GenFMError		; nope, errror not basic or machine code
	
        LDU     HdrExec,X		; get Exec address from header
        STU     <BasExecAddr		; save it in basic exec vector
	
        JSR     <BasChrGetCurr		; get current character from basic
        BEQ     LD524			; no more, skip on
	
        PSHS    X			; save X
        BSR     VarGetComma16		; get load offset in X
        TFR     X,U			; save offset in U
	
        PULS    X			; restore header pointer
        LDD     HdrExec,X		; get exec address
        SUBD    HdrLoad,X		; subtract load address giving exec address offset
        STU     HdrLoad,X		; save new load address
        ADDD    HdrLoad,X		; add exec offset to new load address
        STD     <BasExecAddr		; save it in basic exec address
	
LD524   LDY     HdrLen,X		; get file length into Y
        LDA     <DosCurrCtrlBlk		; get current control block
        LDB     #FileHeadLen		; set file position in U:B
        LDU     <DBZero	; U=0
        LDX     HdrLoad,X		; get load address
        LBSR    DOSFRead		; go read the file into memory
	
        BNE    	LD5F5 			; error, try catching it
	
        TST     DosRunLoadFlag		; test if we should run loaded code?
        BEQ     LD5FD			; nope return to caller
        JMP     [>BasExecAddr]		; exec loaded program

;
; basic SAVE comamnd
;
; Save a basic or binary program to disk.
;
; If basic:
;   SAVE "filename"
;
; If binary:
;   SAVE "filename",start,end,entry
;
; DosCurFilename is used as a buffer to buld the file header :-
;
; DosCurFilename	$55
; DosCurFilename	FileType
; DosCurFilename+2 	Start address
; DosCurFilename+4 	Length
; DosCurFilename+6 	Entry address	
; DosCurFilename+8	$AA
;

CmdSave   
        JSR     >VarGetStr		; get string into temp variable
        JSR     >VarGetExpr		; Get address of string in FAC
        JSR     <BasChrGetCurr		; get current basic character
        BEQ     LD587			; none, skip, it's a basic program being saved
	
        LDY     #DosExtBin		; otherwise it's machine code, point to BIN extension
        BSR     LD572			; try to open or create the file
        
	BSR     VarGetComma16		; get start address
        STX     DosCurFilename+2	; save it away
	
        BSR     VarGetComma16		; get end address
        TFR     X,D			; save it in D
        CMPX    DosCurFilename+2	; compare to start address	
        LBCS    DosPRError		; PR error if it's lower
	
        SUBD    DosCurFilename+2	; subtract start address from end address giving length in D
        LBMI    BasFCError		; FC error if it's -ve
	
        ADDD	#1
	STD     DosCurFilename+4	; save length
        
	BSR     VarGetComma16		; get entry address
        STX     DosCurFilename+6	; save it

        LDB     #FTypeBin		; set filetype to binary
        BRA     LD5A1

LD572   LBSR    DosOpenFileExtY		; get the supplied filename & try opening
        BEQ     LD57B			; all ok, skip on and create
	
        CMPB    #ErrNE		; File does not exist error?
        BEQ     LD57B			; yes, try creating  it

LD5F5	JMP     DosHookSysError
	
LD57B   LBSR    DOSCreateFile		; (re)create file
        BNE     LD5F5			; error skip to handler
LD5FD   RTS
;
; syntax check for a comma then get 16 bit integer.
;
VarGetComma16   
	JSR     >VarCKComma		; syntax check for comma
        JMP     >VarGet16Bit		; get 16 bit integer (in X)

; continuation of save code
LD587   LDY     #DosExtBas		; point at BAS extension
        BSR     LD572			; try to create or open file
	
        LDX     <BasStartProg		; get the address of the start of the basic program
        STX     DosCurFilename+2	; set this as start address
	
        LDD     <BasVarSimpleAddr	; address of simple vars = address of end of program
        SUBD    <BasStartProg		; subtract address of start of program
        STD     DosCurFilename+4	; save it in length
	
        LDX     #BasFCError		; set exec address to the FC error routine
        STX     DosCurFilename+6	; set it
        
	LDB     #FTypeBas		; filetype basic

LD5A1   LDX     #DosCurFilename		; point to DosCurFilename, used as header buffer
        LDA     #MarkerHeadStart	; mark start of header			
        STA     ,X			; save in header
        COMA				; complement it to give end marker
        STA     HdrIDAA,X		; save it in header
	
        STB     HdrType,X		; set filetype
	
        LDA     <DosCurrCtrlBlk		; get current control block
        CLRB				; Y:B, filepointer offset = 0:0
        LDY     <DBZero
        LDU     #FileHeadLen		; length of header
        LBSR    DOSFWrite		; go write it	
        BNE     LD5F5			; error, call error handler

LD5BE   LDA     <DosCurrCtrlBlk		; get current control block
        LDB     #FileHeadLen		; file offset, just after header
        LDX     DosCurFilename+2	; get start address	
        LDU     DosCurFilename+4	; get number of bytes (file len)
        LDY     <DBZero	; Y=0
        LBSR    DOSFWrite		; write the rest of the file
        BNE     LD5F5			; branch on error
	
        CLR     <DosIOInProgress	; clear file io
        RTS

;
; Basic CHAIN command 
;
; The command will save all of the users simple variables, arrays and strings.
; It will re-initialize the stack so as to flush and FOR-NEXT loops or pending
; RETURNS.
; It will also reset the READ-DATA pointer back to the beginning of the program. 
;
; CHAIN "progname"
; CHAIN "progname",runline
;
; Significantly re-arranged from DDv1, to make more space efficient.
;

CmdChain   
	BSR	LD6C4			; check and move strings to string area if needed
	
	LBSR    DosValidateAndOpenBAS	; check and open the basic program file
        BNE    	LD5F5			; error : exit
	
        LBSR    ReadHeadGetTypeA	; go read the file's header, type in A
        BNE     LD5F5			; error : exit
	
        CMPA    #FTypeBas		; make sure it's a basic program type
        LBNE    GenFMError		; nope : generate FM error
        
	JSR     <BasChrGetCurr		; get current character from basic
        BEQ     LD672			; none, assume default line no

        JSR     VarCKComma		; syntax check for comma, error if not
        JSR     BasGetLineNo		; get line number from basic

        BSR     LD67D			; move vars to accomodate new program

        LDD     <BasTempLine		; get line from basic
        JSR     BasSkipLineNo		; 'goto' that line

        BRA     LD67A

LD672	BSR     LD67D			; move vars to accomodate new program

        LDU     <BasStartProg		; point at beginning of program
        LEAU    -1,U			; decrement by 1
        STU     <BasAddrSigByte		; move basic's input pointer to start
LD67A   JMP     LD549			; jump into CmdLoad

LD67D   LDD     DosCurDriveInfo+HdrLen	; get length of file to chain
        TFR	D,Y			; save it 
        ADDD    <BasStartProg		; work out where chained program will end
        SUBD    <BasVarSimpleAddr	; work out offset to current start of vars
        PSHS    D			; save it
	
        ADDD    BasVarEnd		; add offset to end of simple vars
        STD     BasVarEnd		; and update end of simple vars
        LDB     #$40			; reserve 128 bytes of storage
        STB	DosRunLoadFlag		; flag we want to run it once loaded
	JSR     >BasChkB2Free		; check to see if free, error if not
	
        LDD     ,S			; get offset between new end of prog and beginning 
					; of vars
        BPL     LD611			; branch if positive

; -ve offset move vars down, staring at beginning so as not to overwrite ourselves
        LDX     <BasVarSimpleAddr	; point at beginning of vars
        LEAU    D,X			; add offset

LD606   LDA     ,X+			; get a byte from current vars area
        STA     ,U+			; save it in new vars area
        CMPU    BasVarEnd		; reached end of vars yet
        BLS     LD606			; nope, loop again
	
        BRA     LD61F			; branch on over other copy loop

; +ve offset move vars down, staring at end so as not to overwrite ourselves
LD611   LDX     BasVarEnd		; point to end of vars
        LEAX    1,X			; add 1
        LEAU    D,X			; add offset
	
LD617   LDA     ,-X			; get a byte from current vars area
        STA     ,-U			; save it in new vars area
        CMPX    <BasVarSimpleAddr	; reached beginning of vars?
        BCC     LD617			; nope, loop again
	
LD61F   LDD     ,S			; get offset 
        ADDD    <BasVarSimpleAddr	; add simple vars address
        STD     <BasVarSimpleAddr	; save updated address
	
        PULS    D			; pull offset
        ADDD    <BasVarArrayAddr	; add beginning of arrays address
        STD     <BasVarArrayAddr	; save updated address
        JMP     LD568			; branch into CmdLoad, to load program
	
; scan simple variable and array areas, for any string variables, check if they 
; already reside in the cleared string area at the top of RAM. If not move them
; there. This is done as strings may be defined as literals within the program text
; these will be erased when chaining a program so we need to move them so they 
; persist.

;LD669   LDU     <BasAddrSigByte		; get address of current basic program pointer
;        PSHS    X,U			; save regs
	
LD6C4   LDX     <BasVarSimpleAddr	; point at simple variables
        LEAX    2,X			; add 2
LD671   CMPX    <BasVarArrayAddr	; compare to address of array variables
        BCC     LD67F			; Found end, branch and do arrays
	
        TST     -1,X			; test second character of var name
        BPL     LD67B			; branch if numeric
        BSR     LD6AD			; check and move string to string area if needed
	
LD67B   LEAX    7,X			; move to next simple var
        BRA     LD671			; loop again

LD67F   LDU     <BasVarArrayAddr	; point at array area
LD681   CMPU    BasVarEnd		; at end of arrays ?
        BCC     LD6A5			; yep, exit
	
        LDD     2,U			; get length of array
        LEAX    D,U			; point X at array base + len
        PSHS    X			; save X
	
        TST     1,U			; test array name for string array
        BPL     LD6A1			; it's numeric, skip on
	
        LDB     4,U			; get number of dimensions
        CLRA				; zero MSB
        ASLB				; multiply no of dimensions, by 2 
					; as dimension lengths are each 2 bytes
        LEAX    D,U			; point X at beginning of array data
        LEAX    5,X			; add on header size
	
LD698   BSR     LD6AD			; check string descriptor and move to string space if needed
        LEAX    5,X			; move to next descriptor in array
        CMPX    ,S			; reached the end of this array yet?
        BCS     LD698			; nope, keep checking
	
LD6A1   PULS    U			; restore pointer to next array
        BRA     LD681			; and loop again

LD6A5   Jmp     >VarGarbageCollect	; garbage collect

; move a string into the string area if not null and not already there.
; enter with X pointing to the descriptor of the string to move, typically from
; the simple variables area

LD6AD   PSHS    X,U			; save regs
        CMPX    <BasVarStringBase	; is string in string area in high memory?
        BCC     LD6C6			; yes, no need to move it
	
        LDB     ,X			; get string length
        BEQ     LD6C6			; no need to move null string
	
        JSR     >BasResStr2		; reserve bytes in string area for B bytes
        TFR     X,U			; point U at new string		
        LDY     ,S			; point y at old string
        LDX     2,Y			; get address of old string data
        STU     2,Y			; update sescriptor's string pointer with new string
        JSR     >UtilCopyBXtoU		; copy the string data
LD6C6   PULS    X,U,PC			; restore and return

;
; Validate and open DAT file supplied on command
;

DosValidateAndOpenDAT   
	LDY     #DosExtDat		; get pointer to extension 'DAT'
        BRA     DosGetFilenameAndOpenExt

;
; Validate and open Basic program file supplied on command
;

DosValidateAndOpenBAS   
	LDY     #DosExtBas		; get pointer to extension 'BAS'
        BRA     DosGetFilenameAndOpenExt

;
; Get a filename from Dos and open it
; 	Takes a string supplied on command name, fetches it and
;	tries to open the file of that name.
; 
; If entered at DosGetFilenameAndOpenExt then extension must be pointed to by Y
;
; Exits with :-
;	A	= FCB number for opened file
;	B	= Error code 
;	Y	= ptr to extension
;

DosGetFilenameAndOpen   
	LDY     #DosExtNone		; Point to Blank extension

DosGetFilenameAndOpenExt   
	PSHS    Y			; save extension pointer
        JSR     >VarGetStr		; get string into temp variable
        JSR     >VarGetExpr		; Get address of string in FAC
        PULS    Y
	
DosOpenFileExtY   
	LDX     <FPA0+2			; get address of filename string
        PSHS    X
        LDB     ,X			; Get string length
        LDX     2,X			; Get pointer to actual string data
        LBSR    DosValidateAndOpen	; Validate & open file (if valid)
        PULS    X
        PSHS    D
        JSR     >VarDelVar		; delete var
        PULS    D
        TSTB				; Test error and set flags
        RTS

; 
; Close 1 or all files.
;

DosCloseAllFiles   
	LDA     #$0A			; set current control block to 10
        STA     <DosCurrCtrlBlk
	
LD6FF   LBSR    DosCloseFile2		; Try to close the file for this block
        BNE     LD71D			; error : call error handler
	
        DEC     <DosCurrCtrlBlk		; do next
        BPL     LD6FF
	
LD756
LD708   CLR     <DosIOInProgress	; flag no dos io 
LD70A   RTS

; The code between LD70B to the instruction before LD71D seems unused.....
LD70B   BLE     LD70A
        LDX     ,S
        CMPX    #$B7EE
        BNE     LD70A
        LEAS    2,S
        STB     <DosDriveNo
        LBSR    DOSCloseAll
        BEQ     LD708

LD71D   JMP     >DosHookSysError

DosHookReadInput   
	BSR     DosCloseAllFiles	; close all files
        JMP     >CheckAndDoAuto		; check see if auto active, if so do it

;
; CREATE command dispatch routine
;
; Syntax :
;
;	CREATE "filename",size_in_bytes
;

CmdCreate   
	BSR     DosValidateAndOpenDAT	; validate and open the supplied filename
        BEQ     LD732			; branch if no error
	
        CMPB    #ErrFE		; check for file exists
        BEQ     LD732			; if it exists just set length
	
        CMPB    #ErrNE		; File does not exist error
        BNE     LD71D			; call error handler
	
LD732   LDA     <DosCurrCtrlBlk		; get current control block
        LBSR    DOSCreateFile		; go create the file
        BNE     LD71D			; if error, call the error handler
	
        JSR     <BasChrGetCurr		; get current character from basic
        BEQ     LD773			; nothing, return, crate zero length file
	
        JSR     >VarCKComma		; syntax check for comma 
        JSR     >VarGetStr
	
        LBSR    Get24bitUA		; get filesize?
LD746   TST     <FPA0+1
        BEQ     LD75C
	
        LDD     #$FF00			; claculate ammount to extend file by
        BSR     LD76E
	
        LDD     <FPA0+2
        SUBD    #$FF00
        STD     <FPA0+2
        BCC     LD746
        
	DEC     <FPA0+1
        BNE     LD746
	
LD75C   LDD     <FPA0+2
        BEQ     LD773
        
	CMPD    #$FF00
        BLS     LD76E
        
	SUBD    #$FF00
        BSR     LD76E
        
	LDD     #$FF00

LD76E   LBSR    ExtendFile		; extend file
        BNE     LD71D			; call error handler if error
LD773   RTS

;
; Remove a file from disk directory, deallocate any space on the disk
; for this file.
;
;   KILL "filename"
;

CmdKill   
	LBSR    DosGetFilenameAndOpen	; find the filename and open it
        BNE     LD77E			; error, exit
	
        LBSR    DOSDeleteFile		; delete the file
        BEQ     LD708			; no error, return to basic
	
LD77E   JMP     >DosHookSysError	; call error handler

;
; Protect/unprotect a file.
;
;   PROTECT "filename" 		turns ON protection
;   PROTECT ON "filename" 	turns ON protection
;   PROTECT OFF "filename" 	turns OFF protection
;

CmdProtect   
	TSTA				; test current character
        BPL     BasProtectOn		; go get filename, protect on
        
	CMPA    #DTokOFF		; 'OFF' token?
        BEQ     BasProtectOff		; yep go deal with it
	
        CMPA    #DTokON			; 'ON' token
        LBNE    BasSNError		; nope, SN error

BasProtectOn
	BSR     LD798			; get filename
        LDB     #$01			; flag protect on
LD792   LBSR    DOSProtectMC		; go turn it off/on
        BNE     LD77E			; Go handle errors
        RTS

LD798   LBSR    BasChrGet		; get a character from basic
        LBSR    DosGetFilenameAndOpen	; get the filename and open file
        BNE     LD7C4			; Go handle errors
        RTS

BasProtectOff   
	BSR     LD798			; go get filename
        CLRB				; flag protect off
        BRA     LD792			; go do it

;
; Rename a file.
;
;   RENAME "oldname" TO "newname"
;

CmdRename   
	LBSR    DosGetFilenameAndOpen	; get first filename and open it
        BNE     LD7C4			; error, exit
	
        PSHS    A			; save FIB no
        LDB     #DTokTO			; check for 'TO' token
        JSR     >VarCKChar		; syntax check for it
	
        LBSR    DosGetFilenameAndOpen	; get second filename and open
        BEQ     LD7C2			; generate error
	
        CMPB    #ErrNE			; Error from open "Not exist"?
        BNE     LD7C4			; no, real error
	
        PULS    A			; restore FCB no
        LBSR    DOSRename		; go change the name.
        BNE     LD7C4			; deal with error
        RTS

LD7C2   LDB     #ErrFE			; File exists error for destinatiion file
LD7C4   JMP     >DosHookSysError	; deal with errors
	
;
; Basic FLREAD.
; 
; FREAD "filename",FROM xx, FOR yy; variable list
;
; 	0 > xx > 350208
;	0 > yy > 255
;

CmdFLRead   
	BSR     FLReadSetup		; get setup for read open file etc
        LDA     #$FF			; flag FLREAD
        STA     DosFlFreadFlag
        LBSR    LD969			; do the read
        BSR     LD803			; fix the pointers
        JMP     >BasLineInputEntry	; finish off input put it into string
	

FLReadSetup   
	LBSR    DosValidateAndOpenDAT	; validate filename and open it
        BNE     LD82D			; error, exit
	
        BSR    	FLRWGetNamePos		; get name of file, and filepointer pos
        LBSR    DosFCBNoToAddr		; get address of FCB in X
        
	TST     DosTemp2		; did we set the filepointer
        BEQ     LD7F0			; branch if not
	
        LDU     DosFWriteAddr		; get offset to read from
        LDB     DosFWriteAddr+2
        STU     FCBFilePointer,X	; copy to FCB
        STB     FCBFilePointer+2,X
	
LD7F0   LBSR    LD9DC			; check and read file
        LDX     #BasLinInpBuff		; point at basic input buffer
        CLR     ,X			; clear fist byte
        LBSR    BasChrGet		; go read from buffer.
        RTS				

;
; Basic FREAD command
; 

CmdFRead   
	BSR     FLReadSetup		; get setup for read open file etc
        CLR     DosFlFreadFlag		; flag FREAD
	
        JSR    >CmdReadFromX		; read  the input

; give what we didn't use back... this relies on TempL and TempC
; being in conecutive memory locations.
; Count of read bytes ios in TempC:TempL
;
LD803   LBSR    DosFCBNoToAddr		; get address of FCB
        LDB     TempL			; is there anything left?	
        BEQ     LD819			; no
	
        CLR     TempL-1			; clear remainder count MSB
        LDD     FCBFilePointer+1,X	; get filepointer
        SUBD    TempL-1			; subtract ammount read
        STD     FCBFilePointer+1,X	; save it back
        BCC     LD819			; any carry to MSB?
        DEC     FCBFilePointer,X	; yes do carry
	
LD819   TST     <DosRecLenFlag		; test record length flag	
        BEQ     LD82A			; zero, exit, no FOR statement
	
        LDB     <DosNoBytesMove		; check number of bytes specified in FOR
        BEQ     LD82A			; zero, exit
        
	CLRA				; clear MSB
        ADDD    FCBFilePointer+1,X	; add FOR to filepointer
        STD     FCBFilePointer+1,X
        BCC     LD82A			; branch if no carry
        INC     FCBFilePointer,X	; do carry

LD82A   CLR     <TextDevN		; input back to console
LD82C   RTS

LD82D   JMP     >DosHookSysError	; call error handler

FLRWGetNamePos   
	JSR     >BasChkDirect		; check for direct mode, error if so
        LDA     <DosCurrCtrlBlk		; get current FCB no
        LBSR    DOSGetFLen		; get file length
        BNE     LD82D			; branch if error
	
        STU     DosFWriteAddr		; save file length, as write address
        STA     DosFWriteAddr+2		; so we default to appending to the file
	
        CLR     <DosRecLenFlag		; clear record length
        CLR     DosTemp2		; clear temp var
        LDA     #DevDisk		; set device number to disk			
        STA     <TextDevN
	
LD849   JSR     <BasChrGetCurr		; get current character from basic
        CMPA    #';'			; is it a semicolon?	
        BEQ     LD8EF			; yep get data to read/write
	
        JSR     >VarCKComma		; syntax check for comma
        CMPA    #DDTokFROM		; is it 'FROM' token?
        BNE     LD86A			; no, deal with it
	
        JSR     <BasChrGet		; get next character from basic
        JSR     >VarGetStr		; get filepointer pos to read/write
        BSR     Get24bitUA		; convert it to 24 bit in U:A
	
        STU     DosFWriteAddr		; update the write address
        STA     DosFWriteAddr+2		; 
        LDA     #$FF			; set DosTemp2 flag
        STA     DosTemp2
        BRA     LD849			; loop again

LD86A   CMPA    #DTokFOR		; for token?
        BNE     GenBasSNError		; nope, generate SN error
	
        JSR     <BasChrGet		; get it from basic
        LBSR    Get8BitorError		; go get an 8 bit record size
	
        STB     <DosNoBytesMove		; save record size
        LDB     #$FF			; flag record size used
        STB     <DosRecLenFlag		
        BRA     LD849

GenBasSNError   
	JMP     >BasSNError		; generate syntax error

GenBasFCError   
	JMP     >BasFCError		; generate FC error

GenBasTMError   
	JMP     >BasTMError		; generate TM error

; Get a 24 bit number in U:A ?
Get24bitUA   
	TST     <FP0SGN			; test sign of mantissa
        BMI     GenBasFCError		; if minus, generate FC error
	
        TST     <BasVarType		; test variable type
        BNE     GenBasTMError		; if not numeric generate TM error
	
        LDA     #$A0						
        SUBA    <FP0EXP			; calculate number of shifts to do
        BEQ     LD89D			; branch if zero, no ajustment needed
	
LD892   LSR     <FPA0			; rotate / shift FPA0, A times
        ROR     <FPA0+1
        ROR     <FPA0+2
        ROR     <FPA0+3
        DECA
        BNE     LD892
	
LD89D   LDU     <FPA0+1			; return in U:A
        LDA     <FPA0+3
LD8EF   RTS

; 
; basic FWRITE command
;
; FWRITE "filename",AT xx, FOR yy; variable list
;
; 	0 > xx > 350208
;	0 > yy > 255
;

CmdFWrite   
	LBSR    DosValidateAndOpenDAT	; open and validate file
        BEQ     LD8B3			; no error, branch on
	
        CMPB    #ErrNE			; Does the file not exist?
        BNE     LD8A2			; no it was another, error handle it.
	
        LBSR    DOSCreateFile		; create the file if it does not exist
        BNE     LD8A2			; error handle it
	
LD8B3   LBSR    FLRWGetNamePos		; get name and filepointer to start writing at
        LBSR    FindFreeDiskBuffer	; find a free disk buffer, to write data into
        BNE     LD8A2			; error, exit
	
        STX     DosFWriteBPtr		; save the buffer pointer 
        LDA     #BuffInUse		; mark disk buffer in use	
        STA     BuffFlag,X		
        CLR     <DosBytesInDTA		; no bytes ready to write	
        
	JSR     <BasChrGet		; get next character from basic
        JSR     >CmdPrint		; write it
	
        TST     <DosRecLenFlag		; test record length
        BEQ     LD8D8			; not used, skip on
	
        LDB     <DosNoBytesMove		; get record size
        BEQ     LD8D8			; branch if buffer full?
        
	LDA     #' '			; else fill remaing buffer with spaces
LD8D3   BSR     LD91D			; put in buffer
        DECB				; decrement count
        BNE     LD8D3			; branch if any left
	
LD8D8   TST     <DosBytesInDTA		; any bytes in buffer?
        BEQ     LD8F4			; nope, skip
	
        LDX     DosFWriteBPtr		; get pointer to buffer details
        LDX     BuffAddr,X		; get buffer address
        CLRA				; set D to bytes in buffer
        LDB     <DosBytesInDTA
        TFR     D,U			; transfer it to U
        LDA     <DosCurrCtrlBlk		; get current file no
        LDY     DosFWriteAddr		; get filepointer to write at
        LDB     DosFWriteAddr+2
	
        LBSR    DOSFWrite		; go write to the file
        BNE     LD8A2			; branch on error
	
LD8F4   LDX     DosFWriteBPtr		; get pointer to buffer details
        CLR     BuffFlag,X		; mark buffer as free
LD8F9   RTS


;
; Check validity of device number
;

DosHookCheckIONum   
	BLE     LD8F9			; 0 or less, not disk....
        CMPB    #$04			; we only use 1
        BHI     LD914			; higher, generate DN error
        PULS    X,PC			; restore and return

;
; Check for device open.
;   If device number is greater than zero, the a DN error is called for.
;

DosHookOpenDev   
	LEAS    2,S			; drop return address
        JSR     >VarGetStr		; evaluate expression
        JSR     >BasGetStrFirst		; get first byte of string in B
        PSHS    B			; save b
        JSR     >BasGetDevNo		; get device number
        TSTB				; check device number
        LBLE    CmdOpenEntry		; >=0, go open it
	
LD914   LDB	#BErrDN			; device number error
LD8A2	JMP     DosHookSysError		; call system error handler

;
; Character output hook.
;  The hook routine for writing a single byte to a data file.
;

DosHookCharOut   
	TST     <TextDevN		; test device number
        BLE     LD8F9			; brance if 0 or -ve, not ours				
	
        LEAS    2,S			; discard return address
LD91D   PSHS    D,X,Y,U			; save regs

        LDB     <DosRecLenFlag		; buffering flag?
        BEQ     LD927			; not buffering

        LDB     <DosNoBytesMove		; at end of buffer?
        BEQ     LD95E			; yes 

LD927   LDX     DosFWriteBPtr		; get buffer detail pointer
        LDX     BuffAddr,X		; get buffer address	
        LDB     <DosBytesInDTA		; get buffer offset write pointer
        ABX				; add to buffer pointer
        STA     ,X			; save byte in buffer
	
        DEC     <DosNoBytesMove		; decrement bytes left counter
        INC     <DosBytesInDTA		; increment bytes in buffer counter
        BNE     LD95E			; branch if still space in buffer
	
        LDA     <DosCurrCtrlBlk		; get current file no
        LDX     DosFWriteBPtr		; get buffer detail pointer
        LDX     BuffAddr,X		; get buffer address	
        LDU     #$0100			; write 256 bytes
        LDY     DosFWriteAddr		; get filepointer
        LDB     DosFWriteAddr+2
	
        JSR     >DOSFWrite		; go write it
        BEQ     LD950			; branch if no error
        
	JMP     >DosHookSysError	; call error handler

LD950   LDD     DosFWriteAddr+1		; move filepointer on
        ADDD    #$0100
        BCC     LD95B			; branch if no carry
        INC     DosFWriteAddr		; carry if needed
LD95B   STD     DosFWriteAddr+1		; save pilepointer

LD95E   PULS    D,X,Y,U,PC		; resore and return

;
; Hook to scan input from disk.
;
; Should do:
;   If string:
;     do not remove leading spaces.
;     Terminate input on:
;       $0D - CR
;       $2C - comma
;       EOF
;
;   If numeric:
;     remove leading spaces
;     Terminate on:
;       Any character less than or equal to space.
;

L879A	EQU	$879A			; return point (Dragon)
	
DosHookDiskItem   
	TST     <TextDevN		; test device number
        BLE     LDA14 			; 0 or -ve, not ours
	
        LDX     #L879A			; return point in basic ROM
        STX     ,S			; stack it
	
LD969   LDA     <DosRecLenFlag		; reading?
        BEQ     LD971			; yep
	
        LDA     <DosNoBytesMove		; check bytes in buffer
        BEQ     LD9BE			; read no more
	
LD971   LDX     #BasLinInpBuff+1	; point at basic line input buffer
        LDB     <DosBytesInDTA		; current input pointer
        STB     TempC			; save it in count
        ABX				; add to buffer base
	
        LDB     TempL			; save number of bytes in buffer
        STB     DosRunLoadFlag
        CLRB				; clear byte before current input
        STB     -1,X
	
        STB     <BasBreakFlag		; clear flag
LD985   DEC     <DosNoBytesMove		
        DEC     TempL
        INC     <DosBytesInDTA
        LDA     ,X+			; get a byte from buffer
        BEQ     LD9BE			; done if EOF
	
        CMPA    #$0D			; is it CR?
        BEQ     LD9BE			; yep deal with it
	
        TST     DosFlFreadFlag		; are we reading a number?
        BNE     LD9A1			; yes, don't check for string terminators
        
	CMPA    #','			; check for comma
        BEQ     LD9BE			; yep deal with it
	
        CMPA    #':'			; check for colon
        BEQ     LD9BE			; yep deal with it
	
LD9A1   LDA     <DosRecLenFlag		; get bytes to read	
        BEQ     LD9A9			; zero, read no more
	
        LDA     <DosNoBytesMove		; 
        BEQ     LD9BE
	
LD9A9   INCB				; number of bytes in string
        CMPB    #$FF			; max string length
        BEQ     LD9BC			; yep! string full
	
        LDA     TempL						
        BNE     LD985
	
; read a new buffer of data	
        LDB     <BasBreakFlag		; 
        BNE     LD9BC
	
        BSR     LD9C8
        CLRB
        BRA     LD985

; return to input (basic)
LD9BC   LEAX    1,X			; increment past last byte		
LD9BE   LDB     TempC			; get count
        CLR     -1,X			; clear line termination byte
        LDX     #BasLinInpBuff		; point at buffer
        ABX				; add offset
LDA14   RTS

LD9C8   JSR     >DosFCBNoToAddr		; get address of FCB for file
        CLRA				; clear MSB
        LDB     DosRunLoadFlag		; load B with ammount to read
        NOP
        PSHS    D			; save D
        LDD     FCBFilePointer+1,X	; get filepointer
        SUBD    ,S++			; move it back?
        STD     FCBFilePointer+1,X
        BCC     LD9DC
        DEC     FCBFilePointer,X
	
LD9DC   PSHS    Y,U			; save regs
        LDU     #$00FF			; ammount to try reading
        BSR     LDA01			; check enough bytes left
	
        LDU     #DBasLinInpBuff+1	; point at basicline buffer
        CLR     D,U			; clear byte..... (after end?)
        LDA     <DosCurrCtrlBlk		; get current file no
        LDB     FCBFilePointer+2,X	; get filepointer
        LDX     FCBFilePointer,X
        EXG     X,U			; swap X and U to get buffer & offset in correct regs
        JSR     >DOSFRead		; go read the file
        BNE     LD9FE			; error, exit
	
        LDX     #DBasLinInpBuff+1	; point at basicline buffer
        CLR     -1,X			; clear byte before read data
        PULS    Y,U,PC			; restore and return

; take an ammount of bytes we want to read in U, and work out if there
; are still U bytes in the file to read. This is doen by adding U to
; the current file pointer, and then comparing it to the file length.

LDA01   PSHS    U			; save ammount to load?
        LDD     FCBFilePointer+1,X	; get LSW of filepointer
        ADDD    ,S			; add ammount to filepointer
        PSHS    D			; save it
        LDA     FCBFilePointer,X	; get MSB of filepointer	
        ADCA    #$00			; propagate carry
        SUBA    FCBFileLen,X		; subtract file length msb
        PULS    D			; restore LSW of difference, flags untouched
        BCS     LDA26			; was much at least 64K lower so safe to read
        BHI     LD9FC			; error will read past EOF
	
        SUBD    FCBFileLen+1,X		; sheck the LSW of the filepointer
        BLS     LDA26			; was lower so safe to read
	
; if we still have bytes that can be read but have not reached EOF, then work
; out how many bytes left	
	
        LDD     FCBFileLen+1,X		; get the file length
        SUBD    FCBFilePointer+1,X	; subtract filepointer
        BEQ     LD9FC			; special case exactly at EOF
        STD     ,S			; save ammount we can read
        COM     <BasBreakFlag		; ????
	
LDA26   LDD     ,S			; get ammount we can read
        STB     TempL			; save LSB (ammount read?)
        STB     DosRunLoadFlag		; ???
        CLR     TempL-1			; MSB that we read
        CLR     <DosBytesInDTA		
        PULS    Y,PC			; restore and return

LDA7D
LD9FC   LDB     #ErrPE			; read past end of file
LDA7F
LD9FE   JMP     >DosHookSysError	; call error handler	
	
;
; Dir command dispatch routine
;
; Syntax :
;	DIR drivenum	
;

CmdDir  BEQ     LDA3C			; No drive specified, use default
        LBSR    GetDriveNoInB		; Else get from command 
        BRA     LDA3F

LDA3C   LDB     DosDefDriveNo		; Get default drive

LDA3F   STB     <DosDriveNo		; Flag as last drive used
        CLR     ,-S			; make temp on stack (file number on disk counter)
        CLRB
	
LDA44   JSR     >BasPollKeyboard	; poll keyboard
        LBSR    DOSGetDirEntry		; go get next entry
        BNE    	LDA7F			; error : exit
	
        LDA     ,X			; Get Attribute byte
        BITA    #AttrEndOfDir		; Check for end of directory $08
        BNE     CmdDirDoneAll		; Yes : stop processing
	
        BITA    #AttrDeleted+AttrIsCont ; Is entry a deleted file, or continuation entry ? $81				; and another thing
        BNE     CmdDirDoNextEntry	; yes : do next
	
        LEAX    1,X			; Point at filename
        LDB     #$08			; Up to 8 chars
        BSR     PrintBCharsFromX	; Print it
        LDB     #$04			; print extension + '.'
        LDA     #$2E			; '.'
	BSR     PrintBCharsFromX2	; Print it
	
        LDA     -12,X			; Point at attributes
        BITA    #AttrWriteProt		; Is this a protected file ?
        BEQ     LDA70			; no skip on
        LDA     #'p'			; Flag protected $70
        FCB	Skip2
LDA70	LDA	#$20			; space
        JSR     >TextOutChar		; output attribute byte
        JSR     >TextOutSpace		; And a space
        LDU     #$FFFF
        LDX     #DosFCB0Addr		; Point at first FCB
        LDB     ,S			; Get file number
        STB     FCBDiskFileNo,X		; Save in FCB

        LBSR    FindFSNinU		; find last File sector
        BNE     LDA7F			; error : exit

        LBSR    LCE99			; add last bytes to length
        BSR     LDAB7			; Print filesize
	
        JSR	TextOutCRLF		; Output EOL

CmdDirDoNextEntry   
	INC     ,S			; do next
        LDB     ,S			; Get disk file number counter
;***       CMPB    #$A0			; More than max files on disk ?
        CMPB    #$B4			; More than max files on disk ?
        BCS     LDA44			; Less, loop again.	

;
; We come here either when we have processed $A0 entries, which is the maximum,
; or we have reached an entry with the AttrEndOfDir bit set which signals the end
; of the directory.
;

CmdDirDoneAll   
	PULS    A
        LBSR    DOSGetFree		; Get free bytes on drive
        CLRA
        TFR     X,U
        BSR     LDAB7			; Display free bytes
        LDX     #BytesFreeMess-1	; Point to message
        JMP     >TextOutString		; print it, and return

; Print B chars pointed to by X, if char is $00, then output a space.
; Used to print filenames.

PrintBCharsFromX   
	LDA     ,X+			; Fetch a char
        BNE     PrintBCharsFromX2	; Is it zero ? no : skip
        LDA     #$20			; Replace it with a space

PrintBCharsFromX2
	JSR     >TextOutChar		; Output it
        DECB				; Decrement count
        BNE     PrintBCharsFromX	; any left yes : loop again
        RTS

LDAB7   
LDAFD
	ifdef	PGSFix
	JSR	LDD95			; put number in FPA0, normalize it
	JMP	TextOutNumFPA0		; output it

DosHookReadInputFix
	PSHS	U,X,D			; save regs
	LDX	#$837D			; check source PC on stack
	CMPX	8,S
	BNE	LDACB			; nope restore & return
	JSR	DosCloseAllFiles	; close all files
	BRA	CheckAndDoAutoFix	; jump to Auto check
LDACB	PULS	U,X,D,PC		; restore & return
	NOP
	else
	STU     <FPA0+1
        STA     <FPA0+3
        CLR     <FP0SGN
        CLR     <FPA0
        CLR     <FPSByte
        LDA     #$A0
        STA     <FP0EXP
        JSR     >VarNormFPA0
        JSR     >TextOutNumFPA0+5	; $9587
        JMP     >TextOutString
	endc

BytesFreeMess
        FCC    " FREE BYTES"
        FCB    $0D,$0D,$00

;
; Auto command dispatch routine
;
; Syntax :
;
;	AUTO startline,increment
;

CmdAuto 
	LDX     <BasCurrentLine		; Get current line number
        LEAX	1,X			; will be $000 if we where in direct mode
        LBNE    BasFCError		; nope, not direct mode : error

LDAE7   LDX     #AutoStartLine		; set default start line
        LDD     #AutoIncrement		; set default increment
        PSHS    D,X
        
        JSR     <BasChrGetCurr		; get current character
        BEQ     CmdAutoDoAuto		; nothing, used defaults		
	
        JSR     >VarGet16Bit		; get startline
        LDD     <FPA0+2
        STD     2,S			; update value on stack
        JSR     <BasChrGetCurr		; any more parameters ?
        BEQ     CmdAutoDoAuto		; nope use default increment
	
        JSR     >VarCKComma		; get comma, error if not
        JSR     >DVarGet16Bit		; get increment
        LDD     <FPA0+2
        BEQ     CmdAutoErrorExit	; increment is zero : error
        
	STD     ,S			; update increment value
        JSR     <BasChrGetCurr		; get current char
        BEQ     CmdAutoDoAuto		; no more : do it !
	
CmdAutoErrorExit   
	JMP     >BasSNError		; More chars left, SN? error

CmdAutoDoAuto   
	ORCC    #(FlagIRQ+FlagFIRQ)	; Disable interrupts	
        LDD     ,S++			; Get Increment off stack		
        STD     DosAutoInc
        LDD     ,S++			; Get start line off stack
        SUBD    DosAutoInc		; Subtrack increment
        STD     DosAutoCurrent		; Save in current
        LDA     #AutoFlag		; Flag in AUTO mode
        STA     DosAutoFlag
        RTS

CheckAndDoAuto   
	PSHS    D,X,U
CheckAndDoAutoFix   
	TST     DosAutoFlag		; is auto enabled ?
        BNE     LDB30			; yes : handle it
LDB2E   PULS    D,X,U,PC

LDB30   LDD     DosAutoCurrent		; get current line no
        ADDD    DosAutoInc		; add increment
        CMPD    #BasMaxLineNo		; greater than max line no ?
        BHI     LDB2E			; yep : exit
	
        STD     ,S			; save new line on stack
        JSR     >TextOutNum16		; output line no
        LDA     #$20			; output a space
        JSR     TextOutChar		
	
        LDU     #BasBuffer+3		; point at basic buffer
        LDD     ,S			; get current line no
        STD     DosAutoCurrent		; save it
        LDX     #BasLinInpBuff+1	; point to line input buffer
        LDB     #$00
LDB53   LDA     ,U+			; get byte from basic buffer
        BEQ     LDB5C			; zero ?
	
        STA     ,X+			; nope put in buffer
        INCB
        BRA     LDB53

LDB5C   LDA     #$20			; store space
        STA     ,X+
        INCB			
LDB61   JSR     TextWaitKeyCurs2	; Wait with cursor
        CMPA    #$0D			; return ?
        BEQ     LDB6C			
	
        CMPA    #$03			; break ?
        BNE     LDB82
	
LDB6C   CLR     DosAutoFlag		; clear auto flag
        LDA     #$0D			; output a return
        JSR     TextOutChar
	
        LEAS    8,S			; drop stack frame
        CLR     <CasLastSine
        LDX     #DBasLinInpBuff+1	; point at input buffer
        LDA     #$0D
        LDB     #$01
        JMP     >BasInBuffFromX		; input it

LDB82   CMPA    #$20			; space ?
        BCS     LDB61			; yep keep going

        CMPA    #$7B			; max ascii char ?
        BCC     LDB61			; yep keep going

        LEAS    8,S			; drop stack frame
        CLR     <CasLastSine
        JMP     >BasInBuffFromX		

DoBeep  CLRA				; A=0	
        LDX     #$0008			; Repeat 8 times
LDB95   JSR     >CasByteOut		; Output A to cassette
        LEAX    -1,X			; Decrement count
        BNE     LDB95			; Loop again if all not done
LDB9C   RTS

;
; Beep command dispatch routine
;
; Syntax :
;	BEEP nobeeps
;

CmdBeep   
	BEQ     CmdBeepDef		; if no params, default to 1 beep
        LBSR    Get8BitorError		; get beep count 

	FCB	Skip2
CmdBeepDef
	LDB	#$01			; Default beep count

        PSHS    B			; save count on stack
        CLRB
        JSR     SndDTOAOn		; switch sound to D to A
LDBAB   BSR     DoBeep
        JSR     >BasPollKeyboard	; check for key
        DEC     ,S			; decrement beep count
        BEQ     LDBBF			; done all : restore and exit
        LDY     #$6000			; wait a short while
LDBB9   LEAY    -1,Y
        BEQ     LDBAB			; loop back and do next beep
        BRA     LDBB9

LDBBF   PULS    B,PC

;
; Wait command dispatch routine.
;
; Syntax :
;	WAIT miliseconds
;

CmdWait   
	BEQ     WaitExit		; no params : exit
        JSR     >VarGet16Bit		; get no of ms to wait
        LDX     <FPA0+2
LDBC8   JSR     >BasPollKeyboard	; scan keyboard
        LDB     #$A0			; loop to delay
LDBCD   DECB
        BNE     LDBCD			; keep going
	
        LEAX    -1,X			; decrement ms counter
        BNE     LDBC8			; keep going if not zero
WaitExit
        RTS

;
; Swap command dispatch routine.
;
; Syntax :
;	SWAP var1,var2
;

CmdSwap   
	JSR     >VarGetVar		; Get var from basic
        LDX     <BasVarPtrLast		; get variable pointer and type
        LDA     <BasVarType
        PSHS    A,X			; save them
        JSR     >VarCKComma		; check for comma
        JSR     >VarGetVar		; get second var
        PULS    A,Y			; recover pointer and type of first
        CMPA    <BasVarType		; var types the same ?
        LBNE    BasTMError		; nope : error
	
        LDU     <BasVarPtrLast		; get pointer to second var
        LDX     #$0005			; 5 bytes for var discriptor
LDBF1   LDA     ,U			; swap a pair of bytes
        LDB     ,Y
        STB     ,U+			; increment pointers
        STA     ,Y+
        LEAX    -1,X			; decrement count
        BNE     LDBF1			; keep going if more bytes
        RTS

; Writes to Alpha drive control registers
; On entry A contains byte to output
AlphaDskCtl   
	PSHS    X,B,CC			; save registers
        LDB     #AYIOREG		; select AY-8912 I/O register
        LDX     #PIA2DA			; via PIA2
        ORCC    #(FlagIRQ+FlagFIRQ)	; disable interrupts
	
        STB     2,X			; select AY-8912 I/O regiter
        LDB     ,X			; Latch register to modify 
        ORB     #$03
        STB     ,X
        ANDB    #$FC
        STB     ,X
	
        STA     2,X			; HW mask out to AY-8912
        INC     ,X			; tell AY
        DEC     ,X
        PULS    PC,X,B,CC               ; restore and return

; CmdBoot is here in the DDv1.0 code, not needed for Alpha.

;
;Drive command dispatch routine.
;
; Syntax :
;	DRIVE drivenum	
;

CmdDrive
	BEQ     LDC46			; If no parameters : error
        LBSR    GetDriveNoInB		; Get drive number	
        STB     DosDefDriveNo		; set it
        RTS

;	JMP     <BasChrGet		; back to interpreter loop

LDC46   JMP     >BasSNError

;
; Error command dispatch routine.
;
; Syntax :
;	ERROR GOTO lineno
;

	ifdef	Dragon
TokGO	equ	DTokGO
TokTO	equ	DTokTO
	else
TokGO	equ	CTokGO
TokTO	equ	CTokTO
	endc

CmdError   
	CMPA    #TokGO			; Check next token is GO
        BNE     LDC46			; Error if not
        
	JSR     <BasChrGet		; get next char from basic
        
	CMPA    #TokTO			; check next token is TO
        BNE     LDC46			; Error if not TO
	
	JSR     <BasChrGet		; skip to token
        JSR     >BasGetLineNo		; get line number for error handler
        LDX     <BasTempLine		
        CMPX    #BasMaxLineNo		; if bigger than max line no : FCer
        LBHI    BasFCError		
	
        STX     DosErrDestLine		; save in error handler line 
        BEQ     LDC6C			; if Zero : clear error handler flag
        LDA     #ErrGotoEnabled		; flag goto enabled
        STA     DosErrGotoFlag
        RTS

LDC6C   CLR     DosErrGotoFlag		; Turn off error goto
        RTS
	
;
; Gets address of a string  supplied on command line into X
;

DosGetStr   
	JSR     >VarCKComma		; Check for a comma
        JSR     >VarGetVar		; get next variable
        JMP     >VarGetExpr		; and evaluate it
	
;
;Sread command dispatch routine.
;
; Syntax :
;	SREAD driveno,trackno,secno,part1$,part2$
;

CmdSread   
	LBSR    GetSreadWriteParams	; Get drive,track,secno
        BSR     DosGetStr		; Get address of first 128 bytes to read
        PSHS    X			; save on stack
        BSR     DosGetStr		; Get address of second 128 bytes to read
        PSHS    X
	
	LDB     #DosFlagTrue
        STB     <DosIOInProgress	; Flag Dos IO in progress
        LBSR    FindFreeDiskBuffer	; Find a buffer to read sector into
        BNE     LDCB1			; Error : exit
	
        CLR     BuffFlag,X		; Clear buffer flag
        LDX     BuffAddr,X		; Get buffer address
        STX     <DiskBuffPtr		; Save as pointer to do read
        LBSR    DosDoReadSec		; Read the sector
	
        STB     DosErrorCode		; Save error code in temp storage
        LDU     <DiskBuffPtr		; Get pointer to read data
        LEAU    $0080,U			; Point to second half of sector
        PULS    X			; Get address of second string
        BSR     LDCB4			; Copy bytes to string
	
        LDU     <DiskBuffPtr		; Point at begining of disk buffer
        PULS    X			; Get address of first string
        BSR     LDCB4			; Copy bytes to string
        
	CLR     <DosIOInProgress	; Flag Dos IO not in progress
        LDB     DosErrorCode		; Retrieve error code from read
        BNE     LDCB1			; Error : go to error handler
        RTS				; return to caller

LDCB1   JMP     >DosHookSysError	; Jump to error hook

LDCB4   PSHS    X,U			; save regs		
        LDB     #$80			; Make room for 128 bytes
        JSR     >BasResStr		; resize the string
        LEAU    ,X
        PULS    X
        STB     ,X
        STU     2,X
        PULS    X
        JMP     >UtilCopyBXtoU		; copy the bytes, return to caller

;
; Swrite command dispatch routine.
;
; Syntax :
;	SWRITE driveno,side,sector,part1$,part2$
;

CmdSwrite   
	BSR     GetSreadWriteParams	; get parameters
        BSR     LDD14			; get first half of sector
        JSR     >VarGetExpr
        LDX     <FPA0+2		; get address of string
        PSHS    X
        BSR     LDD14			; get second half of sector
        JSR     >BasGetStrLenAddr	; get length & addr
        PSHS    B,X
	
        LDB     #IOInProgress		; flag that IO is in progress
        STB     <DosIOInProgress
        LBSR    FindFreeDiskBuffer	; Go find a disk buffer to use
        BNE     LDCB1			; error : exit
	
        CLR     BuffFlag,X		; make buffer free
        LDX     BuffAddr,X		; get buffer address
        STX     <DiskBuffPtr		; use this for disk io
        CLRB
LDCEA   CLR     ,X+			; Clear buffer
        DECB
        BNE     LDCEA
	
        PULS    B,X			; get saved string pointers for second half of sec
        LDU     <DiskBuffPtr		; point to disk buffer
        LEAU    $0080,U			; start halfway through
        TSTB				; any bytes to transfer ?
        BEQ     LDCFD			; nope skip on
        JSR     UtilCopyBXtoU		; copy bytes

LDCFD   PULS    X			; recover pointer to fist half of sector
        JSR     >VarDelVar		; delete var
        LDU     <DiskBuffPtr		; recover disk buffer ptr
        TSTB				; any bytes to transfer ?
        BEQ     LDD0A			; nope skip
	
        JSR     >UtilCopyBXtoU		; copy first half of sector
	
LDD0A   LBSR    DosDoWriteSecV		; go write it
        BLO	LDCB1			; error : exit
        CLR     <DosIOInProgress	; flag no io in progresss	
        RTS

LDD14   JSR     VarCKComma		; check for comma
        JMP     >VarGetStr		; get string

GetSreadWriteParams   
	BNE     LDD1F			; params, read them
        JMP     >BasSNError		; none : SN Error

;
; Get params for Sread/Swrite
;

LDD1F   LBSR    GetDriveNoInB		; Drive number
        STB     <DosDriveNo
        JSR     >VarGetComma8		; Track number
        CMPB    #MaxTrack		; greater than track 80 ?
	BHI     LDD33			; Yes : error
	
	STB     <DskTrackNo		; Save track number
        JSR     >VarGetComma8		; Get sector number
        STB     <DskSectorNo		; save it
LDD32   RTS

LDD33   JMP     >BasFCError		; Error!
	
;
; Verify command dispatch routine.
;
; Syntax :
;	VERIFY ON
;	VERIFY OFF
;

CmdVerify   
	BEQ     LDD46			; end of command : error

	ifndef	Tandy
        CMPA    #DTokOFF		; is next token 'OFF'
	else
	CMPA    #CTokOFF		; is next token 'OFF'
	endc

        BNE     LDD3F			; no : check for 'ON'
        CLRB				; set verify off
        BRA     LDD48

LDD3F   CMPA    #DTokON			; is next token 'ON' ?
        BEQ     LDD46			; yes : skip
        JMP     >BasSNError		; return error

LDD46   LDB     #$FF			; set verify flag
LDD48   STB     DosVerifyFlag
        JMP     <BasChrGet		; go get char from basic
	
DosHookEOF   
	TST     <BasVarType		; check var type
        BEQ     LDD32			; if numeric exit
	
        CLR     <BasVarType		; set var type numeric !
        LEAS    2,S			; drop return addresss
        LDY     #DosExtDat
        LBSR    DosOpenFileExtY		; validate and open file
        BNE     JMPSysErrHook		; Error : exit
	
        LBSR    DOSGetFLen		; get file length
        BNE     JMPSysErrHook		; Error : exit
	
	LBSR    DosFCBNoToAddr		; get address of FCB
        CMPU    FCBFilePointer,X	; check to see if file pointer is same as length, so EOF
        BHI     LDD6F			; nope
        CMPA    FCBFilePointer+2,X	
        BLS     LDD71			
	
LDD6F   CLRA				; return false
	FCB	Skip2
LDD71	LDA	#$01			; return true
        LDU     <DBZero
        BRA     ReturnFP

JMPSysErrHook   
	JMP     >DosHookSysError	; Deal with errors
;
; LOC "filename" get file pointer
; 
FuncLoc
	LBSR    DosValidateAndOpenDAT	; Validate and open file
        BNE     JMPSysErrHook		; error : exit
        
	LBSR    DosFCBNoToAddr		; get address of current FCB
        LDU     FCBFilePointer,X	; get filepointer from FCB
        LDA     FCBFilePointer+2,X
        BRA     ReturnFP		; return as float in fpa0
		
;
; Function LOF "filename" get length of file.
; bug: file is left open.
; 
FuncLof 
	LBSR    DosValidateAndOpenDAT	; Validate and open file
        BNE     JMPSysErrHook			; error : exit
        
	LBSR    DOSGetFLen		; get file length
        BNE     JMPSysErrHook			; error : exit

ReturnFP   
	CLR     <BasVarType		; numeric var type
        STA     <FPA0+3			; setup as fp var

	ifdef	PGSFix
LDD95	CLRB
	NOP
	CLR     <FPA0
	STD	<FPA0+3
        LDA     #$A0			; Exponent
        STD     <FP0EXP
	else
        STU     <FPA0+1
	CLR     <FPA0
	CLR     <FPSByte
        LDA     #$A0			; Exponent
        STA     <FP0EXP
	endc
	
        JMP     >VarNormFPA0		; normalize FPA0
	
FuncFree   
	JSR     <BasChrGetCurr		; get current bas char
        BEQ     LDDAC			; none, use default drive no
        LBSR    GetDriveNoInB		; else get drive from command line
        BRA     LDDAF			; skip on

LDDAC   LDB     DosDefDriveNo		; get default drive
LDDAF   STB     <DosDriveNo		; set it as current
        LBSR    DOSGetFree		; go get free space
        BNE     JMPSysErrHook		; error : exit
        TFR     X,U			
        CLRA
        BRA     ReturnFP		; return as float in fpa0

FuncErl   
	LDD     DosErrLineNo		; get last error line
FuncRetVal   
	JMP     >VarAssign16Bit2	; return value to basic

FuncErr   
	CLRA				; msb=0
        LDB     DosErrLast		; get last error no
        BRA     FuncRetVal		; return it to basic

FuncHimem   
	LDD     <AddrFWareRamTop	; get hardware ramtop
        BRA     FuncRetVal		; return it to basic

FuncFres   
	LBSR    VarGarbageCollect	; collect garbage...defrag string space !
        LDD     <BasVarStrTop		; work out size of string space
        SUBD    <BasVarStringBase
        BRA     FuncRetVal		; return it to basic

;
; The actual core disk IO routine, accepts a function code $00..$07
; these are dispatched through this table
;

DosFunctionTable  
	FDB     DosFunctionRestore	; Restore to track 0
        FDB     DosFunctionSeek		; Seek to a track
        FDB     DosFunctionReadSec	; Read a sector
        FDB     DosFunctionWriteSec	; Write a sector
        FDB     DosFunctionWriteSec2
        FDB     DosFunctionWriteTrack	; Write (format) track
        FDB     DosFunctionReadAddr	; Read address mark
        FDB     DosFunctionReadSec2	; Read first two bytes of a sector

;
; Data table for Format ?
;
	
SectorIDTable   
	FCB     $01,$0A			; Sector layout table for format ?
        FCB     $02,$0B
        FCB     $03,$0C
        FCB     $04,$0D
        FCB     $05,$0E
        FCB     $06,$0F
        FCB     $07,$10
        FCB     $08,$11
        FCB     $09,$12
	FCB     $00
	
; Track header
TrackHeaderTable	
	FCB	$35,$4E			; 54 bytes of $4E Gap 0
	FCB	$4E			; terminator $4E
	FCB	$08,$00			; 8 bytes of $00 Preamble
	FCB	$00			; terminator $00	
	FCB	$03,$F6			; 3 bytes of $F6 Index maek IDAM
	FCB	$FC			; terminator $FC
	FCB	$1F,$4E			; 31 bytes of $4E Gap 1
	FCB	$4E			; terminator $4E

TrackHeaderSize	EQU	*-TrackHeaderTable	; size of track header table

SectorIDLayout	
	FCB	$07,$00			; 7 bytes of $00, Sector ID gap
	FCB	$00			; terminator $00
	FCB	$03,$F5			; 3 bytes of $F5, ID mark IDAM
	FCB	$FE			; terminator $FE
	
SectorIDP1Len	equ	*-SectorIDLayout	; size of first part of sector ID


SectorIDLayout2
	FCB	$01,$F7			; 1 byte of $F7, generates CRC bytes
	FCB	$4E			; terminator $4E	
	FCB	$14,$4E			; 20 bytes of $4E Gap 2
	FCB	$4E			; terminator $4E
	
; the sector data starts here	
	FCB	$0B,$00			; 11 bytes of $00
	FCB	$00			; terminator $00
	FCB	$03,$F5			; 3 bytes of $F5 Data IDAM
	FCB	$FB			; terminator $FB
	FCB	$00,$E5			; 256 bytes of $E5 sector data
	FCB	$F7			; terminator $F7, generates 2 CRC bytes
	FCB	$17,$4E			; 23 bytes of $4E
	FCB	$4E			; terminator $4E
SectorIDP2Len	equ	*-SectorIDLayout2

; this data is used at the end of the scrtor data for this track
TrackFiller
	FCB	$00,$4E			; 256 bytes of $4E
	FCB	$4E			; terminator $4E

TrackFillerLen	EQU	*-TrackFiller	; length of filler bytes	
	
; Replacement secondary interrupt vectors	
NewVectorTable   
	FCB     (L2@-L1@)		
        FDB     SecVecNMI
L1@     JMP     >NMISrv
        JMP     >IRQSrv
        JMP     >FIRQSrv
L2@

; Dos vars, step rates for drives

        FCB     (EndStep-BeginStep)	; No bytes
        FDB     DosD0StepRate		; address to copy

BeginStep
        FCB     StepRateDefault
        FCB     StepRateDefault
        FCB     StepRateDefault
        FCB     StepRateDefault
EndStep

; New basic dispatch stub
	
        FCB     (EndStub-BeginStub)	; No bytes
	FDB	BasStub1		; Copy to stub 1 ($012A) on Dragons
	
BeginStub	
        FCB     DDTokCountC		; Count of new commands	
        FDB     DosCmdNames		; Pointer to command name table
        FDB     DosCmdDispatch		; Pointer to command dispatch routine
        FCB     DDTokCountF		; Count of new functions
        FDB     DosFuncNames		; Pointer to function name table
        FDB     DosFuncDispatch		; Pointer to function dispatch table

; dummy stub to terminate

        FCB     $00
        FDB     $0000
        FDB     BasSNError
        FCB     $00
        FDB     $0000
        FDB     BasSNError
        FCB     $00,$00,$00,$00,$00
        FCB     $00,$00,$00,$00,$00		
EndStub
	FCB	$00
	
CommandDispatchTable
        FDB    CmdAuto
        FDB    CmdBackup
        FDB    CmdBeep
        FDB    CmdBootdsk
        FDB    CmdChain
        FDB    CmdCopy
        FDB    CmdCreate
        FDB    CmdDir
        FDB    CmdDrive
        FDB    CmdDskInit
        FDB    CmdFRead
        FDB    CmdFWrite
        FDB    CmdError
        FDB    CmdKill
        FDB    $D522
        FDB    CmdMerge
        FDB    CmdProtect
        FDB    CmdWait
        FDB    CmdRename
        FDB    CmdSave
        FDB    CmdSread
        FDB    CmdSwrite
        FDB    CmdVerify
        FDB    BasSNError
        FDB    CmdFLRead
        FDB    CmdSwap

FunctionDipatchTable 	
        FDB    FuncLof
        FDB    FuncFree
        FDB    FuncErl
        FDB    FuncErr
        FDB    FuncHimem
        FDB    FuncLoc
        FDB    FuncFres

RamHookTable   
	FDB     DosHookOpenDev		; open dev/file
	FDB     DosHookCheckIONum	; check io num
	FDB     DosHookRetDevParam	; return dev parameters
	FDB     DosHookCharOut		; char output	
        FDB     DosHookRetDevParam	; char input
        FDB     DosHookRetDevParam	; check dev open for input
        FDB     DosHookRetDevParam	; check dev open for output
        FDB     DosHookRetDevParam	; close all devs and files
	FDB     DosCloseAllFiles	; close single dev/file
	FDB     DosHookRetDevParam	; first char of new statement
	FDB     DosHookDiskItem		; Disk file item scanner
	FDB     DosHookRetDevParam	; poll for break
	FDB	DosHookReadInput	; read line of input
	FDB	DosHookRetDevParam	; finish loading ASCII program
	FDB	DosHookEOF		; EOF function
	FDB	DosHookRetDevParam	; Eval expression
	FDB	DosHookRetDevParam	; User error trap
	FDB	DosHookSysError		; System error trap
	FDB	DosHookRun		; run statement
        
DosFuncNames   
	FCS     /LOF/
        FCS     /FREE/
        FCS     /ERL/
        FCS     /ERR/
        FCS     /HIMEM/
        FCS     /LOC/
        FCS     /FRE$/
        
;
; New Command names table
;
	
DosCmdNames   
	FCS     /AUTO/
        FCS     /BACKUP/
        FCS     /BEEP/
        FCS     /BOOTDSK/
        FCS     /CHAIN/
        FCS     /COPY/
        FCS     /CREATE/
        FCS     /DIR/
        FCS     /DRIVE/
        FCS     /DSKINIT/
        FCS     /FREAD/
        FCS     /FWRITE/
        FCS     /ERROR/
        FCS     /KILL/
        FCS     /LOAD/
        FCS     /MERGE/
        FCS     /PROTECT/
        FCS     /WAIT/
        FCS     /RENAME/
        FCS     /SAVE/
        FCS     /SREAD/
        FCS     /SWRITE/
        FCS     /VERIFY/
        FCS     /FROM/
        FCS     /FLREAD/
        FCS     /SWAP/

;
; Some mesagaes
;
MessInsertSource
        FCC    "INSERT SOURCE"
        FCB    $0D,$00

MessInsertDest
        FCC    "INSERT DESTINATION"
        FCB    $0D,$00

MessPressAnyKey
        FCC    "PRESS ANY KEY"
        FCB    $0D,$00

DosExtBas   
	FCC    "BAS"
DosExtDat   
	FCC    "DAT"
DosExtBin   
	FCC    "BIN"
DosExtNone   
	FCC    "   "

DosErrorCodeTable
	FCC	/NR/			; $80 : not ready
	FCC	/SK/			; $82 : seek
	FCC	/WP/			; $84 : write protect
	FCC	/RT/			; $86 : record type
	FCC	/RF/			; $88 : record not found
	FCC	/CC/			; $8A : CRC
	FCC	/LD/			; $8C : Lost data
	FCC	/BT/			; $8E : boot
	FCC	/IV/			; $90 : invalid volume / directory
	FCC	/FD/			; $92 : Full directory
	FCC	/DF/			; $94 : Disk full
	FCC	/FS/			; $96 : File spec
	FCC	/PT/			; $98 : Protection
	FCC	/PE/			; $9A : (read) Past end 
	FCC	/FF/			; $9C : File not Found 
	FCC	/FE/			; $9E : File exists
	FCC	/NE/			; $A0 : (file does) Not Exist
	FCC	/TF/			; $A2 : Too many Files open
	FCC	/PR/			; $A4 : Parameter
	FCC	/??/			; $A6 : Undefined

DosSignonMess
        FCC    "DRAGONDOS 2.F"

        FCB    $0D,$00

        FCB    $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        FCB    $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        FCB    $FF,$FF,$FF,$FF,$FF,$FF
DosEnd
