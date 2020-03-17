DStubResWordsOfs		EQU	$0000		Offset of number of reserved words
DStubResLookupOfs		EQU	$0001		Offset of reserved word lookup table
DStubResJumpOfs			EQU	$0003		Offset of reserved word jump table
DStubFuncsOfs			EQU	$0005		Offset of nummber of functions
DStubFuncsLookupOfs		EQU	$0006		Offset of function lookup table
DStubFuncsJumpOfs		EQU	$0008		Offset of functions jump table

DSkip1				EQU	$0021		Skip 1 byte (BRN)
DSkip2				EQU	$008C		Skip 2 bytes (CMPX)
DSkip1LD			EQU	$0086		Skip 1 byte (LDA)
DSkip2TST			EQU	$007D		Skip 2 bytes (TST)

DCoCoVec167			EQU	$0000		Vector dest for 167 
DCoCoVect16A			EQU	$0000		Vector dest for 16A
DCoCoVect176			EQU	$0000		Vector dest for 176
DCoCoVect179			EQU	$0000		Vector dest for 179
DCoCoVect18B			EQU	$0000		Vector dest for 18B
DCoCoVect191			EQU	$0000		Vector dest for 191
DCoCoVect194			EQU	$0000		Vector Dest for 194
DCoCoVect197			EQU	$0000		Vector Dest for 197
DCoCoVect19A			EQU	$0000		Vector Dest for 19A
DCoCoVect1A3			EQU	$0000		Vector Dest for 1A3
DAddrFWareRamTop		EQU	$0027		Top of firmware RAM CLEAR xxx,yyyy set this to yyyy
DAddrRamTop			EQU	$0074		Physical end of RAM (4K, 16K, 32K or 64K).
DAddrStack			EQU	$0021		Address of top of machine stack
DBasAddrCmdDisp			EQU	$0123		Address of basic command dispatch
DBasAddrCmdList			EQU	$0121		Address of basic command list
DBasAddrDskCmdDisp		EQU	$012D		Address of disk basic command dispatch
DBasAddrDskCmdList		EQU	$012B		Address of disk basic command list
DBasAddrDskFuncDisp		EQU	$0132		Address of disk basic function dispatcher
DBasAddrDskFuncList		EQU	$0130		Address of disk basic function list
DBasAddrFuncDisp		EQU	$0128		Address of basic function dispatcher
DBasAddrFuncList		EQU	$0126		Address of basic function list
DBasAddrSigByte			EQU	$00A6		Address of current significant bit in command line
DBasAOError			EQU	$B84E		Print ?AO Error and return to basic
DBasArrayEval			EQU	$0005		Array evaluation flag, 0=eval, 1=dimensioning
DBasBootBasic			EQU	$B400		Restart basic, as if power on, also deletes current program
DBasBotStack			EQU	$0017		Bottom of stack at last check
DBasBRARun			EQU	$84DA		BRA to main loop, used by DOS
DBasBreakFlag			EQU	$0000		Break flag, +ve=stop,-ve=end
DBasBuffer			EQU	$03D7		Basic buffer space
DBasChkArrSpaceMv		EQU	$831C		Check memory space at top of arrays + move arrays
DBasChkB2Free			EQU	$8331		Check B*2 bytes free above Arrays, OM error if not
DBasChkDirect			EQU	$9C76		Check for direct mode, ID Error if so
DBasChrGet			EQU	$009F		Get next basic character routine
DBasChrGetCurr			EQU	$00A5		Get current basic ccharacter
DBasCloadMOffs			EQU	$00D3		2s complement of CLOADM offset
DBasCmdMode			EQU	$8371		Return to command mode
DBasContLine			EQU	$0029		Line no used by CONT
DBasCurrentLine			EQU	$0068		Current line no $FFFF in direct mode
DBasDelim1			EQU	$0001		First string delimiter
DBasDelim2			EQU	$0002		Second string delimiter
DBasDirectTextPtr		EQU	$002F		Direct mode text pointer
DBasDisArraySearch		EQU	$0008		Disable array search flag, 0=allow 0<>disable
DBasDNError			EQU	$B851		Print ?DN Error and return to basic
DBasDoDispatch			EQU	$84ED		Do command dispatech, X must point to dispatch table
DBasEditorLineLen		EQU	$00D7		Editor line length
DBasErrorCodeTable		EQU	$82A9		List of 2 byte error codes eg 'SN' 'OM' 'UL' etc
DBasExecAddr			EQU	$009D		Exec address, on D64, at startup points to routine to boot all ram mode
DBasFCError			EQU	$8B8D		Print ?FC Error and return to basic
DBasFindLineNo			EQU	$83FF		Find a line number in basic program
DBasFMError			EQU	$B848		Print ?FM Error and return to basic
DBasGarbageFlag			EQU	$0007		Garbage collection flag
DBasGenCount			EQU	$0003		General count/scratch var
DBasGetDevNo			EQU	$B7D4		Get dev no from line & validate
DBasGetLineNo			EQU	$869A		Get line no and store in BasTempLine
DBasGetStrFirst			EQU	$8DEA		Get first character of string into B
DBasGetStrLenAddr		EQU	$8D9A		Get string len in B and address in X of string desc in FPA2
DBasicCassBitIn			EQU	$8027		Cassette bit input
DBasicCassByIn			EQU	$8024		Cassette byte input
DBasicCassByOut			EQU	$801E		Cassette byte output
DBasicCassOff			EQU	$8018		Cassette player motor off
DBasicCassOn			EQU	$8015		Cassette player motor on
DBasicCassOnRd			EQU	$8021		Cassette on for reading
DBasicCursorB			EQU	$8009		Cursor blink
DBasicHWInit			EQU	$8000		Hardware initialisation
DBasicJoyIn			EQU	$8012		Joystick input
DBasicKbdIn			EQU	$8006		Keyboard input
DBasicPrintOut			EQU	$800F		Printer output
DBasicScreenOut			EQU	$800C		Screen output
DBasicSerIn			EQU	$802A		Read a byte from serial
DBasicSerOut			EQU	$802D		Write a byte to serial port
DBasicSetBaud			EQU	$8030		Set baud rate
DBasicSWInit			EQU	$8003		Software initialisation
DBasicWriteLead			EQU	$801B		Cassette write leader
DBasIDError			EQU	$9C7C		Print ?ID Error and return to basic
DBasIfCount			EQU	$0004		If count - how many in a line
DBasInBuffFromX			EQU	$B5D3		Read input buffer at X as basic input
DBasInputFlag			EQU	$0009		Iinput/read flag, 0=input 0<>read
DBasIOError			EQU	$B84B		Print ?IO Error and return to basic
DBasIRQVec			EQU	$9D3D		Basic IRQ routine, increments timer
DBasJoyVal0			EQU	$015A		Joystick(0) value
DBasJoyVal1			EQU	$015B		Joystick(1) value
DBasJoyVal2			EQU	$015C		Joystick(2) value
DBasJoyVal3			EQU	$015D		Joystick(3) value
DBasLineInputEntry		EQU	$9DD9		Entry into LINE INPUT routine, used by DOS
DBasLinInpBuff			EQU	$02DC		Basic line input buffer
DBasLinInpHead			EQU	$02DA		Basic line input buffer header
DBasList			EQU	$8EAA		List basic program to SysDevN A must be 0 on entry
DBasListLine			EQU	$0066		Current line during list
DBasLocateScreen		EQU	$AA87		Initialise beginning of basic after graphics screen, no of pages in A
DBasLSError			EQU	$8D6B		Print ?LS Error and return to basic
DBasNEError			EQU	$A101		Print ?NE Error and return to basic
DBasNew				EQU	$8417		Remove current basic program from meory, like NEW command
DBasNOError			EQU	$B631		Print ?NO Error and return to basic
DBasNumCmds			EQU	$0120		Number of basic commands
DBasNumDskCmds			EQU	$012A		Number of disk basic commands
DBasNumDskFuncs			EQU	$012F		Number of disk basic functions
DBasNumFuncs			EQU	$0125		Number of basic functions
DBasOldInputPtr			EQU	$002D		Pointer to saved input during a STOP
DBasOMError			EQU	$8342		Print ?OM Error and return to basic
DBasOVError			EQU	$91DB		Print ?OV Error and return to basic
DBasPollKeyboard		EQU	$851B		Basic, poll keyboard and check for break
DBasRandom8			EQU	$978E		Generate an 8 bit random number and place in BasRandomSeed+1
DBasRandomSeed			EQU	$0115		Random number seed for RND function
DBasRelateFlag			EQU	$000A		Relational operator flag
DBasRenumStart			EQU	$00D1		Renum start line no
DBasRenumStartLine		EQU	$00D5		Renum start line number
DBasRenumVal			EQU	$00CF		Renum increment value
DBasResetStack			EQU	$8434		Reset basic stack to initial position
DBasResStr			EQU	$8C52		Reserve B bytes of string space return start in X, setup low mem vars
DBasResStr2			EQU	$8CB3		Reserve B bytes of string space return start in X
DBasRndData			EQU	$00AB		Used by RND
DBasRun				EQU	$849F		Run basic program in memory, like RUN
DBasSetProgPtrX			EQU	$85EE		Sets basic program pointer to X-1
DBasSignonMess			EQU	$B4B2		Signon message address, for CoCo this is for Extended basic.
DBasSkipLineNo			EQU	$85E7		Skip past line no in basic line, UL error if no line no.
DBasSNError			EQU	$89B4		Print ?SN Error and return to basic
DBasStartProg			EQU	$0019		Start addr of basic program
DBasSTError			EQU	$8C99		Print ?OM Error and return to basic
DBasStrDescStack		EQU	$01A9		String descriptor stack
DBasStrFirstFreeTemp		EQU	$000B		First free temory string space pointer
DBasStrLastUsedTemp		EQU	$000D		Last used tempory string space pointer
DBasStrUtil			EQU	$0025		Utility string pointer
DBasStub0			EQU	$0120		Basic Stub 0 (All basic on Dragon, Colour basic on Tandy)
DBasStub1			EQU	$012A		Basic stub 1 (Disk basic on Dragon, Extended basic on Tandy)
DBasStub2			EQU	$0134		Basic Stub 2 (Null on dragon, Disk basic on Tandy)
DBasStub3			EQU	$013E		Basic Stub 3 (do not use on dragon, user stub on Tandy)
DBasTempFPA2			EQU	$0013		Tempory FPA Mantissa for FPA2
DBasTempLine			EQU	$002B		Tempory line no
DBasTempPtr			EQU	$000F		Tempory pointer
DBasTempPtr1			EQU	$0011		Tempory discriptor pointer (stack search)
DBasTempRelateFlag		EQU	$003F		Tempory relational operator flag
DBasTempVarDesc			EQU	$003B		Pointer to a tempory var descriptor
DBasTMError			EQU	$8882		Print ?TM Error and return to basic
DBasTronFlag			EQU	$00AF		Tron flag nonzero=trace on
DBasULError			EQU	$8605		Print ?UL Error and return to basic
DBasUnused1			EQU	$0076		2 unused bytes
DBasUSRTableAddr		EQU	$00B0		Address of USR address table
DBasUsrVecNoDisk		EQU	$0134		USR vector tabl when basic not installed
DBasVarArrayAddr		EQU	$001D		Start address of Array table
DBasVarAssign16			EQU	$0052		Part of FPA1, used for 16bit assigns
DBasVarDataAddr			EQU	$0033		Address of next item in data
DBasVarDataLine			EQU	$0031		Line number of current data statement
DBasVarEnd			EQU	$001F		End of storage in use by basic
DBasVarFPAcc1			EQU	$004F		Floating point acumulator 1
DBasVarFPAcc2			EQU	$005C		Floating point acumulator 2
DBasVarFPAcc3			EQU	$0040		Floating point accumulator 3 (packed)
DBasVarFPAcc4			EQU	$0045		Floating point accumulator 4 (packed)
DBasVarFPAcc5			EQU	$004A		Floating point accumulator 5 (packed)
DBasVarLastInUse		EQU	$0037		Pointer to variable last in use
DBasVarPtrLast			EQU	$0039		Poiinter to VARPTR last in use
DBasVarSimpleAddr		EQU	$001B		Start address of simple variables
DBasVarStringBase		EQU	$0021		Base address of string space (and stack)
DBasVarStrTop			EQU	$0023		Top of string space in use
DBasVarType			EQU	$0006		Variable type flag 0=numeric, $ff=string
DBasVect1			EQU	$841F		Sets up various basic vectors (after load), should be followed by call to BasVect2
DBasVect1a			EQU	$8424		Same as Vect1, but doesn't reset input pointer
DBasVect2			EQU	$83ED		Finalises setup of basic vectors (after load), should be preceeded by call to BasVect1
DBasZDError			EQU	$93B1		Print ?ZD Error and return to basic
DCasASCIIFlag			EQU	$01E3		ASCII flag byte
DCasAudioOff			EQU	$BAC3		Turn off audio from cassette
DCasAudioOn			EQU	$BAEC		Turn on Audio from cassete to speaker
DCasBitCount			EQU	$0083		Cassette bit counter
DCasBitIn			EQU	$BDA5		Reads a bity into the 'Z' flag
DCasBlockIn			EQU	$B93E		Reads a block into the cassete buffer pointed to by CasIOBuffAddr
DCasBlockLen			EQU	$007D		Cassete block length, number of bytes read, or to be written
DCasBlockOut			EQU	$B999		Write a block to cassete pointed to by CasIOBuffAddr
DCasBlockType			EQU	$007C		Cassete block type, 0=filename, 1=data, 255=EOF
DCasByteIn			EQU	$BDAD		Reads a single byte into the A register
DCasByteOut			EQU	$BE12		Write byte in A register to cassete
DCasCkSum			EQU	$0080		Used by cassette routines for calculating checksum
DCasClosFiles			EQU	$B65F		Close any open cassete file
DCasEntryAddr			EQU	$01E5		Entry address for MC programs
DCasEOFFlag			EQU	$0070		Cassette IO Flag, nonzero if EOF reached
DCasFindFile			EQU	$B8B3		Searches a tape for specified filename
DCasFName			EQU	$01D2		Cassete filename to search for or write out
DCasFNameFound			EQU	$01DA		Filename found, when reading
DCasFNameLen			EQU	$01D1		Length of cassette filename can be 0 to 8
DCasFType			EQU	$01E2		File type 0=tokenized basic, 1=ASCII data, 2=Binary
DCasGapFlag			EQU	$01E4		Gap flag byte
DCasHeadBuffAddr		EQU	$007A		Address of cassette file header
DCasIOBuff			EQU	$01DA		COS default IO buffer, if this contains filename block then folloing are valid
DCasIOBuffAddr			EQU	$007E		Cassette IO buffer address, where data will be read/written
DCasIOBuffSize			EQU	$0079		Size of cassette IO buffer
DCasIOErrorCode			EQU	$0081		Cassette IO error code 0=no error, 1=CRC, 2=attempt to load in non-ram area
DCasIOFlag			EQU	$006E		Cassette IO Flag, set to $FF when IO in progress
DCasLastSine			EQU	$0085		Casette last sine tabe entry
DCasLeadCount			EQU	$0090		Cassete leader count, number of $55 bytes in the leader
DCasLoadAddr			EQU	$01E7		Load address
DCasMax12			EQU	$0093		Cassette Upper limit of 1200Hz
DCasMax24			EQU	$0094		Cassette Upper limit of 2400Hz
DCasMotorDelay			EQU	$0095		Cassette motor on delay (also inter-block gap)
DCasMotorOff			EQU	$BDDC		Turn off cassette motor
DCasMotorOn			EQU	$BDCF		Turn on motor, and wait for delay in CasMotorDelay
DCasPartrt			EQU	$0092		Cassette 1200/2400 partition
DCasPhaseFlag			EQU	$0084		Cassette Phase flag
DCasReadBin			EQU	$B748		Read in a binary file, similar to CLOADM
DCasReadBlock1			EQU	$B933		Turns on motor, reads header and then first block into CasIOBufAddr
DCasReadLeader			EQU	$BDE7		Turn on motor and read past leader
DCasStatus			EQU	$0078		Cassette status byte, 0=cassette closed, 1=open for input, 2=open for output
DCasTemp			EQU	$0082		Cassette tempory storage
DCasWriteBasic			EQU	$B6A5		Write tokenized basic program out, similar to CSAVE
DCasWriteBin			EQU	$991B		Write a binary file out push return address, then start,end and entry addresses and then JMP to this
DCasWriteBlock1			EQU	$B991		Turn on motor, write leader and then first block
DCasWriteLeader			EQU	$801B		Turn on motor and write out leader
DCmdABS				EQU	$943E		Basic Command
DCmdAND				EQU	$8A12		Basic Command
DCmdASC				EQU	$8DE6		Basic Command
DCmdATN				EQU	$9877		Basic Command
DCmdAudio			EQU	$BADF		Basic Command
DCmdCHRS			EQU	$8DD2		Basic Command
DCmdCircle			EQU	$B238		Basic Command
DCmdClear			EQU	$8571		Basic Command
DCmdCload			EQU	$B6D5		Basic Command
DCmdClose			EQU	$B64D		Basic Command
DCmdCLS				EQU	$BA60		Basic Command
DCmdColor			EQU	$A8D4		Basic Command
DCmdCont			EQU	$8560		Basic Command
DCmdCOS				EQU	$97CB		Basic Command
DCmdCsave			EQU	$B683		Basic Command
DCmdData			EQU	$8613		Basic Command
DCmdDef				EQU	$9C81		Basic Command
DCmdDelete			EQU	$9D61		Basic Command
DCmdDim				EQU	$8A8B		Basic Command
DCmdDivide			EQU	$933C		Basic Command
DCmdDload			EQU	$A049		Basic Command
DCmdDraw			EQU	$B051		Basic Command
DCmdEdit			EQU	$9965		Basic Command
DCmdEnd				EQU	$8532		Basic Command
DCmdEOF				EQU	$B801		Basic Command
DCmdExec			EQU	$B771		Basic Command
DCmdEXP				EQU	$9713		Basic Command
DCmdExponet			EQU	$96A0		Basic Command
DCmdFIX				EQU	$9956		Basic Command
DCmdFor				EQU	$8448		Basic Command
DCmdGet				EQU	$AAF0		Basic Command
DCmdGo				EQU	$85B9		Basic Command
DCmdHexS			EQU	$A00E		Basic Command
DCmdIF				EQU	$8647		Basic Command
DCmdInkeyS			EQU	$B797		Basic Command
DCmdInput			EQU	$872B		Basic Command
DCmdInstr			EQU	$9BB4		Basic Command
DCmdINT				EQU	$9499		Basic Command
DCmdJoystk			EQU	$BB0D		Basic Command
DCmdLeftS			EQU	$8DF1		Basic Command
DCmdLEN				EQU	$8DC7		Basic Command
DCmdLet				EQU	$86BC		Basic Command
DCmdLine			EQU	$A749		Basic Command
DCmdLineInput			EQU	$9DB1		Line input command
DCmdList			EQU	$8EAA		Basic Command
DCmdLList			EQU	$8EA4		Basic Command
DCmdLOG				EQU	$923C		Basic Command
DCmdMEM				EQU	$8C31		Basic Command
DCmdMidS			EQU	$8E15		Basic Command
DCmdMinus			EQU	$9105		Basic Command
DCmdMotor			EQU	$B982		Basic Command
DCmdMultiply			EQU	$9275		Basic Command
DCmdNew				EQU	$8415		Basic Command
DCmdNext			EQU	$8829		Basic Command
DCmdON				EQU	$8675		Basic Command
DCmdOpen			EQU	$B829		Basic Command
DCmdOpenEntry			EQU	$B835		Entry into Basic open command used by Dragon/SuperDos
DCmdOR				EQU	$8A11		Basic Command
DCmdPaint			EQU	$AC87		Basic Command
DCmdPClear			EQU	$AA19		Basic Command
DCmdPCls			EQU	$A8C0		Basic Command
DCmdPcopy			EQU	$AABE		Basic Command
DCmdPeek			EQU	$8E96		Basic Command
DCmdPlay			EQU	$ADBD		Basic Command
DCmdPlus			EQU	$910E		Basic Command
DCmdPmode			EQU	$A9AF		Basic Command
DCmdPoint			EQU	$BA45		Basic Command
DCmdPoke			EQU	$8E9D		Basic Command
DCmdPOS				EQU	$9ADE		Basic Command
DCmdPPoint			EQU	$A6C7		Basic Command
DCmdPReset			EQU	$A6F3		Basic Command
DCmdPrint			EQU	$903D		Basic Command
DCmdPset			EQU	$A6EF		Basic Command
DCmdPut				EQU	$AAF3		Basic Command
DCmdRead			EQU	$8777		Basic Command
DCmdReadFromX			EQU	$877A		As basic READ command but ptr in X supplied by caller
DCmdREM				EQU	$8616		Basic Command
DCmdRenum			EQU	$9DFA		Basic Command
DCmdReset			EQU	$BA04		Basic Command
DCmdRestore			EQU	$8514		Basic Command
DCmdReturn			EQU	$85F3		Basic Command
DCmdRightS			EQU	$8E0E		Basic Command
DCmdRND				EQU	$9772		Basic Command
DCmdRun				EQU	$85A5		Basic Command
DCmdScreen			EQU	$A9FE		Basic Command
DCmdSet				EQU	$B9D3		Basic Command
DCmdSGN				EQU	$9425		Basic Command
DCmdSIN				EQU	$97D1		Basic Command
DCmdSkipf			EQU	$B81F		Basic Command
DCmdSound			EQU	$BA9B		Basic Command
DCmdSQR				EQU	$9697		Basic Command
DCmdStop			EQU	$8539		Basic Command
DCmdStringS			EQU	$9B84		Basic Command
DCmdSTRS			EQU	$8C40		Basic Command
DCmdTAN				EQU	$9816		Basic Command
DCmdTimer			EQU	$9D59		Basic Command
DCmdTroff			EQU	$9ADA		Basic Command
DCmdTron			EQU	$9AD9		Basic Command
DCmdUSR				EQU	$9D1D		Basic Command
DCmdVAL				EQU	$8E5C		Basic Command
DCmdVarptr			EQU	$9AF4		Basic Command
DGrBackground			EQU	$00B3		Current background colour
DGrBytesPerLine			EQU	$00B9		Number of byts/lin in current mode
DGrCalcPixelPos			EQU	$BA28		Calculates Lo-res pixel pos from data on stack
DGrCircleRadius			EQU	$00D0		Circle radius
DGrCircleXCo			EQU	$00CB		Circle command X
DGrCircleYCo			EQU	$00CD		Circle command Y
DGrClearGrScreen		EQU	$A8C7		Clears grapics screen to value in B
DGrColourSet			EQU	$00C1		Colour set currently in use
DGrColourTemp			EQU	$00B4		Tempory colour in use
DGrCurrColour			EQU	$00B5		Byte value for current colour, to set all pixels in byte to that colour
DGrCurrPmode			EQU	$00B6		Current PMODE number
DGrCurrX			EQU	$00BD		Current X cursor pos
DGrCurrXCo			EQU	$00C7		Current Cursor X
DGrCurrY			EQU	$00BF		Current Y cursor pos
DGrCurrYCo			EQU	$00C9		Current Cursor Y
DGrDirtyFlag			EQU	$00DB		Flag to tell if graphics screen has changed
DGrDisplayStartAddr		EQU	$00BA		Address of first byte in current display
DGrDraw				EQU	$B051		Draw on pmode screen as in DRAW command
DGrDrawAngle			EQU	$00E8		Current angle for DRAW command
DGrDrawScale			EQU	$00E9		Current scale for DRAW command
DGrForeground			EQU	$00B2		Current foreground colour
DGrLastDisplayAddr		EQU	$00B7		Address of last byte in current display
DGrPixelNoX			EQU	$00C3		Current horizontal pixel no
DGrPixelNoY			EQU	$00C5		Current vertical pixel number
DGrPlotFlag			EQU	$00C2		Plot/Unplot flag, 0=reset, nonzero=set
DGrReserveGrRam			EQU	$AA23		Reserves memory for graphics, no graphics pages in B
DGrResetLRGPixel		EQU	$BA07		ReSets lo res pixel
DGrSelectColourSet		EQU	$AA10		Selects colour set dependent on B
DGrSelectDisplay		EQU	$A938		Sets Text or Graphics screen, if Z=1 then text
DGrSelectPage			EQU	$A9E1		On entry B contains Pmode page to be used
DGrSelectVDGColSet		EQU	$A9A4		Select colour set from data in GrColourSet
DGrSetColours			EQU	$A928		Sets up colours in low memory
DGrSetLRGPixel			EQU	$B9DF		Sets lo res pixel
DGrSetResetData			EQU	$0086		Data for Lo-res set/reset
DGrSetVDGMode			EQU	$A989		Set VDG to mode in A register
DGrSetVDGOffset			EQU	$A99D		Set VDG offset to page in A
DGrStartPages			EQU	$00BC		Page number of Start of graphics pages
DIndCasBlockIn			EQU	$A006		Indirect Read cassette block
DIndCasBlockOut			EQU	$A008		Indirect Write cassete block
DIndCasOnRead			EQU	$A004		Indirect prepare cassette for read
DIndCasWriteLead		EQU	$A00C		Indirect Write cassette leader
DIndCharOutput			EQU	$A002		Indirect Character output
DIndJoystickIn			EQU	$A00A		Indirect joystick in
DIndKeyInput			EQU	$A000		Indirect keyboard input jsr()
DIndVecReset			EQU	$0072		Secondary Reset vector address, must point to NOP
DMisc16BitScratch		EQU	$008A		Misc 16 bit scratch register (always zero ??)
DPixMaskTable2Col		EQU	$A66B		Pixel mask table 2 colour mode
DPixMaskTable4Col		EQU	$A673		Pixel mask table 4 colour mode
DPrinterCRLF			EQU	$BD0A		Moves printer head to next line.
DPrinterDirOut			EQU	$BCF5		Sends character in A register to printer (uncooked)
DPrinterOut			EQU	$BD1A		Sends character in A register to printer
DSecVecFIRQ			EQU	$010F		Secondary FIRQ vector JMP+ address
DSecVecIRQ			EQU	$010C		Secondary IRQ vector JMP+ address
DSecVecNMI			EQU	$0109		Secondary NMI vector JMP+ address
DSecVecSWI			EQU	$0106		Secondary NMI vector JMP+ address
DSecVecSWI2			EQU	$0103		Secondary SWI2 vector JMP+ address
DSecVecSWI3			EQU	$0100		Secondary SWI3 vector JMP+ address
DSerDLBaud			EQU	$0000		Baud rate for DLOAD, unknown for Dragon
DSerDLTimeout			EQU	$0000		Timeourt for DLOAD, unknown for Dragon
DSndBeep			EQU	$BAA0		Play a beep duration in B, frequency in SndPitch
DSndDisable			EQU	$BAC3		Disables D/A sound output
DSndDotNoteScale		EQU	$00E5		Dotted note scale factor for Play
DSndDTOAOn			EQU	$BAED		Turn on audio to D/A converter
DSndEnable			EQU	$BAC5		Enables D/A sound output
DSndLength			EQU	$008D		Sound duration
DSndNoteLen			EQU	$00E1		Note length for PLAY
DSndOctave			EQU	$00DE		Sound octave value for PLAY
DSndPitch			EQU	$008C		Sound pitch value
DSndPlayNote			EQU	$AE9A		Plays a note from the A register (ASCII)
DSndTempo			EQU	$00E2		Tempo for PLAY
DSndTimerPlay			EQU	$00E3		Timer for the Play command
DSndVolume			EQU	$00DF		Sound volume for PLAY
DSysBoot64			EQU	$BB80		Dragon 64 only, boots basic into all ram mode, with 48K available to basic.
DSysErr				EQU	$8344		Report error code in B register, cleanup and return to basic
DSysErr2			EQU	$835E		Report error in B, do NOT hook to RAM, or turn of cas etc
DSysReadJoystick		EQU	$BD52		Read hardware joystick values & update BasJoyVal0..3
DSysReset			EQU	$B3B4		Perform soft reset, as if reset button pressed
DSysResetDA			EQU	$BAD4		Reset D/A converter to $7E
DSysSelJoystick			EQU	$BD41		Select joystick alue to read from A
DSysTimeVal			EQU	$0112		Current value of system timer
DSysWriteDA			EQU	$BAD6		Write value in A to D/A, bits 0 &1 should be 0
DTextCapsLock			EQU	$0149		Capslock flag, nonzero=uppercase
DTextClearLine			EQU	$BCA0		Clears a VDU line from current cursor pos to EOL
DTextCls			EQU	$BA77		Clear text mode screen, resets cursor to top left
DTextClsChar			EQU	$BA79		Clears srcrren to character in B register & resets cursor
DTextCursFalshCnt		EQU	$008F		Cusrsor flash counter
DTextDevN			EQU	$006F		Current device number
DTextKbdBuffAddr		EQU	$0035		Address of keyboard input buffer
DTextKbdDelay			EQU	$0097		Keyboard scan delay constant, used to debounce
DTextKbdRollover		EQU	$0150		Rollover table, to check for key releases
DTextLastKey			EQU	$0087		ASCII code of last keypress, not cleard by key release
DTextOutChar			EQU	$B54A		Outputs character in A to screen
DTextOutCRLF			EQU	$90A1		Outputs an EOL sequence to the screen
DTextOutNum16			EQU	$957A		Outputs unsigned integer in D to the TextDevN device
DTextOutNumFPA0			EQU	$9582		Outputs number in FPA0 to screen
DTextOutQuestion		EQU	$90F8		Outputs a question mark to screen
DTextOutSpace			EQU	$90F5		Outputs a space to screen
DTextOutString			EQU	$90E5		Outputs string pointed to by X to screen, X should point to byte before first byte of string
DTextPrnAutoCRLF		EQU	$0148		Printer auto EOL flag, nonzero will send EOL sequence at end of line
DTextPrnCommaW			EQU	$0099		Printer comma width
DTextPrnCurrCol			EQU	$009C		Printer current column
DTextPrnEOLCnt			EQU	$014A		Number of characters in EOL sequence 1..4
DTextPrnEOLSeq			EQU	$014B		End of line characters
DTextPrnLastComma		EQU	$009A		Printer last comma width, should be printer line width - prinnter comma width
DTextPrnLineW			EQU	$009B		Printer line width
DTextPrnSelFlag			EQU	$03FF		Dragon 64 printer selection flag, 0=paralell port, nonzero=RS232
DTextResetVDU			EQU	$A93A		Resets to text mode and screen base address of $400
DTextScanKbd			EQU	$BBE5		Scan keyboard, return Char in A, Zero flag set if no key
DTextSerBaudRate		EQU	$FF07		Serial baud rate, note on Dragon 64, this is the actual hardware baud rate reg.
DTextSerEOLDelay		EQU	$03FD		End of line delay for serial port on Dragon 64 & CoCo
DTextUpdateCurs			EQU	$BBB5		Decrements TextCursFlashCnt, if zero resets and flashes cursor
DTextVDUCommaW			EQU	$006A		VDU comma width field
DTextVDUCurrCol			EQU	$006C		Current column for VDU output
DTextVDUCursAddr		EQU	$0088		Current VDU cursor address
DTextVDULastComma		EQU	$006B		VDU last comma field, should be VDU line width - VDU comma width
DTextVDULineW			EQU	$006D		VDU line width, normally 32
DTextVDUOut			EQU	$BCAB		Outputs Char in A to VDU, does not reset screen.
DTextWaitKey			EQU	$852B		Wait for a keypress, calls TextScanKbd, also handles break
DTextWaitKeyCurs		EQU	$A0EA		Same as TextWaitKey, but with cursor
DTextWaitKeyCurs2		EQU	$B505		Same as TextWaitKey, but with cursor
DUtilCopyBXtoU			EQU	$B7CC		Copy B bytes from X to U
DVarAssign16Bit			EQU	$8C35		Assigns value in D register to a variable, and returns to basic
DVarAssign16Bit2		EQU	$8C37		Assigns value in D register to a variable, and returns to basic (1 less instruction!).
DVarAssign16BitB		EQU	$9C3E		Assigns value in BasVarAssign16 to a variable, and returns to basic
DVarAssign8Bit			EQU	$8C36		Assigns value in B register to a variable, and returns to basic
DVarCKChar			EQU	$89AC		Check for char in B register in command line, SNError if not
DVarCKClBrac			EQU	$89A4		Check for Close bracket ')' in command line, SNError if not
DVarCKComma			EQU	$89AA		Check for Comma in command line, SNError if not
DVarCKOpBrac			EQU	$89A7		Check for Open bracket '(' in command line, SNError if not
DVarDelVar			EQU	$8D9F		Frees up storage used by a variable
DVarGarbageCollect		EQU	$8CD7		Forces garbage collection in string space
DVarGet16Bit			EQU	$8E83		Returns value of variable in D,FCError if more than 16 bits
DVarGet8Bit			EQU	$8E51		Returns value of variable in B,FCError if more than 8 bits
DVarGetComma8			EQU	$8E7E		Checks for comman then gets 8 bit.
DVarGetExpr			EQU	$8877		Evaluate and put the VARPTR of experssion which follows in BasVarAssign16 (carry set)
DVarGetExprCC			EQU	$8874		Evaluate and put the VARPTR of experssion which follows in BasVarAssign16 (carry clear)
DVarGetStr			EQU	$8887		Compiles string and moves to free string space, should be followed by VarGetExpr
DVarGetUsr			EQU	$8B29		Returns argument to USRnn as a 16bit no in D
DVarGetVar			EQU	$8A94		Gets VARPTR address of following name and places in BasVarPtrLast
DVarNormFPA0			EQU	$9165		Normalize FPA0
DVectAccessScreen		EQU	$01A0		Called before CLS, GET & PUT are executed
DVectAssignStr			EQU	$019D		Called before assigning string to string variable
DVectBase			EQU	$015E		Base address of ram hooks/vectors
DVectCheckEOF			EQU	$0188		called before checking for end of file
DVectCheckKeys			EQU	$017F		Called before keyboard is scanned for BREAK,SHIFT-@
DVectCloseAllFiles		EQU	$0173		Called before closing all files
DVectCloseFile			EQU	$0176		Called before closing a file
DVectCloseFileCmd		EQU	$0185		Called before closing an ASCII file read in as basic
DVectCmdInterp			EQU	$0179		Called before interpreting a token in A
DVectDeTokenize			EQU	$01A6		Called before a line is de-tokenized
DVectDevInit			EQU	$0164		Called before initialising a device
DVectDevNo			EQU	$0161		Called when a device number is verified
DVectDevOpen			EQU	$015E		Called before a device is opened
DVectEvaluateExpr		EQU	$018B		Called before evaluating expression
DVectGetNextCmd			EQU	$019A		Called before fetching next command to be executed by BASIC
DVectInChar			EQU	$016A		Called before inputting a char to A
DVectInputFile			EQU	$016D		Called before inputting from a file
DVectLineInputFile		EQU	$0182		Called before LINE INPUT is executed
DVectOutChar			EQU	$0167		Called before outputting char in A to a device
DVectOutputFile			EQU	$0170		Called before outputting to a file
DVectReReqestIn			EQU	$017C		Called before re-requesing input from keyboard
DVectResetBasMem		EQU	$0197		Called before changing BASIC memory vectors after LOAD etc
DVectRunLink			EQU	$0194		Called when RUN about to be executed
DVectSysError			EQU	$0191		Can be patched by system to trap error messages
DVectTokenize			EQU	$01A3		Called before an ASCII line is tokenized
DVectUserError			EQU	$018E		Can be patched by user to trap error messages
DWarmStart			EQU	$B44F		Warm start routine
DWarmStartFlag			EQU	$0071		Warm start flag $55=warm start, else cold start

CStubResWordsOfs		EQU	$0000		Offset of number of reserved words
CStubResLookupOfs		EQU	$0001		Offset of reserved word lookup table
CStubResJumpOfs			EQU	$0003		Offset of reserved word jump table
CStubFuncsOfs			EQU	$0005		Offset of nummber of functions
CStubFuncsLookupOfs		EQU	$0006		Offset of function lookup table
CStubFuncsJumpOfs		EQU	$0008		Offset of functions jump table

CSkip1				EQU	$0021		Skip 1 byte (BRN)
CSkip2				EQU	$008C		Skip 2 bytes (CMPX)
CSkip1LD			EQU	$0086		Skip 1 byte (LDA)
CSkip2TST			EQU	$007D		Skip 2 bytes (TST)

CCoCoVec167			EQU	$8272		Vector dest for 167 
CCoCoVect16A			EQU	$8CF1		Vector dest for 16A
CCoCoVect176			EQU	$8286		Vector dest for 176
CCoCoVect179			EQU	$8E90		Vector dest for 179
CCoCoVect18B			EQU	$8846		Vector dest for 18B
CCoCoVect191			EQU	$88F0		Vector dest for 191
CCoCoVect194			EQU	$829C		Vector Dest for 194
CCoCoVect197			EQU	$87E5		Vector Dest for 197
CCoCoVect19A			EQU	$82B9		Vector Dest for 19A
CCoCoVect1A3			EQU	$8304		Vector Dest for 1A3
CAddrFWareRamTop		EQU	$0027		Top of firmware RAM CLEAR xxx,yyyy set this to yyyy
CAddrRamTop			EQU	$0074		Physical end of RAM (4K, 16K, 32K or 64K).
CAddrStack			EQU	$0021		Address of top of machine stack
CBasAddrCmdDisp			EQU	$0123		Address of basic command dispatch
CBasAddrCmdList			EQU	$0121		Address of basic command list
CBasAddrDskCmdDisp		EQU	$012D		Address of disk basic command dispatch
CBasAddrDskCmdList		EQU	$012B		Address of disk basic command list
CBasAddrDskFuncDisp		EQU	$0132		Address of disk basic function dispatcher
CBasAddrDskFuncList		EQU	$0130		Address of disk basic function list
CBasAddrFuncDisp		EQU	$0128		Address of basic function dispatcher
CBasAddrFuncList		EQU	$0126		Address of basic function list
CBasAddrSigByte			EQU	$00A6		Address of current significant bit in command line
CBasAOError			EQU	$A61C		Print ?AO Error and return to basic
CBasArrayEval			EQU	$0005		Array evaluation flag, 0=eval, 1=dimensioning
CBasBootBasic			EQU	$A0B6		Restart basic, as if power on, also deletes current program
CBasBotStack			EQU	$0017		Bottom of stack at last check
CBasBRARun			EQU	$ADC4		BRA to main loop, used by DOS
CBasBreakFlag			EQU	$0000		Break flag, +ve=stop,-ve=end
CBasBuffer			EQU	$03D7		Basic buffer space
CBasChkArrSpaceMv		EQU	$AC1E		Check memory space at top of arrays + move arrays
CBasChkB2Free			EQU	$AC33		Check B*2 bytes free above Arrays, OM error if not
CBasChkDirect			EQU	$8866		Check for direct mode, ID Error if so
CBasChrGet			EQU	$009F		Get next basic character routine
CBasChrGetCurr			EQU	$00A5		Get current basic ccharacter
CBasCloadMOffs			EQU	$00D3		2s complement of CLOADM offset
CBasCmdMode			EQU	$AC73		Return to command mode
CBasContLine			EQU	$0029		Line no used by CONT
CBasCurrentLine			EQU	$0068		Current line no $FFFF in direct mode
CBasDelim1			EQU	$0001		First string delimiter
CBasDelim2			EQU	$0002		Second string delimiter
CBasDirectTextPtr		EQU	$002F		Direct mode text pointer
CBasDisArraySearch		EQU	$0008		Disable array search flag, 0=allow 0<>disable
CBasDNError			EQU	$A61F		Print ?DN Error and return to basic
CBasDoDispatch			EQU	$ADD4		Do command dispatech, X must point to dispatch table
CBasEditorLineLen		EQU	$00D7		Editor line length
CBasErrorCodeTable		EQU	$ABAF		List of 2 byte error codes eg 'SN' 'OM' 'UL' etc
CBasExecAddr			EQU	$009D		Exec address, on D64, at startup points to routine to boot all ram mode
CBasFCError			EQU	$B44A		Print ?FC Error and return to basic
CBasFindLineNo			EQU	$AD01		Find a line number in basic program
CBasFMError			EQU	$A616		Print ?FM Error and return to basic
CBasGarbageFlag			EQU	$0007		Garbage collection flag
CBasGenCount			EQU	$0003		General count/scratch var
CBasGetDevNo			EQU	$A5A2		Get dev no from line & validate
CBasGetLineNo			EQU	$AF67		Get line no and store in BasTempLine
CBasGetStrFirst			EQU	$B6A4		Get first character of string into B
CBasGetStrLenAddr		EQU	$B654		Get string len in B and address in X of string desc in FPA2
CBasicCassBitIn			EQU	$A755		Cassette bit input
CBasicCassByIn			EQU	$A749		Cassette byte input
CBasicCassByOut			EQU	$A82A		Cassette byte output
CBasicCassOff			EQU	$A7EB		Cassette player motor off
CBasicCassOn			EQU	$A7CA		Cassette player motor on
CBasicCassOnRd			EQU	$A77C		Cassette on for reading
CBasicCursorB			EQU	$A199		Cursor blink
CBasicHWInit			EQU	$0000		Hardware initialisation
CBasicJoyIn			EQU	$A9DE		Joystick input
CBasicKbdIn			EQU	$A1C1		Keyboard input
CBasicPrintOut			EQU	$A2BF		Printer output
CBasicScreenOut			EQU	$A30A		Screen output
CBasicSerIn			EQU	$0000		Read a byte from serial
CBasicSerOut			EQU	$0000		Write a byte to serial port
CBasicSetBaud			EQU	$0000		Set baud rate
CBasicSWInit			EQU	$0000		Software initialisation
CBasicWriteLead			EQU	$A7D8		Cassette write leader
CBasIDError			EQU	$886C		Print ?ID Error and return to basic
CBasIfCount			EQU	$0004		If count - how many in a line
CBasInBuffFromX			EQU	$A39D		Read input buffer at X as basic input
CBasInputFlag			EQU	$0009		Iinput/read flag, 0=input 0<>read
CBasIOError			EQU	$A619		Print ?IO Error and return to basic
CBasIRQVec			EQU	$A9B3		Basic IRQ routine, increments timer
CBasJoyVal0			EQU	$015A		Joystick(0) value
CBasJoyVal1			EQU	$015B		Joystick(1) value
CBasJoyVal2			EQU	$015C		Joystick(2) value
CBasJoyVal3			EQU	$015D		Joystick(3) value
CBasLineInputEntry		EQU	$89E8		Entry into LINE INPUT routine, used by DOS
CBasLinInpBuff			EQU	$02DC		Basic line input buffer
CBasLinInpHead			EQU	$02DA		Basic line input buffer header
CBasList			EQU	$B764		List basic program to SysDevN A must be 0 on entry
CBasListLine			EQU	$0066		Current line during list
CBasLocateScreen		EQU	$96EC		Initialise beginning of basic after graphics screen, no of pages in A
CBasLSError			EQU	$B625		Print ?LS Error and return to basic
CBasNEError			EQU	$8CDD		Print ?NE Error and return to basic
CBasNew				EQU	$AD19		Remove current basic program from meory, like NEW command
CBasNOError			EQU	$A3FB		Print ?NO Error and return to basic
CBasNumCmds			EQU	$0120		Number of basic commands
CBasNumDskCmds			EQU	$012A		Number of disk basic commands
CBasNumDskFuncs			EQU	$012F		Number of disk basic functions
CBasNumFuncs			EQU	$0125		Number of basic functions
CBasOldInputPtr			EQU	$002D		Pointer to saved input during a STOP
CBasOMError			EQU	$AC44		Print ?OM Error and return to basic
CBasOVError			EQU	$BA92		Print ?OV Error and return to basic
CBasPollKeyboard		EQU	$ADEB		Basic, poll keyboard and check for break
CBasRandom8			EQU	$BF3B		Generate an 8 bit random number and place in BasRandomSeed+1
CBasRandomSeed			EQU	$0115		Random number seed for RND function
CBasRelateFlag			EQU	$000A		Relational operator flag
CBasRenumStart			EQU	$00D1		Renum start line no
CBasRenumStartLine		EQU	$00D5		Renum start line number
CBasRenumVal			EQU	$00CF		Renum increment value
CBasResetStack			EQU	$AD33		Reset basic stack to initial position
CBasResStr			EQU	$B50F		Reserve B bytes of string space return start in X, setup low mem vars
CBasResStr2			EQU	$B56D		Reserve B bytes of string space return start in X
CBasRndData			EQU	$00AB		Used by RND
CBasRun				EQU	$AD9E		Run basic program in memory, like RUN
CBasSetProgPtrX			EQU	$AEBB		Sets basic program pointer to X-1
CBasSignonMess			EQU	$80E7		Signon message address, for CoCo this is for Extended basic.
CBasSkipLineNo			EQU	$AEB4		Skip past line no in basic line, UL error if no line no.
CBasSNError			EQU	$B277		Print ?SN Error and return to basic
CBasStartProg			EQU	$0019		Start addr of basic program
CBasSTError			EQU	$B553		Print ?OM Error and return to basic
CBasStrDescStack		EQU	$01A9		String descriptor stack
CBasStrFirstFreeTemp		EQU	$000B		First free temory string space pointer
CBasStrLastUsedTemp		EQU	$000D		Last used tempory string space pointer
CBasStrUtil			EQU	$0025		Utility string pointer
CBasStub0			EQU	$0120		Basic Stub 0 (All basic on Dragon, Colour basic on Tandy)
CBasStub1			EQU	$012A		Basic stub 1 (Disk basic on Dragon, Extended basic on Tandy)
CBasStub2			EQU	$0134		Basic Stub 2 (Null on dragon, Disk basic on Tandy)
CBasStub3			EQU	$013E		Basic Stub 3 (do not use on dragon, user stub on Tandy)
CBasTempFPA2			EQU	$0013		Tempory FPA Mantissa for FPA2
CBasTempLine			EQU	$002B		Tempory line no
CBasTempPtr			EQU	$000F		Tempory pointer
CBasTempPtr1			EQU	$0011		Tempory discriptor pointer (stack search)
CBasTempRelateFlag		EQU	$003F		Tempory relational operator flag
CBasTempVarDesc			EQU	$003B		Pointer to a tempory var descriptor
CBasTMError			EQU	$B151		Print ?TM Error and return to basic
CBasTronFlag			EQU	$00AF		Tron flag nonzero=trace on
CBasULError			EQU	$AED2		Print ?UL Error and return to basic
CBasUnused1			EQU	$0076		2 unused bytes
CBasUSRTableAddr		EQU	$00B0		Address of USR address table
CBasUsrVecNoDisk		EQU	$013E		USR vector tabl when basic not installed
CBasVarArrayAddr		EQU	$001D		Start address of Array table
CBasVarAssign16			EQU	$0052		Part of FPA1, used for 16bit assigns
CBasVarDataAddr			EQU	$0033		Address of next item in data
CBasVarDataLine			EQU	$0031		Line number of current data statement
CBasVarEnd			EQU	$001F		End of storage in use by basic
CBasVarFPAcc1			EQU	$004F		Floating point acumulator 1
CBasVarFPAcc2			EQU	$005C		Floating point acumulator 2
CBasVarFPAcc3			EQU	$0040		Floating point accumulator 3 (packed)
CBasVarFPAcc4			EQU	$0045		Floating point accumulator 4 (packed)
CBasVarFPAcc5			EQU	$004A		Floating point accumulator 5 (packed)
CBasVarLastInUse		EQU	$0037		Pointer to variable last in use
CBasVarPtrLast			EQU	$0039		Poiinter to VARPTR last in use
CBasVarSimpleAddr		EQU	$001B		Start address of simple variables
CBasVarStringBase		EQU	$0021		Base address of string space (and stack)
CBasVarStrTop			EQU	$0023		Top of string space in use
CBasVarType			EQU	$0006		Variable type flag 0=numeric, $ff=string
CBasVect1			EQU	$AD21		Sets up various basic vectors (after load), should be followed by call to BasVect2
CBasVect1a			EQU	$AD26		Same as Vect1, but doesn't reset input pointer
CBasVect2			EQU	$ACEF		Finalises setup of basic vectors (after load), should be preceeded by call to BasVect1
CBasZDError			EQU	$BC06		Print ?ZD Error and return to basic
CCasASCIIFlag			EQU	$01E3		ASCII flag byte
CCasAudioOff			EQU	$A974		Turn off audio from cassette
CCasAudioOn			EQU	$A99D		Turn on Audio from cassete to speaker
CCasBitCount			EQU	$0083		Cassette bit counter
CCasBitIn			EQU	$A755		Reads a bity into the 'Z' flag
CCasBlockIn			EQU	$A70B		Reads a block into the cassete buffer pointed to by CasIOBuffAddr
CCasBlockLen			EQU	$007D		Cassete block length, number of bytes read, or to be written
CCasBlockOut			EQU	$A7F4		Write a block to cassete pointed to by CasIOBuffAddr
CCasBlockType			EQU	$007C		Cassete block type, 0=filename, 1=data, 255=EOF
CCasByteIn			EQU	$A749		Reads a single byte into the A register
CCasByteOut			EQU	$A82A		Write byte in A register to cassete
CCasCkSum			EQU	$0080		Used by cassette routines for calculating checksum
CCasClosFiles			EQU	$A429		Close any open cassete file
CCasEntryAddr			EQU	$01E5		Entry address for MC programs
CCasEOFFlag			EQU	$0070		Cassette IO Flag, nonzero if EOF reached
CCasFindFile			EQU	$A681		Searches a tape for specified filename
CCasFName			EQU	$01D2		Cassete filename to search for or write out
CCasFNameFound			EQU	$01DA		Filename found, when reading
CCasFNameLen			EQU	$01D1		Length of cassette filename can be 0 to 8
CCasFType			EQU	$01E2		File type 0=tokenized basic, 1=ASCII data, 2=Binary
CCasGapFlag			EQU	$01E4		Gap flag byte
CCasHeadBuffAddr		EQU	$007A		Address of cassette file header
CCasIOBuff			EQU	$01DA		COS default IO buffer, if this contains filename block then folloing are valid
CCasIOBuffAddr			EQU	$007E		Cassette IO buffer address, where data will be read/written
CCasIOBuffSize			EQU	$0079		Size of cassette IO buffer
CCasIOErrorCode			EQU	$0081		Cassette IO error code 0=no error, 1=CRC, 2=attempt to load in non-ram area
CCasIOFlag			EQU	$006E		Cassette IO Flag, set to $FF when IO in progress
CCasLastSine			EQU	$0085		Casette last sine tabe entry
CCasLeadCount			EQU	$0092		Cassete leader count, number of $55 bytes in the leader
CCasLoadAddr			EQU	$01E7		Load address
CCasMax12			EQU	$0091		Cassette Upper limit of 1200Hz
CCasMax24			EQU	$0092		Cassette Upper limit of 2400Hz
CCasMotorDelay			EQU	$008A		Cassette motor on delay (also inter-block gap)
CCasMotorOff			EQU	$A7EB		Turn off cassette motor
CCasMotorOn			EQU	$A7CA		Turn on motor, and wait for delay in CasMotorDelay
CCasPartrt			EQU	$008F		Cassette 1200/2400 partition
CCasPhaseFlag			EQU	$0084		Cassette Phase flag
CCasReadBin			EQU	$A511		Read in a binary file, similar to CLOADM
CCasReadBlock1			EQU	$A701		Turns on motor, reads header and then first block into CasIOBufAddr
CCasReadLeader			EQU	$A77C		Turn on motor and read past leader
CCasStatus			EQU	$0078		Cassette status byte, 0=cassette closed, 1=open for input, 2=open for output
CCasTemp			EQU	$0082		Cassette tempory storage
CCasWriteBasic			EQU	$A469		Write tokenized basic program out, similar to CSAVE
CCasWriteBin			EQU	$833D		Write a binary file out push return address, then start,end and entry addresses and then JMP to this
CCasWriteBlock1			EQU	$A7E5		Turn on motor, write leader and then first block
CCasWriteLeader			EQU	$A7D8		Turn on motor and write out leader
CCmdABS				EQU	$BC93		Basic Command
CCmdAND				EQU	$B2D5		Basic Command
CCmdASC				EQU	$B6A0		Basic Command
CCmdATN				EQU	$83B0		Basic Command
CCmdAudio			EQU	$A990		Basic Command
CCmdCHRS			EQU	$B68C		Basic Command
CCmdCircle			EQU	$9E9D		Basic Command
CCmdClear			EQU	$AE41		Basic Command
CCmdCload			EQU	$A498		Basic Command
CCmdClose			EQU	$A416		Basic Command
CCmdCLS				EQU	$A910		Basic Command
CCmdColor			EQU	$9546		Basic Command
CCmdCont			EQU	$AE30		Basic Command
CCmdCOS				EQU	$8378		Basic Command
CCmdCsave			EQU	$A44C		Basic Command
CCmdData			EQU	$AEE0		Basic Command
CCmdDef				EQU	$8871		Basic Command
CCmdDelete			EQU	$8970		Basic Command
CCmdDim				EQU	$B34E		Basic Command
CCmdDivide			EQU	$BB91		Basic Command
CCmdDload			EQU	$8C18		Basic Command
CCmdDraw			EQU	$9CB6		Basic Command
CCmdEdit			EQU	$8533		Basic Command
CCmdEnd				EQU	$AE02		Basic Command
CCmdEOF				EQU	$A5CE		Basic Command
CCmdExec			EQU	$A53E		Basic Command
CCmdEXP				EQU	$84F2		Basic Command
CCmdExponet			EQU	$011D		Basic Command
CCmdFIX				EQU	$8524		Basic Command
CCmdFor				EQU	$AD47		Basic Command
CCmdGet				EQU	$9755		Basic Command
CCmdGo				EQU	$AE86		Basic Command
CCmdHexS			EQU	$8BDD		Basic Command
CCmdIF				EQU	$AF14		Basic Command
CCmdInkeyS			EQU	$A564		Basic Command
CCmdInput			EQU	$AFF5		Basic Command
CCmdInstr			EQU	$877E		Basic Command
CCmdINT				EQU	$BCEE		Basic Command
CCmdJoystk			EQU	$A9C6		Basic Command
CCmdLeftS			EQU	$B6AB		Basic Command
CCmdLEN				EQU	$B681		Basic Command
CCmdLet				EQU	$AF89		Basic Command
CCmdLine			EQU	$93BB		Basic Command
CCmdLineInput			EQU	$89C0		Line input command
CCmdList			EQU	$B764		Basic Command
CCmdLList			EQU	$B75E		Basic Command
CCmdLOG				EQU	$8446		Basic Command
CCmdMEM				EQU	$B4EE		Basic Command
CCmdMidS			EQU	$B6CF		Basic Command
CCmdMinus			EQU	$B9BC		Basic Command
CCmdMotor			EQU	$A7BD		Basic Command
CCmdMultiply			EQU	$BACC		Basic Command
CCmdNew				EQU	$AD17		Basic Command
CCmdNext			EQU	$B0F8		Basic Command
CCmdON				EQU	$AF42		Basic Command
CCmdOpen			EQU	$A5F6		Basic Command
CCmdOpenEntry			EQU	$A603		Entry into Basic open command used by Dragon/SuperDos
CCmdOR				EQU	$B2D4		Basic Command
CCmdPaint			EQU	$98EC		Basic Command
CCmdPClear			EQU	$968B		Basic Command
CCmdPCls			EQU	$9532		Basic Command
CCmdPcopy			EQU	$9723		Basic Command
CCmdPeek			EQU	$B750		Basic Command
CCmdPlay			EQU	$9A22		Basic Command
CCmdPlus			EQU	$B9C5		Basic Command
CCmdPmode			EQU	$9621		Basic Command
CCmdPoint			EQU	$A8F5		Basic Command
CCmdPoke			EQU	$B757		Basic Command
CCmdPOS				EQU	$86AC		Basic Command
CCmdPPoint			EQU	$9339		Basic Command
CCmdPReset			EQU	$9365		Basic Command
CCmdPrint			EQU	$B8F7		Basic Command
CCmdPset			EQU	$9361		Basic Command
CCmdPut				EQU	$9758		Basic Command
CCmdRead			EQU	$B046		Basic Command
CCmdReadFromX			EQU	$B049		As basic READ command but ptr in X supplied by caller
CCmdREM				EQU	$AEE3		Basic Command
CCmdRenum			EQU	$8A09		Basic Command
CCmdReset			EQU	$A8B1		Basic Command
CCmdRestore			EQU	$ADE4		Basic Command
CCmdReturn			EQU	$AEC0		Basic Command
CCmdRightS			EQU	$B6C8		Basic Command
CCmdRND				EQU	$BF1F		Basic Command
CCmdRun				EQU	$AE75		Basic Command
CCmdScreen			EQU	$9670		Basic Command
CCmdSet				EQU	$A880		Basic Command
CCmdSGN				EQU	$BC7A		Basic Command
CCmdSIN				EQU	$BF78		Basic Command
CCmdSkipf			EQU	$A5EC		Basic Command
CCmdSound			EQU	$A94B		Basic Command
CCmdSQR				EQU	$8480		Basic Command
CCmdStop			EQU	$AE09		Basic Command
CCmdStringS			EQU	$874E		Basic Command
CCmdSTRS			EQU	$B4FD		Basic Command
CCmdTAN				EQU	$8381		Basic Command
CCmdTimer			EQU	$8968		Basic Command
CCmdTroff			EQU	$86A8		Basic Command
CCmdTron			EQU	$86A7		Basic Command
CCmdUSR				EQU	$0112		Basic Command
CCmdVAL				EQU	$B716		Basic Command
CCmdVarptr			EQU	$86BE		Basic Command
CGrBackground			EQU	$00B3		Current background colour
CGrBytesPerLine			EQU	$00B9		Number of byts/lin in current mode
CGrCalcPixelPos			EQU	$A8D9		Calculates Lo-res pixel pos from data on stack
CGrCircleRadius			EQU	$00D0		Circle radius
CGrCircleXCo			EQU	$00CB		Circle command X
CGrCircleYCo			EQU	$00CD		Circle command Y
CGrClearGrScreen		EQU	$9539		Clears grapics screen to value in B
CGrColourSet			EQU	$00C1		Colour set currently in use
CGrColourTemp			EQU	$00B4		Tempory colour in use
CGrCurrColour			EQU	$00B5		Byte value for current colour, to set all pixels in byte to that colour
CGrCurrPmode			EQU	$00B6		Current PMODE number
CGrCurrX			EQU	$00BD		Current X cursor pos
CGrCurrXCo			EQU	$00C7		Current Cursor X
CGrCurrY			EQU	$00BF		Current Y cursor pos
CGrCurrYCo			EQU	$00C9		Current Cursor Y
CGrDirtyFlag			EQU	$00DB		Flag to tell if graphics screen has changed
CGrDisplayStartAddr		EQU	$00BA		Address of first byte in current display
CGrDraw				EQU	$9CB6		Draw on pmode screen as in DRAW command
CGrDrawAngle			EQU	$00E8		Current angle for DRAW command
CGrDrawScale			EQU	$00E9		Current scale for DRAW command
CGrForeground			EQU	$00B2		Current foreground colour
CGrLastDisplayAddr		EQU	$00B7		Address of last byte in current display
CGrPixelNoX			EQU	$00C3		Current horizontal pixel no
CGrPixelNoY			EQU	$00C5		Current vertical pixel number
CGrPlotFlag			EQU	$00C2		Plot/Unplot flag, 0=reset, nonzero=set
CGrReserveGrRam			EQU	$9695		Reserves memory for graphics, no graphics pages in B
CGrResetLRGPixel		EQU	$A8B5		ReSets lo res pixel
CGrSelectColourSet		EQU	$9682		Selects colour set dependent on B
CGrSelectDisplay		EQU	$95AA		Sets Text or Graphics screen, if Z=1 then text
CGrSelectPage			EQU	$9653		On entry B contains Pmode page to be used
CGrSelectVDGColSet		EQU	$9616		Select colour set from data in GrColourSet
CGrSetColours			EQU	$959A		Sets up colours in low memory
CGrSetLRGPixel			EQU	$A88D		Sets lo res pixel
CGrSetResetData			EQU	$0086		Data for Lo-res set/reset
CGrSetVDGMode			EQU	$95FB		Set VDG to mode in A register
CGrSetVDGOffset			EQU	$960F		Set VDG offset to page in A
CGrStartPages			EQU	$00BC		Page number of Start of graphics pages
CIndCasBlockIn			EQU	$A006		Indirect Read cassette block
CIndCasBlockOut			EQU	$A008		Indirect Write cassete block
CIndCasOnRead			EQU	$A004		Indirect prepare cassette for read
CIndCasWriteLead		EQU	$A00C		Indirect Write cassette leader
CIndCharOutput			EQU	$A002		Indirect Character output
CIndJoystickIn			EQU	$A00A		Indirect joystick in
CIndKeyInput			EQU	$A000		Indirect keyboard input jsr()
CIndVecReset			EQU	$0072		Secondary Reset vector address, must point to NOP
CMisc16BitScratch		EQU	$008A		Misc 16 bit scratch register (always zero ??)
CPixMaskTable2Col		EQU	$92DD		Pixel mask table 2 colour mode
CPixMaskTable4Col		EQU	$92E5		Pixel mask table 4 colour mode
CPrinterCRLF			EQU	$0000		Moves printer head to next line.
CPrinterDirOut			EQU	$0000		Sends character in A register to printer (uncooked)
CPrinterOut			EQU	$A2BF		Sends character in A register to printer
CSecVecFIRQ			EQU	$010F		Secondary FIRQ vector JMP+ address
CSecVecIRQ			EQU	$010C		Secondary IRQ vector JMP+ address
CSecVecNMI			EQU	$0109		Secondary NMI vector JMP+ address
CSecVecSWI			EQU	$0106		Secondary NMI vector JMP+ address
CSecVecSWI2			EQU	$0103		Secondary SWI2 vector JMP+ address
CSecVecSWI3			EQU	$0100		Secondary SWI3 vector JMP+ address
CSerDLBaud			EQU	$00E6		Baud rate for DLOAD, unknown for Dragon
CSerDLTimeout			EQU	$00E7		Timeourt for DLOAD, unknown for Dragon
CSndBeep			EQU	$A951		Play a beep duration in B, frequency in SndPitch
CSndDisable			EQU	$A974		Disables D/A sound output
CSndDotNoteScale		EQU	$00E5		Dotted note scale factor for Play
CSndDTOAOn			EQU	$A99E		Turn on audio to D/A converter
CSndEnable			EQU	$A976		Enables D/A sound output
CSndLength			EQU	$008D		Sound duration
CSndNoteLen			EQU	$00E1		Note length for PLAY
CSndOctave			EQU	$00DE		Sound octave value for PLAY
CSndPitch			EQU	$008C		Sound pitch value
CSndPlayNote			EQU	$9AFF		Plays a note from the A register (ASCII)
CSndTempo			EQU	$00E2		Tempo for PLAY
CSndTimerPlay			EQU	$00E3		Timer for the Play command
CSndVolume			EQU	$00DF		Sound volume for PLAY
CSysBoot64			EQU	$0000		Dragon 64 only, boots basic into all ram mode, with 48K available to basic.
CSysErr				EQU	$AC46		Report error code in B register, cleanup and return to basic
CSysErr2			EQU	$AC60		Report error in B, do NOT hook to RAM, or turn of cas etc
CSysReadJoystick		EQU	$A9DE		Read hardware joystick values & update BasJoyVal0..3
CSysReset			EQU	$A027		Perform soft reset, as if reset button pressed
CSysResetDA			EQU	$A985		Reset D/A converter to $7E
CSysSelJoystick			EQU	$A9A2		Select joystick alue to read from A
CSysTimeVal			EQU	$0112		Current value of system timer
CSysWriteDA			EQU	$A987		Write value in A to D/A, bits 0 &1 should be 0
CTextCapsLock			EQU	$011A		Capslock flag, nonzero=uppercase
CTextClearLine			EQU	$A323		Clears a VDU line from current cursor pos to EOL
CTextCls			EQU	$A928		Clear text mode screen, resets cursor to top left
CTextClsChar			EQU	$A92A		Clears srcrren to character in B register & resets cursor
CTextCursFalshCnt		EQU	$008F		Cusrsor flash counter
CTextDevN			EQU	$006F		Current device number
CTextKbdBuffAddr		EQU	$0035		Address of keyboard input buffer
CTextKbdDelay			EQU	$011B		Keyboard scan delay constant, used to debounce
CTextKbdRollover		EQU	$0152		Rollover table, to check for key releases
CTextLastKey			EQU	$0087		ASCII code of last keypress, not cleard by key release
CTextOutChar			EQU	$A282		Outputs character in A to screen
CTextOutCRLF			EQU	$B958		Outputs an EOL sequence to the screen
CTextOutNum16			EQU	$BDCC		Outputs unsigned integer in D to the TextDevN device
CTextOutNumFPA0			EQU	$BDD4		Outputs number in FPA0 to screen
CTextOutQuestion		EQU	$B9AF		Outputs a question mark to screen
CTextOutSpace			EQU	$B9AC		Outputs a space to screen
CTextOutString			EQU	$B99C		Outputs string pointed to by X to screen, X should point to byte before first byte of string
CTextPrnAutoCRLF		EQU	$0148		Printer auto EOL flag, nonzero will send EOL sequence at end of line
CTextPrnCommaW			EQU	$0099		Printer comma width
CTextPrnCurrCol			EQU	$009C		Printer current column
CTextPrnEOLCnt			EQU	$014A		Number of characters in EOL sequence 1..4
CTextPrnEOLSeq			EQU	$014B		End of line characters
CTextPrnLastComma		EQU	$009A		Printer last comma width, should be printer line width - prinnter comma width
CTextPrnLineW			EQU	$009B		Printer line width
CTextPrnSelFlag			EQU	$03FF		Dragon 64 printer selection flag, 0=paralell port, nonzero=RS232
CTextResetVDU			EQU	$95AC		Resets to text mode and screen base address of $400
CTextScanKbd			EQU	$A1C1		Scan keyboard, return Char in A, Zero flag set if no key
CTextSerBaudRate		EQU	$0095		Serial baud rate, note on Dragon 64, this is the actual hardware baud rate reg.
CTextSerEOLDelay		EQU	$0097		End of line delay for serial port on Dragon 64 & CoCo
CTextUpdateCurs			EQU	$A199		Decrements TextCursFlashCnt, if zero resets and flashes cursor
CTextVDUCommaW			EQU	$006A		VDU comma width field
CTextVDUCurrCol			EQU	$006C		Current column for VDU output
CTextVDUCursAddr		EQU	$0088		Current VDU cursor address
CTextVDULastComma		EQU	$006B		VDU last comma field, should be VDU line width - VDU comma width
CTextVDULineW			EQU	$006D		VDU line width, normally 32
CTextVDUOut			EQU	$A30A		Outputs Char in A to VDU, does not reset screen.
CTextWaitKey			EQU	$ADFB		Wait for a keypress, calls TextScanKbd, also handles break
CTextWaitKeyCurs		EQU	$8CC6		Same as TextWaitKey, but with cursor
CTextWaitKeyCurs2		EQU	$A171		Same as TextWaitKey, but with cursor
CUtilCopyBXtoU			EQU	$A59A		Copy B bytes from X to U
CVarAssign16Bit			EQU	$B4F2		Assigns value in D register to a variable, and returns to basic
CVarAssign16Bit2		EQU	$B4F3		Assigns value in D register to a variable, and returns to basic (1 less instruction!).
CVarAssign16BitB		EQU	$880E		Assigns value in BasVarAssign16 to a variable, and returns to basic
CVarAssign8Bit			EQU	$B4F3		Assigns value in B register to a variable, and returns to basic
CVarCKChar			EQU	$B26F		Check for char in B register in command line, SNError if not
CVarCKClBrac			EQU	$B267		Check for Close bracket ')' in command line, SNError if not
CVarCKComma			EQU	$B26D		Check for Comma in command line, SNError if not
CVarCKOpBrac			EQU	$B26A		Check for Open bracket '(' in command line, SNError if not
CVarDelVar			EQU	$B659		Frees up storage used by a variable
CVarGarbageCollect		EQU	$B591		Forces garbage collection in string space
CVarGet16Bit			EQU	$B73D		Returns value of variable in D,FCError if more than 16 bits
CVarGet8Bit			EQU	$B70B		Returns value of variable in B,FCError if more than 8 bits
CVarGetComma8			EQU	$B738		Checks for comman then gets 8 bit.
CVarGetExpr			EQU	$B146		Evaluate and put the VARPTR of experssion which follows in BasVarAssign16 (carry set)
CVarGetExprCC			EQU	$B143		Evaluate and put the VARPTR of experssion which follows in BasVarAssign16 (carry clear)
CVarGetStr			EQU	$B156		Compiles string and moves to free string space, should be followed by VarGetExpr
CVarGetUsr			EQU	$B3E9		Returns argument to USRnn as a 16bit no in D
CVarGetVar			EQU	$B357		Gets VARPTR address of following name and places in BasVarPtrLast
CVarNormFPA0			EQU	$BA1C		Normalize FPA0
CVectAccessScreen		EQU	$01A0		Called before CLS, GET & PUT are executed
CVectAssignStr			EQU	$019D		Called before assigning string to string variable
CVectBase			EQU	$015E		Base address of ram hooks/vectors
CVectCheckEOF			EQU	$0188		called before checking for end of file
CVectCheckKeys			EQU	$017F		Called before keyboard is scanned for BREAK,SHIFT-@
CVectCloseAllFiles		EQU	$0173		Called before closing all files
CVectCloseFile			EQU	$0176		Called before closing a file
CVectCloseFileCmd		EQU	$0185		Called before closing an ASCII file read in as basic
CVectCmdInterp			EQU	$0179		Called before interpreting a token in A
CVectDeTokenize			EQU	$01A6		Called before a line is de-tokenized
CVectDevInit			EQU	$0164		Called before initialising a device
CVectDevNo			EQU	$0161		Called when a device number is verified
CVectDevOpen			EQU	$015E		Called before a device is opened
CVectEvaluateExpr		EQU	$018B		Called before evaluating expression
CVectGetNextCmd			EQU	$019A		Called before fetching next command to be executed by BASIC
CVectInChar			EQU	$016A		Called before inputting a char to A
CVectInputFile			EQU	$016D		Called before inputting from a file
CVectLineInputFile		EQU	$0182		Called before LINE INPUT is executed
CVectOutChar			EQU	$0167		Called before outputting char in A to a device
CVectOutputFile			EQU	$0170		Called before outputting to a file
CVectReReqestIn			EQU	$017C		Called before re-requesing input from keyboard
CVectResetBasMem		EQU	$0197		Called before changing BASIC memory vectors after LOAD etc
CVectRunLink			EQU	$0194		Called when RUN about to be executed
CVectSysError			EQU	$0191		Can be patched by system to trap error messages
CVectTokenize			EQU	$01A3		Called before an ASCII line is tokenized
CVectUserError			EQU	$018E		Can be patched by user to trap error messages
CWarmStart			EQU	$80C0		Warm start routine
CWarmStartFlag			EQU	$0071		Warm start flag $55=warm start, else cold start

			ifdef Dragon

StubResWordsOfs		EQU	DStubResWordsOfs
StubResLookupOfs		EQU	DStubResLookupOfs
StubResJumpOfs			EQU	DStubResJumpOfs
StubFuncsOfs			EQU	DStubFuncsOfs
StubFuncsLookupOfs		EQU	DStubFuncsLookupOfs
StubFuncsJumpOfs		EQU	DStubFuncsJumpOfs
Skip1				EQU	DSkip1
Skip2				EQU	DSkip2
Skip1LD				EQU	DSkip1LD
Skip2TST			EQU	DSkip2TST
CoCoVec167			EQU	DCoCoVec167
CoCoVect16A			EQU	DCoCoVect16A
CoCoVect176			EQU	DCoCoVect176
CoCoVect179			EQU	DCoCoVect179
CoCoVect18B			EQU	DCoCoVect18B
CoCoVect191			EQU	DCoCoVect191
CoCoVect194			EQU	DCoCoVect194
CoCoVect197			EQU	DCoCoVect197
CoCoVect19A			EQU	DCoCoVect19A
CoCoVect1A3			EQU	DCoCoVect1A3
AddrFWareRamTop			EQU	DAddrFWareRamTop
AddrRamTop			EQU	DAddrRamTop
AddrStack			EQU	DAddrStack
BasAddrCmdDisp			EQU	DBasAddrCmdDisp
BasAddrCmdList			EQU	DBasAddrCmdList
BasAddrDskCmdDisp		EQU	DBasAddrDskCmdDisp
BasAddrDskCmdList		EQU	DBasAddrDskCmdList
BasAddrDskFuncDisp		EQU	DBasAddrDskFuncDisp
BasAddrDskFuncList		EQU	DBasAddrDskFuncList
BasAddrFuncDisp			EQU	DBasAddrFuncDisp
BasAddrFuncList			EQU	DBasAddrFuncList
BasAddrSigByte			EQU	DBasAddrSigByte
BasAOError			EQU	DBasAOError
BasArrayEval			EQU	DBasArrayEval
BasBootBasic			EQU	DBasBootBasic
BasBotStack			EQU	DBasBotStack
BasBRARun			EQU	DBasBRARun
BasBreakFlag			EQU	DBasBreakFlag
BasBuffer			EQU	DBasBuffer
BasChkArrSpaceMv		EQU	DBasChkArrSpaceMv
BasChkB2Free			EQU	DBasChkB2Free
BasChkDirect			EQU	DBasChkDirect
BasChrGet			EQU	DBasChrGet
BasChrGetCurr			EQU	DBasChrGetCurr
BasCloadMOffs			EQU	DBasCloadMOffs
BasCmdMode			EQU	DBasCmdMode
BasContLine			EQU	DBasContLine
BasCurrentLine			EQU	DBasCurrentLine
BasDelim1			EQU	DBasDelim1
BasDelim2			EQU	DBasDelim2
BasDirectTextPtr		EQU	DBasDirectTextPtr
BasDisArraySearch		EQU	DBasDisArraySearch
BasDNError			EQU	DBasDNError
BasDoDispatch			EQU	DBasDoDispatch
BasEditorLineLen		EQU	DBasEditorLineLen
BasErrorCodeTable		EQU	DBasErrorCodeTable
BasExecAddr			EQU	DBasExecAddr
BasFCError			EQU	DBasFCError
BasFindLineNo			EQU	DBasFindLineNo
BasFMError			EQU	DBasFMError
BasGarbageFlag			EQU	DBasGarbageFlag
BasGenCount			EQU	DBasGenCount
BasGetDevNo			EQU	DBasGetDevNo
BasGetLineNo			EQU	DBasGetLineNo
BasGetStrFirst			EQU	DBasGetStrFirst
BasGetStrLenAddr		EQU	DBasGetStrLenAddr
BasicCassBitIn			EQU	DBasicCassBitIn
BasicCassByIn			EQU	DBasicCassByIn
BasicCassByOut			EQU	DBasicCassByOut
BasicCassOff			EQU	DBasicCassOff
BasicCassOn			EQU	DBasicCassOn
BasicCassOnRd			EQU	DBasicCassOnRd
BasicCursorB			EQU	DBasicCursorB
BasicHWInit			EQU	DBasicHWInit
BasicJoyIn			EQU	DBasicJoyIn
BasicKbdIn			EQU	DBasicKbdIn
BasicPrintOut			EQU	DBasicPrintOut
BasicScreenOut			EQU	DBasicScreenOut
BasicSerIn			EQU	DBasicSerIn
BasicSerOut			EQU	DBasicSerOut
BasicSetBaud			EQU	DBasicSetBaud
BasicSWInit			EQU	DBasicSWInit
BasicWriteLead			EQU	DBasicWriteLead
BasIDError			EQU	DBasIDError
BasIfCount			EQU	DBasIfCount
BasInBuffFromX			EQU	DBasInBuffFromX
BasInputFlag			EQU	DBasInputFlag
BasIOError			EQU	DBasIOError
BasIRQVec			EQU	DBasIRQVec
BasJoyVal0			EQU	DBasJoyVal0
BasJoyVal1			EQU	DBasJoyVal1
BasJoyVal2			EQU	DBasJoyVal2
BasJoyVal3			EQU	DBasJoyVal3
BasLineInputEntry		EQU	DBasLineInputEntry
BasLinInpBuff			EQU	DBasLinInpBuff
BasLinInpHead			EQU	DBasLinInpHead
BasList				EQU	DBasList
BasListLine			EQU	DBasListLine
BasLocateScreen			EQU	DBasLocateScreen
BasLSError			EQU	DBasLSError
BasNEError			EQU	DBasNEError
BasNew				EQU	DBasNew
BasNOError			EQU	DBasNOError
BasNumCmds			EQU	DBasNumCmds
BasNumDskCmds			EQU	DBasNumDskCmds
BasNumDskFuncs			EQU	DBasNumDskFuncs
BasNumFuncs			EQU	DBasNumFuncs
BasOldInputPtr			EQU	DBasOldInputPtr
BasOMError			EQU	DBasOMError
BasOVError			EQU	DBasOVError
BasPollKeyboard			EQU	DBasPollKeyboard
BasRandom8			EQU	DBasRandom8
BasRandomSeed			EQU	DBasRandomSeed
BasRelateFlag			EQU	DBasRelateFlag
BasRenumStart			EQU	DBasRenumStart
BasRenumStartLine		EQU	DBasRenumStartLine
BasRenumVal			EQU	DBasRenumVal
BasResetStack			EQU	DBasResetStack
BasResStr			EQU	DBasResStr
BasResStr2			EQU	DBasResStr2
BasRndData			EQU	DBasRndData
BasRun				EQU	DBasRun
BasSetProgPtrX			EQU	DBasSetProgPtrX
BasSignonMess			EQU	DBasSignonMess
BasSkipLineNo			EQU	DBasSkipLineNo
BasSNError			EQU	DBasSNError
BasStartProg			EQU	DBasStartProg
BasSTError			EQU	DBasSTError
BasStrDescStack			EQU	DBasStrDescStack
BasStrFirstFreeTemp		EQU	DBasStrFirstFreeTemp
BasStrLastUsedTemp		EQU	DBasStrLastUsedTemp
BasStrUtil			EQU	DBasStrUtil
BasStub0			EQU	DBasStub0
BasStub1			EQU	DBasStub1
BasStub2			EQU	DBasStub2
BasStub3			EQU	DBasStub3
BasTempFPA2			EQU	DBasTempFPA2
BasTempLine			EQU	DBasTempLine
BasTempPtr			EQU	DBasTempPtr
BasTempPtr1			EQU	DBasTempPtr1
BasTempRelateFlag		EQU	DBasTempRelateFlag
BasTempVarDesc			EQU	DBasTempVarDesc
BasTMError			EQU	DBasTMError
BasTronFlag			EQU	DBasTronFlag
BasULError			EQU	DBasULError
BasUnused1			EQU	DBasUnused1
BasUSRTableAddr			EQU	DBasUSRTableAddr
BasUsrVecNoDisk			EQU	DBasUsrVecNoDisk
BasVarArrayAddr			EQU	DBasVarArrayAddr
BasVarAssign16			EQU	DBasVarAssign16
BasVarDataAddr			EQU	DBasVarDataAddr
BasVarDataLine			EQU	DBasVarDataLine
BasVarEnd			EQU	DBasVarEnd
BasVarFPAcc1			EQU	DBasVarFPAcc1
BasVarFPAcc2			EQU	DBasVarFPAcc2
BasVarFPAcc3			EQU	DBasVarFPAcc3
BasVarFPAcc4			EQU	DBasVarFPAcc4
BasVarFPAcc5			EQU	DBasVarFPAcc5
BasVarLastInUse			EQU	DBasVarLastInUse
BasVarPtrLast			EQU	DBasVarPtrLast
BasVarSimpleAddr		EQU	DBasVarSimpleAddr
BasVarStringBase		EQU	DBasVarStringBase
BasVarStrTop			EQU	DBasVarStrTop
BasVarType			EQU	DBasVarType
BasVect1			EQU	DBasVect1
BasVect1a			EQU	DBasVect1a
BasVect2			EQU	DBasVect2
BasZDError			EQU	DBasZDError
CasASCIIFlag			EQU	DCasASCIIFlag
CasAudioOff			EQU	DCasAudioOff
CasAudioOn			EQU	DCasAudioOn
CasBitCount			EQU	DCasBitCount
CasBitIn			EQU	DCasBitIn
CasBlockIn			EQU	DCasBlockIn
CasBlockLen			EQU	DCasBlockLen
CasBlockOut			EQU	DCasBlockOut
CasBlockType			EQU	DCasBlockType
CasByteIn			EQU	DCasByteIn
CasByteOut			EQU	DCasByteOut
CasCkSum			EQU	DCasCkSum
CasClosFiles			EQU	DCasClosFiles
CasEntryAddr			EQU	DCasEntryAddr
CasEOFFlag			EQU	DCasEOFFlag
CasFindFile			EQU	DCasFindFile
CasFName			EQU	DCasFName
CasFNameFound			EQU	DCasFNameFound
CasFNameLen			EQU	DCasFNameLen
CasFType			EQU	DCasFType
CasGapFlag			EQU	DCasGapFlag
CasHeadBuffAddr			EQU	DCasHeadBuffAddr
CasIOBuff			EQU	DCasIOBuff
CasIOBuffAddr			EQU	DCasIOBuffAddr
CasIOBuffSize			EQU	DCasIOBuffSize
CasIOErrorCode			EQU	DCasIOErrorCode
CasIOFlag			EQU	DCasIOFlag
CasLastSine			EQU	DCasLastSine
CasLeadCount			EQU	DCasLeadCount
CasLoadAddr			EQU	DCasLoadAddr
CasMax12			EQU	DCasMax12
CasMax24			EQU	DCasMax24
CasMotorDelay			EQU	DCasMotorDelay
CasMotorOff			EQU	DCasMotorOff
CasMotorOn			EQU	DCasMotorOn
CasPartrt			EQU	DCasPartrt
CasPhaseFlag			EQU	DCasPhaseFlag
CasReadBin			EQU	DCasReadBin
CasReadBlock1			EQU	DCasReadBlock1
CasReadLeader			EQU	DCasReadLeader
CasStatus			EQU	DCasStatus
CasTemp				EQU	DCasTemp
CasWriteBasic			EQU	DCasWriteBasic
CasWriteBin			EQU	DCasWriteBin
CasWriteBlock1			EQU	DCasWriteBlock1
CasWriteLeader			EQU	DCasWriteLeader
CmdABS				EQU	DCmdABS
CmdAND				EQU	DCmdAND
CmdASC				EQU	DCmdASC
CmdATN				EQU	DCmdATN
CmdAudio			EQU	DCmdAudio
CmdCHRS				EQU	DCmdCHRS
CmdCircle			EQU	DCmdCircle
CmdClear			EQU	DCmdClear
CmdCload			EQU	DCmdCload
CmdClose			EQU	DCmdClose
CmdCLS				EQU	DCmdCLS
CmdColor			EQU	DCmdColor
CmdCont				EQU	DCmdCont
CmdCOS				EQU	DCmdCOS
CmdCsave			EQU	DCmdCsave
CmdData				EQU	DCmdData
CmdDef				EQU	DCmdDef
CmdDelete			EQU	DCmdDelete
CmdDim				EQU	DCmdDim
CmdDivide			EQU	DCmdDivide
CmdDload			EQU	DCmdDload
CmdDraw				EQU	DCmdDraw
CmdEdit				EQU	DCmdEdit
CmdEnd				EQU	DCmdEnd
CmdEOF				EQU	DCmdEOF
CmdExec				EQU	DCmdExec
CmdEXP				EQU	DCmdEXP
CmdExponet			EQU	DCmdExponet
CmdFIX				EQU	DCmdFIX
CmdFor				EQU	DCmdFor
CmdGet				EQU	DCmdGet
CmdGo				EQU	DCmdGo
CmdHexS				EQU	DCmdHexS
CmdIF				EQU	DCmdIF
CmdInkeyS			EQU	DCmdInkeyS
CmdInput			EQU	DCmdInput
CmdInstr			EQU	DCmdInstr
CmdINT				EQU	DCmdINT
CmdJoystk			EQU	DCmdJoystk
CmdLeftS			EQU	DCmdLeftS
CmdLEN				EQU	DCmdLEN
CmdLet				EQU	DCmdLet
CmdLine				EQU	DCmdLine
CmdLineInput			EQU	DCmdLineInput
CmdList				EQU	DCmdList
CmdLList			EQU	DCmdLList
CmdLOG				EQU	DCmdLOG
CmdMEM				EQU	DCmdMEM
CmdMidS				EQU	DCmdMidS
CmdMinus			EQU	DCmdMinus
CmdMotor			EQU	DCmdMotor
CmdMultiply			EQU	DCmdMultiply
CmdNew				EQU	DCmdNew
CmdNext				EQU	DCmdNext
CmdON				EQU	DCmdON
CmdOpen				EQU	DCmdOpen
CmdOpenEntry			EQU	DCmdOpenEntry
CmdOR				EQU	DCmdOR
CmdPaint			EQU	DCmdPaint
CmdPClear			EQU	DCmdPClear
CmdPCls				EQU	DCmdPCls
CmdPcopy			EQU	DCmdPcopy
CmdPeek				EQU	DCmdPeek
CmdPlay				EQU	DCmdPlay
CmdPlus				EQU	DCmdPlus
CmdPmode			EQU	DCmdPmode
CmdPoint			EQU	DCmdPoint
CmdPoke				EQU	DCmdPoke
CmdPOS				EQU	DCmdPOS
CmdPPoint			EQU	DCmdPPoint
CmdPReset			EQU	DCmdPReset
CmdPrint			EQU	DCmdPrint
CmdPset				EQU	DCmdPset
CmdPut				EQU	DCmdPut
CmdRead				EQU	DCmdRead
CmdReadFromX			EQU	DCmdReadFromX
CmdREM				EQU	DCmdREM
CmdRenum			EQU	DCmdRenum
CmdReset			EQU	DCmdReset
CmdRestore			EQU	DCmdRestore
CmdReturn			EQU	DCmdReturn
CmdRightS			EQU	DCmdRightS
CmdRND				EQU	DCmdRND
CmdRun				EQU	DCmdRun
CmdScreen			EQU	DCmdScreen
CmdSet				EQU	DCmdSet
CmdSGN				EQU	DCmdSGN
CmdSIN				EQU	DCmdSIN
CmdSkipf			EQU	DCmdSkipf
CmdSound			EQU	DCmdSound
CmdSQR				EQU	DCmdSQR
CmdStop				EQU	DCmdStop
CmdStringS			EQU	DCmdStringS
CmdSTRS				EQU	DCmdSTRS
CmdTAN				EQU	DCmdTAN
CmdTimer			EQU	DCmdTimer
CmdTroff			EQU	DCmdTroff
CmdTron				EQU	DCmdTron
CmdUSR				EQU	DCmdUSR
CmdVAL				EQU	DCmdVAL
CmdVarptr			EQU	DCmdVarptr
GrBackground			EQU	DGrBackground
GrBytesPerLine			EQU	DGrBytesPerLine
GrCalcPixelPos			EQU	DGrCalcPixelPos
GrCircleRadius			EQU	DGrCircleRadius
GrCircleXCo			EQU	DGrCircleXCo
GrCircleYCo			EQU	DGrCircleYCo
GrClearGrScreen			EQU	DGrClearGrScreen
GrColourSet			EQU	DGrColourSet
GrColourTemp			EQU	DGrColourTemp
GrCurrColour			EQU	DGrCurrColour
GrCurrPmode			EQU	DGrCurrPmode
GrCurrX				EQU	DGrCurrX
GrCurrXCo			EQU	DGrCurrXCo
GrCurrY				EQU	DGrCurrY
GrCurrYCo			EQU	DGrCurrYCo
GrDirtyFlag			EQU	DGrDirtyFlag
GrDisplayStartAddr		EQU	DGrDisplayStartAddr
GrDraw				EQU	DGrDraw
GrDrawAngle			EQU	DGrDrawAngle
GrDrawScale			EQU	DGrDrawScale
GrForeground			EQU	DGrForeground
GrLastDisplayAddr		EQU	DGrLastDisplayAddr
GrPixelNoX			EQU	DGrPixelNoX
GrPixelNoY			EQU	DGrPixelNoY
GrPlotFlag			EQU	DGrPlotFlag
GrReserveGrRam			EQU	DGrReserveGrRam
GrResetLRGPixel			EQU	DGrResetLRGPixel
GrSelectColourSet		EQU	DGrSelectColourSet
GrSelectDisplay			EQU	DGrSelectDisplay
GrSelectPage			EQU	DGrSelectPage
GrSelectVDGColSet		EQU	DGrSelectVDGColSet
GrSetColours			EQU	DGrSetColours
GrSetLRGPixel			EQU	DGrSetLRGPixel
GrSetResetData			EQU	DGrSetResetData
GrSetVDGMode			EQU	DGrSetVDGMode
GrSetVDGOffset			EQU	DGrSetVDGOffset
GrStartPages			EQU	DGrStartPages
IndCasBlockIn			EQU	DIndCasBlockIn
IndCasBlockOut			EQU	DIndCasBlockOut
IndCasOnRead			EQU	DIndCasOnRead
IndCasWriteLead			EQU	DIndCasWriteLead
IndCharOutput			EQU	DIndCharOutput
IndJoystickIn			EQU	DIndJoystickIn
IndKeyInput			EQU	DIndKeyInput
IndVecReset			EQU	DIndVecReset
Misc16BitScratch		EQU	DMisc16BitScratch
PixMaskTable2Col		EQU	DPixMaskTable2Col
PixMaskTable4Col		EQU	DPixMaskTable4Col
PrinterCRLF			EQU	DPrinterCRLF
PrinterDirOut			EQU	DPrinterDirOut
PrinterOut			EQU	DPrinterOut
SecVecFIRQ			EQU	DSecVecFIRQ
SecVecIRQ			EQU	DSecVecIRQ
SecVecNMI			EQU	DSecVecNMI
SecVecSWI			EQU	DSecVecSWI
SecVecSWI2			EQU	DSecVecSWI2
SecVecSWI3			EQU	DSecVecSWI3
SerDLBaud			EQU	DSerDLBaud
SerDLTimeout			EQU	DSerDLTimeout
SndBeep				EQU	DSndBeep
SndDisable			EQU	DSndDisable
SndDotNoteScale			EQU	DSndDotNoteScale
SndDTOAOn			EQU	DSndDTOAOn
SndEnable			EQU	DSndEnable
SndLength			EQU	DSndLength
SndNoteLen			EQU	DSndNoteLen
SndOctave			EQU	DSndOctave
SndPitch			EQU	DSndPitch
SndPlayNote			EQU	DSndPlayNote
SndTempo			EQU	DSndTempo
SndTimerPlay			EQU	DSndTimerPlay
SndVolume			EQU	DSndVolume
SysBoot64			EQU	DSysBoot64
SysErr				EQU	DSysErr
SysErr2				EQU	DSysErr2
SysReadJoystick			EQU	DSysReadJoystick
SysReset			EQU	DSysReset
SysResetDA			EQU	DSysResetDA
SysSelJoystick			EQU	DSysSelJoystick
SysTimeVal			EQU	DSysTimeVal
SysWriteDA			EQU	DSysWriteDA
TextCapsLock			EQU	DTextCapsLock
TextClearLine			EQU	DTextClearLine
TextCls				EQU	DTextCls
TextClsChar			EQU	DTextClsChar
TextCursFalshCnt		EQU	DTextCursFalshCnt
TextDevN			EQU	DTextDevN
TextKbdBuffAddr			EQU	DTextKbdBuffAddr
TextKbdDelay			EQU	DTextKbdDelay
TextKbdRollover			EQU	DTextKbdRollover
TextLastKey			EQU	DTextLastKey
TextOutChar			EQU	DTextOutChar
TextOutCRLF			EQU	DTextOutCRLF
TextOutNum16			EQU	DTextOutNum16
TextOutNumFPA0			EQU	DTextOutNumFPA0
TextOutQuestion			EQU	DTextOutQuestion
TextOutSpace			EQU	DTextOutSpace
TextOutString			EQU	DTextOutString
TextPrnAutoCRLF			EQU	DTextPrnAutoCRLF
TextPrnCommaW			EQU	DTextPrnCommaW
TextPrnCurrCol			EQU	DTextPrnCurrCol
TextPrnEOLCnt			EQU	DTextPrnEOLCnt
TextPrnEOLSeq			EQU	DTextPrnEOLSeq
TextPrnLastComma		EQU	DTextPrnLastComma
TextPrnLineW			EQU	DTextPrnLineW
TextPrnSelFlag			EQU	DTextPrnSelFlag
TextResetVDU			EQU	DTextResetVDU
TextScanKbd			EQU	DTextScanKbd
TextSerBaudRate			EQU	DTextSerBaudRate
TextSerEOLDelay			EQU	DTextSerEOLDelay
TextUpdateCurs			EQU	DTextUpdateCurs
TextVDUCommaW			EQU	DTextVDUCommaW
TextVDUCurrCol			EQU	DTextVDUCurrCol
TextVDUCursAddr			EQU	DTextVDUCursAddr
TextVDULastComma		EQU	DTextVDULastComma
TextVDULineW			EQU	DTextVDULineW
TextVDUOut			EQU	DTextVDUOut
TextWaitKey			EQU	DTextWaitKey
TextWaitKeyCurs			EQU	DTextWaitKeyCurs
TextWaitKeyCurs2		EQU	DTextWaitKeyCurs2
UtilCopyBXtoU			EQU	DUtilCopyBXtoU
VarAssign16Bit			EQU	DVarAssign16Bit
VarAssign16Bit2			EQU	DVarAssign16Bit2
VarAssign16BitB			EQU	DVarAssign16BitB
VarAssign8Bit			EQU	DVarAssign8Bit
VarCKChar			EQU	DVarCKChar
VarCKClBrac			EQU	DVarCKClBrac
VarCKComma			EQU	DVarCKComma
VarCKOpBrac			EQU	DVarCKOpBrac
VarDelVar			EQU	DVarDelVar
VarGarbageCollect		EQU	DVarGarbageCollect
VarGet16Bit			EQU	DVarGet16Bit
VarGet8Bit			EQU	DVarGet8Bit
VarGetComma8			EQU	DVarGetComma8
VarGetExpr			EQU	DVarGetExpr
VarGetExprCC			EQU	DVarGetExprCC
VarGetStr			EQU	DVarGetStr
VarGetUsr			EQU	DVarGetUsr
VarGetVar			EQU	DVarGetVar
VarNormFPA0			EQU	DVarNormFPA0
VectAccessScreen		EQU	DVectAccessScreen
VectAssignStr			EQU	DVectAssignStr
VectBase			EQU	DVectBase
VectCheckEOF			EQU	DVectCheckEOF
VectCheckKeys			EQU	DVectCheckKeys
VectCloseAllFiles		EQU	DVectCloseAllFiles
VectCloseFile			EQU	DVectCloseFile
VectCloseFileCmd		EQU	DVectCloseFileCmd
VectCmdInterp			EQU	DVectCmdInterp
VectDeTokenize			EQU	DVectDeTokenize
VectDevInit			EQU	DVectDevInit
VectDevNo			EQU	DVectDevNo
VectDevOpen			EQU	DVectDevOpen
VectEvaluateExpr		EQU	DVectEvaluateExpr
VectGetNextCmd			EQU	DVectGetNextCmd
VectInChar			EQU	DVectInChar
VectInputFile			EQU	DVectInputFile
VectLineInputFile		EQU	DVectLineInputFile
VectOutChar			EQU	DVectOutChar
VectOutputFile			EQU	DVectOutputFile
VectReReqestIn			EQU	DVectReReqestIn
VectResetBasMem			EQU	DVectResetBasMem
VectRunLink			EQU	DVectRunLink
VectSysError			EQU	DVectSysError
VectTokenize			EQU	DVectTokenize
VectUserError			EQU	DVectUserError
WarmStart			EQU	DWarmStart
WarmStartFlag			EQU	DWarmStartFlag

			ENDC

			ifdef Tandy

StubResWordsOfs			EQU	CStubResWordsOfs
StubResLookupOfs		EQU	CStubResLookupOfs
StubResJumpOfs			EQU	CStubResJumpOfs
StubFuncsOfs			EQU	CStubFuncsOfs
StubFuncsLookupOfs		EQU	CStubFuncsLookupOfs
StubFuncsJumpOfs		EQU	CStubFuncsJumpOfs
Skip1				EQU	CSkip1
Skip2				EQU	CSkip2
Skip1LD				EQU	CSkip1LD
Skip2TST			EQU	CSkip2TST
CoCoVec167			EQU	CCoCoVec167
CoCoVect16A			EQU	CCoCoVect16A
CoCoVect176			EQU	CCoCoVect176
CoCoVect179			EQU	CCoCoVect179
CoCoVect18B			EQU	CCoCoVect18B
CoCoVect191			EQU	CCoCoVect191
CoCoVect194			EQU	CCoCoVect194
CoCoVect197			EQU	CCoCoVect197
CoCoVect19A			EQU	CCoCoVect19A
CoCoVect1A3			EQU	CCoCoVect1A3
AddrFWareRamTop			EQU	CAddrFWareRamTop
AddrRamTop			EQU	CAddrRamTop
AddrStack			EQU	CAddrStack
BasAddrCmdDisp			EQU	CBasAddrCmdDisp
BasAddrCmdList			EQU	CBasAddrCmdList
BasAddrDskCmdDisp		EQU	CBasAddrDskCmdDisp
BasAddrDskCmdList		EQU	CBasAddrDskCmdList
BasAddrDskFuncDisp		EQU	CBasAddrDskFuncDisp
BasAddrDskFuncList		EQU	CBasAddrDskFuncList
BasAddrFuncDisp			EQU	CBasAddrFuncDisp
BasAddrFuncList			EQU	CBasAddrFuncList
BasAddrSigByte			EQU	CBasAddrSigByte
BasAOError			EQU	CBasAOError
BasArrayEval			EQU	CBasArrayEval
BasBootBasic			EQU	CBasBootBasic
BasBotStack			EQU	CBasBotStack
BasBRARun			EQU	CBasBRARun
BasBreakFlag			EQU	CBasBreakFlag
BasBuffer			EQU	CBasBuffer
BasChkArrSpaceMv		EQU	CBasChkArrSpaceMv
BasChkB2Free			EQU	CBasChkB2Free
BasChkDirect			EQU	CBasChkDirect
BasChrGet			EQU	CBasChrGet
BasChrGetCurr			EQU	CBasChrGetCurr
BasCloadMOffs			EQU	CBasCloadMOffs
BasCmdMode			EQU	CBasCmdMode
BasContLine			EQU	CBasContLine
BasCurrentLine			EQU	CBasCurrentLine
BasDelim1			EQU	CBasDelim1
BasDelim2			EQU	CBasDelim2
BasDirectTextPtr		EQU	CBasDirectTextPtr
BasDisArraySearch		EQU	CBasDisArraySearch
BasDNError			EQU	CBasDNError
BasDoDispatch			EQU	CBasDoDispatch
BasEditorLineLen		EQU	CBasEditorLineLen
BasErrorCodeTable		EQU	CBasErrorCodeTable
BasExecAddr			EQU	CBasExecAddr
BasFCError			EQU	CBasFCError
BasFindLineNo			EQU	CBasFindLineNo
BasFMError			EQU	CBasFMError
BasGarbageFlag			EQU	CBasGarbageFlag
BasGenCount			EQU	CBasGenCount
BasGetDevNo			EQU	CBasGetDevNo
BasGetLineNo			EQU	CBasGetLineNo
BasGetStrFirst			EQU	CBasGetStrFirst
BasGetStrLenAddr		EQU	CBasGetStrLenAddr
BasicCassBitIn			EQU	CBasicCassBitIn
BasicCassByIn			EQU	CBasicCassByIn
BasicCassByOut			EQU	CBasicCassByOut
BasicCassOff			EQU	CBasicCassOff
BasicCassOn			EQU	CBasicCassOn
BasicCassOnRd			EQU	CBasicCassOnRd
BasicCursorB			EQU	CBasicCursorB
BasicHWInit			EQU	CBasicHWInit
BasicJoyIn			EQU	CBasicJoyIn
BasicKbdIn			EQU	CBasicKbdIn
BasicPrintOut			EQU	CBasicPrintOut
BasicScreenOut			EQU	CBasicScreenOut
BasicSerIn			EQU	CBasicSerIn
BasicSerOut			EQU	CBasicSerOut
BasicSetBaud			EQU	CBasicSetBaud
BasicSWInit			EQU	CBasicSWInit
BasicWriteLead			EQU	CBasicWriteLead
BasIDError			EQU	CBasIDError
BasIfCount			EQU	CBasIfCount
BasInBuffFromX			EQU	CBasInBuffFromX
BasInputFlag			EQU	CBasInputFlag
BasIOError			EQU	CBasIOError
BasIRQVec			EQU	CBasIRQVec
BasJoyVal0			EQU	CBasJoyVal0
BasJoyVal1			EQU	CBasJoyVal1
BasJoyVal2			EQU	CBasJoyVal2
BasJoyVal3			EQU	CBasJoyVal3
BasLineInputEntry		EQU	CBasLineInputEntry
BasLinInpBuff			EQU	CBasLinInpBuff
BasLinInpHead			EQU	CBasLinInpHead
BasList				EQU	CBasList
BasListLine			EQU	CBasListLine
BasLocateScreen			EQU	CBasLocateScreen
BasLSError			EQU	CBasLSError
BasNEError			EQU	CBasNEError
BasNew				EQU	CBasNew
BasNOError			EQU	CBasNOError
BasNumCmds			EQU	CBasNumCmds
BasNumDskCmds			EQU	CBasNumDskCmds
BasNumDskFuncs			EQU	CBasNumDskFuncs
BasNumFuncs			EQU	CBasNumFuncs
BasOldInputPtr			EQU	CBasOldInputPtr
BasOMError			EQU	CBasOMError
BasOVError			EQU	CBasOVError
BasPollKeyboard			EQU	CBasPollKeyboard
BasRandom8			EQU	CBasRandom8
BasRandomSeed			EQU	CBasRandomSeed
BasRelateFlag			EQU	CBasRelateFlag
BasRenumStart			EQU	CBasRenumStart
BasRenumStartLine		EQU	CBasRenumStartLine
BasRenumVal			EQU	CBasRenumVal
BasResetStack			EQU	CBasResetStack
BasResStr			EQU	CBasResStr
BasResStr2			EQU	CBasResStr2
BasRndData			EQU	CBasRndData
BasRun				EQU	CBasRun
BasSetProgPtrX			EQU	CBasSetProgPtrX
BasSignonMess			EQU	CBasSignonMess
BasSkipLineNo			EQU	CBasSkipLineNo
BasSNError			EQU	CBasSNError
BasStartProg			EQU	CBasStartProg
BasSTError			EQU	CBasSTError
BasStrDescStack			EQU	CBasStrDescStack
BasStrFirstFreeTemp		EQU	CBasStrFirstFreeTemp
BasStrLastUsedTemp		EQU	CBasStrLastUsedTemp
BasStrUtil			EQU	CBasStrUtil
BasStub0			EQU	CBasStub0
BasStub1			EQU	CBasStub1
BasStub2			EQU	CBasStub2
BasStub3			EQU	CBasStub3
BasTempFPA2			EQU	CBasTempFPA2
BasTempLine			EQU	CBasTempLine
BasTempPtr			EQU	CBasTempPtr
BasTempPtr1			EQU	CBasTempPtr1
BasTempRelateFlag		EQU	CBasTempRelateFlag
BasTempVarDesc			EQU	CBasTempVarDesc
BasTMError			EQU	CBasTMError
BasTronFlag			EQU	CBasTronFlag
BasULError			EQU	CBasULError
BasUnused1			EQU	CBasUnused1
BasUSRTableAddr			EQU	CBasUSRTableAddr
BasUsrVecNoDisk			EQU	CBasUsrVecNoDisk
BasVarArrayAddr			EQU	CBasVarArrayAddr
BasVarAssign16			EQU	CBasVarAssign16
BasVarDataAddr			EQU	CBasVarDataAddr
BasVarDataLine			EQU	CBasVarDataLine
BasVarEnd			EQU	CBasVarEnd
BasVarFPAcc1			EQU	CBasVarFPAcc1
BasVarFPAcc2			EQU	CBasVarFPAcc2
BasVarFPAcc3			EQU	CBasVarFPAcc3
BasVarFPAcc4			EQU	CBasVarFPAcc4
BasVarFPAcc5			EQU	CBasVarFPAcc5
BasVarLastInUse			EQU	CBasVarLastInUse
BasVarPtrLast			EQU	CBasVarPtrLast
BasVarSimpleAddr		EQU	CBasVarSimpleAddr
BasVarStringBase		EQU	CBasVarStringBase
BasVarStrTop			EQU	CBasVarStrTop
BasVarType			EQU	CBasVarType
BasVect1			EQU	CBasVect1
BasVect1a			EQU	CBasVect1a
BasVect2			EQU	CBasVect2
BasZDError			EQU	CBasZDError
CasASCIIFlag			EQU	CCasASCIIFlag
CasAudioOff			EQU	CCasAudioOff
CasAudioOn			EQU	CCasAudioOn
CasBitCount			EQU	CCasBitCount
CasBitIn			EQU	CCasBitIn
CasBlockIn			EQU	CCasBlockIn
CasBlockLen			EQU	CCasBlockLen
CasBlockOut			EQU	CCasBlockOut
CasBlockType			EQU	CCasBlockType
CasByteIn			EQU	CCasByteIn
CasByteOut			EQU	CCasByteOut
CasCkSum			EQU	CCasCkSum
CasClosFiles			EQU	CCasClosFiles
CasEntryAddr			EQU	CCasEntryAddr
CasEOFFlag			EQU	CCasEOFFlag
CasFindFile			EQU	CCasFindFile
CasFName			EQU	CCasFName
CasFNameFound			EQU	CCasFNameFound
CasFNameLen			EQU	CCasFNameLen
CasFType			EQU	CCasFType
CasGapFlag			EQU	CCasGapFlag
CasHeadBuffAddr			EQU	CCasHeadBuffAddr
CasIOBuff			EQU	CCasIOBuff
CasIOBuffAddr			EQU	CCasIOBuffAddr
CasIOBuffSize			EQU	CCasIOBuffSize
CasIOErrorCode			EQU	CCasIOErrorCode
CasIOFlag			EQU	CCasIOFlag
CasLastSine			EQU	CCasLastSine
CasLeadCount			EQU	CCasLeadCount
CasLoadAddr			EQU	CCasLoadAddr
CasMax12			EQU	CCasMax12
CasMax24			EQU	CCasMax24
CasMotorDelay			EQU	CCasMotorDelay
CasMotorOff			EQU	CCasMotorOff
CasMotorOn			EQU	CCasMotorOn
CasPartrt			EQU	CCasPartrt
CasPhaseFlag			EQU	CCasPhaseFlag
CasReadBin			EQU	CCasReadBin
CasReadBlock1			EQU	CCasReadBlock1
CasReadLeader			EQU	CCasReadLeader
CasStatus			EQU	CCasStatus
CasTemp				EQU	CCasTemp
CasWriteBasic			EQU	CCasWriteBasic
CasWriteBin			EQU	CCasWriteBin
CasWriteBlock1			EQU	CCasWriteBlock1
CasWriteLeader			EQU	CCasWriteLeader
CmdABS				EQU	CCmdABS
CmdAND				EQU	CCmdAND
CmdASC				EQU	CCmdASC
CmdATN				EQU	CCmdATN
CmdAudio			EQU	CCmdAudio
CmdCHRS				EQU	CCmdCHRS
CmdCircle			EQU	CCmdCircle
CmdClear			EQU	CCmdClear
CmdCload			EQU	CCmdCload
CmdClose			EQU	CCmdClose
CmdCLS				EQU	CCmdCLS
CmdColor			EQU	CCmdColor
CmdCont				EQU	CCmdCont
CmdCOS				EQU	CCmdCOS
CmdCsave			EQU	CCmdCsave
CmdData				EQU	CCmdData
CmdDef				EQU	CCmdDef
CmdDelete			EQU	CCmdDelete
CmdDim				EQU	CCmdDim
CmdDivide			EQU	CCmdDivide
CmdDload			EQU	CCmdDload
CmdDraw				EQU	CCmdDraw
CmdEdit				EQU	CCmdEdit
CmdEnd				EQU	CCmdEnd
CmdEOF				EQU	CCmdEOF
CmdExec				EQU	CCmdExec
CmdEXP				EQU	CCmdEXP
CmdExponet			EQU	CCmdExponet
CmdFIX				EQU	CCmdFIX
CmdFor				EQU	CCmdFor
CmdGet				EQU	CCmdGet
CmdGo				EQU	CCmdGo
CmdHexS				EQU	CCmdHexS
CmdIF				EQU	CCmdIF
CmdInkeyS			EQU	CCmdInkeyS
CmdInput			EQU	CCmdInput
CmdInstr			EQU	CCmdInstr
CmdINT				EQU	CCmdINT
CmdJoystk			EQU	CCmdJoystk
CmdLeftS			EQU	CCmdLeftS
CmdLEN				EQU	CCmdLEN
CmdLet				EQU	CCmdLet
CmdLine				EQU	CCmdLine
CmdLineInput			EQU	CCmdLineInput
CmdList				EQU	CCmdList
CmdLList			EQU	CCmdLList
CmdLOG				EQU	CCmdLOG
CmdMEM				EQU	CCmdMEM
CmdMidS				EQU	CCmdMidS
CmdMinus			EQU	CCmdMinus
CmdMotor			EQU	CCmdMotor
CmdMultiply			EQU	CCmdMultiply
CmdNew				EQU	CCmdNew
CmdNext				EQU	CCmdNext
CmdON				EQU	CCmdON
CmdOpen				EQU	CCmdOpen
CmdOpenEntry			EQU	CCmdOpenEntry
CmdOR				EQU	CCmdOR
CmdPaint			EQU	CCmdPaint
CmdPClear			EQU	CCmdPClear
CmdPCls				EQU	CCmdPCls
CmdPcopy			EQU	CCmdPcopy
CmdPeek				EQU	CCmdPeek
CmdPlay				EQU	CCmdPlay
CmdPlus				EQU	CCmdPlus
CmdPmode			EQU	CCmdPmode
CmdPoint			EQU	CCmdPoint
CmdPoke				EQU	CCmdPoke
CmdPOS				EQU	CCmdPOS
CmdPPoint			EQU	CCmdPPoint
CmdPReset			EQU	CCmdPReset
CmdPrint			EQU	CCmdPrint
CmdPset				EQU	CCmdPset
CmdPut				EQU	CCmdPut
CmdRead				EQU	CCmdRead
CmdReadFromX			EQU	CCmdReadFromX
CmdREM				EQU	CCmdREM
CmdRenum			EQU	CCmdRenum
CmdReset			EQU	CCmdReset
CmdRestore			EQU	CCmdRestore
CmdReturn			EQU	CCmdReturn
CmdRightS			EQU	CCmdRightS
CmdRND				EQU	CCmdRND
CmdRun				EQU	CCmdRun
CmdScreen			EQU	CCmdScreen
CmdSet				EQU	CCmdSet
CmdSGN				EQU	CCmdSGN
CmdSIN				EQU	CCmdSIN
CmdSkipf			EQU	CCmdSkipf
CmdSound			EQU	CCmdSound
CmdSQR				EQU	CCmdSQR
CmdStop				EQU	CCmdStop
CmdStringS			EQU	CCmdStringS
CmdSTRS				EQU	CCmdSTRS
CmdTAN				EQU	CCmdTAN
CmdTimer			EQU	CCmdTimer
CmdTroff			EQU	CCmdTroff
CmdTron				EQU	CCmdTron
CmdUSR				EQU	CCmdUSR
CmdVAL				EQU	CCmdVAL
CmdVarptr			EQU	CCmdVarptr
GrBackground			EQU	CGrBackground
GrBytesPerLine			EQU	CGrBytesPerLine
GrCalcPixelPos			EQU	CGrCalcPixelPos
GrCircleRadius			EQU	CGrCircleRadius
GrCircleXCo			EQU	CGrCircleXCo
GrCircleYCo			EQU	CGrCircleYCo
GrClearGrScreen			EQU	CGrClearGrScreen
GrColourSet			EQU	CGrColourSet
GrColourTemp			EQU	CGrColourTemp
GrCurrColour			EQU	CGrCurrColour
GrCurrPmode			EQU	CGrCurrPmode
GrCurrX				EQU	CGrCurrX
GrCurrXCo			EQU	CGrCurrXCo
GrCurrY				EQU	CGrCurrY
GrCurrYCo			EQU	CGrCurrYCo
GrDirtyFlag			EQU	CGrDirtyFlag
GrDisplayStartAddr		EQU	CGrDisplayStartAddr
GrDraw				EQU	CGrDraw
GrDrawAngle			EQU	CGrDrawAngle
GrDrawScale			EQU	CGrDrawScale
GrForeground			EQU	CGrForeground
GrLastDisplayAddr		EQU	CGrLastDisplayAddr
GrPixelNoX			EQU	CGrPixelNoX
GrPixelNoY			EQU	CGrPixelNoY
GrPlotFlag			EQU	CGrPlotFlag
GrReserveGrRam			EQU	CGrReserveGrRam
GrResetLRGPixel			EQU	CGrResetLRGPixel
GrSelectColourSet		EQU	CGrSelectColourSet
GrSelectDisplay			EQU	CGrSelectDisplay
GrSelectPage			EQU	CGrSelectPage
GrSelectVDGColSet		EQU	CGrSelectVDGColSet
GrSetColours			EQU	CGrSetColours
GrSetLRGPixel			EQU	CGrSetLRGPixel
GrSetResetData			EQU	CGrSetResetData
GrSetVDGMode			EQU	CGrSetVDGMode
GrSetVDGOffset			EQU	CGrSetVDGOffset
GrStartPages			EQU	CGrStartPages
IndCasBlockIn			EQU	CIndCasBlockIn
IndCasBlockOut			EQU	CIndCasBlockOut
IndCasOnRead			EQU	CIndCasOnRead
IndCasWriteLead			EQU	CIndCasWriteLead
IndCharOutput			EQU	CIndCharOutput
IndJoystickIn			EQU	CIndJoystickIn
IndKeyInput			EQU	CIndKeyInput
IndVecReset			EQU	CIndVecReset
Misc16BitScratch		EQU	CMisc16BitScratch
PixMaskTable2Col		EQU	CPixMaskTable2Col
PixMaskTable4Col		EQU	CPixMaskTable4Col
PrinterCRLF			EQU	CPrinterCRLF
PrinterDirOut			EQU	CPrinterDirOut
PrinterOut			EQU	CPrinterOut
SecVecFIRQ			EQU	CSecVecFIRQ
SecVecIRQ			EQU	CSecVecIRQ
SecVecNMI			EQU	CSecVecNMI
SecVecSWI			EQU	CSecVecSWI
SecVecSWI2			EQU	CSecVecSWI2
SecVecSWI3			EQU	CSecVecSWI3
SerDLBaud			EQU	CSerDLBaud
SerDLTimeout			EQU	CSerDLTimeout
SndBeep				EQU	CSndBeep
SndDisable			EQU	CSndDisable
SndDotNoteScale			EQU	CSndDotNoteScale
SndDTOAOn			EQU	CSndDTOAOn
SndEnable			EQU	CSndEnable
SndLength			EQU	CSndLength
SndNoteLen			EQU	CSndNoteLen
SndOctave			EQU	CSndOctave
SndPitch			EQU	CSndPitch
SndPlayNote			EQU	CSndPlayNote
SndTempo			EQU	CSndTempo
SndTimerPlay			EQU	CSndTimerPlay
SndVolume			EQU	CSndVolume
SysBoot64			EQU	CSysBoot64
SysErr				EQU	CSysErr
SysErr2				EQU	CSysErr2
SysReadJoystick			EQU	CSysReadJoystick
SysReset			EQU	CSysReset
SysResetDA			EQU	CSysResetDA
SysSelJoystick			EQU	CSysSelJoystick
SysTimeVal			EQU	CSysTimeVal
SysWriteDA			EQU	CSysWriteDA
TextCapsLock			EQU	CTextCapsLock
TextClearLine			EQU	CTextClearLine
TextCls				EQU	CTextCls
TextClsChar			EQU	CTextClsChar
TextCursFalshCnt		EQU	CTextCursFalshCnt
TextDevN			EQU	CTextDevN
TextKbdBuffAddr			EQU	CTextKbdBuffAddr
TextKbdDelay			EQU	CTextKbdDelay
TextKbdRollover			EQU	CTextKbdRollover
TextLastKey			EQU	CTextLastKey
TextOutChar			EQU	CTextOutChar
TextOutCRLF			EQU	CTextOutCRLF
TextOutNum16			EQU	CTextOutNum16
TextOutNumFPA0			EQU	CTextOutNumFPA0
TextOutQuestion			EQU	CTextOutQuestion
TextOutSpace			EQU	CTextOutSpace
TextOutString			EQU	CTextOutString
TextPrnAutoCRLF			EQU	CTextPrnAutoCRLF
TextPrnCommaW			EQU	CTextPrnCommaW
TextPrnCurrCol			EQU	CTextPrnCurrCol
TextPrnEOLCnt			EQU	CTextPrnEOLCnt
TextPrnEOLSeq			EQU	CTextPrnEOLSeq
TextPrnLastComma		EQU	CTextPrnLastComma
TextPrnLineW			EQU	CTextPrnLineW
TextPrnSelFlag			EQU	CTextPrnSelFlag
TextResetVDU			EQU	CTextResetVDU
TextScanKbd			EQU	CTextScanKbd
TextSerBaudRate			EQU	CTextSerBaudRate
TextSerEOLDelay			EQU	CTextSerEOLDelay
TextUpdateCurs			EQU	CTextUpdateCurs
TextVDUCommaW			EQU	CTextVDUCommaW
TextVDUCurrCol			EQU	CTextVDUCurrCol
TextVDUCursAddr			EQU	CTextVDUCursAddr
TextVDULastComma		EQU	CTextVDULastComma
TextVDULineW			EQU	CTextVDULineW
TextVDUOut			EQU	CTextVDUOut
TextWaitKey			EQU	CTextWaitKey
TextWaitKeyCurs			EQU	CTextWaitKeyCurs
TextWaitKeyCurs2		EQU	CTextWaitKeyCurs2
UtilCopyBXtoU			EQU	CUtilCopyBXtoU
VarAssign16Bit			EQU	CVarAssign16Bit
VarAssign16Bit2			EQU	CVarAssign16Bit2
VarAssign16BitB			EQU	CVarAssign16BitB
VarAssign8Bit			EQU	CVarAssign8Bit
VarCKChar			EQU	CVarCKChar
VarCKClBrac			EQU	CVarCKClBrac
VarCKComma			EQU	CVarCKComma
VarCKOpBrac			EQU	CVarCKOpBrac
VarDelVar			EQU	CVarDelVar
VarGarbageCollect		EQU	CVarGarbageCollect
VarGet16Bit			EQU	CVarGet16Bit
VarGet8Bit			EQU	CVarGet8Bit
VarGetComma8			EQU	CVarGetComma8
VarGetExpr			EQU	CVarGetExpr
VarGetExprCC			EQU	CVarGetExprCC
VarGetStr			EQU	CVarGetStr
VarGetUsr			EQU	CVarGetUsr
VarGetVar			EQU	CVarGetVar
VarNormFPA0			EQU	CVarNormFPA0
VectAccessScreen		EQU	CVectAccessScreen
VectAssignStr			EQU	CVectAssignStr
VectBase			EQU	CVectBase
VectCheckEOF			EQU	CVectCheckEOF
VectCheckKeys			EQU	CVectCheckKeys
VectCloseAllFiles		EQU	CVectCloseAllFiles
VectCloseFile			EQU	CVectCloseFile
VectCloseFileCmd		EQU	CVectCloseFileCmd
VectCmdInterp			EQU	CVectCmdInterp
VectDeTokenize			EQU	CVectDeTokenize
VectDevInit			EQU	CVectDevInit
VectDevNo			EQU	CVectDevNo
VectDevOpen			EQU	CVectDevOpen
VectEvaluateExpr		EQU	CVectEvaluateExpr
VectGetNextCmd			EQU	CVectGetNextCmd
VectInChar			EQU	CVectInChar
VectInputFile			EQU	CVectInputFile
VectLineInputFile		EQU	CVectLineInputFile
VectOutChar			EQU	CVectOutChar
VectOutputFile			EQU	CVectOutputFile
VectReReqestIn			EQU	CVectReReqestIn
VectResetBasMem			EQU	CVectResetBasMem
VectRunLink			EQU	CVectRunLink
VectSysError			EQU	CVectSysError
VectTokenize			EQU	CVectTokenize
VectUserError			EQU	CVectUserError
WarmStart			EQU	CWarmStart
WarmStartFlag			EQU	CWarmStartFlag

			ENDC
			
DBZero		EQU	Misc16BitScratch ; this is always set to Zero.
			
StackBuf	equ	$3A		; stack buffer size
RelPTR		equ	$03D		; Tempory arithmetic/logical table ptr
LineBufMax	equ	250		; maximum line buffer length

; For compatibility with CoCo ROM listings
; first floating point accumulator
FP0EXP		equ	BasVarFPAcc1	; exponent
FPA0		equ	BasVarFPAcc1+1	; mantissa
FP0SGN		equ	BasVarFPAcc1+5	; sign
COEFCT		equ	BasVarFPAcc1+6	; polynomial coeficient counter
StrDesc		equ	BasVarFPAcc1+7	; tempory string descriptor (5 bytes)

; second floating point accumulator
FP1EXP		equ	BasVarFPAcc2	; exponent $5C
FPA1		equ	BasVarFPAcc2+1	; mantissa $5D
FP1SGN		equ	BasVarFPAcc2+5	; sign $61
ResSGN		equ	BasVarFPAcc2+6	; sign of result of FP operation $62
FPSByte		equ	BasVarFPAcc2+7	; floating point sub byte $63
CoefPTR		equ	BasVarFPAcc2+8	; Coeficient pointer

FPCARY		equ	$005B	

FPA2		equ	BasTempFPA2	; FPA2

VarAB		equ	$00AB		; temp vars / RND
VarAC		equ	$00AC		; temp vars / RND
VarAD		equ	$00AD		; temp vars / RND
VarAE		equ	$00AE		; temp vars / RND

;
; Device numbers
;

DevConsole	equ 	0		; console
DevCasette	equ 	-1		; cassette
DevPrinter	equ 	-2		; printer
	
;
; Cassette file types
;
SyncByte	equ 	$55 		; Sync byte in cassete files
BlockBegin	equ 	$3C		; Begining of block marker

;Block Types
BtFileName	equ 	$00		; File name block
BtData		equ 	$01		; Data block
BtEOF		equ 	$FF		; End of file block

FNameBlockLen   equ 	15   		; 15 bytes in header block
CasFilenameLen	equ 	8		; Cassette filename length
DefBlockSize    equ 	250  		; Default block size

;File Types, as stored in filename block
FtBasic		equ 	$00		; Basic program
FtDataFile	equ 	$01		; Data file
FtMachineCode	equ 	$02		; Machine code program
FtBinary	equ 	$03		; Binary file
FtDream         equ	$88	  	; Dream Assembler source file
FtHeaderless    equ 	$FF	  	; Headerless

;Ascii/Binary flag from filename block
AsAscii		equ 	$FF		; ASCII file
AsBinary	equ 	$00		; Binary file (tokenised basic)

; Gap Flag from filename block
GfUngapped	equ 	$00		; No gaps
GfGapped	equ 	$FF		; Gaps between blocks

; Cassette file IO types
CasInputFile	equ 	1		; input file
CasOutputFile	equ 	2		; output file

;
; Screen metrics.
;
GrMaxX		equ 	255		; Maximum X co-ordinate
GrMaxY		equ 	191		; Maximum Y co-ordinate
GrMaxColour	equ 	8		; maximum colour number
GrMaxPmode	equ 	4		; maximum pmode	
GrMaxColourSet	equ 	1		; maximum colour set	
GrMaxPages	equ 	8		; maximum PCLEAR pages
GrPageSize	equ 	$600		; Graphic (pclear) page size in bytes

GrStartPage	equ 	$06		; default start page for graphics memory $0600
BasStartPage	equ 	$1E		; default start page for basic program $1E00

TextScreenBase	equ 	$0400		; base of text screen
GrScreenBase	equ 	$0600		; base of graphics pages
TextScreenLen	equ 	$0200		; Length of text screen
TextScreenLast	equ 	(TextScreenBase+TextScreenLen)-1	; last character of text screen

;
; Lo-res colour masks
;

MaskGreen	equ 	$00		; Green
MaskYellow	equ	$10		; Yellow
MaskBlue	equ	$20		; Blue
MaskRed		equ 	$30		; red
MaskBuff	equ	$40		; buff / white
MaskCyan	equ	$50		; cyan
MaskMagenta	equ	$60		; magenta
MaskOrange	equ	$70		; orange

LRGColourDiff	equ	$10		; difference between colours	

;
; Lo-res pixel masks
;
MaskUpR		equ	$08		; upper right
MaskUpL		equ	$04		; upper left
MaskLowR	equ	$02		; lower right
MaskLowL	equ	$01		; lower left
MaskAllOn	equ	(MaskUpR+MaskUpL+MaskLowR+MaskLowL)	; all on
MaskAllOff	equ	$00		; all off

MaskLRG		equ	$80		; low res graphics

; 
; Lo-res (semigraphics) and text screen metrics.
;

LoMaxX		equ 	63		; Maximum lo-res X co-ordinate
LoMaxY		equ 	31		; Maximum lo-res Y co-ordinate
LoMaxColour	equ 	8		; max lo-res colour
TextCharsLine	equ 	32		; Text/Lo-res character cells per line

;
; Cartridge entry points
;

CartBase	equ	$c000		; cartridge area base
CartEntryFIRQ	equ	CartBase	; entry point when FIRQ generated
CartEntryDOS	equ	CartBase+2	; entry point when dos flag found 'DK' at $C000
CartDOSFlag	equ	$444B		; Dos flag word : 'DK' 

;
; Dragon 64 RAM basic stuff
;

D64RAMBase	equ	$C000		; Dragon 64 RAM basic starts at $c000
D64RAMTop	equ	$FEFF		; Dragon 64 RAM ends here 

;
; Firmware flag used by warm start and RAM basic boot
;
FFlagTrue	equ	$55		; flag initialised / true
NOPFlag		equ	$12		; op-code fro NOP, used to mark start of reset vector		

;
; General evaluation vars, multiple uses
;

Eval37	equ	$37
Eval38	equ	$38
Eval39	equ	$39
Eval3A	equ	$3A
Eval3B	equ	$3B
Eval3C	equ	$3C
Eval3E	equ	$3E
Eval3F	equ	$3F

Eval40	equ	$40
Eval41	equ	$41
Eval42	equ	$42
Eval43	equ	$43
Eval44	equ	$44
Eval45	equ	$45
Eval46	equ	$46
Eval47	equ	$47
Eval48	equ	$48
Eval49	equ	$49
Eval4A	equ	$4A
Eval4B	equ	$4B
Eval4C	equ	$4C
Eval4D	equ	$4D
Eval4E	equ	$4E

EvalCF	equ	$CF

EvalD1	equ	$D1
EvalD2	equ	$D2
EvalD3	equ	$D3
EvalD4	equ	$D4
EvalD5	equ	$D5
EvalD6	equ	$D6
EvalD7	equ	$D7
EvalD8	equ	$D8
EvalD9	equ	$D9
EvalDA	equ	$DA
EvalDB	equ	$DB
EvalDC	equ	$DC	

BasFoundLineNo	EQU	Eval47		; Address of line number found by BasFindLineNo