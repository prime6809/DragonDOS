;
; DragonDos,  Copyright (C) 1983,1984 Dragon Data Ltd.
;
; Disassembled 2004-11-05, P.Harvey-Smith.
;
; 2005-10-10, forked ram vars into seperate include file.
;
		
TrackPrecomp	EQU	$10		; Track to enable precomp if greater

SectorsPerTrack	EQU	$12		; Sectors per track
FmtDefTracks	EQU	$28		; Default tracks for format
FmtDefTracksA	EQU	$50		; Dragon Alpha default tracks for format
FmtDefSides	EQU	$01

MaxDriveNo	EQU	$04		; Maximum valid drive no
MaxFilenameLen	EQU	$0E		; Max filename length, DriveNo:Filename.EXT
MaxTrack	EQU	$4F		; Maximum track no (80)

;
; Boot command related.
;

BootFirstSector	EQU	$03		; Boot sector is track 0 sector 3
BootLastSector	EQU	$12		; Last sector of boot
BootSignature	EQU	$4F53		; Boot signature 'OS'
BootLoadAddr	EQU	$2600		; Boot area load address.
BootEntryAddr	EQU	$2602		; Boot entry address

;
; Dir track related
;

DirPrimary	EQU	$14		; Primary dir track is track 20
DirBackup	EQU	$10		; Backup on track 16

;
; DOS Low memory variables
;

AutoFlag	EQU	$FF		; Auto re-number flag, if $613=this then auto enter basic lines

DosCommand	EQU	$00EA		; Dos function no to execute
DosDriveNo	EQU	$00EB		; Last active drive number
DskTrackNo	EQU	$00EC		; Disk track no
DskSectorNo	EQU	$00ED		; Disk sectror no
DiskBuffPtr	EQU	$00EE		; Disk buffer pointer
DosDiskError	EQU	$00F0		; Disk error status byte
DosCurrCtrlBlk	EQU	$00F1		; Current file control block (FIB) (0..9) $FF=no files open
DosBytesInDTA	EQU	$00F2		; Number of bytes in DTA (also used for tracks in DSKINIT).
DosDSKINITraks	EQU	$00F2		
DosNoBytesMove	EQU	$00F3		; Number of bytes to transfer to/from buffer, also head during format
DosDSKINIHead	EQU	$00F3		
DosRecLenFlag	EQU	$00F4		; Record length flag, 00 don'r care $FF=do care, also DS flag in format
DosRWFlag	EQU	$00F5		; Read write flag, $00=read, $01= write, $FF=verifying
DosIOInProgress	EQU	$00f6		; I/O currently in progress flag 0x00 check for time out, Non-0x00 skip timeout check
DosMakeSysDsk	EQU	$00f7		; Make system disk during format, $00=no, $FF=yes

DosSectorSeek	EQU	$00F8		; Sector currently seeking {SuperDos Rom}

*************************************************************************

DosAreaStart	EQU	$0600		; Start of RAM used by DOS
Thrash		EQU	$0600		; Work byte, yes that's what it's called in the DD sourcecode!
Temps1		EQU	$0601		; Temp storage for registers #1
Temps2		EQU	$0602		; Temp storage for registers #2
TempC		EQU	$0603		; Temp for FREAD
TempL		EQU	$0604		; Temp for FREAD

DosErrorCode	EQU	$0603		; Error code from DOS
DosTimeout	EQU	$0605		; Timeout count, Motor timer
					; timeout occurs when this location is decremented from 0x01 to 0x00,			
DosHWMaskFF40	EQU	$0606		; Hardware command byte mask for FF40 (disk option)
DosHWMaskFF48	EQU	$0607		; hardware control mask for $ff48 (latch temp)
DosVerifyFlag	EQU	$0608		; Verify flag, 00=no verify $FF=verify
DosErrorMask	EQU	$0609		; Error mask, ANDed with error code from WD
DosDefDriveNo	EQU	$060A		; Default drive number
DosFWriteBPtr	EQU	$060B		; FWrite buffer pointer	
DosBuffStart	EQU	$060D		; Buffer start
DosBuffEnd	EQU	$060F		; Buffer end
DosRunLoadFlag	EQU	$0611		; Run/load flag $00=LOAD
DosFlFreadFlag	EQU	$0612		; Fread/FLread flag 00=fread, $FF=FLread

*************************************************************************

; Auto flags share their locations with FWrite buffers as they cannot both be 
; active together
DosAutoFlag	EQU	$0613		; Auto flag, $FF=auto, $00=no auto
DosAutoCurrent	EQU	DosBuffStart	; AUTO current line no
DosAutoInc	EQU	DosBuffEnd	; AUTO line increment

*************************************************************************

; variables for ON ERROR
DosErrGotoFlag	EQU	$0614		; ERROR GOTO flag, 0x00 Off Non-0x00 On	
DosErrDestLine	EQU	$0615		; Error destination line
DosErrLineNo	EQU	$0617		; line no error occured in (ERR line no)
DosErrLast	EQU	$0619		; Error that occured
DosErrPtr	EQU	$061A		; pointer to the statement that caused the error

*************************************************************************

Drv0Details	EQU	$061C		; Drive 0 details (6 bytes)
Drv1Details	EQU	$0622		; Drive 1 details (6 bytes)
Drv2Details	EQU	$0628		; Drive 2 details (6 bytes)
Drv3Details	EQU	$062E		; Drive 3 details (6 bytes)
DrvDeatailLen	EQU	$06		; Entries are 6 bytes long

; Offsets into drive details
DrvDetDirLSN	EQU	$00		; LSN of directory
DrvDetFreeLen	EQU	$02		; Free length
DrvDetFreeEPtr	EQU	$03		; Free extent pointer (LSN)
DrvDetUseCnt	EQU	$05		; Usage/open file count ?

*************************************************************************

BuffCount	EQU	$04		; 4 disk buffers
BuffDetailSize	EQU	$07		; Buffer detail entries ar 7 bytes long

Buff1Details	EQU	$0634		; Disk buffer 1 details
Buff2Details	EQU	$063B		; Disk buffer 2 details
Buff3Details	EQU	$0642		; Disk buffer 3 details
Buff4Details	EQU	$0649		; Disk buffer 4 details


; Disk buffer details offsets for above table
BuffLSN		EQU	$00		; LSN number 
BuffFlag	EQU	$02		; Flag, see below...
BuffDrive	EQU	$03		; Drive no 1..4
BuffAge		EQU	$04		; Age of buffer since last use, 1=oldest..4=youngest
BuffAddr	EQU	$05		; Buffer address

;BuffFlag values

BuffFree	EQU	$00		; Disk buffer is free
BuffUnknown	EQU	$01		; Valid, clean
BuffInUse	EQU	$55		; Buffer in use** not documented in DDv2c source.
BuffDirtyExpire	EQU	$FE		; Buffer has been modified, but not written to disk, grace period expired
BuffDirty	EQU	$FF		; Buffer has been modified, but not written to disk

*************************************************************************

DosCurDriveInfo EQU	$0650		; Dos current drive info
DosCurFilename	EQU	$0650		; Current filename
DosCurExtension	EQU	$0658		; Current extension, used in validation

DosCurDriveNo	EQU	$065B		; Current drive no
DosLSNCounter	EQU	$065C		; LSN counter in directory search
DosSaveBuffAddr	EQU	$065E		; Save buffer address


DosCurCount	EQU	$0660		; Current count, used in various places (extent found?)
					; marked as "Temp1, extent found" in DDv2c source
DosBytesRead	EQU	$0661		; Number of bytes to read in DosFRead
DosSecOffset	EQU	$0663		; Offset within sector for DosFRead to start
DosFWriteAddr	EQU	$0664		; Fwrite address
DosPageBufAddr	EQU	$0667		; Page buffer address
DosCurrSector	EQU	$0669		; Current sector (LSN?)
DosTotalSFound	EQU	$066B		; Total sectors found
DosFSNToFind	EQU	$066D		; FSN sought
DosCurLSN	EQU	$066F		; Current LSN, of current DIR sector being processed
DosFWBufPtr	EQU	$0671		; Pointer to buffer in FWrite
DosFWByteCount	EQU	$0673		; Number of bytes to write in FWrite
DosFWFPoint	EQU	$0675		; File pointer to write at in FWrite

DosSaveFCB	EQU	$0678		; Temparary FCB pointer used by DosCreateFile.
DosSaveExt	EQU	$067A		; Temporary extension used during DosCreateFile
DosTempFCBNo	EQU	$067D		; Temporary Fileno / FCB no storage used by DosCreateFile.
DosCurDirBuff	EQU	$067F		; Pointer to the current Dir sector, Buffer def block
DosTempFileNo	EQU	$0681		; Temp file no whilst opening file
DosCurFileNo	EQU	$0682		; Current file number on disk, to get dir entry for

DosNewUSRTable	EQU	$0683		; New USR table, relocated from low ram

;Drive parameter tables, note these need to be in this order.
DosD0Online	EQU	$0697		; Drive 0 online flag
DosD1Online	EQU	$0698		; Drive 1 online flag
DosD2Online	EQU	$0699		; Drive 2 online flag
DosD3Online	EQU	$069A		; Drive 3 online flag

DosD0Track	EQU	$069B		; Drive 0 current track
DosD1Track	EQU	$069C		; Drive 1 current track
DosD2Track	EQU	$069D		; Drive 2 current track
DosD3Track	EQU	$069E		; Drive 3 current track

DosTemp2	EQU	$067E		; Temp, used in FindEmptyDir, CmdFLRead.... 
DosLBASec	EQU	$069F		; as step rates meaningless for disk image

DosD0StepRate	EQU	$069F		; Drive 0 step rate
DosD1StepRate	EQU	$06A0		; Drive 1 step rate
DosD2StepRate	EQU	$06A1		; Drive 2 step rate
DosD3StepRate	EQU	$06A2		; Drive 3 step rate

DosD0Tracks	EQU	$06A3		; Tracks on disk in drive 0
DosD1Tracks	EQU	$06A4		; Tracks on disk in drive 1
DosD2Tracks	EQU	$06A5		; Tracks on disk in drive 2
DosD3Tracks	EQU	$06A6		; Tracks on disk in drive 3

DosD0SecTrack	EQU	$06A7		; Sectors per track drive 0
DosD1SecTrack	EQU	$06A8		; Sectors per track drive 1
DosD2SecTrack	EQU	$06A9		; Sectors per track drive 2
DosD3SecTrack	EQU	$06AA		; Sectors per track drive 3

; Offsets of tables from Online table, this way we can point an index register
; at the drive online byte and access the other tables with offset,IR

DosTrackTblOfs	EQU	(DosD0Track-DosD0Online)
DosStepTblOfs	EQU	(DosD0StepRate-DosD0Online)
DosTracksTblOfs	EQU	(DosD0Tracks-DosD0Online)
DosSecTrkTblOfs	EQU	(DosD0SecTrack-DosD0Online)

DosDirSecStatus	EQU	$06AB		; Directory sector status $06ab-$06bc

DosFCB0Addr	EQU	$06BD		; File Control Block 0 Address 
DosFCB1Addr	EQU	$06DC		; File Control Block 1 Address
DosFCB2Addr	EQU	$06FB		; File Control Block 2 Address 
DosFCB3Addr	EQU	$071A		; File Control Block 3 Address 
DosFCB4Addr	EQU	$0739		; File Control Block 4 Address 
DosFCB5Addr	EQU	$0758		; File Control Block 5 Address 
DosFCB6Addr	EQU	$0777		; File Control Block 6 Address 
DosFCB7Addr	EQU	$0796		; File Control Block 7 Address 
DosFCB8Addr	EQU	$07B5		; File Control Block 8 Address 
DosFCB9Addr	EQU	$07D4		; File Control Block 9 Address 
DosFCBEnd	EQU	$07F3		; First byte beyond last FCB

DosDiskBuffBase EQU	$0800		; Base of Disk buffers

DosFCBLength	EQU	$1F		; 31 bytes per FCB

DosNumFCBs	EQU	(DosFCBEnd-DosFCB0Addr)/DosFCBLength	; number of FCBs

;
; FCB structure is :
;
; FCB format :
; Pos	Len	Use
; $00	8	File name
; $08	3	Extension
; $0B	1	Drive no (1..4)
; $0C	3	Next read byte (file pointer?)
; $0F	1	Directory flags
; $10	3	Length of file
; $13	2	First sector number (FSN) of extent 1
; $15	2	LSN of extent 1
; $17	1	sextors in extent 1
; $18	2	First sector number (FSN) of extent 1
; $1A	2	LSN of extent 1
; $1C	1	sectors in extent 1
; $1D	1	Number of dir entry
; $1E	1	Directory number of last entry

FCBFileName	EQU	$00	; Filename (zero padded)
FCBExtension	EQU	$08	; Extension (zero padded)
FCBDrive	EQU	$0B	; Drive no
FCBFilePointer	EQU	$0C	; File Pointer 
FCBDirFlags	EQU	$0F	; Dir flags (attributes)
FCBFileLen	EQU	$10	; File Length
FCBFSNExtent1	EQU	$13	; FSN of extent 1
FCBLSNExtent1	EQU	$15	; LSN of extent 1 
FCBSecExtent1	EQU	$17	; Sectors in extent 1
FCBFSNExtent2	EQU	$18	; FSN of extent 2
FCBLSNExtent2	EQU	$1A	; LSN of extent 2 
FCBSecExtent2	EQU	$1C	; Sectors in extent 2
FCBDiskFileNo	EQU	$1D	; File number on disk, (dir entry no).
FCBDirNoLast	EQU	$1E	; Dir no of last entry

;
; Backup command stack frame offsets
;
; These are offset from U on stack
;

BupSrcDrive	EQU	0	; Drive number of source 
BupSrcTrk	EQU	1	; Source track 
BupSrcSec	EQU	2	; Source sector no
BupSrcMess	EQU	3	; Insert source message ptr
BupSrcBuff	EQU	5	; Source sector buffer addr 
BupDestDrive	EQU	7	; Drive number of dest 
BupDestTrk	EQU	8	; Dest track a
BupDestSec	EQU	9	; Dest sector no
BupDestMess	EQU	10	; Insert dest message ptr
BupDestBuff	EQU	12	; Dest sector buffer addr 
BupSecTrk	EQU	14	; Sector count per track to copy 
BupAvailPages	EQU	15	; Pages available to buffer sectors

;
; Offset from X, which will point to BupSrcDrive, or BupDestDrive
; 

BupDrive	EQU	0	; Drive number  
BupTrk		EQU	1	; track 
BupSec		EQU	2	; sector
BupMess		EQU	3	; backup message
BupBuff		EQU	5	; Source sector buffer addr ?

BupStackFrame	EQU	16	; backup stack frame size

SpinUpDelay	EQU	$D800	; Value for timeout loop


;
; Sync dir stack frame offsets
;
; These are offset from U on stack
;

SyncDriveCount	EQU	0	; drive counter, done by bitshift
SyncDrive	EQU	1	; Drive we are syncing
SyncBufferNo	EQU	2	; Current buffer no
SyncSecNo	EQU	3	; Sector we are syncing
SyncSectors	EQU	4	; 4 sector numbers in the disk buffers

;
; Dos function codes used by hardware routine.
; 

DosFnRestore	EQU	$00	; Restore to track 0
DosFnSeek	EQU	$01	; Seek to a track
DosFnReadSec	EQU	$02	; Read a sector
DosFnWriteSecV	EQU	$03	; Write a sector with verify
DosFnWriteSecN	EQU	$04     ; Write a sector no verify
DosFnWriteTrack	EQU	$05	; Write (format) track
DosFnReadAddr	EQU	$06	; Read address mark
DosFnReadSec2	EQU	$07	; Read first two bytes of a sector
DosFnWriteDef	EQU	$08	; Write defective sector

*******************************************
***** Directory Track realted defines *****
*******************************************

;
; Dir entry format(s).
;
; Dragon/Super dos directory entries can take one of 2 formats, they can be either a
; filename block, containing filename, attributes & 4 allocation entries, or they can
; be a continuation block, containing just allocation entries. 
; This is controled by the byte at offset $18, and the attributes.
;
; if AttrContinued = 0 then 
; 	the byte at offset $18, contains the number of number of bytes in the last sector (256 bytes = 0).
;
; if AttrContinued = 1 then 
; 	the byte at offset $18 controls the format of the entry :
;		if 0 then 
;			Entry is a filename entry
;		else
;			Entry is a continuation block.

; Filename block format 

DirEntAttr	EQU	$00		; Attributes (see below)
DirEntFilename	EQU	$01		; Filename, zero padded
DirEntExtension	EQU	$09		; Extension, zero padded
DirEntFnBlock1	EQU	$0C		; Allocation block #1
DirEntFnBlock2	EQU	$0F		; Allocation block #2
DirEntFnBlock3	EQU	$12		; Allocation block #3
DirEntFnBlock4	EQU	$15		; Allocation block #4
DirEntFlag	EQU	$18		; Filename/Continuation flag 0/nonzero
DirEntLastBytes	EQU	$18		; bytes in last sector

DirEntryLen	EQU	$19		; Length of Dir Entry.
DirEntExts	EQU	4		; 4 allocatoion blocks in filename block

; Continuation block, DirEntAttr, and DirEntFlag, as above.

DirEntCntBlock1	EQU	$01		; Allocation block #1
DirEntCntBlock2	EQU	$04		; Allocation block #2
DirEntCntBlock3	EQU	$07		; Allocation block #3
DirEntCntBlock4	EQU	$0A		; Allocation block #4
DirEntCntBlock5	EQU	$0D		; Allocation block #5
DirEntCntBlock6	EQU	$10		; Allocation block #6
DirEntCntBlock7	EQU	$13		; Allocation block #7

DirEntCntExts	EQU	7		; 7 allocatoion blocks in continuation block

DirBlksPerSec	EQU	$0A		; Number of dir blocks per dir sector

;
; Allocation block format.
;

AllocLSN	EQU	$00		; Logical sector number of start of allocation
AllocCount	EQU	$02		; Count of number of sectors allocated.
AllocEntrySize	EQU	$03		; Size of allocation entry

AllocLen	EQU	3		; Allocation length

;
; File Attributes
;

AttrDeleted	EQU	%10000000	; Deleted, may be reused
AttrContinued	EQU	%00100000	; Continuation entry, byte at $18 giver next entry no
AttrEndOfDir	EQU	%00001000	; End of directory, no more entries need to be scanned
AttrWriteProt	EQU	%00000010	; Write protect flag
AttrIsCont	EQU	%00000001	; This is a continuation entry.

AttrAtFormat	EQU	AttrDeleted+AttrEndOfDir+AttrIsCont	; Attributes set in DSKINIT	
AttrAfterDel	EQU	AttrDeleted+AttrIsCont			; Attributes set by DosDeleteFile

DirEntPerSec	EQU	$0A				; Directory entries per sector
DirLastByte	EQU	(DirEntPerSec*DirEntryLen)	; First byte beyond last dire entry in sector

;
; Offsets in Sector 0 on Dir track
;

BitmapPart1	EQU	$00		; Bitmap uses bytes $00..$FB on first sector
DirTracks	EQU	$FC		; Tracks on disk
DirSecPerTrk	EQU	$FD		; Sectors/track 18=Single sided, 36=Double sided
DirTracks1s	EQU	$FE		; complement of DirTracks (used to validate)
DirSecPerTrk1s	EQU	$FF		; Complement of DirSecPerTrk (used to validate)

;
; DOS Error codes.
;

ErrNR	EQU	$80 			; not ready
ErrSK	EQU	$82 			; seek
ErrWP	EQU	$84 			; write protect
ErrRT	EQU	$86 			; record type
ErrRF	EQU	$88 			; record not found
ErrCC	EQU	$8A 			; CRC
ErrLD	EQU	$8C 			; Lost data
ErrBT	EQU	$8E 			; boot
ErrIV	EQU	$90 			; invalid volume / directory
ErrFD	EQU	$92 			; Full directory
ErrDF	EQU	$94 			; Disk full
ErrFS	EQU	$96 			; File spec
ErrPT	EQU	$98 			; Protection
ErrPE	EQU	$9A 			; (read) Past end 
ErrFF	EQU	$9C 			; File not Found 
ErrFE	EQU	$9E 			; File exists
ErrNE	EQU	$A0 			; (file does) Not Exist
ErrTF	EQU	$A2 			; Too many Files open
ErrPR	EQU	$A4 			; Parameter
ErrUD	EQU	$A6 			; Undefined
ErrSFF	EQU	$FF			; ????

	if 0
DDErrNR		EQU	$80		; Not ready
DDErrSK		EQU	$82		; seek
DDErrWP		EQU	$84		; write protect
DDErrRT		EQU	$86		; record type
DDErrRF		EQU	$88		; record not found
DDErrCC		EQU	$8A		; CRC
DDErrLD		EQU	$8C		; Lost data
DDErrBT		EQU	$8E		; boot
DDErrIV		EQU	$90		; invalid volume / directory
DDErrFD		EQU	$92		; Full directory
DDErrDF		EQU	$94		; Disk full
DDErrFS		EQU	$96		; File spec
DDErrPT		EQU	$98		; Protection
DDErrPE		EQU	$9A		; (read) Past end 
DDErrFF		EQU	$9C		; File not Found 
DDErrFE		EQU	$9E		; File exists
DDErrNE		EQU	$A0		; (file does) Not Exist
DDErrTF		EQU	$A2		; Too many Files open
DDErrPR		EQU	$A4		; Parameter
DDErrUD		EQU	$A6		; Undefined
DDErrSFF	EQU	$FF		; ????
	endc
DDFirstError	EQU	ErrNR		; First error code

SecTrkSS	EQU	18		; 18 sectors / track single sided
SecTrkDS	EQU	36		; 36 sectors / track double sided

MaxLSN		EQU	(SecTrkDS*80)	; Maximum possible LSN

DskInitBuffer	EQU	$0800		; Address of format disk buffer
SectorsPerBAM	EQU	$08		; Each BAM entry is for 8 sectors
BAMEntries40SS	EQU	(SectorsPerTrack*40)/SectorsPerBAM	; Number of BAM entries for a single sided 40 track disk
BAMEntriesSec	EQU	(MaxLSN/2)/SectorsPerBAM		; Number of BAM entries per BAM sector
BAMLSNPerSec	EQU	MaxLSN/2	; Number of LSNs per BAM sector

; Offsets within BAM of directory track flags
BAMOffDirPriSS	EQU	(SectorsPerTrack*DirPrimary)/SectorsPerBAM
BAMOffDirBakSS	EQU	(SectorsPerTrack*DirBackup)/SectorsPerBAM
BAMOffDirPriDS	EQU	(SectorsPerTrack*(DirPrimary*2))/SectorsPerBAM
BAMOffDirBakDS	EQU	(SectorsPerTrack*(DirBackup*2))/SectorsPerBAM

BAMSecCount	EQU	$02		; No of BAM Sectors
DIRSecCount	EQU	$10		; Dir sector count

DosFlagTrue	EQU	$FF		; flag true / enabled
DosFlagFalse	EQU	$00		; flag false /disabled

IOInProgress	EQU	DosFlagTrue	; Flag that DOS IO in progress
ErrGotoEnabled	EQU	DosFlagTrue	; Flag that on error goto is enabled

AutoStartLine	EQU	100		; Default start line for AUTO
AutoIncrement	EQU	10		; Default increment for AUTO

; DosFRead code shared between file read wrie and verify
; DosRWFlag determines the operation to be performed

FileOpRead	EQU	$00		; Reading file
FileOpWrite	EQU	$01		; Writing file
FileOpVerify	EQU	$FF		; verifying written data

; Extend file stack fram offsets

ExtendSecCount	EQU	$00		; Sector count
;ExtendDirEntPtr	EQU	$04		; Pointer to dir entry with last sector 
;ExtendGeomPtr	EQU	$08		; Pointer to disk geometry buffer
ExtendSecCount2	EQU	$0A		; Sector count

;
; Offsets within file header, compatible with DragonDos/SuperDos
;
HdrID55		EQU	$00		; magic marker byte #1
HdrType		EQU	$01		; file type see filetypes below
HdrLoad		EQU	$02		; Load address of file
HdrLen		EQU	$04		; length of file
HdrExec		EQU	$06		; entry address of file
HdrIDAA		EQU	$08		; magic marker byte #2

FileHeadLen	EQU	$09		; header length in bytes

; File types

FTypeBas	EQU	$01		; Basic files
FTypeBin	EQU	$02		; Binary file types

; Marker bytes
MarkerHeadStart EQU     $55		; start header marker
MarkerHeadEnd   EQU     $AA		; end header marker

; Device number
DevDisk		EQU	$01		; disk device
