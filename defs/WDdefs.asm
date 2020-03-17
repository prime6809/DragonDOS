;
; WD17xx / WD27xx defines.
;
		ifdef	Tandy
; Disk command codes WD1793, WD1773, RSDos FDC carts.
WDCmdRestore	EQU	$00		; Restore to track 0
WDCmdSeek	EQU	$10		; Seek to track command
WDCmdReadSec	EQU	$80		; Read sector command
WDCmdWriteSec	EQU	$A0		; Write sector command
WDCmdReadAddr	EQU	$C0		; Read address mark
WDCmdForceInt	EQU	$D0		; Force inturrupt
WDCmdWriteTrack	EQU	$F4		; Write (format) track	
		else
; Disk command codes WD2797, Dragon Dos, Cumana Dos, Dragon Alpha/Professional, Dragon Beta
WDCmdRestore	EQU	$00		; Restore to track 0
WDCmdSeek	EQU	$10		; Seek to track command
WDCmdReadSec	EQU	$88		; Read sector command
WDCmdWriteSec	EQU	$A8		; Write sector command
WDCmdReadAddr	EQU	$C0		; Read address mark
WDCmdForceInt	EQU	$D0		; Force inturrupt
WDCmdWriteTrack	EQU	$F4		; Write (format) track
		endc
;
; Step rates.
;

StepRate6ms	EQU	$00		;  6ms step rate
StepRate12ms	EQU	$01		; 12ms step rate
StepRate20ms	EQU	$02		; 20ms step rate
StepRate30ms	EQU	$03		; 30ms step rate

StepRateDefault	EQU	StepRate20ms	; Default

;
; WD Error flag / status bits
;

WDErrNotReady	EQU	$80		; Not ready
WDErrWriteProt	EQU	$40		; Write protect
WDErrHeadLoaded	EQU	$20		; Head loaded, type 1
WDErrRecType	EQU	$20		; Record type, read sec
WDErrSeek	EQU	$10		; Seek error, type 1
WDErrRNF	EQU	$10		; Record not found, read address, sector write sector
WDErrCRC	EQU	$08		; CRC error all but read/write track
WDErrTrack0	EQU	$04		; Head on track 0, type 1
WDErrLostData	EQU	$04		; Lost data 
WDErrIndex	EQU	$02		; Index pulse, type 1
WDErrDRQ	EQU	$02		; Data request
WDErrBusy	EQU	$01		; Busy

WDErrMask0F	EQU	WDErrCRC+WDErrLostData+WDErrDRQ+WDErrBusy
WDErrMask5F	EQU	WDErrWriteProt+WDErrRNF+WDErrMask0F
WDErrMaskDF	EQU	WDErrNotReady+WDErrMask5F
WDErrMaskFormat	EQU	WDErrWriteProt+WDErrLostData+WDErrDRQ+WDErrBusy
WDDefErrMask	EQU	WDErrRecType+WDErrSeek+WDErrMask0F
WDErrMaskRW	EQU	WDErrWriteProt+WDErrRecType+WDErrRNF+WDErrCRC+WDErrLostData


