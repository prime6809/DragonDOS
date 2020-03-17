;
; BasicDefs.asm : definitions for Dragon / CoCo BASIC.
;

BasMaxLineNo	EQU	$F9FF	; Maximum allowable line number.

;
; Dragon Basic Error codes
; 
DBErrNF		EQU	$00		; Next without for		
DBErrSN		EQU	$02		; Syntax
DBErrRG		EQU	$04		; Return without GOSUB
DBErrOD		EQU	$06		; Out of Data
DBErrFC		EQU	$08		; Function Call
DBErrOV		EQU	$0A		; OVerflow
DBErrOM		EQU	$0C		; Out of Memory
DBErrUL		EQU	$0E		; Undefined Line
DBErrBS		EQU	$10		; Bad Subscript
DBErrDD		EQU	$12		; Direct Dimension
DBErrZD		EQU	$14		; Zero Divide
DBErrID		EQU	$16		; Illegal Direct
DBErrTM		EQU	$18		; Type Mismatch
DBErrOS		EQU	$1A		; Out of String space
DBErrLS		EQU	$1C		; Long String (len > 255)
DBErrST		EQU	$1E		; String formula Too complex
DBErrCN		EQU	$20		; Can't coNtinue
DBErrUF		EQU	$22		; Undefined Function
DBErrFD		EQU	$24		; bad File Data
DBErrAO		EQU	$26		; Already Open
DBErrDN		EQU	$28		; Device Number
DBErrIO		EQU	$2A		; Input Output
DBErrFM		EQU	$2C		; File Mismatch
DBErrNO		EQU	$2E		; file Not Open
DBErrIE		EQU	$30		; Input past End of file
DBErrDS		EQU	$32		; Direct Statement
DBErrNE		EQU	$34		; file does Not Exist

;
; CoCo Basic Error codes.
;

CBErrNF		EQU	$00		; Next without for		
CBErrSN		EQU	$02		; Syntax
CBErrRG		EQU	$04		; Return without GOSUB
CBErrOD		EQU	$06		; Out of Data
CBErrFC		EQU	$08		; Function Call
CBErrOV		EQU	$0A		; OVerflow
CBErrOM		EQU	$0C		; Out of Memory
CBErrUL		EQU	$0E		; Undefined Line
CBErrBS		EQU	$10		; Bad Subscript
CBErrDD		EQU	$12		; Direct Dimension
CBErrZD		EQU	$14		; Zero Divide
CBErrID		EQU	$16		; Illegal Direct
CBErrTM		EQU	$18		; Type Mismatch
CBErrOS		EQU	$1A		; Out of String space
CBErrLS		EQU	$1C		; Long String (len > 255)
CBErrST		EQU	$1E		; String formula Too complex
CBErrCN		EQU	$20		; Can't coNtinue

CBErrFD		EQU	$22		; bad File Data
CBErrAO		EQU	$24		; Already Open
CBErrDN		EQU	$26		; Device Number
CBErrIO		EQU	$28		; Input Output
CBErrFM		EQU	$2A		; File Mismatch
CBErrNO		EQU	$2C		; file Not Open
CBErrIE		EQU	$2E		; Input past End of file
CBErrDS		EQU	$30		; Direct Statement

;
; CoCo Extended basic error codes
;

CBErrUF		EQU	$32		; Undefined Function
CBErrNE		EQU	$34		; file does Not Exist

	ifdef	Dragon
BErrNF		EQU	DBErrNF		; Next without for		
BErrSN		EQU	DBErrSN		; Syntax
BErrRG		EQU	DBErrRG		; Return without GOSUB
BErrOD		EQU	DBErrOD		; Out of Data
BErrFC		EQU	DBErrFC		; Function Call
BErrOV		EQU	DBErrOV		; OVerflow
BErrOM		EQU	DBErrOM		; Out of Memory
BErrUL		EQU	DBErrUL		; Undefined Line
BErrBS		EQU	DBErrBS		; Bad Subscript
BErrDD		EQU	DBErrDD		; Direct Dimension
BErrZD		EQU	DBErrZD		; Zero Divide
BErrID		EQU	DBErrID		; Illegal Direct
BErrTM		EQU	DBErrTM		; Type Mismatch
BErrOS		EQU	DBErrOS		; Out of String space
BErrLS		EQU	DBErrLS		; Long String (len > 255)
BErrST		EQU	DBErrST		; String formula Too complex
BErrCN		EQU	DBErrCN		; Can't coNtinue
BErrUF		EQU	DBErrUF		; Undefined Function
BErrFD		EQU	DBErrFD		; bad File Data
BErrAO		EQU	DBErrAO		; Already Open
BErrDN		EQU	DBErrDN		; Device Number
BErrIO		EQU	DBErrIO		; Input Output
BErrFM		EQU	DBErrFM		; File Mismatch
BErrNO		EQU	DBErrNO		; file Not Open
BErrIE		EQU	DBErrIE		; Input past End of file
BErrDS		EQU	DBErrDS		; Direct Statement
BErrNE		EQU	DBErrNE		; file does Not Exist
	else
BErrNF		EQU	CBErrNF		; Next without for		
BErrSN		EQU	CBErrSN		; Syntax
BErrRG		EQU	CBErrRG		; Return without GOSUB
BErrOD		EQU	CBErrOD		; Out of Data
BErrFC		EQU	CBErrFC		; Function Call
BErrOV		EQU	CBErrOV		; OVerflow
BErrOM		EQU	CBErrOM		; Out of Memory
BErrUL		EQU	CBErrUL		; Undefined Line
BErrBS		EQU	CBErrBS		; Bad Subscript
BErrDD		EQU	CBErrDD		; Direct Dimension
BErrZD		EQU	CBErrZD		; Zero Divide
BErrID		EQU	CBErrID		; Illegal Direct
BErrTM		EQU	CBErrTM		; Type Mismatch
BErrOS		EQU	CBErrOS		; Out of String space
BErrLS		EQU	CBErrLS		; Long String (len > 255)
BErrST		EQU	CBErrST		; String formula Too complex
BErrCN		EQU	CBErrCN		; Can't coNtinue
BErrUF		EQU	CBErrUF		; Undefined Function
BErrFD		EQU	CBErrFD		; bad File Data
BErrAO		EQU	CBErrAO		; Already Open
BErrDN		EQU	CBErrDN		; Device Number
BErrIO		EQU	CBErrIO		; Input Output
BErrFM		EQU	CBErrFM		; File Mismatch
BErrNO		EQU	CBErrNO		; file Not Open
BErrIE		EQU	CBErrIE		; Input past End of file
BErrDS		EQU	CBErrDS		; Direct Statement
BErrNE		EQU	CBErrNE		; file does Not Exist
	endc
