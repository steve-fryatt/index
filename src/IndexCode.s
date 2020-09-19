; Copyright 1997-2014, Stephen Fryatt (info@stevefryatt.org.uk)
;
; This file is part of Index:
;
;   http://www.stevefryatt.org.uk/software/
;
; Licensed under the EUPL, Version 1.2 only (the "Licence");
; You may not use this work except in compliance with the
; Licence.
;
; You may obtain a copy of the Licence at:
;
;   http://joinup.ec.europa.eu/software/page/eupl
;
; Unless required by applicable law or agreed to in
; writing, software distributed under the Licence is
; distributed on an "AS IS" basis, WITHOUT WARRANTIES
; OR CONDITIONS OF ANY KIND, either express or implied.
;
; See the Licence for the specific language governing
; permissions and limitations under the Licence.

; IndexCode.s
;
; Index Code Source
; Squashed Load & Save routines for !Index
; WildCard Comparison routines
; Memory Management

;
; 26/32 bit neutral

	GET	$Include/SWINames

; ----------------------------------------------------------------------------------------------------------------------
; Set up constants

IOSize			*	1024

; ======================================================================================================================
; Module Header

	AREA	Module,CODE,READONLY
	ENTRY

ModuleHeader
Param1
	DCD	0				; Parameter 1
Param2
	DCD	0				; Parameter 2
Param3
	DCD	0				; Parameter 3

Heap
	DCD	0				; Pointer to heap put here
Himem
	DCD	0				; Pointer to HIMEM put here

BranchTable
	B	FileInfo			; +20 Branch to File Info Code
	B	LoadFile			; +24 Branch lo load file code
	B	SaveFile			; +28 Branch to save file code
	B	CompStrings			; +32 Branch to string compare code
	B	Init				; +36 Branch to initialization code
	B	SearchAll			; +40 Branch to search code
	B	ClaimExt			; +44 Branch to claim code
	B	ReleaseExt			; +48 Branch to release code

HeapSize
	DCD	24				; Size of WimpSlot heap
MinHeap
	DCD	24				; Minimum size of heap
AppSize
	DCD	0				; Size of application
FileName
	DCD	0				; Pointer to file name
IBlock
	DCD	0				; Pointer to save from block
IBlockSize
	DCD	0				; Save block size
FileHandle
	DCD	0				; File handle
WorkArea
	DCD	0				; Pointer to heap area
IOPointer
	DCD	0				; Pointer to ioarea

;-----------------------------------------------------------------------------------------------
; Internal
; General file opening routine
;
; R0 -> Filename to open
;
; File handle stored at handle)
;-----------------------------------------------------------------------------------------------

OpenFile
	STMFD	R13!,{R1-R7,R14}		; R0 -> Filename

	MOV	R1,#0				; Zero the pointers
	STR	R1,FileHandle
	STR	R1,WorkArea

	MOV	R1,R0				; R1 -> Filename
	MOV	R0,#20
	SWI	OS_File				; Get file information

	CMP	R0,#1				; Check that a file has been read
	BEQ	IsFile

NotFile
	ADR	R0,NotFileError			; Point to error message
	STR	R0,Param3
	MOV	R0,#-1				; Return -1
	STR	R0,Param1
	B	OpenExit

NotFileError
	DCB	"NotFile",13
	ALIGN

IndexFiletype
	DCD	&0E1				; The index filetype

IsFile
	LDR	R0,IndexFiletype		; Check the file is an index
	CMP	R0,R6
	BEQ	IsIndex

IsNotIndex
	ADR	R0,NotIndexError		; Point to error message
	STR	R0,Param3
	MOV	R0,#-1				; Return -1
	STR	R0,Param1
	B	OpenExit

NotIndexError
	DCB	"NotIndex",13
	ALIGN

SquashHeader
	DCB	"SQSH"				; The first word in a squashed file

IsIndex
	MOV	R0,#&40				; Open the file
	SWI	OS_Find

	CMP	R0,#0				; Check it's OK
	BEQ	IsNotIndex
	STR	R0,FileHandle			; Save the file handle

	LDR	R0,IOPointer			; Get the first 20 bytes of the file
	MOV	R1,#20
	BL	GetBytes
	BVS	BadIndexFile

	LDR	R2,SquashHeader			; Check header
	LDR	R3,IOArea
	CMP	R2,R3
	BNE	BadIndexFile

	B	OpenExit			; Done!

BadIndexFile
	ADR	R0,BadIndexFileError		; Point to error message
	STR	R0,Param3
	MOV	R0,#-1				; Return -1
	STR	R0,Param1
	B	OpenExit

BadIndexFileError
	DCB	"BadIndex",13
	ALIGN

OpenExit
	LDMFD	R13!,{R1-R7,R15}		; Return

;-----------------------------------------------------------------------------------------------
; External
; File information routine
;
; P1 -> Filename of file to be examined
;
; P1 == File size (bytes)
;-----------------------------------------------------------------------------------------------

FileInfo
	STMFD	R13!,{R0-R11,R14}		; P1 -> Filename

	LDR	R0,Param1			; R0 -> Filename
	BL	OpenFile

	MOV	R1,#-1				; Exit if an error occurred
	CMP	R0,R1
	BEQ	FileInfoExit

	LDR	R0,IOPointer			; Get size
	LDR	R1,[R0,#4]
	STR	R1,Param1

FileInfoExit
	BL	UnClaim				; Close the file; de-allocate memory

	LDMFD	R13!,{R0-R11,R15}		; Return to BASIC

;-----------------------------------------------------------------------------------------------
; External
; Load file routine
;
; P1 -> Filename of file to be loaded
; P2 == Address to load file
;-----------------------------------------------------------------------------------------------

LoadFile
	STMFD	R13!,{R0-R11,R14}		; P1 -> filename; P2 = address

	LDR	R0,Param1			; R0 -> filename
	LDR	R4,Param2			; R4 = address to load to
	BL	OpenFile

	MOV	R1,#-1				; Exit if an error occurred
	CMP	R0,R1
	BEQ	LoadExit

	LDR	R9,IOPointer			; R9 -> io work area

	MOV	R0,#2_1000			; Find out how much work area we need
	MOV	R1,#IOSize
	SWI	Squash_Decompress

	MOV	R3,R0				; Claim the workarea
	LDR	R1,Heap
	MOV	R0,#2
	SWI	XOS_Heap
	STR	R2,WorkArea

	CMP	R2,#0				; Check we got some space
	BNE	LoadHeapOK

LoadHeapFail
	ADR	R0,LoadHeapFailError		; Point to error message
	STR	R0,Param3
	MOV	R0,#-1				; Return -1
	STR	R0,Param1
	B	LoadExit

LoadHeapFailError
	DCB	"DecompHeap",13
	ALIGN

LoadHeapOK
	LDR	R5,[R9,#4]			; R5 = data length
	MOV	R3,#0				; R3 = Bytes left in input buffer
	MOV	R8,#0				; R8 = 0 = Start

DeCompLoop
	CMP	R3,#0				; If there are any bytes left,
	BEQ	NoShuffle			; we must shuffle them down
	LDR	R1,IOPointer			; to the start of the input
	ADD	R2,R1,#IOSize			; buffer, so that they can be used
	SUB	R2,R2,R3
	MOV	R9,R3

Shuffle
	LDRB	R0,[R2],#1			; Do the shuffle!
	STRB	R0,[R1],#1
	SUBS	R9,R9,#1
	BNE	Shuffle

NoShuffle
	LDR	R0,IOPointer			; Fill up input buffer from file
	ADD	R0,R0,R3
	RSB	R1,R3,#IOSize
	BL	GetBytes			; (RO = not got)

	MOVS	R9,R0				; Save the no of bytes left (& set continue%)
	RSB	R3,R0,#IOSize			; R3 = No of bytes in input buffer

	MOV	R0,R8
	ORREQ	R0,R0,#2_10			; RO = start% OR (continue%<<1)

	LDR	R1,WorkArea			; R1 -> Internal workarea
	LDR	R2,IOPointer			; R2 -> Data to be decompressed
						; R4 -> Place to decompress to
						; R5 = Decompress area size

	SWI	XSquash_Decompress		; Do-o-o it!
	BVC	DeCompOK

DeCompFail
	ADR	R0,DeCompFailError		; Point to error message
	STR	R0,Param3
	MOV	R0,#-1				; Return -1
	STR	R0,Param1
	B	LoadExit

DeCompFailError
	DCB	"DecompErr",13
	ALIGN

DeCompOK
	MOV	R8,#1				; Show that we have started

	CMP	R9,#0				; Keep going until we finish
	BEQ	DeCompLoop

LoadExit
	BL	UnClaim				; Close the file, de-allocate memory

	LDMFD	R13!,{R0-R11,R15}		; Return to BASIC

;-----------------------------------------------------------------------------------------------
; External
; File save routine
;
; P1 -> Filename of file to be saved
; P2 == Start address in memory fo data
; P3 == End address in memory of data
;-----------------------------------------------------------------------------------------------

SaveFile
	STMFD	R13!,{R0-R11,R14}		; P1 -> Filename; P2 = Start address
						; P3 = End address
	LDR	R0,Param1			; Load in parameters
	LDR	R1,Param2
	LDR	R2,Param3

	SUBS	R2,R2,R1			; Check that the addresses make sense
	BGT	MemOK

BadMem
	ADR	R0,BadMemError			; Point to error message
	STR	R0,Param3
	MOV	R0,#-1				; Return -1
	STR	R0,Param1
	B	SaveExit

BadMemError
	DCB	"BadSaveParam",13
	ALIGN

MemOK
	STR	R0,FileName			; Store filename, and mem addresses
	STR	R1,IBlock
	STR	R2,IBlockSize

	MOV	R1,#0				; Zero the pointers
	STR	R1,WorkArea
	STR	R1,FileHandle

	MOV	R0,#2_1000			; Find size of workspace
	MOV	R1,R2
	SWI	Squash_Compress

	MOV	R3,R0				; Claim the workarea
	LDR	R1,Heap
	MOV	R0,#2
	SWI	XOS_Heap
	STR	R2,WorkArea

	CMP	R2,#0				; Check we got some space
	BNE	SaveHeapOK

SaveHeapFail
	ADR	R0,SaveHeapFailError		; Point to error message
	STR	R0,Param3
	MOV	R0,#-1				; Return -1
	STR	R0,Param1
	B	SaveExit

SaveHeapFailError
	DCB	"CompHeap",13
	ALIGN

SaveHeapOK
	MOV	R0,#11				; Create a new, blank, file
	LDR	R1,FileName
	LDR	R2,IndexFiletype
	MOV	R4,#0
	MOV	R5,#0
	SWI	OS_File
	BVC	CreatedOK

CreateFail
	ADR	R0,CreateFailError		; Point to error message
	STR	R0,Param3
	MOV	R0,#-1				; Return -1
	STR	R0,Param1
	B	SaveExit

CreateFailError
	DCB	"NoFile",13
	ALIGN

CreatedOK
	MOV	R0,#&C0				; Open the file for I/O
	LDR	R1,FileName
	SWI	XOS_Find
	BVS	CreateFail
	STR	R0,FileHandle

	LDR	R0,SquashHeader			; Get file header details
	LDR	R1,IBlockSize
	MOV	R2,#0				; The load and execution address
	MOV	R3,#0				; zero as they are meaningless here!
	MOV	R4,#0

	LDR	R9,IOPointer			; Save header
	STMIA	R9,{R0-R4}
	MOV	R0,#20
	BL	PutBytes

	LDR	R1,WorkArea			; Set up for compress loop
	LDR	R2,IBlock
	LDR	R3,IBlockSize
	MOV	R9,#0

CompLoop
	MOV	R0,R9
	LDR	R4,IOPointer
	MOV	R5,#IOSize
	SWI	XSquash_Compress
	BVC	CompOK

CompFail
	ADR	R0,CompFailError		; Point to error message
	STR	R0,Param3
	MOV	R0,#-1				; Return -1
	STR	R0,Param1
	B	SaveExit

CompFailError
	DCB	"CompErr",13
	ALIGN

CompOK
	MOV	R9,R0
	RSB	R0,R5,#IOSize
	BL	PutBytes

	CMP	R9,#0
	MOVNE	R9,#1
	BNE	CompLoop

SaveExit
	BL	UnClaim				; Close the file, de-allocate memory

	LDMFD	R13!,{R0-R11,R15}		; Return to BASIC


GetBytes
	STMFD	R13!,{R1-R4,R14}		; R0 -> Work area; R1 = No. of bytes

	MOV	R3,R1
	MOV	R2,R0
	LDR	R1,FileHandle
	MOV	R0,#4
	SWI	OS_GBPB
	MOV	R0,R3

	LDMFD	R13!,{R1-R4,R15}		; Return; R0 = Bytes not transferred


PutBytes
	STMFD	R13!,{R0-R3,R14}		; R0 = Bytes to write

	MOV	R3,R0
	MOV	R0,#2
	LDR	R1,FileHandle
	LDR	R2,IOPointer
	SWI	OS_GBPB

	LDMFD	R13!,{R0-R3,R15}


UnClaim
	STMFD	R13!,{R0-R2,R14}

	MOV	R0,#3				; Deallocate heap area
	LDR	R1,Heap
	LDR	R2,WorkArea
	CMP	R2,#0
	SWINE	OS_Heap

	MOV	R0,#0				; Close the open file
	LDR	R1,FileHandle
	CMP	R1,#0
	SWINE	OS_Find

	LDMFD	R13!,{R0-R2,R15}		; Return

;-----------------------------------------------------------------------------------------------
; External
; String comparison routine
;
; P1 -> First string (original, lower case, contains wildcards)
; P2 -> Second string (string to compare, any case, no wildcards)
;
; P1 == 0 - strings different | 1 - strings equal
;-----------------------------------------------------------------------------------------------

CompStrings
	STMFD	R13!,{R0-R11,R14}		; P1 -> String 1; P2 -> String 2

	LDR	R0,Param1			; R0 = s1 (ie R0 -> String 1)
	LDR	R1,Param2			; R1 = s2 (ie R1 -> String 2)

	BL	StringComp			; Do comparison

	STR	R8,Param1			; Save result

	LDMFD	R13!,{R0-R11,R15}		; Return to BASIC

;-----------------------------------------------------------------------------------------------
; Internal
; String comparison routine
;
; R0 -> First string (original, lower case, contains wildcards)
; R1 -> Second string (string to compare, any case, no wildcards)
;
; R8 == 0 - strings different | 1 - strings equal
;-----------------------------------------------------------------------------------------------

StringComp
	STMFD	R13!,{R0-R7,R14}		; Subroutine; R0 -> s1; R1 -> s2

	LDRB	R2,[R0]				; R2 = c1
	LDRB	R3,[R1]				; R3 = c2

	CMP	R3,#"A"				; Convert c2 TO lower case
	BLT	IsLower
	CMP	R3,#"Z"
	BGT	IsLower
	ADD	R3,R3,#32

IsLower						; c1 & c2 are both lower case
	CMP	R3,#32				; If c2 is null, check match
	BGE	NotNull

StripLoop
	LDRB	R5,[R0]				; While s1 = '*'
	CMP	R5,#"*"
	BNE	EndStrip
	ADD	R0,R0,#1
	B	StripLoop

EndStrip
	MOV	R8,#0				; return (*s1 == 0)
	LDRB	R5,[R0]
	CMP	R5,#32
	MOVLT	R8,#1
	LDMFD	R13!,{R0-R7,R15}

NotNull
	CMP	R2,R3				; If (c1 == c2 || c1 == '#')
	CMPNE	R2,#"#"
	BNE	NotEqual

	ADD	R0,R0,#1			; Return strcmp(s1+1, s2+1)
	ADD	R1,R1,#1
	BL	StringComp
	LDMFD	R13!,{R0-R7,R15}

NotEqual
	CMP	R2,#"*"				; if (c1 == '*')

	MOVNE	R8,#0				; If not, return FALSE
	LDMNEFD	R13!,{R0-R7,R15}

	LDRB	R5,[R0,#1]			; If s1+1 == 0, return TRUE
	CMP	R5,#32
	MOVLT	R8,#1
	LDMLTFD	R13!,{R0-R7,R15}

	MOV	R4,#0				; ok = FALSE
	ADD	R0,R0,#1

WhileWild
	CMP	R4,#0				; while (!ok && *S2 != 0)
	BNE	EndWild
	LDRB	R5,[R1]
	CMP	R5,#32
	BLT	EndWild

	BL	StringComp			; ok = ok OR strcmp(s1+1, s2)
	ORR	R4,R4,R8

	ADD	R1,R1,#1			; s2++

	B	WhileWild

EndWild
	MOV	R8,R4				; return OK
	LDMFD	R13!,{R0-R7,R15}

SearchAll

;-----------------------------------------------------------------------------------------------
; External
; Claim WimpSlot block
;
; P1 == Size of block being claimed
;
; P1 == Address of block | -1 - claim failed
;-----------------------------------------------------------------------------------------------

ClaimExt
	STMFD	R13!,{R14}
	LDR	R0,Param1
	BL	ClaimHeap
	STR	R0,Param1
	LDMFD	R13!,{R15}

;-----------------------------------------------------------------------------------------------
; External
; Release WimpSlot block
;
; P1 == Address of block to release
;-----------------------------------------------------------------------------------------------

ReleaseExt
	STMFD	R13!,{R14}
	LDR	R0,Param1
	BL	ReturnHeap
	LDMFD	R13!,{R15}

;-----------------------------------------------------------------------------------------------
; Internal
; Claim WimpSlot block
;
; R0 == Size of block being claimed
;
; R0 == Address of block | -1 - claim failed
;-----------------------------------------------------------------------------------------------

ClaimHeap
	STMFD	R13!,{R1-R5,R14}		; R0 = size required

	ADD	R5,R0,#8			; Check to see if there is a free block
	MOV	R0,#1				; that is big enough
	LDR	R1,Himem
	SWI	OS_Heap
	CMP	R2,R5
	BHS	EnoughFree

GrowSlot
	LDR	R0,AppSize			; If there isn't, try and extend the
	LDR	R1,HeapSize			; WimpSlot
	ADD	R0,R0,R1
	ADD	R0,R0,R5
	MOV	R3,R0
	MOV	R1,#-1
	SWI	Wimp_SlotSize

	CMP	R0,R3				; Check that the slot was extended.
	BHS	ClaimOK

ClaimNotOK
	SUB	R0,R3,R5
	MOV	R1,#-1
	SWI	Wimp_SlotSize

	MOV	R0,#-1
	B	ClaimExit

ClaimOK
	MOV	R0,#5				; If it was, extend the heap accordingly.
	LDR	R1,Himem
	MOV	R3,R5
	SWI	OS_Heap
	LDR	R0,HeapSize
	ADD	R0,R0,R5
	STR	R0,HeapSize

EnoughFree
	MOV	R0,#2				; Finally, allocate the block.
	SUB	R3,R5,#8
	SWI	XOS_Heap
	MOVVS	R2,#-1
	MOV	R0,R2

ClaimExit
	LDMFD     R13!,{R1-R5,R15}		; Return; R0 -> Block

;-----------------------------------------------------------------------------------------------
; Internal
; Release WimpSlot block
;
; R0 == Address of block to release
;-----------------------------------------------------------------------------------------------

ReturnHeap
	STMFD	R13!,{R1-R11,R14}		; R0 -> Block

	MOV	R2,R0				; Release the block from the heap
	LDR	R1,Himem
	MOV	R0,#3
	SWI	OS_Heap

	LDR	R4,HeapSize			; R4 = The current heap size
	LDR	R3,MinHeap			; R3 = The minimum heap size
	SUB	R3,R3,R4			; R3 = R3-R4 (ie max shrink)
	MOV	R0,#5
	SWI	XOS_Heap
	CMP	R3,#0				; ABS(shrink)
	RSBLT	R3,R3,#0
	SUB	R4,R4,R3			; Update and store new heap-size
	STR	R4,HeapSize
	CMP	R3,#0
	BLE	ReturnExit

	LDR	R0,AppSize			; The heap changed size, so alter wimpslot.
	ADD	R0,R0,R4
	MOV	R1,#-1
	SWI	Wimp_SlotSize

ReturnExit
	LDMFD	R13!,{R1-R11,R15}

;-----------------------------------------------------------------------------------------------
; External
; Initialse code and WimpSlot heap
;-----------------------------------------------------------------------------------------------

Init
	STMFD	R13!,{R0-R3,R14}		; Initialize the code...

	ADR	R0,IOArea			; Point to IO file buffer
	STR	R0,IOPointer

	MOV	R0,#-1				; Get app wimpslot size
	MOV	R1,#-1
	SWI	Wimp_SlotSize
	STR	R0,AppSize

	LDR	R3,HeapSize
	ADD	R3,R3,R0
	MOV	R0,R3
	SWI	Wimp_SlotSize
	CMP	R0,R3
	BHS	InitSpaceOK

InitSpaceFail
	ADR	R0,InitSpaceError		; Point to error message
	STR	R0,Param3
	MOV	R0,#-1				; Return -1
	STR	R0,Param1
	B	InitExit

InitSpaceError
	DCB	"NoSlot",13
	ALIGN

InitSpaceOK
	MOV	R0,#0				; Initialise the heap
	LDR	R1,Himem
	LDR	R3,HeapSize
	SWI	OS_Heap

InitExit
	LDMFD	R13!,{R0-R3,R15}

	ALIGN

IOArea
	END
