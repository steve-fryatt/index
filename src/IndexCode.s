REM >IndexCode
REM Squashed Load & Save routines for !Index
REM WildCard Comparison routines
REM Memory Management
REM
REM Index 1.06a
REM
REM Requires BAX extensions
:
ON ERROR PRINT REPORT$+" at line "+STR$(ERL) : END
:
LIBRARY "BASIC:Debug"
:
file$="<Index$Dir>.Code"
:
sp=13
link=14
pc=15
iosize=1024
:
code_size%=6000
:
DIM code% code_size%
:
FOR pass%=8 TO 10 STEP 2
P%=code%
L%=code%+code_size%
[OPT pass%
.param1   EQUD      0                             \ Parameter 1 (!code%)
.param2   EQUD      0                             \ Parameter 2 (code%!4)
.param3   EQUD      0                             \ Parameter 3 <code%!8)

.heap     EQUD      0                             \ Pointer to heap put here
.himem    EQUD      0                             \ Pointer to HIMEM put here

.btable   B         fileinfo                      \ +20 Branch to file info code
          B         loadfile                      \ +24 Branch lo load file code
          B         savefile                      \ +28 Branch to save file code
          B         compstrings                   \ +32 Branch to string compare code
          B         init                          \ +36 Branch to initialization code
          B         searchall                     \ +40 Branch to search code
          B         claimext                      \ +44 Branch to claim code
          B         releaseext                    \ +48 Branch to release code

.heapsize EQUD      24                            \ Size of WimpSlot heap
.minheap  EQUD      24                            \ Minimum size of heap
.appsize  EQUD      0                             \ Size of application
.filename EQUD      0                             \ Pointer to file name
.iblock   EQUD      0                             \ Pointer to save from block
.ibsize   EQUD      0                             \ Save block size
.handle   EQUD      0                             \ File handle
.workarea EQUD      0                             \ Pointer to heap area
.ioptr    EQUD      0                             \ Pointer to ioarea

\-----------------------------------------------------------------------------------------------
\ Internal
\ General file opening routine
\
\ R0 -> Filename to open
\
\ File handle stored at handle)
\-----------------------------------------------------------------------------------------------

.openfile
          STMFD     (sp)!,{R1-R7,link}            \ R0 -> Filename

          MOV       R1,#0                         \ Zero the pointers
          STR       R1,handle
          STR       R1,workarea

          MOV       R1,R0                         \ R1 -> Filename
          MOV       R0,#20
          SWI       "OS_File"                     \ Get file information

          CMP       R0,#1                         \ Check that a file has been read
          BEQ       isfile

.notfile
          ADR       R0,notfileerror               \ Point to error message
          STR       R0,param3
          MVN       R0,#NOT-1                     \ Return -1
          STR       R0,param1
          B         exitopen

.notfileerror
          EQUS      "NotFile"
          EQUB      13
          ALIGN

.indextype
          EQUD      &0E1                          \ The index filetype

.isfile
          LDR       R0,indextype                  \ Check the file is an index
          CMP       R0,R6
          BEQ       isindex

.notindex
          ADR       R0,notindexerror              \ Point to error message
          STR       R0,param3
          MVN       R0,#NOT-1                     \ Return -1
          STR       R0,param1
          B         exitopen

.notindexerror
          EQUS      "NotIndex"
          EQUB      13
          ALIGN

.squashhead
          EQUS      "SQSH"                        \ The first word in a squashed file

.isindex
          MOV       R0,#&40                       \ Open the file
          SWI       "OS_Find"

          CMP       R0,#0                         \ Check it's OK
          BEQ       notindex
          STR       R0,handle                     \ Save the file handle

          LDR       R0,ioptr                      \ Get the first 20 bytes of the file
          MOV       R1,#20
          BL        getbytes
          BVS       badixfile

          LDR       R2,squashhead                 \ Check header
          LDR       R3,ioarea
          CMP       R2,R3
          BNE       badixfile

          B         exitopen                      \ Done!

.badixfile
          ADR       R0,badixerror                 \ Point to error message
          STR       R0,param3
          MVN       R0,#NOT-1                     \ Return -1
          STR       R0,param1
          B         exitopen

.badixerror
          EQUS      "BadIndex"
          EQUB      13
          ALIGN

.exitopen
          LDMFD     (sp)!,{R1-R7,pc}              \ Return

\-----------------------------------------------------------------------------------------------
\ External
\ File information routine
\
\ P1 -> Filename of file to be examined
\
\ P1 == File size (bytes)
\-----------------------------------------------------------------------------------------------

.fileinfo
          STMFD     (sp)!,{R0-R11,link}           \ P1 -> Filename

          LDR       R0,param1                     \ R0 -> Filename
          BL        openfile

          MVN       R1,#NOT-1                     \ Exit if an error occurred
          CMP       R0,R1
          BEQ       exitinfo

          LDR       R0,ioptr                      \ Get size
          LDR       R1,[R0,#4]
          STR       R1,param1

.exitinfo
          BL        unclaim                       \ Close the file; de-allocate memory

          LDMFD     (sp)!,{R0-R11,pc}             \ Return to BASIC

\-----------------------------------------------------------------------------------------------
\ External
\ Load file routine
\
\ P1 -> Filename of file to be loaded
\ P2 == Address to load file
\-----------------------------------------------------------------------------------------------

.loadfile
          STMFD     (sp)!,{R0-R11,link}           \ P1 -> filename; P2 = address

          LDR       R0,param1                     \ R0 -> filename
          LDR       R4,param2                     \ R4 = address to load to
          BL        openfile

          MVN       R1,#NOT-1                     \ Exit if an error occurred
          CMP       R0,R1
          BEQ       exitload

          LDR       R9,ioptr                      \ R9 -> io work area

          MOV       R0,#%1000                     \ Find out how much work area we need
          MOV       R1,#iosize
          SWI       "Squash_Decompress"

          MOV       R3,R0                         \ Claim the workarea
          LDR       R1,heap
          MOV       R0,#2
          SWI       "XOS_Heap"
          STR       R2,workarea

          CMP       R2,#0                         \ Check we got some space
          BNE       heapok

.heapfail
          ADR       R0,heapfailerror              \ Point to error message
          STR       R0,param3
          MVN       R0,#NOT-1                     \ Return -1
          STR       R0,param1
          B         exitload

.heapfailerror
          EQUS      "DecompHeap"
          EQUB      13
          ALIGN

.heapok
          LDR       R5,[R9,#4]                    \ R5 = data length
          MOV       R3,#0                         \ R3 = Bytes left in input buffer
          MOV       R8,#0                         \ R8 = 0 = Start

.deloop
          CMP       R3,#0                         \ If there are any bytes left,
          BEQ       noshuffle                     \ we must shuffle them down
          LDR       R1,ioptr                      \ to the start of the input
          ADD       R2,R1,#iosize                 \ buffer, so that they can be used
          SUB       R2,R2,R3
          MOV       R9,R3

.shuffle
          LDRB      R0,[R2],#1                    \ Do the shuffle!
          STRB      R0,[R1],#1
          SUBS      R9,R9,#1
          BNE       shuffle

.noshuffle
          LDR       R0,ioptr                      \ Fill up input buffer from file
          ADD       R0,R0,R3
          RSB       R1,R3,#iosize
          BL        getbytes                      \ (RO = not got)

          MOVS      R9,R0                         \ Save the no of bytes left (& set continue%)
          RSB       R3,R0,#iosize                 \ R3 = No of bytes in input buffer

          MOV       R0,R8
          ORREQ     R0,R0,#%10                    \ RO = start% OR (continue%<<1)

          LDR       R1,workarea                   \ R1 -> Internal workarea
          LDR       R2,ioptr                      \ R2 -> Data to be decompressed
                                                  \ R4 -> Place to decompress to
                                                  \ R5 = Decompress area size

          SWI       "XSquash_Decompress"          \ Do-o-o it!
          BVC       decompok

.decompfail
          ADR       R0,decompfailerror            \ Point to error message
          STR       R0,param3
          MVN       R0,#NOT-1                     \ Return -1
          STR       R0,param1
          B         exitload

.decompfailerror
          EQUS      "DecompErr"
          EQUB      13
          ALIGN

.decompok
          MOV       R8,#1                         \ Show that we have started

          CMP       R9,#0                         \ Keep going until we finish
          BEQ       deloop

.exitload
          BL        unclaim                       \ Close the file, de-allocate memory

          LDMFD     (sp)!,{R0-R11,pc}             \ Return to BASIC

\-----------------------------------------------------------------------------------------------
\ External
\ File save routine
\
\ P1 -> Filename of file to be saved
\ P2 == Start address in memory fo data
\ P3 == End address in memory of data
\-----------------------------------------------------------------------------------------------

.savefile
          STMFD     (sp)!,{R0-R11,link}           \ P1 -> Filename; P2 = Start address
                                                  \ P3 = End address
          LDR       R0,param1                     \ Load in parameters
          LDR       R1,param2
          LDR       R2,param3

          SUBS      R2,R2,R1                      \ Check that the addresses make sense
          BGT       memok

.badmem
          ADR       R0,badmemerror                \ Point to error message
          STR       R0,param3
          MVN       R0,#NOT-1                     \ Return -1
          STR       R0,param1
          B         exitsave

.badmemerror
          EQUS      "BadSaveParam"
          EQUB      13
          ALIGN

.memok
          STR       R0,filename                   \ Store filename, and mem addresses
          STR       R1,iblock
          STR       R2,ibsize

          MOV       R1,#0                         \ Zero the pointers
          STR       R1,workarea
          STR       R1,handle

          MOV       R0,#%1000                     \ Find size of workspace
          MOV       R1,R2
          SWI       "Squash_Compress"

          MOV       R3,R0                         \ Claim the workarea
          LDR       R1,heap
          MOV       R0,#2
          SWI       "XOS_Heap"
          STR       R2,workarea

          CMP       R2,#0                         \ Check we got some space
          BNE       svheapok

.svheapfail
          ADR       R0,svheapfailerror            \ Point to error message
          STR       R0,param3
          MVN       R0,#NOT-1                     \ Return -1
          STR       R0,param1
          B         exitsave

.svheapfailerror
          EQUS      "CompHeap"
          EQUB      13
          ALIGN

.svheapok
          MOV       R0,#11                        \ Create a new, blank, file
          LDR       R1,filename
          LDR       R2,indextype
          MOV       R4,#0
          MOV       R5,#0
          SWI       "OS_File"
          BVC       created

.createfail
          ADR       R0,createerror                \ Point to error message
          STR       R0,param3
          MVN       R0,#NOT-1                     \ Return -1
          STR       R0,param1
          B         exitsave

.createerror
          EQUS      "NoFile"
          EQUB      13
          ALIGN

.created
          MOV       R0,#&C0                       \ Open the file for I/O
          LDR       R1,filename
          SWI       "XOS_Find"
          BVS       createfail
          STR       R0,handle

          LDR       R0,squashhead                 \ Get file header details
          LDR       R1,ibsize
          MOV       R2,#0                         \ The load and execution address:
          MOV       R3,#0                         \ zero as they are meaningless here!
          MOV       R4,#0

          LDR       R9,ioptr                      \ Save header
          STMIA     R9,{R0-R4}
          MOV       R0,#20
          BL        putbytes

          LDR       R1,workarea                   \ Set up for compress loop
          LDR       R2,iblock
          LDR       R3,ibsize
          MOV       R9,#0

.comploop
          MOV       R0,R9
          LDR       R4,ioptr
          MOV       R5,#iosize
          SWI       "XSquash_Compress"
          BVC       compok

.compfail
          ADR       R0,compfailerror              \ Point to error message
          STR       R0,param3
          MVN       R0,#NOT-1                     \ Return -1
          STR       R0,param1
          B         exitsave

.compfailerror
          EQUS      "CompErr"
          EQUB      13
          ALIGN

.compok
          MOV       R9,R0
          RSB       R0,R5,#iosize
          BL        putbytes

          CMP       R9,#0
          MOVNE     R9,#1
          BNE       comploop

.exitsave
          BL        unclaim                       \ Close the file, de-allocate memory

          LDMFD     (sp)!,{R0-R11,pc}             \ Return to BASIC


.getbytes
          STMFD     (sp)!,{R1-R4,link}            \ R0 -> Work area; R1 = No. of bytes

          MOV       R3,R1
          MOV       R2,R0
          LDR       R1,handle
          MOV       R0,#4
          SWI       "OS_GBPB"
          MOV       R0,R3

          LDMFD     (sp)!,{R1-R4,pc}              \ Return; R0 = Bytes not transferred


.putbytes
          STMFD     (sp)!,{R0-R3,link}            \ R0 = Bytes to write

          MOV       R3,R0
          MOV       R0,#2
          LDR       R1,handle
          LDR       R2,ioptr
          SWI       "OS_GBPB"

          LDMFD     (sp)!,{R0-R3,pc}


.unclaim
          STMFD     (sp)!,{R0-R2,link}

          MOV       R0,#3                         \ Deallocate heap area
          LDR       R1,heap
          LDR       R2,workarea
          CMP       R2,#0
          SWINE     "OS_Heap"

          MOV       R0,#0                         \ Close the open file
          LDR       R1,handle
          CMP       R1,#0
          SWINE     "OS_Find"

          LDMFD     (sp)!,{R0-R2,pc}              \ Return

\-----------------------------------------------------------------------------------------------
\ External
\ String comparison routine
\
\ P1 -> First string (original, lower case, contains wildcards)
\ P2 -> Second string (string to compare, any case, no wildcards)
\
\ P1 == 0 - strings different | 1 - strings equal
\-----------------------------------------------------------------------------------------------

.compstrings
          STMFD     (sp)!,{R0-R11,link}           \ P1 -> String 1; P2 -> String 2

          LDR       R0,param1                     \ R0 = s1 (ie R0 -> String 1)
          LDR       R1,param2                     \ R1 = s2 (ie R1 -> String 2)

          BL        stringcomp                    \ Do comparison

          STR       R8,param1                     \ Save result

          LDMFD     (sp)!,{R0-R11,pc}             \ Return to BASIC

\-----------------------------------------------------------------------------------------------
\ Internal
\ String comparison routine
\
\ R0 -> First string (original, lower case, contains wildcards)
\ R1 -> Second string (string to compare, any case, no wildcards)
\
\ R8 == 0 - strings different | 1 - strings equal
\-----------------------------------------------------------------------------------------------

.stringcomp
          STMFD     (sp)!,{R0-R7,link}            \ Subroutine; R0 -> s1; R1 -> s2

          LDRB      R2,[R0]                       \ R2 = c1
          LDRB      R3,[R1]                       \ R3 = c2

          CMP       R3,#ASC("A")                  \ Convert c2 TO lower case
          BLT       islower
          CMP       R3,#ASC("Z")
          BGT       islower
          ADD       R3,R3,#32

.islower                                          \ c1 & c2 are both lower case
          CMP       R3,#32                        \ If c2 is null, check match
          BGE       notnull

.striploop
          LDRB      R5,[R0]                       \ While s1 = '*'
          CMP       R5,#ASC("*")
          BNE       endstrip
          ADD       R0,R0,#1
          B         striploop

.endstrip
          MOV       R8,#0                         \ return (*s1 == 0)
          LDRB      R5,[R0]
          CMP       R5,#32
          MOVLT     R8,#1
          LDMFD     (sp)!,{R0-R7,pc}

.notnull
          CMP       R2,R3                         \ If (c1 == c2 || c1 == '#')
          CMPNE     R2,#ASC("#")
          BNE       notequal

          ADD       R0,R0,#1                      \ Return strcmp(s1+1, s2+1)
          ADD       R1,R1,#1
          BL        stringcomp
          LDMFD     (sp)!,{R0-R7,pc}

.notequal
          CMP       R2,#ASC("*")                  \ if (c1 == '*')

          MOVNE     R8,#0                         \ If not, return FALSE
          LDMNEFD   (sp)!,{R0-R7,pc}

          LDRB      R5,[R0,#1]                    \ If s1+1 == 0, return TRUE
          CMP       R5,#32
          MOVLT     R8,#1
          LDMLTFD   (sp)!,{R0-R7,pc}

          MOV       R4,#0                         \ ok = FALSE
          ADD       R0,R0,#1

.whilewild
          CMP       R4,#0                         \ while (!ok && *S2 != 0)
          BNE       endwild
          LDRB      R5,[R1]
          CMP       R5,#32
          BLT       endwild

          BL        stringcomp                    \ ok = ok OR strcmp(s1+1, s2)
          ORR       R4,R4,R8

          ADD       R1,R1,#1                      \ s2++

          B         whilewild

.endwild
          MOV       R8,R4                         \ return OK
          LDMFD     (sp)!,{R0-R7,pc}

.searchall

\-----------------------------------------------------------------------------------------------
\ External
\ Claim WimpSlot block
\
\ P1 == Size of block being claimed
\
\ P1 == Address of block | -1 - claim failed
\-----------------------------------------------------------------------------------------------

.claimext
          STMFD     (sp)!,{link}
          LDR       R0,param1
          BL        claimheap
          STR       R0,param1
          LDMFD     (sp)!,{pc}

\-----------------------------------------------------------------------------------------------
\ External
\ Release WimpSlot block
\
\ P1 == Address of block to release
\-----------------------------------------------------------------------------------------------

.releaseext
          STMFD     (sp)!,{link}
          LDR       R0,param1
          BL        returnheap
          LDMFD     (sp)!,{pc}

\-----------------------------------------------------------------------------------------------
\ Internal
\ Claim WimpSlot block
\
\ R0 == Size of block being claimed
\
\ R0 == Address of block | -1 - claim failed
\-----------------------------------------------------------------------------------------------

.claimheap
          STMFD     (sp)!,{R1-R5,link}            \ R0 = size required

          ADD       R5,R0,#8                      \ Check to see if there is a free block
          MOV       R0,#1                         \ that is big enough
          LDR       R1,himem
          SWI       "OS_Heap"
          CMP       R2,R5
          BHS       enoughfree

.growslot
          LDR       R0,appsize                    \ If there isn't, try and extend the
          LDR       R1,heapsize                   \ WimpSlot
          ADD       R0,R0,R1
          ADD       R0,R0,R5
          MOV       R3,R0
          MVN       R1,#NOT-1
          SWI       "Wimp_SlotSize"

          CMP       R0,R3                         \ Check that the slot was extended.
          BHS       claimok

.claimnotok
          SUB       R0,R3,R5
          MVN       R1,#NOT-1
          SWI       "Wimp_SlotSize"

          MVN       R0,#NOT-1
          B         claimexit

.claimok
          MOV       R0,#5                         \ If it was, extend the heap accordingly.
          LDR       R1,himem
          MOV       R3,R5
          SWI       "OS_Heap"
          LDR       R0,heapsize
          ADD       R0,R0,R5
          STR       R0,heapsize

.enoughfree
          MOV       R0,#2                         \ Finally, allocate the block.
          SUB       R3,R5,#8
          SWI       "XOS_Heap"
          MVNVS     R2,#NOT-1
          MOV       R0,R2

.claimexit
          LDMFD     (sp)!,{R1-R5,pc}              \ Return; R0 -> Block

\-----------------------------------------------------------------------------------------------
\ Internal
\ Release WimpSlot block
\
\ R0 == Address of block to release
\-----------------------------------------------------------------------------------------------

.returnheap
          STMFD     (sp)!,{R1-R11,link}           \ R0 -> Block

          MOV       R2,R0                         \ Release the block from the heap
          LDR       R1,himem
          MOV       R0,#3
          SWI       "OS_Heap"

          LDR       R4,heapsize                   \ R4 = The current heap size
          LDR       R3,minheap                    \ R3 = The minimum heap size
          SUB       R3,R3,R4                      \ R3 = R3-R4 (ie max shrink)
          MOV       R0,#5
          SWI       "XOS_Heap"
          CMP       R3,#0                         \ ABS(shrink)
          RSBLT     R3,R3,#0
          SUB       R4,R4,R3                      \ Update and store new heap-size
          STR       R4,heapsize
          CMP       R3,#0
          BLE       returnexit

          LDR       R0,appsize                    \ The heap changed size, so alter wimpslot.
          ADD       R0,R0,R4
          MVN       R1,#NOT-1
          SWI       "Wimp_SlotSize"

.returnexit
          LDMFD     (sp)!,{R1-R11,pc}

\-----------------------------------------------------------------------------------------------
\ External
\ Initialse code and WimpSlot heap
\-----------------------------------------------------------------------------------------------

.init
          STMFD     (sp)!,{R0-R3,link}            \ Initialize the code...

          ADR       R0,ioarea                     \ Point to IO file buffer
          STR       R0,ioptr

          MVN       R0,#NOT-1                     \ Get app wimpslot size
          MVN       R1,#NOT-1
          SWI       "Wimp_SlotSize"
          STR       R0,appsize

          LDR       R3,heapsize
          ADD       R3,R3,R0
          MOV       R0,R3
          SWI       "Wimp_SlotSize"
          CMP       R0,R3
          BHS       initspaceok

.initspacefail
          ADR       R0,initspaceerror             \ Point to error message
          STR       R0,param3
          MVN       R0,#NOT-1                     \ Return -1
          STR       R0,param1
          B         exitinit

.initspaceerror
          EQUS      "NoSlot"
          EQUB      13
          ALIGN

.initspaceok
          MOV       R0,#0                         \ Initialise the heap
          LDR       R1,himem
          LDR       R3,heapsize
          SWI       "OS_Heap"

.exitinit
          LDMFD     (sp)!,{R0-R3,pc}

          ALIGN

.ioarea
]
NEXT pass%
:
SYS "OS_File",10,file$,&FFD,,code%,P%
:
PRINT "Code compiled and saved as: "file$
PRINT "Size: ";P%-code%;" bytes"
