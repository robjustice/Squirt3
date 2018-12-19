        .PROC SCREENWRITER
		.SEGMENT "RAM"

; if a basic file is selected, then load Basic.system and make the file
;  name part of the auto launch feature in Basic.system
; If a text file, src file or mousetext file is selected, then display it
; if a system file is selected, then load it @$2000 and launch it
; if a directory is selected, then enter it and display its contents
; if a binary file is selected then special case scenarios.
;   if load address = $2000 or $4000 and file length = $2000, $1FF8 or $4000
;     then view it as a graphic
;   if load address < 7 or > 8 then load Basic.system and save
;     PATH in autolaunch
;   if load address = 7 or 8, then move loader to $300, load file
;   and launch it

; Zero Page used
; $0.5 - Read Block Parm
;    4.5 read count
; $6.7 - Dir Block address ptr ($2000)
; $8.9 - File list adr ($3000)
; $14.18 - Page info ptrs
; $80.85 - Open Parm
; $A1.A1 - Close Parm
; $E0 - file counter
; PAGECNT - page counter
; $E2 - temp usage in Txt file display
; $E3.E4 - temp ptr
; $E8.E9 - Txt file display ptrs
; $EA - Graphics flag
; $EB.EF - End of File Parm
; $F8.FF - Read Parm

;
; Interpreter Header
;
CODESTART   =       $3FFD       ; Code begins at $4000
        .ORG    CODESTART-$0E   ;   Leave 12 bytes for header
                                ;and three for the jmp

        .BYTE   "SOS NTRP"      ; label for SOS.INTERP
        .WORD   $0000           ; opt_header_length = 0
        .WORD   CODESTART       ; loading_address
        .WORD   CODELEN         ; code_length

        JMP     BEGIN           ; Jump to beginning of code
;
;       Macros, Equates, and Global Data Area
;
;       The syntax for a SOS call using the macro below is
;
;       SOS     call_num, parameter_list pointer
;
        .MACRO  SOS CALL,PARAM  ; Macro def for SOS call block
        BRK                     ; Begin SOS call block
        .BYTE   CALL            ; call_num
        .WORD   PARAM           ; parameter_list pointer
        .ENDMACRO               ; end of macro definition
;
;       Here are equates for SOS call_nums:
;File
VOLUME        = $C5
SET_PREFIX    = $C6
GET_PREFIX    = $C7
OPEN          = $C8
READ          = $CA             ; call_num for READ
WRITE         = $CB             ; call_num for WRITE
CLOSE         = $CC
SET_MARK      = $CE
GET_EOF       = $D1

;Device
D_READ        = $80
D_STATUS      = $82             ; call_num for D_STATUS
D_CNTL  =       $83             ; call_num for D_CONTROL
D_INFO  =       $85             ; call_num for D_INFO

;Memory
REQUEST_SEG   = $40
RELEASE_SEG   = $45
  
;Utility
TERMINATE     = $65

;       Here are more equates:
FALSE     = $00
TRUE      = $80
ESC       = $1B   ;escape char
LARROW    = 08    ;left arrow
RARROW    = 09    ;right arrow
UARROW    = 11    ;up arrow
DARROW    = 10    ;down arrow

BREG    =       $FFEF           ; Bank register

DIRPTR    = $06     ;$6.7 - Dir Block address ptr ($2000)
FILEPTR   = $08     ;$8.9 - File list adr ($3000)
KEYVALUE  = $1E     ;key press value
PAGECNT   = $E1     ;page counte

READBUFP  = $00     ;zero page indirect pointer for file read
READBUFPX = $1601   ;xbyte

TXTFILEP  = $02     ;zero page indirect pointer for text file output
TXTFILEPX = $1603   ;xbyte


;need to page align these, just add at the start for now and take 3 of the org
DEVBUFF:    .RES  $200     ;sos device name buffer ;16 devs, 16 bytes each
FILEBUFF:   .RES  $200     ;sos volumes online (was $2000)
FILELIST:   .RES  $500     ;file list/read buffer (was $3000)

PATH:       .RES  100

DEVNBUFF:   .RES  16       ;hold devnum for each volume name
CURRDEVN:   .RES  1



;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;
;       These variables are used for communication between the
;       main program and the OPENCONS subroutine.
;
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


CNAME:   .BYTE   $08             ; name_length
         .BYTE   ".CONSOLE"      ; pathname of console
REF:     .BYTE   $00             ; Console ref_num
CONSNUM: .BYTE   $00             ; Console dev_num

;
; CONSOLE in/out buffer
;
BUFFER:   .BYTE   $00, $0A        ; data buffer with trailing LF

;
; SOS call parameter lists
;
FENLIST: .BYTE   $01             ; 1 parameter for SET_FENCE
         .BYTE   $00             ;   fence = 0

RCLIST:  .BYTE   $04             ; 4 parameters for READ
RREF:    .BYTE   $00             ;   ref_num
         .WORD   BUFFER          ;   data_buffer pointer
         .WORD   $0001           ;   request_count
RCNT:    .WORD   $0000           ;   transfer_count

WPLIST:  .BYTE   $03             ; 3 parameters for WRITE
WREF:    .BYTE   $00             ;   ref_num (from OPEN call)
         .WORD   BUFFER          ;   data_buffer pointer
WCNT:    .WORD   $0001           ;   request_count


;Set Cursor Vertical position
WCVPLST: .BYTE   $03             ; 3 parameters for WRITE
WCVREF:  .BYTE   $00             ;   ref_num (from OPEN call)
         .WORD   VBUFFER         ;   data_buffer pointer
         .WORD   $0002           ;   request_count

VBUFFER:  .BYTE   25            ; data buffer for vert cursor position
VERTPOS:  .BYTE   00

;Set Cursor Horizontal position
WCHPLST: .BYTE   $03             ; 3 parameters for WRITE
WCHREF:  .BYTE   $00             ;   ref_num (from OPEN call)
         .WORD   HBUFFER         ;   data_buffer pointer
         .WORD   $0002           ;   request_count

HBUFFER:  .BYTE   24            ; data buffer for horiz cursor position
HORIZPOS: .BYTE   00

OPENPARM:   .BYTE 04
            .WORD PATH    ;pathname pointer - PATH
OFILREFN:   .BYTE 00          ;ref_num result
            .WORD $0000       ;option_list pointer
            .BYTE $00         ;length

;OPENOPTL:   .BYTE $01         ;req_access - read only
;            .BYTE $04         ;pages - use io buffer
;            .WORD $2000       ;io buffer pointer

;OnlineParm: .BYTE $02          ;Prodos
;            .BYTE $00         ;unit_num 0=get all online volumes
;            .WORD $2000       ;data_buffer result
            
D_InfoParm: .BYTE 04
DInfoDnum:  .BYTE $00         ;dev_num
DINFONAMEP: .WORD DEVBUFF    ;dev_name pointer
            .WORD D_OPTLIST   ;option_list pointer
            .BYTE $03         ;length

D_OPTLIST:  .BYTE $00         ;slot number
            .BYTE $00         ;unit number
DEVTYPE:    .BYTE $00         ;dev type
            
VOLPARM:    .BYTE 04
DEVBUFFP:   .WORD $0000       ;dev_name pointer
FILEBUFFP:   .WORD $0000       ;vol_name pointer result
TOTBLKS:    .WORD $0000       ;total blocks result
FREEBLKS:   .WORD $0000       ;free blocks result


PRFXPARM: .BYTE 1
            .WORD PATH     ;PATH pointer

D_READPRM: .BYTE 5         ;#params
DRDDEVNUM: .RES 1          ;dev_num
           .WORD FILEBUFF   ;data_buffer pointer
           .WORD $200      ;request_count - read in only 1 block
READBLK:   .WORD $0000     ;block
           .RES 2          ;transfer count - returns actual # of bytes read

; $0080
OPENPRM2:  .BYTE 4         ;80    ;
OPENPRM2P: .WORD PATH      ;81 82 ;pathname pointer - PATH
O2REFNUM:  .BYTE 00        ;83    ;ref_num result
           .WORD $0000     ;84 85 ;option_list pointer
           .BYTE $00       ;86    ;length

; $EB.EF - End of File Parm
GTEOFPRM:  .BYTE 2    ;e0     ;#params
EOFREFNM:  .RES 1     ;e1     ;ref_num
EOFRESULT: .RES 4     ;e2..e5 ;EOF             

; $F8.FF - Read Parm
READPARM:  .BYTE 4         ;f8    ;#params
RDREFNUM:  .RES 1          ;f9    ;ref_num
           .WORD READBUFP  ;fa fb ;indirect pointer to load memory
RDREQCNT:  .RES 2          ;fc fd ;request_count
           .RES 2          ;fe ff ;transfer count - returns actual # of bytes read

READPARM2: .BYTE 4         ;#params
           .RES 1          ;ref_num
           .WORD FILEBUFF   ;data_buffer pointer
           .WORD $200      ;request_count - read in only 1 block
           .RES 2          ;transfer count - returns actual # of bytes read

SETMARKPRMS: .BYTE 3       ;#params
             .RES 1        ;ref_num
             .BYTE $00     ;base - absolute
             .RES 4,00     ;mark

REQMEMPRM: .BYTE 4         ;#params
           .BYTE $01       ;base  - bank 1 
           .BYTE $21       ;      - page $21 (2100)
           .BYTE $01       ;limit - bank 1 
           .BYTE $9F       ;      - page $9F (9F00)
           .BYTE $10       ;seg_id
           .BYTE $00       ;seg_num - result
           
RELMEMPRM: .BYTE 1         ;#params
           .BYTE $00       ;seg_num - result

CLOSEPRM:  .BYTE 1         ;#params
CLREFNUM:  .BYTE $00       ;ref_num

D_STATCURS: .BYTE 3        ;#params
DSTATDEVN:  .BYTE $00      ;dev_num
            .BYTE 16       ;status_code - cursor pos
            .WORD CURHPOS  ;status_list pointer

CURHPOS:    .BYTE $00
CURVPOS:    .BYTE $00
;*****************************************************************

;
;       Main Program Code
;
BEGIN    =       *
         JSR     OPENCONS        ; Open .CONSOLE
         JSR     GETDNUM         ; Get dev_num
         JSR     SETCONS         ; Disable echo
         JSR     ARMCTRLQ        ; Arm attention event
         SOS     $60, FENLIST    ; Set event fence to 0:
                                 ;   here we coded "60" directly

		 LDA     REF             ; Set up ref_num
         STA     RREF            ;   for reads
         STA     WREF            ;   and writes
         STA     WCVREF          ;   and cursor pos vert writes
         STA     WCHREF          ;   and cursor pos horiz writes

         
;L1000: LDA $C082     ; make sure ROM is in and not Pdos8
; STA $C001
; LDY #$16
; LDA #0
; STA PAGECNT
;@LOOP: STA $BF58,Y
; DEY
; BNE @LOOP
; LDA #$CE
; STA $BF58
; LDA #1
; STA $BF6F

; lda #<reset
; STA $3F2
; LDA #>reset
; STA $3F3
; JSR $FB6F     ; fix reset byte

;reset: LDY #0
;; STY $02       ; Read Blk
;; STY $0C       ; for CLOSE call
; INY
;; STY $0B       ; for CLOSE call
; INY
; STY $EB       ; part of GetEOF
; INY
; STY $80       ; for OPEN call
; INY
; STY $F8       ; for READ file call
;; LDA #$20
;; STA $03       ; Read Blk @$2000


          LDA  #16       ; Text Mode
          STA  BUFFER
          LDA  #2        ; 80x24
          STA  BUFFER+1
          INC  WCNT      ; send two bytes 
          SOS  WRITE, WPLIST   ; Write out 2 byte
          LDA  #21       ; Cursor Movement Control
          STA  BUFFER
          LDA  #$0F      ; Advance=1,LineFeed=1,Wrap=1,Scroll=1
          STA  BUFFER+1 
          SOS  WRITE, WPLIST   ; Write out 2 byte
	      DEC  WCNT      ;restore wcnt to 1
          JSR  L1210     ; Clear screen and print "Squirt"

          BRK
           .BYTE GET_PREFIX    ; GetPfx
           .WORD PRFXPARM

          LDX PATH   ;first byte contains length
          BEQ L1020
          DEX
          STX PATH
          BCS L1020     ; if error then print all volumes
          JMP L1065     ; otherwise read and display directory

L1020:    LDX #Volumes_Msg-Squirt_Msg
          JSR PRINTMSG     ; print "VOLUMES"
L1042:    LDA #0
          TAY
@LOOP:    STA FILEBUFF,Y      ;zero name buffer??
          STA DEVBUFF,Y       ;zero dev buffer
          DEY
          BNE @LOOP
          STY PATH        ; set prefix length to zero

;get block device names
          LDA #$01          ;start from dev 1
          STA DInfoDnum
          STA CURRDEVN      ;init current devnum to start from 1
          LDA #$00
          STA DINFONAMEP    ;lowbyte to 0
NEXTDEV:  SOS D_INFO,D_InfoParm  ;Get device name
          ;BNE L1068     ; branch if error
          LDA DEVTYPE
          BPL SKIPDEV      ;not a block device
          CLC
          LDA DINFONAMEP
          ADC #$10         ;next name buffer
          STA DINFONAMEP          
SKIPDEV:  INC DInfoDnum    ;next device number
          LDA DInfoDnum
          CMP #$10
          BNE NEXTDEV

;get volume names
          LDA #<DEVBUFF   ;init the pointers
          STA DEVBUFFP
          LDA #>DEVBUFF
          STA DEVBUFFP+1
          LDA #<FILEBUFF
          STA FILEBUFFP
          LDA #>FILEBUFF
          STA FILEBUFFP+1
NEXTDEV2: BRK
           .BYTE VOLUME
           .WORD VOLPARM  ;get volume name
          ;BNE L1068          ; branch if error
          ;LDA FILEBUFFP       
          ;BEQ NOVOL
          CLC
          LDA DEVBUFFP
          ADC #$10         ;next dev name
          STA DEVBUFFP
          CLC          
          LDA FILEBUFFP     ;next vol name
          ADC #$10
          STA FILEBUFFP
          BNE NEXTDEV2

          LDY #$10      ;maximum 16 volumes
          STY $18       ;num volumes? 
          STY $19       ;entry size?
          LDY #$12
          STY $1A
          JSR L1245     ;reset file buffer pointers
          JSR RESVOLP   ;reset dev buffer pointer  (old RESVOLP)
@LOOP2:   JSR DISPNAME     ; display one volume name
          DEC $18       ; counters are auto adjusted
          BNE @LOOP2
          JMP L10E8     ; print "Press a key" and get keypress

L1068:    JMP L119F

L1056:    JSR L1210     ; clear screen and print Squirt
          LDX PATH      ; length of current prefix
          BNE CONT          
          JMP L1020     ; jump to volumes if prefix is zero
 
CONT:     BRK             ; Set Prefix
           .BYTE SET_PREFIX
           .WORD PRFXPARM
          BNE L1068
L1065:    JSR PRTPATH
          BRK              ;Open directory
           .BYTE OPEN
           .WORD OPENPARM
          BNE L1068
          LDA OPENPARM+3   ;file ref_num returned from open
          STA READPARM2+1  ;set for read call
          BRK              ;Read directory
           .BYTE READ
           .WORD READPARM2 ;reads 1 block into FILEBUFF
          BNE L1068          
;          BRK
;           .BYTE CLOSE     ;Close directory
;           .WORD CLOSEPRM
          JSR L1245       ;reset file buffer ptrs
          JSR RESVOLP     ;reset buffer ptrs
          LDY #$25
          LDA (DIRPTR),Y
          INY
          ORA (DIRPTR),Y
          BNE NOTEMPTY  ; directory is not empty
          LDX #DirEmpty_Msg-Squirt_Msg
          JSR PRINTMSG     ; prints <Directory is empty>

NOTEMPTY: LDY #$50      ; gets header block # from the first files parent ptr
          LDA (DIRPTR),Y    ; will be block #0000 if new directory and is empty
          STA $F00      ; blk # of subdirectory header
          INY
          LDA (DIRPTR),Y
          STA $F01      ; blk # of subdirectory header
          LDA #<FILEBUFF+4
          STA $F02
          LDA #>FILEBUFF
          STA $F03
          LDA #$E
          STA $F04
          LDA CURRDEVN     ;$BF30     ;current devnum
          STA DRDDEVNUM
          LDA #$27      ;: # of bytes per entry
          STA $19
L108B:    JSR RESVOLP     ; reset ptr @$2000 after reading in a new block
          LDA READBLK
          LDY READBLK+1
          STA $14
          STY $15
          LDY #2
          LDA ($06),Y
          STA READBLK       ; Read Blk - Blk # lo
          INY
          STY $00
          LDA ($06),Y
          STA READBLK+1     ; Read Blk - Blk # hi
          INY
          STY $06       ; for reading Dir entries at $2004
          LDA #$D       ; # of entries per Dir block
          STA $18
          LDA #$11
          STA $1A

L10B4:    LDY #$10
          LDA (DIRPTR),Y
          BNE L10BC     ; branch if filetype is also 0?

; filetype zero is a NON file

          JSR L11C6
          BCS L10D0     ; always

L10BC:    JSR DISPNAME      ; display another file name
          BCS L10D0         ; branch if a deleted file or subdirectory header
          DEC $E0           ; no more than 57 files per page
          BMI L10E8
L10D0:    DEC $18           ; # of entries in each dir block
          BNE L10B4
          LDA READBLK       ; Block # lo for Read Blk
          ORA READBLK+1     ; Block # hi for Read Blk
          BEQ L10E8         ; no more forward links
PREVPAGE: BRK
           .BYTE READ
           .WORD READPARM2  ;reads next block into FILEBUFF
          BEQ L108B     ; branch if no error

L10E5:    JMP L119F     ; jump to error routine

PRTPATH:  LDX #Path_Msg-Squirt_Msg
          JSR PRINTMSG     ; prints PATH:
          LDX #0
@LOOP:    LDA PATH+1,X
          JSR L1231     ; print the prefix
          INX
          CPX PATH
          BCC @LOOP
          JMP L121A

L10E8:    LDA #0
          TAY
          STA (FILEPTR),Y
          LDA #22
          STA VERTPOS          
          JSR SETCURSV
          LDA #31        ; Clear line cursor is on
          JSR COUT
          LDA #32
          STA HORIZPOS
          JSR SETCURSH
          LDX #PageNm_Msg-Squirt_Msg ; print "Page #"
          JSR PRINTMSG
          LDX PAGECNT
          INX
          TXA
          JSR PRINTDEC

          LDA PAGECNT
          BEQ FWDARW
          LDA #0
          STA HORIZPOS
          JSR SETCURSH
          LDX #PageBk_Msg-Squirt_Msg ; Print "Page Bkwd"
          JSR PRINTMSG

FWDARW:   LDA $18       ; counter for 57 files listed
          BNE MORLTR
          LDA READBLK       ; check for last file of last directory block
          ORA READBLK+1
          BNE MORLTR
          DEC $18       ; force a #$FF if last file of the dir
MORLTR:   LDA $18       ; LC - letter counter
          BMI LC        ; branch if end of directory
; $18 will equal #$FF if at the root block of the subdirectory

; save block #, ptr address and file # of the last file listed for
;   future reference
; one page of these 5 byte ptrs in memory will allow up to 51 pages of
; file listings for a total of 51x57=2907 files in one directory


          LDA #32
          STA HORIZPOS        
          JSR SETCURSH
          LDX #PageNm_Msg-Squirt_Msg ; Print "Page# "
          JSR PRINTMSG
          LDX PAGECNT
          INX
          TXA
          JSR PRINTDEC

          LDA #67
          STA HORIZPOS
          JSR SETCURSH
          LDX #PageFw_Msg-Squirt_Msg ; Print "Page Fwd"
          JSR PRINTMSG

          LDA DIRPTR
          STA $16
          LDA DIRPTR+1
          STA $17
          INC PAGECNT
          LDA PAGECNT       ; page #
          ASL
          ASL
          CLC
          ADC PAGECNT       ; multiply by 5
          DEC PAGECNT
          TAY
          LDX #0
@LOOP:    LDA $14,X
          STA $F00,Y
          INY
          INX
          CPX #5
          BCC @LOOP

LC:       LDX #PressLtr_Msg-Squirt_Msg
          LDA #$16
          JSR L1223     ; print "Press a letter or ESC"
L10F8:    JSR L123C     ; get a key press
          CMP #$91      ; Ctrl-Q
          BNE CHKRT
          JSR $FB39   ;set textmode
          JMP $FF69

          BRK
           .BYTE TERMINATE
           .WORD BYEPARM
BYEPARM:  .BYTE 4
          .RES 6

CHKRT:    CMP #RARROW      ;right arrow
          BNE CHKLFT
          LDA $18
          BMI L10F8     ; branch if end of directory
          INC PAGECNT
          JMP COMMON

CHKLFT:   CMP #LARROW      ; left arrow
          BNE CHKESC
          LDA PAGECNT       ; counter for # of pages
          BEQ L10F8     ; branch if at first page of listed files
          DEC PAGECNT
RELOAD:   JSR RELOAD2   ; get correct ptrs from $4.7 and $18
          LDA READPARM2+1    ;ref_num for directory
          STA SETMARKPRMS+1
          BRK                ;reset to start of directory
           .BYTE SET_MARK
           .WORD SETMARKPRMS
          BCC @L2
          JMP L119F

@L2:       BRK
           .BYTE READ        ;           .BYTE D_READ
           .WORD READPARM2   ;           .WORD D_READPRM

          BCC @L1
          JMP L119F

@L1:      LDA FILEBUFF+2    ; check for more directory blocks
          STA READBLK       ; Read Blk - Blk # lo
          LDA FILEBUFF+3
          STA READBLK+1     ; Read Blk - Blk # hi

COMMON:   LDA #3
          STA $22
          ;LDA #$8C
          ;JSR COUT
          LDA #0        ; this will leave the cursor on the 3rd line
          STA $22
          LDA #'A'
          STA $1D
          LDA #$38
          STA $E0
          LDA #<FILELIST
          STA FILEPTR
          LDA #>FILELIST
          STA FILEPTR+1
          LDA #$11
          STA $1A
          JMP L10D0

CHKESC:   CMP #ESC      ; ESC
          BNE PROCLTR
          JSR L111B     ; back up one directory
RESETPG:  LDA #0
          STA PAGECNT
          JMP L1056     ; display directory

PROCLTR:   STA KEYVALUE  ; holds keypress value
           JSR L1245     ; reset buffer ptrs ($8.9 - $3000)
           LDX PATH
           BNE @1
           LDA #$12      ; for volumes
           .BYTE $2C
@1:        LDA #$15      ; for files
           STA $1A
L1108:     LDA (FILEPTR),Y   ; ($8.9 - $3000) - Y is always 0
           BNE L1111
BELL:      ;JSR $FBDD    ; Bell
           JMP L10F8     ; branch to Get Kepress

L1111:     CMP KEYVALUE   ; Keypress value
           BEQ ETRYFOUND  ; found entry to match keypress
           JSR L125B      ; increment $8.9 by $1A value bytes
           BEQ L1108      ; always

L111B:     LDX PATH      ; backs up one directory
           BEQ RTN1
@LOOP:     LDA PATH,X
           DEX
           CMP #$2F
           BNE @LOOP
           STX PATH     ; X = 0 when at root prefix
RTN1:      RTS

; comes here if a directory is selected

L112E:    AND #$F
          STA $1B        ;name length
          LDX PATH      ; length of prefix
          BNE *+5
          LDY #1
          .BYTE $2C
          LDY #5
          LDA #'/'  
@LOOP:    STA PATH+1,X   ; add name to prefix
          INX
          INY
          LDA (FILEPTR),Y
          DEC $1B        ;name length
          BPL @LOOP
          STX PATH
          LDA (FILEPTR),Y ;grab devnum for volume
          STA CURRDEVN    ;and store in current devnum
          JMP RESETPG

; comes here if entry found
; an entry has the following info
;  letter, storage type, file type, 2 byte load address, PATH length
;  and PATH for a total of $15 bytes per entry

ETRYFOUND: LDY #1
           LDA (FILEPTR),Y
           BMI L112E     ; negative means a directory
           LDX PATH
           BEQ L112E

; default load address is $2000 for all files except games that start
;  with $07FD and $0800
; graphics, text and src files, system files and when Basic.system is loaded

           BRK                 ;request memory for loading the file into 
            .BYTE REQUEST_SEG  ;from SOS, bank0 2100-9FFF
            .WORD REQMEMPRM
           BEQ @1
           JMP L11AB
          
@1:        LDA REQMEMPRM+6      ;copy seg_num so we can release it later
           STA RELMEMPRM+1
           LDA #$00     
           LDX #$02
           STA READBUFP       ; extended address to load at, $0:0200
           STX READBUFP+1
           LDA #$81
           STA READBUFPX      ; xbyte for bank 1/2

; set PATH ptr for OPEN call

           CLC
           LDA FILEPTR
           ADC #5          ; skip past letter, storage type and
           STA OPENPRM2P   ;$81       ; PATH length and binary load adr
           LDA FILEPTR+1
           ADC #0
           STA OPENPRM2P+1 ;$82

; $83.84 - default buffer for OPEN is set at $0A00
; large binary files at $BB00

           LDA #0
;           STA $83   sos allocates its own buffer
           STA $EA
;           LDA #$A
;           STA $84

           INY
           LDA (FILEPTR),Y   ; filetype
           TAX
           CPX #6        ; binary file
           BEQ BINFILE
           JMP CHKBAS

;binary file
BINFILE:  JSR CHKFONT
          JSR OPENIT    ;open file and get EOF
          LDY #3        ; get load address
          LDA (FILEPTR),Y
          STA $FA       ; lo byte
          INY
          LDA (FILEPTR),Y
          STA $FB       ; hi byte
ADJBIN:   CMP #7        ; in case a $07FD load adr is needed
          BCC RELY3     ; Load Basic.system first
          CMP #9
          BCC MOVLDR
          CMP #$20
          BEQ CHKLNGTH
          CMP #$40      ; graphics loaded at $4000
          BNE YESBIN
          LDA #$20
          STA $FB

; a binary file that loads at $2000 or $4000 comes here

CHKLNGTH: LDA $ED       ; File length
 BNE CHKF8
 LDA $EE
 CMP #$20
 BEQ SETMODE
 CMP #$40
 BNE YESBIN
 LDA #$C0
 STA $EA
 BNE RLY

CHKF8: CMP #$F8      ; not a graphics file so let Bas.sys load the
 BNE YESBIN    ; binary file
 LDA $EE
 CMP #$1F
 BNE YESBIN

SETMODE: LDA #$80
 STA $EA
RLY: JMP L1189

; Loader can load big binary games as long as it
; does not extend past $BAFF

MOVLDR: LDY #MOVEND-L1188
@LOOP: LDA L1188,Y
 STA $300,Y
 DEY
 BPL @LOOP
 STY $FC
 LDA #$B2
 STA $FD

 BRK
  .BYTE CLOSE
  .WORD CLOSEPRM  ; CLOSEPARM

 LDA #$BB      ; set OPEN buffer to $BB00
 STA $84
 STA $C000
 LDA #$15
 JSR COUT
 JSR $FE93
 LDA #0
 STA $38
 LDA #3
 STA $39
 LDA #$F0
 STA $31E
 JMP $E000

; load Basic.system @$2000, then put the basic
;  or binary file's name into the autoload at the beginning

CHKBAS:   CPX #$FC       ; bas file
          BEQ CLOSE3
          JMP OPEYIT
RELY3:    BCC CLOSE3-2   ; only $300 loads come here

YESBIN:   ldy #$f
@LOOP:    lda ($83),y   ; the OPEN buffer at $A00 or $BB00
          asl
          ror $1D
          ror $1C
          dey
          bpl @LOOP

          lda $1C
          ora $1D
          beq jptxt     ; 16 hi-bit off ascii bytes
          lda $1C
          and $1D
          cmp #$FF      ; 16 hi-bit on ascii bytes
          bne *+5
jptxt:    jmp VIEWTXT

          LDX #$FC
CLOSE3:   TXA
          PHA
          BRK
           .BYTE CLOSE
           .WORD CLOSEPRM  ; CLOSEPARM

; copies the volumename to $2C0 to append Basic.sys to it
 LDX #2
@LOOP: LDA PATH,X
 STA $2C0,X
 CMP #$2F
 BEQ FWD3
 INX
 CPX PATH
 BCC @LOOP
 LDA #$2F
 BCS *+5       ; always

FWD3: STX $2C0
 STA $2C1
 LDY #0
@LOOP: INX
 LDA BS,Y
 STA $2C0,X
 INY
 CPY #9
 BCC @LOOP
 STX $2C0

 LDA #0
 STA $FA
 LDA #$20
 STA $FB
 LDX PATH
 LDA #$2F
 STA PATH+1,X
 INX
 LDY #0        ; the actual PATH starts at 6
 LDA ($81),Y
 STA $1D
@LOOP2: INY
 LDA ($81),Y   ; then copy the PATH
 STA PATH+1,X
 INX
 DEC $1D
 BNE @LOOP2
 STX PATH
 LDA #$15
 JSR COUT
 LDY #$C0
 LDA #2
 STY $81       ; for OPEN call for Basic.system
 STA $82
 PLA
 TAX

OPEYIT:   JSR OPENIT    ;open file and get EOF (size)
          CPX #$FF      ; system file?
          BEQ L1189
          CPX #$FC      ; applesoft file so open Basic.system
          BEQ L1189
          CPX #6        ; no binary files should fall through
          BEQ L1189
          CPX #$B0      ; src file
          BEQ VIEWTXT
          CPX #7      ; A/// font file
          BEQ VIEWA3FNT
          CPX #4        ; text file
          BNE RETURN

VIEWTXT:   JMP TXTDSP

VIEWA3FNT: JMP A3FONT

RETURN:  BRK
         .BYTE CLOSE
         .WORD CLOSEPRM
         JMP L10F8     ; back to keypress

; This gets moved to $300 to open a very large binary file @$7FD or $800

L1188: BRK
  .BYTE OPEN
  .WORD $0080  ; OPENPARM
 BCS CLOSE2
 LDA $85       ; REFnum for large binary file
 STA $F9

; Only a binary file needs to be loaded from $300
; A system file is loaded at $2000, Basic files and some Binary files
;   load Basic.system first
; text, src and mousetext screen files also come here
; and binary graphics and binary text are also loaded here

L1189:    BRK
           .BYTE READ    ; Read in file
           .WORD $00F8

; NOTE: CloseParm should also be in zero page as it may get
; overwritten by a loaded program
CLOSE2:   PHP
          BRK
           .BYTE CLOSE    ; Close
           .WORD CLOSEPRM
          PLP
relay1:   BCS relay3

CHKBIN:   CPX #6
          BNE CHKSYS

          BIT $EA
          BMI VIEWGR
          BVS VIEWGR
          LDA $FD
          CMP #$28      ; chk if BS is loaded
          BNE *+5
          JMP CHKBAS2+7 ; copy PATH and launch BS
          JMP ($00FA)   ; only binary files that are loaded using the $300
                        ; routine comes here (either $7FD or $800)

VIEWGR:   JMP VIEWHGR

relay3: JSR $FF58     ; forced RTS
 TSX
 LDA $100,X
 CMP #3
 BNE relay2    ; carry will be set since nothing can load under $300
 LDA #$15
 STA VERTPOS
 JSR SETCURSV
 JSR CROUT
; LDA #<(LDER-L1188+$300)
; LDY #>(LDER-L1188+$300)
; JSR $DB3A
;replacement
           LDX #0
L11:       LDA LDER-L1188+$300,X
           BEQ L12
           INX
           JSR COUT
           BNE L11        

L12:       BRK
           .BYTE $00

LDER:      .BYTE $0B,$0D
           .BYTE "ERROR LOADING LARGE FILE. PLEASE REBOOT"
           .BYTE $0D,$00

MOVEND = *

CHKSYS:   CPX #$FF
          BNE CHKBAS2
          JMP $2000     ; launch system file

CHKBAS2: CPX #$FC
 BEQ *+5
 SEC
relay2: BCS L119F

; store the PATH @$2005
 LDX PATH
@LOOP: LDA PATH,X    ; copy the prefix first from PATH
 STA $2006,X
 DEX
 BPL @LOOP
 JMP $2000

OPENIT:   BRK
           .BYTE OPEN     ; Open file or BS
           .WORD OPENPRM2 ; PATH is at $8.9 ptr or to BS
          BNE L119F-4
          LDA O2REFNUM    ;$85
          STA EOFREFNM    ;$EC       ; part of EOF, ref num
          STA RDREFNUM    ;$F9       ; part of READ, ref num
          STA CLREFNUM               ; close refnum
          STA SETMARKPRMS+1          ; setmark refnum
          BRK
           .BYTE GET_EOF ; GetEOFile
           .WORD GTEOFPRM ;$00EB
          LDA EOFRESULT   ;$ED       ; part of EOF
          STA RDREQCNT    ;$FC       ; # of bytes to read lo
          LDA EOFRESULT+1 ;$EE       ; part of EOF
          STA RDREQCNT+1  ;$FD       ; # of bytes to read hi
          RTS

          TAX
          PLA
          PLA
          TXA
L119F:    PHA           ; ERROR routine
          LDA #21
          STA VERTPOS
          JSR SETCURSV
          JSR CROUT
          LDA #29       ; clear from cursor to bottom of screen
          JSR COUT
          PLA
          AND #$B0
          BNE L11AB
          LDX #PathNF_Msg-Squirt_Msg     ; Path Not Found error
          JSR PRINTMSG     ; Print text starting at X
          JSR L123C     ; wait for keypress
          JMP LC

L11AB:    JSR CROUT
          LDX #IOError_Msg-Squirt_Msg     ; I/O error
          JSR PRINTMSG
          LDX #PressKey_Msg-Squirt_Msg
          JSR PRINTMSG     ; print error
          JSR L123C     ; wait for keypress
          JMP CLOSEALL

; This will have to save storage type, file type, file name length and block pt
; Will this routine also work for displaying volume names?

DISPNAME: LDY #0
          LDA ($06),Y   ; storage type
          LDX PATH
          BEQ SKIP
          CMP #$E0
          BCS L11C6     ; skip subdirectory header
SKIP:     TAX
          AND #$F
          BNE L11C9     ; branch if file not deleted or volume not empty
L11C6:    JSR L1266     ; adjust ptr to next entry
          INC CURRDEVN  ;increment the devnum, this one not used
          SEC
          RTS

L11C9:    JSR L1301     ; check when to tab to next column
          LDY #0
          STA (FILEPTR),Y   ; store a letter (capital A to small y)
          JSR COUT
          LDA #'-'      ; print a dash
          JSR COUT
          TXA           ; storage type and name length
          INY
          STA (FILEPTR),Y
          AND #$F
          TAX           ; PATH length
          LDA PATH      ; if 0 then has a volume list instead
          BEQ skipvol
          LDY #$10
          LDA (DIRPTR),Y    ; get filetype
          LDY #2
          STA (FILEPTR),Y
          LDY #$1F      ; get binary starting adr lo
          LDA (DIRPTR),Y
          LDY #3
          STA (FILEPTR),Y
          LDY #$20      ; get binary starting adr hi
          LDA (DIRPTR),Y
          LDY #4
          STA (FILEPTR),Y
          CLC
          LDA FILEPTR
          ADC #4
          STA FILEPTR
          BCC *+4
          INC FILEPTR+1
skipvol:  TXA           ; PATH length
          LDY #1
          STA (FILEPTR),Y
@LOOP:    LDA ($06),Y
          INY
          STA (FILEPTR),Y
          JSR L1231     ; prints each character of a file name
          DEX
          BNE @LOOP
          LDA CURRDEVN
          INY
          STA (FILEPTR),Y  ;store devnum after volume name
          DEY
          INC CURRDEVN  ;inc for next time
@LOOP2:   LDA #' '      ; clears rest of the line
          JSR COUT
          INY
          CPY #$12      ; up to 17 characters
          BCC @LOOP2
          LDA PATH  ; if 0 then has a volume list instead
          BEQ L120A
          LDY #$10
          LDA ($06),Y
          JSR CHKTYPE     ; check for all the filetypes to display
          JSR PRINTMSG     ; one of SYSBINDIRBASTXTCMDSRC
L120A:    JSR L125B     ; advance one entry
          JSR L1266
          LDA #$0D      ; print a return char
          JSR COUT
          RTS
          
L1210:    LDA #01        ; Reset Viewport
          JSR COUT
          LDA #28        ; Clear Viewport
          JSR COUT
          LDA #0         ;reset our cursor positions
          STA HORIZPOS
          STA VERTPOS
          LDX  #0        ; print "SQUIRT"
          BEQ  PRINTMSG

L121A:    LDA #2
L121C:    STA VERTPOS
          JSR SETCURSV
          JMP CROUT

L1236:    LDA #1
          .BYTE $2C
L1221:    LDA #$15
L1223:    JSR L121C     ; VTAB

PRINTMSG: LDA  Squirt_Msg,X
          BMI  L1231           ; last char?(high bit set)
          JSR  COUT
          INX
          BNE  PRINTMSG
L1231:    AND  #$7F            ; mask off the high bit
          JSR  COUT
	      RTS

;L123C:    BIT $C061     ; OA
;          BMI L1240
;GETKEY:   BIT $C000
;          BPL L123C
;          LDA $C010
;          RTS
          
L123C:
GETKEY:   SOS READ,RCLIST
          LDA BUFFER 
          RTS

L1240:    LDA PATH
          BEQ L123C
          LDA #$15
          STA $1A
GOAGAIN:  JSR L1245     ; reset $8.9 to $3000 and $1D to #$41
          JSR L121A     ; set vtab 3
@LOOP:    JSR L1301     ; set column to print in
          CLC
          LDA HORIZPOS
          ADC #$12
          STA HORIZPOS
          JSR SETCURSH
          LDY #0
          LDA (FILEPTR),Y
          BEQ END
          LDA #'$'      ; print a "$"
          JSR COUT
          LDY #3
          LDA (FILEPTR),Y
          TAX
          INY
          LDA (FILEPTR),Y
          JSR $F941     ; print the Aux Type value in hex
          JSR L125B     ; increment $8.9
          JSR CROUT
          CLC
          BCC @LOOP       ; do only files currently listed

END:      BIT $C061
          BMI GOAGAIN

          JSR L1245     ; reset ptrs
          JSR L121A     ; set vtab 3
@LOOP:    JSR L1301     ; set correct col
          CLC
          LDA HORIZPOS
          ADC #$12
          STA HORIZPOS
          LDY #0
          LDA (FILEPTR),Y
          BEQ L123C
          LDY #2
          LDA (FILEPTR),Y
          JSR CHKTYPE     ; print file types
          JSR PRINTMSG
          LDA #' '
          JSR COUT
          JSR COUT
          JSR L125B     ; increment $8.9
          JSR CROUT
          CLC
          BCC @LOOP

RELOAD2:  LDA PAGECNT
          ASL
          ASL
          CLC
          ADC PAGECNT       ; multiply by 5 to get proper ptr to page
          TAY
          LDA $F00,Y
          STA READBLK       ; get block# and file ptr $4.$7
          LDA $F00+1,Y
          STA READBLK+1     ; get block# and file ptr $4.$7
          LDA $F00+2,Y
          STA DIRPTR        ; get block# and file ptr $4.$7
          LDA $F00+3,Y
          STA DIRPTR+1      ; get block# and file ptr $4.$7
                    
          LDA $F00+4,Y
          STA $18       ; and file count
          CLC
          RTS

L1245:    LDA #'A'
          STA $1D
          LDA #$38      ; file count of 57 files
          STA $E0
          LDA #>FILELIST ;#$30 
          LDY #<FILELIST ;#0
          STA FILEPTR+1
          STY FILEPTR
          STY $1609     ;disable extended indirect addressing
          RTS

RESVOLP:  LDA #>FILEBUFF
          LDY #<FILEBUFF
          STA $07
          STY $06
          STY $1607     ;disable extended indirect addressing
          RTS

;reset buffer pointer for DEVBUFF
RESDEVP:  LDA #>DEVBUFF
          LDY #<DEVBUFF
          STA $07
          STY $06
          STY $1607     ;disable extended indirect addressing
          RTS

L125B:    CLC
          LDA FILEPTR
          ADC $1A       ; letter, storage type, file type, bin strt adr
          STA FILEPTR       ; PATH length, PATH
          BCC *+4
          INC FILEPTR+1
          LDY #0
          RTS

L1266:    CLC
          LDA $19    ;entry size?
          ADC $06    ;add to pointer low byte
          STA $06  
          BCC *+4    ;check if carry
          INC $07    ;yes, inc high byte
          LDY #0
          RTS

L1301:    LDA $1D
          LDY #$1B
          CMP #'T'      ; first column is A-S
          BCC L131A
          BEQ L1313
          CMP #'g'      ; 2nd column is T-f
          BCC L1318     ; 3rd column is g-y
          PHP
          LDY #$38
          PLP
          BNE L1318
L1313:    PHA
          JSR L121A     ; sets a vtab 3
          PLA
L1318:    STY HORIZPOS
          JSR SETCURSH
L131A:    INC $1D       ; letter counter 41- C1-F8
          RTS

CHKTYPE:   LDX #$7       ;list length
@LOOP:     CMP TYPELIST,X
           BEQ L132C
           DEX
           BPL @LOOP
           LDX #1        ; default to binary file
L132C:     LDA L1340,X
           TAX
           RTS

TYPELIST: .BYTE $04     ;TXT ASCII Text
          .BYTE $06     ;BIN Binary File
          .BYTE $07     ;FNT Apple /// Font File
;          .BYTE $09     ;BA3 Apple III BASIC Program
          .BYTE $0F     ;DIR SOS/ProDOS Directory
          .BYTE $F0     ;CMD ProDOS Command File
          .BYTE $FC     ;BAS Applesoft BASIC Program
          .BYTE $FF     ;SYS ProDOS-8 System File
          .BYTE $B0     ;SRC Apple IIgs Source Code

; SysBinDirBasTxtCmdSrc

L1340:    .BYTE TYPE_TXT-Squirt_Msg
          .BYTE TYPE_BIN-Squirt_Msg
          .BYTE TYPE_FNT-Squirt_Msg
          .BYTE TYPE_DIR-Squirt_Msg
          .BYTE TYPE_CMD-Squirt_Msg
          .BYTE TYPE_BAS-Squirt_Msg
          .BYTE TYPE_SYS-Squirt_Msg
          .BYTE TYPE_SRC-Squirt_Msg

Squirt_Msg:   .BYTE "SQUIRT"
              .BYTE $8D
Volumes_Msg:  .BYTE $0D
              .BYTE "Volumes:"
              .BYTE $8D
DirEmpty_Msg: .BYTE "<Directory is empty>"
              .BYTE $8D
PressLtr_Msg: .BYTE "Press a letter | <ESC> prev Dir | "
              .BYTE "<OA> Show Bin Start Adr"
              .BYTE $A0

PageFw_Msg:   .BYTE "Page Fwd -"
              .BYTE $BE

PageBk_Msg:   .BYTE "<- Page Bkw"
              .BYTE $E4

PageNm_Msg:   .BYTE "Page#"
              .BYTE $A0

Path_Msg:     .BYTE "Path:"
              .BYTE $A0

TYPE_SYS:     .BYTE "Sy"
              .BYTE $F3   ;s
TYPE_BIN:     .BYTE "Bi"
              .BYTE $EE   ;n
TYPE_FNT:     .BYTE "Fn"
              .BYTE $F4   ;t
TYPE_DIR:     .BYTE "Di"
              .BYTE $F2   ;r
TYPE_BAS:     .BYTE "Ba"
              .BYTE $F3   ;s
TYPE_TXT:     .BYTE "Tx"
              .BYTE $F4   ;t
TYPE_CMD:     .BYTE "Cm"
              .BYTE $E4   ;d
TYPE_SRC:     .BYTE "Sr"
              .BYTE $E3   ;c

IOError_Msg: .BYTE "I/O Error"
 .BYTE $8D
PathNF_Msg:  .BYTE "Path not found"
 .BYTE $8D
PressKey_Msg: .BYTE "Press a key"
 .BYTE $AE

BS:         .BYTE "BASIC_SYS"


VIEWHGR:  STA $C057
          STA $C052
          STA $C054
          BVS L2100

NOTDBL:   STA $C050
          JSR L123C     ; wait for keypress
          STA $C051
          JMP RELOAD

; copy screen to Aux memory

L2100: STA $C000
 LDA #$80
 STA $E0
 LDA #$20
 STA $43
 STA $3D
 LDA #$3F
 STA $3F
 LDY #$FF
 STY $3E
 INY
 STY $3C
 STY $42
 SEC           ; copy from main to Aux
 JSR $C311
 LSR $43
 LDA #$5F
 STA $3F
 JSR $FE2C     ; copy $4000 of main to $2000 of main
 BIT $C05E
 BIT $C050
L2120 = *
 JSR L123C
 CMP #$81      ; Ctrl-A
 BEQ L2135
 BIT $C05F
 BIT $C051
 STA $C001
L2132: JMP RELOAD

L2135: BIT $E0
 BMI L214D
 ROR $E0
 JMP L2100

L214D: STA $C000
 LSR $E0       ; copy the AUX $2000 bytes
 LDA #$20      ; back to Main first
 STA $43
 STA $3D
 LDA #$3F
 STA $3F
 LDY #$FF
 STY $3E
 INY
 STY $3C
 STY $42
 CLC           ; copy from Aux to Main
 JSR $C311
 LDA #$20
 STA $43
 LDA #$5F
 STA $3F
 LDA #$40
 STA $3D
 LDY #$FF
 STY $3E
 INY
 STY $3C
 STY $42
 SEC           ; copy from Main to Aux
 JSR $C311
 JMP L2120
;
; read in and display textfile
;
TXTDSP:    STX $E7          ;filetype of $B0 or $04
           LSR $1F
           JSR HOME         ;clear screen

;output help message on bottom line of screen          
           LDA #23          ;bottom line
           STA VERTPOS
           JSR SETCURSV
           LDX #0           ;output message for bottom line
L10:       LDA TYPEINSTR,X
           BEQ CONTINUE1
           INX
           JSR COUT
           BNE L10
          
;set viewport to 22 lines
;this protects the bottom line from the scrolling text
CONTINUE1: LDA #79
           STA HORIZPOS
           JSR SETCURSH
           LDA #22
           STA VERTPOS
           JSR SETCURSV
           LDA #03          ;set viewport bottom
           JSR COUT
           JSR HOME         ;clear viewport and home cursor          
           JSR READFILE     ;read in whole file now with sos
           STY FLAG
           LDA (TXTFILEP),Y
           CMP #$1E         ;esc ?
           BNE NORMAL
           JMP MOUSETEXT

NORMAL:    LDA (TXTFILEP),Y
           BNE *+5          ;end of text file?
           JMP PAUSE
           CMP #$2A ;AA
           BNE @1
           LSR $1F
@1:        CMP #$21 ;A1
           BCS NOT_CTRL      ;greater than or equal?
           CMP #$20 ;A0
           BCC YESCTRL       ;less than?
           BIT $1F
           BPL NOT_CTRL
           LSR $1F
           LDX #8
           CPX $57B
           BCC NOT_CTRL
           STX $57B
           BCS NOT_CTRL  ; always

YESCTRL:   JSR CTRL
           BCC PRINT2
           BCS NEXT_BYTE

NOT_CTRL:  LDX $57B
           INX
           CPX $21
           BCC PRNT2
           CMP #$20 ;A0
           BEQ PRINT2
XDECR:     DEX
YDECR:     TYA
           BNE DECRY
           DEC TXTFILEP+1
DECRY:     DEY
           LDA (TXTFILEP),Y
           ;ORA #$80
           CMP #$20 ;A0
           BCC YDECR
           BNE XDECR
           DEX
           STX $57B
           INX
           LDA #$20 ;A0
@LOOP:     JSR COUT
           INX
           CPX $21
           BCC @LOOP
PRINT2:    JSR COUT
          
           ;get cursor position
           BRK
           .BYTE D_STATUS
           .WORD D_STATCURS
           
           LDX CURVPOS
           CPX #22        ;at second last row?, this is left as a gap
           BCC NEXT_BYTE
           STY $E3
           TXA
          
          
           JSR PAUSE
           LDY $E3
           CLC
           BCC NEXT_BYTE

PRNT2:     JSR COUT
NEXT_BYTE: INY
           BNE NORMAL
           INC TXTFILEP+1     ;increment pointer high byte
NORMAL2:   JMP NORMAL

PAUSE:     BNE PAGEBTM
           LDA $E5
           ORA #$40
           .BYTE $2C
PAGEBTM:   LDA #0
           STA $E5
           LDA #22
           STA VERTPOS
           JSR SETCURSV

NOKEY:     ORA $E5       ; sets hi-bit for a new page
           STA $E5
           JSR GETKEY
           
           CMP #ESC      ; ESC
           BEQ EXIT
           CMP #UARROW   ; up arrow
           BEQ RESET
           CMP #DARROW   ; down arrow
           BNE NOKEY
           BIT $E5       ; at bottom of page so, no go
           BVS NOKEY
           BMI HOME
           RTS

HOME:      LDA #28        ;clear viewport
           JSR COUT          
           LDA #0         ;reset our cursor positions
           STA HORIZPOS   ;to home
           STA VERTPOS
           RTS

FLAG:      .RES 1

;reset file mark to start of file
RESET:     BRK                 ;set mark to start of file
            .BYTE SET_MARK
            .WORD SETMARKPRMS
           JMP TXTDSP+2

EXIT:      JMP CLOSEALL

CTRL:     CMP #$0A
          BEQ CLARY
          CMP #$0D
          BNE TAB
          LDX $E7
          CPX #$B0
          BNE CLARY
          SEC
          ROR $1F
CLARY:    CLC
          RTS

TAB:      CMP #$09     ;HT
          BNE SCARY
          LDX #0
          SEC
          LDA $57B
@LOOP:    INX
          SBC #5
          BCS @LOOP
          LDA #0
@LOOP2:   ADC #5
          DEX
          BNE @LOOP2
          STA HORIZPOS
          JSR SETCURSH
SCARY:    SEC
          RTS

MOUSETEXT = *
          LDA #$8C
          JSR COUT
          LDA #$8E
          BNE NOCLR     ; always
RETURNMT2: JMP RETURNMT

L031C:    LDA (TXTFILEP),Y
          BEQ RETURNMT2
          BMI L033D
          LDX #1
          CMP #$D
          BNE L0360
          LDX $57B
          BNE NOCLR
          LDX VERTPOS
          CPX $22
          BEQ NOCLR
          CMP FLAG
          STA FLAG
          BNE L036F
NOCLR:    LDX #1
          BNE L0360     ; always

L033D:    AND #$7F
          TAX
          JSR INCRY2
          BCS RETURNMT2
          CMP #$D
          BNE L0360
          LDA #$0D
          JSR COUT
          DEX
ADDCR:    TXA
          CLC
          ADC VERTPOS
          STA VERTPOS
          JSR $FC24
          BNE L036F
L0360:    STA FLAG
          ORA #$80
@LOOP:    JSR COUT
          DEX
          BNE @LOOP
L036F:    JSR INCRY2
          BCS RETURNMT
CHKCURS:  LDX VERTPOS
          INX
          CPX $23
          BCC L031C
          CLC
          LDA $20
          ADC $21
          TAX
          DEX
          DEX
          CPX $57B
          BCC *+5
          JMP L031C

 LDA (TXTFILEP),Y   ; handle last byte scenario
 TAX
 LDY $57B
 TYA
 LSR
 TAY
 PHP
 BCS MAIN
 SEI
 BIT $C055
MAIN: TXA
 STA ($28),Y
 BIT $C054
 PLP
RETURNMT: JSR $FE84
 LDX $23
 DEX
 DEX
 STX VERTPOS
 JSR SETCURSV
 JSR GETKEY

CLOSEALL: BRK           ;close the open file
          .BYTE CLOSE
          .WORD CLOSEPRM
          BRK                ;release memory for loading the file into 
           .BYTE RELEASE_SEG
           .WORD RELMEMPRM

          JSR L1210     ; HOME and print SQUIRT
          JSR PRTPATH   ; prints PATH followed by the prefix
          JMP RELOAD

INCRY2:   CLC
          INY
          BNE RETINY
          INC TXTFILEP+1
RETINY:   LDA (TXTFILEP),Y
          RTS

TYPEINSTR: .BYTE " Use "
;           .BYTE $9B,$8F,$4A,$8E       ;down arrow
           .BYTE " for line down | "
;           .BYTE $8F,$41,$4A,$8E       ;open apple down arrow
           .BYTE " for page down | "
;           .BYTE $8F,$41,$4B,$8E,$98   ;open apple up arrow
           .BYTE " document top |  ESC to directory"
           .BYTE $00

READFILE:  BRK
            .BYTE READ
            .WORD READPARM  ;was readparm2
                            ;this reads in whole file up to 32k long
           BCS RTN3         ;error?
           LDA RDREQCNT
           STA TXTFILEP
           LDA RDREQCNT+1
           ADC #$02           ;adjust with loading offset
           STA TXTFILEP+1
           LDA #$81           ;init xbyte
           STA TXTFILEPX
           LDY #0
           TYA
           STA (TXTFILEP),Y   ; to make sure there is a zero at the
                              ; end of the text file
           LDA #0             ;setup to point to start of load buffer
           STA TXTFILEP       ;0:0200
           LDA #02
           STA TXTFILEP+1
RTN3:      RTS


;check if last three chars of filename is FNT
CHKFONT:   LDX #3
           LDY #0
           ;LDA (OPENPRM2P),Y   ; where PATH is for OPEN, get name length
           TAY
AGAIN1:    ;LDA (OPENPRM2P),Y   ; grab last chars from filename
           CMP FNT,X
           BEQ NEXTCHR         ;if match, check next char
           LDX #6              ;restore X
           RTS                 ;not FNT file, return
           
NEXTCHR:   DEY
           DEX
           BPL AGAIN1

;yes, it is a FNT file
           LDY #$10
@LOOP2:    ;LDA (OPENPRM2P),Y
           STA $300,Y
           DEY
           BPL @LOOP2

           PLA
           PLA
           LDA $36
           PHA
           LDA $37
           PHA
           LDA #<START
           STA $36       ;address of monitor output routine - lo
           LDA #>START
           STA $37       ;hi
           JSR OPENIT
           LDA $85
           STA $F9
           LDA #$40
           STA $FB
           LDA #0
           STA $FA
           BRK
            .BYTE READ
            .WORD $00F8
           BRK
            .BYTE CLOSE
            .WORD CLOSEPRM

           CLD
           JSR $FC58   ;home
           JSR $F3E2   ;

           LDA #5
           STA VERTPOS
           JSR SETCURSV
           LDA #8
           STA HORIZPOS
           JSR SETCURSH
           LDY #0
           LDA $300,Y    ; where PATH is for OPEN
           TAX
           INY
@LOOP3:    TYA
           PHA
           LDA $300,Y
           ORA #$80
           JSR COUT
           PLA
           TAY
           INY
           DEX
           BNE @LOOP3
           INC VERTPOS
           INC VERTPOS

           LDA #$B0
           STA $18
           LDY #$A0
@LOOP4:    LDA #4
           STA HORIZPOS
           JSR SETCURSH
           ;JSR $FC22
@LOOP5:    TYA
           PHA
           JSR COUT
           INC HORIZPOS
           JSR SETCURSH
           PLA
           TAY
           INY
           CPY $18
           BNE @LOOP5
           INC VERTPOS
           INC VERTPOS
           JSR SETCURSV
           TYA
           BEQ FINIS
           CLC
           ADC #$10
           STA $18
 BCC       @LOOP4
 BCS       @LOOP4

FINIS:     JSR L123C
           JSR $FB2F
           PLA
           STA $37
           PLA
           STA $36
           JMP CLOSEALL

;output char in graphics screen (font file)
START:     TAY
           TXA
           PHA
           TYA
           LDX $FB
           ASL
           ASL
           BCS NORM
           BPL EXIT2
           BMI INVERSE
NORM:      BPL NORM1
           INX
NORM1:     INX
INVERSE:   ASL
           STX $1B
           CLC
           ADC $FA
           STA $1A
           BCC *+4
           INC $1B
           LDA $28
           STA FILEPTR
           LDA $29
           AND #3
           ORA $E6
           STA FILEPTR+1
           LDX #08
@LOOP:     LDY #00
           LDA ($1A),Y
           BIT $32
           BMI *+4
           EOR #$7F
           LDY HORIZPOS
           STA ($8),Y
           INC $1A
           BNE *+4
           INC $1B
           LDA FILEPTR+1
           CLC
           ADC #04
           STA FILEPTR+1
           DEX
           BNE @LOOP
EXIT2:     INC HORIZPOS
           JSR SETCURSH
           PLA
           TAX
           RTS

FNT: .BYTE "_FNT"



OPNGFXPRM: .BYTE 4           ;#params
           .WORD GFXPATH     ;pathname pointer
           .BYTE $00         ;ref_num -result
           .WORD $0000       ;option list pointer
           .BYTE $00         ;option list length

GFXPATH:   .BYTE 7
           .BYTE ".GRAFIX"

GFXWRITE:  .BYTE 3          ;#params
           .BYTE $00        ;ref_num
           .WORD GFXWRBUF   ;data buffer pointer        
           .BYTE $00        ;request count

GFXWRBUF:  .RES 4,00

GFXWRPEN:  .BYTE 3          ;#params
           .BYTE $00        ;ref_num
           .WORD GFXWRPENB  ;data buffer pointer        
           .BYTE 5          ;request count

GFXWRPENB: .BYTE 26         ;move pen
XPOS:      .WORD 1          ;x position
YPOS:      .WORD 1          ;y position

GFXWRBLK:  .BYTE 3          ;#params
           .BYTE $00        ;ref_num
           .WORD GFXWRBLB   ;data buffer pointer        
           .BYTE $00        ;request count

GFXWRBLB:  .BYTE 4          ;Draw Block
           .BYTE $00        ;block address
GFXBLKP:   .BYTE $00
           .BYTE $02
           .WORD 0          ;Column bit displacement
           .WORD 0          ;Row bit displacement
           .WORD 7          ;Pixel width of block
           .WORD 8          ;Pixel height of block
           
CHRCOUNT:  .RES 1

REQGRMEMPRM: .BYTE 4         ;#params
           .BYTE $00       ;base  - bank 0 
           .BYTE $21       ;      - page $20 (2000)
           .BYTE $00       ;limit - bank 0 
           .BYTE $3F       ;      - page $3F (3F00)
           .BYTE $11       ;seg_id
           .BYTE $00       ;seg_num - result
           
A3FONT:    JSR READFILE      ;read in entire font file


;         SOS     CLOSE, HBLK3      ;   THEN CLOSE all files

           BRK               ;open .GRAFIX
            .BYTE OPEN
            .WORD OPNGFXPRM
           ;error handling?
           LDA OPNGFXPRM+3   ;open'd ref_num
           STA GFXWRITE+1
           STA GFXWRPEN+1
           STA GFXWRBLK+1

           
           BRK                 ;request memory for graphics display 
            .BYTE REQUEST_SEG  ;from SOS, bank0 2000-3FFF
            .WORD REQGRMEMPRM
            
           LDA #$20            ;update sos global GRSIZE variable
           STA $1907           ;.GRAFIX driver checks this
           

;           LDA #16           ;set graphics mode
;           STA GFXWRBUF
;           LDA #0            ;280x192 BW
;           STA GFXWRBUF+1
;           LDA #2
;           STA GFXWRITE+4    ;request_count=2
;           BRK               ;write .GRAFIX
;            .BYTE WRITE
;            .WORD GFXWRITE
;           ;error handling?
           
;           LDA #1           ;reset viewport
;           STA GFXWRBUF
;           LDA #1
;           STA GFXWRITE+4    ;request_count=1
;           BRK               ;write .GRAFIX
;            .BYTE WRITE
;            .WORD GFXWRITE
;           ;error handling?
                    
           LDA #28           ;clear viewport
           STA GFXWRBUF
           LDA #1
           STA GFXWRITE+4    ;request_count=1
           BRK               ;write .GRAFIX
            .BYTE WRITE
            .WORD GFXWRITE
           ;error handling?

           LDA #15           ;turn graphics on
           STA GFXWRBUF
           LDA #1
           STA GFXWRITE+4    ;request_count=1
           BRK               ;write .GRAFIX
            .BYTE WRITE
            .WORD GFXWRITE

           LDA #$41          ;print A
           STA GFXWRBUF
           LDA #1
           STA GFXWRITE+4    ;request_count=1
           BRK               ;write .GRAFIX
            .BYTE WRITE
            .WORD GFXWRITE


            ;init variables
           LDA #0
           STA GFXBLKP
           STA XPOS
           STA XPOS+1
           STA YPOS
           STA YPOS+1
           LDA #2
           STA GFXBLKP+1
           LDA #128
           STA CHRCOUNT
           
NXTCHR:    BRK               ;Set pen postion
            .BYTE WRITE
            .WORD GFXWRPEN

           BRK               ;write graphics block
            .BYTE WRITE
            .WORD GFXWRBLK
           CLC
           LDA GFXBLKP       ;increment block pointer by 8
           ADC #8            ;8 bytes per char
           STA GFXBLKP
           BCC @5
           INC GFXBLKP+1
@5:        CLC
           LDA XPOS
           ADC #16
           CMP #128
           BEQ STEPY
           STA XPOS
           BNE NXTCHR2 
STEPY:     LDA YPOS
           ADC #12
           STA YPOS
           LDA #1
           STA XPOS
           
NXTCHR2:   DEC CHRCOUNT
           BNE NXTCHR
           RTS
           
;output CR
CROUT:    LDA    #$0D

;output char in A to console
COUT:     STA    BUFFER
          TYA
          PHA
          TXA
          PHA
          SOS    WRITE, WPLIST   ; Write out 1 or 2 bytes
	      PLA
          TAX
          PLA
          TAY
          LDA    BUFFER
          RTS

;set cursor vertical position
SETCURSV: PHA
          TYA
          PHA
          TXA
          PHA
          SOS    WRITE, WCVPLST
	      PLA
          TAX
          PLA
          TAY
          PLA
          RTS

;set cursor horizontal position
SETCURSH: PHA
          TYA
          PHA
          TXA
          PHA
          SOS    WRITE, WCHPLST
	      PLA
          TAX
          PLA
          TAY
          PLA
          RTS


PRINTDEC: LDX #$FF        ;Prepare for subtraction
          SEC
PRDEC100: INX
          SBC #100
          BCS PRDEC100    ;Count how many 100s
          ADC #100
          JSR PRDECDIG    ;Print the 100s
          LDX #$FF        ;Prepare for subtraction
          SEC
PRDEC10:  INX
          SBC #10
          BCS PRDEC10     ;Count how many 10s
          ADC #10
          JSR PRDECDIG    ;Print the 10s
          TAX             ;Pass 1s into X
PRDECDIG: PHA
          TXA             ;Save A, pass digit to A
          ORA #'0'        ;Convert to character
          JSR COUT        ;print it
          PLA
          RTS

;*****************************************************************
;
;        SUBROUTINES
;
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;
;        OPENCONS:  open the .CONSOLE file for reading
;
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

COLIST:  .BYTE   $04             ; 4 required parameters for OPEN
         .WORD   CNAME           ;   pathname pointer
CREF:    .BYTE   $00             ;   ref_num returned here
         .WORD   COPLIST         ;   option_list pointer
         .BYTE   $01             ;   length of opt parm list

COPLIST: .BYTE   $03              ; Open for reading and writing


OPENCONS:                        ; Here we didn't use a macro.
         BRK                     ; Begin SOS call block.
         .BYTE   OPEN             ; Open the console.
         .WORD   COLIST          ; Pointer to parameter list
         LDA     CREF            ; Save the result ref_num
         STA     REF             ;   for READs and WRITEs.
         RTS

;
;        GETDNUM:  Get the device number of .CONSOLE
;

GDLIST:  .BYTE   $02             ; 2 parameters for GET_DEV_NUM
         .WORD   CNAME           ;   dev_name pointer
GDNUM:   .BYTE   $00             ;   dev_num goes here


GETDNUM: SOS $84, GDLIST         ; Call GET_DEV_NUM
         LDA     GDNUM           ; Save the result dev_num
         STA     CONSNUM         ;   for console control
         STA     DSTATDEVN       ;for d_status call
         RTS

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;
;        SETCONS:  set the .CONSOLE file to suppress screen echo
;
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

SETLIST: .BYTE   $03             ; 3 required parms for D_CONTROL
CNUM:    .BYTE   $00             ;   dev_num of .CONSOLE
         .BYTE   $0B             ;   control_code = 0B: screen echo
         .WORD   CONLIST         ;   control_list pointer

CONLIST: .BYTE   FALSE           ;   Disable screen echo


SETCONS:
         LDA     CONSNUM         ; Set up device number
         STA     CNUM            ;   of .CONSOLE
         SOS     D_CNTL, SETLIST
         RTS

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;
;        ARMCTRLQ:  Arm the Attention Event for CONTROL-Q
;
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

DCLIST:  .BYTE   $03             ; 3 required parms for D_CONTROL
DNUM:    .BYTE   $00             ;   dev_num of .CONSOLE goes here
         .BYTE   $06             ;   control_code= 06:
                                 ;       Arm Attention Event
         .WORD   CLIST           ;   control_list pointer

CLIST:                           ; Control list
         .BYTE   $FF             ;   Event priority
         .BYTE   $02             ;   Event ID
         .WORD   HANDLER         ;   Event handler address
BANK:    .BYTE   $00             ;   Event handler bank
         .BYTE   $11             ;   Attention char = CTRL-Q


ARMCTRLQ:
         LDA     BREG            ; Set up bank number
         STA     BANK            ;   of event handler
         LDA     CONSNUM         ; Set up device number
         STA     DNUM            ; for control request
         SOS     D_CNTL, DCLIST  ; D_CONTROL call macro
         RTS

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;
;       HANDLER:  Attention event handler subroutine
;
;       This subroutine reads the attention character (CONTROL-Q)
;       from .CONSOLE, then beeps thrice.  If the previous
;       character was ESCAPE, the program terminates.
;
;       A buffer separate from the main data buffer is used for
;       reading the attention character, as otherwise the
;       attention character would sometimes clobber the character
;       in the data buffer before it could be written.
;
;       The buffer BELLS contains three BEL characters, separated
;       by a number of SYNC characters.  When written to the
;       console, these cause a total delay of about 150 ms.
;
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

BELLS    =       *               ; Buffer with BELs and delay:
         .BYTE   $07                                      ; BEL
         .BYTE   $16,$16,$16,$16,$16,$16,$16,$16,$16      ; SYNCs
         .BYTE   $07                                      ; BEL
         .BYTE   $16,$16,$16,$16,$16,$16,$16,$16,$16      ; SYNCs
         .BYTE   $07                                      ; BEL
BELLEN   =       *-BELLS         ; Calculate buffer length

HBLK1:   .BYTE   $04             ; 4 required parameters for READ
HREF1:   .BYTE   $00             ;   ref_num
         .WORD   HBUF1           ;   data_buffer pointer
         .WORD   $0001           ;   request_count
         .WORD   $0000           ;   transfer_count

HBUF1:   .BYTE   $00             ; Buffer for attention character

HBLK2:   .BYTE   $03             ; 3 required parameters for WRITE
HREF2:   .BYTE   $00             ;   ref_num
         .WORD   BELLS           ;   data_buffer pointer
         .WORD   BELLEN          ;   request_count

HBLK3:   .BYTE   $01             ; 1 required parameter for CLOSE
         .BYTE   $00             ;   ref_num = 0: CLOSE all files

HBLK4:   .BYTE   $00             ; 0 required parms for TERMINATE

HANDLER:
         LDA     REF             ; Set up reference numbers
         STA     HREF1           ;   for console READ
         STA     HREF2           ;   and console WRITE
         
         SOS     READ, HBLK1     ; Read attention character
         
         SOS     WRITE, HBLK2    ; Write three BELs to .CONSOLE
         
         LDA     BUFFER
         CMP     #ESC            ; IF last keystroke was ESCAPE
         BNE     @010
         
         SOS     CLOSE, HBLK3      ;   THEN CLOSE all files
         SOS     $65, HBLK4      ;       and TERMINATE
         
@010:    JSR     ARMCTRLQ        ;   ELSE re-arm attention event
         RTS                     ;       and resume execution

;*****************************************************************

;*****************************************************************
;
;        End of program -- calculate length
;
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

CODELEN =       *-CODESTART     ; Calculate number of bytes in
        .ENDPROC                ;    program

;*****************************************************************


