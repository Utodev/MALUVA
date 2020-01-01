
; MALUVA for MSX (C) 2018 FX, Armando Perez Abad & Uto
; LGPL License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS
; Thanks to Boriel for his 8 bits division code in ZX-Basic, DivByTen function is based on his code

                        ORG $013C
                        OUTPUT "MLV_MSX.BIN"



; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************


                        
                        define BDOS                      $0005   ; MSXDOS function calls
                        define FCB                       $005C   ; File control block address

                        define RG1SAV                    $f3e0   ; VDP copy 1



                        define F_OPEN                    $0F
                        define F_CLOSE                   $10
                        define F_CREATE                  $16
                        define F_SET_TRANSFER_ADDRESS    $1A
                        define F_RANDOM_BLOCK_READ       $27
                        define F_RANDOM_BLOCK_WRITE      $26

                        define DAAD_READ_FILENAME_ES     $BFE3
                        define DAAD_READ_FILENAME_EN     $BF83

                        define DAAD_SYSMESS             $B402
                        define DAAD_FILENAME_ADDR_ES    $C011
                        define DAAD_FILENAME_ADDR_EN    $BFB1

            			define JIFFY					$FC9E ; The timer variable

            			define MALUVA_REPORT_FLAG	20


Start                   
                        DI
; ---- Preserve registers
                        PUSH    BC
                        PUSH    IX
       					RES		7,(IX+MALUVA_REPORT_FLAG)	; Clear bit 7 of flag 28 (mark of EXTERN executed)



; --- Check function selected
                        LD      D, A
                        LD      A, (BC)
                        OR      A
                        JR      Z, loadImg
                        CP      1
                        JP      Z, saveGame
                        CP      2
                        JP      Z, loadGame
                        CP      3
                        JP      Z, xMessage
						CP      4
						JP      Z, XPart
						CP 		5
						JP 		Z, XBeep
	    				CP  7
    					JP  Z, XUndone
						CP 		255
						JP 		Z, RestoreXMessage
                        JP      ExitWithError

; ---- Set the filename
loadImg                 LD      A, D
                        CALL    DivByTen
                        ADD     '0'
                        LD      HL, ImageFilename + 2
                        LD      (HL),A
                        LD      A, D
                        CALL    DivByTen
                        ADD     '0'
                        DEC     HL
                        LD      (HL),A
                        DEC     HL
                        LD      A, '0'
                        ADD     D
                        LD      (HL),A


   						LD A, $FF               ; If a file was open, then the workbuffer  is going to be overwritten by the picture, so any Xmessage loaded there will be corruted, we set last XMessage file to 255 to avoid data to be used
						LD (LastOffset), A	
						LD (LastOffset+1), A	


; --- Open file        
                        LD      HL, ImageFilename                    
                        CALL    openFile                        ; Prepares the FCB and opens the file
                        OR      A                               ; On failure to open, exit
                        JR      NZ, ExitWithError

; --- Patch FCB so record size for reading is 1, and define where to read to when reading
                        LD      DE, WorkBuffer
                        CALL    setFCBParams

                        LD      HL, 1                                   ; Blocks to read
                        LD      C, F_RANDOM_BLOCK_READ
                        LD      DE, FCB
                        CALL    BDOS


; ----  Calculate number of lines divided by 8

                        LD      A, (WorkBuffer)
                        SRL     A
                        SRL     A
                        SRL     A


; --- Clears Picture zone screen attributes
                        PUSH    AF
                        LD      DE, $2000
                        call    ClearScreen
                        POP     AF


; --- And load from file, pixels data first, then atributes
                        PUSH    AF                              ; Preserve A value
                        LD      DE, 0                           ; Points to VRAM  (pixel data)
                        CALL    CopyToVRAM
                        POP     AF
                        LD      DE, $2000                      ; Points to VRAM  (attributes data)
                        CALL    CopyToVRAM


; ----- Close file
cleanAndClose           CALL    closeFile

cleanExit               POP     IX
                        POP     BC
                        EI
                        RET


; It happens the DAAD code sets the "done" status after the execution of an EXTERN, something which happens in a function called NXTOP, which does a few thing and then jumps to 
; another function named CHECK. Due to that , it is not possible to exit an EXTERN without getting the done status set. To avoid that we are going to go through the DAAD interpreter
; code to make what NXTOP does, and then jump to the JP CHECK at the end of that NXTOP function.

cleanExitNotdone		POP 	IX			; Copied from Cleanexit, without the EI
						POP 	BC
						
						POP 	HL			;This is the return address for Extern, there we should fin the "JP NXTOP" 
						INC 	HL			;Now we are pointing to address where NXTOP is stored
						LD 		E, (HL)
						INC 	HL
						LD 		D, (HL)		; Now DE contains NXTOP
						INC 	DE
						INC 	DE
						INC 	DE
						INC 	DE
						INC 	DE
						INC 	DE							; Inc six times to skip all code in NXTOP up to the JP CHECK
						LD  	(PatchNXTOPJMP + 1), DE		; Now the code below is just like NXTOP function, but without the done status set, and plus EI

FakeNXTOP				INC		BC			; This is the fake NXTOP code, with the JP at the end that jumps to the real JP CHECK
						POP 	HL
						EI					; EI is not in NXTOP but it was in Cleanexit
PatchNXTOPJMP			JP		0			; This will be patched above


ExitWithError			POP		IX											; Extract and push again real IX value from stack
						PUSH 	IX		
						SET 	7, (IX + MALUVA_REPORT_FLAG)				; Mark error has happened
						LD 		A, (IX + MALUVA_REPORT_FLAG)				
						AND 	1											; If bit 0 of flag 28 was set, then also exit extern without marking as DONE
						JR 		NZ, cleanExitNotdone
						JR 		cleanExit


; ******************* SAVE AND LOAD GAME ROUTINES   *************************************************

loadGame                CALL    prepareSaveGame                 ; Reads file name and stores at SavegameFilename properly
                        LD      HL, SavegameFilename            
                        CALL    openFile                        ; Sets the FCB and opens
                        OR      A                               ; On failure to open, exit
                        JR      NZ, diskFailure
                        POP     DE                              ; Get IX from stack, and pushes it again. IX points to where flags/object locations are located in RAM
                        PUSH    DE
                        CALL    setFCBParams                    ; Set to read in 1 byte blocks at DE

                        LD      HL, 512                         ; Blocks to read, 512 bytes
                        LD      C, F_RANDOM_BLOCK_READ
                        LD      DE, FCB
                        CALL    BDOS
                        CALL    closeFile
                        JR      cleanExit


saveGame                CALL    prepareSaveGame                 ; Reads file name and stores at SavegameFilename properly
                        LD      HL, SavegameFilename            ; Opens the file and sets the FCB
                        CALL    createFile
                        OR      A                               ; On failure to open, exit
                        JR      NZ, diskFailure
                        POP     DE                              ; Get IX from stack, and pushes it again. IX points to where flags/object locations are located in RAM
                        PUSH    DE
                        CALL    setFCBParams                    ; Set to write in 1 byte blocks at DE

                        LD      HL, 512                         ; Blocks to read, 512 bytes
                        LD      C, F_RANDOM_BLOCK_WRITE
                        LD      DE, FCB
                        CALL    BDOS
                        CALL    closeFile
                        JR      cleanExit



diskFailure             LD      A, 57                           ; E/S error sysmess
                        LD      HL, $B456                       ; in MSX condacts routies has no RET at the end, they jump to pipeline $B17C again
                        LD      (HL),201                        ; as we need it to return, we are patching temporaryly the SYSMES code. This replaces POP HL with RET
                        PUSH    HL
                        CALL    DAAD_SYSMESS
                        POP     HL                              
                        LD      (HL),03                         ; and this restores POP HL
                        JR      ExitWithError

prepareSaveGame         LD      A, ($B001)
                        CP      $3A
                        JR      z, prepareSaveEnglish
                        CALL    DAAD_READ_FILENAME_ES              ; request a file name from input 
                        LD      HL, DAAD_FILENAME_ADDR_ES          ; copies 8 characters to SavegameFilename where ther eextension is already present
prepareSaveGame2        LD      DE, SavegameFilename            ; DAAD routine alrady stores a 8 byte length file name at DAAD_FILENAME_ADDR_ES and fills with spaces if needed
                        LD      BC, 8                           
                        LDIR
                        RET

prepareSaveEnglish      CALL    DAAD_READ_FILENAME_EN              ; request a file name from input 
                        LD      HL, DAAD_FILENAME_ADDR_EN          ; copies 8 characters to SavegameFilename where ther eextension is already present
                        JR      prepareSaveGame2


; ********************************************************************                        
;                           AUX FUNCTIONS
; *******************************************************************

;- -------------------------------------------------------------------------------------------
; Input: DE= address to read/write to, it's input to F_SET_TRANSFER_ADDRESS
setFCBParams            LD      HL, 1                           ; SET read block size to 1 byte
                        LD      (FCB + 14), HL            

                        LD      C, F_SET_TRANSFER_ADDRESS       ; Set where to read to (or write to )
                        CALL      BDOS
                        RET



; ---------------------------------------------------------------------------
; DE    Direccion destino en VRAM
; A     Lineas a copiar
CopyToVRAM
                        PUSH    AF
                        LD      A,E                             ; VRAM address setup    
                        OUT     ($99),A
                        LD      A, D
                        OR      $40
                        OUT     ($99),A
                        POP     AF

copyToVRAMLoop          PUSH    AF                              ;lines to load

                        LD      C, F_SET_TRANSFER_ADDRESS
                        LD      DE, WorkBuffer                 ; where to load them
                        CALL    BDOS

                        LD      HL, 100H                        ; Blocks to read = bytes to read
                        LD      C, F_RANDOM_BLOCK_READ
                        LD      DE, FCB
                        CALL    BDOS

                        LD      HL, WorkBuffer
                        LD      C, $98
                        LD      B, 0
;IFNDEF MSX2
copyToVRAMLoop2         OUTI                                    ; Minimum VRAM access timing for MSX1: 29 cycles
                        JP      NZ, copyToVRAMLoop2
;ELSE
;                       OTIR                                    ; Minimum VRAM access timing for MSX2: 15 cycles
;ENDIF

                        POP     AF
                        DEC     A
                        JR      NZ, copyToVRAMLoop
                        RET

; ---------------------------------------------------------------------------
; DE    Direccion destino en VRAM
; A     Lineas a copiar
ClearScreen
                        PUSH    AF                              ; VRAM address setup
                        LD      A,E
                        OUT     ($99),A
                        LD      A, D
                        OR      $40
                        OUT     ($99),A

                        XOR     A                               ; Fill VRAM buffer with zeroes
                        LD      HL, WorkBuffer
                        LD      (HL),A
                        LD      DE, WorkBuffer + 1
                        LD      BC, $FF
                        LDIR
                        POP     AF


ClearScreenLoop         LD      HL, WorkBuffer
                        LD      C, $98
                        LD      B, 0
;IFNDEF MSX2
clearScreenLoop2        OUTI                                    ; Minimum VRAM access timing for MSX1: 29 cycles
                        JP      NZ, clearScreenLoop2
;ELSE
;                       OTIR                                    ; Minimum VRAM access timing for MSX2: 15 cycles
;ENDIF
                        DEC     A
                        JR      NZ, ClearScreenLoop
                        RET

;----------------------------------------------------------------------------
prepareFCB              PUSH    HL
clearFCB                LD      HL, FCB                       ; Fills FCB with Zeroes
                        XOR     A
                        LD      (HL), A
                        LD      DE, FCB +1
                        LD      BC, 1Eh + 11
                        LDIR                   
                        POP     HL                      ; And copies Filename on FCB to open/create
                        LD      DE, FCB + 1           
                        LD      BC, 11
                        LDIR                                                           
                        RET


; openFile -----------------------------------------------------------------
;input:   HL --> points to 11 chars filename
openFile                CALL    prepareFCB
                        LD      C, F_OPEN
                        LD      DE, FCB
                        CALL    BDOS                  
                        RET

; closefile --------------------------------------------------------------------------------------------------------------
closeFile               LD      C, F_CLOSE
                        LD      DE, FCB
                        CALL    BDOS                  
                        RET

; createFile -----------------------------------------------------------------
;input:   HL --> points to 11 chars filename
createFile              CALL    prepareFCB
                        LD      C, F_CREATE
                        LD      DE, FCB
                        CALL    BDOS                  
                        RET


XUndone			    	POP IX				; Make sure IX is correct
				    	PUSH IX
				    	RES		4, (IX-1)						
				    	JP 		cleanExitNotdone


; XPart function
XPart				    LD 		A, D
					    ADD		'0'
					    LD      (XMESSFilename), A
					    JP 		cleanExit


; XBeep , beep replacement

XBeep				    LD 		L, D    ; First parameter (duration) to L					
						POP 	IX
						POP 	BC
						INC 	BC
						LD 		A, (BC) ; Third parameter (tone) to A
                        LD      E, A
						XOR 	A
						LD 		D, A  ;  At this point, DE=tone, L=duration
						PUSH 	BC
						PUSH 	IX

						LD   IX,sfxFreqPSG - 48	; DE=get frequency from tone table
						ADD  IX,DE
						LD   E,(IX+0)
						LD   D,(IX+1)

						LD   C,$A1

						XOR  A					; REG#0 ChannelA Tone LowByte
						OUT  ($A0),A
						OUT  (C),E
						INC  A					; REG#1 ChannelA Tone HighByte
						OUT  ($A0),A
						OUT  (C),D

						LD   A,8				; REG#8 ChannelA Volume to 8
						OUT  ($A0),A
						OUT  (C),A

						LD   A,7				; REG#7 Mixer enable ChannelA
						OUT  ($A0),A
						LD   A,00111110b
						OUT  ($A1),A

BeepSilence				EX   DE,HL
						SRL  E
						CALL NZ,SilenceWait
					
						LD   A,7				; REG#7 Mixer disable ChannelA
						OUT  ($A0),A
						LD   A,00111111b
						OUT  ($A1),A

						JP   cleanExit

SilenceWait									; Wait for E = 1/50 seconds
                        EI
						LD  HL,JIFFY			; Cogemos la address donde se cuenta el tiempo en 1/50sec
loop0					LD  A,(HL)
loop1					CP  (HL)
						JR  Z,loop1
						DEC E
						JR  NZ,loop0
                        DI
						RET



; Frequencies table
sfxFreqPSG				DW	0xD65, 0xC9D, 0xBEB, 0xB42, 0xA9A, 0xA04, 0x971, 0x8E8, 0x86B, 0x7E2, 0x77F, 0x719	// Octave 1 (48-70) 
						DW	0x6B3, 0x64E, 0x5F5, 0x5A1, 0x54D, 0x502, 0x4B9, 0x474, 0x434, 0x3F9, 0x3C0, 0x38C	// Octave 2 (72-98)
						DW	0x359, 0x327, 0x2F6, 0x2D1, 0x2A7, 0x281, 0x25C, 0x23A, 0x21A, 0x1FD, 0x1E0, 0x1C6, // Octave 3 (96-118) 
						DW	0x1AD, 0x194, 0x17D, 0x168, 0x153, 0x141, 0x12E, 0x11D, 0x10D, 0x0FE, 0x0F0, 0x0E3  // Octave 4 (120-142)
						DW	0x0D6, 0x0CA, 0x0BF, 0x0B4, 0x0AA, 0x0A0, 0x097, 0x08F, 0x087, 0x07F, 0x078, 0x072	// Octave 5 (144-166) 
						DW	0x06B, 0x065, 0x05F, 0x05A, 0x055, 0x050, 0x04C, 0x047, 0x043, 0x040, 0x03C, 0x039	// Octave 6 (168-190)
						DW	0x036, 0x033, 0x030, 0x02D, 0x02A, 0x028, 0x026, 0x024, 0x022, 0x020, 0x01E, 0x01C	// Octave 7 (192-214) 
						DW	0x01B, 0x019, 0x018, 0x017, 0x015, 0x014, 0x013, 0x012, 0x011, 0x010, 0x00F, 0x00E  // Octave 8 (216-238)


; Xmessage printing
xMessage			    LD 		L, D ;  LSB at L
				    	POP 	IX
				    	POP 	BC
				    	INC 	BC	 ; We need not only to increase BC, but also make sure when exiting it returns increased, and cleanExit will restore it from stack so we have to update valus at stack
				    	PUSH 	BC
				    	PUSH 	IX
				    	LD 		A, (BC)
				    	LD 		H, A ; MSB AT H, so Message address at HL


						LD 		IX, (LastOffset)  ; Let's check if it's same message than last time
						CP 		IXH
						JR 		NZ, NotSameMessage
						LD 		A, L
						CP 		IXL
						JR 		Z, XmessPrintMessage  ;If same offset, just print again


NotSameMessage          LD      (LastOffset), HL
                        
; ------- Open file      

                        LD      HL, XMESSFilename
                        CALL    openFile                        ; Prepares the FCB and opens the file
                        OR      A                               ; On failure to open, exit
                        JP      NZ, cleanExit

                        LD      DE, WorkBuffer
                        CALL    setFCBParams                    ; Patch the FCB so read unit is 1 byte and define where to read to when reading

; ------- Seek File              

                        LD      HL, (LastOffset)
                        LD      (FCB+33), HL                     ; At FCB+33 there is a 32 bit value meaning the offest of the file
                        XOR     A
                        LD      (FCB+35),A
                        LD      (FCB+36),A

; ------- Read file
                        LD      HL, 512                         ; Blocks to read
                        LD      C, F_RANDOM_BLOCK_READ
                        LD      DE, FCB
                        CALL    BDOS

; ------- Print message 
                        ; OK, this below needs to be explained: I didn't find a way to call the DAAD function to print text and make it work with an xmessage already loaded in RAM. Everything
						; worked but the \n or  #n carriage return. The function was at address $1629 in the spanish interpreter. Then I decided to do the following: try to make the text
						; being printed by the MES condact, and to do that I as going to execute a MES condact. How do I do that?
						; 1) I modified first entry in the messages offsets table, preserving the value there before, to the address where the xmessage is loaded in RAM
						; 2) I was udpating BC and making sure it is returned modified to the stack. DAAD uses BC as it's internal "PC" register, so changing BC actually makes DAAD run 
						;    opcodes somewhere else. I pointed it to a zone in RAM (see below) where I already had sorted two condacts: MES 0, and EXTERN 0 255. MES 0 prints the text 
						;    using MES condact and then EXTERN 0 255...
						; 3) is another function in Maluva I'm using to restore the Message 0 pointer, and restoring BC, so the execution continues just after the XMES/XMESSAGE call


XmessPrintMessage   	LD 		HL, $0112      ; DAAD Header pointer to SYSMESS table
						LD 		E, (HL)
						INC 	HL
						LD 		D, (HL)
						EX      HL, DE		   ; HL points to SYSMESS pointers table
						LD 		E, (HL)
						INC		HL
						LD 		D, (HL)  		; Now DE has the value of first message pointer, and HL points to where that pointer is

						LD      (PreserveFirstSYSMES),DE
						LD		DE, WorkBuffer
						LD 		(HL), D
						DEC		HL
						LD 		(HL), E			; Store the offset at first message pointer 

						POP 	IX
						POP 	BC
						LD 		(PreserveBC), BC
						LD 		BC, FakeCondacts - 1		; Make DAAD "PC" point to our fake condacts
						PUSH 	BC
						PUSH 	IX

                        JP      cleanAndClose

						; So this is an unreachable (by the Z80 CPU) piece of codem which is actually DAAD code 
FakeCondacts			DB 		$36, 0,     $3D, 0, $FF   ; SYSMESS 0 EXTERN 0 255

                        
RestoreXMessage			LD 		HL, $0112      ; DAAD Header pointer to SYSMESS table
						LD 		E, (HL)
						INC 	HL
						LD 		D, (HL)
						LD 		HL, PreserveFirstSYSMES
						LDI
						LDI
						POP 	IX
						POP 	BC
						LD 		BC, (PreserveBC)
						EI
						RET
          

; ---------------------------------------------------------------------------
; *** Divides A by 10 and returns the remainder in A and the quotient in D^***
DivByTen
                        LD      D, A           ; Does A / 10
                        LD      E, 10          ; At this point do H / 10
                        LD      B, 8
                        XOR     A              ; A = 0, Carry Flag = 0
DivByTenLoop
                        SLA     D
                        RLA            
                        CP      E               
                        JR      C, DivByTenNoSub
                        SUB     E               
                        INC     D               
DivByTenNoSub
                        DJNZ    DivByTenLoop
                        RET                    ;A= remainder, D = quotient


; ---------------------------------------------------------------------------
ImageFilename           DB      "000     MS2"
SavegameFilename        DB      "UTO     SAV"        
XMESSFilename			DB      "0       XMB"
PreserveFirstSYSMES		DW      0
PreserveBC				DW      0
LastOffset              DW      $FFFF
WorkBuffer              DS      $200                                                ; WE only need $100 for the pictures buffer but we are using $200 for XMEssages, so we 


; ---------------------------------------------------------------------------
