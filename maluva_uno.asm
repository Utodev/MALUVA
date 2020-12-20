; MALUVA (C) 2018 Uto
; LGPL License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS
; Thanks to Boriel for his 8 bits division code in ZX-Basic, DivByTen function is based on his code
; Thanks to Mcleod_ideafix and Yombo for help and brainstorming that lead to this feature done this way

			ORG $843C
            OUTPUT  MLV_UNO.BIN
	

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************


			define M_GETSETDRV  		$89
			define F_OPEN  				$9a
			define F_CLOSE 				$9b
			define F_READ  				$9d
			define F_WRITE 				$9e
			define F_SEEK				$9f       
			define FA_READ 				$01
			define FA_WRITE				$02
			define FA_CREATE_AL			$0C
			define BANKM 				$5B5C

			define INTERPRETER_ADDRESS $6000  ; Address where the interpreter is loaded

			define VRAM_ADDR 	  	  $4000 ; The video RAM address
			define VRAM_ATTR_ADDR     VRAM_ADDR + $1800 ;  points to attributes zone in VRAM 

			define DAAD_READ_FILENAME_ES $7056	; DAAD function to request a file name
			define DAAD_READ_FILENAME_EN $6FF6	;

			define DAAD_SYSMESS_ES 	  $6DE8 ; DAAD function to print a system message
			define DAAD_SYSMESS_EN 	  $6D94 

			define DAAD_FILENAME_ADDR_ES $70B5 ; DAAD address where the file name read by DAAD_READ_FILENAME_ES is stored
			define DAAD_FILENAME_ADDR_EN $7055 

			
			define DDB_SYSMESS_TABLE_ADDR $8412 ;  Location of the pointer to the SYSMESS table in the DDB header

			define DAAD_PATCH_ES 		$707A		    ; Address where the interpreter sets the internal flag which makes the words be cutted when printed
			define DAAD_PATCH_EN 		$701A	

			define MALUVA_REPORT_FLAG	20

      		define ZXUNO_PORT 			$FC3B
			define REG_SCANDBLCTRL 		$0B
			define REG_RASTERLINE  		$0C
			define REG_RASTERCTRL  		$0D
			define REG_DEVCONTROL		$0E
			define REG_DEVCTRL2			$0F

			define ULAPLUS_CTRL_PORT 	$BF3B
			define ULAPLUS_DATA_PORT	$FF3B

			define BOTTOM_SPLIT_LINE    248
			define MIDDLE_SPLIT_LINE    96
		


; ********************************************************************                        
;                                 MAIN
; ********************************************************************

Start			
					DI
					PUSH 	BC
					PUSH 	IX
					RES		7,(IX+MALUVA_REPORT_FLAG)	; Clear bit 7 of flag 28 (mark of EXTERN executed)

; ------ Detect if english or spanish interpreter
					PUSH 	AF
					LD 		A, (INTERPRETER_ADDRESS+1)
					CP 		$55			
					JR		NZ, Spanish				 ; In english interpreter, the patch will be applied everytime the extern is called. Although that may 
					CALL 	PatchForEnglish    		 ; look like losing time, I prefered keeping the code as smaller as possible, so I don't check if patch 
					JR 		LangCont
Spanish				LD 		A, $C9					; (RET)
					LD      (DAAD_PATCH_ES), A 		; Patching the "ask for file name" routine so it doesn't break the words the nwriting messages afterwards
LangCont			POP 	AF

			
Init				LD 		D, A		; Preserve first parameter
					LD 		A, (BC)		; Get second parameter (function number) on A

					OR 		A
					JR 		Z, LoadImg
					CP 		1
					JP 		Z, SaveGame
					CP 		2
					JP 		Z, LoadGame
					CP 		3
					JP 		Z, XMessage
					CP 		4
					JP 		Z, XPart
					CP 		6
					JP 		Z, XSplitScreen
					CP  	7
					JP 		Z, XUndone
					CP  	10
					JP 	Z	, XSpeed

					CP 		255
					JP 		Z, RestoreXMessage
					JP 		ExitWithError
; ---- Set the filename
LoadImg
					LD 	A, D		; Restore first parameter
					CALL 	DivByTen
					ADD 	'0'
					LD 	HL, Filename+2
					LD 	(HL),A
					LD 	A, D
					CALL 	DivByTen
					ADD 	'0'
					DEC 	HL
					LD 	(HL),A
					DEC 	HL
					LD 	A, '0'
					ADD 	D
					LD 	(HL),A


; --- Set default disk  
					CALL 	setDefaultDisk
                    JP      C, ExitWithError


; --- open file
                    LD      B, FA_READ   
					LD   	IX, Filename
					RST     $08
                    DB      F_OPEN      
                    JP      C, ExitWithError


;  --- From this point we don't check read failure, we assume the graphic file is OK. Adding more fail control would increase the code size and chances of fail are low from now on


; --- Update code to use file handler obtained
                    LD      (drawHalfScreen + 1),A	
					LD      (CloseImgFile + 1),A	
					LD		(fSeekFileHandler + 1), A ; Same for fSeek feature
					LD      (drawLoop + 1),A	

; --- read palette
					LD 		IX, XMessBuffer		; Ah, yes, we are using the same buffer we use for Xmessages, to avoid wasting 64 bytes more
					LD      BC, 64
					RST     $08
					DB      F_READ     

; --- set the palette

					LD 		HL, XMessBuffer
					LD 		E, 0
PaletteLoop			LD 		BC, ULAPLUS_CTRL_PORT
					LD 		A, E
					OUT		(C),A
					LD 		BC, ULAPLUS_DATA_PORT
					LD 		A, (HL)
					OUT		(C),A
					INC 	HL
					INC 	E
					LD 		A, E
					CP 		64
					JR 		NZ, PaletteLoop

; --- Page in Page 7

pageIn				LD   	E, 7              ;switch in RAM page 7
					CALL   	page128KBank

; --- Load Pixels data 
					LD	 	IX, $C000
					CALL 	drawHalfScreen ; Read half second third

; --- Load Attrs data at E000

					LD	 	IX, $E000
					CALL 	drawHalfScreen ; Read half second third

; --- Page in bank 0

pageOut				LD 		E, 0
					CALL   	page128KBank


; ---- Close file	
CloseImgFile		LD 		A, 0
					RST     $08
                    DB      F_CLOSE
	
cleanExit			POP 	IX
					POP 	BC
					EI
					RET

; --- Pages 128K bank at register E at $C000 (must be 0-7)
page128KBank		LD   BC,$7FFD       ; I/O address of horizontal ROM/RAM switch
					LD   A,(BANKM)      ; get current switch state
					AND  $F8            ; RAM page 0
					OR 	 E
					LD   (BANKM),A      ; update the system variable (very important)
					OUT  (C),A          ; make the switch						
					RET
					

; --- Loads from file the information for half a screen (96 lines) to the address set at IX
drawHalfScreen	   	LD 		A, 0		; read first thrid
					LD      BC, 2048
					PUSH 	IX
					PUSH 	BC
					RST     $08
					DB      F_READ 
					POP		BC
					POP 	IX
					ADD 	IX, BC		; read half of second third
					LD		BC, 128		; 4 character lines remaining (4*32)
					LD 		E, 8		; Times to do the loop, will be used as counter. We don't use B and DJNZ cause we need BC all the time and in the end is less productive
drawLoop			LD 		A, 0 		; file handle
					PUSH 	DE
					PUSH 	IX
					RST     $08
					DB      F_READ
					POP 	IX
					INC  	IXH		; Increment  256 to point to next line address
					POP 	DE
					DEC 	E
					JR 	NZ, drawLoop
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

; Both read savegame and load savegame use the same code, that is just slightly modified before jumping in the common part at DoReadOrWrite

LoadGame			LD  	A, FA_READ
					LD		(OpenMode+1),A
					LD  	A, F_READ
					LD 		(ReadWrite),A
					JR		DoReadOrWrite

SaveGame			LD  	A, FA_CREATE_AL
					LD 		(OpenMode+1),A
					LD  	A, F_WRITE
					LD 		(ReadWrite),A


DoReadOrWrite		
					EI
ReadFilenameCall	CALL	DAAD_READ_FILENAME_ES	; Ask for file name
					DI
					CALL 	cleanSaveName		; Convert DAAD 10 filename into 8.3 filename

; --- Set default disk  
					CALL 	setDefaultDisk
                    JR      C, diskFailure

; --- open file
OpenMode            LD      B, FA_READ		; May be modified by FA_CREATE_AL above
					LD   	IX, SaveLoadFilename
					RST     $08			
					DB      F_OPEN      
					JR      C, diskFailure
					LD 		(CloseFile+1), A
; --- read or write file

					
					POP		IX			; Gets IX value back from stack, then push again
					PUSH 	IX
					LD 		BC, 512			; Save flags and objects
 					RST     $08
ReadWrite         	DB      F_READ     		; May be modified by F_WRITE ABOVE
					JR		C, diskFailure
                      

CloseFile			LD 		A, $FF			; That $FF will be modifed by code above
					RST     $08
                 	DB      F_CLOSE


					JP 		cleanExit	

diskFailure			LD 		L, 57			; E/S error
DAADSysmesCall		CALL    DAAD_SYSMESS_ES
					JR 		ExitWithError


XPart				LD 		A, D
					ADD		'0'
					LD      (XMESSFilename), A
					JP 		cleanExit

XUndone				POP IX				; Make sure IX is correct
					PUSH IX
					RES		4, (IX-1)						
					JP 		cleanExitNotdone


; XSplitScreen chages the interrupt mode to raster, and sets if active at XSplitActive. Once started it can be stopped, but the interrupts will continue to be raster based,
; and the one at bottom of scree will just not change to Timex mode
XSplitScreen		CALL 	ZXUnoInit
					

					LD 		A, D
					LD 		(XSplitActive), A	
					OR 		A
					JP 		Z, cleanExit
XSplitEnable		
;------             Set Raster Interrupt
					LD 		E, BOTTOM_SPLIT_LINE
					CALL 	SetRasterINT
					JP 		cleanExit

SetRasterINT		LD 		BC, ZXUNO_PORT
					LD 		A, REG_RASTERLINE
					OUT 	(C), A
					INC 	B
					LD 		A, E
					OUT 	(C), A				; Set Raster line to that which comes in E
					
				
					DEC 	B
					LD 		A, REG_RASTERCTRL
					OUT 	(C), A
					INC 	B
					LD 		A, 00000110b ; Enable raster INT, disable normal int, bit 9 of rasterline set to 0 too
					OUT 	(C), A
					RET


ZXUnoInit			LD BC, 	ZXUNO_PORT				; Make sure 128K pagination is ON
					LD A, REG_DEVCONTROL
					OUT (C),A
					INC B
					IN A, (C)
					AND 11111011b 					; Make sure bit 2, DI7FFD, is 0
					OUT (C), A

					DEC B							; Make sure Timex modes adn ULAPLUS are ON
					LD A, REG_DEVCTRL2
					OUT (C),A
					INC B
					IN A, (C)
					AND 11111100b				   ; make sure bits 0 and 1, DITIMEX and DIULAPLUS, value is 0
					OUT (C), A

					LD 		A, 16					; Initialize BANKM as we may not be in pure 128K mode (either 48K or 128K but USR 0)
					LD      (BANKM), A
					RET
					

XSpeed				LD A, D
					OR A
					LD A, 126
					
					JR NZ, XSpeed2
					LD A, 63
XSpeed2				LD (LoseTimeLoop-1), A
					CALL SetUnoSpeed
					JP cleanExit


					


SetUnoSpeed			LD 		A, D
					CP  	2
					JP 		NC, cleanExit				; Value > 2, invalid CPU speed
					AND 	1
					RRCA
					RRCA
					LD 		D, A						; Now mode is in bits 6-7 of D
					LD 		BC, ZXUNO_PORT
					LD 		A, REG_SCANDBLCTRL
					OUT 	(C),A
					INC 	B
					IN   	A, (C)
					AND 	$3F
					OR 		D
					OUT 	(C),A
					RET

XMessage			LD 		L, D ;  LSB at L
					POP 	IX
					POP 	BC
					INC 	BC	 ; We need not only to increase BC, but also make sure when exiting it returns increased, and cleanExit will restore it from stack so we have to update valus at stack
					LD 		A, (BC)
					LD 		(PreserveBC), BC   ; Preserve BC so RestoreXMessage can go back to execution after XMES/XMESSAGE call
					LD 		BC, FakeCondacts-1 ; Point to execution of fake condacts (SYSMESS 0 , RestoreXMessage)
					PUSH 	BC
					PUSH 	IX
					
					LD 		H, A ; MSB AT H, so Message address at HL
					LD	 	(XMessBuffer), HL   ; Preserve file offset, using the buffer as temporary address
					CALL 	setDefaultDisk
					JP 		C, diskFailure
; Open file					
	                LD      B, FA_READ   
					LD   	IX, XMESSFilename
					RST     $08
                    DB      F_OPEN      
                    JP      C, ExitWithError
					LD 		(CloseFile + 1), A ; Preserve file handle to be able to close it later
					LD 		(XmessReadFile + 1), A ; Preserve file handle to be able to read from it

; Seek file										; To be compatible with utoboot for ESXDOS, we are not using F_SEEK. Please notice we mean utoboot, not autoboot (https://github.com/Utodev/utoboot)

;					LD		DE, (XMessBuffer)	 ; Restore file offset
;					LD 		BC, 0   			 ; BCDE --> Offset 
;					LD 		IXL, 0    			 ; L=0 --> Seek from start
;					LD L, 0
;					RST 	$08
;					DB 		F_SEEK
					LD 		HL, (XMessBuffer)
					CALL 	fSeek
					JP 		C, CloseFile


; Read file					
					LD 		IX, XMessBuffer
					LD      BC, 512
XmessReadFile		LD 		A, $FF; // Self modified above
					RST     $08
					DB      F_READ  ; Read 

; At this point we have the message at XmessBuffer		

; OK, this below needs to be explained: I didn't find a way to call the DAAD function to print text and make it work with an xmessage already loaded in RAM. Everything
					; worked but the \n or  #n carriage return. The function was at address $1629 in the spanish interpreter. Then I decided to do the following: try to make the text
					; being printed by the MES condact, and to do that I as going to execute a MES condact. How do I do that?
					; 1) I modified first entry in the messages offsets table, preserving the value there before, to the address where the xmessage is loaded in RAM
					; 2) I was udpating BC and making sure it is returned modified to the stack. DAAD uses BC as it's internal "PC" register, so changing BC actually makes DAAD run 
					;    opcodes somewhere else. I pointed it to a zone in RAM (see below) where I already had sorted two condacts: MES 0, and EXTERN 0 255. MES 0 prints the text 
					;    using MES condact and then EXTERN 0 255...
					; 3) is another function in Maluva I'm using to restore the Message 0 pointer, and restoring BC, so the execution continues just after the XMES/XMESSAGE call

XmessPrintMessage	LD 		HL, DDB_SYSMESS_TABLE_ADDR      ; DAAD Header pointer to SYSMESS table
					LD 		E, (HL)
					INC 	HL
					LD 		D, (HL)
					EX      HL, DE		   ; HL points to sysmess  pointers table
					LD 		E, (HL)
					INC		HL
					LD 		D, (HL)  		; Now DE has the value of first message pointer, and HL points to where that pointer is
					LD      (PreserveFirstSYSMES) , DE

					LD		DE, XMessBuffer	; Addr where the message is stored
					LD 		(HL), D
					DEC		HL
					LD 		(HL), E			; Store the offset at first message pointer 

				
XmesageExitPoint	JP 		CloseFile			  ; Close file and exit, we will actually jump to executing the fake condacts below

					; So this is an unreachable (by the Z80 CPU) piece of codem which is actually DAAD code 
FakeCondacts		DB 		$36, 0, 	$3D, 0, $FF   ; SYSMESS 0 EXTERN 0 255

RestoreXMessage		LD 		HL, DDB_SYSMESS_TABLE_ADDR      ; DAAD Header pointer to SYSMESS table
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


; ********************************************************************                        
;                           AUX FUNCTIONS
; *******************************************************************

;***** replaces ESXDOS F_SEEK function so in case utoboot is used, where F_SEEK it's not available, the game still works fine
; 		we will read first as much 512 byte blocks as possible, then the remaining, and that way the file pointer will be 
; 		where F_SEEK would have moved it
; 		@param HL = offset in the file (64K maximum)

fSeek				OR A 			; Clear carry flag
fSeekLoop			LD A, H
					CP 2			; CP H,2 is like CP HL, 512
					JR C, fSeekLastBlock
					PUSH HL
					LD  BC, 512
					CALL fSeekRead
					POP HL
					RET C 			; If problems with reading, return
					LD DE, 512
					SUB HL, DE
					JR fSeekLoop
fSeekLastBlock		PUSH HL
					POP BC	
					CALL fSeekRead
					RET

fSeekRead			LD 		IX, XMessBuffer
fSeekFileHandler	LD 		A, $FF; // Self modified above
					RST     $08
					DB      F_READ  ; Read 
					RET 



; *** Makes filename read by DAAD to be compliant with ESXDOS 8+3 filenames***
cleanSaveName			LD 	HL, DAAD_FILENAME_ADDR_ES			; address of filename requested
						LD 	BC, $08FF			; we will use LDI together with DJNZ, B will work for DJNZ but we set C to $FF
						LD 	DE, SaveLoadFilename
cleanSaveNameLoop		LD 	A, (HL)				; to avoid LDI decrement of BC to affect B
						OR 	A
						JR	Z, cleanSaveSetExtension
						CP 	$20
						JR	Z, cleanSaveSetExtension
						LDI
						DJNZ 	cleanSaveNameLoop
cleanSaveSetExtension	LD 	HL, SaveLoadExtension
						LD 	BC, 5
						LDIR
						RET

; **** Sets the default disk active
setDefaultDisk			XOR	A  
                        RST     $08 
                        DB      M_GETSETDRV
                        RET
			

; *** Divides A by 10 and returns the remainder in A and the quotient in D^***
DivByTen				LD 	D, A			; Does A / 10
						LD 	E, 10			; At this point do H / 10
						LD 	B, 8
						XOR 	A				; A = 0, Carry Flag = 0
DivByTenLoop			SLA	D
						RLA			
						CP	E		
						JR	C, DivByTenNoSub
						SUB	E		
						INC	D		
DivByTenNoSub			DJNZ DivByTenLoop
						RET				;A= remainder, D = quotient

; ** Function that patches Maluva code so it calls DAAD functions at the proper locations whe the english interpreter has been detected
PatchForEnglish			LD HL, DAAD_READ_FILENAME_EN
						LD (ReadFilenameCall+1), HL
						LD HL, DAAD_SYSMESS_EN
						LD (DAADSysmesCall+1), HL
						LD HL, DAAD_FILENAME_ADDR_EN
						LD(cleanSaveName+1), HL
						LD A, $C9						; Also patch the "ask for a file name" function so it doesn't make the words be cutted between lines after calling it (patch is RET replacing a RES 6,(IX-$0a))
						LD (DAAD_PATCH_EN), A
						RET


Filename				DB 	"UTO.UNO",0
SaveLoadFilename		DB 	"PLACEHOLD.SAV",0
SaveLoadExtension		DB 	".SAV", 0
XMESSFilename			DB  "0.XMB",0
PreserveFirstSYSMES		DW 0
PreserveBC				DW 0
XSplitActive			DB 0
XMessBuffer				DS 512
EndOfMainCode


; ------------------------------------------------------------------------------
; --------------- The interrupt Handlers ---------------------------------------
; ------------------------------------------------------------------------------

						ORG EndOfMainCode
            			OUTPUT "MLV_UNO_INT.BIN",t					; Save to a separated file

; ------------ Interrupt handler for XSplitScreen, called by every interrupt, including normal and raster
InterruptHandler		DI
						PUSH BC
						PUSH HL
						PUSH DE
						PUSH AF

; ----- Check which line it was	the raster interrupt
						LD 		BC, ZXUNO_PORT
						LD 		A, REG_RASTERLINE
						OUT		(C),A
						INC 	B
						IN 		A, (C)
						CP 		MIDDLE_SPLIT_LINE
						JR 		Z, RasterMiddleHandler  
						CP 		BOTTOM_SPLIT_LINE
						JR 		Z, RasterBottomHandler
						JR 		InterruptHandlerEndPre			; the interrupt is the standard ULA refresh one, do nothing but returning with carry set so DAAD runs usual IM1 code


; --- if the interrupt happens at the middle raster line 
RasterMiddleHandler		CALL 	DisableExtendedGraphics			
						LD 		E, BOTTOM_SPLIT_LINE
						CALL 	SetRasterINT					;Set Rater Int at scanlime BOTTOM_SPLIT_LINE
						CCF										; Avoid DAAD executing the standar interrupt code
						JR 		InterruptHandlerEnd

; --- if the interrupt happens at the bottom raster line 
RasterBottomHandler		LD 		A, (XSplitActive)				
						OR 		A
						JR 		Z, SkipExtendedGraphics
						CALL	EnableExtendedGraphics
SkipExtendedGraphics	LD 		E, MIDDLE_SPLIT_LINE
						CALL	SetRasterINT					; Set Next raster int at 96

InterruptHandlerEndPre	SCF										; Make DAAD do its usual interrupt code
				
InterruptHandlerEnd		POP 	AF
						POP 	DE
						POP		HL
						POP 	BC
						RET

DisableExtendedGraphics 

						LD B, 63
LoseTimeLoop			DJNZ LoseTimeLoop

						XOR 	A					; Disable Timex mode
						OUT 	(255),A

						LD   BC,$7FFD       ; the horizontal ROM switch/RAM switch I/O address
						LD   A,(BANKM)      ; system variable that holds current switch state
						AND  $F7            ; disable shadow RAM
						LD   (BANKM),A      ; must keep system variable up to date (very important)
						OUT  (C),A          ; make the switch


						LD 		BC, ULAPLUS_CTRL_PORT
						LD 		A, 01000000b 			 ; Normal mode
						OUT 	(C),A
						LD 		BC, ULAPLUS_DATA_PORT
						LD 		A, 0					; Disable ULAPlus
						OUT 	(C),A


						RET

EnableExtendedGraphics  LD 		BC, ULAPLUS_CTRL_PORT
						LD 		A, 01000000b			; HiRes mode, bank 7
						OUT 	(C),A
						LD 		BC, ULAPLUS_DATA_PORT
						LD 		A, 1					; Enable ULAPlus
						OUT 	(C),A

						LD 		A, 2					; Enable Timex mode (HiColor)
						OUT 	(255),A

						LD   BC,$7FFD       ; the horizontal ROM switch/RAM switch I/O address
						LD   A,(BANKM)      ; system variable that holds current switch state
						OR   8              ; enable shadow RAM
						LD   (BANKM),A      ; must keep system variable up to date (very important)
						OUT  (C),A          ; make the switch


						RET

