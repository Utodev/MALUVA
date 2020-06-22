; MALUVA (C) 2018 Uto
; LGPL License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS
; Thanks to Boriel for his 8 bits division code in ZX-Basic, DivByTen function is based on his code
; Thanks to Ped7g, Alcoholics Anonymous and SevenFFF for their help in the Spectrum Next forum

			ORG $843C
            OUTPUT  MLV_NEXT.BIN
	

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************


			define M_GETSETDRV  	$89
			define F_OPEN  			$9a
			define F_CLOSE 			$9b
			define F_READ  			$9d
			define F_WRITE 			$9e       
			define F_SEEK			$9f       
			define FA_READ 			$01
			define FA_WRITE			$02
			define FA_CREATE_AL		$0C

			define ULA_ATTR_ADDR	$5800

			define LAYER2_ACCESS_PORT   $123B
			

			define REG_LAYER2_RAM_PAGE  $12
			define REG_TRANSPARENCY	    $14
			define REG_SPRITE_LAYERS	$15
			define REG_CLIP_LAYER2		$18
			define REG_CLIP_WINDOW_CTRL $1C
			define REG_PALETTE_INDEX    $40
			define REG_PALETTE_VALUE	$44
			define REG_PALETTE_CRTL		$43
			define REG_MMU7			 	$57
			define REG_CPUSPEED			$07


			define INTERPRETER_ADDRESS $6000  	; Address where the interpreter is loaded

			define DAAD_READ_FILENAME_ES $7056	; DAAD function to request a file name
			define DAAD_READ_FILENAME_EN $6FF6	

			define DAAD_SYSMESS_ES 	  $6DE8 	; DAAD function to print a system message
			define DAAD_SYSMESS_EN 	  $6D94 	

			define DAAD_FILENAME_ADDR_ES $70B5 ; DAAD function where the file name read by DAAD_READ_FILENAME_ES is stored
			define DAAD_FILENAME_ADDR_EN $7055 

			define DDB_SYSMESS_TABLE_ADDR $8412 ;  Location of the pointer to the SYSMESS table in the DDB header

			define DAAD_PATCH_ES $707A		    ; Address where the interpreter sets the internal flag which makes the words be cutted when printed
			define DAAD_PATCH_EN $701A			

			define MALUVA_REPORT_FLAG	20

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
					PUSH 	BC
					LD 		A, (INTERPRETER_ADDRESS+1)
					CP 		$55			
					JR		NZ, Spanish				 ; In english interpreter, the patch will be applied everytime the extern is called. Although that may 
					CALL 	PatchForEnglish    		 ; look like losing time, I prefered keeping the code as smaller as possible, so I don't check if patch 
					JR 		LangCont
Spanish				LD 		A, $C9					; (RET)
					LD      (DAAD_PATCH_ES), A 		; Patching the "ask for file name" routine so it doesn't break the words when writing messages afterwards
LangCont			POP		BC						 ; already applied. It takes milliseconds and no one could notice when playing anyway
					POP 	AF

			
Init				LD 	D, A		; Preserve first parameter
					LD 	A, (BC)		; Get second parameter (function number) on A

					OR 	A
					JR 	Z, LoadImg
					CP 	1
					JP 	Z, SaveGame
					CP 	2
					JP 	Z, LoadGame
					CP  3
					JP 	Z, XMessage
					CP  4
					JP  Z, XPart
					CP  7
					JP  Z, XUndone
					CP 	8
					JP 	Z, XNextCLS
					CP  9
					JP 	Z, XNextReset
					CP  10
					JP 	Z, XSpeed
					CP 	255
					JP 	Z, RestoreXMessage
					JP 	ExitWithError
; ---- Set the filename
LoadImg				LD 	A, D		; Restore first parameter
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
                    JP      NC, fileOpened
					XOR 	A
					LD 		BC, LAYER2_ACCESS_PORT
					OUT 	(C),A					 ; Disable layer 2
					JP 		ExitWithError



; --- Preserve A register, containing the file handle 
fileOpened          LD	    (FileHandle),A

; --- read header - first two bytes (IMGNumLine is just after TransparencyColor in the RAM)
					LD 	    IX, TransparencyColor
					LD      BC, 2
					RST     $08
					DB      F_READ     

; ---- deactivate Layer2 to make cleaning invisible
					XOR 	A
					LD 		BC, LAYER2_ACCESS_PORT
					OUT 	(C),A


; ---- set layers order to SLU (Sprite, Layer2, ULA). Actually, unnecesery, as SLU is the default option after reset
					NEXTREG REG_SPRITE_LAYERS, 0

; ---- place the Layer2 RAM at a higher slot
					NEXTREG REG_LAYER2_RAM_PAGE, 12 ; Use page 12, 13 and 14 for Layer2

; ---- set layer 2 clip window
					NEXTREG REG_CLIP_WINDOW_CTRL, 1   	; reset Layer2 clip window index (to X1)
        			NEXTREG REG_CLIP_LAYER2, 0  	    ; clip.layer2.x1 = 0
        			NEXTREG REG_CLIP_LAYER2, $FF    	; clip.layer2.x2 = 255
					LD 		A, (IMGNumLine)
					DEC 	A							; If we have N lines, then last line is N-1
        			NEXTREG REG_CLIP_LAYER2, 0	    	; clip.layer2.y1 = (IMGNumLine)
        			NEXTREG REG_CLIP_LAYER2, A      	; clip.layer2.y2 = (IMGNumLine)

; ---- clear the layer

					CALL ClearLayer2

; ---- activate Layer2
					LD 		A, 2
					LD 		BC, LAYER2_ACCESS_PORT
					OUT 	(C),A


; ---- read and set palette
					
					NEXTREG REG_PALETTE_CRTL, %00010000			; auto-increment ON, active palette = first layer2 pal
					NEXTREG REG_PALETTE_INDEX,0
					LD 		B, 0
PaletteLoop			LD 		IX, PaletteVal
					PUSH 	BC
					LD 		A,(FileHandle)
					LD      BC, 2
					RST     $08
					DB      F_READ          		; Read one byte
					LD 		A, (PaletteVal)
					NEXTREG REG_PALETTE_VALUE, A
					LD 		A, (PaletteVal+1)
					NEXTREG REG_PALETTE_VALUE, A
					POP		BC
					DJNZ    PaletteLoop

; ---- set Transparency color if needed
					LD 		A, (TransparencyColor)
					NEXTREG REG_TRANSPARENCY, A;      
					OR 		A
					CALL 	Z, FillUlaLayer			; If color is 0, we will fill the zone below the Layer2 picture with color 0 (black) so although black pixels are considered transparent, what it's seen below is also a black pixel
					
; ---- read pixels data
					LD 		E,  24					; Page 12 * 2, as we access via 8K block when using MMU
					LD 		A, (IMGNumLine)
drawNormalLoop		OR 		A
					JR 		Z, drawEnd
					SUB		32
					JR 		C, drawPartialBlock
					PUSH 	AF
					LD 		A, E
					NEXTREG REG_MMU7, A				; Change MMU7 bank
					INC 	E					    ; point to next MMU bank
					PUSH 	DE
					LD 		IX, $E000				; MMU 7 addres
					LD 		BC, $2000				; 8K
					LD 		A, (FileHandle)		
					RST     $08
					DB      F_READ          		; Read 8Kb
					POP 	DE
					POP 	AF
					JR 		drawNormalLoop
					

					
					LD 		IX, $E000				; MMU 7 adddress location
					
drawPartialBlock	ADD 	32
					LD 		B, A
					XOR 	A						
					LD 		A, E
					NEXTREG REG_MMU7, A				
					LD 		C, 0					; BC = NumLines*256
					LD 		IX, $E000				; MMU 7 addres
					LD 		A, (FileHandle)		
					RST     $08
					DB      F_READ          		; Read 8Kb

drawEnd				NEXTREG REG_MMU7, 1				; Change MMU7 bank to to original one


; ---- Close file	
					LD 		A,  (FileHandle)
					RST     $08
                    DB      F_CLOSE
	
cleanExit			EI
					POP 	IX
					POP 	BC
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

DisableLayer2			NEXTREG REG_CLIP_WINDOW_CTRL, 1   	; reset Layer2 clip window index (to X1)
        				NEXTREG REG_CLIP_LAYER2, 0  	    ; clip.layer2.x1 = 0
        				NEXTREG REG_CLIP_LAYER2, 0    	; clip.layer2.x2 = 255
        				NEXTREG REG_CLIP_LAYER2, 0	    	; clip.layer2.y1 = (IMGNumLine)
        				NEXTREG REG_CLIP_LAYER2, 0      	; clip.layer2.y2 = (IMGNumLine)
						RET						

XNextCLS				CALL DisableLayer2
						CALL ClearLayer2
						JP cleanExit

XNextReset				CALL DisableLayer2
						CALL ClearLayer2
						NEXTREG REG_CPUSPEED, $0		; On exit set 3.5Mhz back
						RST 0				

XSpeed					LD 	A, D
						CP  2
						JP 	NC, cleanExit				; Value > 3, invalid CPU speed
						NEXTREG REG_CPUSPEED, A
						JP cleanExit




; Both read savegame and load savegame use the same code, that is just slightly modified before jumping in the common part at DoReadOrWrite

LoadGame			LD  A, FA_READ
		
					LD	(OpenMode+1),A
					LD  A, F_READ
					LD 	(ReadWrite),A
					JR	DoReadOrWrite

SaveGame			LD  A, FA_CREATE_AL
					LD 	(OpenMode+1),A
					LD  A, F_WRITE
					LD 	(ReadWrite),A


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

					POP	IX			; Gets IX value back from stack, then push again
					PUSH 	IX
					LD 	BC, 512			; Save flags and objects
 					RST     $08
ReadWrite           DB      F_READ     		; May be modified by F_WRITE ABOVE
					JR	C, diskFailure
                      

CloseFile			LD 	A, 	$FF			; That $FF will be modifed by code above
					RST     $08
                    DB      F_CLOSE


					JP 	cleanExit	

diskFailure			LD 	L, 57			; E/S error
DAADSysmesCall		CALL    DAAD_SYSMESS_ES
					JP 	ExitWithError

XPart				LD 		A, D
					ADD		'0'
					LD      (XMESSFilename), A
					JP 		cleanExit

XUndone				POP IX				; Make sure IX is correct
					PUSH IX
					RES		4, (IX-1)						
					JP 		cleanExitNotdone



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
					JR 		C, diskFailure
; Open file					
	                LD      B, FA_READ   
					LD   	IX, XMESSFilename
					RST     $08
                    DB      F_OPEN      
                    JP      C, ExitWithError
					LD 		(CloseFile + 1), A ; Preserve file handle to be able to close it later
					LD 		(XmessReadFile + 1), A ; Preserve file handle to be able to read from it
; Seek file					
					LD		DE, (XMessBuffer)	 ; Restore file offset
					LD 		BC, 0   			 ; BCDE --> Offset 
					LD 		IXL, 0    			 ; L=0 --> Seek from start
					LD L, 0
					RST 	$08
					DB 		F_SEEK
					JR 		C, CloseFile

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

; ** Set attributes in ULA Layer in the zone below Layer 2 to 00, to virtually disable transparency
FillUlaLayer		    XOR 	A
						LD	H, A
						LD 	A, (IMGNumLine)	; restore number of lines
						LD 	L, A			; now HL = number of lines 
						ADD 	HL, HL
						ADD 	HL, HL			; Multiply by 4 (32 bytes of attributes per each 8 lines  = means 4 per line)
						PUSH 	HL
						POP 	BC
						XOR 	A
						LD (ULA_ATTR_ADDR), A	; Set first attribute to zero
						DEC 	BC
						LD 		HL, ULA_ATTR_ADDR
						LD 		DE, ULA_ATTR_ADDR + 1
						LDIR					; Copy that zero BC -1 times
						RET
						
; Clear the layer 2

ClearLayer2				LD 		B, 6
						LD 		E, 24
ClearLoop				LD 		A, E
						NEXTREG REG_MMU7, A				; Change MMU7 bank
						INC 	E
						PUSH 	DE
						PUSH 	BC
						LD 		HL, $E000
						XOR 	A	
						LD 		(HL),A
						LD 		BC, $2000 - 1
						LD 		DE, $E001
						LDIR
						POP 	BC
						POP 	DE
						DJNZ 	ClearLoop
						NEXTREG REG_MMU7, 1				; REstore original MMU bank
						RET
												


Filename				DB 	"UTO.NXT",0
TransparencyColor		DB  0
IMGNumLine				DB 	0
PaletteVal				DW 	0
FileHandle				DB	0
SaveLoadFilename		DB 	"PLACEHOLD.SAV",0
SaveLoadExtension		DB 	".SAV", 0
XMESSFilename			DB  "0.XMB",0
PreserveFirstSYSMES		DW 0
PreserveBC				DW 0
XMessBuffer				DS 512
