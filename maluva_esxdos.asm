; MALUVA (C) 2018 Uto
; LGPL License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS
; Thanks to Boriel for his 8 bits division code in ZX-Basic, DivByTen function is based on his code

			ORG $843C
            OUTPUT  MLV_ESX.BIN
	

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************


			define M_GETSETDRV  	$89
			define F_OPEN  		$9a
			define F_CLOSE 		$9b
			define F_READ  		$9d
			define F_WRITE 		$9e
			define F_SEEK		$9f       
			define FA_READ 		$01
			define FA_WRITE		$02
			define FA_CREATE_AL	$0C

			define INTERPRETER_ADDRESS $6000  ; Address where the interpreter is loaded

			define VRAM_ADDR 	  	  $4000 ; The video RAM address
			define VRAM_ATTR_ADDR     VRAM_ADDR + $1800 ;  points to attributes zone in VRAM 

			define DAAD_READ_FILENAME_ES $7056	; DAAD function to request a file name
			define DAAD_READ_FILENAME_EN $6FF6	;

			define DAAD_SYSMESS_ES 	  $6DE8 ; DAAD function to print a system message
			define DAAD_SYSMESS_EN 	  $6D94 

			define DAAD_FILENAME_ADDR_ES $70B5 ; DAAD function where the file name read by DAAD_READ_FILENAME_ES is stored
			define DAAD_FILENAME_ADDR_EN $7055 

			define DAAD_PRINTMSG_ADDR_ES $6DF1 ; DAAD function that prints the message pointed by HL
			define DAAD_PRINTMSG_ADDR_EN $6D90

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
					LD 		A, (INTERPRETER_ADDRESS+1)
					CP 		$55			
					JR		NZ, Spanish				 ; In english interpreter, the patch will be applied everytime the extern is called. Although that may 
					CALL 	PatchForEnglish    		 ; look like losing time, I prefered keeping the code as smaller as possible, so I don't check if patch 
					JR 		LangCont
Spanish				LD 		A, $C9					; (RET)
					LD      (DAAD_PATCH_ES), A 		; Patching the "ask for file name" routine so it doesn't break the words the nwriting messages afterwards
LangCont			POP 	AF

			
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
					JP 	ExitWithError
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


; --- Preserve A register, containing the file handle 
                    LD      D,A	

; --- read header
					LD 	IX, ImgNumLine
					PUSH 	DE
					PUSH 	BC
					LD      BC, 1
					RST     $08
					DB      F_READ     
					POP 	BC
					POP 	DE
					LD 	A, (ImgNumLine) ; A register contains number of lines now
                                            

; read data - for Spectrum we start by reading  as much thirds of screen as possible, in the first byte of file the number of lines appears, so if there is carry when comparing to 64,
;             it means there are less than 128 lines, and if there is carry when comparing to 128, it means there are more than 128 but less than 192. If no carry, then it's a full
;	      screen. 

			
					SUB 	64
					JR 		C, drawPartialThird0  ; if thre is no carry, there is at least one whole third of screen
					LD 		BC, 2048
					LD 		H, B
					LD 		L, C
					SUB 	64
					JR 		C, drawWholeThirds   ;if there is still no carry, there are at least two thirds of screen
					ADD 	HL, BC
nextThird	        SUB 	64
					JR 		C, drawWholeThirds ; if there is still no carry, it's a full screen (3 thirds)
					ADD		HL, BC	           ; read one, two or the three whole thirds
drawWholeThirds		LD 		B, H
					LD 		C, L		
					LD 		IX, VRAM_ADDR
					PUSH    AF
					LD 		A, D 		; file handle
					PUSH 	DE
					RST     $08
					DB      F_READ
					POP 	DE
					POP 	AF
					LD 		IX, VRAM_ADDR
					ADD 	IX, BC		; Hl points to next position in VRAM to paint at
					JR 		drawPartialThird
                        
; --- This draws the last third, that is an uncomplete one, to do so, the file will come prepared so instead of an standar third with 8 character rows, it's a rare third with less rows.
;     For easier undesrtanding we will call 'row" each character row left, and 'line' each pixel line left, so each row is built by 8 lines.
;     Usually, you can read all first pixel lines for each rows in a third by reading 256 bytes (32 * 8), but if instead of 8 rows we have less, then we have to read 32 * <number of rows>
;     To determine how many rows are left we divide lines left by 8, but then to calculate how many bytes we have to read to load first pixel line for those rows we multiply by 32,
;     so in the end we multiply by 4. Once we know how much to read per each iteration we have to do 8 iterations, one per each line in a character. So we first prepare <lines left>*4 in
;     BC register,and then just read BC bytes 8 times, increasin IX (pointing to VRAM) by 256 each time to point to the next line inside the row

drawPartialThird0		LD 		IX, VRAM_ADDR  ; If the image does not cover a whole third at all, IX is not initialized so it is initialized here



drawPartialThird    ADD		A, 64		; restore the remaining number of lines (last SUB went negative)                
                    OR 		A
                    JR 		Z, readAttr	; if A = 0, then there were exactly  64, 128, or 192 lines, just jump to attributes section	
					ADD		A, A
					ADD		A, A		; A=A*4. Will never exceed 1 byte as max value for lines is 63, and 63*4 = 252
					LD 		B, 0
					LD 		C, A		; BC = number of bytes to read each time (numlines/ 8 x 32). 
					LD 		E, 8		; Times to do the loop, will be used as counter. We don't use B and DJNZ cause we need BC all the time and in the end is less productive
drawLoop			LD 		A, D 		; file handle
					PUSH 	DE
					PUSH 	IX
					RST     $08
					DB      F_READ
					POP 	IX
					INC  	IXH		; Increment  256 to point to next line address
					POP 	DE
					DEC 	E
					JR 	NZ, drawLoop

; read the attributes 

readAttr			XOR 	A
					LD		H, A
					LD 		A, (ImgNumLine)	; restore number of lines
					LD 		L, A			; now HL = number of lines 
					ADD 	HL, HL
					ADD 	HL, HL			; Multiply by 4 (32 bytes of attributes per each 8 lines  = means 4 per line)
					PUSH 	HL
					POP 	BC
					LD 		IX, VRAM_ATTR_ADDR	; attributes VRAM
					LD 		A, D 			; file handle
					PUSH	DE
					RST 	$08
					DB 		F_READ
					POP		DE

; ---- Close file	
					LD 		A, D
					RST     $08
                    DB      F_CLOSE
	
cleanExit			POP 	IX
					POP 	BC
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


					JR 		cleanExit	

diskFailure			LD 		L, 57			; E/S error
DAADSysmesCall		CALL    DAAD_SYSMESS_ES
					JR 		ExitWithError


XPart				LD 		A, D
					ADD		'0'
					LD      (XMESSFilename), A
					JR 		cleanExit

XUndone				POP IX				; Make sure IX is correct
					PUSH IX
					RES		4, (IX-1)						
					JP 		cleanExitNotdone


XMessage			LD 		L, D ;  LSB at L
					POP 	IX
					POP 	BC
					INC 	BC	 ; We need not only to increase BC, but also make sure when exiting it returns increased, and cleanExit will restore it from stack so we have to update valus at stack
					PUSH 	BC
					PUSH 	IX
					LD 		A, (BC)
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
					LD 		(CloseFile+1),A ; Preserve file handle to be able to close it later
					LD 		(XmessReadFile+1),A ; Preserve file handle to be able to read from it
; Seek file					
					LD		DE, (XMessBuffer)	 ; Restore file offset
					LD 		BC, 0   			; BCDE --> Offset 
					LD 		IXL, 0    			; L=0 --> Seek from start
					RST 	$08
					DB 		F_SEEK

; Read file					
					LD 		IX, XMessBuffer
					LD      BC, 512
XmessReadFile		LD 		A, $FF; // Self modified above
					RST     $08
					DB      F_READ  ; Read 

; At this point we have the message at XmessBuffer		
XmessPrintMessage	POP 	IX
					SET 	6, (IX-01)  ; Required by DAAD to print messages
					PUSH 	IX
					LD 		HL, XMessBuffer
					EI
CallPrintMsg		CALL	DAAD_PRINTMSG_ADDR_ES					
					DI
					JR CloseFile  


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
						LD HL, DAAD_PRINTMSG_ADDR_EN    ; Path the DAAD "PrintText" function
						LD (CallPrintMsg+1), HL
						RET


Filename				DB 	"UTO.ZXS",0
ImgNumLine					DB 	0
SaveLoadFilename		DB 	"PLACEHOLD.SAV",0
SaveLoadExtension		DB 	".SAV", 0
XMESSFilename			DB  "0.XMB",0
XMessBuffer				DS 512


