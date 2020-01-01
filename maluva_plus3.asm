; MALUVA (C) 2018 Uto
; LGPL License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS
; Thanks to Boriel for his 8 bits division code in ZX-Basic, DivByTen function is based on his code.
; Also thanks to Wilco2009, whose help with the +3DOS routines has been absolutely needed!

			ORG $843C
            OUTPUT  MLV_P3.BIN
	

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************


			define P3DOS_INITIALISE 	$100
			define P3DOS_OPEN 			$106
			define P3DOS_CLOSE			$109
			define P3DOS_READ       	$112
			define P3DOS_WRITE			$115
			define P3DOS_SET_POSITION 	$136
			define BANKM 				$5B5C
			
			
			define INTERPRETER_ADDRESS $6000  ; Address where the interpreter is loaded

			define VRAM_ADDR 	  $4000 ; The video RAM address
			define VRAM_ATTR_ADDR     VRAM_ADDR + $1800 ;  points to attributes zone in VRAM 

			define DAAD_READ_FILENAME_ES $7056	; DAAD function to request a file name
			define DAAD_READ_FILENAME_EN $6FF6	

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
						PUSH 	BC
						LD 		A, (INTERPRETER_ADDRESS+1)
						CP 		$55			
						JR		NZ, Spanish				; In english interpreter, the patch will be applied everytime the extern is called. Although that may 
						CALL 	PatchForEnglish    		; look like losing time, I prefered keeping the code as smaller as possible, so I don't check if patch has been applied
						JR 		LangCont
Spanish					LD 		A, $C9					; (RET)
						LD      (DAAD_PATCH_ES), A 		; Patching the "ask for file name" routine so it doesn't break the words the nwriting messages afterwards
LangCont				POP		BC						; already applied. It takes milliseconds and no one could notice when playing anyway
						POP 	AF
						
; ------- Checking function called						
Init					LD 	D, A		; Preserve first parameter
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
						CP 	7
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


; --- Init OS
						CALL pageinDOS
						CALL P3DOS_INITIALISE
            			JP NC, ExitWithError


; --- open file
            			LD      B,  0		; File handler 0
						LD   	HL, Filename
						LD 		C, 5   ; Open for read exclusive
						LD 		DE, 1  ;  Open Action= open and if has header, skip it.  Create action =fail if not exist
						CALL 	P3DOS_OPEN
                        JP      NC, ExitWithError

;  --- From this point we don't check read failure, we assume the graphic file is OK. Adding more fail control would increase the code size and chances of fail are low from now on

; --- We will load the whole file at $C000, and then move to $4000 using LDIR. We could load the image directly to $4000, but then when reading from slow +3 DISK there would
;     be possible to notice how it is painted.

; --- read file
					PUSH 	DE
					PUSH 	BC
					LD      B, 0
					LD 		C, 7 ; Load at page 7, which happens to be paged at C000 at the moment
					LD 		HL, $C000; Load at C000
					LD 		DE, 6913;  6913 is the maximum possible file (192 lines -fulls screen - plus one byte header header)
					CALL 	P3DOS_READ
					LD 		A, ($C000) ; A register contains number of lines now
					POP 	BC
					POP 	DE
                        
                                            

		
					SUB 	64
					JR 		C, drawPartialThird0  ; if thre is no carry, there is at least one whole third of screen, then we continue, otherwise we jump to paint the partial third
					LD 		BC, 2048 			 ; length of a third
					LD 		H, B
					LD 		L, C
					SUB 	64
					JR 		C, drawWholeThirds   ;if there is still no carry, there are at least two thirds of screen
					ADD 	HL, BC
nextThird	        SUB 	64
					JR 		C, drawWholeThirds ; if there is still no carry, it's a full screen (3 thirds)
					ADD		HL, BC	           ; read one, two or the three whole thirds
					SUB 	64					
drawWholeThirds		PUSH 	HL
					POP 	BC 					; Number of bytes to copy at BC
					LD 		HL, $C001			; origin
					LD 		DE, VRAM_ADDR		; destination
					LDIR 
					JR 		drawPartialThird


                        
; --- This draws the last third, that is an uncomplete one, to do so, the file will come prepared so instead of an standar third with 8 character rows, it's a rare third with less rows.
;     For easier undesrtanding we will call 'row' each character row left, and 'line' each pixel line left, so each row is built by 8 lines.
;     Usually, you can read all first pixel lines for each rows in a third by reading 256 bytes (32 * 8), but if instead of 8 rows we have less, then we have to read 32 * <number of rows>
;     To determine how many rows are left we divide lines left by 8, but then to calculate how many bytes we have to read to load first pixel line for those rows we multiply by 32,
;     so in the end we multiply by 4. Once we know how much to read per each iteration we have to do 8 iterations, one per each line in a character. So we first prepare <lines left>*4 in
;     BC register,and then just read DE bytes 8 times, increasin HL (pointing to VRAM) by 256 each time to point to the next line inside the row

drawPartialThird0		LD 		HL, $C001
						LD 		DE, VRAM_ADDR  ; If the image does not cover a whole third at all, HL and DE are not initialized so they are initialized here


drawPartialThird        ADD	A, 64		; restore the remaining number of lines (last SUB went negative)                
                        OR 	A
                        JR 	Z,readAttr	; if A = 0, then there were exactly  64, 128, or 192 lines, just jump to attributes section	

						ADD	A, A
						ADD	A, A		; A=A*4. Will never exceed 1 byte as max value for lines is 63, and 63*4 = 252
						LD 	B, 0
						LD 	C, A		; BC = number of bytes to copy each time (numlines/ 8 x 32). 

						LD 	A, 8		; Times to do the loop, will be used as counter. We don't use B and DJNZ cause we need BC all the time and in the end is less productive

drawLoop				PUSH BC			; Preserve block to copy length
						PUSH DE			; Preserve destination
						LDIR
						POP DE 			; Restore destination

						INC D			; Point to next line

						POP BC
                        DEC A			; Decrement loop variable
                        JR 	NZ, drawLoop

; read the attributes 

readAttr				PUSH 	HL    ;preserve buffer pointing to attributes
						XOR 	A
						LD		H, A
						LD 		A, ($C000)	; restore number of lines
						LD 		L, A			; now HL = number of lines 
						ADD 	HL, HL
						ADD 	HL, HL			; Multiply by 4 (32 bytes of attributes per each 8 lines  = means 4 per line)
						PUSH 	HL
						POP 	BC
						POP 	HL 				; restore pointer to buffer
						LD 		DE, VRAM_ATTR_ADDR	; attributes VRAM
						LDIR

; ---- Close file	
closeFile				LD 	B, 0
						CALL P3DOS_CLOSE
                        
	
cleanExit				CALL    pageOutDOS
						POP 	IX
						POP 	BC
						EI
						RET


; It happens the DAAD code sets the "done" status after the execution of an EXTERN, something which happens in a function called NXTOP, which does a few thing and then jumps to 
; another function named CHECK. Due to that , it is not possible to exit an EXTERN without getting the done status set. To avoid that we are going to go through the DAAD interpreter
; code to make what NXTOP does, and then jump to the JP CHECK at the end of that NXTOP function.

cleanExitNotdone		CALL    pageOutDOS
						POP 	IX			; Copied from Cleanexit, without the EI
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

LoadGame				LD  	A, P3DOS_READ & $FF
						LD 		(DOSFunction+1),A
						LD 		A, 1 ;  Open Action= open and if has header, skip it.  
						LD  	(OpenAction+1),A
						XOR 	A  ;Create action =fail if not exist
						LD  	(OpenAction+2),A
						JR		DoReadOrWrite

SaveGame				LD  	A, P3DOS_WRITE & $FF
						LD 		(DOSFunction+1),A
						LD 		A, $4 ;  Open Action= Erase existing version. Follow create action.
						LD  	(OpenAction+1),A
						LD 		A, 1 ;    Create action = Create and open new file with a header.
						LD  	(OpenAction+2),A
						JR		DoReadOrWrite


DoReadOrWrite		
						EI
ReadFilenameCall		CALL DAAD_READ_FILENAME_ES	; Ask for file name
						DI
PatchFileName			LD A, $FF							; +3DOS expects it to be $FF terminated
PatchFilenameEON		LD (DAAD_FILENAME_ADDR_ES + 10),A
						

; --- Intialize +3DOS
						CALL 	pageinDOS
						CALL 	P3DOS_INITIALISE
                        JR      NC, ExitWithError

; --- open file
		                LD      B,  0						; File handler 0
DAADFileName			LD   	HL, DAAD_FILENAME_ADDR_ES
						LD 		C, 3   						; Open for read-write
OpenAction				LD 		DE, 1  
						CALL 	P3DOS_OPEN 					; May be modified above
						JR 		NC, diskFailure 			; fail message, no need to close file


; --- read or write file


						POP		HL					; Gets HL value back from stack, then push again. This is done cause flags position is in the stack
						PUSH 	HL
						LD 		DE, 512				; Save flags and objects
                        LD      B, 0				; File handler
						LD 		C, 2 				; Load at page 2, where flags are
DOSFunction			    CALL 	P3DOS_READ
						JP C,   closeFile			; Job done, close file and exit

; ------ close file and fail
diskFailureAndClose		LD 	B, 0	
						CALL P3DOS_CLOSE

diskFailure				CALL pageOutDOS
						LD 		L, 57			; E/S error
DAADSysmesCall			CALL    DAAD_SYSMESS_ES
						JR	 	ExitWithError

XUndone					POP IX					 ; Make sure IX has the proper value
						PUSH IX
						RES		4, (IX-1)						
						JP 		cleanExitNotdone


XPart					LD 		A, D
						ADD		'0'
						LD      (XMESSFilename), A
						JP 		cleanExit


XMessage				LD 		L, D ;  LSB at L
						POP 	IX
						POP 	BC
						INC 	BC	 ; We need not only to increase BC, but also make sure when exiting it returns increased, and cleanExit will restore it from stack so we have to update valus at stack
						PUSH 	BC
						PUSH    IX
						LD 		A, (BC)
						LD 		H, A ; MSB AT H, so Message address at HL

						LD 		IX, (LastOffset)  ; Let's check if it's same message than last time
						CP 		IXH
						JR 		NZ, NotSameMessage
						LD 		A, L
						CP 		IXL
						JR 		Z, XmessPrintMessage  ;If same offset, just print again


NotSameMessage			LD	 	(LastOffset), HL   ; Preserve file offset, using the buffer as temporary address

						CALL pageinDOS
						CALL P3DOS_INITIALISE
            			JP NC, ExitWithError
; Open file				
	            		LD      B,  0		; File handler 0
						LD   	HL, XMESSFilename
						LD 		C, 5   ; Open for read exclusive
						LD 		DE, 1  ;  Open Action= open and if has header, skip it.  Create action =fail if not exist
						CALL 	P3DOS_OPEN
                        JP      NC, ExitWithError

; Seek file						
						LD 		B,0
						LD      HL, (LastOffset)   ; Restore offset
						LD 		E, B                 ; E-HL = offset
					    CALL	P3DOS_SET_POSITION

; --- read file
						LD      B, 0
						LD 		C, 7 ; Load at page 7, which happens to be paged at C000 at the moment
						LD 		HL, $C000			; Load at C000
						LD 		DE, 512;  
						CALL 	P3DOS_READ
						LD 		HL, $C000			; Move data read to Buffer
						LD 		DE, XMessBuffer
						LD 		BC, 512
						LDIR

; --- close file						
 						LD 	B, 0
						CALL P3DOS_CLOSE
						CALL pageOutDOS
				
; At this point we have the message at XmessBuffer		
XmessPrintMessage		POP 	IX
						SET 	6, (IX-01) ; Required by DAAD to print messages
						PUSH 	IX
						LD 		HL, XMessBuffer
						EI
CallPrintMsg			CALL	DAAD_PRINTMSG_ADDR_ES					
						DI
						JP 		cleanExit  



; ********************************************************************                        
;                           AUX FUNCTIONS
; *******************************************************************

;** Functions to page in ROM/RAM for DOS Calls
pageinDOS				ld   BC,$7FFD       ;the horizontal ROM switch/RAM switch I/O address
						LD   A,(BANKM)      ;system variable that holds current switch state
						RES  4,A            ;move right to left in horizontal ROM switch (3 to 2)
						OR   7              ;switch in RAM page 7
						LD   (BANKM),A      ;must keep system variable up to date (very important)
						OUT  (C),A          ;make the switch
						RET

pageOutDOS				LD   BC,$7FFD       ;I/O address of horizontal ROM/RAM switch
						LD   A,(BANKM)      ;get current switch state
						SET  4,A            ;move left to right (ROM 2 to ROM 3)
						AND  248            ;also want RAM page 0
						LD   (BANKM),A      ;update the system variable (very important)
						OUT  (C),A          ;make the switch						
		
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
DivByTenNoSub			DJNZ	DivByTenLoop
						RET				;A= remainder, D = quotient


; ** Function that patches Maluva code so it calls DAAD functions at the proper locations whe the english interpreter has been detected
PatchForEnglish			LD HL, DAAD_READ_FILENAME_EN
						LD (ReadFilenameCall+1), HL
						LD HL, DAAD_SYSMESS_EN
						LD (DAADSysmesCall+1), HL
						LD HL, DAAD_FILENAME_ADDR_EN + 10
						LD (PatchFilenameEON+1), HL
						LD HL, DAAD_FILENAME_ADDR_EN
						LD(DAADFileName+1), HL
						LD A, $C9						; Also patch the "ask for a file name" function so it doesn't make the words be cutted between lines after calling it (patch is RET replacing a RES 6,(IX-$0a))
						LD (DAAD_PATCH_EN), A
						LD HL, DAAD_PRINTMSG_ADDR_EN    ; Path the DAAD "PrintText" function
						LD (CallPrintMsg+1), HL
						RET


Filename				DB 	"UTO.ZXS",$FF
XMESSFilename			DB  "0.XMB",$FF
LastOffset				DW 	$FFFF
XMessBuffer				DS 512


