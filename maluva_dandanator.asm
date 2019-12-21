; MALUVA (C) 2018 Uto, 
; LGPL License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS

; Important note: this is a preliminary work on Maluva for Dandanator for ZX Spectrum. It has worked ofr XPICTURE,
; but other Maluva functions has not been created, and the code is partially abandoned, or actually waiting for
; someone to make use of it


; Notes: 
; Dandanator can page any of its 32 slots over $0000-$3999, and the original ROM if the slot selected is slot 33
; Dandanator can also be locked to avoid unwanted paging. Each game has a slot, the "slot 1", which loads the game, 
; but as there may be more than one game inside a Dandanator, there may be more than one "Slot 1", and not necessarily
; be the real slot #1 of Dandanator. So we will refer to that slot 1 as "myslot". The packager that creates ROM to be
; pushed to Dandanator will patch this extern with the real slot that will be used as Slot 1, that's why there is a
; variable named MySlot in this code in a position that is allways the same.
; Also, this extern will contain the routines that need to be in RAM, but to save space, part of the code that is 
; used only when dandanator "slot 1"  is paged will be located at slot1, to avoid using space in RAM. That's why
; you will see some RST calls down there, they are actually calls to the ROM present at Slot 1, not real Spectrum ROM calls


				ORG $843C							
            			OUTPUT "MLV_DAN.BIN"

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************
				define VRAM_ADDR 		$4000 ; The video RAM address
				define VRAM_ATTR_ADDR   	VRAM_ADDR + $1800 ;  points to attributes zone in VRAM 

				define DAAD_SYSMESS		$6DE8
				define DAAD_ASK_FILENAME	$7056

				define dandaSavegame		$30
				define dandaLoadGame		$20
				define dandaGetImageCoords	$28

; ********************************************************************                        
;                                 MAIN
; ********************************************************************
; Note: the following DB and include should be placed here and should not be modified at all.  Some code stored at Dandanator ROM depend on this code
; and will call to addresses directly, so if you change just one byte it will fail. So ** DO NOT MOVE THIS INCLUDE AND DO NOT CHANGE ITS CONTENTS **
Start				JR	 RealStart
MySlot				DB 	0			; Dandanator ROM Generator will store the slot that works as "Slot1" here
				INCLUDE "dandanAPI.asm"
RealStart			DI
				PUSH 	BC
				PUSH 	IX			; IX points  to flags on entry, 256 bytes above it's current location of objects, and those 512 bytes is what a savegame stores
			
				LD 	D, A			; Preserve first parameter
				LD 	A, (BC)			; Get second parameter (function number) on A

				OR 	A
				JR 	Z, LoadImg
				CP 	1
				JP 	Z, SaveGame
				CP 	2
				JP 	Z, LoadGame
				JP 	cleanexitMin		




LoadImg				LD	L,D			; preserve image number
				CALL 	dandaUnlock 		; dandaUnlock unlocks Dandanator an selects Slot 1
				LD	A,L						
				RST 	dandaGetImageCoords	; given a image number in A record, returns slot where image is in A, and offset in that slot in HL
				JR      C, cleanExit		; if carry is set, the given image was not found
				
	
; Change to sector where the image is
				CALL 	dandaSelectSlot                    
			

; --- read header
                	        LD 	A, (HL)
                        	LD  	(IMGNumLines),A
                        	INC 	HL


; read data - for Spectrum we start by reading  as much thirds of screen as possible, in the first byte of file the number of lines appears, so if there is carry when comparing to 64,
;             it means there are less than 128 lines, and if there is carry when comparing to 128, it means there are more than 128 but less than 192. If no carry, then it's a full
;	      screen. 

				PUSH HL			     	; Preserve data origin
				SUB 	64
				JR 	C, drawPartialThird  	; if thre is no carry, there is at least one whole third of screen
				LD 	BC, 2048
				LD 	H, B
				LD 	L, C
				SUB 	64
				JR 	C, drawWholeThirds   	; if there is still no carry, there are at least two thirds of screen
				ADD 	HL, BC
nextThird		        SUB 	64
				JR 	C, drawWholeThirds 	; if there is still no carry, it's a full screen (3 thirds)
				ADD	HL, BC	           	; read one, two or the three whole thirds
drawWholeThirds			LD 	B, H
				LD 	C, L		
				POP 	HL
				LD 	DE, VRAM_ADDR
				LDIR
	                        
; --- This draws the last third, that is an uncomplete one, to do so, the file will come prepared so instead of an standar third with 8 character rows, it's a rare third with less rows.
;     For easier undesrtanding we will call 'row" each character row left, and 'line' each pixel line left, so each row is built by 8 lines.
;     Usually, you can read all first pixel lines for each rows in a third by reading 256 bytes (32 * 8), but if instead of 8 rows we have less, then we have to read 32 * <number of rows>
;     To determine how many rows are left we divide lines left by 8, but then to calculate how many bytes we have to read to load first pixel line for those rows we multiply by 32,
;     so in the end we multiply by 4. Once we know how much to read per each iteration we have to do 8 iterations, one per each line in a character. So we first prepare <lines left>*4 in
;     BC register,and then just read BC bytes 8 times, increasin DE (pointing to VRAM) by 256 each time to point to the next line inside the row


drawPartialThird        	ADD	A, 64			; restore the remaining number of lines (last SUB went negative)                
	                        JR 	Z,readAttr		; if A = 0, then there were exactly  64, 128, or 192 lines, just jump to attributes section	
				ADD	A, A
				ADD	A, A			; A=A*4. Will never exceed 1 byte as max value for lines is 63, and 63*4 = 252
				LD 	B, 0
				LD 	C, A			; BC = number of bytes to read each time (numlines/ 8 x 32). 
				LD 	IXH, 8			; Times to do the loop, will be used as counter. We don't use B and DJNZ cause we need BC all the time and in the end is less productive
drawLoop			PUSH 	BC
				LDIR
				LD 	E, B			; E =0
				POP 	BC
	                        INC  	D			; Increment  256 to point to next line address
	                        DEC 	IXH
	                        JR 	NZ, drawLoop

; read the attributes 

readAttr			PUSH 	HL
				XOR 	A
				LD	H, A
				LD 	A, (IMGNumLines)	; restore number of lines
				LD 	L, A			; now HL = number of lines 
				ADD 	HL, HL
				ADD 	HL, HL			; Multiply by 4 (32 bytes of attributes per each 8 lines  = means 4 per line)
				LD 	B, H
				LD 	C, L
				POP	HL
				LD 	DE, VRAM_ATTR_ADDR	; attributes VRAM
				LDIR


	
cleanExit   			CALL dandaLock			; ---- Lock dandanator (also select internal rom)
cleanexitMin			EI
				POP 	IX
				POP 	BC
				RET

; Both read savegame and load savegame use the same code, that is just slightly modified before jumping in the common part at DoReadOrWrite

LoadGame			CALL 	askWhichSavegameSlot
				CALL 	dandaUnlock		
				POP 	DE				
				PUSH 	DE
				RST 	dandaLoadGame		; Loads game slot whose number is in A, taken 512 bytes from dandanator to address pointed by DE
				JR 	cleanExit


SaveGame			CALL 	askWhichSavegameSlot
				CALL 	dandaUnlock		
				POP 	HL		
				PUSH 	HL
				RST 	dandaSavegame		; Saves game slot whose number is in A, taken 512 bytes from address pointed by HL
				JR 	cleanExit


; ********************************************************************                        
;                           AUX FUNCTIONS
; *******************************************************************


; �------------------------------------------------------------------------------------�-------------
; This routine ask the user to type a slot number 1-8, and if successful returns slot minus one (0-7)  
; �--------------------------------------------------------------------------------------------------
askWhichSavegameSlot		EI
				CALL 	DAAD_ASK_FILENAME	; Ask for savegame slot number
				DI

; ----- Check slot number is valid 1-8
				LD 	HL, $70B6		; address of filename requested + 1 
				LD 	A, (HL)
				OR 	A			; should be zero
				JR 	NZ, badSlot
				DEC 	HL
				LD 	A, (HL)
				CP	'9'				
				JR 	NC, badSlot
				CP      '1'
				JR 	C, badSlot
				SUB 	'0'+1			; Now A contains the savegame slot number 0-7 
				RET
badSlot				POP 	HL			; This POP is just to keep the stack coherent, as we entered this routine with CALL and leave with JR
				LD 	L, 57			; E/S error -> should be "Invalid slot number"
				CALL    DAAD_SYSMESS
				JR 	cleanExit
			
; ********************************************************************                        
;                           Variables
; *******************************************************************
IMGNumLines			DB 	0


