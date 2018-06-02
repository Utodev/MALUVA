; MALUVA (C) 2018 Uto
; MIT License applies, see LICENSE file
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
			define FA_READ 		$01
			define FA_WRITE		$02
			define FA_CREATE_AL	$0C

			define VRAM_ADDR 	  $4000 ; The video RAM address
			define VRAM_ATTR_ADDR     VRAM_ADDR + $1800 ;  points to attributes zone in VRAM 

			define DAAD_READ_FILENAME $7056	; DAAD function to request a file name
			define DAAD_SYSMESS 	  $6DE8 ; DAAD function to print a system message

			define DAAD_FILENAME_ADDR $70B5 ; DAAD function where the file name read by DAAD_READ_FILENAME is stored


; ********************************************************************                        
;                                 MAIN
; ********************************************************************

Start			
			DI
			PUSH 	BC
			PUSH 	IX
			
			LD 	D, A		; Preserve first parameter
			LD 	A, (BC)		; Get second parameter (function number) on A

			OR 	A
			JR 	Z, LoadImg
			CP 	1
			JP 	Z, SaveGame
			CP 	2
			JP 	Z, LoadGame
			JP 	cleanExit
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
                        JR      C, cleanExit


; --- open file
                        LD      B, FA_READ   
			LD   	IX, Filename
			RST     $08
                        DB      F_OPEN      
                        JR      C, cleanExit


;  --- From this point we don't check read failure, we assume the graphic file is OK. Adding more fail control would increase the code size and chances of fail are low from now on


; --- Preserve A register, containing the file handle 
                        LD      D,A	

; --- read header
                        LD 	IX, IMGNumLine
                        PUSH 	DE
                        PUSH 	BC
                        LD      BC, 1
                        RST     $08
                        DB      F_READ     
                        POP 	BC
                        POP 	DE
                        LD 	A, (IMGNumLine) ; A register contains number of lines now
                                            

; read data - for Spectrum we start by reading  as much thirds of screen as possible, in the first byte of file the number of lines appears, so if there is carry when comparing to 64,
;             it means there are less than 128 lines, and if there is carry when comparing to 128, it means there are more than 128 but less than 192. If no carry, then it's a full
;	      screen. 

			
			SUB 	64
			JR 	C, drawPartialThird  ; if thre is no carry, there is at least one whole third of screen
			LD 	BC, 2048
			LD 	H, B
			LD 	L, C
			SUB 	64
			JR 	C, drawWholeThirds   ;if there is still no carry, there are at least two thirds of screen
			ADD 	HL, BC
nextThird	        SUB 	64
			JR 	C, drawWholeThirds ; if there is still no carry, it's a full screen (3 thirds)
			ADD	HL, BC	           ; read one, two or the three whole thirds
drawWholeThirds		LD 	B, H
			LD 	C, L		
			LD 	IX, VRAM_ADDR
			PUSH    AF
			LD 	A, D 		; file handle
			PUSH 	DE
                        RST     $08
                        DB      F_READ
                        POP 	DE
                        POP 	AF
                        LD 	IX, VRAM_ADDR
                        ADD 	IX, BC		; Hl points to next position in VRAM to paint at
                        
; --- This draws the last third, that is an uncomplete one, to do so, the file will come prepared so instead of an standar third with 8 character rows, it's a rare third with less rows.
;     For easier undesrtanding we will call 'row" each character row left, and 'line' each pixel line left, so each row is built by 8 lines.
;     Usually, you can read all first pixel lines for each rows in a third by reading 256 bytes (32 * 8), but if instead of 8 rows we have less, then we have to read 32 * <number of rows>
;     To determine how many rows are left we divide lines left by 8, but then to calculate how many bytes we have to read to load first pixel line for those rows we multiply by 32,
;     so in the end we multiply by 4. Once we know how much to read per each iteration we have to do 8 iterations, one per each line in a character. So we first prepare <lines left>*4 in
;     BC register,and then just read BC bytes 8 times, increasin IX (pointing to VRAM) by 256 each time to point to the next line inside the row


drawPartialThird        ADD	A, 64		; restore the remaining number of lines (last SUB went negative)                
                        OR 	A
                        JR 	Z,readAttr	; if A = 0, then there were exactly  64, 128, or 192 lines, just jump to attributes section	
			ADD	A, A
			ADD	A, A		; A=A*4. Will never exceed 1 byte as max value for lines is 63, and 63*4 = 252
			LD 	B, 0
			LD 	C, A		; BC = number of bytes to read each time (numlines/ 8 x 32). 
			LD 	E, 8		; Times to do the loop, will be used as counter. We don't use B and DJNZ cause we need BC all the time and in the end is less productive
drawLoop		LD 	A, D 		; file handle
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

readAttr		XOR 	A
			LD	H, A
			LD 	A, (IMGNumLine)	; restore number of lines
			LD 	L, A			; now HL = number of lines 
			ADD 	HL, HL
			ADD 	HL, HL			; Multiply by 4 (32 bytes of attributes per each 8 lines  = means 4 per line)
			PUSH 	HL
			POP 	BC
			LD 	IX, VRAM_ATTR_ADDR	; attributes VRAM
			LD 	A, D 			; file handle
			PUSH	DE
			RST 	$08
			DB 	F_READ
			POP	DE

; ---- Close file	
			LD 	A, D
			RST     $08
                        DB      F_CLOSE
	
cleanExit		EI
			POP 	IX
			POP 	BC
			RET

; Both read savegame and load savegame use the same code, that is just slightly modified before jumping in the common part at DoReadOrWrite

LoadGame		LD  	A, FA_READ
			LD 	(OpenMode+1),A
			LD  	A, F_READ
			LD 	(ReadWrite),A
			JR	DoReadOrWrite

SaveGame		LD  	A, FA_CREATE_AL
			LD 	(OpenMode+1),A
			LD  	A, F_WRITE
			LD 	(ReadWrite),A


DoReadOrWrite		
			EI
			CALL 	DAAD_READ_FILENAME	; Ask for file name
			DI
			CALL 	cleanSaveName		; Convert DAAD 10 filename into 8.3 filename

; --- Set default disk  
			CALL 	setDefaultDisk
                        JR      C, diskFailure

; --- open file
OpenMode                LD      B, FA_READ		; May be modified by FA_CREATE_AL above
			LD   	IX, SaveLoadFilename
			RST     $08			
                        DB      F_OPEN      
                        JR      C, diskFailure
                        LD 	(SavegameHandle+1), A
; --- read or write file

			POP	IX			; Gets IX value back from stack, then push again
			PUSH 	IX
			LD 	BC, 512			; Save flags and objects
 			RST     $08
ReadWrite               DB      F_READ     		; May be modified by F_WRITE ABOVE
			JR	C, diskFailure
                      

SavegameHandle		LD 	A, $FF			; That $FF will be modifed by code above
			RST     $08
                        DB      F_CLOSE


			JR 	cleanExit	

diskFailure		LD 	L, 57			; E/S error
			CALL    DAAD_SYSMESS
			JR 	cleanExit
			



; ********************************************************************                        
;                           AUX FUNCTIONS
; *******************************************************************

; *** Makes filename read by DAAD to be compliant with ESXDOS 8+3 filenames***
cleanSaveName		LD 	HL, DAAD_FILENAME_ADDR			; address of filename requested
			LD 	BC, $08FF			; we will use LDI together with DJNZ, B will work for DJNZ but we set C to $FF
			LD 	DE, SaveLoadFilename
cleanSaveNameLoop	LD 	A, (HL)				; to avoid LDI decrement of BC to affect B
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
setDefaultDisk		XOR	A  
                        RST     $08 
                        DB      M_GETSETDRV
                        RET
			

; *** Divides A by 10 and returns the remainder in A and the quotient in D^***
DivByTen		LD 	D, A			; Does A / 10
			LD 	E, 10			; At this point do H / 10
			LD 	B, 8
			XOR 	A				; A = 0, Carry Flag = 0
DivByTenLoop		SLA	D
			RLA			
			CP	E		
			JR	C, DivByTenNoSub
			SUB	E		
			INC	D		
DivByTenNoSub		djnz 	DivByTenLoop
			RET				;A= remainder, D = quotient

Filename		DB 	"000.ZXS",0
IMGNumLine		DB 	0
SaveLoadFilename	DB 	"PLACEHOL.SAV",0
SaveLoadExtension	DB 	".SAV", 0


