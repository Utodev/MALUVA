; MALUVA (C) 2018 Uto
; MIT License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS
; Thanks to Boriel for his 8 bits division code in ZX-Basic, DivByTen function is based on his code
; Thanks to rafagr32, wilco, augustoruiz, fer and some others I'm surely forgetting to mention, who helped
; a lot with their valuable CPC knowledge

			ORG $28BC
                        OUTPUT  MLV_CPC.BIN
	

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************

			; Firmware calls
			define CAS_IN_OPEN     	$bc77
			define CAS_IN_DIRECT	$bc83
			define CAS_IN_CLOSE    	$bc7a
			define CAS_IN_CHAR 	$bc80
			define SCR_SET_INK	$bc32
			define SCR_SET_MODE	$bc0e			
			define TXT_OUTPUT 	$bb5a
			define KM_WAIT_CHAR 	$bb06


			; Other contants
			define BUFFER2K_ADDR	$0040
			define TWO_KILOBYTES   	$0800
			define VRAM_ADDR 	$C000 ; The video RAM address


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
			JP 	NZ, cleanExit


; ---- Set the filename
LoadPicture		LD 	A, D		; Restore first parameter
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


; --- silence CPC firmware messages
			LD A, 201 				; RET
			LD (TXT_OUTPUT),A

			;LD 	A, $00
			;CALL 	BIOS_SET_MESSAGE			; Silences as A!=0 (for sure, as it is at least the ascii code of '0')

; --- open file and reads CPC header, leaving file pointer just after
                        LD 	B, 7			; Filename length
                        LD 	HL, Filename		; Points to file name
                        LD 	DE, BUFFER2K_ADDR	; Load file header at VRAM address
                        CALL 	CAS_IN_OPEN			
                        JP 	NC, cleanExit
                        JP 	Z, cleanExit

                        LD 	DE, -4
                        ADD 	HL, DE
                        PUSH 	HL
                        POP 	IY


; Check if it is full picture to load it in a different way, a full screen picture is $3E84 bytes long (16.388, 16.384 data, 4 for palette)
                        LD 	A, (IY+28)			
                        OR 	A
                        JR 	NZ, LoadPartialPicture 
                        LD 	A, (IY+29)
                        CP 	$40
                        JP 	NZ, LoadPartialPicture

; Load Whole graphic and palette
LoadFullPicture		LD	HL, VRAM_ADDR + 16384 - 8  ; Point to spare zone, plenty of zeros. Blan Palette
			CALL 	SetPalette
			LD 	HL, VRAM_ADDR		   ;  Read all picture date in screen
			CALL 	CAS_IN_DIRECT
			LD 	HL, VRAM_ADDR + 16384 -4   ;  Palette fw colors have been loaded at the end
			CALL    SetPalette
			JR 	FileLoaded		; Finish but close file                        

; read file info
LoadPartialPicture      CALL 	CAS_IN_CHAR		; Force loading first 2K buffer
                        LD 	(ScansPer2kBuffer),A    ; Preserve number of blocks per 2KBuffer
                        LD 	IXH, A 			; But keep it at IXH too for fast use
                        
; read the palette	
			LD 	HL, BUFFER2K_ADDR + 1	; Preserves the palette for later
			LD 	DE, Palette
			LDI
			LDI
			LDI
			LDI

			PUSH 	HL
			LD 	HL, Palette
			LD 	E, 4			; Set 4 colors palette
			CALL 	SetPalette
			POP 	HL

                        LD 	C, (HL)
                        INC	HL
                        LD 	B, (HL)			; BC Contains how many bytes to read per iteration
                        INC 	HL

                                     
; read data - for CPC we read as much bytes as numLines / 8 * 80, that is numLines * 10, 8 times. 



			LD 	IXL, 8			; 8 times
			LD 	DE, VRAM_ADDR		; Point to RAM addr
			
ReadFileLoop		PUSH 	BC
			PUSH 	DE
			LDIR
			POP 	DE
			POP 	BC	
			DEC 	IXH			; One scan less in the 2K block
			JR 	Z, Next2KBlock
ReadFileCont		PUSH 	HL
			LD 	HL, $800		; Offset from one CPC line to the one inmediatly below
			ADD 	HL, DE			; Make HL point to next line
			LD	D, H			
			LD 	E, L			; and move it to DE
			POP 	HL			; restore HL
			DEC 	IXL
			JR 	NZ, ReadFileLoop


; ---- Close file		
FileLoaded		CALL CAS_IN_CLOSE
	
cleanExit		LD 	A, $CF			; restore original value (RST $8)
			LD 	(TXT_OUTPUT),A

			EI
			POP 	IX
			POP 	BC
			RET




; Both read savegame and load savegame use the same code, that is just slightly modified before jumping in the common part at DoReadOrWrite



; ********************************************************************                        
;                           AUX FUNCTIONS
; *******************************************************************


; ** Receives at HL a pointer to a 4 byte buffer with ink colors, E is the number of colors to set
SetPalette		XOR A
SetPaletteLoop 		LD 	B,(HL)
			LD 	C, B
			PUSH 	AF
			PUSH 	HL
			CALL 	SCR_SET_INK
			POP     HL
			POP 	AF
			INC 	HL
			INC 	A
			CP 	4
			JR 	NZ, SetPaletteLoop
			RET




; ** Forces another 2K buffer to be loaded from disk and sets back IXH and HL registers to control that buffer 

Next2KBlock		LD 	HL, BUFFER2K_ADDR + TWO_KILOBYTES	; Set buffer pointer to end of buffer
			LD 	(IY+2), L
			LD 	(IY+3), H
			LD 	HL, TWO_KILOBYTES
			LD 	(IY+$17), 0			; Change the header so length is 2K
			LD 	(IY+$18), 0
			CALL 	CAS_IN_CHAR			; force loading another 2K

			LD 	A, (ScansPer2kBuffer)
			LD 	IXH, A				; restore number of scans to read on next buffer
			LD 	HL, BUFFER2K_ADDR			; restore source pointer to beginning of buffer
			JR 	ReadFileCont



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

; *** Clears the screen by filling with zeroes

ClearScreen		XOR 	A
			LD 	HL, VRAM_ADDR
			LD 	(HL),A
			LD 	DE, VRAM_ADDR + 1
			LD 	BC, 16383
			LDIR  
			RET


Filename		DB 	"000.CPC"
ScansPer2kBuffer	DB 	0
Palette			DS 	4

