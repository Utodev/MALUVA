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


; --- open file and reads CPC header, leaving file pointer just after
                        LD 	B, 7			; Filename length
                        LD 	HL, Filename		; Points to file name
                        LD 	DE, BUFFER2K_ADDR	
                        CALL 	CAS_IN_OPEN		; Open file
                        JP 	NC, cleanExit
                        JP 	Z, cleanExit

                        LD 	DE, -4			; after opening file, DE points to ASMDOS header, but 4 bytes below the header
                        ADD 	HL, DE			; the address of the 2K buffer prepared for reading is stored, and 2 below
                        PUSH 	HL			; the header it's the current reading pointer when reading with CAS_IN_CHAR
                        POP 	IY			; We store header-4 address ay IY for later use


; Check if it is full picture to load it in a different way, a full screen picture is 16384  bytes long (and includes the palette in the last 4 bytes, over the "spare" zone in any CPC picture)
                        LD 	A, (IY+28)			
                        OR 	A
                        JR 	NZ, LoadPartialPicture 
                        LD 	A, (IY+29)
                        CP 	$40
                        JP 	NZ, LoadPartialPicture

; Load Whole graphic and palette
LoadFullPicture		LD	HL, VRAM_ADDR + 16384 - 8  ; Point to spare zone, plenty of zeros. Blank Palette
			CALL 	SetPalette
			LD 	HL, VRAM_ADDR		   ;  Read all picture data in screen
			CALL 	CAS_IN_DIRECT
			LD 	HL, VRAM_ADDR + 16384 -4   ;  Palette fw colors have been loaded at the end
			CALL    SetPalette
			JR 	FileLoaded		   ; Ok, let's go to exit procedure


; Ok, this codebelow needs an explanation: when CPC reads char by char using CAS_IN_CHAR, it actually reads 2K of data on first CAS_IN_CHAR, and then every 
; subsequent CAS_IN_CHAR reads from the buffer, until the buffer is over, and then the next CAS_IN_CHAR loads another 2K, etc. Despite of that, loading using 
; CAS_IN_CHAR  is extremely slow, and specially in this case where we are really reading to then move to VRAM_ADDR, it's a big waste of time. So what we do? We 
; are going to load the buffer using CAS_IN_CHAR, but then instead of reading character by character using CAS_IN_CHAR, we will use LDIR to copy data to VRAM_ADDR. 
; How does AMSDOS then realizes when the buffer is over? Easy, some lines above we stored at IY the value of two pointers that AMSDOS keeps in RAM when a file is
; opened, the first word value stores the address of the buffer, and the second word value, the address of the current reading position of that buffer, so to
; force AMSDOS to load another 2K when we have not read them using CAS_IN_CHAR, we have just to modiy the pointer, and move it 2K above the start of the buffer
; address, and at that moment, we call again CAS_IN_CHAR, which will realize the buffer is over, and load another 2K.

; Apart of that, the CPC image files contain some padding, so every 2K part of the file contains an integer number of scanline data, that is no data that 
; should be copied with LDIR, will go over the end of block of 2K. So for instance if a scanline of the image uses 0.8K, then two scanlines fit (1.6k), and the
; remaining 0.4K is filled with zeroes in the file, so we read 2K, paint (LDIR) two scanlines, then skip to end of buffer, read another 2K, paint 2 scanlines, skip 
; to en of buffer, etc.


; read file info
LoadPartialPicture      CALL 	CAS_IN_CHAR		; Force loading first 2K buffer
                        LD 	(ScansPer2kBuffer),A    ; First byte in the file (after the header) contains the number of scanlines per 2KBuffer (as describe in previous comment)
                        LD 	IXH, A 			; we also keep it at IXH too for fast use
                        
; read the palette	
			LD 	HL, BUFFER2K_ADDR + 1   ; Next 4 bytes are the palette
			LD 	E, 4			; Set 4 colors palette
			CALL 	SetPalette

			LD 	HL, BUFFER2K_ADDR + 5
                        LD 	C, (HL)
                        INC	HL
                        LD 	B, (HL)			; BC Contains how many bytes there are per scanline
                        INC 	HL

                                     
; read data - for CPC we read as much bytes as numLines / 8 * 80, that is numLines * 10, 8 times. 



			LD 	IXL, 8			; 8 times
			LD 	DE, VRAM_ADDR		; Point to RAM addr
			
ReadFileLoop		PUSH 	BC
			PUSH 	DE			; Yes, you are thiking this can be made faster with LDI, or even with PUSH/POP, and you are right, but
			LDIR				; the bottleneck is the disk reading, so improving this is not noticeable for users, and any other solution
			POP 	DE			; will just require more RAM so it's not worth the effort. 
			POP 	BC	
			DEC 	IXH			; One scanline less in the 2K block
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
FileLoaded		CALL 	CAS_IN_CLOSE
	
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
			XOR 	A
			LD 	(IY+$17), A			; Change the header so length is ok, what will load everything avaliable (equals 65536)
			LD 	(IY+$18), A
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



Filename		DB 	"000.CPC"
ScansPer2kBuffer	DB 	0
Palette			DS 	4

