; MALUVA (C) 2018 Uto
; LGPL License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS , use --exp export.asm parameter so export.asm is generated for the interrupt handler
; Thanks to Boriel for his 8 bits division code in ZX-Basic, DivByTen function is based on his code
; Thanks to Rafagr32, Wilco, Augustoruiz, Fran Gallego, Fer and some others I'm surely forgetting to mention, who helped
; a lot with their valuable CPC knowledge

						ORG $28BC
            			OUTPUT MLV_CPC.BIN
	

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************

						; Firmware calls
						define CAS_IN_OPEN     	$bc77
						define CAS_IN_DIRECT	$bc83
						define CAS_IN_CLOSE    	$bc7a
						define CAS_IN_CHAR 		$bc80
						define SCR_SET_INK		$bc32
						define SCR_SET_MODE		$bc0e			
						define TXT_OUTPUT 		$bb5a
						define KM_WAIT_CHAR 	$bb06


						; Other contants
						define BUFFER2K_ADDR			$0040
						define TWO_KILOBYTES   			$0800
						define VRAM_ADDR 				$C000 ; The video RAM address
						define TIME						$B8B4 ; Auto-increment address
						define CPC_DDB_BASE_ADDRESS 	$2880
						define MALUVA_REPORT_FLAG		20

						; For the interrupt handler
						define NEW_FAST_TICKER			 $BCE0 
						define DEL_FAST_TICKER			 $BCE6
						define FIRMWARE_MODE_NUMBER		 $B7C3
						define DAAD_FAST_TICK_SPACE		 $A609  ;  This is a small area I've found, that is just after the frame flyback area
						define UPPER_MODE				 0 ; default upper mode in split screen 
						define LOWER_MODE				 1 ; default lower mode in split screen 
						define FADE_TO_BLACK			 0 ; default fade or not when in split screen


; ********************************************************************                        
;                                 MAIN
; ********************************************************************
Start			
						DI
						PUSH 	BC
						PUSH 	IX
						RES		7,(IX+MALUVA_REPORT_FLAG)	; Clear bit 7 of flag 28 (mark of EXTERN executed)
						
						LD 		D, A		; Preserve first parameter
						LD 		A, (BC)		; Get second parameter (function number) on A


						CP 		3
						JP 		Z, XMessage
						CP  	4
						JP  	Z, XPart
						CP 		5
						JP  	Z, XBeep
						CP 		6
						JP 		Z, XSplitScreen
						CP 		7
						JP 		Z, XUndone
						CP 		255
						JP 		Z, RestoreXMessage
						OR 		A
						JP 		NZ, ExitWithError


LoadPicture				

LoadPictureCont			LD 	A, D		; Restore first parameter
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


						CALL SilenceFW

; Determine if we make a fade to black 
DoFadeToBlack			CALL 	NZ, HideScreen
						CALL 	WaitForVsync


; --- open file and reads CPC header, leaving file pointer just after
                        LD 		B, 7			; Filename length
                        LD 		HL, Filename		; Points to file name
                        LD 		DE, BUFFER2K_ADDR	
                        CALL 	CAS_IN_OPEN		; Open file
                        JP 		NC, cleanExitRestorePal
                        JP 		Z, cleanExitRestorePal

; ---- Set the filename


						LD 		A, $FF
						LD 		(LastXmessFile), A	; If a file was open, then the 2K buffer is going to be overwritten by the picture, so any Xmessage loaded there will be corruted, we set last XMessage file to 255 to avoid data to be used

                        DEC		HL
                        DEC 	HL			; after opening file, DE points to ASMDOS header, but 2 bytes below the header
                        PUSH 	HL			;  it's the current reading pointer when reading from buffer  with CAS_IN_CHAR
                        POP 	IY			; We store header-2 address at IY for later use


												
; Check if it is full picture to load it in a different way, a full screen picture is 16384  bytes long (and includes the palette in the last 4 bytes, over the "spare" zone in any CPC picture)

				        LD 	A, (IY+26)			
                        OR 	A
                        JR 	NZ, LoadPartialPicture 
                        LD 	A, (IY+27)
                        CP 	$40
                        JP 	NZ, LoadPartialPicture

; Load Whole graphic and palette
LoadFullPicture			LD 		HL, VRAM_ADDR		   		;  Read all picture data in screen
						CALL 	CAS_IN_DIRECT
						LD 		HL, VRAM_ADDR + 16384 - 16   ;  Palette fw colors have been loaded at the end
						CALL	BackupPalette
						CALL    SetPalette
						JR 	FileLoaded		   			; Ok, let's go to exit procedure


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
						LD 		HL, BUFFER2K_ADDR + 1   ; Next 16 bytes are the palette
						LD 		DE, PaletteBuffer
						LD 		BC, 16
						LDIR
						

						LD 		HL, BUFFER2K_ADDR + 17
                        LD 		C, (HL)
                        INC		HL
                        LD 		B, (HL)			; BC Contains how many bytes there are per scanline
                        INC 	HL
						

                                     
; read data - for CPC we read as much bytes as numLines / 8 * 80, that is numLines * 10, 8 times. 



						LD 	IXL, 8				; 8 times
						LD 	DE, VRAM_ADDR		; Point to RAM addr
			
ReadFileLoop			PUSH 	BC
						PUSH 	DE			; Yes, you are thiking this can be made faster with LDI, or even with PUSH/POP, and you are right, but
						LDIR				; the bottleneck is the disk reading, so improving this is not noticeable for users, and any other solution
						POP 	DE			; will just require more RAM so it's not worth the effort. 
						POP 	BC	
						DEC 	IXH			; One scanline less in the 2K block
						JP 		Z, Next2KBlock
ReadFileCont			PUSH 	HL
						LD 		HL, $800		; Offset from one CPC line to the one inmediatly below
						ADD 	HL, DE			; Make HL point to next line
						LD		D, H			
						LD 		E, L			; and move it to DE
						POP 	HL			; restore HL
						DEC 	IXL
						JR 	NZ, ReadFileLoop

; --- Set palette
						LD 		HL, PaletteBuffer
						CALL 	BackupPalette
						CALL 	SetPalette



; ---- Close file		
FileLoaded				CALL 	CAS_IN_CLOSE
						CALL    WaitForVsync
						CALL    WaitForVsync	; Waiting for two Vsync(1/25 second) to make sure everything restabilishes properly




	
cleanExit				LD 	A, $CF			; restore original value (RST $8)
						LD 	(TXT_OUTPUT),A

cleanExit2				EI
						POP 	IX
						POP 	BC
						RET

; It happens the DAAD code sets the "done" status after the execution of an EXTERN, something which happens in a function called NXTOP, which does a few thing and then jumps to 
; another function named CHECK. Due to that , it is not possible to exit an EXTERN without getting the done status set. To avoid that we are going to go through the DAAD interpreter
; code to make what NXTOP does, and then jump to the JP CHECK at the end of that NXTOP function.

cleanExitNotdone		LD 	A, $CF			; this four instructions are just a copy of Cleanexit code except the EI
						LD 	(TXT_OUTPUT),A
cleanExitNotdone2		POP 	IX
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



ExitWithError			CALL ExitWithErrorCommon
						JR 		NZ, cleanExitNotdone
						JR 		cleanExit

						
ExitWithError2 			CALL ExitWithErrorCommon
						JR 		NZ, cleanExitNotdone2
						JR 		cleanExit2

ExitWithErrorCommon		POP 	HL
						POP		IX											; Extract and push again real IX value from stack
						PUSH 	IX		
						PUSH 	HL											; HL is poped and pushed cause it's the function return value 
						SET 	7, (IX + MALUVA_REPORT_FLAG)				; Mark error has happened
						LD 		A, (IX + MALUVA_REPORT_FLAG)				
						AND 	1											; If bit 0 of flag 28 was set, then also exit extern without marking as DONE
						RET


; Please notice parameter <mode> of XSPLITSCREEN doesn't directly match with video modes. For CPC it works like this:
;<Mode> 	upper/lower video mode
; 0               1  /   1        (default)
; 1               0  /   1
; 2               2  /   1
; 3 or above are invaluid, will default to <0> --> 1/1

XSplitScreen			LD 		A, D							; We will store real CPC video mode at E register, to be used later
						LD 		E, 2
						CP 		2
						JR 		Z, XSplitScreenCont				; If mode = 2 them upper screen video mode it's also 2
						LD 		E, 1
						CP 		3
						JR 		NC, XSplitScreenCont			; If no carry, then mode was above 2, we leave with E=1 (upper video mode 1, as <modes> above 2 are invalida), OK
						XOR 	1								; If below below 2, then it could only be 1 or 0, this XOR changes 0 to 1 or 1 to 0
						LD 		E, A							; And we store the upper mode in E, cause as you see in the table above, when <mode> is 0, upper video mode is 1, and viceversa
						
XSplitScreenCont		LD 		A, 1							; For the time being, lower screen mode is always 1
						LD	 	HL, IntPatch2L+1				; Lower  Screen Mode
						LD 		(HL),A
						LD	 	HL, IntPatch3L+1				; Lower  Screen Mode
						LD 		(HL),A
						LD	 	HL, IntPatch4L+1				; Lower  Screen Mode
						LD 		(HL),A
						LD	 	HL, IntPatch5L+1				; Lower  Screen Mode
						LD 		(HL),A

						LD 		A, E							; Upper  Screen Mode
						LD	 	HL, IntPatch1U+1				
						LD 		(HL),A
						JR 		cleanExit2

XPart 					LD 		A, D
						OR 		A
						JR 		Z, cleanExit
						LD 		A, 50
						LD 		(XpartPart),A    ; If parameter != 0, then XPART equals 50 so files are in the range 50-81 instead of 0-31

						JR 		cleanExit2


; XUndone changes DONE status

XUndone					RES		4, (IX-1)						
						JR 		cleanExitNotdone


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

						LD   IX,sfxFreqAY - 48	; DE=get frequency from tone table
						ADD  IX,DE
						LD   E,(IX+0)
						LD   D,(IX+1)


						XOR  A					; REG#0 ChannelA Tone LowByte
						LD 	 C, E
						CALL SetAYREG
						LD   A, 1				; REG#1 ChannelA Tone HighByte
						LD   C, D
						CALL SetAYREG

						LD   A, 8				; REG#8 ChannelA Volume to 8
						LD   C, A
						CALL SetAYREG

						LD   A,7				; REG#7 Mixer enable ChannelA
						LD   C,00111110b
						CALL SetAYREG

BeepSilence				EX   DE,HL
						SRL  E
						CALL NZ,SilenceWait
					
						LD   A,7				; REG#7 Mixer disable ChannelA
						LD   C,00111111b
						CALL SetAYREG

						JP   cleanExit2

SilenceWait									; Wait for E = 1/50 seconds
                        
						LD  HL,TIME			; Cogemos la address donde se cuenta el tiempo en 1/50sec

						
loop0					LD  B, 6
loopA					LD  A,(HL)
loop1					EI
						CP  (HL)
						JR  Z,loop1
						DI
						DJNZ loopA
						DEC E
						JR  NZ,loop0
						RET

; A = port, C = Value
SetAYREG				PUSH BC
			            LD 	 B, $F4
						LD 	 C, A
						OUT  (C), C  ; Register

						LD 	 BC, $F6C0 ; Select Register
						OUT  (C), C

						LD 	 BC, $F600 ; Inactive
						OUT  (C), C

						LD 	 BC, $F680 ; Wtite Value
						OUT  (C), C

						POP BC
			            LD 	 B, $F4
						OUT  (C), C   ; F4 value


						LD 	 BC, $F600 ; Inactive
						OUT  (C), C
						RET						



; Frequencies table
sfxFreqAY				DW	0xD65, 0xC9D, 0xBEB, 0xB42, 0xA9A, 0xA04, 0x971, 0x8E8, 0x86B, 0x7E2, 0x77F, 0x719	// Octave 1 (48-70) 
						DW	0x6B3, 0x64E, 0x5F5, 0x5A1, 0x54D, 0x502, 0x4B9, 0x474, 0x434, 0x3F9, 0x3C0, 0x38C	// Octave 2 (72-96)
						DW	0x359, 0x327, 0x2F6, 0x2D1, 0x2A7, 0x281, 0x25C, 0x23A, 0x21A, 0x1FD, 0x1E0, 0x1C6, // Octave 3 (98-118) 
						DW	0x1AD, 0x194, 0x17D, 0x168, 0x153, 0x141, 0x12E, 0x11D, 0x10D, 0x0FE, 0x0F0, 0x0E3  // Octave 4 (120-142)
						DW	0x0D6, 0x0CA, 0x0BF, 0x0B4, 0x0AA, 0x0A0, 0x097, 0x08F, 0x087, 0x07F, 0x078, 0x072	// Octave 5 (144-166) 
						DW	0x06B, 0x065, 0x05F, 0x05A, 0x055, 0x050, 0x04C, 0x047, 0x043, 0x040, 0x03C, 0x039	// Octave 6 (168-190)
						DW	0x036, 0x033, 0x030, 0x02D, 0x02A, 0x028, 0x026, 0x024, 0x022, 0x020, 0x01E, 0x01C	// Octave 7 (192-214) 
						DW	0x01B, 0x019, 0x018, 0x017, 0x015, 0x014, 0x013, 0x012, 0x011, 0x010, 0x00F, 0x00E  // Octave 8 (216-238)





XMessage				LD 		L, D   ; First parameter (LSB) to L
						POP 	IX
						POP 	BC
						INC 	BC
						LD 		A, (BC) ; Third parameter (MSB) to H
						PUSH 	BC
						PUSH 	IX
						LD 		H, A
						LD 		(AuxVar), HL  ;  Preserve offset 
						LD 		A, H
						SRL		A
						SRL		A
						SRL		A					; Now A contains the file number
						LD 		B,A
						LD 		A, (XpartPart)
						ADD     B

						LD 		B, A				;Check if file loaded is the same than last time, to avoid loading again						
						LD 		A, (LastXmessFile)
						CP 		B
						JR 		Z, AlreadyInRAM

						LD 		A, B				; Otherwise save current file to be read as last read file
						LD		(LastXmessFile),A
						
						

						CALL 	DivByTen
						ADD 	'0';
						LD 		(XMESSFilename+1), A
						LD 		A, D
						ADD 	'0';
						LD 		(XMESSFilename), A ; File name is ready

						CALL 	SilenceFW
						
; --- open file and reads CPC header
                        LD 	B, 6			; Filename length
                        LD 	HL, XMESSFilename		; Points to file name
                        LD 	DE, BUFFER2K_ADDR	
                        CALL 	CAS_IN_OPEN		; Open file
                        JP 	NC, ExitWithError2
                        JP 	Z, ExitWithError

; ---- read the file
						CALL 	CAS_IN_CHAR  ; Although this is supposed to read only one char, in fact it reads one char in A, and a whole 2K at the BUFFER2K_ADDR


AlreadyInRAM			LD 		DE, (AuxVar) ; Restore Global Offset
						LD 		A, D
						AND 	7
						LD 		D, A 		  ; Remove first 5 bits to get the offset within this specific file


						; OK, this below needs to be explained: I didn't find a way to call the DAAD function to print text and make it work with an xmessage already loaded in RAM. Everything
						; worked but the \n or  #n carriage return. The function was at address $1629 in the spanish interpreter. Then I decided to do the following: try to make the text
						; being printed by the MES condact, and to do that I as going to execute a MES condact. How do I do that?
						; 1) I modified first entry in the messages offsets table, preserving the value there before, to the address where the xmessage is loaded in RAM
						; 2) I was udpating BC and making sure it is returned modified to the stack. DAAD uses BC as it's internal "PC" register, so changing BC actually makes DAAD run 
						;    opcodes somewhere else. I pointed it to a zone in RAM (see below) where I already had sorted two condacts: MES 0, and EXTERN 0 255. MES 0 prints the text 
						;    using MES condact and then EXTERN 0 255...
						; 3) is another function in Maluva I'm using to restore the Message 0 pointer, and restoring BC, so the execution continues just after the XMES/XMESSAGE call

						LD 		HL, BUFFER2K_ADDR
						ADD 	HL, DE
						PUSH 	HL			; Preserve the offset where the xmessage has been loaded
						
						LD 		HL, $2892      ; DAAD Header pointer to SYSMESS table
						LD 		E, (HL)
						INC 	HL
						LD 		D, (HL)
						EX      HL, DE		   ; HL points to sysmess  pointers table
						LD 		E, (HL)
						INC		HL
						LD 		D, (HL)  		; Now DE has the value of first message pointer, and HL points to where that pointer is

						LD      (PreserveFirstSYSMES),DE
						POP		DE				; Restore the message offset
						LD 		(HL), D
						DEC		HL
						LD 		(HL), E			; Store the offset at first message pointer 

						POP 	IX
						POP 	BC
						LD 		(PreserveBC), BC
						LD 		BC, FakeCondacts - 1		; Make DAAD "PC" point to our fake condacts
						PUSH 	BC
						PUSH 	IX
						JP 		FileLoaded  ; Close file and exit

						; So this is an unreachable (by the Z80 CPU) piece of codem which is actually DAAD code 
FakeCondacts			DB 		$36, 0, 	$3D, 0, $FF   ; SYSMESS 0 EXTERN 0 255

						


RestoreXMessage			LD 		HL, $2892      ; DAAD Header pointer to SYSMESS table
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
						



						

; Now HL holds the global offset



; ********************************************************************                        
;                           AUX FUNCTIONS
; *******************************************************************




; --- silence CPC firmware messages
SilenceFW				LD A, 201 				; RET
						LD (TXT_OUTPUT),A
						RET


; ** Receives at HL a pointer to a 16 byte buffer with ink colors
SetPalette				XOR 	A
SetPaletteLoop 			LD 		B,(HL)
						LD 		C, B	; Both C and B should receive the color for SCR_SET_INK
						PUSH 	AF
						PUSH 	HL
						PUSH 	DE
						CALL 	SCR_SET_INK
						POP 	DE
						POP     HL
						POP 	AF
						INC 	HL
						INC 	A
						CP 		16
						JR 		NZ, SetPaletteLoop
						RET

; COpies the 16  bits pointed by HL in LastPaletteBuffer. No registers modified
BackupPalette			PUSH 	HL
						PUSH 	DE
						PUSH 	BC
						LD 	 	DE, LastPaletteBuffer
						LD 	 	BC, 16
						LDIR
						POP 	BC
						POP 	DE
						POP 	HL
						RET						


; *** REstores last palette and exits

cleanExitRestorePal     LD HL, LastPaletteBuffer
						CALL SetPalette
						JP ExitWithError


; ** Forces another 2K buffer to be loaded from disk and sets back IXH and HL registers to control that buffer 

Next2KBlock		LD 	HL, BUFFER2K_ADDR + TWO_KILOBYTES	; Set buffer pointer to end of buffer
				LD 	(IY), L
				LD 	(IY+1), H
				XOR 	A
				LD 	(IY+$15), A			; Change the header so length is ok, what will load everything avaliable (equals 65536)
				LD 	(IY+$16), A
				CALL 	CAS_IN_CHAR			; force loading another 2K

				LD 	A, (ScansPer2kBuffer)
				LD 	IXH, A				; restore number of scans to read on next buffer
				LD 	HL, BUFFER2K_ADDR			; restore source pointer to beginning of buffer
				JP 	ReadFileCont



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
DivByTenNoSub			DJNZ 	DivByTenLoop
						RET				;A= remainder, D = quotient


WaitForVsync				LD 	B, $F5   ; PPI port B input
VsyncLoop				IN	A,(C)    ; read PPI port B input
						RRA 
						JR  NC, VsyncLoop
						RET


HideScreen				LD 	HL, PaletteBuffer
						LD 	(HL),0
						LD 	DE, PaletteBuffer + 1
						LD 	BC, 15
						LDIR
						LD 	HL, PaletteBuffer
						CALL SetPalette
						RET


ErrorMode				DB 	0				; 0 = Report errors in flag 128, 1= Report Errors as DONE status
Filename				DB 	"000.CPC"
ScansPer2kBuffer		DB 	0
AuxVar					DW 	0
XMESSFilename			DB  "00.XMB"
PreserveFirstSYSMES		DW 0
PreserveBC				DW 0
XpartPart				DB 0
PaletteBuffer 			DS 16
LastPaletteBuffer 		DB 0,26,24,11  ; Last PaletteBuffer is 16 bytes long, but we want the first 4 bytes to be a paletta with some contrast, in case the firs picture loading fails. Thus, instead of DS 16, we have 4xDB and DS 12
						DS 12
LastXmessFile			DB 255
Time					DB 0			; Used by the MODE0/1 interrupt core
EndOfMainCode


; ------------------------------------------------------------------------------
; --------------- The interrupt Handlers ---------------------------------------
; ------------------------------------------------------------------------------

						ORG EndOfMainCode
            			OUTPUT "MLV_CPC_INT.BIN",t					; Save to a separated file

; ------------ Interrupt handler for XSplitScreen, called on Frame Flyback
InterruptHandler		PUSH BC
						PUSH HL
						PUSH DE
						PUSH AF

; ------------   Set Mode Upper Screen
						DI
IntPatch1U				LD 	A, UPPER_MODE
						OR  $8C
   						LD  B, $7F  
   						OUT (C), A
						LD 	HL, FIRMWARE_MODE_NUMBER ; Set mode to lower part mode
IntPatch2L				LD	(HL),LOWER_MODE   ; Make sure FW is printing text in lower screen mode, always. Please notice text may be being printed at any time, so in case we change mode unadvertedly, part of the text will show wrong
						EXX
						LD 	C, A	; Avoid DAAD or firmware from setting video mode again, put mode in C', used by DAAD after execution of this code to restore mode
						EXX


;PUES NO SE QUE LECHES PASA QUE SI CAMBIO ESE 2 POR UNA A salen mal los textos

; --------------  Init fast tick event

						LD 		HL, Time
						LD 		(HL), 5
						LD 		DE, FastTickerHandler
						LD 		HL, DAAD_FAST_TICK_SPACE
						LD 		C, $FF
						LD		B, 10000000b
						CALL	NEW_FAST_TICKER		; Create tickblock

						
						POP 	AF
						POP 	DE
						POP		HL
						POP 	BC
						EI
						RET


FastTickerHandler		DI
						PUSH 	BC
						PUSH 	HL
						PUSH 	DE
						PUSH 	AF
						LD 		HL, FIRMWARE_MODE_NUMBER
IntPatch5L				LD		(HL), LOWER_MODE
						LD 		HL, Time
						DEC 	(HL)					
						LD 		A, (HL)
						CP 		2					; Do change only in tick 3  (5-3  =2)
						JR 		NZ,FastTickerExit

						LD 		B, 1*64				; Delay to keep a few more lines in mode 0
LoopWait				NOP
						DJNZ 	LoopWait						



; ------------   Set Mode Bottom
						LD 	HL, FIRMWARE_MODE_NUMBER
IntPatch4L				LD	(HL), LOWER_MODE   ; Make sure FW is printing text in mode [dynamically modified], always
IntPatch3L				LD  A, LOWER_MODE
						OR $8C 
   						LD  B, $7F  
   						OUT (C), A
						EXX
						LD 	C, A	; Avoid DAAD or firmware from setting video mode again
						EXX

; ------------- Remove fast ticker
						LD  	HL, DAAD_FAST_TICK_SPACE
						CALL	DEL_FAST_TICKER     ; Delete ticker 
FastTickerExit			POP 	AF
						POP 	DE
						POP		HL
						POP 	BC
						EI
						RET


