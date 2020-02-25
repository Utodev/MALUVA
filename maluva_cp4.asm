; MALUVA (C) 2018 Uto
; LGPL License applies, see LICENSE file
; TO BE COMPILED WITH 64tass Turbo Assembler Macro V1.54.1900
; This is a Commodore 64 and Commodore Plus/4 addon, same binary file could be used as EXTERN for both interpreters
; To compile:    64tass.exe -a maluva_c64.asm -o MLV_C64.BIN -b
; Thanks to Lasse at the lemon64 forums for his invaluable help. Thanks to Imre Iszell for adapting code to be also compatible with Commodore Plus/4

; Please notice this is my first ever code using the 65xx assembler. I have no previous experience so I haven't worked too much in optimization

; ORG $70BC
*       = $70BC

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************

			
					PTR              	= $02
					PTR2             	= $04
					DRVNUMPLUS4      	= $AE
					LOGICAL_FILE		= $02
					KERNAL_SETLFS    	= $FFBA
					KERNAL_SETNAM    	= $FFBD 
					KERNAL_OPEN      	= $FFC0
					KERNAL_CLRCHN    	= $FFCC
					KERNAL_CHKIN     	= $FFC6
					KERNAL_CLOSE     	= $FFC3
					KERNAL_CHRIN     	= $FFCF
					KERNAL_READST	 	= $FFB7     

					BITMAP_RAM  		= $2000
					SCREEN_RAM			= $0800
					COLOR_RAM   		= SCREEN_RAM + 1024
					BG_COLOR			= $FF15
					MULTI2_COLOR		= $FF16

					PLUS4_PAGE_ROM  	= $FF3E 		; Store anything in this address to page in ROM
					PLUS4_PAGE_RAM  	= $FF3F 		; Store anything in this address to page in RAM
					SCREEN_CTRL			= $FF06			; Bitwise register control several things in the screen
					BITMAP_MODE			= $FF07			; More screen bitwise data

					DDB_ADDRESS 		= $7080			; DAAD DDB load address

					FLAG				= $02A7 		; DAAD misc variables area
					DONEI				= FLAG+5

					USER				= DDB_ADDRESS - 514; Location of the flags
					MALUVA_REPORT_FLAG = 20


; 					DAAD seems to simulate some Z80 REGS with 0-page addresses
					BC			      	= 253
					B       			= 254
					C			       	= 253
					HL     				= 6 ; it was 251
					H      				= 7 ; it was 252
					L      				= 6 ; it was 251
					DE      			= 139
					E      				= 139
					D      				= 140

; 					Other Page0 addresses usable for a while

					TempVar     		= 141
					TempVarH  			= 142
					TempVarL  			= 141



; ********************************************************************                        
;                                 MAIN
; ********************************************************************

Start				PHP							; Save status register
					SEI 						; Disable interrupt
					STA     Registro1

					LDA 	USER+MALUVA_REPORT_FLAG		; Clear Maluva Report flag bit 7
					AND     #$7F
					STA 	USER+MALUVA_REPORT_FLAG

					TYA
					LDX     Registro1            ; Now X has first parameter, and A has second (function number)
					CMP 	#0
					BEQ		LoadImg
					CMP 	#3
					BEQ 	Xmessage
					CMP 	#4
					BEQ     XPart
					CMP 	#7
					BEQ     XUndone
					CMP		#255
					BEQ 	restoreXmessage
					JMP		ExitWithError2
					

; ---- Set the filename
LoadImg				TXA							; Move first parameter (image number) to A
					LDY   	#100				; Divide by 100
					JSR 	Divide              
					PHA							; preseve reminder
					TXA 						; get quotient
					CLC							; Clear carry
					CLV							; Clear Overflow
					ADC 	#'0'				; inc to ascii equivalence
					STA     Filename			; store value at file name area
					PLA							; restore reminder
					LDY   	#10 				; Divide by 10
					JSR 	Divide              
					CLC							; Clear carry
					ADC 	#'0'				; inc to ascii equivalence
					STA     Filename+2
					TXA
					ADC 	#'0'				; inc to ascii equivalence
					STA     Filename+1

					;LDA #$0b					; Disable screen
        			;STA SCREEN_CTRL	 

        			LDX 	#<Filename
        			LDY 	#>Filename
					LDA     #4
					JSR 	OpenFile

ReadFile			JSR 	KERNAL_CHRIN				; Read number of attribute lines

      				STA 	Registro1					; Save number of attr lines
					JSR 	KERNAL_READST				; Read file status
        			BNE 	ExitWithError				; either EOF or read error, leave and don't close cause it was likely not opened
					LDA 	Registro1
					PHA

; Clear the screen
ClearPixels         PLA						; Restore number of attr  lines
					PHA						; and save it again
					ASL
					ASL
					ASL						; multiply by 8 to get the real number of lines
					TAX                     ; Move number of lines to X
					LDA #<BITMAP_RAM
        			STA PTR
        			LDA #>BITMAP_RAM
        			STA PTR+1
					LDY #0					; fill with $00
					JSR ClearMem

ClearAttr1          LDA #<SCREEN_RAM		; Luminance area
        			STA PTR
        			LDA #>SCREEN_RAM
					STA PTR+1
					PLA						; Restore number of attr  lines
					PHA 					; And save it again
        			TAX						; and move to X
					LDY #$44				; Fill with 00 luminance (medium ink, medium paper)
					JSR ClearMem

ClearAttr2          LDA #<COLOR_RAM			; Color area
        			STA PTR
        			LDA #>COLOR_RAM
					STA PTR+1
					PLA						; Restore number of attr  lines
					STA PTR2			    ; and save it for plus/4
        			TAX						; and move to X
					LDY #$10				; Fill with B0 color
					JSR ClearMem


LoadPixels			LDA #<BITMAP_RAM
        			STA PTR
        			LDA #>BITMAP_RAM
        			STA PTR+1
					JSR ReadCompressedBlock

LoadColors			LDA #<COLOR_RAM
					STA PTR
             		LDA #>COLOR_RAM
        			STA PTR+1
					JSR ReadCompressedBlock

LoadLuminance		LDA #<SCREEN_RAM
					STA PTR
			 		LDA #>SCREEN_RAM
        			STA PTR+1
					JSR ReadCompressedBlock


LoadBGColor			JSR	KERNAL_CHRIN
					STA BG_COLOR


;--- And close file					
					JSR CloseFile

CleanExit  			STA PLUS4_PAGE_RAM	
					
					LDA #$3b
        			STA SCREEN_CTRL
                    LDA #$2c
					PLP						; Restore status register (and previous interrupt status as interrupt status is a flag just like Z or C)
					RTS

CloseFile			LDA #LOGICAL_FILE
        			JSR KERNAL_CLOSE  		; close file
					JSR KERNAL_CLRCHN 		; restore input to default channel (keyboard)
        			RTS					



cleanExitNotdone	STA PLUS4_PAGE_RAM	
					LDA #$3b
        			STA SCREEN_CTRL
					PLP						

					PLA 				; Get LSB of return address
					STA E
					PLA 
					STA D
					JSR IncDE			; First we increase return address by one cause JSR does not stack the return address, but return address-1 (RTS is the one that increases the value)
					JSR IncDE			; The we increment once more to step over the JMP opcode and point to where NXTOP is actually stored in RAM
					LDY #0
					LDA (DE), Y
					STA L
					JSR IncDE
					LDA (DE), Y
					STA H
					LDY #14
-					JSR IncHL			; IncHL 14 times
					DEY
					BNE -
					LDA L
					STA PatchNXTOPJMP+1
					LDA H
					STA PatchNXTOPJMP+2

FakeNXTOP			JSR IncBC
					PLA 
					STA L
					PLA
					STA H
					CLI					; EI is not in NXTOP but it was in Cleanexit
PatchNXTOPJMP		JMP		0			; This will be patched above



; Useful functions
IncHL   			INC     L
        			BNE     +
        			INC     H
+			        RTS

IncBC   			INC     C
        			BNE     +
        			INC     B
+			        RTS


IncDE   			INC     E
        			BNE     +
        			INC     D
+			        RTS

ExitWithError  			JSR 	CloseFile
ExitWithError2			LDA 	USER+MALUVA_REPORT_FLAG			; Sets bit 7 of Maluva report flag
						ORA     #$80
						STA 	USER+MALUVA_REPORT_FLAG
						AND 	#1								; checks if Maluva report flag first bit is 1
						BNE 	cleanExitNotdone 				; if so, exits without setting done status
						JMP		cleanExit



XUndone				LDA #0
					STA DONEI
					JMP cleanExitNotdone



; ----------------------------- Xpart --- Handles wich part (part 1 or part 2 of the game is being played)

XPart 				TXA
					BEQ CleanExit	; If part == 0 just ignore
					LDA #50
					STA XPartPart
					JMP CleanExit



;-------------------------------------------------------------
;                             AUX FUNCTIONS
;-------------------------------------------------------------

;Preserves address of Sysmess0 and replaces with XMessageBuffer address

preserveSysmess0	LDY     DDB_ADDRESS + 18 ; Address of Sysmess Table
        			STY     TempVar
        			LDY     DDB_ADDRESS + 19
        			STY     TempVar+1
					LDY		#0
					LDA     (TempVar),Y         ;  LSB
        			STA     PreserveSysmess0L
					LDA 	<#XmessageBuffer
					STA 	(TempVar),Y
        			INY
        			LDA     (TempVar),Y         ; MSB 
        			STA     PreserveSysmess0L
					LDA 	>#XMessageBuffer
					STA 	(TempVar),Y
					RTS

restoreSysmes0 		LDY     DDB_ADDRESS + 18 ; Address of Sysmess Table
        			STY     TempVar
        			LDY     DDB_ADDRESS + 19
        			STY     TempVar+1
					LDY		#0
					STA     (TempVar),Y         	;  LSB
					LDA     PreserveSysmess0H
        			INY
					STA 	(TempVar),Y				; MSB
					RTS

preserveBC			LDA		C
					STA 	PreserveBC_C
					LDA 	B
					STA 	PreserveBC_B
					RTS

restoreBC			LDA		PreserveBC_C
					STA 	C
					LDA 	PreserveBC_B
					STA 	B
					RTS

					; So this is an unreachable (by the 6502 CPU) piece of codem which is actually DAAD code 
FakeCondacts		.byte 	$36, 0,	$3D, 0, $FF ; SYSMESS 0 EXTERN 0 255




; Opens file whose name it's at X-Y and length at A				

OpenFile			STY RegistroY
					STX RegistroX
					PHA
        			STA PLUS4_PAGE_ROM       
					LDA #0
					STA SecondaryAddress+1  ; SETLFS for open
					PLA
					LDX RegistroX
					LDY RegistroY
		 			JSR KERNAL_SETNAM
        			LDA #LOGICAL_FILE		; Logical number
        			LDX DRVNUMPLUS4       	; last used device number
        			BNE SecondaryAddress
        			LDX #$08      			; default to device 8
SecondaryAddress	LDY #0					; not $01 means: load to address stored in file
        			JSR KERNAL_SETLFS
					JSR KERNAL_OPEN 		; open file
					BCS OpenError
					LDX #LOGICAL_FILE		; Use file for input/output
        			JSR KERNAL_CHKIN		; Set input to file
					JSR KERNAL_READST
					BNE OpenError
					RTS

OpenError			PLA 
					PLA					
					JMP ExitWithError2

; Clears Mem at ($AE), as many bytes as the value received in X multuplied by 40, and filling with the value at Y
ClearMem			STY ClearValue+1
					LDY #0

ClrOuterLoop		LDA #40
					STA Registro1
ClearValue			LDA #00					; this value is modified above
					
ClrInnerLoop		STA	(PTR),Y	  			; Store value in RAM
  					INC PTR 				; Increase pointer LSB
        			BNE ClrDoNotIncMSBAttr              
        			INC PTR+1               ; Increase pointer MSB
ClrDoNotIncMSBAttr  DEC Registro1
					BNE ClrInnerLoop
					DEX
					BNE ClrOuterLoop
					RTS


; Reads RLE compressed data from disk and stores it at ($AE) until a given value with zero repeats appears
ReadCompressedBlock JSR KERNAL_READST
        			BNE ExitWithError  			; either EOF or read error
					JSR KERNAL_CHRIN			; Read repeat value
					STA CompressedCMP+1     	; Save repeat Value self-modifying the code


CompressedLoop		JSR KERNAL_READST			; Read file status
        			BNE ExitWithError			; either EOF or read error, leave
					JSR KERNAL_CHRIN    		; Read value
CompressedCMP		CMP #0						; This #0 value is replaced above with the "repeat follows" byte. If found two bytes follow (repeats, value)
					BNE CompressNoRep
					JSR KERNAL_READST			; Read file status
        			BNE ExitWithError			; either EOF or read error, leave
					JSR KERNAL_CHRIN    		; Read number of repeats
					CMP #0        				; if zero repeats, it's end of block
					BNE CompresRep
					RTS							; End of block found

CompresRep          STA Registro1           	; Preserve repeats
					JSR KERNAL_READST			; Read file status
        			BNE ExitWithError			; either EOF or read error, leave
					JSR KERNAL_CHRIN    		; Read value to repeat
					LDX Registro1           	; Restore number of repeats
					JMP RepeatLoop

CompressNoRep		LDX #1					; If not repeated, value is repeated just once

RepeatLoop 			LDY #0					
					STA	(PTR),Y				; Store value in RAM
  					INC PTR 				;  Increase pointer LSB
        			BNE DoNotIncMSBAttr              
        			INC PTR+1               ; Increase pointer MSB
DoNotIncMSBAttr		DEX 
					BNE RepeatLoop
					JMP CompressedLoop



	

; *** Divides A by  Y and returns the remainder in A and the quotient in X. This code
; is not optimized at all, just using substractions. As this is not the real bottleneck in the
; picture loading, but the disk access, it has been considered optimizing it it's not worth the
; effort, unless it can be done to reduce memory used
Divide				STA Registro2
					STY Registro1
 					LDA #0
   					LDX #8
 					ASL Registro2
DivideL1			ROL
 					CMP Registro1
   					BCC DivideL2
 					SBC Registro1
DivideL2			ROL Registro2
   					DEX
   					BNE DivideL1
					LDX Registro2
-					RTS



; ---------------------------- XMessage

restoreXmessage		JSR restoreBC
					JSR restoreSysmes0
					JMP CleanExit


; We get here wih the LSB of the offset at X and the MSB at the next place pointed by 'BC'
XMessage			STX L
					INC C
					BNE +
					INC B   	; Increase BC, which DAAD uses as PC counter
+					LDY #0
					LDA (BC), Y   ; Load byte pointed by BC (as X=0)
					STA H					
					LSR	
					LSR	
					LSR	; Now X has file number
					LDY #10					
					JSR Divide
					CLC							; Clear carry
					ADC 	#'0'				; inc to ascii equivalence
					STA XmessageFilename + 1
					TXA
					ADC 	#'0'				; inc to ascii equivalence
					STA XmessageFilename + 0  ; File name string is now ready

; -------           Open Messages file
					LDA #$02
        			LDX #<XmessageFilename
        			LDY #>XmessageFilename
					JSR OpenFile


; --------  Simulate fseek					
					LDA H	
					AND #$07
					STA H					; Store with real offset within file
					ORA L
					BEQ ReadMsg             ; Ofsset is zero, no fseek		

					LDA H          			; HL now has the real offset within the file, but with MSB (H) increased by 1, which will make the DEC H down here end loop on first decrement
					INC H					; otherwise, first decrement would turn H int $FF, not zero 
-					JSR KERNAL_CHRIN	
					JSR KERNAL_READST
					BNE ExitWithError
					DEC L
					BNE -
					DEC H
					BNE -

; --------- Read message
ReadMsg				LDA <#XmessageBuffer   ; LSB of XMessageBuffer
					STA L
					LDA >#XmessageBuffer   ; MSB of XMessageBuffer
					STA H
					LDY #0

ReadMsgLoop			JSR KERNAL_CHRIN		; Read number of attribute lines
					TAX
					JSR KERNAL_READST
					AND #$BF					; Discard EOF error
					BNE ExitWithError
					TXA
					EOR #$FF
					CMP #10					; End of message mark
					BEQ TextLoaded
					TXA
					STA (HL), Y
					INC Y
					BNE ReadMsgLoop
					INC H
					BNE ReadMsgLoop        ; BNE cause I'm sure it's not Zero and is faster and shorter than JMP loop


TextLoaded			TXA
					STA (HL), Y				; Save mark of end of message, now message is at XmessageBuffer


; ---------- Print message

XmessPrintMsg		JSR 	preserveBC
					JSR 	preserveSysmess0
					LDA		<#FakeCondacts-1
					STA 	C
					LDA	 	>#FakeCondacts-1
					STA 	B
					JSR 	CloseFile
					JMP 	cleanExit





; ------------------------------- Variables and tables  -----------------

Filename			.text 	'0004'          ; 000 will be replaced by location number (i.e. 128, 078, 003)
XmessageFilename	.text   '00'			 ; 00 will be replaced by file number depending on offset
bclrtab				.byte $00, $01, $02, $03, $04, $05, $06, $07
					.byte $08, $09, $02, $01, $01, $05, $0e, $01
fclrtab				.byte $00, $10, $20, $30, $40, $50, $60, $70
					.byte $80, $90, $20, $10, $10, $50, $e0, $10
blumtab				.byte $00, $70, $30, $40, $30, $40, $10, $60
					.byte $30, $00, $40, $20, $40, $60, $40, $50
flumtab				.byte $00, $07, $03, $04, $03, $04, $01, $06
					.byte $03, $00, $04, $02, $04, $06, $04, $05

XPartPart			.byte 0
; ------------------------------------ Additional memory address used as auxiliary register
Registro1			.byte 0
Registro2           .byte 0
; ------------------------------------ Variables to preserve register Y while extern is running
RegistroX			.byte 0			
RegistroY			.byte 0			
; ------------------------------------
PreserveSysmess0L	.byte 0
PreserveSysmess0H   .byte 0
PreserveBC_C		.byte 0
PreserveBC_B		.byte 0
;------------------------ Buffer is left last on purpose, so in case someone does not use xmessages, the binary file can be cutted to have 512 bytes less
XmessageBuffer		.fill   511
XmessageBufferLast  .byte 0			; .fill doesn't work if there is nothing after the fill, so instead of a 512 bytes fill, I do 511 and then a "db"
