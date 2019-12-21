; MALUVA (C) 2018 Uto
; LGPL License applies, see LICENSE file
; TO BE COMPILED WITH 64tass Turbo Assembler Macro V1.54.1900
; 64tass.exe -a maluva_c64.asm -o MLV_C64.BIN -b
; Thanks to Lasse at the lemon64 forums for his invaluable help. Thanks to Iszell for adapting code to be also compatible with Commodore Plus/4

; Please notice this is my first ever code usin the 65xx assembler. I have no previous experience so
; I haven't worked too much in optimization

*       = $38BC

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************

			
					PTR              = $02
					PTR2             = $04
					DRVNUM           = $BA
					DRVNUMPLUS4      = $AE
					KERNAL_SETLFS    = $FFBA
					KERNAL_SETNAM    = $FFBD 
					KERNAL_OPEN      = $FFC0
					KERNAL_CLRCHN    = $FFCC
					KERNAL_CHKIN     = $FFC6
					KERNAL_CLOSE     = $FFC3
					KERNAL_CHRIN     = $FFCF
					KERNAL_READST	 = $FFB7     

					VIDEORAM_PIXELS  = $E000
					VIDEORAM_ATTRS   = $CC00
					VIDEORAM_ATTRS_PLUS4   = $BC00
					BACKGROUND_COLOR = $D021
					BACKGROUND_COLOR_PLUS4 = $FF19

					DAAD_PRINT_MSG_C64_ES = $1C0D
					DAAD_PRINT_MSG_C64_EN = $1B78
					DAAD_PRINT_MSG_PLUS4_ES = $1BFC
					DAAD_PRINT_MSG_PLUS4_EN = $1B67

; 					DAAD seems to simulate some Z80 REGS with 0-page addresses
					BC      	=    253
					B       	=    254
					C       	=    253
					HL      	=    251
					H       	=    252
					L       	=    251
;					Also, DAAD "PRINT" routine expecst data here
					MSGDATAH   	=     66
					MSGDATAL   	=     65


; ********************************************************************                        
;                                 MAIN
; ********************************************************************

; Extern call will bring first parameter in A register and second parameter in Y register

        	
Start				PHP							; Save status register
					SEI 						; Disable interrupt
					STA     Registro1
					TYA
					LDX     Registro1            ; Now X has first parameter, and A has second (function number)
					CMP 	#0
					BEQ		LoadImg
					CMP 	#4
					BEQ     XPart
					CMP 	#3
					BEQ 	+
					JMP		CleanExit
+					JMP 	Xmessage				
					

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

        			LDX 	#<Filename
        			LDY 	#>Filename
					LDA     #5
					JSR 	OpenFile

					JSR KERNAL_CHRIN		; Read number of attribute lines
					STA Registro1			; Save number of attr lines
					JSR KERNAL_READST		; Read file status
        			BNE Eof      			; either EOF or read error, leave
					LDA Registro1
					PHA

; Clear the screen
ClearAttr1          LDA #<VIDEORAM_ATTRS
        			STA PTR
PatchAttrs1			LDA #>VIDEORAM_ATTRS
					STA PTR+1
					PLA						; Restore number of attr  lines
					PHA 					; And save it again
        			TAX						; and move to X
					LDY #$00				; Fill with 00 color
					JSR ClearMem

ClearPixels         PLA						; Restore number of attr  lines
					PHA						; and save it again
					ASL
					ASL
					ASL						; multiply by 8 to get the real number of lines
					TAX                     ; Move number of lines to X
					LDA #<VIDEORAM_PIXELS
        			STA PTR
        			LDA #>VIDEORAM_PIXELS
        			STA PTR+1
					LDY #0					; fill with $00
					JSR ClearMem

ClearAttr2          LDA #<VIDEORAM_ATTRS
        			STA PTR
PatchAttrs2			LDA #>VIDEORAM_ATTRS
					STA PTR+1
					PLA						; Restore number of attr  lines
					STA PTR2			    ; and save it for plus/4
        			TAX						; and move to X
					LDY #$B0				; Fill with B0 color
					JSR ClearMem


LoadPixels			LDA #<VIDEORAM_PIXELS
        			STA PTR
        			LDA #>VIDEORAM_PIXELS
        			STA PTR+1
					JSR ReadCompressedBlock

LoadAttrs			LDA #<VIDEORAM_ATTRS
					STA PTR
PatchAttrs3 		LDA #>VIDEORAM_ATTRS
        			STA PTR+1
					JSR ReadCompressedBlock
PatchJSR=*+1
					LDA #$2c
					STA PatchJSR1

Eof        			LDA #$02
        			JSR KERNAL_CLOSE  		; close file
        			JSR KERNAL_CLRCHN 		; restore input to default channel (keyboard)

PatchSTA2
CleanExit			BIT $ff3f
PatchJSR1			BIT ConvertColors
					LDA #$3b
PatchSTA4			BIT $ff06
                    LDA #$2c
					STA PatchJSR1
					PLP						; Restore status register (and previous interrupt status as interrupt status is a flag just like Z or C)
					RTS


; ----------------------------- Xpart --- Handles wich part (part 1 or part 2 of the game is being played)

XPart 				TXA
					BEQ CleanExit	; If part == 0 just ignore
					LDA #50
					STA XPartPart
					JMP CleanExit

; ---------------------------- XMessage

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
					LSR	; Now A has file number (based on first file is 00)
					CLC	
					ADC XPartPart ; Now A has fiel number + 50 if part is not first part
					LDY #10					
					JSR Divide
					CLC							; Clear carry
					ADC 	#'0'				; inc to ascii equivalence
					STA XmessageFilename + 1
					TXA
					ADC 	#'0'				; inc to ascii equivalence
					STA XmessageFilename + 0  ; File name string is now ready

; -------           Open Messages file
        			LDX #<XmessageFilename
        			LDY #>XmessageFilename
					LDA #2
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

					LDA <#XmessageBuffer   		; LSB of XMessageBuffer
					STA MSGDATAL
					LDA >#XmessageBuffer   		; LSB of XMessageBuffer
					STA MSGDATAH				; DAAD print message code expects data here
					CLI
					LDA $0810					; Spanish interpreter has a 0x85 at address $0810, while English one has a 0xDC
					CMP #$DC
					BNE +
					JSR DAAD_PRINT_MSG_C64_EN
					JMP Eof
+					JSR DAAD_PRINT_MSG_C64_ES
					JMP Eof




;-------------------------------------------------------------
;                             AUX FUNCTIONS
;-------------------------------------------------------------

; Opens file whose name it's at X-Y and length at A				

OpenFile			STY RegistroY
					STX RegistroX
					PHA
PatchBIT1			JSR PatchPlus4
PatchSTA1			BIT $ff3e
					LDA #$0b
PatchSTA3			BIT $ff06
					LDA #0
					STA SecondaryAddress+1  ; SETLFS for open
					PLA
					LDX RegistroX
					LDY RegistroY
		 			JSR KERNAL_SETNAM
        			LDA #$02				; Logical number
PatchDrvNum			LDX DRVNUM       		; last used device number
        			BNE SecondaryAddress
        			LDX #$08      			; default to device 8
SecondaryAddress	LDY #$00      			; not $01 means: load to address stored in file
        			JSR KERNAL_SETLFS
					JSR KERNAL_OPEN 		; open file
        			BCC +
					PLA						; Just to clear stack return value
					JMP CleanExit
+					LDX #$02				; Use file #2 for input/output
        			JSR KERNAL_CHKIN		; Set input to file
					JSR KERNAL_READST
        			BNE OpenFileError		; either EOF or read error
					RTS
OpenFileError		PLA						; Just to clear stack return value
					JMP Eof					



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
        			BNE Eof      			; either EOF or read error
					JSR KERNAL_CHRIN		; Read repeat value
					STA CompressedCMP+1     ; Save repeat Value self-modifying the code


CompressedLoop		JSR KERNAL_READST		; Read file status
        			BNE Eof      			; either EOF or read error, leave
					JSR KERNAL_CHRIN    	; Read value
CompressedCMP		CMP #0					; This #0 value is replaced above with the "repeat follows" byte. If found two bytes follow (repeats, value)
					BNE CompressNoRep
					JSR KERNAL_READST		; Read file status
        			BNE Eof      			; either EOF or read error, leave
					JSR KERNAL_CHRIN    	; Read number of repeats
					CMP #0        			; if zero repeats, it's end of block
					BNE CompresRep
					RTS						; End of block found

CompresRep          STA Registro1           ; Preserve repeats
					JSR KERNAL_READST		; Read file status
        			BNE Eof      			; either EOF or read error, leave
					JSR KERNAL_CHRIN    	; Read value to repeat
					LDX Registro1           ; Restore number of repeats
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

PatchPlus4			LDA #$2c
					STA PatchBIT1			 ; Patch JSR to BIT $xxxx: do not call this patch anymore
					LDA $ff00				 ; Check if we have TED timer1 at $ff00
					CMP $ff00
					BEQ -					 ; No, we don't have TED so this is a C64

					LDA #$8d				 ; STA $xxxx
					STA PatchSTA1
					STA PatchSTA2
					STA PatchSTA3
					STA PatchSTA4

					LDA #DRVNUMPLUS4
					STA PatchDrvNum+1

        			LDA #>VIDEORAM_ATTRS_PLUS4
					STA PatchAttrs1+1
					STA PatchAttrs2+1
					STA PatchAttrs3+1
					
					LDA #$20
					STA PatchJSR			 ; JSR $xxxx
					LDA #$74				 ; SDIplus4 magic byte
					STA PatchCMP
					LDA #<DAAD_PRINT_MSG_PLUS4_ES
					STA PatchJSR2
					LDA #>DAAD_PRINT_MSG_PLUS4_ES
					STA PatchJSR2+1
					LDA #<DAAD_PRINT_MSG_PLUS4_EN
					STA PatchJSR3
					LDA #>DAAD_PRINT_MSG_PLUS4_EN
					STA PatchJSR3+1
					RTS

PatchPlus4_2 		RTS					

ConvertColors		LDA PTR2
					STA Registro1
					LDA #<VIDEORAM_ATTRS_PLUS4
					STA PTR
					STA PTR2
					LDA #>VIDEORAM_ATTRS_PLUS4
					STA PTR+1
					AND #$F8
					STA PTR2+1
-					LDY #39
-					LDA (PTR),Y
					PHA
					AND #$0f
					TAX
					LDA bclrtab,x
					STA (PTR),Y
					LDA blumtab,x
					STA (PTR2),Y
					PLA
					LSR
					LSR
					LSR
					LSR
					TAX
					LDA fclrtab,x
					ORA (PTR),Y
					STA (PTR),Y
					LDA flumtab,x
					ORA (PTR2),Y
					STA (PTR2),Y
					DEY
					BPL -
					CLC
					LDA #40
					ADC PTR
					STA PTR
					STA PTR2
					BCC +
					INC PTR+1
					INC PTR2+1
+					DEC Registro1
					BNE --
					RTS

; ---------------------------- XMessage

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

PatchBIT1_2
					JSR PatchPlus4_2
PatchSTA1_2			BIT $ff3e
					LDA #$0b
PatchSTA3_2			BIT $ff06
					LDA #0
					STA SecondaryAddress2+1  ; SETLFS for open
					LDA #$02
        			LDX #<XmessageFilename
        			LDY #>XmessageFilename
		 			JSR KERNAL_SETNAM
        			LDA #$02				; Logical number
PatchDrvNum_2		LDX DRVNUM       		; last used device number
        			BNE SecondaryAddress2
        			LDX #$08      			; default to device 8
SecondaryAddress2	LDY #$00      			; not $01 means: load to address stored in file
        			JSR KERNAL_SETLFS
					JSR KERNAL_OPEN 		; open file
        			BCC +
					JMP CleanExit
+					LDX #$02				; Use file #2 for input/output
        			JSR KERNAL_CHKIN		; Set input to file
					JSR KERNAL_READST
        			BNE Eof      

; --------  Simulate fseek					
					LDA H	
					AND #$07
					STA H					; Store with real offset within file
					ORA L
					BEQ ReadMsg             ; Ofsset is zero, no fseek		

					LDA H          			; HL now has the real offset within the file, but with MSB (H) increased by 1, which will make the DEC H down here end loop on first decrement
					INC H					; otherwise, first decrement would turn H int $FF, not zero 
-					JSR KERNAL_CHRIN		
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

					LDA <#XmessageBuffer   		; LSB of XMessageBuffer
					STA MSGDATAL
					LDA >#XmessageBuffer   		; LSB of XMessageBuffer
					STA MSGDATAH				; DAAD print message code expects data here
					CLI
					LDA $0810					; Spanish interpreter has a 0x93 at address $0810
PatchCmp=*+1
					CMP #$93
					BNE +
PatchJSR2=*+1
					JSR DAAD_PRINT_MSG_C64_ES
					JMP Eof
PatchJSR3=*+1
+					JSR DAAD_PRINT_MSG_C64_EN
					JMP Eof

; ------------------------------- Variables and tables  -----------------

Filename			.text 	'00064'          ; 000 will be replaced by location number (i.e. 128, 078, 003)
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
;------------------------ Buffer is left last on purpose, so in case someone does not use xmessages, the binary file can be cutted to have 512 bytes less
XmessageBuffer		.fill   511
XmessageBufferLast  .byte 0			; .fill doesn't work if there is nothing after the fill, so instead of a 512 bytes fill, I do 511 and then a "db"
