; MALUVA (C) 2018 Uto
; MIT License applies, see LICENSE file
; TO BE COMPILED WITH 64tass Turbo Assembler Macro V1.54.1900
; 64tass.exe -a maluva_c64.asm -o MLV_C64.BIN -b
; Thanks to Lasse at the lemon64 forums for his invaluable help

; Please notice this is my first ever code usin the 65xx assembler. I have no previous experience so
; I haven't worked too much in optimization


*       = $38BC

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************

			
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
					BACKGROUND_COLOR = $D021



; ********************************************************************                        
;                                 MAIN
; ********************************************************************

; Extern call will bring first parameter in A register and second parameter in X register

Start				SEI 						; Disable interrupt
					STY 	RegistroY           ; Preserve Y register (who knows what's in)
					STA     Registro1
					TXA
					LDX     Registro1            ; swap X and A , now X has first parameter, and A has second (function number)
					CMP 	#0
					BEQ		LoadImg
					JMP		CleanExit



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

OpenImgFile			LDA #0
					STA SecondaryAddress+1  ; SETLFS for open
					LDA #$05
        			LDX #<Filename
        			LDY #>Filename
		 			JSR KERNAL_SETNAM
        			LDA #$02				; Logical number
        			LDX $BA       			; last used device number
        			BNE SecondaryAddress
        			LDX #$08      			; default to device 8
SecondaryAddress	LDY #$00      			; not $01 means: load to address stored in file
        			JSR KERNAL_SETLFS
					JSR KERNAL_OPEN 		; open file
        			BCS  CleanExit
					LDX #$02				; Use file #2 for input/output
        			JSR KERNAL_CHKIN		; Set input to file
					JSR KERNAL_READST
        			BNE Eof      			; either EOF or read error
					JSR KERNAL_CHRIN		; Read number of attribute lines
					STA Registro1			; Save number of attr lines
					JSR KERNAL_READST		; Read file status
        			BNE Eof      			; either EOF or read error, leave
					LDA Registro1
					PHA

; Clear the screen
ClearAttr1          LDA #<VIDEORAM_ATTRS
        			STA $AE
        			LDA #>VIDEORAM_ATTRS
					STA $AF
					PLA						; Restore number of attr  lines
					PHA 					; And save it again
        			TAX						; and move to X
					LDY #$00				; Fill with 00 color
					JSR ClearMem


ClearPixels         PLA						; Restore number of attr  lines
					PHA						; amnd save it again
					ROL
					ROL
					ROL						; multiply by 8 to get the real number of lines
					TAX                     ; Move number of lines to X
					LDA #<VIDEORAM_PIXELS
        			STA $AE
        			LDA #>VIDEORAM_PIXELS
        			STA $AF
					LDY #0					; fill with $00
					JSR ClearMem

ClearAttr2          LDA #<VIDEORAM_ATTRS
        			STA $AE
        			LDA #>VIDEORAM_ATTRS
					STA $AF
					PLA						; Restore number of attr  lines
        			TAX						; and move to X
					LDY #$B0				; Fill with B0 color
					JSR ClearMem
				

LoadPixels			LDA #<VIDEORAM_PIXELS
        			STA $AE
        			LDA #>VIDEORAM_PIXELS
        			STA $AF
					JSR ReadCompressedBlock

LoadAttrs			LDA #<VIDEORAM_ATTRS
        			STA $AE
        			LDA #>VIDEORAM_ATTRS
        			STA $AF
					JSR ReadCompressedBlock

Eof        			LDA #$02
        			JSR KERNAL_CLOSE  		; close file
        			JSR KERNAL_CLRCHN 		; restore input to default channel (keyboard)

CleanExit 			LDY RegistroY			; Restore original Y value
					CLI
					RTS


; Clears Mem ar ($AE), as many bytes as the value received in X multuplied by 40, and filling with the value at Y
ClearMem			STY ClearValue+1
					LDY #0

ClrOuterLoop		LDA #40
					STA Registro1
ClearValue			LDA #00					; this value is modified above
					
ClrInnerLoop		STA	($AE),Y	  			; Store value in RAM
  					INC $AE 				; Increase pointer LSB
        			BNE ClrDoNotIncMSBAttr              
        			INC $AF                 ; Increase pointer MSB
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
					STA	($AE),Y				; Store value in RAM
  					INC $AE 				;  Increase pointer LSB
        			BNE DoNotIncMSBAttr              
        			INC $AF                 ; Increase pointer MSB
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
					RTS



Filename			.text 	'00064'          ; 000 will be replaced by location number (i.e. 128, 078, 003)


; ------------------------------------ Additional memory address used as auxiliary register
Registro1			.byte 0
Registro2           .byte 0
; ------------------------------------ Variables to preserve register Y while extern is running
RegistroY			.byte 0			


