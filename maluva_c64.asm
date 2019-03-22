
; MALUVA (C) 2018 Uto
; MIT License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS
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

Start				PHA							; Preserve A, X, Y
					STX RegistroX
					STY RegistroY
					SEI 						; Disable interrupt
					STA     Registro1
					TXA
					LDX     Registro1            ; swap X and A 
					CMP 	#$00
					BEQ 	LoadImg
					CMP 	#$01
					BEQ 	SaveGame
					CMP 	#$02
					BEQ 	LoadGame
					JMP		CleanExit



LoadGame			JMP 	CleanExit
SaveGame			JMP 	CleanExit

; ---- Set the filename
LoadImg				TXA 						; Move first parameter (image number) to A
					JSR 	DivByTen
					CLC
					ADC 	#'0'
					STA     Filename+2
					LDA  	Registro2
					JSR 	DivByTen
					CLC
					ADC 	#'0'
					STA     Filename+1
					LDA  	Registro2
					CLC
					ADC 	#'0'
					STA     Filename

OpenImgFile			LDA #$05
        			LDX #<Filename
        			LDY #>Filename
        			JSR KERNAL_SETNAM
        			LDA #$02				; Logical number
        			LDX $BA       			; last used device number
        			BNE Setlfsskip
        			LDX #$08      			; default to device 8
Setlfsskip   		LDY #$00      			; not $01 means: load to address stored in file
        			JSR KERNAL_SETLFS
        			JSR KERNAL_OPEN 		; open file
        			BCS  CleanExit			; On Carry, error


        			LDX #$02				; Use file #2 for input
        			JSR KERNAL_CHKIN		; Set input to file

					JSR KERNAL_READST
        			BNE Eof      			; either EOF or read error
					JSR KERNAL_CHRIN		; Read number of lines
					


ClearScr           TAX                     ; Move number of lines to X
					LDA #<VIDEORAM_ATTRS
        			STA $AE
        			LDA #>VIDEORAM_ATTRS
        			STA $AF
					LDY #0

ClrOuterLoop		LDA #40
					STA Registro1
					LDA #0
					
ClrInnerLoop		STA	($AE),Y	  			; Store value in RAM
  					INC $AE 				;  Increase pointer LSB
        			BNE ClrDoNotIncMSBAttr              
        			INC $AF                 ; Increase pointer MSB
ClrDoNotIncMSBAttr  DEC Registro1            					
					BNE ClrInnerLoop
					DEX
					BNE ClrOuterLoop
				

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

CleanExit 			LDY RegistroY			; Restore original X,Y,A values and return
					LDX RegistroX
					PLA		
					CLI
					RTS


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








			

; *** Divides A by 10 and returns the remainder in A and the quotient in Registro2. This code
; is not optimized at all, just using substractions. As this is not the real bottleneck in the
; picture loading, but the disk access, it has been considered optimizing it it's not worth the
; effort, unless it can be done to reduce memory used
DivByTen    		LDX 	#$00
 					STX 	Registro1
DivByTenLoop		SEC
					SBC 	#$0A
					BMI     DivByTenEnd
DivByTenCont 		INC 	Registro1
					JMP 	DivByTenLoop
DivByTenEnd 		CLC
					ADC     #$0A
					RTS


Filename			.text 	'00164'
SaveLoadFilename	.text 	'PLACEHOL.SAV'
SaveLoadExtension	.text 	'.SAV'


; ------------------------------------ Additional memory addresses used as CPU registers
Registro1			.byte 0
Registro2			.byte 0
; ------------------------------------ variables to preserve registers X and Y while extern is running
RegistroY			.byte 0			
RegistroX			.byte 0


