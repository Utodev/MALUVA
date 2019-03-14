; MALUVA (C) 2018 Uto
; MIT License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS
; Thanks to Boriel for his 8 bits division code in ZX-Basic, DivByTen function is based on his code

; Please notice this is my first ever code usin the 65xx assembler. I have no previous experience so
; I haven't worked too much in optimization, except for the most important bottleneck in the code, that
; is, the image loading from disk part


*       = $38BC

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************

			
					KERNAL_SETLFS   = $FFBA
					KERNAL_SETNAM   = $FFBD 
					KERNAL_OPEN     = $FFC0
					KERNAL_CLRCHN   = $FFCC
					KERNAL_CHKIN    = $FFC6
					KERNAL_CLOSE    = $FFC3
					KERNAL_CHRIN    = $FFCF
					KERNAL_READST	= $FFB7     

					VIDEORAM_PIXELS = $E000
					VIDEORAM_ATTRS  = $CC00



; ********************************************************************                        
;                                 MAIN
; ********************************************************************

; Extern call will bring first parameter in A register and second parameter in X register

Start				SEI 						; Disable interrupt
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



LoadGame			RTS
SaveGame			RTS

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



					LDA #$05
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

					; ???? check drive error channel here to test for
        			; ???? FILE NOT FOUND error etc.


        			LDX #$02				; Use file #2 for input
        			JSR KERNAL_CHKIN		; Set input to file

 					LDA #<VIDEORAM_PIXELS
        			STA $AE
        			LDA #>VIDEORAM_PIXELS
        			STA $AF


        			JSR KERNAL_READST
        			BNE Eof      			; either EOF or read error
					JSR KERNAL_CHRIN		; Read number of lines from file
        			STA Registro1       	; number of lines
        			STA Registro3       	; preserve number of lines for later 

ClearScreen			; Pending

LoadPixels
LineLoop   			LDA #40 				; Bytes per line, number of columns
        			STA Registro2       	; Preserve at Reg2
        			
ColLoop   			JSR KERNAL_READST		; Read file status
        			BNE Eof      			; either EOF or read error, leave
					JSR KERNAL_CHRIN    	; Read a char, value returned in RegA, RegY modified
					LDY #$00
        			STA ($AE),Y   			; Paint pixels read
  					INC $AE 				;  Increase pointer LSB
        			BNE DoNotIncMSB              
        			INC $AF                 ; Increase pointer MSB
DoNotIncMSB			DEC Registro2           ; Decrement columns left
        			BNE ColLoop             ; If more columns iterate over columns
        			DEC Registro1           ; Decrement number of lines
        			BNE LineLoop            ; If more lines, lineloop

JMP Eof
LoadAttrs
					LDA #<VIDEORAM_ATTRS
        			STA $AE
        			LDA #>VIDEORAM_ATTRS
        			STA $AF

LineLoopAttr		LDA Registro3
					ROR
					ROR
					ROR 					; Div by 8
					STA Registro1
					LDA #40 				; Bytes per line, number of columns
        			STA Registro2       	; Preserve at Reg2
        			
ColLoopAttr			JSR KERNAL_READST		; Read file status
        			BNE Eof      			; either EOF or read error, leave
					JSR KERNAL_CHRIN    	; Read a char, value returned in RegA, RegY modified
					LDY #$00
        			STA ($AE),Y   			; Paint pixels read
  					INC $AE 				;  Increase pointer LSB
        			BNE DoNotIncMSBAttr              
        			INC $AF                 ; Increase pointer MSB
DoNotIncMSBAttr		DEC Registro2           ; Decrement columns left
        			BNE ColLoopAttr         ; If more columns iterate over columns
        			DEC Registro1           ; Decrement number of lines
        			BNE LineLoopAttr        ; If more lines, lineloop


Eof        			LDA #$02
        			JSR KERNAL_CLOSE  		; close file
        			JSR KERNAL_CLRCHN 		; restore input to default channel (keyboard)

CleanExit 			CLI
					RTS









			

; *** Divides A by 10 and returns the remainder in A and the quotient in Registro2. This code
; is not optimized at all, just using substractions. As this is not the real bottleneck in the
; picture loading, but the disk access, it has been considered optimizing it it's not worth the
; effort, unless it can be done to reduce memory used
DivByTen    		LDX 	#$00
 					STX 	Registro2
DivByTenLoop		SEC
					SBC 	#$0A
					BMI     DivByTenEnd
DivByTenCont 		INC 	Registro2
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
Registro3			.byte 0


