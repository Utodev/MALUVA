; MALUVA (C) 2023 Uto
; LGPL License applies, see LICENSE file
; TO BE COMPILED WITH vasm 64tass Turbo Assembler Macro V1.54.1900
; This is a Commodore Amiga addon
; To compile:    vasmm68k_std -m68000 -no-opt -Fbin -o MLV_AMIGA.BIN maluva_amiga.asm

; Please notice this is my first ever code for the Amiga, and depite a used 68000 assembly for 
; some weeks 30 years ago, that is all the experience I have, so don't expect too much optimization


; WORK IN PROGRESS, NOT COMPLETED


; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************


                        .equ MALUVA_REPORT_FLAG, 20             ; Flag 20 is internally used by MALUVA for error reporting and control

; ********************************************************************                        
;                                 MAIN
; ********************************************************************

Start:		
                                bra Begin

; ------------------------------- Variables and tables  --------------------------------------
XPartPart:			.byte       0
PreserveA2:                     .long       0
FakeCondacts: 		        .byte 	0x36,0x00,0x3D,0x00,0xFF ; SYSMESS 0 EXTERN 0 255
XmessageBuffer:                 .space     512, 0
Filename:			.byte 	"0.XMB", 0
; --------------------------------------------------------------------------------------------


Begin:
                                move.l    d0,-(sp)                      ; PUSH D0
                                move.l    d1,-(sp)                      ; PUSH D1
                                move.l    a1,-(sp)                      ; PUSH A1
                                move.l    a3,-(sp)                      ; PUSH A3
                                move.l    a4,-(sp)                      ; PUSH A4
                                move.b    0x00(a6,a2), D0               ; Maluva function number to D0 (EXTERN second parameter)
                                move.b    -0x01(a6,a2), D1              ; Maluva parameter (EXTERN first parameter)
                                suba.l    D1, A1                        ; Makes A1 point to start of flags table, by decrementing A1 (points to EXTERN first parameter flag, by D1 (first parameter))
                                adda.l    #MALUVA_REPORT_FLAG, A1       ; Makes A1 point to Maluva control flag
                                and.b     #0x7F, (a6,a1)                ; Clears Maluva error bit in control flag


                                cmp.b #4, D0                            ; Choose function
                                beq XPart
                                cmp.b #3, D0
                                beq XMessage
                                cmp.b #255, D0
                                beq restoreXMessage
                                cmp.b, #7, D0
                                beq XUndone
                                bra ExitWithError                       ; Unsupported function
                                

cleanExit:                      ;//falta cerrar ficheros
                                move.l    (sp)+,a4                      ; POP A4
                                move.l    (sp)+,a3                      ; POP A3
                                move.l    (sp)+,a1                      ; POP A1
                                move.l    (sp)+,d1                      ; POP D1
                                move.l    (sp)+,d0                      ; POP D0
                                rts


cleanExitNotdone:                                                       ; exits without marking the 'done' flag
;// FALTA


ExitWithError:          
                                or.b     #0x80, (a6,a1)                 ; Sets bit 7 of Maluva report flag
                                move.b   0(a6,a1), d0
                                or.b     #1, d0                         ; checks if Maluva report flag first bit is 1
                                bne      cleanExitNotdone               ; if so, exits without setting done status
                                bra      cleanExit                      
                        

						



;-------------------------------------------------------------
;                         MALUVA FUNCTIONS
;-------------------------------------------------------------

restoreXMessage:
                                move.l (PreserveA2), A2
                                bsr restoreSysmess0
                                bra cleanExit


; We get here wih the LSB of the offset at X and the MSB at the next place pointed by 'A2'
XMessage:
                                bsr LoadXMBFile                 ; Loads XMBFile into RAM if not loaded yet
                                bsr preserveSysmess0            ; preserves Sysmess 0
                                bsr PrepareOffset               ; Takes extra parameters from DDB and stores Offset wihin the XMB file in A4
                                bsr replaceSysmess0             ; replaces Sysmess0 with the message pointed by A4
                                move.l A2, (PreserveA2)         ; preserves A2 (BC in Z80)
                                move.l A2, (PreserveA2)         ; preserves A2 (BC in Z80)
                                move.l #FakeCondacts, A2        ; makes DAAD point to fake condacts that will do the required SYSMESS 0
                                bra cleanExit

; ----------------------------- Xpart --- Handles wich part (part 1 or part 2 of the game is being played)
XPart:              
                                cmp.b #2, D1
                                bcc XPartOK 
                                move.b #1, D1           ; If parameter > 1 then parameter = 1
XPartOK:            
                                move.b D1, (XPartPart)
                                bra cleanExit    


; ----------------------------- XUndone --- removes the 'DONE' status
XUndone:
;XUndone			LDA #0
;				STA DONEI
;				BRA cleanExitNotdone


;-------------------------------------------------------------
;                             AUX FUNCTIONS
;-------------------------------------------------------------

; Preserves 512 bytes from the DDB in the location where sysmess0 is stored.
preserveSysmess0:
                                move.l #0, A3
                                move.w 18(A2), A3
                                move.w (A3), A3                     ; Source address within DDB space
                                add.l A2, A3                        ; Source address withing RAM space

                            
                                move.l #XmessageBuffer, A4          ; Destination address within DDB space
                                add.l A2, A4                        ; Destination address within RAM space
                                move.w #512, D0
preserveSysmess0Loop:   
                                move.b 0(A3,D0), 0(A4,D0)
                                dbra D0, preserveSysmess0Loop
                                rts


; restores the 512 bytes previously stored away in the Sysmess 0 area of the DDB
restoreSysmess0:            
                                move.l #0, A3
                                move.w 18(A2), A3
                                move.w (A3), A3                     ; Destination address within DDB space
                                add.l A2, A3                        ; Destination address withing RAM space

                                
                                move.l #XmessageBuffer, A4          ; Source address within DDB space
                                add.l A2, A4                        ; Source address within RAM space
                                move.w #512, D0
restoreSysmess0Loop:
                                move.b 0(A4,D0), 0(A3,D0)
                                dbra D0, preserveSysmess0Loop
                                rts



; Replaces 512 bytes in the Sysmess 0 area of the DDB with 512 bytes in the Xmessages file
; Absolute source address where the XMessages is stored should come in A4 already when this function is called
replaceSysmess0:
                                move.l #0, A3
                                move.w 18(A2), A3
                                move.w (A3), A3                     ; Destination address within DDB space
                                add.l A2, A3                        ; Destination address withing RAM space                        
                                move.w #512, D0
replaceSysmess0Loop:        
                                move.b 0(A4,D0), 0(A3,D0)
                                dbra D0, preserveSysmess0Loop
                                rts



