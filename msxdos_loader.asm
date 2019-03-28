
; MALUVA for MSX (C) 2018 FX, Armando Perez Abad & Uto
; MIT License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS


                        OUTPUT "msxdaad.com"

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************
                        define  Start                    $0100
                        define  Start2                   $8100


                        define  FORCLR                   $f3e9
                        define  BAKCLR                   $f3ea
                        define  BDRCLR                   $f3eb
                        define  EXPTBL                   $fcc1
                        define  RAMAD0                   $f341          

                        ;For MSXDOS
                        define  BDOS                     $0005
                        define  FCB                      $005C
                        define  F_SET_TRANSFER_ADDRESS   $1A
                        define  F_RANDOM_BLOCK_READ      $27
                        define  F_OPEN                   $0F
                        define  F_CLOSE                  $10

                        define  CurrentSlotAddress       $D053         ; This is te one used by DAAD, otherwise use $FFE0

                        define  VRAM_BUFFER              $7400

                        define  GameStart                $cff3        ; Address to start the game after the OUT(A8)

                        define  PatchSetROM              $cf84
                        define  PatchSetRAM              $cf8f

                        define  PatchInterrupt           $d026        ;Interrupt address 


;------------------------------------------------------------------------

                        ORG             Start


; ---- Copy code to real execution address (where is safe to run)

                        DI
                        LD      HL, Start2Copy
                        LD      DE, Start2
                        LD      BC, End2Copy - Start2Copy
                        LDIR
                        JP      Start2

;---------------------------------------------------------

Start2Copy

                        phase   Start2

; ---- Load FILES.BIN
			      LD      DE, FilesBinContent	; Load FILES.BIN at FilesBinContent if avaliable
                        LD      HL, FilesBinFileName	
                        CALL    MSXDOS1Load

; ---- MDG files should be loaded so last byte is at $AFFF, that is, $B000 minus file size, but as we don't know size beforehand, we load it at $100, then move it to proper position

                        LD      DE, $100		            ; Load graphics database at $100 (temporary location)
                        LD 	  HL, FilesBinContent		; Filename pointed by first file in FILES.BIN
                        CALL    MSXDOS1Load

                        LD 	  BC, (FCB + 10h)		       ; Position of real number of bytes readed
                        LD 	  HL, $B000		
                        OR 	  A			              ; Clear Carry flag
                        SBC	  HL, BC			 
                        LD      D, H
                        LD      E, L                        ; DE = $B000 - file size
                        PUSH    DE                          ; Save position of  MDG for later use
                        LD 	  HL, $100		
                        LDIR 				            ; Move MDG to proper position

; ----- Load database at address $100
                        LD      HL, FilesBinContent + 11
                        LD      DE, $100
                        CALL    MSXDOS1Load

; ----- Load interpreter at address $B000

                        LD      HL, FilesBinContent + 22
                        LD      DE, $B000
                        CALL    MSXDOS1Load

; ------ Multiple patchs on interpreter                        

PatchCode	        LD      A, $F3                         ;DI
                        LD      (PatchSetRAM), A

                        LD      A, $CD                         ;CALL
                        LD      (PatchSetROM), A
                        LD      (PatchSetRAM + 1), A

                        LD      A, $FB                         ;EI
                        LD      (PatchSetROM + 3), A

                        LD      A, $C9                         ;RET
                        LD      (PatchSetROM + 4), A
                        LD      (PatchSetRAM + 4), A

                        LD      HL, ActivateBIOS
                        LD      (PatchSetROM + 1), HL

                        LD      HL, ActivateGame
                        LD      (PatchSetRAM + 2), HL

                        LD      A, $56                         ;IM1 instead of IM2
                        LD      (PatchInterrupt), A

                        POP     HL                                ; Restore position where MDG was loaded
                        DEC     H                                 
                        DEC     H                                 ; Point  512 bytes below
                        LD      ($C453), HL                       ; Patch all scroll and copy routines
                        LD      ($C465), HL
                        LD      ($C50C), HL
                        LD      ($C522), HL
                        LD      ($C53B), HL
                        LD      ($C54F), HL
                        LD      ($C649), HL
                        LD      ($CCF7), HL
                        LD      ($CD1B), HL

                        LD       A, $F                            ; Changes screen colors
                        LD       (FORCLR), A
                        LD       A, 1
                        LD       (BAKCLR), A
                        LD       (BDRCLR), A

                        LD       A, $FF                         ;Tells the current selected slot is the game slot, not BIOS slot
                        LD       (CurrentSlotAddress), A        ; No need to preserve it, the jumping routine does it


                        JP       GameStart                      ; Run game!


; --- MSXDOS1Load -----------------------------------------------------------
;Input:	 HL	 Points to filename
;	 DE 	 Destination Address
; Output: Carry flag is set on return in case of failure
; ---------------------------------------------------------------------------
MSXDOS1Load		      PUSH 	    DE			; Preserve destintion address
			      PUSH 	    HL	 		; Preserve file name 
                        

                        LD        DE, FCB			; Fills FCB with Zeroes
                        XOR       A
                        LD        (DE), A
                        LD        H, D
                        LD        L, E
                        INC       DE
                        LD        BC, 1Eh + 11
                        LDIR				

                        POP 	    HL			; Copy Filaname into FCB to conform an unopened FCB
                        LD        DE, FCB + 1		
                        LD        BC, 11
                        LDIR				                     			

                        LD        C, F_OPEN
                        LD        DE, FCB
                        CALL      BDOS

                        POP 	    DE			; restore destination address
                        OR        A
                        RET       NZ			

 
ReadFile                LD        C, F_SET_TRANSFER_ADDRESS
	                  CALL      BDOS
                
			      LD        HL, 1
                        LD        (FCB + 14), HL         ; set record size = 1

	                  LD 	    HL, $FFFF		; Read as much bytes as possible
                        LD        C, F_RANDOM_BLOCK_READ
                        LD        DE, FCB
                        CALL      BDOS

CloseFile               LD        C, F_CLOSE
                        LD        DE, FCB
                        CALL      BDOS			
                        RET
               
; ---------------------------------------------------------------------------
ActivateBIOS
                        PUSH    HL
                        PUSH    DE
                        PUSH    BC
                        LD      A, (EXPTBL)
                        CALL    SetSlotOnPage0
                        XOR     A
                        LD      (CurrentSlotAddress), A
                        POP     BC
                        POP     DE
                        POP     HL
                        RET

; ---------------------------------------------------------------------------
ActivateGame
                        PUSH    HL
                        PUSH    DE
                        PUSH    BC
                        LD      A, (RAMAD0)
                        CALL    SetSlotOnPage0
                        LD      A, $FF
                        LD      (CurrentSlotAddress), A
                        POP     BC
                        POP     DE
                        POP     HL
                        RET

; ---------------------------------------------------------------------------

; ---------------------------
; SetSlotOnPage0
; Sets Slot received as parameter as page 0 in the Z80 memory address space
; A = Slot in  FxxxSSPP format
; ----------------------------
            

SetSlotOnPage0          DI
                        LD      B,A
                        IN      A,(#A8)
                        AND     11111100b
                        LD      D,A
                        LD      A,B
                        AND     #03
                        OR      D
                        LD      D,A
                        OUT     (#A8),A
                        LD      A,B
                        BIT     7,A
                        RET     Z
                        AND     #03
                        RRCA
                        RRCA
                        AND     11000000b
                        LD      C,A
                        LD      A,D
                        AND     00111111b
                        OR      C
                        LD      C,A
                        LD      A,B
                        AND     00001100b
                        RRCA
                        RRCA
                        AND     $03
                        LD      B,A
                        LD      A,C
                        OUT     ($A8),A
                        LD      A,($FFFF)
                        CPL
                        AND     11111100b
                        OR      B
                        LD      ($FFFF),A
                        LD      C,A
                        LD      A,D
                        OUT     ($A8),A
                        RET
 

; ---------------------------------------------------------------------------
FilesBinFileName	      db      "FILES   BIN"
FilesBinContent	      db      "DAAD    MDG"
           		      db      "DAAD    DDB"
           		      db      "DAAD    Z80"


        dephase

End2Copy
        
