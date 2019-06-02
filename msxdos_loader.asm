
; MALUVA for MSX (C) 2018 FX, Armando Perez Abad & Uto
; MIT License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS


                        OUTPUT "msxdaad.com"

; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************
                        define  Start                    $0100
                        define  Start2                   $8100


                        define  FORCLR                   $F3E9
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

                        define  SC2BUFFER                Start2 + End2Copy - Start2Copy + 10

                        define  LDIRVM                   $005C          ; BIOS call to copy data to video memory
                        define  CHGMOD                   $005F          ; Bios call to change video mode
                        define  CALSLT                   $001C
                        define  INIGRP                   $0072

;Spanish Interpreter Address
                        define  CurrentSlotAddress_S     $d053         ; This is te one used by DAAD, otherwise use $FFE0

                        define  GameStart_Spanish        $cff3        ; Address to start the game after the OUT(A8)

                        define  PatchSetROM_Spanish      $cf84
                        define  PatchSetRAM_Spanish      $cf8f

                        define  PatchInterrupt_Spanish   $d026        ;Interrupt address 

                        define  Patch1S                  $c453        ; Patch all scroll and copy routines to the new MDG Position
                        define  Patch2S                  $c465
                        define  Patch3S                  $c50c
                        define  Patch4S                  $c522
                        define  Patch5S                  $c53b
                        define  Patch6S                  $c54f
                        define  Patch7S                  $c649
                        define  Patch8S                  $ccf7
                        define  Patch9S                  $cd1b

;English Interpreter Address
                        define  CurrentSlotAddress_E     $cfbe         ; This is te one used by DAAD, otherwise use $FFE0

                        define  GameStart_English        $cf5e        ; Address to start the game after the OUT(A8)

                        define  PatchSetROM_English      $ceef
                        define  PatchSetRAM_English      $cefa

                        define  PatchInterrupt_English   $cf91        ;Interrupt address 

                        define  Patch1E                  $c3c0        ; Patch all scroll and copy routines to the new MDG Position
                        define  Patch2E                  $c3d2
                        define  Patch3E                  $c479
                        define  Patch4E                  $c48f
                        define  Patch5E                  $c4a8
                        define  Patch6E                  $c4bc
                        define  Patch7E                  $c5b6
                        define  Patch8E                  $cc64
                        define  Patch9E                  $cc88

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


; ---- Load Loading Screen

                        LD      HL, FilesBinContent + 33
                        LD      DE, SC2BUFFER
                        CALL    MSXDOS1Load
                        LD      A, (LoadSuccessful)             ; This is checked only for the loading screen as it's optional and we need to know if we should draw it or not
                        OR      A
                        CALL    Z, DrawLoadingScreen



; ---- MDG files should be loaded so last byte is at $AFFF, that is, $B000 minus file size, but as we don't know size beforehand, we load it at $100, then move it to proper position

                        LD      DE, $100		            ; Load graphics database at $100 (temporary location)
                        LD 	HL, FilesBinContent		; Filename pointed by first file in FILES.BIN
                        CALL    MSXDOS1Load

                        LD 	BC, (FCB + 10h)		       ; Position of real number of bytes readed
                        LD 	HL, $B000		
                        OR 	A			              ; Clear Carry flag
                        SBC	HL, BC			 
                        LD      D, H
                        LD      E, L                        ; DE = $B000 - file size
                        PUSH    DE                          ; Save position of  MDG for later use
                        LD 	HL, $100		
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

                        LD      A, ($B001)
                        CP      $3A
                        JR      z, PatchCodeEnglish

PatchCodeSpanish        LD      A, $F3                         ;DI
                        LD      (PatchSetRAM_Spanish), A

                        LD      A, $CD                         ;CALL
                        LD      (PatchSetROM_Spanish), A
                        LD      (PatchSetRAM_Spanish + 1), A

                        LD      A, $FB                         ;EI
                        LD      (PatchSetROM_Spanish + 3), A

                        LD      A, $C9                         ;RET
                        LD      (PatchSetROM_Spanish + 4), A
                        LD      (PatchSetRAM_Spanish + 4), A

                        LD      HL, ActivateBIOS
                        LD      (PatchSetROM_Spanish + 1), HL

                        LD      HL, ActivateGame
                        LD      (PatchSetRAM_Spanish + 2), HL

                        LD      A, $56                         ;IM1 instead of IM2
                        LD      (PatchInterrupt_Spanish), A

                        POP     HL                                ; Restore position where MDG was loaded
                        DEC     H                                 
                        DEC     H                                 ; Point  512 bytes below
                        LD      (Patch1S), HL                      ; Patch all scroll and copy routines
                        LD      (Patch2S), HL
                        LD      (Patch3S), HL
                        LD      (Patch4S), HL
                        LD      (Patch5S), HL
                        LD      (Patch6S), HL
                        LD      (Patch7S), HL
                        LD      (Patch8S), HL
                        LD      (Patch9S), HL

                        LD      A, $FF                         ;Tells the current selected slot is the game slot, not BIOS slot
                        LD      (CurrentSlotAddress_S), A        ; No need to preserve it, the jumping routine does it
                        JR      RunGame


PatchCodeEnglish        LD      A, $F3                         ;DI
                        LD      (PatchSetRAM_English), A

                        LD      A, $CD                         ;CALL
                        LD      (PatchSetROM_English), A
                        LD      (PatchSetRAM_English + 1), A

                        LD      A, $FB                         ;EI
                        LD      (PatchSetROM_English + 3), A

                        LD      A, $C9                         ;RET
                        LD      (PatchSetROM_English + 4), A
                        LD      (PatchSetRAM_English + 4), A

                        LD      HL, ActivateBIOS
                        LD      (PatchSetROM_English + 1), HL

                        LD      HL, ActivateGame
                        LD      (PatchSetRAM_English + 2), HL

                        LD      A, $56                         ;IM1 instead of IM2
                        LD      (PatchInterrupt_English), A

                        POP     HL                                ; Restore position where MDG was loaded
                        DEC     H                                 
                        DEC     H                                 ; Point  512 bytes below
                        LD      (Patch1E), HL                     ; Patch all scroll and copy routines
                        LD      (Patch2E), HL
                        LD      (Patch3E), HL
                        LD      (Patch4E), HL
                        LD      (Patch5E), HL
                        LD      (Patch6E), HL
                        LD      (Patch7E), HL
                        LD      (Patch8E), HL
                        LD      (Patch9E), HL

                        LD       A, $FF                           ;Tells the current selected slot is the game slot, not BIOS slot
                        LD       (CurrentSlotAddress_E), A        ; No need to preserve it, the jumping routine does it

                        LD      HL, CurrentSlotAddress_E
                        LD      (BiosPatch+1),HL
                        LD      (GamePatch+1),HL



RunGame                 LD       A, $F                            ; Changes screen colors
                        LD       (FORCLR), A
                        LD       A, 1
                        LD       (BAKCLR), A
                        LD       (BDRCLR), A

                        LD      A, ($B001)
                        CP      $3A
                        JP      z, GameStart_English
                        JP      GameStart_Spanish                ; Run game!



; --- MSXDOS1Load -----------------------------------------------------------
;Input:	 HL	 Points to filename
;	 DE 	 Destination Address
; Output: Carry flag is set on return in case of failure
; ---------------------------------------------------------------------------
MSXDOS1Load		PUSH 	  DE			; Preserve destintion address
			PUSH 	  HL	 		; Preserve file name 
                        

                        LD        DE, FCB			; Fills FCB with Zeroes
                        XOR       A
                        LD        (DE), A
                        LD        H, D
                        LD        L, E
                        INC       DE
                        LD        BC, 1Eh + 11
                        LDIR				

                        POP 	  HL			; Copy Filaname into FCB to conform an unopened FCB
                        LD        DE, FCB + 1		
                        LD        BC, 11
                        LDIR				                     			

                        LD        C, F_OPEN
                        LD        DE, FCB
                        CALL      BDOS

                        POP 	  DE			; restore destination address
                        OR        A
                        RET       NZ			

 
ReadFile                LD        C, F_SET_TRANSFER_ADDRESS
	                CALL      BDOS
                
			LD        HL, 1
                        LD        (FCB + 14), HL         ; set record size = 1

	                LD 	  HL, $FFFF		; Read as much bytes as possible
                        LD        C, F_RANDOM_BLOCK_READ
                        LD        DE, FCB
                        CALL      BDOS

CloseFile               LD        C, F_CLOSE
                        LD        DE, FCB
                        CALL      BDOS	
                        XOR       A
                        LD        (LoadSuccessful),A   
                        RET
               
; ---------------------------------------------------------------------------
ActivateBIOS
                        PUSH    HL
                        PUSH    DE
                        PUSH    BC
                        LD      A, (EXPTBL)
                        CALL    SetSlotOnPage0
                        XOR     A
BiosPatch               LD      (CurrentSlotAddress_S), A
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
GamePatch               LD      (CurrentSlotAddress_S), A
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

; ---------------------------
; DrawLoadingScreen
; Sets video mode 2 and copy SC2 file loaded at $100 to video ram
; ----------------------------

DrawLoadingScreen       
SetMode                 LD      HL, FORCLR 
                        LD      (HL), $0f
                        INC     HL
                        LD      (HL), $01
                        INC     HL
                        LD      (HL), $01
                        ld	A,2
                        ld	($FCAF),A
                        LD      IY,(EXPTBL-1)
                        LD      IX, CHGMOD
                        CALL    CALSLT                  ; CALSLT is used to call BIOS functions when MSXDOS is loadd, IX should have
                        ld	IY,(EXPTBL-1)
	                ld	IX, INIGRP
	                call	CALSLT	          

                        
PaintPixels             LD      HL, SC2BUFFER + 7  ; SC2 files have a 7 bytes header
                        LD      DE, 0
                        LD      BC, $3800 ; Whole SC2 file minus 7
                        LD      IY,(EXPTBL-1)
                        LD      IX, LDIRVM
                        CALL    CALSLT
PaintAttrs              RET


; ---------------------------------------------------------------------------
FilesBinFileName	db      "FILES   BIN"
FilesBinContent	        db      "DAAD    MDG"
           		db      "DAAD    DDB"
           		db      "DAAD    Z80"
                        db      "DAAD    SC2"
LoadSuccessful          db      1                        

        dephase

End2Copy
        
