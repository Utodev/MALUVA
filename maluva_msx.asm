
; MALUVA for MSX (C) 2018 FX, Armando Perez Abad & Uto
; MIT License applies, see LICENSE file
; TO BE COMPILED WITH SJASMPLUS
; Thanks to Boriel for his 8 bits division code in ZX-Basic, DivByTen function is based on his code

                        ORG $013C
                        OUTPUT "MLV_MSX.BIN"



; ********************************************************************                        
;                           CONSTANTS 
; *******************************************************************


                        
                        define BDOS                      $0005   ; MSXDOS function calls
                        define FCB                       $005C   ; File control block address

                        define RG1SAV                    $f3e0   ; VDP copy 1



                        define F_OPEN                    $0F
                        define F_CLOSE                   $10
                        define F_CREATE                  $16
                        define F_SET_TRANSFER_ADDRESS    $1A
                        define F_RANDOM_BLOCK_READ       $27
                        define F_RANDOM_BLOCK_WRITE      $26

                        define DAAD_READ_FILENAME       $BFE3
                        define DAAD_SYSMESS             $B402
                        define DAAD_FILENAME_ADDR       $C011
                        

Start                   
                        DI
; ---- Preserve registers
                        PUSH    BC
                        PUSH    IX


; --- Check function selected
                        LD      D, A
                        LD      A, (BC)

                        OR      A
                        JR      Z, loadImg
                        CP      1
                        JP      Z, saveGame
                        CP      2
                        JP      Z, loadGame
                        JP      cleanExit


; ---- Set the filename

loadImg                 LD      A, D
                        CALL    DivByTen
                        ADD     '0'
                        LD      HL, ImageFilename + 2
                        LD      (HL),A
                        LD      A, D
                        CALL    DivByTen
                        ADD     '0'
                        DEC     HL
                        LD      (HL),A
                        DEC     HL
                        LD      A, '0'
                        ADD     D
                        LD      (HL),A


; --- Open file        
                        LD      HL, ImageFilename                    
                        CALL    openFile                        ; Prepares the FCB and opens the file
                        OR      A                               ; On failure to open, exit
                        JR      NZ, cleanExit

                        




; --- Patch FCB so record size for reading is 1, and define where to read to when reading
                        LD      DE, VRAM_BUFFER
                        CALL    setFCBParams

                        LD      HL, 1                                   ; Blocks to read
                        LD      C, F_RANDOM_BLOCK_READ
                        LD      DE, FCB
                        CALL    BDOS


; ----  Calculate number of lines divided by 8

                        LD      A, (VRAM_BUFFER)
                        SRL     A
                        SRL     A
                        SRL     A


; --- Clears Picture zone screen attributes
                        PUSH    AF
                        LD      DE, $2000
                        call    ClearScreen
                        POP     AF


; --- And load from file, pixels data first, then atributes
                        PUSH    AF                              ; Preserve A value
                        LD      DE, 0                           ; Points to VRAM  (pixel data)
                        CALL    CopyToVRAM
                        POP     AF
                        LD      DE, $2000                      ; Points to VRAM  (attributes data)
                        CALL    CopyToVRAM


; ----- Close file
                        CALL    closeFile

cleanExit               POP     IX
                        POP     BC
                        EI
                        RET


; ******************* SAVE AND LOAD GAME ROUTINES   *************************************************

loadGame                CALL    prepareSaveGame                 ; Reads file name and stores at SavegameFilename properly
                        LD      HL, SavegameFilename            
                        CALL    openFile                        ; Sets the FCB and opens
                        OR      A                               ; On failure to open, exit
                        JR      NZ, diskFailure
                        POP     DE                              ; Get IX from stack, and pushes it again. IX points to where flags/object locations are located in RAM
                        PUSH    DE
                        CALL    setFCBParams                    ; Set to read in 1 byte blocks at DE

                        LD      HL, 512                         ; Blocks to read, 512 bytes
                        LD      C, F_RANDOM_BLOCK_READ
                        LD      DE, FCB
                        CALL    BDOS
                        CALL    closeFile
                        JR      cleanExit


saveGame                CALL    prepareSaveGame                 ; Reads file name and stores at SavegameFilename properly
                        LD      HL, SavegameFilename            ; Opens the file and sets the FCB
                        CALL    createFile
                        OR      A                               ; On failure to open, exit
                        JR      NZ, diskFailure
                        POP     DE                              ; Get IX from stack, and pushes it again. IX points to where flags/object locations are located in RAM
                        PUSH    DE
                        CALL    setFCBParams                    ; Set to write in 1 byte blocks at DE

                        LD      HL, 512                         ; Blocks to read, 512 bytes
                        LD      C, F_RANDOM_BLOCK_WRITE
                        LD      DE, FCB
                        CALL    BDOS
                        CALL    closeFile
                        JR      cleanExit



diskFailure             LD      A, 57                           ; E/S error sysmess
                        LD      HL, $B456                       ; in MSX condacts routies has no RET at the end, they jump to pipeline $B17C again
                        LD      (HL),201                        ; as we need it to return, we are patching temporaryly the SYSMES code. This replaces POP HL with RET
                        PUSH    HL
                        CALL    DAAD_SYSMESS
                        POP     HL                              
                        LD      (HL),03                         ; and this restores POP HL
                        JR      cleanExit                        

prepareSaveGame         CALL    DAAD_READ_FILENAME              ; request a file name from input 
                        LD      HL, DAAD_FILENAME_ADDR          ; copies 8 characters to SavegameFilename where ther eextension is already present
                        LD      DE, SavegameFilename            ; DAAD routine alrady stores a 8 byte length file name at DAAD_FILENAME_ADDR and fills with spaces if needed
                        LD      BC, 8                           
                        LDIR
                        RET

; ********************************************************************                        
;                           AUX FUNCTIONS
; *******************************************************************

;- -------------------------------------------------------------------------------------------
; Input: DE= address to read/write to, it's input to F_SET_TRANSFER_ADDRESS
setFCBParams            LD      HL, 1                           ; SET read block size to 1 byte
                        LD      (FCB + 14), HL            

                        LD      C, F_SET_TRANSFER_ADDRESS       ; Set where to read to (or write to )
                        CALL      BDOS
                        RET



; ---------------------------------------------------------------------------
; DE    Direccion destino en VRAM
; A     Lineas a copiar
CopyToVRAM
                        PUSH    AF
                        LD      A,E                             ; VRAM address setup    
                        OUT     ($99),A
                        LD      A, D
                        OR      $40
                        OUT     ($99),A
                        POP     AF

copyToVRAMLoop          PUSH    AF                              ;lines to load

                        LD      C, F_SET_TRANSFER_ADDRESS
                        LD      DE, VRAM_BUFFER                 ; where to load them
                        CALL    BDOS

                        LD      HL, 100H                        ; Blocks to read = bytes to read
                        LD      C, F_RANDOM_BLOCK_READ
                        LD      DE, FCB
                        CALL    BDOS

                        LD      HL, VRAM_BUFFER
                        LD      C, $98
                        LD      B, 0
;IFNDEF MSX2
copyToVRAMLoop2         OUTI                                    ; Minimum VRAM access timing for MSX1: 29 cycles
                        JP      NZ, copyToVRAMLoop2
;ELSE
;                       OTIR                                    ; Minimum VRAM access timing for MSX2: 15 cycles
;ENDIF

                        POP     AF
                        DEC     A
                        JR      NZ, copyToVRAMLoop
                        RET

; ---------------------------------------------------------------------------
; DE    Direccion destino en VRAM
; A     Lineas a copiar
ClearScreen
                        PUSH    AF                              ; VRAM address setup
                        LD      A,E
                        OUT     ($99),A
                        LD      A, D
                        OR      $40
                        OUT     ($99),A

                        XOR     A                               ; Fill VRAM buffer with zeroes
                        LD      HL, VRAM_BUFFER
                        LD      (HL),A
                        LD      DE, VRAM_BUFFER + 1
                        LD      BC, $FF
                        LDIR
                        POP     AF


ClearScreenLoop         LD      HL, VRAM_BUFFER
                        LD      C, $98
                        LD      B, 0
;IFNDEF MSX2
clearScreenLoop2        OUTI                                    ; Minimum VRAM access timing for MSX1: 29 cycles
                        JP      NZ, clearScreenLoop2
;ELSE
;                       OTIR                                    ; Minimum VRAM access timing for MSX2: 15 cycles
;ENDIF
                        DEC     A
                        JR      NZ, ClearScreenLoop
                        RET

;----------------------------------------------------------------------------
prepareFCB              PUSH    HL
clearFCB                LD      HL, FCB                       ; Fills FCB with Zeroes
                        XOR     A
                        LD      (HL), A
                        LD      DE, FCB +1
                        LD      BC, 1Eh + 11
                        LDIR                   
                        POP     HL                      ; And copies Filename on FCB to open/create
                        LD      DE, FCB + 1           
                        LD      BC, 11
                        LDIR                                                           
                        RET


; openFile -----------------------------------------------------------------
;input:   HL --> points to 11 chars filename
openFile                CALL    prepareFCB
                        LD      C, F_OPEN
                        LD      DE, FCB
                        CALL    BDOS                  
                        RET

; closefile --------------------------------------------------------------------------------------------------------------
closeFile               LD      C, F_CLOSE
                        LD      DE, FCB
                        CALL    BDOS                  
                        RET

; createFile -----------------------------------------------------------------
;input:   HL --> points to 11 chars filename
createFile              CALL    prepareFCB
                        LD      C, F_CREATE
                        LD      DE, FCB
                        CALL    BDOS                  
                        RET



; ---------------------------------------------------------------------------
; *** Divides A by 10 and returns the remainder in A and the quotient in D^***
DivByTen
                        LD      D, A           ; Does A / 10
                        LD      E, 10          ; At this point do H / 10
                        LD      B, 8
                        XOR     A              ; A = 0, Carry Flag = 0
DivByTenLoop
                        SLA     D
                        RLA            
                        CP      E               
                        JR      C, DivByTenNoSub
                        SUB     E               
                        INC     D               
DivByTenNoSub
                        DJNZ    DivByTenLoop
                        RET                    ;A= remainder, D = quotient


; ---------------------------------------------------------------------------
ImageFilename           DB      "000     MS2"
SavegameFilename        DB      "UTO     SAV"        

VRAM_BUFFER
                        DS      $100

; ---------------------------------------------------------------------------
