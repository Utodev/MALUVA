
; API calls, external subroutines for DAAD to relocate in RAM   ALL OF THEM HAVE TO BE EXECUTED FROM RAM
; dandaUnlock: Unlocks Dandanator (also calls first slot)
; dandaLock: Changes to Internal ROM and Lock Dandanator
; dandaSelectSlot: calls a Dandanator slot 

;		Internal routines (but in RAM) not to be used directly from DAAD
;dandaLoadGameRAM: part of dandaLoadGame
;dandaRestoreScratch: Restore scratch zone for dandaSavegame routine

				define HDANCMD		 	0		; For Dandanators commands used as HL#00xx (L is not used)
				define J5555			$1555		; Jedec $5555 with a15,a14=0 to force rom write (PIC will set page 1 so final address will be $5555)
				define J2AAA			$2AAA		; Jedec $2AAA, Pic will select page 0
				define PAUSELOOPSN		64
				define ScratchRAM		$8000		;Scratch area in RAM (4k). Each time to be used (savegame), 1st it's saved on EEPROM 
										;  to SectorScratch, once used it will be restored again prior to return to DAAD
										; Caution: ScratchRAM have to be aligned to #x0000

BeginDanAPI

;dandaUnlock: Unlocks Dandanator (also calls first slot)
;   In  - Nothing
;   Out - Nothing
dandaUnlock
				LD	A,46			; Special Command 46, Lock/Unlock/Inhibit
				LD	DE,$1010		; Command #10=16 for Unlock
				CALL	SENDSPCMD		;Unlock Dandanator Commands
				LD	A,(MySlot)
				JR	SENDNRCMD		;Activate DAAD slot		

;dandaLock: Changes to Internal ROM and Lock Dandanator
;   In  - Nothing
;   Out - Nothing
dandaLock
				LD 	A,40			; Special Command 40, Fast change
				LD	DE,$2104		; Bank #21=33 to select internal rom , E=4 to disable commands until command 46
				;JP	SENDSPCMD		;no jump, the routine is just below

; ----------------------------------------------------------------------------------------
; SENDSPCMD:Send Special Command to Dandanator - Sends Command (a), Data 1 (d) and Data 2 (e)- Prepare for Pulse
; Destroys A, B, D, E, H.
;
;   ********************  MUST BE RUN FROM RAM IF CHANGING SLOTS ********************
;
; ----------------------------------------------------------------------------------------
SENDSPCMD			CALL 	SENDNRCMD		; Send command 	
				LD 	A,D			; Data 1
				CALL 	SENDNRCMD		; Send Data 1
				LD 	A,E			; Data 2
				CALL 	SENDNRCMD		; Send Data 2
SENDCONF			LD 	(HL),A			; Send Confirmation Pulse to dandanator and wait a bit - Also Pause Routine
				JR 	DNRPAUSE		;  replaced the pause here with the existent in SENDRCMD

;dandaSelectSlot: calls a Dandanator slot
; Detroys A,B
dandaSelectSlot
				PUSH	HL
				CALL	SENDNRCMD
				POP	HL
				RET
				
				
; ----------------------------------------------------------------------------------------
; Send Normal Command to Dandanator - Sends Command/Data
;     A  = Cmd/Data,
; Destroys B, H
; NOTE: 0 is signaled by 256 pulses.
;
;    		********************  MUST BE RUN FROM RAM  ********************
;
; ----------------------------------------------------------------------------------------
SENDNRCMD			LD	H,HDANCMD
				LD 	B,A	
NRCMDLOOP			RRD				; Send pulse, this is a 18 t-states instruction which takes some time to avoid very fast pulses (replace 3xNOP)
				DJNZ 	NRCMDLOOP
	
DNRPAUSE			LD 	B, PAUSELOOPSN
WAITXCMD			DJNZ 	WAITXCMD		; Wait command detection timeout and Command execution 
				RET
;DNREPPROMCOMMON: Firsts 2 commands for Erase/Program are the same, so to reduce use of RAM we use them as common
;  IN   - Nothing
;
DNREPPROMCOMMON
				LD	DE,J5555		;We'll use DE to we replace LD(J555),A with LD(DE),A (For jedec commands requiring this address)
				;1st command
				LD 	A, #AA
				LD 	(DE),A			;LD (J5555),A; DE=J5555 so replace with LD (DE),A
				;2nd command
				RRCA				;LD A, $55	; replaced as $AA >> is $55
				LD 	(J2AAA),A
				RET
; ----------------------------------------------------------------------------------------
; DNRSECTERASE: Reduced routine for erasing sector. Customized for DAAD
;	A=Sector to Erase (0..127)
;	Untouched: L
;   Destroys: A,B,C,D,E,H
; ----------------------------------------------------------------------------------------
DNRSECTERASE
				CALL	DNREPPROMCOMMON		;1st and 2nd commands are common to Erase and Prog
				;3rd command
				LD 	A, $80
				LD 	(DE),A
				;4h command
				LD 	A, $AA
				LD 	(DE),A
				;5th command
				RRCA				;LD A, $55	; replaced as $AA >> is $55
				LD 	(J2AAA),A

				;6th command, this commands send the last order for ERASING, only during that time slot will be unavailable for accesing
				;		so these last commands have to be mandatory executed from RAM
				LD 	A, $30
				LD 	(HL),A			; Address into zone of sector to erase
				;Wait for finish programming byte
				
WAITSECRAM			LD	A,(HL)			;1st value readed
				XOR 	(HL)			;Compare tu current value
								;While EEPROM is erasing, it respond toggling D6 bit 0/1 each time reading any address from EEPROM...
								;  ...so 1st value =current value when Erase have finished
				JR 	NZ, WAITSECRAM		;Repeat if togglebit indicate EEPROM is still erasing

				RET
								
; ----------------------------------------------------------------------------------------
; DNRSECTPROG:Reduced routine for programming sector. Customized for DAAD
;     Simplified for programming only one sector
;   B=#00, #10, #20 or #30 depends on sector to write to
;	HL=Address in RAM for origin of data to write to sector (4k). 4K from RAM is saved completely
; ************  MUST BE RUN FROM RAM, DI, AND WITH EXTERNAL EEPROM PAGED IN  *************
; ... Sector must be erased first, as this is an EEPROM, writing can only toggle 1's to 0's
;  ... if not erased sector, the bits in 1 can be toggled to 0, but the bits with 0 cannot be returned to 1
; ----------------------------------------------------------------------------------------
DNRSECTPROG

SECTLPROG			; Sector Loop 4096 times (4k sector)
				CALL	DNREPPROMCOMMON		;1st and 2nd commands are common to Erase and Prog
				;3rd command
				LD 	A, #A0			;LD A,#A0
				LD 	(DE),A			;LD (J5555),A; DE=J5555 so replace with LD (DE),A
				;Data to write
				LD 	A,(HL)			;Read from RAM
				LD	(BC),A			;Write to EEPROM
				
				INC 	HL			;Next addr in RAM
				INC 	BC			;Next addr in EEPROM
					
				LD 	A,B
				AND	#0F
				OR	C			;IF BC=x000 then finish (4k written), 
				JR 	NZ,SECTLPROG		;if not then Continue in SECTLPROG. Program byte is so fast that no additional check are required
				RET
;dandaLoadGameRAM: part of dandaLoadGame
;  IN - A=Num slot to call
;     - HL/BC/DE values for LDIR
;  Destroys B,C,D,E,H,L
dandaLoadGameRAM

				PUSH	HL,BC			;Save HL y BC. register DE is not modified by SENDNRCMD
				CALL	SENDNRCMD		;Call slot per A register
				POP	BC,HL
				JR	intldir			;Jump to LDIR+RET below (so we save 2 bytes in RAM)

;dandaRestoreScratch
;   IN - A=Num slot where Scratch is stored
;      - DE=#0000, #1000, #2000 or #3000 as per sector (4k) to restore to Scratch
;  Destroys B,C,D,E,H,L
dandaRestoreScratch
				CALL	SENDNRCMD				
				EX	DE,HL			;HL have to be the origin (into slot)
				LD	DE,ScratchRAM		;DE=Scratch zone
				LD	BC,#1000		;Copy all the sector (4k)
intldir				LDIR
				RET

				
		
		
EndDanAPI

; ********************************************************************                        
;                       REAL MAIN
; ********************************************************************
