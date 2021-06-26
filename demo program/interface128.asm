;version 1.2 	5-25-2021
;version 1.3	6-26-2021		; cosmetic changes, isr handling optimize, changed to tass assembler, added 128 version;64tass.exe ./interface/interface128.asm -o ./interface/interface128.prg
;acme --cpu 6510 -f cbm -o ./interface/interface128.prg interface/interface128.asm
;exomizer sfx $1c10 -t128 ./interface/interface128.prg -o ./interface/interface128c.prg -Di_effect=2 -x "inc $d020"
;exomizer sfx $1c10 -t128 ./interface/interface128.prg -o ./interface/interface128c.prg -Di_effect=2 -x "inc $fb lda $fb sta $d020 lda #0 sta $d020"
;exomizer sfx $1c10 -t128 ./interface/interface128.prg -o ./interface/interface128c.prg -Di_effect=2 -x "inc $d020 inc $d021 lda #0 sta $d020 sta $d021"



clrscreen	= $e544
portout		= $dd03				; set ports to 255 (output) (#56579)
portloc		= $dd01				; 8-bit controller (0-7)	(#56577)
screenloc	= $0400				; beginning of screen location
borderloc	= $d020				; boreder color location
backgrloc	= $d021				; background color location
setchar		= $0a2c				; 128		;$d018				; charater location
isr			= $0314				; interupt location
returnint	= $ea31				; rti
char		= $3000				; custom char loc
charoff		= $18
ssizel		= $00				; size of screen in LO/HI
ssizeh		= $04
csizel		= $c0				; size of char set LO/HI
csizeh		= $0f
textcolor	= $05				; text color ($05 = white)
bordbackcol	= $00				; background and border color ($00 = black)
on			= $01				; on bit
off			= $00				; off bit


	;
	;	KERNAL FUNCTIONS
	;
	

chrout 		= $ffd2				;write byte to default output. (if not screen, must call open and chkout beforehands.)
								;input: a = byte to write.
								;output: –
								;used registers: –
								;real address: ($0326), $f1ca.
								
plot 		= $cc6a				;128	;$fff0				;save or restore cursor position.
								;input: carry: 0 = restore from input, 1 = save to output; x = cursor column (if carry = 0); y = cursor row (if carry = 0).
								;output: x = cursor column (if carry = 1); y = cursor row (if carry = 1).
								;used registers: x, y.
								;real address: $e50a.
								
getin 		= $eeeb				;128	;$ffe4				;read byte from default input. (if not keyboard, must call open and chkin beforehands.)
								;input: –
								;output: a = byte read.
								;used registers: a, x, y.
								;real address: ($032a), $f13e.

	;	
	;	screen loctions
	;

pbloc		= $04a9				; screen locations (add 3 for each port) 04d1


	;
	;	PORTS
	;
	
pboutput	= %11111111			; 255	(userport output)	
pboff		= %00000000			; 0 	(all off)	
pb0			= %00000001			; 1		(port0)
pb1			= %00000010			; 2 	(port1)
pb2			= %00000100			; 4		(port2)
pb3			= %00001000			; 8		(port3)
pb4			= %00010000			; 16	(port4)
pb5			= %00100000			; 32	(port5)
pb6			= %01000000			; 64	(port6)
pb7			= %10000000			; 128	(port7)
pball		= %11111111			; 255 	(all on)

zp1			= $fb				; zero page 1 location
zp2			= $fd				; zero page 2 location
zp3			= $fe				; zero page 3 location


rvson		= $80				; scrren code to turn ON reverse character
rvsoff		= $7f				; screen code to turn OFF reverse character


	;
	; BASIC STUB
	; 10 SYS (2064)
	; uncomment the following line to start as basic program

;	basic start
;
;*			= $1c01
;bstart		.byte $0d, $08, $0a, $00, $9e
;btext		.text "7184"			; Start address! Be *very* careful!
;			
;	basic stop

*			= $1c10				; SYS7184


codeentry	jsr init			; setup and initialize
						
			jmp loop			; jump to loop
			

init		sei					; stop interupt operation (needed to setup new ISR)
			
			lda #textcolor		; set char color
			jsr clearscreen		; clear the screen
			
			
			;jsr chrout
						
			lda setchar			; get current char info
			sta charbackup		; store it to recover later
			
			; start custom charset code , it is not needed for functionality
			
			lda #<charset		; load custom char set (LO byte)
			ldy #>charset		; (HI byte)
			sta zp1				; set zero page 1
			sty zp1+1
			
			lda #<char			; load to this location (LO byte)
			ldy #>char			; (HI byte)
			sta zp2				; set zero page 2
			sty zp2+1
			
			lda #csizel			; char set size to move (LO byte)
			ldy #csizeh			; (HI byte)
			sta sizel			; store it (LO byte)
			sty sizeh			; (HI byte)
			
			jsr movedown		; move the memory
			
			lda setchar			;#$18			; set to current char location
			and #240
			ora #12
			sta setchar
			
			; end of custom character code			
			

			
			lda #bordbackcol	; set border/background
			sta backgrloc
			sta borderloc
			
			sec
			jsr plot			; set plot points for screen
			stx xstore
			sty ystore		
			
			lda #<screentxt		; setup screen 				(LO byte)
			ldy #>screentxt		; 							(HI byte)	
			sta zp1				; set up zeropage 1 with screen text location (LO byte)
			sty zp1+1			;							(HI byte)
			
			lda #<screenloc		; setup scrren location		(LO byte)
			ldy #>screenloc		; 							(HI byte)
			sta zp2				; set up zeropage 2 with screen location ($0400) to move to (LO byte)
			sty zp2+1			; 							(HI byte)
			
			lda #ssizel			; set size of block to move	(LO byte) 
			ldy #ssizeh			; 							(HI byte)
			sta sizel			; store it 					(LO byte)
			sty sizeh			;							(HI byte)
			
			jsr movedown		; print screen (copy from location to location)
			
			lda #<portloc		; set portloc to zp3		(LO byte)
			ldy #>portloc		; 							(HI byte)
			sta zp3				; store it					(LO byte)
			sty zp3+1			; 							(HI byte)
			
			lda isr				; set up interupt backup	(LO byte)
			ldy isr+1			; to recover on exit		(HI byte)
			sta isrbackup		; store it					(LO byte)
			sty isrbackup+1		; 							(HI byte)
			
			lda #<startscan		; ISR is at this location	(LO byte)
			ldy #>startscan		; 							(Hi byte)
			sta isr				; store it					(LO byte)
			sty isr+1			;							(HI byte)
			
			lda #pboutput		; set ports to output
			sta portout			
			
			lda #pboff			; turn all ports off
			sta portloc
			
			lda #<pbloc			; port locations on screen	(LO byte)
			ldy #>pbloc			; add "3" for each port		(HI byte)			
			sta zp1
			sty zp1+1
			
			cli					; restore interupt operation
			
			rts
	
	
	;
	;	MOVE BLOCK MEMORY
	;


movedown	ldy #0
			ldx sizeh
			beq md2
md1  		lda (zp1),y 		; move a page at a time
			sta (zp2),y
			iny
			
			bne md1
			inc zp1+1			; 255 byte reached, increment hi byte
			inc zp2+1
			dex
			bne md1				; not finished
md2      	ldx sizel
			beq md4
md3      	lda (zp1),y			; move the remaining bytes
			sta (zp2),y
			iny
			dex
			bne md3
md4      	rts
			

exit		lda #pboff			; load all ports off (0)
			sta portloc			; store it in port	; update port status
			
			ldx xstore			; restore x
			ldy ystore			; restore y
			clc					
			jsr plot			; cursor position
			
			lda #textcolor
			jsr clearscreen		; clear the screen before exit
			
			sei					; stop interupt operations
			
			lda isrbackup		; load back previous	(LO byte)
			ldy isrbackup+1		; ISR location			(HI byte)
			sta isr
			sty isr+1
			
			lda charbackup		; restore char set to previous
			sta setchar	
			
			cli					; start interupt operations

			rts

		;	
		;	MAIN LOOP (ISR takes care of most functions)
		;
				
loop		jsr getin			; get a character from keyboard
						
			beq loop
			cmp #$0d			; <return> key pressed
			beq exit			
			cmp #"0"			; "0" key pressed
			beq zero
			cmp #"1"			; "1" key pressed
			beq one
			cmp #"2"			; "2" key pressed
			beq two
			cmp #"3"			; "3" key pressed
			beq three
			cmp #"4"			; "4" key pressed
			beq four			
			cmp #"5"			; "5" key pressed
			beq five
			cmp #"6"			; "6" key pressed
			beq six
			cmp #"7"			; "7" key pressed
			beq seven
			cmp #"+"			; "*" key pressed
			beq allon
			cmp #"-"			; "-" key pressed
			beq alloff
			
			bne loop			; everything else, back to loop
			;beq loop
			rts
	;	
	;	MENU COMMANDS (removed JSR's for speed)
	;

zero		lda #pb0			; switch the port on/off							
			jmp switchport
			
one			lda #pb1			; switch the port on/off							
			jmp switchport
			
two			lda #pb2			; switch the port on/off			
			jmp switchport
			
three		lda #pb3			; switch the port on/off			
			jmp switchport
			
four		lda #pb4			; switch the port on/off			
			jmp switchport
			
five		lda #pb5			; switch the port on/off			
			jmp switchport
			
six			lda #pb6			; switch the port on/off			
			jmp switchport

seven		lda #pb7			; switch the port on/off			
			jmp switchport
			
allon		lda #pball			; load all on
						
			sta portloc			; store it in port loc	; update port status
			
			lda #on				; set flag to indicate a change
			sta flag			; store it
			
			jmp loop			; jump back to loop
			
alloff		lda #pboff			; load all off
			sta portloc			; store it in port loc ; update port status
			
			lda #on				; set flag to indicate a change
			sta flag			; store it
			
			jmp loop			; jump back to loop
		
switchport	eor portloc
			sta portloc
			
			lda #on				; set flag to indicate a change
			sta flag
			
			jmp loop			; jump back to loop
			

clearscreen	jsr chrout
			lda #$93
			jsr chrout
			rts
	
		;
		;	SCAN PORTS FOR 'ON' STATUS
		;	
			
startscan	lda flag			; look for flag, no need to change if nothing changed
			cmp #off		
			bne +				; somehthing changed continue on
			
			;rti
			jmp (isrbackup)
			;jmp returnint		; no flag set go back
			
+			lda #off			; reset flag back to OFF
			sta flag			; store it	
			
			lda #pb0 			; load port 0	; check to see if it's already on
			bit portloc
			beq +				; it is, jump ahead
			
			ldy #0				; set loc to port 0	
				
			jsr rvsont			; set rvs indicator to 'on'
						
+			lda #pb1			; load port 1	; check to see if it's already on
			bit portloc
			beq +				; it is, jump ahead
			
			ldy #3				; set loc to port 1	
			
			jsr rvsont			; set rvs indicator to 'on'
			
+			lda #pb2			; load port 2	; check to see if it's already on
			bit portloc
			beq +				; it is, jump ahead
			
			ldy #6				; set loc to port 2
				
			jsr rvsont			; set rvs indicator to 'on'
			
+			lda #pb3			; load port 3	; check to see if it's already on
			bit portloc
			beq +				; it is, jump ahead
			
			ldy #9				; set loc to port 3		
			
			jsr rvsont			; set rvs indicator to 'on'

+			lda #pb4			; load port 4	; check to see if it's already on
			bit portloc
			beq +				; it is, jump ahead
			
			ldy #12				; set loc to port 4
			
			jsr rvsont			; set rvs indicator to 'on'
			
+			lda #pb5			; load port 5	; check to see if it's already on
			bit portloc
			beq +				; it is, jump ahead
				
			ldy #15				; set loc to port 5
			
			jsr rvsont			; set rvs indicator to 'on'
			
+			lda #pb6			; load port 6	; check to see if it's already on
			bit portloc
			beq +				; it is, jump ahead
			
			ldy #18				; set loc to port 6
						
			jsr rvsont			; set rvs indicator to 'on'
			
+			lda #pb7			; load port 7	; check to see if it's already on
			bit portloc
			beq +				; it is, jump ahead
			
			ldy #21				; set loc to port 7
			
			jsr rvsont			; set rvs indicator to 'on'	
		
			
		;	
		;	SCAN PORTS FOR 'OFF' STATUS
		;

+			lda #pb0			; port 0	; check to see if it's alread off
			bit portloc 
			bne +				; it is, jump ahead
			
			ldy #0
			
			jsr rvsofft			; set rvs indicator to 'off'
			
+			lda #pb1			; load port 1	; check to see if it's already off
			bit portloc
			bne +				; it is, jump ahead
			
			ldy #3				; set loc to port 1
			
			jsr rvsofft			; set rvs indicator to 'off'
			
+			lda #pb2			; load port 2	; check to see if it's already off
			bit portloc
			bne +				; it is, jump ahead
			
			ldy #6				; set loc to port 2			
			
			jsr rvsofft			; set rvs indicator to 'off'
			
+			lda #pb3			; load port 3	; check to see if it's already off
			bit portloc
			bne +				; it is, jump ahead
			
			ldy #9				; set loc to port 3
			
			jsr rvsofft			; set rvs indicator to 'off'

+			lda #pb4			; load port 4	; check to see if it's already off
			bit portloc
			bne +				; it is, jump ahead
			
			ldy #12				; set loc to port 4
			
			jsr rvsofft			; set rvs off indicator
			
+			lda #pb5			; load port 5	; check to see if it's already off
			bit portloc
			bne +				; set loc port 5	
			
			ldy #15				; set loc to port 5
			
			jsr rvsofft			; set rvs indicator to 'off'
			
+			lda #pb6			; load port 6	; check to see if it's already off
			bit portloc
			bne +				; it is, jump head
			
			ldy #18				; set loc to port 6
			
			jsr rvsofft			; set rvs off indicator
			
+			lda #pb7			; port 7	; check to see if it's already off
			bit portloc
			bne +				; it is, jump ahead
			
			ldy #21				; set loc to port 7
			
			jsr rvsofft			; set rvs indicator to 'off'
		
+			;rti
			;jmp returnint
			jmp (isrbackup)

rvsont		lda #rvson			; load the RVS ON character
			ora (zp1),y			; turn ON RVS for selected character
			sta (zp1),y			; store it
			
			rts
			
			
rvsofft		lda #rvsoff			; load the RVS OFF character
			and (zp1),y			; turn OFF RVS for selected character
			sta (zp1),y			; store it
	
			rts	
			
			
flag		.byte 0	
sizel		.byte 0
sizeh		.byte 0
portsw		.byte 0
xstore		.byte 0	
ystore		.byte 0
charbackup	.byte 0
isrbackup	.byte 0,0			


	;	
	;	SCREEN DATA
	;
			
screentxt	.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$10,$0F,$12,$14,$20,$20,$13,$14,$01,$14,$15,$13,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$55,$43,$49,$55,$43,$49,$55,$43,$49,$55,$43,$49,$55,$43,$49,$55,$43,$49,$55,$43,$49,$55,$43,$49,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$5D,$30,$5D,$5D,$31,$5D,$5D,$32,$5D,$5D,$33,$5D,$5D,$34,$5D,$5D,$35,$5D,$5D,$36,$5D,$5D,$37,$5D,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$4A,$43,$4B,$4A,$43,$4B,$4A,$43,$4B,$4A,$43,$4B,$4A,$43,$4B,$4A,$43,$4B,$4A,$43,$4B,$4A,$43,$4B,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$27,$2D,$27,$20,$0B,$05,$19,$20,$01,$0C,$0C,$20,$0F,$06,$06,$20,$27,$2b,$27,$20,$0B,$05,$19,$20,$01,$0C,$0C,$20,$0F,$0E,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$0B,$05,$19,$13,$20,$30,$2D,$37,$20,$03,$0F,$0E,$14,$12,$0F,$0C,$20,$10,$0F,$12,$14,$13,$20,$30,$2D,$37,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$10,$12,$05,$13,$13,$20,$27,$05,$0E,$14,$05,$12,$27,$20,$20,$14,$0F,$20,$11,$15,$09,$14,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7,$20,$20,$09,$0E,$04,$09,$03,$01,$14,$05,$13,$20,$10,$0F,$12,$14,$20,$20,$0F,$0E,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$30,$31,$32,$33,$34,$35,$36,$37,$20,$20,$09,$0E,$04,$09,$03,$01,$14,$05,$13,$20,$10,$0F,$12,$14,$20,$0F,$06,$06,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$0E,$0F,$14,$05,$20,$14,$08,$09,$13,$20,$09,$0E,$04,$09,$03,$01,$14,$05,$13,$20,$15,$13,$05,$12,$10,$0F,$12,$14,$20,$10,$0F,$17,$05,$12,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$09,$0E,$14,$05,$12,$06,$01,$03,$05,$20,$17,$09,$0C,$0C,$20,$20,$0C,$09,$0B,$05,$0C,$19,$20,$02,$05,$20,$0F,$10,$10,$0F,$13,$09,$14,$05,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			.byte	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$02,$19,$20,$20,$0A,$0F,$08,$0E,$20,$03,$01,$12,$0D,$0F,$0E,$19,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

charset
;.include "charset.asm"
.binary "1-writer.64c",2