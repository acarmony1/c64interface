;version 1.2 	5-25-2020
;acme --cpu 6510 -f cbm -o ./interface/interface64.prg interface/interface64.asm
;exomizer sfx $0810 ./interface/interface64.prg -o ./interface/interface64c.prg -Di_effect=2 -x "inc $d020"
;exomizer sfx $0810 ./interface/interface64.prg -o ./interface/interface64c.prg -Di_effect=2 -x "inc $fb lda $fb sta $d020 lda #0 sta $d020"
;exomizer sfx $0810 ./interface/interface64.prg -o ./interface/interface64c.prg -Di_effect=2 -x "inc $d020 inc $d021 lda #0 sta $d020 sta $d021"


clrscreen	= $e544
portout		= $dd03				; set ports to 255 (output) (#56579)
portloc		= $dd01				; 8-bit controller (0-7)	(#56577)
screenloc	= $0400				; beginning of screen location
borderloc	= $d020				; boreder color location
backgrloc	= $d021				; background color location
setchar		= $d018				; charater location
isr			= $0314				; interupt location
returnint	= $ea31				; rti
char		= $2000				; custom char loc
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
	
clearscreen	= $e544

chrout 		= $ffd2				;write byte to default output. (if not screen, must call open and chkout beforehands.)
								;input: a = byte to write.
								;output: –
								;used registers: –
								;real address: ($0326), $f1ca.
								
plot 		= $fff0				;save or restore cursor position.
								;input: carry: 0 = restore from input, 1 = save to output; x = cursor column (if carry = 0); y = cursor row (if carry = 0).
								;output: x = cursor column (if carry = 1); y = cursor row (if carry = 1).
								;used registers: x, y.
								;real address: $e50a.
								
getin 		= $ffe4				;read byte from default input. (if not keyboard, must call open and chkin beforehands.)
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
zp3			= $02				; zero page 3 location


rvson		= $80				; scrren code to turn ON reverse character
rvsoff		= $7f				; screen code to turn OFF reverse character


	;
	; BASIC STUB
	; 10 SYS (2064)
	; uncomment the following line to start as basic program

;	basic start
;
;*			= $0801
;			!by $0d, $08, $0a, $00, $9e
;			!tx "2064"			; Start address! Be *very* careful!
;			
;	basic stop

*			= $0810				; SYS2064


codeentry	jsr init			; setup and initialize
						
			jmp loop			; jump to loop
			

init		sei					; stop interupt operation (needed to setup new ISR)
						
			lda #textcolor		; set char color
			jsr chrout
			
			jsr clearscreen		; clear the screen
			
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
			
			lda #$18			; set to current char location
			sta setchar
			
			jsr movedown		; move the memory
			
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
			

		;	
		;	MAIN LOOP (ISR takes care of most functions)
		;
				
exit		lda #pboff			; load all ports off (0)
			sta portloc			; store it in port	; update port status
			
			ldx xstore			; restore x
			ldy ystore			; restore y
			clc					
			jsr plot			; cursor position
						
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
			
	
		;
		;	SCAN PORTS FOR 'ON' STATUS
		;	
			
startscan	lda flag			; look for flag, no need to change if nothing changed
			cmp #off		
			bne +				; somehthing changed continue on
	
			jmp returnint		; no flag set go back
			
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
		
+			jmp returnint


rvsont		lda #rvson			; load the RVS ON character
			ora (zp1),y			; turn ON RVS for selected character
			sta (zp1),y			; store it
			
			rts
			
			
rvsofft		lda #rvsoff			; load the RVS OFF character
			and (zp1),y			; turn OFF RVS for selected character
			sta (zp1),y			; store it
	
			rts	
			
			
flag		!by 0	
sizel		!by 0
sizeh		!by 0
portsw		!by 0
xstore		!by 0	
ystore		!by 0
charbackup	!by 0
isrbackup	!by 0,0			


	;	
	;	SCREEN DATA
	;
			
screentxt	!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$10,$0F,$12,$14,$20,$20,$13,$14,$01,$14,$15,$13,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$55,$43,$49,$55,$43,$49,$55,$43,$49,$55,$43,$49,$55,$43,$49,$55,$43,$49,$55,$43,$49,$55,$43,$49,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$5D,$30,$5D,$5D,$31,$5D,$5D,$32,$5D,$5D,$33,$5D,$5D,$34,$5D,$5D,$35,$5D,$5D,$36,$5D,$5D,$37,$5D,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$4A,$43,$4B,$4A,$43,$4B,$4A,$43,$4B,$4A,$43,$4B,$4A,$43,$4B,$4A,$43,$4B,$4A,$43,$4B,$4A,$43,$4B,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$27,$2D,$27,$20,$0B,$05,$19,$20,$01,$0C,$0C,$20,$0F,$06,$06,$20,$27,$2b,$27,$20,$0B,$05,$19,$20,$01,$0C,$0C,$20,$0F,$0E,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$0B,$05,$19,$13,$20,$30,$2D,$37,$20,$03,$0F,$0E,$14,$12,$0F,$0C,$20,$10,$0F,$12,$14,$13,$20,$30,$2D,$37,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$10,$12,$05,$13,$13,$20,$27,$05,$0E,$14,$05,$12,$27,$20,$20,$14,$0F,$20,$11,$15,$09,$14,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7,$20,$20,$09,$0E,$04,$09,$03,$01,$14,$05,$13,$20,$10,$0F,$12,$14,$20,$20,$0F,$0E,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$30,$31,$32,$33,$34,$35,$36,$37,$20,$20,$09,$0E,$04,$09,$03,$01,$14,$05,$13,$20,$10,$0F,$12,$14,$20,$0F,$06,$06,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$0E,$0F,$14,$05,$20,$14,$08,$09,$13,$20,$09,$0E,$04,$09,$03,$01,$14,$05,$13,$20,$15,$13,$05,$12,$10,$0F,$12,$14,$20,$10,$0F,$17,$05,$12,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$09,$0E,$14,$05,$12,$06,$01,$03,$05,$20,$17,$09,$0C,$0C,$20,$20,$0C,$09,$0B,$05,$0C,$19,$20,$02,$05,$20,$0F,$10,$10,$0F,$13,$09,$14,$05,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
			!by	$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$02,$19,$20,$20,$0A,$0F,$08,$0E,$20,$03,$01,$12,$0D,$0F,$0E,$19,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20


	;
	;	CUSTOM CHARACTER SET
	;
	
charset		!by	$18,$00,$3C,$66,$7E,$66,$00,$00,$00,$3C,$66,$7E,$66,$66,$00,$00,$00,$7C,$66,$7C,$66,$7C,$00,$00,$00,$3E,$60,$60,$60,$3E,$00,$00
			!by	$00,$7C,$66,$66,$66,$7C,$00,$00,$00,$7E,$60,$78,$60,$7E,$00,$00,$00,$7E,$60,$78,$60,$60,$00,$00,$00,$3E,$60,$66,$66,$3E,$00,$00
			!by	$00,$66,$66,$7E,$66,$66,$00,$00,$00,$3C,$18,$18,$18,$3C,$00,$00,$00,$06,$06,$06,$66,$3C,$00,$00,$00,$66,$6C,$78,$6C,$66,$00,$00
			!by	$00,$60,$60,$60,$60,$7E,$00,$00,$00,$63,$7F,$6B,$63,$63,$00,$00,$00,$63,$7B,$6F,$67,$63,$00,$00,$00,$3C,$66,$66,$66,$3C,$00,$00
			!by	$00,$7C,$66,$7C,$60,$60,$00,$00,$00,$3C,$66,$66,$6E,$3C,$06,$00,$00,$7C,$66,$7C,$66,$66,$00,$00,$00,$3E,$60,$3C,$06,$7C,$00,$00
			!by	$00,$7E,$18,$18,$18,$18,$00,$00,$00,$66,$66,$66,$66,$3C,$00,$00,$00,$66,$66,$66,$3C,$18,$00,$00,$00,$63,$63,$6B,$7F,$63,$00,$00
			!by	$00,$66,$3C,$18,$3C,$66,$00,$00,$00,$66,$66,$3C,$18,$18,$00,$00,$00,$7E,$06,$18,$60,$7E,$00,$00,$66,$00,$3C,$66,$7E,$66,$00,$00
			!by	$07,$03,$7D,$CC,$CC,$CC,$78,$00,$66,$00,$3C,$66,$66,$3C,$00,$00,$00,$00,$00,$00,$5A,$BD,$BD,$99,$00,$10,$10,$10,$FE,$10,$10,$10
			!by	$00,$00,$00,$00,$00,$00,$00,$00,$18,$18,$18,$18,$00,$00,$18,$00,$66,$66,$66,$00,$00,$00,$00,$00,$66,$66,$FF,$66,$FF,$66,$66,$00
			!by	$18,$3E,$60,$3C,$06,$7C,$18,$00,$62,$66,$0C,$18,$30,$66,$46,$00,$3C,$66,$3C,$38,$67,$66,$3F,$00,$06,$0C,$18,$00,$00,$00,$00,$00
			!by	$0C,$18,$30,$30,$30,$18,$0C,$00,$30,$18,$0C,$0C,$0C,$18,$30,$00,$00,$6C,$38,$FE,$38,$6C,$00,$00,$00,$18,$18,$7E,$18,$18,$00,$00
			!by	$00,$00,$00,$00,$00,$18,$18,$30,$00,$00,$00,$7E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$18,$18,$00,$00,$03,$06,$0C,$18,$30,$60,$00
			!by	$00,$3C,$6E,$66,$76,$3C,$00,$00,$00,$18,$78,$18,$18,$7E,$00,$00,$00,$3C,$66,$1C,$70,$7E,$00,$00,$00,$3C,$66,$0C,$66,$3C,$00,$00
			!by	$00,$66,$66,$7E,$06,$06,$00,$00,$00,$7E,$60,$7C,$0E,$7C,$00,$00,$00,$3E,$60,$7C,$66,$3C,$00,$00,$00,$7E,$06,$0C,$18,$30,$00,$00
			!by	$00,$3C,$66,$3C,$66,$3C,$00,$00,$00,$3C,$66,$3E,$06,$7C,$00,$00,$00,$00,$18,$00,$00,$18,$00,$00,$00,$00,$18,$00,$00,$18,$18,$30
			!by	$0E,$18,$30,$60,$30,$18,$0E,$00,$00,$00,$7E,$00,$7E,$00,$00,$00,$70,$18,$0C,$06,$0C,$18,$70,$00,$3C,$66,$06,$0C,$18,$00,$18,$00
			!by	$00,$00,$00,$FF,$FF,$00,$00,$00,$08,$1C,$3E,$7F,$7F,$1C,$3E,$00,$18,$18,$18,$18,$18,$18,$18,$18,$00,$00,$00,$FF,$FF,$00,$00,$00
			!by	$00,$00,$FF,$FF,$00,$00,$00,$00,$00,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$00,$00,$30,$30,$30,$30,$30,$30,$30,$30
			!by	$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$00,$00,$00,$E0,$F0,$38,$18,$18,$18,$18,$1C,$0F,$07,$00,$00,$00,$18,$18,$38,$F0,$E0,$00,$00,$00
			!by	$C0,$C0,$C0,$C0,$C0,$C0,$FF,$FF,$80,$C0,$60,$30,$18,$0C,$06,$03,$01,$03,$06,$0C,$18,$30,$60,$C0,$FF,$FF,$C0,$C0,$C0,$C0,$C0,$C0
			!by	$FF,$FF,$03,$03,$03,$03,$03,$03,$00,$3C,$7E,$7E,$7E,$7E,$3C,$00,$00,$00,$00,$00,$00,$FF,$FF,$00,$36,$7F,$7F,$7F,$3E,$1C,$08,$00
			!by	$60,$60,$60,$60,$60,$60,$60,$60,$00,$00,$00,$07,$0F,$1C,$18,$18,$C3,$E7,$7E,$3C,$3C,$7E,$E7,$C3,$00,$3C,$7E,$66,$66,$7E,$3C,$00
			!by	$18,$18,$66,$66,$18,$18,$3C,$00,$06,$06,$06,$06,$06,$06,$06,$06,$08,$1C,$3E,$7F,$3E,$1C,$08,$00,$18,$18,$18,$FF,$FF,$18,$18,$18
			!by	$C0,$C0,$30,$30,$C0,$C0,$30,$30,$18,$18,$18,$18,$18,$18,$18,$18,$00,$00,$03,$3E,$76,$36,$36,$00,$FF,$7F,$3F,$1F,$0F,$07,$03,$01
			!by	$00,$00,$00,$00,$03,$07,$07,$0F,$00,$00,$1E,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$C0,$F0,$F8,$F8,$FC,$0F,$0F,$0C,$08,$08,$0C,$07,$07
			!by	$FF,$FF,$7C,$38,$6C,$EE,$C7,$D7,$FC,$FC,$3C,$1C,$18,$38,$F8,$F0,$00,$03,$03,$01,$01,$00,$00,$00,$FF,$7F,$AA,$55,$FF,$7F,$00,$00
			!by	$00,$30,$B0,$30,$E0,$00,$00,$00,$00,$10,$10,$10,$FE,$10,$10,$10,$18,$0C,$00,$00,$FF,$C0,$60,$30,$FF,$FF,$00,$00,$18,$3C,$7E,$FF
			!by	$04,$0A,$12,$21,$41,$01,$00,$00,$00,$00,$9F,$00,$E7,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0C,$33,$C0,$00,$00,$00
			!by	$01,$03,$06,$0C,$18,$30,$60,$C0,$C0,$60,$30,$18,$0C,$06,$03,$01,$00,$7E,$FF,$7E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$18,$3C,$66
			!by	$00,$30,$78,$78,$30,$00,$00,$00,$00,$38,$7C,$7C,$7C,$38,$00,$00,$00,$18,$3C,$7E,$7E,$3C,$18,$00,$81,$C3,$E7,$FF,$FF,$E7,$C3,$81
			!by	$9E,$90,$90,$FE,$12,$12,$F2,$00,$F2,$12,$12,$FE,$90,$90,$9E,$00,$C6,$C6,$C6,$FE,$C6,$C6,$C6,$C6,$00,$FF,$FF,$18,$18,$FF,$FF,$00
			!by	$00,$00,$60,$F0,$F9,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$FF,$81,$81,$81,$81,$81,$81,$FF,$FF,$C3,$C3,$C3,$C3,$C3,$C3,$FF
			!by	$E7,$FF,$C3,$99,$81,$99,$FF,$FF,$FF,$C3,$99,$81,$99,$99,$FF,$FF,$FF,$83,$99,$83,$99,$83,$FF,$FF,$FF,$C1,$9F,$9F,$9F,$C1,$FF,$FF
			!by	$FF,$83,$99,$99,$99,$83,$FF,$FF,$FF,$81,$9F,$87,$9F,$81,$FF,$FF,$FF,$81,$9F,$87,$9F,$9F,$FF,$FF,$FF,$C1,$9F,$99,$99,$C1,$FF,$FF
			!by	$FF,$99,$99,$81,$99,$99,$FF,$FF,$FF,$C3,$E7,$E7,$E7,$C3,$FF,$FF,$FF,$F9,$F9,$F9,$99,$C3,$FF,$FF,$FF,$99,$93,$87,$93,$99,$FF,$FF
			!by	$FF,$9F,$9F,$9F,$9F,$81,$FF,$FF,$FF,$9C,$80,$94,$9C,$9C,$FF,$FF,$FF,$9C,$84,$90,$98,$9C,$FF,$FF,$FF,$C3,$99,$99,$99,$C3,$FF,$FF
			!by	$FF,$83,$99,$83,$9F,$9F,$FF,$FF,$FF,$C3,$99,$99,$91,$C3,$F9,$FF,$FF,$83,$99,$83,$99,$99,$FF,$FF,$FF,$C1,$9F,$C3,$F9,$83,$FF,$FF
			!by	$FF,$81,$E7,$E7,$E7,$E7,$FF,$FF,$FF,$99,$99,$99,$99,$C3,$FF,$FF,$FF,$99,$99,$99,$C3,$E7,$FF,$FF,$FF,$9C,$9C,$94,$80,$9C,$FF,$FF
			!by	$FF,$99,$C3,$E7,$C3,$99,$FF,$FF,$FF,$99,$99,$C3,$E7,$E7,$FF,$FF,$FF,$81,$F9,$E7,$9F,$81,$FF,$FF,$99,$FF,$C3,$99,$81,$99,$FF,$FF
			!by	$F8,$FC,$82,$33,$33,$33,$87,$FF,$99,$FF,$C3,$99,$99,$C3,$FF,$FF,$FF,$E7,$C3,$81,$E7,$E7,$E7,$E7,$FF,$EF,$CF,$80,$80,$CF,$EF,$FF
			!by	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$E7,$E7,$E7,$E7,$FF,$FF,$E7,$FF,$99,$99,$99,$FF,$FF,$FF,$FF,$FF,$99,$99,$00,$99,$00,$99,$99,$FF
			!by	$E7,$C1,$9F,$C3,$F9,$83,$E7,$FF,$9D,$99,$F3,$E7,$CF,$99,$B9,$FF,$C3,$99,$C3,$C7,$98,$99,$C0,$FF,$F9,$F3,$E7,$FF,$FF,$FF,$FF,$FF
			!by	$F3,$E7,$CF,$CF,$CF,$E7,$F3,$FF,$CF,$E7,$F3,$F3,$F3,$E7,$CF,$FF,$FF,$93,$C7,$01,$C7,$93,$FF,$FF,$FF,$E7,$E7,$81,$E7,$E7,$FF,$FF
			!by	$FF,$FF,$FF,$FF,$FF,$E7,$E7,$CF,$FF,$FF,$FF,$81,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$E7,$E7,$FF,$FF,$FC,$F9,$F3,$E7,$CF,$9F,$FF
			!by	$FF,$C3,$91,$99,$89,$C3,$FF,$FF,$FF,$E7,$87,$E7,$E7,$81,$FF,$FF,$FF,$C3,$99,$E3,$8F,$81,$FF,$FF,$FF,$C3,$99,$F3,$99,$C3,$FF,$FF
			!by	$FF,$99,$99,$81,$F9,$F9,$FF,$FF,$FF,$81,$9F,$83,$F1,$83,$FF,$FF,$FF,$C1,$9F,$83,$99,$C3,$FF,$FF,$FF,$81,$F9,$F3,$E7,$CF,$FF,$FF
			!by	$FF,$C3,$99,$C3,$99,$C3,$FF,$FF,$FF,$C3,$99,$C1,$F9,$83,$FF,$FF,$FF,$FF,$E7,$FF,$FF,$E7,$FF,$FF,$FF,$FF,$E7,$FF,$FF,$E7,$E7,$CF
			!by	$F1,$E7,$CF,$9F,$CF,$E7,$F1,$FF,$FF,$FF,$81,$FF,$81,$FF,$FF,$FF,$8F,$E7,$F3,$F9,$F3,$E7,$8F,$FF,$C3,$99,$F9,$F3,$E7,$FF,$E7,$FF
			!by	$FF,$FF,$FF,$00,$00,$FF,$FF,$FF,$F7,$E3,$C1,$80,$80,$E3,$C1,$FF,$E7,$E7,$E7,$E7,$E7,$E7,$E7,$E7,$FF,$FF,$FF,$00,$00,$FF,$FF,$FF
			!by	$FF,$FF,$00,$00,$FF,$FF,$FF,$FF,$FF,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$FF,$FF,$CF,$CF,$CF,$CF,$CF,$CF,$CF,$CF
			!by	$F3,$F3,$F3,$F3,$F3,$F3,$F3,$F3,$FF,$FF,$FF,$1F,$0F,$C7,$E7,$E7,$E7,$E7,$E3,$F0,$F8,$FF,$FF,$FF,$E7,$E7,$C7,$0F,$1F,$FF,$FF,$FF
			!by	$3F,$3F,$3F,$3F,$3F,$3F,$00,$00,$7F,$3F,$9F,$CF,$E7,$F3,$F9,$FC,$FE,$FC,$F9,$F3,$E7,$CF,$9F,$3F,$00,$00,$3F,$3F,$3F,$3F,$3F,$3F
			!by	$00,$00,$FC,$FC,$FC,$FC,$FC,$FC,$FF,$C3,$81,$81,$81,$81,$C3,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$FF,$C9,$80,$80,$80,$C1,$E3,$F7,$FF
			!by	$9F,$9F,$9F,$9F,$9F,$9F,$9F,$9F,$FF,$FF,$FF,$F8,$F0,$E3,$E7,$E7,$3C,$18,$81,$C3,$C3,$81,$18,$3C,$FF,$C3,$81,$99,$99,$81,$C3,$FF
			!by	$E7,$E7,$99,$99,$E7,$E7,$C3,$FF,$F9,$F9,$F9,$F9,$F9,$F9,$F9,$F9,$F7,$E3,$C1,$80,$C1,$E3,$F7,$FF,$E7,$E7,$E7,$00,$00,$E7,$E7,$E7
			!by	$3F,$3F,$CF,$CF,$3F,$3F,$CF,$CF,$E7,$E7,$E7,$E7,$E7,$E7,$E7,$E7,$FF,$FF,$FC,$C1,$89,$C9,$C9,$FF,$00,$80,$C0,$E0,$F0,$F8,$FC,$FE
			!by	$00,$00,$00,$00,$03,$07,$07,$0F,$00,$00,$1E,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$C0,$F0,$F8,$F8,$FC,$0F,$0F,$0C,$08,$08,$0C,$07,$07
			!by	$FF,$FF,$7C,$38,$6C,$EE,$C7,$D7,$FC,$FC,$3C,$1C,$18,$38,$F8,$F0,$00,$03,$03,$01,$01,$00,$00,$00,$FF,$7F,$AA,$55,$FF,$7F,$00,$00
			!by	$00,$30,$B0,$30,$E0,$00,$00,$00,$00,$10,$10,$10,$FE,$10,$10,$10,$00,$08,$08,$D0,$38,$16,$20,$20,$00,$04,$88,$68,$10,$2C,$22,$40
			!by	$00,$04,$C4,$28,$10,$28,$46,$40,$00,$82,$44,$28,$10,$28,$44,$82,$00,$40,$46,$28,$10,$28,$C4,$04,$00,$40,$22,$2C,$10,$68,$88,$04
			!by	$00,$20,$20,$16,$38,$D0,$08,$08,$00,$00,$00,$00,$5A,$BD,$BD,$99,$00,$00,$00,$5A,$BD,$BD,$99,$42,$00,$00,$5A,$BD,$BD,$99,$42,$24
			!by	$00,$5A,$BD,$BD,$99,$42,$24,$24,$5A,$BD,$BD,$99,$42,$24,$24,$18,$00,$5A,$BD,$BD,$99,$42,$24,$18,$00,$00,$5A,$BD,$BD,$BD,$24,$18
			!by	$00,$00,$00,$5A,$BD,$BD,$BD,$18
