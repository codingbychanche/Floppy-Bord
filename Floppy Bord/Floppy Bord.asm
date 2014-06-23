; Floppy Bord
;
; Another Bird- Conversion
;
; BF 22.6.2014
;

; ANTIC

DLPTR	EQU 560	
VDLIST	EQU $200	 
NMIEN	EQU $D40E
WSYNC	EQU $D40A
VCOUNT	EQU $D40B
RTCLK	EQU $14
SDMCTL	EQU 559

; FARBEN

COLPF0	EQU 708  
COLPF1	EQU 709 
COLPF2	EQU 710
COLPF3	EQU 711
COLBAK	EQU 712

COLPF0S	EQU $D016 
COLPF1S	EQU $D017
COLPF2S	EQU $D018
COLPF3S	EQU $D019
COLBAKS	EQU $D01A

; DISK I/O

DSKINV	EQU $E453  
DSKCMD	EQU $302	  
DSKAUX1	EQU $30A      
DSKDEV	EQU $300   
DSKUNIT	EQU $301	  
DSKBY	EQU $308   
DSKBUFF	EQU $304   
DSKTMOT	EQU $306 

; PM GRAFIK

PMADR	EQU $B800 ;PM SPEICHER
PMCNTL	EQU $D01D ;PM GRAFIK EIN/AUS

HPOSP0	EQU $D000 ;X POS PLAYER 1
HPOSP1	EQU $D001
HPOSP2	EQU $D002
HPOSP3	EQU $D003

SIZEP0	EQU $D008 ;GROESE PLAYER 1
SIZEP1	EQU $D009
SIZEP2	EQU $D00A
SIZEP3	EQU $D00B

COLPM0	EQU 704   ;FARBE PLAYER 1
COLPM1	EQU 705
COLPM2	EQU 706
COLPM3	EQU 707

PMBASE	EQU $D407 ;BASISADRESS

GRACTL	EQU $D01D ;PM CONTROLREG

; Zeropage

ZP		equ $e0
zp2		equ $e2
zp3		equ $e4
zp4		equ $e6
zp5		equ	$e8

; Parameter

maxlin	equ 20					;Maximale Anzahl Zeilen des Spielebildschirms
bytlin	equ 39					;Bytes je Zeile
screens	equ 6					;6 komplette Bildschirme sollen gescrollt werden

; TASTATUR

CONSOL	EQU 53279

;
; Start
;

	org $a000
	jmp titelscr
kill
	.byte 0
wait
	.byte 0
	
;
; Display Titel
;

titelscr

	jsr clpm					; Clear player 0 
	
	lda #<dltitel				; Show titel screen
	sta dlptr	
	lda #>dltitel
	sta dlptr+1
	
	lda #>chset					; Activate char set for graphics 0,1,2
	sta 756
st
	lda consol					; Now wait until the start key is pressed
	and #1
	beq begin
	jmp st	
;
; Start Game
;

begin
	jsr screeninit				; Game- screen init (draw playfield, init antic- program)
	jsr clscor					; Clear old score
	jsr showscor				; Show score

	lda #0						; Bird is alive!
	sta kill
	
	lda #30						; Bird will apear at line 30 on the screen
	sta ypos

	lda #<dli   				; Display- List- Interrupt on
	sta vdlist
	lda #>dli
	sta vdlist+1
	lda #$C0
	sta NMIEN
	
	lda #<dlgame				; Show playfield		
	sta dlptr						
	lda #>dlgame
	sta dlptr+1
	
	lda #<message				; Show message below score, just for fun
	sta msg+1
	lda #>message
	sta msg+2
	
	jsr pminit					; Init sprites
wt
	lda 644						; Wait for trigger
	bne wt
	
	lda #0						; Init frame- counter for bird animation
	sta frame
	ldy #<movebird				; Activate deffered VBI for player (bird) movement
	ldx #>movebird				
	lda #7
	jsr $e45c
	
	lda #1						; Delay for scrolling
	sta wait
	lda #3						; Color clocks for fine scroll
	sta clocks
	lda #240					;40 x 6 =240 Bytes = 6 Bildschimre 
	sta blocks   				;werden gescrollt, danach alles von Vorne :-)
	
	ldy #<scroll				;Scroll Routine im Immediate VBI
	ldx #>scroll
	lda #6
	jsr $e45c	
	
;
; Main Loop
;

main							
	lda #1						
	sta delta					;Show Score
	jsr score
	jsr showscor

	lda kill					;Bird still alive?
	cmp #1
	bne notdeath				;Yes

	;
	; Death Code
	;
	
	ldy #<vbi_imm_off			;Nein!
	ldx #>vbi_imm_off			;VBI für Scroll- und Vogelbewegung
	lda #6						;stoppen => ins leere zeigen lassen
	jsr $e45c
	
	ldy #<vbi_deff_off
	ldx #>vbi_deff_off
	lda #7
	jsr $e45c
gover	
	lda #<m1					; Inform the player that he has just died
	sta msg+1					; (in case he won't belive)
	lda #>m1
	sta msg+2
trig	
	lda 644						;Wait for trigger
	bne trig
	
	jmp titelscr				;Repeat!	
	
notdeath						
	jmp main					; Vogel lebt, also alles wiederholen
	
;
; Leere VBI- Routinen
;

vbi_imm_off
	jmp $e45f
vbi_deff_off
	jmp $e462
;
; Do Scroll => Immidiate VBI
;
; Es kann eine (nahezu) beliebige Anzahl Zeilen, horizontal, verschoben werden
; Da der Offset auf die Zeilenadressen in der Display List - das Y- Register - 
; nur 8- Bit breit ist, und die Zeilenadresse aus 2 Bytes besteht, sind maximal
; 256/2= 128 Zeilen möglich.... 
;

lines	
	.byte 0					;Ablage für Anzahl der Zeilen, die gescrollt werden
blocks
	.byte 0					;Ablage für die Anzahl der Bildschirme die gescrollt werden
clocks
	.byte 0					;Finescroll, Anzahl der Color Clocks um die geschoben werden soll

xr	.byte 0					;Sicherer Platz für Register
yr	.byte 0

scroll
	stx xr					;Register sichern
	sty yr

	dec wait				;wait gibt an wie oft scroll aufgerufen werden
	beq s11					;muss, damit einmal gescrollt werden wird
	ldx xr					;wait <>0=> zurück, nicht scrollen!
	ldy yr					;Register zurück schreiben
	jmp $e462				;VBI verlassen
s11
	lda #1					;wait zurücksetzen
	sta wait	
	lda clocks				;Feinscroll?
	beq hard				;Nein! => Hardscroll
	dec clocks				;Feinscroll	
	lda clocks				;clocks=3,2,1,0 => beim Herunterzählen werden die Zeichen 		
	sta $d404				;nach links verschoben, pixelweise
	ldx xr					;Register zurück schreiben
	ldy yr					
	jmp $e45f				;VBI verlassen
hard	
	lda #3					;Finescroll Register zurücksetzen					
	sta $d404
	sta clocks
	
	lda #19					;Anzahl der zu scrollenden Zeilen
	sta lines
	
	lda #<(z0+1)			;Adresse für den Inhalt der Zeile 0
	sta zp					;in die Zeropage schreiben
	lda #>(z0+1)
	sta zp+1
	
	ldy #0
s1	
	clc					
	lda (zp),y				;Hole Zeilenadresse (Low)
	adc #1					;Eins dazu
	sta (zp),y				
	iny
	lda (zp),y				;Hole Zeilenadresse (High)
	adc #0					;Carry- Flag dazu
	sta (zp),y
	iny						;Offset auf Adresse für nächste Zeile
	iny						;verbiegen
	dec lines				;Alle Zeilen durch?		
	bne s1					;Nein!
	dec blocks				;Alle Bildschirme durch?
	bne out					;nein!
	
	;
	; Scrollbereich zurücksetzen auf Anfang
	; Analog zur Routine in "Screeninit"
	;
	
	lda #240				;ja!
	sta blocks				;Anzahl der zu scrollenden Bildschirme zurücksetzen
	
	lda #<(adtab+1)			;Zeiger auf Adresstabelle		
	sta zp2					;welche die Startadressen des Spielbildschirms
	lda #>(adtab+1)			;enthält in zp- Register 2
	sta zp2+1
	
	lda #<(z0+1)			;Zeiger auf Bildadresse in Zeile 0		
	sta zp					;der Display- List des Spiele-
	lda #>(z0+1)			;bildschirms in zp- Register 1
	sta zp+1

	ldx #19					;20 Zeilen
	ldy #0
lll01
	lda (zp2),y				;low- Byte
	sta (zp),y
	iny
	lda (zp2),y				;High- Byte
	sta (zp),y
	iny
	iny
	dex
	bne lll01				;Alle Adressen? => Wenn ja, VBI verlassen	
out	
	ldx xr					;Register zurück schreiben
	ldy yr					
	jmp $e45f				;VBI verlassen

;
; PM- Grafik initialisieren
;
	
pminit
	lda #pmadr/256	
	sta pmbase

	lda #24
	sta colpm0

	lda #204
	sta colpm1

	lda #208
	sta colpm2

	lda #210
	sta colpm3

	lda #0
	sta sizep0

	lda #46
	sta sdmctl

	lda #2
	sta gractl

	lda #%00100011
	sta 623
	
	rts					

;
; Movebird => Deffered VBI
;

	;
	; Frames for our bird
	;

bird
	.byte $00,$00,$7c,$42,$b7,$e7,$d2,$92,$0c,$00
bird1
	.byte $00,$00,$7c,$42,$b7,$e7,$d2,$12,$0c,$00
bird2
	.byte $00,$00,$7c,$42,$b7,$e7,$12,$12,$0c,$00

framet
	.word bird,bird1,bird2,bird1
frame
	.byte 0
		
ypos	
	.byte 30				; Birds y- pos
	
xrr	.byte 0					; Memory for our registers
yrr	.byte 0

waitc
	.byte 1					; Wait- counter. 

movebird
	stx xrr					; Save registers
	sty yrr
	
	lda frame				; Get index of frame
	asl						; We are dealing with words, not bytes => multiply index by two
	tax						; to get low- and then high byte from adress table 'framet'
	lda framet,x			; Now get adress of frame data from table
	sta zp4					; store in zeropage, low byte
	inx						; move to high- byte in table
	lda framet,x			; Get high- byte and put in zero- page
	sta zp4+1
	
	ldx ypos				; Get y- pos
	ldy #9					; Init y- reg as index for frame data
l12	
	lda (zp4),y				; Display frame, get frame- data
	sta pmadr+512,x			; Write in memory area for player 0
	inx					
	dey						; Do so, until all 10 bytes (including the 0 at the
	bne l12					; top and bottom of frame to avoid garbage after changing vertikal pos od player
	
	lda #100				; Set horiz. position, always 100 :-)
	sta hposp0
	
	dec waitc				; Wait?
	bne eee					; <>0? => yes! => do nothing
	
	lda #2					; =0 => Reset 'waitc'	and go on
	sta waitc
	
	lda 644					; Get trigger
	bne down				; Not presssed? Bird loses altitute :-)

	lda frame				; Trigger pressed?=> Already 3 frames shown?
	cmp #3					
	bne skip				; No, next frame
	lda #0					; Yes, reset frame- counter
	sta frame
	jmp skip2				; Now, don's increase framecounter, skip it
skip	
	inc frame
skip2
	lda ypos				; Trigger pressed? If so, check
	cmp #30					; if max height reached
	beq eee					; in that case, do nothing
	dec ypos				; Below max heigth, increase y- pos => move bird higher
	jmp eee					; skip downward movement
	
down	
	inc ypos				; Trigger not pressed, move bird lower
	lda ypos
	cmp #100				; As soon as bird reaches bottom of playfield
	bne eee					; let him die => GAME OVER
	lda #1							
	sta kill
eee		
	ldx xrr					; Write x- and y- reg. back
	ldy yrr		
	
	jmp $e462				; Leave intermediate VBI

;
; Clear PM- Graphics
;

clpm
	lda #<(pmadr+512)					
	sta zp4
	lda #>(pmadr+512)
	sta zp4+1
	ldy #255
cl1
	lda #0
	sta (zp4),y
	dey
	bne cl1
	rts

;
; Game- Screen initialisiern
;
; Prinzip:
; Wir haben insgesammt 6 Spielebildschirme die endlos gescrollt werden und nach jedem
; Durchgang neu gezeichnet werden
;
; +---+---+---+---+---+---+  
; | 1 | 2 | 3 | 4 | 5 | 6 |   => Spielebildschirme
; +--+---+---+---+---+---+
;
; 1:Startbedingung: Bildschirm 1 und 6 sind leer
; 
; 2:Auf Spielbildschirm 2 bis 5 werden die Spielfelddaten
; gezeichnet
;
; 3:Es wird bis Bildschirm 6 gescrollt
;
; 4: Alle Bildschirme werden gelöscht, da nun der sowieso leere
; Bildschirm 6 angezeigt wird, merk das der Spieler nich :-)
;
; 5: Der Zeiger auf die Display-List wird auf Bildschirm 1 verbogen
;
; 7: Wir fahren mit 1 fort 
; 

zeile
	.byte 0
	
screeninit	

	;
	; Zeiger auf das Bild- Ram des Spielebildschirms, in der 
	; Display- List für den Spielebildschirm zurücksetzen.
	;

	lda #<(adtab+1)					;Zeiger auf Adresstabelle		
	sta zp2							;welche die Startadressen des Spielbildschirms
	lda #>(adtab+1)					;enthält in zp- Register 2
	sta zp2+1
	
	lda #<(z0+1)					;Zeiger auf Bildadresse in Zeile 0		
	sta zp							;der Display- List des Spiele-
	lda #>(z0+1)					;bildschirms in zp- Register 1
	sta zp+1

	ldx #19							;20 Zeilen
	ldy #0
lll0
	lda (zp2),y						;low- Byte
	sta (zp),y
	iny
	lda (zp2),y						;High- Byte
	sta (zp),y
	iny
	iny
	dex
	bne lll0						;Alle Adressen?

	;
	; Alle Bildschirme löschen!
	;
	
	lda #<screen					;Zeiger auf Bildspeicher
	sta zp							;In Zero- Page
	lda #>screen
	sta zp+1
	
	lda #maxlin						;Anzahl Zeilen
	sta zeile

ll0	
	ldx #(bytlin*screens)	    	;Bytes je Zeile
	ldy #0							;Offset Zeropage Zeiger
ll1
	lda #0							;Zeile füllen
	sta (zp),y
	iny
	dex
	bne ll1							;Zeile beendet?
	dey								;Ja!
	clc
	lda zp
	adc #(bytlin*screens)
	sta zp
	lda zp+1
	adc #0
	sta zp+1
	
	dec zeile
	bne ll0							;Alle Zeilen?						

	;
	; Zeichen in Bildschirm 2 bis 5 ablegen
	;

	ldx #240
	ldy #19
	lda #2+127
lp1
	jsr plot
	dex
	bne lp1
	
	
	ldy #10
zz31
	ldx #100
	lda #2
zz3
	jsr plot
	dex
	cpx #90
	bne zz3
	dey
	bne zz31
	
	ldy #10
zz311
	ldx #150
	lda #2
zz33
	jsr plot
	dex
	cpx #130
	bne zz33
	iny
	cpy #19
	bne zz311

	rts

;
; Plot Routine
;
; Setzt ein belibiges Zeichen in den Bildspeicher
;
; x-Reg	= Xpos
; y-Reg	= Ypos
; a		= Zeichen
;

zeichen	.byte 0
xr4		.byte 0
yr4		.byte 0

plot	
	stx xr4							;Register sichern
	sty yr4
	
	sta zeichen						;Zeichen zwischenspeichern
	lda #<screen					;Zeiger auf Bildspeicher
	sta zp							;In Zero- Page
	lda #>screen
	sta zp+1
	
	cpy #0							;Y=0, trivial, Zeichen an Pos. x ausgeben
	beq set		
p1	
	clc								;Y!=0, Zeilenadresse berechnen
	lda zp
	adc #bytes
	sta zp
	lda zp+1
	adc #0
	sta zp+1
	dey
	bne p1
set
	txa
	tay
	lda zeichen
	sta (zp),y
	
	ldx xr4							;Register zurück
	ldy yr4
	
	rts	
	
;
; DLI
;

dli
	pha								;Register retten
	txa
	pha
	tya
	pha
	
	lda vcount
	asl
	cmp #38
	bcs dli1						;Aktuelle Zeile > 38?
	
	lda #>chset						;Nein!=> Wir sind also noch im Anzeigefeld 
	sta $d409						;für die Punkte
	lda #13							;Farbe für Großbuchstaben Gr.1/2						
	sta colpf0s						
	lda #13							;Farbe für Kleinbuchstaben Gr.1/2. Farbe für Buchstaben Gr.0
	sta colpf1s
	lda #116
	sta colpf2s
	lda #155
	sta colpf3s
	lda #116						;Dunkelblau der Hintergrund
	sta wsync
	sta colbaks

dli1								
	lda vcount						;Aktuelle Zeile < 38?
	asl								;Wir sind also noch im Anzeigefeld für
	cmp #38							;die Punkte
	bcc dlout						;=> nix tun
												
	lda #>chset12					;Nein: Wir sind im Spielfeldbereich	
	sta $d409						;=>Spielfeldbereich einfärben.
	lda #103						; Für Bit Kombi: 01
	sta colpf0s										
	lda #120						; Für Bit Kombi: 10
	sta colpf1s
	lda #196						; Für Bit Kombi: 11
	sta colpf2s
	lda #255						;Für die inversen Zeichen=Farbe 5
	sta colpf3s
	lda #123						;Helblau der Hintergrund :)
	sta wsync
	sta colbaks
dlout
	pla								;Register zurückschreiben
	tay
	pla
	tax
	pla
	
	rti
;
; Aktuellen Punktestand ausgeben
; 
; Aufruf: 
; delta= Anzahl der Punkte, um die der Zähler erhöt werden soll
;

points
	.byte "0000000"
delta
	.byte 0
	
xr5
	.byte 0
yr5
	.byte 0
	
score	
	stx xr5
	sty yr5

	jsr sl0
	dec delta
	bne score
	
	ldx xr5
	ldy yr5
	
	rts

sl0
	ldx #6

sl1
	lda points,x
	cmp #"9"       					;Ziffer= 9?
	bne sl2							;Nein!

	lda #"0"     					;0!
	sta points,X
	dex
	bne sl1

	rts

sl2
	clc
	adc #1
	sta points,X
	
	rts
	
;
; Punkte ausgeben
;

showscor
	ldy #7
	ldx #0
	lda #<scorelin
	sta zp3
	lda #>scorelin
	sta zp3+1
ss
	lda points,x
	sta (zp3),y
	inx
	iny
	cpx #7
	bne ss
	
	rts

;
; CLSCORE
; Punktestand löschen
;

clscor
	ldx #0
cl	
	lda #16							;=0
	sta points,X
	inx
	cpx #6
	bne cl
	rts

;
; Game Screen/ Titel usw.
;

gr0		equ $02					; Gr. 0
gr1		equ $06					; Gr. 1
gr12	equ $14					; Gr. 12 mit Horizontalem Scrolling

	;
	; Display- List und Bilddaten für den Titelbildschirm
	;

dltitel							;Titel Screen
	.byte $70,$70,$70
	.byte $40+gr1,a(titel)
:2	.byte gr1
	.byte $41,a(dltitel)
	
titel
	.byte "  bored of FLOPPY   "
	.byte "                    "
	.byte "   press <START>    "
	
	;
	; Display- List für den Spielebildschirm
	;
	org $1000

dlgame							;Game Screen						
	.byte $70+128				;Leer

	; Jede Zeile hat 40 Bytes= 40 Zeichen
	; Das Spielfeld besteht aus 6 Bildschirmen
	; Damit ist jede Zeile 6 x 40 = 240 Bytes lang

bytes	equ 239							;Bytes je Zeile

	.byte $40+gr0,a(ln1)
	.byte $40+gr1,a(scorelin)		;Punkte- Anzeige
msg
	.byte $40+gr1,a(message)		; Message line, tell the player what's going on

	.byte $40+gr0,a(ln2)
	
	.byte $70+128
	
z0	.byte $40+gr12,a(screen)		 ;Gamescreen, Zeile 0
z1	.byte $40+gr12,a(screen+1*bytes) ;Gamescreen, Zeile 1	
z2	.byte $40+gr12,a(screen+2*bytes) ;    ''      Zeile 2
z3	.byte $40+gr12,a(screen+3*bytes) ;Und so fort.......
z4	.byte $40+gr12,a(screen+4*bytes)
z5	.byte $40+gr12,a(screen+5*bytes)
z6	.byte $40+gr12,a(screen+6*bytes)
z7	.byte $40+gr12,a(screen+7*bytes)
z8	.byte $40+gr12,a(screen+8*bytes)
z9	.byte $40+gr12,a(screen+9*bytes)
z10	.byte $40+gr12,a(screen+10*bytes)
z11	.byte $40+gr12,a(screen+11*bytes)
z12	.byte $40+gr12,a(screen+12*bytes)
z13	.byte $40+gr12,a(screen+13*bytes)
z14	.byte $40+gr12,a(screen+14*bytes)
z15	.byte $40+gr12,a(screen+15*bytes)
z16	.byte $40+gr12,a(screen+16*bytes)
z17	.byte $40+gr12,a(screen+17*bytes)
z18	.byte $40+gr12,a(screen+18*bytes)
z19	.byte $40+gr12,a(screen+19*bytes)

	; Ende, Sprung zum AnFang der Display-List

	.byte $41,a(dlgame)
	
scorelin
	.byte $02,"score             !"
message
	.byte $02,"FLY LITTLE BIRD.. !"
m1
	.byte $02,"    GAME OVER     !"
	
	; Rahmen für die Punkteanzeige
	
ln1	.byte $51,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52
	.byte $52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$45
	
ln2 .byte $5a,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52
	.byte $52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$43
	
	;
	; Adress- Tabelle Spielebildschirm, Scrollzeilen
	; Startzustand.
	;
	; Das muss hier gesichert werden, weil, die Zeiger auf den
	; Bildspeicher in den zu scrollenden Zeilen verändert werden
	; damit es scrollt :-)
	;

dummy	equ 0							;Platzhalter
	
adtab
	.byte dummy,a(screen)				; Zeile 1
	.byte dummy,a(screen+1*bytes)
	.byte dummy,a(screen+2*bytes)
	.byte dummy,a(screen+3*bytes)
	.byte dummy,a(screen+4*bytes)
	.byte dummy,a(screen+5*bytes)
	.byte dummy,a(screen+6*bytes)
	.byte dummy,a(screen+7*bytes)
	.byte dummy,a(screen+8*bytes)
	.byte dummy,a(screen+9*bytes)
	.byte dummy,a(screen+10*bytes)
	.byte dummy,a(screen+11*bytes)
	.byte dummy,a(screen+12*bytes)
	.byte dummy,a(screen+13*bytes)
	.byte dummy,a(screen+14*bytes)
	.byte dummy,a(screen+15*bytes)
	.byte dummy,a(screen+16*bytes)
	.byte dummy,a(screen+17*bytes)
	.byte dummy,a(screen+18*bytes)
	.byte dummy,a(screen+19*bytes)		;Zeile 20
	
;
; Zeichensatz Daten
;
; Text, Gr. 0
;

	 org $2000
chset
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $02,$02,$02,$02,$02,$02,$02,$02
	.byte $40,$40,$40,$40,$40,$40,$40,$40,$00,$66,$FF,$FF,$66,$FF,$66,$00
	.byte $18,$7E,$7E,$60,$7E,$06,$7E,$18,$00,$66,$6C,$18,$30,$66,$46,$00
	.byte $3E,$36,$1E,$78,$6F,$7E,$7B,$00,$00,$18,$18,$18,$00,$00,$00,$00
	.byte $00,$3E,$3E,$38,$38,$38,$3E,$00,$00,$7C,$7C,$1C,$1C,$1C,$7C,$00
	.byte $00,$66,$3C,$FF,$3C,$66,$00,$00,$00,$18,$18,$7E,$7E,$18,$18,$00
	.byte $00,$00,$00,$00,$00,$18,$18,$38,$00,$00,$00,$7E,$7E,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$38,$38,$00,$00,$06,$0E,$1C,$38,$70,$60,$00
	.byte $00,$7E,$7E,$66,$66,$66,$7E,$00,$00,$38,$38,$18,$18,$7E,$7E,$00
	.byte $00,$7E,$76,$0C,$38,$70,$7E,$00,$00,$7E,$7E,$0C,$1C,$66,$3C,$00
	.byte $00,$1C,$3C,$7C,$6E,$7E,$0C,$00,$00,$7E,$7E,$60,$7E,$0E,$7C,$00
	.byte $00,$7E,$7E,$60,$7E,$66,$7E,$00,$00,$7E,$7E,$0C,$18,$30,$30,$00
	.byte $00,$7E,$7E,$66,$3C,$66,$7E,$00,$00,$7E,$7E,$66,$7E,$06,$7E,$00
	.byte $00,$00,$38,$38,$00,$38,$38,$00,$00,$00,$18,$18,$00,$18,$38,$38
	.byte $06,$0C,$18,$30,$18,$0C,$06,$00,$00,$00,$7E,$00,$00,$7E,$00,$00
	.byte $60,$30,$18,$0C,$18,$30,$60,$00,$00,$7E,$7E,$06,$1E,$00,$1C,$1C
	.byte $00,$7E,$7A,$6E,$6E,$60,$7E,$00,$00,$7E,$7E,$66,$66,$7E,$66,$00
	.byte $00,$7E,$7E,$66,$7C,$66,$7E,$00,$00,$7E,$7E,$66,$60,$66,$7E,$00
	.byte $00,$7C,$7E,$66,$66,$6C,$78,$00,$00,$7E,$7E,$60,$7E,$60,$7E,$00
	.byte $00,$7E,$7E,$60,$78,$60,$60,$00,$00,$7E,$7E,$60,$6E,$66,$7E,$00
	.byte $00,$66,$66,$66,$7E,$7E,$66,$00,$00,$7E,$7E,$18,$18,$18,$7E,$00
	.byte $00,$1E,$1E,$06,$66,$66,$7E,$00,$00,$66,$66,$6C,$78,$7C,$66,$00
	.byte $00,$60,$60,$60,$66,$7E,$7E,$00,$00,$63,$77,$77,$7F,$6B,$63,$00
	.byte $00,$76,$76,$7E,$6E,$66,$66,$00,$00,$7E,$7E,$66,$66,$66,$7E,$00
	.byte $00,$7E,$7E,$66,$66,$7E,$60,$00,$00,$7E,$7E,$66,$66,$6C,$76,$00
	.byte $00,$7E,$7E,$66,$64,$7C,$66,$00,$00,$7E,$7E,$60,$7E,$06,$7E,$00
	.byte $00,$7E,$7E,$18,$18,$18,$18,$00,$00,$66,$66,$66,$66,$7E,$7E,$00
	.byte $00,$66,$66,$66,$66,$3C,$3C,$00,$00,$63,$63,$6B,$7F,$7F,$77,$00
	.byte $00,$66,$66,$3C,$3C,$7E,$66,$00,$00,$66,$66,$7E,$7E,$18,$18,$00
	.byte $00,$7E,$7E,$0C,$38,$60,$7E,$00,$00,$1E,$1E,$18,$18,$18,$1E,$00
	.byte $00,$40,$60,$30,$18,$0C,$06,$00,$00,$78,$78,$18,$18,$18,$78,$00
	.byte $00,$08,$1C,$3E,$63,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$00
	.byte $00,$36,$7F,$7F,$3E,$1C,$08,$00,$18,$18,$18,$1F,$1F,$18,$18,$18
	.byte $03,$03,$03,$03,$03,$03,$03,$03,$18,$18,$18,$F8,$F8,$00,$00,$00
	.byte $18,$18,$18,$F8,$F8,$18,$18,$18,$00,$00,$00,$F8,$F8,$18,$18,$18
	.byte $03,$07,$0E,$1C,$38,$70,$E0,$C0,$C0,$E0,$70,$38,$1C,$0E,$07,$03
	.byte $01,$03,$07,$0F,$1F,$3F,$7F,$FF,$00,$00,$00,$00,$0F,$0F,$0F,$0F
	.byte $80,$C0,$E0,$F0,$F8,$FC,$FE,$FF,$0F,$0F,$0F,$0F,$00,$00,$00,$00
	.byte $F0,$F0,$F0,$F0,$00,$00,$00,$00,$FF,$FF,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$FF,$FF,$00,$00,$00,$00,$F0,$F0,$F0,$F0
	.byte $00,$1C,$1C,$7F,$77,$1C,$3E,$00,$00,$00,$00,$1F,$1F,$18,$18,$18
	.byte $00,$00,$00,$FF,$FF,$00,$00,$00,$18,$18,$18,$FF,$FF,$18,$18,$18
	.byte $00,$00,$3C,$7E,$7E,$7E,$3C,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF
	.byte $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$00,$00,$00,$FF,$FF,$18,$18,$18
	.byte $18,$18,$18,$FF,$FF,$00,$00,$00,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0
	.byte $18,$18,$18,$1F,$1F,$00,$00,$00,$7C,$7C,$60,$78,$60,$7E,$18,$1E
	.byte $00,$18,$3C,$7E,$18,$18,$18,$00,$00,$18,$18,$18,$7E,$3C,$18,$00
	.byte $00,$18,$30,$7E,$30,$18,$00,$00,$00,$18,$0C,$7E,$0C,$18,$00,$00
	.byte $00,$18,$3C,$7E,$7E,$3C,$18,$00,$00,$00,$7E,$06,$7E,$66,$7E,$00
	.byte $00,$60,$60,$7E,$7E,$66,$7E,$00,$00,$00,$7E,$7E,$60,$60,$7E,$00
	.byte $00,$06,$06,$7E,$7E,$66,$7E,$00,$00,$00,$7E,$66,$7E,$60,$7E,$00
	.byte $00,$1E,$1E,$18,$7E,$18,$18,$00,$00,$00,$7E,$7E,$66,$7E,$06,$7E
	.byte $00,$60,$60,$7E,$7E,$66,$66,$00,$38,$38,$00,$38,$38,$18,$3C,$00
	.byte $00,$06,$00,$06,$06,$06,$3E,$3E,$00,$60,$60,$66,$7C,$7C,$66,$00
	.byte $00,$38,$38,$18,$18,$3C,$3C,$00,$00,$00,$77,$77,$7F,$7F,$6B,$00
	.byte $00,$00,$7E,$7E,$66,$66,$66,$00,$00,$00,$7E,$7E,$66,$66,$7E,$00
	.byte $00,$00,$7E,$7E,$66,$7E,$60,$60,$00,$00,$7E,$7E,$66,$7E,$06,$06
	.byte $00,$00,$7E,$7E,$60,$60,$60,$00,$00,$00,$7E,$60,$7E,$06,$7E,$00
	.byte $00,$18,$7E,$7E,$18,$18,$1E,$00,$00,$00,$66,$66,$66,$7E,$7E,$00
	.byte $00,$00,$66,$66,$7E,$3C,$18,$00,$00,$00,$63,$6B,$7F,$7F,$77,$00
	.byte $00,$00,$66,$7E,$3C,$7E,$66,$00,$00,$00,$66,$66,$7E,$7E,$0E,$7E
	.byte $00,$00,$7E,$7E,$18,$30,$7E,$00,$00,$18,$3C,$7E,$7E,$18,$3C,$00
	.byte $18,$18,$18,$18,$18,$18,$18,$18,$00,$7E,$78,$7C,$6E,$67,$03,$00
	.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$54,$54,$54,$54,$54,$54,$00
	
;
; Zeichensatz, Spielfed Gr.12/13
;

	org $4000
chset12
:8	.byte 0
:1000 .byte $FF

;
; Bildspeicher Spielebildschirm
;
	
screen								; Game Screen Data						
	org *+8000



	
	

