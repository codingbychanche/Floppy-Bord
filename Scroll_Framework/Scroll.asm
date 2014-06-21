;
; Scrolling, horizontal
;

screen	equ 0
zp	equ $e0

	org $a800
	lda #<dlgame
	sta $230
	lda #>dlgame
	sta $231
	
	lda #1
	sta wait
	lda #3
	sta clocks
	
	ldy #<scroll
	ldx #>scroll
	lda #6
	jsr $e45c
	
	ldy #<bird
	ldx #>bird
	lda #7
	jsr $e45c
		
e	jmp e

;
; Deffered VBI
;
bird
	lda $14
	sta 710
	jmp $e462

;
; Imidiate VBI, Scrolling
;

clocks	
	.byte 0
lines
	.byte 0
wait
	.byte 0
	
scroll
	dec wait				;wait gibt an wie oft scroll aufgerufen werden
	beq s11					;muss, damit einmal gescrollt werden wird
	jmp $e462				;wait <>0=> zurück, nicht scrollen!
s11
	lda #1					;wait zurücksetzen
	sta wait	
	lda clocks				;Feinscroll?
	beq hard				;Nein! => Hardscroll
	dec clocks				;Feinscroll	
	lda clocks
	sta $d404						
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

	jmp $e45f

	
gr0		equ $02					; Gr. 0
gr1		equ $06					; Gr. 1
gr12	equ $14					; Gr. 12 mit Horizontalem Scrolling


dlgame							;Game Screen						
	.byte $70+128,$70			;Leer

	; Jede Zeile hat 40 Bytes= 40 Zeichen
	; Das Spielfeld besteht aus 6 Bildschirmen
	; Damit ist jede Zeile 6 x 40 = 240 Bytes lang

bytes		equ 239					;Bytes je Zeile

	.byte $40+gr0,a(ln1)
	.byte $40+gr1,a(scorelin)		 ;Punkte- Anzeige
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

	; Ende, Sprung zum Angang der Display-List

	.byte $41,a(dlgame)
	
scorelin
	.byte $02,"score             !"
	
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
; Bildspeicher Spielebildschirm
;
	

	