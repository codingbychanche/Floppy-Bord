; Floppy Bord
;
; Another Bird- Conversion
;
; BF 16.6.2014
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

ZP		EQU $e0

; TASTATUR

CONSOL	EQU 53279

; Eigene

;
; Start
;
	org $a800

;
; Init Vars's
;

	lda #50
	sta yp
	lda #150
	sta xp
	
;
; Display Titel
;

titelscr
	lda #<dltitel
	sta dlptr	
	lda #>dltitel
	sta dlptr+1
st
	lda consol
	and #1
	beq begin
	jmp st	
;
; Start Game
;

begin
	jsr screeninit				; Game- Screen init
	lda #<dlgame				; Game- Screen ein
	sta dlptr	
	lda #>dlgame
	sta dlptr+1
	
	jsr pminit					; PM Grafik ein
	jmp showbird				; Player anzeigen

;
; PM- Grafik initialisieren
;
	
pminit
	lda #pmadr/256	
	sta pmbase

	lda #200
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
; Player 0 anzeigen
;

bird
	.byte 0,0,124,66,183,231,210,146,12

xp	.byte 0
yp	.byte 0

showbird
	lda #<(pmadr+512)	
	sta zp
	lda #>(pmadr+512)
	sta zp+1
	
	ldy yp
	ldx #8
l1	lda  bird,x
	sta (zp),y
	iny
	dex
	bne l1
	
	lda #100
	sta hposp0
	
;
; Do Scroll
;

scroll
	ldy #240
s0
	ldx #15
s1
;	stx $d404				;Fine- Scroll
;	jsr wait
;	dex
;	bne s1
	
	clc						;Hard-Sscroll
	lda z0+1
	adc #1
	sta z0+1
	lda z0+2
	adc #0
	sta z0+2
	
	jsr wait
	dey
	bne s0
	
	
;
; Ende
;

e	jmp e

;
; Game- Screen initialisiern
;

zeile
	.byte 0
	
screeninit
	lda #<screen
	sta zp
	lda #>screen
	sta zp+1
	
	lda #15
	sta zeile
	
	ldx #240						;240 Bytes je Zeile
ll1
	txa
	sta (zp),y
	iny
	dex
	bne ll1
	
	rts
	
; 
; Wait
;
; Eine simple Warteschleife
;

xr	.byte 0
yr	.byte 0

wait
	stx xr						;Save register
	sty yr

	ldx #250
w0
	ldy #250
w1
	dey
	bne w1	
	dex
	bne w0
	
	lda xr						;Register zurück
	tax
	lda yr
	tay

	rts

;
; Game Screen/ Titel usw.
;

gr1		equ $06						; Gr. 1
gr12	equ $14						; Gr. 12 mit Horizontalem Scrolling

dltitel								;Titel Screen
	.byte $70,$70,$70
	.byte $40+gr1,a(titel)
:2	.byte gr1
	.byte $41,a(dltitel)
	
titel
	.byte "  bored of FLOPPY   "
	.byte "                    "
	.byte "   press <START>    "

dlgame								;Game Screen						
	.byte $70,$70,$70				;3 x leer.....

	; Jede Zeile hat 40 Bytes= 40 Zeichen
	; Das Spielfeld besteht aus 6 Bildschirmen
	; Damit ist jede Zeile 6 x 40 = 240 Bytes lang

bytes		equ 240

	.byte $40+gr1,a(scorelin)		 ;Punkte- Anzeige
z0	.byte $40+gr12,a(screen)		 ;Gamescreen, Zeile 0
z1	.byte $40+gr12,a(screen+1*bytes) ;Gamescreen, Zeile 1	
z2	.byte $40+gr12,a(screen+2*bytes) ;    ''      Zeile 2
z3	.byte $40+gr12,a(screen+3*bytes)
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

	; Ende, Sprung zum Angang der Display-List

	.byte $41,a(dlgame)
	
scorelin
	.byte "SCORE:               "
	
screen								; Game Screen Data						
	org *+8000
	



	
	

