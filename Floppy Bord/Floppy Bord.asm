; Floppy Bord
; PAL- Version
;
; Another Bird- Conversion
;
; BF 
;
; V1.0.6 // 15.11.2014  => Changed adress space. Should now run with basic off, DOS+ DUP in RAM
;						=> No Flickering anymore! :-)
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

PMADR	EQU $B800 
PMCNTL	EQU $D01D 

HPOSP0	EQU $D000 
HPOSP1	EQU $D001
HPOSP2	EQU $D002
HPOSP3	EQU $D003

SIZEP0	EQU $D008 
SIZEP1	EQU $D009
SIZEP2	EQU $D00A
SIZEP3	EQU $D00B

COLPM0	EQU 704   
COLPM1	EQU 705
COLPM2	EQU 706
COLPM3	EQU 707

PMBASE	EQU $D407

GRACTL	EQU $D01D 

; Zeropage

ZP		equ $e0
zp2		equ $e2
zp3		equ $e4
zp4		equ $e6
zp5		equ	$e8
zp6		equ $ea
zp7		equ $ec
zp8		equ $ee

; Parameter

maxlin	equ $14		; # of rows of playfield
bytlin	equ $27		; byte/ row
screens	equ $06		; playfield consists of $06 screens

; Keyboard

CONSOL	EQU $d01f

;
; Start
;

	org $afc8
	
	jmp titelscr
kill
	.byte $00			; Killflag. $01 means, player (bird) has died.....
wait
	.byte $00			; Delay for srolling, the bigger, the slower
seqend
	.byte $00			; Flag. If this equals to $01, scroll sequence has ended...
level
	.byte $00			; Yes, it's a Level Counter
;
; Display Titel
;

titelscr
	jsr clpm		; Clear player 0 

	lda #0			; We start at level 0... sorry, actually it is level 1....
	sta level	

	lda #9	
	sta $d01e		; Clear all collision registers

	lda #<dltitel	; Show titel screen
	sta dlptr	
	lda #>dltitel
	sta dlptr+1

	lda #>chset		; Activate char set for graphics 0,1,2
	sta 756
	
	lda #14			; Set colors for titel, backrground of (Atari Basic) mode 1 and 2 
	sta colbak
	lda #200		; Color for lower case characters (Atari Basic) mode 1 and 2
	sta colpf1
	lda #14			; Background for (Atari Basic) mode 0
	sta colpf2
	lda #196		; Color for upper case characters (Atari Basic) mode 1 and 2
	sta colpf0
st
	lda consol		; Now wait until the start key is pressed
	and #1
	beq begin
	jmp st	
;
; Start Game
;

begin
	lda #0			; Screen off. It will be turned on in 'pminit'
	sta 559			; It is much nicer this way, so we won't notice what is happening when playfield is initialized...
	
	lda #0			; Background color= black, we do that here, so we
	sta colbak		; don't have to take care later within dli- routine (which would complicate things...)
	
	jsr screeninit	; Game- screen init (draw playfield, init antic- program)
	jsr clscor		; Clear old score
	jsr showscor	; Show score

	lda #0			; Bird is alive!
	sta kill	
	
	lda #0			; Flag: Scroll routine alters that to 1 if the entire playfield
	sta seqend		; is scrolled and has reached the right border....		
	lda #50			; Bird will apear at line 50 on the screen
	sta ypos

	lda #<dli   	; Display- List- Interrupt on
	sta vdlist
	lda #>dli
	sta vdlist+1
	lda #$C0
	sta NMIEN
	
	lda #<dlgame	; Show playfield		
	sta dlptr						
	lda #>dlgame
	sta dlptr+1
	
	lda #<message	; Show message below score, just for fun
	sta msg+1
	lda #>message
	sta msg+2
	
	lda #<poem		; Re- init zp8 with startin adress of
	sta zp8			; text containing our poem
	lda #>poem
	sta zp8+1
	
	jsr pminit		; Init sprites
wt
	lda 644			; Wait for trigger
	bne wt
	
	lda #0			; Init frame- counter for bird animation
	sta frame
	ldy #<movebird	; Activate deferred VBI for player (bird) movement
	ldx #>movebird				
	lda #7			;= Deferred VBI
	jsr $e45c
	
	lda #2			; Delay for scrolling
	sta wait
	lda #3			; Color clocks for fine scroll
	sta clocks
	lda #184		; Number of bytes to be scrolled
	sta blocks   	
	
	ldy #<scroll	;Scroll Routine: Immediate VBI
	ldx #>scroll
	lda #6			;= Immediate VBI
	jsr $e45c
	
;
; Main Loop
;

main		
	lda seqend		;Scroll sequence end? 
	beq scrollon	;No, scroll on=> VBI remains on!
	
	;
	; Init new playfield
	;
	
	jsr screeninit
	
	lda #0			; Message=> scroll playfield from start
	sta seqend	
	
	inc level		; Next level	
	lda level
	cmp #32			; If = 32, we start over
	bne goon
	lda #0
	sta level
goon	
	ldx #100
incsc
	lda #1			; Increase score!	
	sta delta		; Show Score
	jsr score
	jsr showscor	
	jsr wtt			; Wait!	
	dex				; Still alive!				
	bne incsc		; Increase score!

	lda zp8
	sta msg+1
	lda zp8+1
	sta msg+2
	
	clc				; Display next line of poem
	lda zp8
	adc #20
	sta zp8
	lda zp8+1
	adc #0
	sta zp8+1
scrollon					
	jmp main		; Bird is alive. Repeat main loop
	
;
; Empty VBI
;

vbi_imm_off
	jmp $e45f
vbi_deff_off
	jmp $e462

;
; Death Code
;

death
	ldy #<vbi_imm_off	; Bird is death!
	ldx #>vbi_imm_off	; Stop bird movement= Bird VBI
	lda #6				; VBI is now re- routet
	jsr $e45c
	
	ldy #<vbi_deff_off	; Same for deffered VBI
	ldx #>vbi_deff_off
	lda #7
	jsr $e45c

	lda #%10000111	; BOOOOM- sound :-)
	sta $d201		; AUDC1 => Bit 765=> Distortion Bit 012=> Volume
	lda #25			; Freq.
	sta $d200		; AUDF1
	
	ldx #16
deathsound			; Play sound
	txa
	and $d201
	sta $d201
	jsr wtt
	dex
	bne deathsound
	
	lda #0			; Silent!
	sta $d201	
gover	
	lda #<m1		; Inform the player that he has just died
	sta msg+1		; (in case he won't belive)
	lda #>m1
	sta msg+2
trig	
	lda	644			; Wait for trigger key
	bne trig		; and, if predded =>
	jmp titelscr	; show titel = > this game is over!	
	
;
; Do Scroll => Immidiate VBI
;
; Zero Page: zp
;			 zp2
;

lines	
	.byte 0			; Number of rows to be scrolled
blocks
	.byte 0			; Blocks= One TV- Screen in Gr. 12 mode= 40 Bytes x 20 rows
clocks
	.byte 0			; Finescroll, number of color clocks

xr	.byte 0			; Save place for our registers
yr	.byte 0
a	.byte 0

scroll
	stx xr			; Save registers
	sty yr
	sta a		
s11
	lda clocks		; Fine Scroll?
	beq hard		; No! Do hard scroll
	dec clocks		; Do fine scroll	
	lda clocks		; Colocks=3,2,1 counting down this values scrolls		
	sta $d404		; character to the left
	ldx xr			; Write regsiters back
	ldy yr	
	lda a				
	jmp $e45f		; Leave VBI
hard	
	lda #3			; Reset fine scroll register				
	sta $d404		; after chracter was moved to it's leftmost position 
	sta clocks		
	
	lda #20 		; We scroll 20 rows of our playfield
	sta lines
	
	lda #<(z0+1)	; Store adress of ram area where we have saved our adress for screen ram
	sta zp			; of line 0 of playfield (complicated? :-) Noooooooooo, just bad english :-(
	lda #>(z0+1)
	sta zp+1
	ldy #0
s1					
	lda (zp),y		; Get adress of row (Low)
	clc
	adc #1			; add => move row to the left => hardscroll
	sta (zp),y				
	iny
	lda (zp),y		; Get adress of row (High)
	adc #0			; Add contents of carry flag
	sta (zp),y
	iny				; Get adress of next row 
	iny				
	dec lines		; Do we have scrolled all rows of our playfield?		
	bne s1			; No!
	dec blocks		; All sceens of our playfield scrolled?
	bne out			; No!
	
	;
	; Re- init playfield.
	; => Scrolling beginns from the leftmost byte 
	;
	
	lda #184		; Yes!
	sta blocks		; reset # of screens/ playfield (184= 4 Screens, that is the # of bytes/ row of our playfield 

lll01
	lda #1			; Inform main that scroll sequence is done, start all over 
	sta seqend		; and increase score					
out	
	ldx xr
	ldy yr
	lda a
	
	jmp $e45f		; Leave VBI

;
; Init PM- Graphics
;
	
pminit
	lda #pmadr/256	
	sta pmbase

	lda #255
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

	lda #%00111101
	sta 623
	
	;
	; Draw borders for our
	; playfield: Player 1 serves as left, player 2 as right border
	;

	ldx #255
	lda #$ff
plo1
	sta pmadr+640,x
	sta pmadr+768,x
	dex
	bne plo1
	
	lda #40
	sta hposp1
	lda #208
	sta hposp2
	
	lda #0
	sta colpm1
	sta colpm2
	
	rts					

;
; Movebird => Deffered VBI
;
; Zeropage: zp4
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
	.byte 30		; Birds y- pos
	
xrr	.byte 0			; Save place for our registers
yrr	.byte 0

waitc
	.byte 1			; Wait- counter. 

movebird
	stx xrr			; Save registers
	sty yrr			
	
	lda $d004		; Check collision reg. for Player 0 (our bird)
	cmp #0
	beq next33		; with background of playfield
	jmp death	
				
next33	
	lda frame		; Get index of frame
	asl				; We are dealing with words, not bytes => multiply index by two
	tax				; to get low- and then high byte from adress table 'framet'
	lda framet,x	; Now get adress of frame data from table
	sta zp4			; store in zeropage, low byte
	inx				; move to high- byte in table
	lda framet,x	; Get high- byte and put in zero- page
	sta zp4+1
	
	ldx ypos		; Get y- pos
	ldy #9			; Init y- reg as index for frame data
l12	
	lda (zp4),y		; Display frame, get frame- data
	sta pmadr+512,x	; Write in memory area for player 0
	inx					
	dey				; Do so, until all 10 bytes (including the 0 at the
	bne l12			; top and bottom of frame to avoid garbage after changing vertical pos of player
	
	lda #100		; Set horiz. position, always 100 :-)
	sta hposp0
	
	dec waitc		; Wait?
	bne ee2			; <>0? => yes! => do nothing

	lda #2			; =0 => Reset 'waitc'	and go on
	sta waitc
	lda #0			; Sound off
	sta $d201
	
	lda 644			; Get trigger
	bne down		; Not presssed? Bird loses altitute :-)

	lda #3			; Init Pokey
	sta $d20f		; SKCTL
	lda #0	 
	sta $d208		; AUDCTL
	
	lda #%10000111
	sta $d201		; AUDC1 => Bit 765=> Distortion Bit 012=> Volume
	lda #15			; Freq.
	sta $d200		; AUDF1

	lda frame		; Trigger pressed! Already 3 frames shown?
	cmp #3					
	bne skip		; No, next frame

	lda #0			; Yes, reset frame- counter
	sta frame
	jmp skip2		; Now, dont increase framecounter, skip it
skip	
	inc frame
skip2
	lda ypos		; Trigger is pressed => check
	cmp #30			; if max height reached
	beq ee2			; in that case, do nothing
	dec ypos		; Below max heigth, decrease y- pos => move bird higher
	jmp ee2			; skip downward movement
down	
	lda ypos		; Too low?
	cmp #100
	bne e2			; No!
	jmp death		; Yes, dieeeeeeeeeee!
e2
	inc ypos		; Trigger not pressed, move bird lower
ee2
	ldx xrr			; Write x- and y- reg. back
	ldy yrr		
	
	jmp $e462		; Leave intermediate VBI

;
; Clear PM- graphics => all!
;

clpm
	ldy #255
cl1
	lda #0
	sta pmadr+512,y	; Clear player 0 (bird)
	sta pmadr+612,y	; Clear player 1 (right border)
	sta pmadr+768,y	; Clear player 2 (left border)
	dey
	bne cl1
	rts

;
; Game- Screen initialization
;
; Principle of operation:
;
; +---+---+---+---+---+ 
; | 1 | 2 | 3 | 4 | 5 |   => Playfield consists of 5 screens, each 48 bytes wide
; +--+---+---+---+---+
;
; 1:Initial conditions: Screens 1 and 5 are twins, means the contain exactly the same picture
; 
; 2:Screens 2 trough 4 contain random playfield data
;
; 3:Scroll playfield until right border of 5th screen is reached
;
; 4:Erase screens 2 trough 4. The player will not notice, because we are showing 
; screen 4 at that time
;
; 5:Change pointer at screen ram to adress of screen 1, in that case - again - the player won't notice anything
; strange happening on the screen, because screen 5 - the one we have just shown - looks exactly the same
; as screen 1, the screen which apears within 1/50 sec on CRT. To fast dor our brain :-)
;
; 7:Continue with step 1
; 
; Zeropage: zp5
;			zp6
;

; This tabel holds values for number of pillars and window height
; You can change the difficulty of the game..... 

	; Height of window in pilar, first value is for level 1, second value for....

wheight
	.byte	7,6,5,4,7,7,7,7,7,7,7,7
	.byte	7,6,5,7,7,7,7,7,7,7,7,7
	.byte   5,5,5,5,5,5,4,5,5,5,6,6
	.byte   5,5,5,5,5,5,5,5,5,5,5,5
	.byte   4,4,4,4,4,4,4,4,4,4,4,4
	
	; ......space between pillars. Same as above

dist
	.byte 	10,10,10,10,10,10,10,10,10
	.byte	8,8,8,8,8,8,8,8,8,8,8,8
	.byte 	8,8,8,8,8,8,8,8,8,8,8,8
	.byte   8,8,8,8,8,8,8,8,8,8,8,8
	.byte   10,10,10,10,10,10,10,10,10
	
space	
	.byte 8					; Space between pillars
window	
	.byte 5					; Big, or small window....

; Other important var's

zeile
	.byte 0
yp
	.byte 0
wide 
	.byte 0
length
	.byte 0
col
	.byte 0

; Adress table

dummy	equ 0						; a, well, a dummy.....
	
adtab
	.byte dummy,a(screen)			; Row 1
	.byte dummy,a(screen+1*bytes)	; Now we see why there is this suspicous dummy,
	.byte dummy,a(screen+2*bytes)	; It is there to let 'adtab' look exactly like 
	.byte dummy,a(screen+3*bytes)	; the structure of our antic program.
	.byte dummy,a(screen+4*bytes)	; In our antic programm dummy contains the 'lsm'
	.byte dummy,a(screen+5*bytes)	; instruction
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
	.byte dummy,a(screen+19*bytes)	; Row 20

screeninit	

	;
	; Reset Playfield= Begin scrolling at screen 1
	; To do this, we change the screen- ram adresses in our display list
	; line by line by replacing them with our in "adtab" saved adresses
	; 

	pha				; Save registers
	txa
	pha
	tya
	pha

	ldx #20			; 20 rows
	ldy #0
lll0
	lda adtab+1,y	; Get adress from table
	sta z0+1,y		; Put it into lms of antic program => low byte
	iny
	lda adtab+1,y	; Same for high byte
	sta z0+1,y
	iny
	iny
	dex
	bne lll0		; All rows done?

	;
	; Get number of pillars (obstacles) and height of window / pillar
	;
	
	ldx level
	lda wheight,x
	sta window
	lda dist,x
	sta space

	;
	; Clear screen 2,3,4
	; That way the player wont't notice that anythig happens on the screen.
	; Reason: Screen 5 is displayed, we don't change anything here
	; All other changes happen on screens which are not displayed
	;
	; Screen 2 starts at x = 60 that is because we change our screns while scrolling!
	; We have to hurry ahead => do changes before they apear on the visible part
	; of our screen.
	;

	lda #<screen	; Pointer on screen RAM
	sta zp6			; Store it in zero page
	lda #>screen
	sta zp6+1
	
	ldy #0
cll1
	ldx #192		; Here begins screen #5
cll2
	lda #0			; That's the char we plot= blank
	jsr plot		; Do plot
	dex				; Count backwards
	cpx #60			; until we get to x- pos 60, that is where screen 2 starts (should be 40? :-)
	bne cll2		; Next x- pos
	iny				; Next row
	cpy #21			; Until all 20 rows are done
	bne cll1	
	
	;
	; Draw Obstacles
	;
	; First thing you must remember: Screen 1 and 5 have to look the same
	; if not, it is no endless ing :-) 
	; Screen 1 starts at x=4 Screen 5 starts at x=188
	;
	
	ldx #3		; Draw top of screen (clouds)
	ldy #0
	lda #5
lli
	jsr plot
	inx
	cpx #240
	bne lli
	
	ldx #3
	ldy #1
	lda #2
lli1
	jsr plot	
	inx			
	cpx #240
	bne lli1
	
	ldx #60		; Draw bottom of screen.....
	ldy #18		; mother earth :-)
	lda #7
lli2
	jsr plot
	inx
	cpx #185
	bne lli2
	
	;
	; Draw first Screen
	; Screen 1 and 5 must look alike! So, actually we draw two
	; screens - exactly the same - at two different locations
	; on our playfield
	;

	ldx #24		; SCREEN 1

	lda #4		; Pilar is 4 bytes wide
	sta wide
	lda #8		; Start color. Color will vary across width
	sta col		; to simulate some depth
ppp1
	ldy #13		; Start at middle of screen
ppp2
	lda col		; Color
	jsr plot	; Plot it
	iny			; until we reach bottom of playfield
	cpy #20
	bne ppp2	
	inx			; Repeat  
	inc col     ; Next color
	dec wide	; Drawn pilar until full width reached
	bne ppp1
	
	ldy #12		; Draw flag pole
	ldx #26
flag1
	lda #12
	jsr plot
	dey
	cpy #8
	bne flag1
	
	lda #13+128	; Set flag atop
	jsr plot
	inx 
	lda #14+128
	jsr plot
	
	ldx #188+20	; SCREEN 5

	lda #4		; Pilar is 4 bytes wide
	sta wide
	lda #8		; Start color. Color will vary across width
	sta col		; to simulate some depth
ppp12
	ldy #13		; Start at middle of screen
ppp22
	lda col		; Color
	jsr plot	; Plot it
	iny			; until we reach bottom of playfield
	cpy #20
	bne ppp22	
	inx			; Repeat  
	inc col     ; Next color
	dec wide	; Drawn pilar until full width reached
	bne ppp12
	
	ldy #12		; Draw flag pole
	ldx #188+22
flag2
	lda #12
	jsr plot
	dey
	cpy #8
	bne flag2
	
	lda #13+128	; Set flag atop. +128 for inv. char= color 5
	jsr plot
	inx 
	lda #14+128
	jsr plot
	
	; Draw random screen
	; x pos 60 to 144 is the area of our playfield
	; we do not see, when scroling is reset and starts at screen 1 again.
	;
	; City Background....
	
	ldx #65
getlen
	lda 53770
	cmp #18		; a>18?
	bcs getlen	; yes!
	cmp #14 	; no. a<14?
	bcc getlen  ; no!
	
	tay
	lda #6
drw
	jsr plot
	iny			
	cpy #18
	bne drw		; Draw until we reach bottom of playfield
	inx
	cpx #184	; Draw until we reach end of playfield (right border)
	bne getlen	; Next  building

	;
	; Second Part
	; Draw obstacles (pilars)
	;

	ldx #75 	; Start at x=75 (off screen) => once again. We change pur playfield
				; while ing, so we have to hurry ahead ang change things before
				; they apear on screen.....
pp0
	lda #4		; Each pilar is 4 bytes wide
	sta wide
	lda #8		; Start color. Color will vary across width
	sta col		; to simulate some depth
pp1
	ldy #1		; Start at top of screen, well, almost...
pp2
	lda col		; Color
	jsr plot	; Plot it
	iny			; until we reach bottom of playfield
	cpy #20
	bne pp2	
	inx			; Repeat  
	inc col     ; Next color
	dec wide	; Drawn pilar until full width reached
	bne pp1
	txa			; Next pilar
	clc
	adc space	; Space between pilars
				; CHANGE THIS, TO DRAW MORE OR LESS PILARS
	tax
	cpx #184	; All pilars? That is the case when
	bcc pp0		; xpos> 184

	; 
	; Now we insert windows in our pilars
	;

	ldx #75		; once again, we start off screen
gety			
	lda 53770	; First= get random y- pos 
	cmp #10		; a>10?
	bcs gety	; yes!=> to big, get another
	cmp #2  	; no. a<0?
	bcc gety    ; no!
	sta yp   	; y pos is between upper and lower border, save it!	
pp01
	lda #4		; Window has same width as pilar
	sta wide
pp02
	lda window	; Window heigth
				; CHANGE THIS TO INCREASE/ DECREASE DIFFICULTY
	sta length
	lda yp
	tay
pp03	
	lda #0		; Color= blank space
	jsr plot	; Plot it
	iny			
	dec length	
	bne pp03
	inx			; Repeat until we have drawn 
	dec wide	; full window 
	bne pp02
	txa			; Next window
	clc
	adc space	; SPACE BETWEEN PILARS	
	tax
	cpx #188	; All pilars? That is the case when
	bcc gety	; xpos> 188?
	
	pla			; Get registers back
	tay
	pla
	tax
	pla
	
	rts			; Return

;
; Plot 
;
; Writes any character you want, at any position in screen ram
;
; x-reg	= xpos
; y-reg	= ypos
; a		= Char
;
; Zeropage: zp7

zeichen	.byte 0
xr4		.byte 0
yr4		.byte 0

plot	
	stx xr4		; Save registers
	sty yr4
	
	sta zeichen		; Save character
	lda #<screen	; Init pointer at screen ram
	sta zp7		
	lda #>screen
	sta zp7+1
	
	cpy #0		; ypos equals 0, that is easy, jump
	beq set		; to our set- routine, we don't have to calcualte line- adress
p1	
	lda zp7		; Get pointer at screen ram
	clc			; Now calculate adress of line from given y- pos
	adc #bytes	; The slow way :-)
	sta zp7				
	lda zp7+1
	adc #0
	sta zp7+1
	dey
	bne p1
set
	txa
	tay
	lda zeichen
	sta (zp7),y
	
	ldx xr4		; Get registers back
	ldy yr4
	
	rts	
	
;
; DLI
;

dli
	pha			;  Save registers
	txa
	pha
	tya
	pha
	
	lda vcount
	asl
	cmp #38
	bcs dli1	; Is electron beam at row > 38?

	;
	; Set Chset and colors for score display
	;
	
	lda #>chset	; No! Electron beam is at line # below 38
	sta $d409	; so we are still within our score display
	lda #5		; Change colors for capital charcters (basic-)mode 1 or 2 	
	sta wsync
	sta colpf0s					
	lda #20 	; Change colors for lower case characters in (basic)mode 1,2 or 0
	sta colpf1s
	lda #116
	sta colpf2s
	lda #155
	sta colpf3s
	lda #14		; Bright white for the background
	sta colbaks 	; End of screen area for score display

dli1								
	lda vcount	; Is electron beam at row # bigger than 38?
	asl			; No? So we are still in area of score display
	cmp #38		; 
	bcc dlout	; Do nothing!

	;
	; Set chset for playfield and playfield colors
	;
								
	lda #>chset12 ; Electron beam has crossed row 38 that means
	sta $d409	; we are in playfild area of our screen

	lda #200	; Color for bit combination: 10
	sta colpf1s									
	sta wsync
	
	lda #14		; Color for bit combination: 11
	sta colpf2s
	sta wsync
	
	lda #10		; Color for bit combination: 01	
	sta colpf0s									
	sta wsync

	lda #24		; Color for bit combination=color 5 bit combination 11 (reverse character)
	clc			; The only object on screen with that color is our flag that marks
	adc level	; the begining of the next level. We change that color every new
	sta colpf3s	; level to visualize that event :-)

	; Draw sky

	ldx #15		; Draw Background
	lda #127	; Bright blue
dd1
	sta wsync	; Wait until scanline is finished
	sta colbaks ; Init background color reg.
	ldy #75		; This determins the height of each color cycle
dd2
	dey
	bne dd2
	
	sec			; Blue get's darker
	sbc #1
	dex			; Until we reach the lower third
	bne dd1		; of our playfield

	; Draw ground

	ldx #15		; Draw ground
	lda #194	; Start with dark green
dc1
	sta wsync
	sta colbaks
	ldy #19
dc2
	dey			; Height of each color cycle
	bne dc2
	clc
	adc #1
	dex
	bne dc1
dlout
	pla			; Get registers back
	tay
	pla
	tax
	pla
	
	rti
;
; Show score
; 
; Call: 
; delta= Amount of points to be added to current score
;

points
	.byte "000000"
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
	ldx #5

sl1
	lda points,x	; Get figure
	cmp #"9"       	; Is it "9"?
	bne sl2			; no, increase it

	lda #"0"     	; Yes! set to zero
	sta points,X	; and get
	dex				; next
	bne sl1			; figure....

	rts

sl2
	clc
	adc #1
	sta points,X
	
	rts
	
;
; Show score
;

showscor
	stx xr5			; Save registers
	sty yr5
	ldy #7			; Number of digits
	ldx #0
ss
	lda points,x	; Output- loop
	sta scorelin,y
	inx
	iny
	cpx #7
	bne ss
	
	ldx xr5
	ldy yr5
	
	rts

;
; CLSCORE
; Clear score
;

clscor
	ldx #0
cl	
	lda #16			;=0
	sta points,X
	inx
	cpx #6
	bne cl
	rts
	
;
; Little Dirty Debugger
; Obsolete, game runs perfectly now :-)
; Use this to show any parameter you want't 
; It works fine as long it's not bigger than 16......
;

fig
	.byte "0123456789ABVDEF"
	
debug
	ldx level
	lda fig,x
	sta scorelin
	rts
	
;
; Wait
; Sometimes our code is to fast........
;

wtt
	pha				; Save registers
	txa
	pha
	tya
	pha

	ldx #30
ww0	
	ldy #195
ww1	
	dey
	bne ww1
	dex
	bne ww0
	
	pla				; Get registers back
	tay
	pla
	tax
	pla
	
	rts
	
;
; Antic program and contents of screen RAM for
; our titel screen
;

; Some useful equates ;-)

gr0		equ $02					; Gr. 0
gr1		equ $06					; Gr. 1
gr2		equ $07
gr12	equ $14					; Gr. 12, horiz. scrolling enabeled

dltitel							;Titel Screen
	.byte $70,$70,$70
	.byte $40+gr2,a(titel)
:3	.byte gr1
:5	.byte 112
	
:13	.byte gr0

	.byte $41,a(dltitel)
titel
	.byte "  BORED OF FLOPPY   "
	.byte "                    "
	.byte "   press <start>    "
	.byte "                    "
	
	.byte "           RetroZock 2014               "
	.byte "          www.retrozock.com             "
	.byte "   Source code available at:GitHub      "
	.byte "                                        "
	.byte "                                        "
	.byte "                                        "
	.byte "                                        "
	.byte "                                        "
	.byte "                                        "
	.byte "                                        "
	.byte "                                        "
	.byte "                                        "
	.byte "                   V 1.0.6// 15.11.2014 "	
;	
; Antic program for our playfield
;

bytes	equ 246							; Our playfield is 249 bytes wide

dlgame						 	
	.byte 112+128						; Start of Antic programm for our playfield			
	.byte 112
	.byte $40+gr1,a(scorelin)			; Gr.1 display. Tha's where we can see our score
	.byte 112							; and other important messages.....
msg
	.byte $40+gr1,a(message)			; Message line, tell the player what's going on

	.byte $70+128						; 8 empty lines, start display- list interrupt
	
z0	.byte $40+gr12,a(screen)			; Playfield row 0
z1	.byte $40+gr12,a(screen+1*bytes) 	
z2	.byte $40+gr12,a(screen+2*bytes) 
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
z16	.byte $40+gr12,a(screen+16*bytes) 
z17	.byte $40+gr12,a(screen+17*bytes)
z18	.byte $40+gr12,a(screen+18*bytes)
z19	.byte $40+gr12,a(screen+19*bytes)	; Row 20

	.byte $41,a(dlgame)				 	; End of display- list, start all over again....
	
scorelin								; Contents of screen ram for status display
	.byte "score               "
message
	.byte "FLY LITTLE BIRD..   "
m1
	.byte "    GAME OVER       "
	
;
; Charset data
;
; For: Text (Atari Basic-) text modes 0,1,2 
;

	 org $4400
chset
	;------------------
; Dump of:	ANTIK2.FNT

.byte	$00,$00,$00,$00,$00,$00,$00,$00,$38
.byte	$38,$38,$38,$38,$00,$38,$38,$66
.byte	$66,$66,$00,$00,$00,$00,$00,$00
.byte	$66,$ff,$66,$66,$ff,$66,$00,$18
.byte	$3e,$60,$3c,$06,$7c,$18,$00,$00
.byte	$66,$6c,$18,$30,$66,$46,$00,$1c
.byte	$36,$1c,$38,$6f,$66,$3b,$00,$00
.byte	$18,$18,$18,$00,$00,$00,$00,$06
.byte	$0e,$1c,$18,$18,$1c,$0e,$06,$60
.byte	$70,$38,$18,$18,$38,$70,$60,$00
.byte	$66,$3c,$ff,$3c,$66,$00,$00,$00
.byte	$18,$18,$7e,$18,$18,$00,$00,$00
.byte	$00,$00,$00,$00,$18,$18,$30,$00
.byte	$00,$00,$7e,$00,$00,$00,$00,$00
.byte	$00,$00,$00,$00,$18,$18,$00,$00
.byte	$06,$0c,$18,$30,$60,$40,$00,$7c
.byte	$ce,$c6,$c6,$c6,$e6,$7c,$00,$38
.byte	$38,$18,$18,$18,$18,$18,$00,$7c
.byte	$e6,$0c,$18,$30,$60,$fe,$00,$7e
.byte	$0c,$18,$0c,$06,$66,$3c,$00,$0c
.byte	$1c,$3c,$6c,$cc,$fe,$0c,$00,$7e
.byte	$60,$7c,$06,$06,$66,$3c,$00,$7c
.byte	$c6,$c0,$fc,$ce,$e6,$7c,$00,$7e
.byte	$06,$0c,$18,$30,$30,$30,$00,$7c
.byte	$ce,$e6,$7c,$ce,$e6,$7c,$00,$7c
.byte	$ce,$c6,$e6,$7e,$0c,$18,$30,$00
.byte	$38,$38,$00,$00,$38,$38,$00,$00
.byte	$00,$18,$18,$00,$18,$18,$30,$06
.byte	$0c,$18,$30,$18,$0c,$06,$00,$00
.byte	$00,$7e,$00,$00,$7e,$00,$00,$60
.byte	$30,$18,$0c,$18,$30,$60,$00,$3c
.byte	$66,$66,$0c,$18,$00,$18,$00,$6a
.byte	$c9,$00,$7e,$00,$93,$56,$00,$7a
.byte	$9c,$34,$36,$3e,$66,$66,$c3,$ee
.byte	$73,$6b,$6b,$7e,$6b,$6b,$de,$3d
.byte	$66,$d4,$d0,$d0,$d0,$e6,$7c,$ee
.byte	$73,$6b,$6b,$6b,$6b,$63,$de,$fe
.byte	$66,$60,$78,$60,$63,$66,$7c,$fe
.byte	$66,$60,$78,$60,$60,$68,$70,$3d
.byte	$66,$d6,$d0,$de,$d6,$66,$3c,$c7
.byte	$c6,$c6,$ce,$fe,$e6,$c6,$ce,$34
.byte	$18,$18,$18,$18,$18,$1c,$2c,$7f
.byte	$16,$16,$56,$06,$de,$7c,$78,$c6
.byte	$6d,$6c,$78,$78,$6c,$6c,$c6,$e0
.byte	$60,$60,$60,$61,$66,$7e,$78,$c6
.byte	$ee,$fe,$d6,$c6,$c6,$c6,$c6,$c6
.byte	$c6,$e6,$f6,$de,$ce,$c6,$c6,$7d
.byte	$ce,$d6,$d6,$d6,$d6,$e6,$7c,$fc
.byte	$66,$6e,$66,$6d,$60,$60,$c0,$7d
.byte	$e6,$d6,$d6,$d6,$d6,$ce,$7f,$dd
.byte	$66,$6e,$66,$7c,$6c,$66,$e3,$ba
.byte	$66,$60,$3c,$06,$46,$66,$5c,$fe
.byte	$30,$68,$68,$62,$6c,$74,$3c,$e6
.byte	$66,$66,$66,$66,$6e,$6e,$3f,$c3
.byte	$66,$66,$66,$66,$66,$3d,$18,$e3
.byte	$c3,$c3,$d3,$cb,$df,$77,$62,$c0
.byte	$c3,$66,$3c,$3c,$66,$c3,$03,$c3
.byte	$66,$66,$3c,$5a,$18,$18,$18,$7e
.byte	$c6,$0c,$38,$7c,$60,$c3,$fe,$00
.byte	$1e,$18,$18,$18,$18,$1e,$00,$00
.byte	$40,$60,$30,$18,$0c,$06,$00,$00
.byte	$78,$18,$18,$18,$18,$78,$00,$00
.byte	$08,$1c,$36,$63,$00,$00,$00,$00
.byte	$00,$00,$00,$00,$00,$ff,$00,$00
.byte	$36,$7f,$7f,$3e,$1c,$08,$00,$18
.byte	$18,$0c,$07,$07,$0c,$18,$18,$03
.byte	$03,$03,$03,$03,$03,$03,$03,$18
.byte	$18,$38,$f0,$e0,$00,$00,$00,$18
.byte	$18,$30,$e0,$e0,$30,$18,$18,$00
.byte	$00,$00,$c0,$e0,$30,$18,$18,$03
.byte	$07,$0e,$1c,$38,$70,$e0,$c0,$c0
.byte	$e0,$70,$38,$1c,$0e,$07,$03,$01
.byte	$03,$07,$0f,$1f,$3f,$7f,$ff,$00
.byte	$00,$00,$00,$0f,$0f,$0f,$0f,$80
.byte	$c0,$e0,$f0,$f8,$fc,$fe,$ff,$0f
.byte	$0f,$0f,$0f,$00,$00,$00,$00,$f0
.byte	$f0,$f0,$f0,$00,$00,$00,$00,$ff
.byte	$ff,$00,$00,$00,$00,$00,$00,$00
.byte	$00,$00,$00,$00,$00,$ff,$ff,$00
.byte	$00,$00,$00,$f0,$f0,$f0,$f0,$00
.byte	$00,$1c,$1c,$77,$77,$08,$1c,$00
.byte	$00,$00,$03,$07,$0c,$18,$18,$00
.byte	$00,$00,$ff,$ff,$00,$00,$00,$18
.byte	$18,$18,$ff,$ff,$18,$18,$18,$00
.byte	$00,$3c,$7e,$7e,$7e,$3c,$00,$00
.byte	$00,$00,$00,$ff,$ff,$ff,$ff,$c0
.byte	$c0,$c0,$c0,$c0,$c0,$c0,$c0,$00
.byte	$00,$00,$c3,$e7,$3c,$18,$18,$18
.byte	$18,$3c,$e7,$c3,$00,$00,$00,$f0
.byte	$f0,$f0,$f0,$f0,$f0,$f0,$f0,$18
.byte	$18,$0c,$07,$03,$00,$00,$00,$78
.byte	$60,$78,$60,$7e,$18,$1e,$00,$00
.byte	$18,$3c,$7e,$18,$18,$18,$00,$00
.byte	$18,$18,$18,$7e,$3c,$18,$00,$00
.byte	$18,$30,$7e,$30,$18,$00,$00,$00
.byte	$18,$0c,$7e,$0c,$18,$00,$00,$18
.byte	$3c,$66,$c3,$18,$3c,$66,$c3,$00
.byte	$40,$3e,$66,$66,$66,$3b,$40,$30
.byte	$60,$62,$7c,$66,$66,$7c,$02,$00
.byte	$40,$3c,$76,$60,$66,$3c,$40,$0c
.byte	$06,$46,$3e,$66,$66,$3a,$40,$00
.byte	$40,$3c,$66,$7e,$60,$38,$04,$2e
.byte	$18,$18,$18,$3e,$18,$18,$00,$00
.byte	$86,$7c,$cc,$cc,$78,$c2,$7c,$c0
.byte	$60,$62,$6c,$76,$66,$66,$00,$30
.byte	$00,$18,$0c,$0c,$0c,$0e,$00,$0c
.byte	$00,$0c,$0c,$0c,$0c,$18,$38,$00
.byte	$c0,$66,$6c,$78,$6c,$e6,$00,$30
.byte	$18,$18,$18,$18,$18,$0c,$00,$00
.byte	$c0,$66,$7e,$7e,$6a,$63,$00,$00
.byte	$e2,$7c,$66,$66,$66,$63,$00,$00
.byte	$40,$3c,$6e,$66,$76,$3c,$40,$00
.byte	$c2,$7c,$66,$66,$7c,$62,$e0,$00
.byte	$40,$3b,$66,$66,$3e,$46,$07,$00
.byte	$e0,$7c,$76,$60,$60,$60,$00,$00
.byte	$40,$3e,$70,$3c,$8e,$7c,$02,$30
.byte	$30,$7e,$30,$30,$30,$36,$1c,$00
.byte	$01,$e6,$66,$66,$6e,$3b,$00,$00
.byte	$0d,$e6,$66,$66,$3c,$18,$00,$00
.byte	$00,$e3,$6b,$7f,$3e,$36,$00,$00
.byte	$c0,$66,$3c,$18,$3c,$66,$03,$00
.byte	$00,$e6,$66,$66,$3e,$cc,$78,$00
.byte	$00,$7e,$0c,$7e,$30,$7e,$00,$00
.byte	$18,$3c,$7e,$7e,$18,$3c,$00,$18
.byte	$18,$18,$18,$18,$18,$18,$18,$00
.byte	$7e,$78,$7c,$6e,$66,$06,$00,$08
.byte	$18,$38,$78,$38,$18,$08,$00,$00
.byte	$00,$00,$ff,$7e,$3c,$18,$00,$01
	
;
; Charset data
;
; For: Text (Atari Basic-) text modes 12 and 13 
;
; They are not ordered :(
;

		org $4800														
		
chset12
:8		.byte 0																; Empty						//0
 		.byte 127,127,127,31,31,7,7,1										; Cloud tile 1, bottom left //1
		.byte 255,125,20,0,0,0,0,0											; Cloud tile bottom			//2
		.byte 0	,	0	,	20	,	125	,	255	,	255	,	255	,	255		; Cloud tile top			//3
		.byte 253	,	253	,	244	,	244	,	208	,	208	,	64	,	64  ; Cloud tile, bottom right	//4
:8		.byte 255															; Cloud tile, solid block	//5

:8		.byte 85															; Solid black block			//6		
		.byte 85	,	85	,	85	,	85	,	85	,	101	,	166	,	170 ; Solid black block, bottom //7

:8		.byte 187													        ; Light green block 		//8
		.byte 186	,	186	,	186	,	186	,	186	,	186	,	186	,	186	; Not so light green block  //9
:8		.byte 174															; Solid green block			//10
:8		.byte 171															; Solid green block			//11
:8		.byte 64															; Flag pole 				//12
	    .byte 76	,	127	,	127	,	127	,	127	,	127	,	127	,	76  ; Flag part 	1			//13
		.byte 207	,	255	,	252	,	252	,	252	,	252	,	255	,	207 ; Flag Part 	2			//14
		.byte 85	,	85	,	65	,	65	,	65	,	85	,	85	,	85  ; Building part with window //15
	
;
; A poem
;

poem
	.byte "oh have i surely...."
	.byte "..slipped the bonds."
	.byte "..of earth and...   "
	.byte "danced the skies...."
	.byte "on laughter silvered"
	.byte "wings               "
	.byte "sunward i climbed   "
	.byte "and joined the      "	
	.byte "tumbeling mirth     "
	.byte "of sun splid clouds "
	.byte "and done hundred    "
	.byte "things you have not "
	.byte "dreamed of --       "
	.byte "wheeled and soared  "
	.byte "and swung high in   "
	.byte "the sunlit silence  "
	.byte "hov'ring there      "
	.byte "i chased the        "
	.byte "souting wind        "
	.byte "along and flung     "
	.byte "my eager craft      "
	.byte "throgh footless     "
	.byte "halls of air.       "
	.byte "up up the long      "
	.byte "delirious burning   "
	.byte "blue........        "
	.byte "i've topped the     "
	.byte "windswept heights   "
	.byte "with easy grace     "
	.byte "where never lark or "
	.byte "even eagle flew.    "
	.byte "and while, with     "
	.byte "with silent lifting "
	.byte "mind i've trod the  "
	.byte "heigh untresspassed "
	.byte "canctity of space   "
	.byte "put out my hand and "
	.byte "touced the d�face   "
	.byte "of god.             "    		
							
;
; Screen- ram of playfield
;
			org 5000
screen	.byte 0







	
	

