	processor 6502
        include "vcs.h"
        include "macro.h"
        include "xmacro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variables segment

        seg.u Variables
	org $80

JetXPos		byte
JetYPos		byte
BomberXPos	byte
BomberYPos	byte
JetSpritePtr	word
JetColorPtr	word
BomberSpritePtr word
BomberColorPtr	word

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constantes

JET_HEIGHT = 9
BOMBER_HEIGHT = 9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Code segment

	seg Code
        org $f000

Reset:
	CLEAN_START
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Inicializar variáveis

	lda #10
        sta JetYPos
	lda #60
        sta JetXPos
        
        lda #83
        sta BomberYPos
        lda #54
        sta BomberXPos
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Inicializar ponteiros
	lda #<JetSprite
        sta JetSpritePtr
        lda #>JetSprite
        sta JetSpritePtr+1
        
        lda #<JetColor
        sta JetColorPtr
        lda #>JetColor
        sta JetColorPtr+1
        
        lda #<BomberSprite
        sta BomberSpritePtr
        lda #>BomberSprite
        sta BomberSpritePtr+1
        
        lda #<BomberColor
        sta BomberColorPtr
        lda #>BomberColor
        sta BomberColorPtr+1

StartFrame:
; 1 + 3 lines of VSYNC
	VERTICAL_SYNC
; 37 lines of underscan
	TIMER_SETUP 37
        TIMER_WAIT


GameVisibleLine:
; 96 visible lines of frame porque vamos usar 2 lines kernel
	lda #$84
        sta COLUBK	;SET BACKGROUND
        lda #$C2
        sta COLUPF	; cor do playfield
        lda #%00000001
        sta CTRLPF	; refletir o playfield
        
        ; Desenha o playfield
        lda #$F0
        sta PF0
        lda #$FC
        sta PF1
        lda #0
        sta PF2
        
        ldx #96	; conta as scanlines remanecentes
        
.GameLineLoop:
.AreWeInsideJetSprite:
	txa
        sec
        sbc JetYPos
        cmp JET_HEIGHT
        bcc .DrawSpriteP0
        lda #0

.DrawSpriteP0:
	tay
        lda (JetSpritePtr),Y
        sta WSYNC
        sta GRP0
        lda (JetColorPtr),Y
        sta COLUP0
        
.AreWeInsideBomberSprite:
	txa
        sec
        sbc BomberYPos
        cmp BOMBER_HEIGHT
        bcc .DrawSpriteP1
        lda #0

.DrawSpriteP1:
	tay
        
        lda #%00000101
        sta NUSIZ1
        
        lda (BomberSpritePtr),Y
        sta WSYNC
        sta GRP1
        lda (BomberColorPtr),Y
        sta COLUP1
        
        dex
        bne .GameLineLoop
        
; 29 lines of overscan
	TIMER_SETUP 30
        TIMER_WAIT
; total = 262 lines, go to next frame
        jmp StartFrame


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JetSprite:
    .byte #%00000000         ;
    .byte #%00010100         ;   # #
    .byte #%01111111         ; #######
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

JetSpriteTurn:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

BomberSprite:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00101010         ;  # # #
    .byte #%00111110         ;  #####
    .byte #%01111111         ; #######
    .byte #%00101010         ;  # # #
    .byte #%00001000         ;    #
    .byte #%00011100         ;   ###

JetColor:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColor:
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Epilogue

	org $fffc
        .word Reset	; reset vector
        .word Reset	; BRK vector
