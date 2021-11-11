	processor 6502
        include "vcs.h"
        include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Váriaveis

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
JetAnimOffset   byte
Random		byte

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
	lda #0
        sta JetXPos
        
        lda #83
        sta BomberYPos
        lda #54
        sta BomberXPos
        
        lda #%11010100
        sta Random
        
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

; Pré-VBLANK processing
	lda JetXPos
        ldy #0
        jsr SetObjectXPos
        
        lda BomberXPos
        ldy #1
        jsr SetObjectXPos
        
        sta WSYNC
        sta HMOVE

; 1 + 3 lines of VSYNC
	lda #2
        sta VBLANK               ; turn on VBLANK
        sta VSYNC                ; turn on VSYNC
        REPEAT 3
            sta WSYNC            ; display 3 recommended lines of VSYNC
        REPEND
        lda #0
    	sta VSYNC                ; turn off VSYNC
; 37 lines of underscan
	REPEAT 37
        sta WSYNC            ; display the 37 recommended lines of VBLANK
        REPEND
        sta VBLANK               ; turn off VBLANK

; Scoreboard
	lda #0
        sta PF0
        sta PF1
        sta PF2
        sta GRP0
        sta GRP1
        sta COLUPF
        REPEAT 20
        	sta WSYNC
        REPEND
        

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
        
        ldx #84	; conta as scanlines remanecentes
        
.GameLineLoop:
.AreWeInsideJetSprite:
	txa
        sec
        sbc JetYPos
        cmp JET_HEIGHT
        bcc .DrawSpriteP0
        lda #0

.DrawSpriteP0:
	clc
	adc JetAnimOffset	;pular para o frame correto para gerar uma animacao de curva
        
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
        
	lda #0
        sta JetAnimOffset
        
; 29 linhas do overscan
        lda #2
        sta VBLANK               ; turn on VBLANK again
        REPEAT 30
            sta WSYNC            ; display 30 recommended lines of VBlank Overscan
        REPEND
        lda #0
        sta VBLANK               ; turn off VBLANK

; Joystick
CheckP0up:
	lda #%00010000
        bit SWCHA
        bne CheckP0Down
        inc JetYPos
        lda #0
        sta JetAnimOffset
        
CheckP0Down:
	lda #%00100000
        bit SWCHA
        bne CheckP0Left
        dec JetYPos
        lda #0
        sta JetAnimOffset

CheckP0Left:
	lda #%01000000
        bit SWCHA
        bne CheckP0Right
        dec JetXPos
        lda JET_HEIGHT
        sta JetAnimOffset

CheckP0Right:
	lda #%10000000
        bit SWCHA
        bne EndInputCheck
        inc JetXPos
        lda JET_HEIGHT
        sta JetAnimOffset
        
EndInputCheck:

; Atualizar posićões para o próximo frame
UpdateBomberPosition:
	lda BomberYPos
        clc
        cmp #0
        bmi .ResetBomberPosition
        dec BomberYPos
        jmp EndPositionUpdate
.ResetBomberPosition
        jsr GetRandomBomberPos	; pegar uma posicao aleatoria para o bomber

EndPositionUpdate:

; Check Colisões
CheckCollisionP0P1:
	lda #%10000000		;CXPPMM bit 7 detecta colisao do P0 com P1
        bit CXPPMM		;Checa o registrador CXPPMM teve a colisão acima
        bne .CollisionP0P1	;Se colidir vai para o tratamento da colisão
        jmp CheckCollisionP0PF
.CollisionP0P1:
	jsr GameOver

CheckCollisionP0PF:
	lda #%10000000		; CXP0FB detecta colisao do P0 com PF
	bit CXP0FB
        bne .CollisionP0PF
        jmp EndCollisionCheck	
        
.CollisionP0PF:
	jsr GameOver
        
EndCollisionCheck:   
	sta CXCLR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loopback to new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of our object
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
        sta WSYNC                ; start a fresh new scanline
        sec                      ; make sure carry-flag is set before subtracion
.Div15Loop
        sbc #15                  ; subtract 15 from accumulator
        bcs .Div15Loop           ; loop until carry-flag is clear
        eor #7                   ; handle offset range from -8 to 7
        asl
        asl
        asl
        asl                      ; four shift lefts to get only the top 4 bits
        sta HMP0,Y               ; store the fine offset to the correct HMxx
        sta RESP0,Y              ; fix object position in 15-step increment
        rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GameOver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
	lda #$30
        sta COLUBK
        rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subrotina para gerar um numero aleatorio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dividir por 4 e adicionar 30 para caber dentro do Rio no mapa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPos subroutine
	lda Random
        asl
        eor Random
        asl
        eor Random
        asl
        asl
        eor Random
        asl
        rol Random
        
        lsr		; Divide por 2
        lsr		; Divide por 2 totalizando divisão por 4
        sta BomberXPos
        lda #30
        adc BomberXPos
        sta BomberXPos
        
        lda #96
        sta BomberYPos
        rts

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
