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
MissileXPos	byte
MissileYPos	byte
Score		byte	;2 digit score
Timer		byte	;2 digit timer
Temp		byte 	; variavel auxiliar para calcular o score e timer
OnesDigitOffset	word
TensDigitOffset word
JetSpritePtr	word
JetColorPtr	word
BomberSpritePtr word
BomberColorPtr	word
JetAnimOffset   byte
Random		byte
ScoreSprite	byte
TimerSprite	byte
TerrainColor	byte
RiverColor	byte


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constantes

JET_HEIGHT = 9
JET_TOP_LIMIT = 77
JET_BOT_LIMIT = 1
JET_LEF_LIMIT = 101
JET_RIG_LIMIT = 31
BOMBER_HEIGHT = 9
DIGITS_HEIGHT = 5

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
	lda #63
        sta JetXPos
        
        lda #83
        sta BomberYPos
        lda #54
        sta BomberXPos
        
        lda #%11010100
        sta Random
        
        lda #0
        sta Score
        sta Timer

        lda #%00010000          ;seta o missile0 no tamanho 2x
        sta NUSIZ0

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
	lda #2
        sta VBLANK               ; liga o VBLANK
        sta VSYNC                ; liga o VSYNC
        REPEAT 3
            sta WSYNC            ; aguarda as 3 linhas do VSYNC
        REPEND
        lda #0
    	sta VSYNC                ; desliga o VSYNC
; 37 lines of underscan
	REPEAT 34
        	sta WSYNC            ; mostra 34 linhas do VBLANK o resto são os ciclos do processamento abaixo até completar 37
        REPEND
        
; VBLANK processing
	lda JetXPos
  
        ldy #0
        jsr SetObjectXPos
        
        lda BomberXPos
        ldy #1
        jsr SetObjectXPos
        
        lda MissileXPos
        ldy #2
        jsr SetObjectXPos

        jsr CalculateDigitOffset ; calcula o offset do digito
        
        jsr JetSound

        sta WSYNC
        sta HMOVE
        
        lda #0
        sta VBLANK               ; turn off VBLANK

; Scoreboard
    
        lda #0
        sta COLUBK
        sta PF0
        sta PF1
        sta PF2
        sta GRP0
        sta GRP1
        sta CTRLPF
	sta COLUBK
         
        lda #$1E
        sta COLUPF              
        
        ldx #DIGITS_HEIGHT

.ScoreDigitLoop:
; Score
	ldy TensDigitOffset
        lda Digits,Y
        and #$F0
        sta ScoreSprite
        ldy OnesDigitOffset
        lda Digits,Y
        and #$0F
        ora ScoreSprite
        sta ScoreSprite
        sta WSYNC
        sta PF1
        
;Timer
	ldy TensDigitOffset+1
        lda Digits,Y
        and #$F0
        sta TimerSprite
        
        ldy OnesDigitOffset+1
        lda Digits,Y
        and #$0F
        ora TimerSprite
        sta TimerSprite
        
        jsr Sleep12Cycles
        
        sta PF1
        
        ldy ScoreSprite
        sta WSYNC
        
        sty PF1
        
        inc TensDigitOffset
        inc TensDigitOffset+1
        inc OnesDigitOffset
        inc OnesDigitOffset+1
        
        jsr Sleep12Cycles
        
        dex
        sta PF1
        bne .ScoreDigitLoop
        
        sta WSYNC
        
        lda #0
        sta PF0
        sta PF1
        sta PF2
        sta WSYNC
        sta WSYNC
        sta WSYNC
        sta WSYNC

GameVisibleLine:
	lda TerrainColor
        sta COLUPF
        
        lda RiverColor
        sta COLUBK
        
        lda #%00000001
        sta CTRLPF	; refletir o playfield
        
        ; Desenha o playfield
        lda #$F0
        sta PF0
        lda #$FC
        sta PF1
        lda #0
        sta PF2
        
        ldx #88          ; conta as scanlines remanecentes usando kernel * 2 ou seja 89 * 2 scanlines = 178 
                         ; as 21 faltantes são desenhadas ao longo do processamento
        
.GameLineLoop:
	lda #%00000000
        cpx MissileYPos
        bne .SkipMissileDraw
.MissileDraw:
	lda #%00000010
        inc MissileYPos
.SkipMissileDraw:
	sta ENAM0

.AreWeInsideJetSprite:
	txa
        sec
        sbc JetYPos
        cmp #JET_HEIGHT
        bcc .DrawSpriteP0
        lda #0

.DrawSpriteP0:
	clc
	adc JetAnimOffset	;pular para o frame correto para gerar a animação de curva
        
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
        cmp #BOMBER_HEIGHT
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
        sta WSYNC
        
; 30 linhas do overscan
        lda #2
        sta VBLANK               ; liga o VBLANK de novo para o overscan
        REPEAT 26
            sta WSYNC            ; desenha as 26 linhas do overscan as 4 faltantes é o processamento de colisão abaixo
        REPEND
        lda #0
        sta VBLANK               ; desliga o VBLANK

; Joystick
CheckP0up:
	lda #%00010000
        bit SWCHA
        bne CheckP0Down
        lda #JET_TOP_LIMIT
        cmp JetYPos
        bmi CheckP0Down
.P0UpPressed:
        inc JetYPos
        lda #0
        sta JetAnimOffset
        
CheckP0Down:
	lda #%00100000
        bit SWCHA
        bne CheckP0Left
	lda #JET_BOT_LIMIT
        cmp JetYPos
        bpl CheckP0Left
.P0DownPressed:
        dec JetYPos
        lda #0
        sta JetAnimOffset

CheckP0Left:
	lda #%01000000
        bit SWCHA
        bne CheckP0Right
        lda #JET_RIG_LIMIT
        cmp JetXPos
        bpl CheckP0Right
.P0LeftPressed:
        dec JetXPos
        lda #JET_HEIGHT
        sta JetAnimOffset

CheckP0Right:
	lda #%10000000
        bit SWCHA
        bne CheckButtonPressed
        lda #JET_LEF_LIMIT
        cmp JetXPos
        bmi CheckButtonPressed
.P0RightPressed:
        inc JetXPos
        lda #JET_HEIGHT
        sta JetAnimOffset

CheckButtonPressed:
	lda #%10000000
	bit INPT4
        bne EndInputCheck
.ButtonPressed:        
        lda JetXPos
        clc 
        adc #5
        sta MissileXPos
        clc
        adc #8
        lda JetYPos
        clc
        adc #2
        sta MissileYPos

EndInputCheck:

; Atualizar posições para o próximo frame
UpdateBomberPosition:
	lda BomberYPos
        clc
        cmp #0
        bmi .ResetBomberPosition
        dec BomberYPos
        jmp EndPositionUpdate
.ResetBomberPosition
        jsr GetRandomBomberPos	; pegar uma posicao aleatória para o bomber
        
.SetTimerValues        
        sed
        lda Timer
        clc
        adc #1
        sta Timer
        cld

EndPositionUpdate:

; Check Colisões
CheckCollisionP0P1:
	lda #%10000000		;CXPPMM bit 7 detecta colisao do P0 com P1
        bit CXPPMM		;Checa o registrador CXPPMM teve a colisão acima
        bne .P0P1Collided	;Se colidir vai para o tratamento da colisão
        jsr SetTerrainRiverColor
        jmp CheckCollisionM0P1
.P0P1Collided:
	jsr GameOver

CheckCollisionM0P1:		;Checa a colisão do missil com o avião inimigo Bomber
	lda #%10000000
        bit CXM0P
        bne .M0P1Collided
        jmp EndCollisionCheck
.M0P1Collided:
	sed
        lda Score
	clc
        adc #1
        sta Score
        cld
        lda #0
        sta MissileYPos		; Some o missil
        jsr GetRandomBomberPos	; Gera um novo bomber
        
EndCollisionCheck:   
	sta CXCLR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loopback to new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Audio do aviao e vai mudar o PITCH de acordo com a posição Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JetSound:
	lda #3
        sta AUDV0
        
        lda JetYPos
        lsr
        lsr
        lsr
        sta Temp
        lda #31
        sec
        sbc Temp
        sta AUDF0
        
        lda #8
        sta AUDC0
        
	rts

SetTerrainRiverColor subroutine
	lda #$C2
        sta TerrainColor
        lda #$84
        sta RiverColor
        rts

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
        sta TerrainColor
        sta RiverColor
        lda #$0
        sta Score
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calcula o endereco de memoria para escolher o offset certo do digito
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cada sprite do score tem o tamanho de 5, então você tem que multiplicar
;; por 5 para encontrar o sprite correto
;; Para multiplicar vc shifita 2 vezes para multiplicar por 4 e soma
;; mais uma vez o próprio valor. (N*2*2)+N
;;
;; PAra a outra parte precisa dividir por 16 e multiplicar por 5
;; Para isso precisa fazer N/16*5 = ((N/2/2)+(N/2/2/2/2)) * 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
	ldx #1
.PrepareScoreLoop:		; Loopa 2 vezes 1 para o timer outra para o score
	lda Score,X		; Load acumulator com o timer X=1 Timer X=0 Score
        and #%00001111
        sta Temp		; salva o valor do A no temp
        asl
        asl
        adc Temp
        sta OnesDigitOffset,X
        
        lda Score,X
        and #%11110000
        lsr
        lsr
        sta Temp
        lsr
        lsr
        adc Temp
        sta TensDigitOffset,X
               
        dex
        bpl .PrepareScoreLoop
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desperdica ciclos para sincronizar com o TIA (Race the Beam)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

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
    .byte #$34;
    .byte #$34;
    .byte #$02;
    .byte #$02;
    .byte #$C8;
    .byte #$1C;
    .byte #$04;
    .byte #$06;
    .byte #$0A;
    
JetColorTurn:
    .byte #$34;
    .byte #$34;
    .byte #$02;
    .byte #$02;
    .byte #$C8;
    .byte #$1C;
    .byte #$04;
    .byte #$06;
    .byte #$0A;

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