	processor 6502
        include "vcs.h"
        include "macro.h"
        include "xmacro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variables segment

        seg.u Variables
	org $80

JetXPos		.byte
JetYPos		.byte
BomberXPos	.byte
BomberyPos	.byte

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


StartFrame:
; 1 + 3 lines of VSYNC
	VERTICAL_SYNC
; 37 lines of underscan
	TIMER_SETUP 37
        TIMER_WAIT


GameVisibleLine:
; 192 visible lines of frame
	lda #$84
        sta COLUBK	;SET BACKGROUND
        lda #$C2
        sta COLUPF	; cor do playfield
        lda #%00000001
        sta CTRLPF	; refletir o playfield
        
        ; Desenha o playfield
        lda #$F0
        sta PF0
        lda #$F0
        sta PF1
        lda #0
        sta PF2
        
        ldx #192	; conta as scanlines remanecentes
        
.GameLineLoop:
	sta WSYNC
        dex
        bne .GameLineLoop
        
; 29 lines of overscan
	TIMER_SETUP 30
        TIMER_WAIT
; total = 262 lines, go to next frame
        jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Epilogue

	org $fffc
        .word Reset	; reset vector
        .word Reset	; BRK vector
