	CPU 
	 org $7b00
	jp block1
Screen_Count:	dw	0
Screen_Loc:	dw	0
Screen_c:	db	0
Screen_ga:	db	0
Screen_u:	db	0
Screen_i:	db	0
Screen_j:	db	0
Screen_gfxval:	db	0
Screen_col_and:	db	0
Screen_col_or:	db	0
Screen_dx:	db	0
Screen_dy:	db	0
Screen_d:	db	0
Screen_xi:	db	0
Screen_yi:	db	0
Screen_ai:	db	0
Screen_bi:	db	0
Screen_x1:	db	0
Screen_y1:	db	0
Screen_x2:	db	0
Screen_y2:	db	0
Screen_rx:	db	0
Screen_ry:	db	0
Screen_w:	db	0
Screen_h:	db	0
Screen_x:	db	$00
Screen_y:	db	$00
Screen_tab32:	dw $00, $20, $40, $60, $80, $a0, $c0, $e0
	dw $100, $120, $140, $160, $180, $1a0, $1c0, $1e0
	dw $200, $220, $240, $260, $280, $2a0, $2c0, $2e0
	dw $300, $320, $340, $360, $380, $3a0, $3c0, $3e0
	dw $400, $420, $440, $460, $480, $4a0, $4c0, $4e0
	dw $500, $520, $540, $560, $580, $5a0, $5c0, $5e0
	dw $600, $620, $640, $660, $680, $6a0, $6c0, $6e0
	dw $700, $720, $740, $760, $780, $7a0, $7c0, $7e0
Memory_c:	db	$00
Compression_in: dw  0
Compression_out: dw  0
Compression_outsize:	dw	0
Compression_len:	dw	0
Font_text: dw  0
Font_currentFont: dw  0
Font_zp: dw  0
Font_src: dw  0
Font_scr: dw  0
Font_tx:	db	0
Font_ty:	db	0
Font_i:	db	0
Font_j:	db	0
Font_tpos:	dw	0
Font_tran:	db	0
Font_font8x8tab:	dw $00, $02, $04, $06, $08, $0a, $0c, $0e
	dw $10, $12, $14, $16, $18, $1a, $1c, $1e
	dw $100, $102, $104, $106, $108, $10a, $10c, $10e
	dw $110, $112, $114, $116, $118, $11a, $11c, $11e
Font_font1:	db $00, $00, $00, $00, $00, $010, $010, $010
	db $00, $010, $044, $044, $00, $00, $00, $044
	db $054, $044, $054, $044, $010, $054, $050, $014
	db $054, $044, $04, $010, $040, $044, $010, $044
	db $011, $044, $011, $010, $010, $00, $00, $00
	db $04, $010, $010, $010, $04, $040, $010, $010
	db $010, $040, $044, $010, $054, $010, $044, $00
	db $010, $054, $010, $00, $00, $00, $00, $010
	db $040, $00, $00, $054, $00, $00, $00, $00
	db $00, $00, $040, $04, $04, $010, $040, $040
	db $054, $044, $044, $044, $054, $010, $050, $010
	db $010, $054, $054, $04, $054, $040, $054, $054
	db $04, $014, $04, $054, $044, $044, $054, $04
	db $04, $054, $040, $054, $04, $054, $054, $040
	db $054, $044, $054, $054, $04, $04, $04, $04
	db $054, $044, $054, $044, $054, $054, $044, $054
	db $04, $04, $00, $040, $00, $040, $00, $00
	db $010, $00, $010, $040, $04, $010, $040, $010
	db $04, $00, $054, $00, $054, $00, $040, $010
	db $04, $010, $040, $054, $04, $010, $00, $010
	db $010, $044, $044, $040, $014, $010, $044, $054
	db $044, $044, $050, $044, $050, $044, $050, $014
	db $040, $040, $040, $014, $050, $044, $044, $044
	db $050, $054, $040, $050, $040, $054, $054, $040
	db $050, $040, $040, $014, $040, $044, $044, $014
	db $044, $044, $054, $044, $044, $054, $010, $010
	db $010, $054, $054, $04, $04, $044, $010, $044
	db $044, $050, $044, $044, $040, $040, $040, $040
	db $054, $044, $054, $044, $044, $044, $010, $044
	db $044, $044, $044, $010, $044, $044, $044, $010
	db $050, $044, $050, $040, $040, $010, $044, $044
	db $044, $014, $050, $044, $050, $044, $044, $014
	db $040, $010, $04, $050, $054, $010, $010, $010
	db $010, $044, $044, $044, $044, $014, $044, $044
	db $044, $044, $010, $044, $044, $044, $054, $044
	db $044, $044, $010, $044, $044, $044, $044, $044
	db $010, $010, $054, $04, $010, $040, $054, $054
	db $040, $040, $040, $054, $040, $040, $010, $04
	db $04, $054, $04, $04, $04, $054, $010, $044
	db $00, $00, $00, $00, $00, $00, $00, $054
	db $040, $010, $00, $00, $00, $00, $014, $044
	db $044, $014, $040, $050, $044, $044, $050, $00
	db $014, $040, $040, $014, $04, $014, $044, $044
	db $014, $010, $044, $050, $040, $014, $014, $010
	db $054, $010, $010, $010, $044, $014, $04, $050
	db $040, $040, $050, $044, $044, $010, $00, $010
	db $010, $010, $04, $00, $04, $044, $010, $040
	db $044, $050, $044, $044, $010, $010, $010, $010
	db $010, $00, $044, $054, $044, $044, $00, $010
	db $044, $044, $044, $00, $010, $044, $044, $010
	db $00, $010, $044, $050, $040, $00, $010, $044
	db $014, $04, $00, $010, $044, $040, $040, $00
	db $014, $050, $014, $050, $040, $050, $040, $044
	db $010, $00, $044, $044, $044, $014, $00, $044
	db $044, $044, $010, $00, $044, $044, $054, $044
	db $00, $044, $010, $010, $044, $00, $044, $014
	db $04, $010, $00, $054, $014, $050, $054, $014
	db $010, $040, $010, $014, $010, $010, $010, $010
	db $010, $050, $010, $04, $010, $050, $011, $044
	db $00, $00, $00
Input_c:	db	$00
Sound_freq:	dw	0
Sound_dur:	dw	0
Functions_i:	dw	0
Functions_j:	db	0
Functions_h:	db	0
Functions_l:	db	0
Functions_p: dw  0
Functions_s: dw  0
Sprite_spritex:	db	0
Sprite_spritey:	db	0
Sprite_no:	db	0
Sprite_spritedatasize:	db	0
Sprite_spritewidth:	db	0
Sprite_spriteheight:	db	0
Sprite_i:	db	0
Sprite_x:	db	0
Sprite_y:	db	0
Sprite_loc:	dw	0
Sprite_src: dw  0
Sprite_dst: dw  0
Sprite_spritedata: dw  0
data:
	incbin	 "D:/VZ/TRSE_Mysrc/Millipede///title.bin_c"
tempint:	dw	0
mychar:		db	"A"
	db	0
myp: dw  0
mym: dw  0
strpos:	db	$00
Value:	db	0
BitFlag:	db	0
milliMoveSpeed:	db	0
	gameGrid:	 ds 448
gridRandoms:	db $018, $04, $01d, $016, $01a, $018, $01a, $013
	db $015, $08, $017, $014, $014, $01, $01e, $01b
	db $013, $02, $010, $014, $0b, $02, $012, $011
	db $01, $09, $0e, $09, $01d, $01d, $011, $08
	db $07, $05, $02, $00, $01f, $0a, $016, $018
	db $08, $013, $01, $01d, $07, $019, $01d, $014
	db $04, $0f, $0c, $019, $00, $0b, $017, $06
	db $06, $01f, $015, $02, $01a, $01d, $0a, $0f
	db $014, $017, $01e, $01e, $01f, $09, $06, $02
	db $0e, $0c, $01c, $016, $07, $015, $06, $01
	db $014, $01f, $04, $019, $01e, $08, $0b, $018
	db $011, $018, $01b, $0e, $014, $09, $012, $012
	db $01e, $04, $01c, $06, $012, $04, $014, $09
	db $01a, $0c, $04, $0c, $01a, $013, $012, $0c
	db $019, $01e, $05, $0f, $0e, $05, $017, $018
	db $04, $01b, $0d, $04, $01a, $0b, $01a, $014
	db $016, $010, $05, $01c, $01, $02, $012, $01f
	db $014, $014, $011, $015, $06, $0b, $018, $00
	db $0a, $018, $01f, $02, $01b, $0c, $013, $01f
	db $014, $08, $0d, $013, $04, $012, $06, $07
	db $01e, $02, $01b, $013, $010, $07, $01, $012
	db $011, $01f, $06, $018, $011, $01, $010, $06
	db $06, $014, $00, $01d, $010, $017, $03, $010
	db $012, $01, $01c, $01c, $015, $0c, $011, $019
	db $012, $00, $016, $015, $0f, $05, $0a, $012
	db $018, $011, $06, $01f, $05, $017, $0c, $01
	db $011, $014, $0c, $01e, $019, $03, $0a, $01a
	db $01e, $05, $01e, $0b, $00, $01a, $013, $010
	db $0b, $017, $0b, $012, $0e, $01b, $01a, $014
	db $019, $017, $01c, $011, $0d, $01f, $0f, $013
	db $01d, $019, $09, $03, $01d, $01b, $01, $0e
	db $0e, $01e, $0f, $010, $01f, $01f, $012, $015
smallSprites:	db $00, $00, $00, $00, $014, $055, $028, $028
	db $014, $055, $028, $08, $014, $055, $00, $00
	db $014, $00, $00, $00, $028, $0aa, $03c, $03c
	db $028, $0aa, $03c, $00, $028, $0aa, $00, $00
	db $028, $00, $00, $00, $03c, $0df, $0ff, $0cc
	db $014, $0d7, $055, $014, $014, $00, $00, $00
	db $014, $055, $055, $014, $017, $055, $055, $017
	db $014, $055, $055, $0d7, $0d4, $055, $055, $0d4
	db $0d7, $055, $055, $014, $00, $010, $00, $00
	db $00, $010, $04, $00, $00, $04, $010, $00
	db $044, $011, $044, $011, $00, $030, $00, $00
	db $00, $030, $0c, $00, $00, $0c, $030, $00
	db $0cc, $033, $0cc, $033
bigSprites:	db $00, $00, $030, $0c, $0c7, $0d3, $0f, $0f0
	db $030, $0c, $030, $0c, $030, $0c, $0cc, $033
	db $07, $0d0, $0f, $018, $00, $030, $0c, $0c0
	db $03, $0f0, $0f, $0c, $030, $07, $0d0, $03f
	db $0fc, $0c0, $03, $00, $00, $030, $0c, $0cc
	db $033, $07, $0d0, $0f, $018, $00, $030, $0c
	db $0c0, $03, $00, $00, $0cc, $0cc, $030, $030
	db $037, $07c, $0f, $0ff, $03, $0fc, $00, $00
	db $0cc, $0cc, $030, $030, $037, $073, $0f, $0ff
	db $03, $0fc, $00, $00, $030, $030, $030, $030
	db $037, $07c, $0f, $0ff, $03, $0fc, $00, $00
	db $0cc, $0cc, $030, $030, $037, $073, $0f, $0ff
	db $03, $0fc, $00, $00, $033, $033, $0c, $0c
	db $03d, $0dc, $0ff, $0f0, $03f, $0c0, $00, $00
	db $033, $033, $0c, $0c, $0cd, $0dc, $0ff, $0f0
	db $03f, $0c0, $00, $00, $0c, $0c, $0c, $0c
	db $03d, $0dc, $0ff, $0f0, $03f, $0c0, $00, $00
	db $033, $033, $0c, $0c, $0cd, $0dc, $0ff, $0f0
	db $03f, $0c0, $0cc, $033, $037, $0dc, $0d0, $043
	db $0c1, $07, $037, $0dc, $0cc, $033, $00, $00
	db $0cc, $033, $037, $0cc, $033, $0dc, $0cc, $033
	db $00, $00, $00, $04, $010, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00
	db $01, $0c0, $03, $040, $00, $00, $00, $00
	db $00, $00
lookupGridY:	dw $00, $20, $40, $60, $80, $a0, $c0, $e0
	dw $100, $120, $140, $160, $180, $1a0
lookupScreenY:	dw $00, $04, $08, $0c, $10, $14, $18, $1c
	dw $20, $24, $28, $2c, $30, $34
screenYtogridY:	db $00, $00, $00, $00, $01, $01, $01, $01
	db $02, $02, $02, $02, $03, $03, $03, $03
	db $04, $04, $04, $04, $05, $05, $05, $05
	db $06, $06, $06, $06, $07, $07, $07, $07
	db $08, $08, $08, $08, $09, $09, $09, $09
	db $0a, $0a, $0a, $0a, $0b, $0b, $0b, $0b
	db $0c, $0c, $0c, $0c, $0d, $0d, $0d, $0d
varPrefixed_p:	db	0
q:	db	0
s:	db	0
t:	db	0
u:	db	0
v:	db	0
w:	db	0
x:	db	0
y:	db	0
message:		db	"Press 'S' to start! You can use either Joystick or Keys:QA = Up/Down and M, = Left/Right Space to fire. Millipede was developed by Jason Oakley / Blue Bilby using TRSE. Check out more stuff at BlueBilby.com ... "
	db	0
strlen:	db	$d2
playerBullet_playerBullet_record_playerBullet_record_fired	db	0
playerBullet_playerBullet_record_playerBullet_record_moveCount	db	0
playerBullet_playerBullet_record_playerBullet_record_x	db	0
playerBullet_playerBullet_record_playerBullet_record_y	db	0
playerSnake_playerSnake_record_playerSnake_record_moveCount	db	0
playerSnake_playerSnake_record_playerSnake_record_x	db	0
playerSnake_playerSnake_record_playerSnake_record_y	db	0
gameStats_gameStats_record_gameStats_record_lives	db	0
gameStats_gameStats_record_gameStats_record_level	db	0
gameStats_gameStats_record_gameStats_record_score	dw	0
gameStats_gameStats_record_gameStats_record_hiScore	dw	0
gameStats_gameStats_record_gameStats_record_gameRunning	db	0
fleaEnemy_flea_record_flea_record_enabled	db	0
fleaEnemy_flea_record_flea_record_dropCount	db	0
fleaEnemy_flea_record_flea_record_shotCount	db	0
fleaEnemy_flea_record_flea_record_moveSpeed	db	0
fleaEnemy_flea_record_flea_record_animFrame	db	0
fleaEnemy_flea_record_flea_record_x	db	0
fleaEnemy_flea_record_flea_record_y	db	0
milliSegments_milliSegments_record_milliSegments_record_x	db	0
	    db 0,0,0,0,0,0,0,0,0,0,0
milliSegments_milliSegments_record_milliSegments_record_y	db	0
	    db 0,0,0,0,0,0,0,0,0,0,0
milliSegments_milliSegments_record_milliSegments_record_flags	db	0
	    db 0,0,0,0,0,0,0,0,0,0,0
	 
; //	Clears the screen using mode parameter 0 or 1
; //	
	; ***********  Defining procedure : Screen_Cls
	;    Procedure type : User-defined procedure
 ; Temp vars section
DrawPadded_stringassignstr298: db "0",0
DrawPadded_stringassignstr324: db "0",0
DrawHUD_stringassignstr327: db "Score:",0
DrawHUD_stringassignstr329: db "Lives:",0
DrawHUD_stringassignstr331: db "Hi:",0
 ; Temp vars section ends
Screen_Cls_block2:
Screen_Cls:
	; ****** Inline assembler section
		ld a, [Screen_u]
		cp #00
		jp z,mode0
		ld a,#00
		jr cls
mode0
		ld a,#20
cls
		ld hl,#7000
		ld de,#7001
		ld bc,#0800
		ld(hl),a
		ldir
	
	ret
	;*
; //Sets the VZ200 resolution mode.
; //<ul>
; //<li>0: Text mode</li>
; //<li>1: Graphics mode 128x64</li>
; //</ul>
; 

	; ***********  Defining procedure : Screen_SetMode
	;    Procedure type : User-defined procedure
Screen_SetMode_block3:
Screen_SetMode:
	ld a, $783b
	ld [Screen_j], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[Screen_ga]
	cp $1
	jr nz,Screen_SetMode_elseblock6
Screen_SetMode_ConditionalTrueBlock5: ;Main true block ;keep :
	ld a, $2
	ld [Screen_gfxval], a
	ld a, $8
	ld [Screen_col_or], a
	ld a, $ef
	ld [Screen_col_and], a
	; generic assign 
	ld a,[Screen_gfxval]
	ld b,a
	ld a,[Screen_col_or]
	add  a, b
	ld [Screen_i], a
	; generic assign 
	ld b,a
	ld a,[Screen_j]
	and  b
	ld [Screen_j], a
	ld [$783b],a
	ld a,[Screen_j]
	ld [$6800],a
	jr Screen_SetMode_elsedoneblock7
Screen_SetMode_elseblock6:
	ld a, $0
	ld [Screen_gfxval], a
	ld a, $8
	ld [Screen_col_or], a
	ld a, $ef
	ld [Screen_col_and], a
	; generic assign 
	ld a,[Screen_gfxval]
	ld b,a
	ld a,[Screen_col_or]
	add  a, b
	ld [Screen_i], a
	; generic assign 
	ld b,a
	ld a,[Screen_j]
	and  b
	ld [Screen_j], a
	ld [$783b],a
Screen_SetMode_elsedoneblock7:
	ret
	;*
; //	Set pen colour(0-3) for plotting pixels in mode(1)
; //	*
	; ***********  Defining procedure : Screen_SetPen
	;    Procedure type : User-defined procedure
Screen_SetPen_block12:
Screen_SetPen:
	; ****** Inline assembler section
		ld a,[Screen_c]
		ld(#7846),a
		
	ret
	;*
; //	Get pen colour in mode(1)
; //	*
	; ***********  Defining procedure : Screen_GetPen
	;    Procedure type : User-defined procedure
Screen_GetPen:
	; ****** Inline assembler section
		ld a,(#7846)
		ld [Screen_c],a
		
	ret
	;*
; //	Set paper(background) color 0 or 1
; //	*
	; ***********  Defining procedure : Screen_SetPaper
	;    Procedure type : User-defined procedure
Screen_SetPaper_block14:
Screen_SetPaper:
	; ****** Inline assembler section
		ld a,[Screen_c]
		cp #00
		jr z,bg0
		ld a,(#6800)
		set 4,a
		ld(#6800),a
		ld(#783B),a
		ret
bg0		
		ld a,(#6800)
		res 4,a
		ld(#6800),a
		ld(#783B),a
	
	ret
	; ***********  Defining procedure : Screen_PutPixel
	;    Procedure type : User-defined procedure
Screen_PutPixel_block15:
Screen_PutPixel:
	; ****** Inline assembler section
	ld a,(#7846)
	ld c,a
	ld      a, [Screen_y]            ; get y
	ld 			h,a
	cp      #40             ; >= 64 ?
	jr      nc, psetx       ; nah, wont pset there
	ld      a, [Screen_x]            ; get x
	ld      l,a
	cp      #80            ; >= 128 ?
	jr      nc, psetx
	sla     l               ; calculate screen offset
	srl     h
	rr      l
	srl     h
	rr      l
	srl     h
	rr      l
	and     #03              ; pixel offset
	inc     a
	ld      b, #fc
pset1   rrc     b
	rrc     b
	rrc     c
	rrc     c
	dec     a
	jr      nz, pset1
	ld	de, #7000
	add	hl, de
	ld      a,(hl)
	and     b
	or      c
	ld(hl), a
psetx
	
	ret
	; ***********  Defining procedure : Screen_DrawLine
	;    Procedure type : User-defined procedure
Screen_DrawLine_block16:
Screen_DrawLine:
	ld a, $0
	ld [Screen_dx], a
	ld [Screen_dy], a
	ld [Screen_d], a
	; Binary clause core: LESS
	; Compare two vars optimization
	ld a,[Screen_x2]
	ld b,a
	ld a,[Screen_x1]
	cp b
	jr nc,Screen_DrawLine_elseblock19
Screen_DrawLine_ConditionalTrueBlock18: ;Main true block ;keep :
	ld a, $1
	ld [Screen_xi], a
	; generic assign 
	ld a,[Screen_x1]
	ld b,a
	ld a,[Screen_x2]
	sub  b
	ld [Screen_dx], a
	jr Screen_DrawLine_elsedoneblock20
Screen_DrawLine_elseblock19:
	ld a, $ff
	ld [Screen_xi], a
	; generic assign 
	ld a,[Screen_x2]
	ld b,a
	ld a,[Screen_x1]
	sub  b
	ld [Screen_dx], a
Screen_DrawLine_elsedoneblock20:
	; Binary clause core: LESS
	; Compare two vars optimization
	ld a,[Screen_y2]
	ld b,a
	ld a,[Screen_y1]
	cp b
	jr nc,Screen_DrawLine_elseblock27
Screen_DrawLine_ConditionalTrueBlock26: ;Main true block ;keep :
	ld a, $1
	ld [Screen_yi], a
	; generic assign 
	ld a,[Screen_y1]
	ld b,a
	ld a,[Screen_y2]
	sub  b
	ld [Screen_dy], a
	jr Screen_DrawLine_elsedoneblock28
Screen_DrawLine_elseblock27:
	ld a, $ff
	ld [Screen_yi], a
	; generic assign 
	ld a,[Screen_y2]
	ld b,a
	ld a,[Screen_y1]
	sub  b
	ld [Screen_dy], a
Screen_DrawLine_elsedoneblock28:
	; generic assign 
	ld a,[Screen_x1]
	ld [Screen_x], a
	; generic assign 
	ld a,[Screen_y1]
	ld [Screen_y], a
	call Screen_PutPixel
	; Binary clause core: GREATEREQUAL
	; Compare two vars optimization
	ld a,[Screen_dy]
	ld b,a
	ld a,[Screen_dx]
	cp b
	jp c, Screen_DrawLine_elseblock35
Screen_DrawLine_ConditionalTrueBlock34: ;Main true block ;keep :
	; generic assign 
	ld a,[Screen_dx]
	ld b,a
	ld a,[Screen_dy]
	sub  b
	ld [Screen_ai], a
	; generic assign 
	ld a,[Screen_dy]
	ld [Screen_bi], a
	; generic assign 
	; Generic 16-bit binop
	ld hl,$2
	ex de,hl
	; Variable is 16-bit
	ld a,[Screen_dx]
	ld l,a
	ld h,0
	ld a,h
	ld c,l
	call div_16x16
	ld h,a
	ld l,c
	ex de,hl
	; Variable is 16-bit
	ld a,[Screen_bi]
	ld l,a
	ld h,0
	xor a ; clear carry
	sbc hl,de
	ld a,l ; word assigned to byte
	ld [Screen_d], a
Screen_DrawLine_while88:
Screen_DrawLine_loopstart92:
	; Binary clause core: NOTEQUALS
	; Compare two vars optimization
	ld a,[Screen_x2]
	ld b,a
	ld a,[Screen_x1]
	cp b
	jr z, Screen_DrawLine_elsedoneblock91
Screen_DrawLine_ConditionalTrueBlock89: ;Main true block ;keep :
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[Screen_d]
	cp $80
	jr nc,Screen_DrawLine_elseblock106
Screen_DrawLine_ConditionalTrueBlock105: ;Main true block ;keep :
	; 'a:=a + expression'  optimization 
	ld a,[Screen_yi]
	ld b,a
	ld a,[Screen_y1]
	add  a, b
	ld [Screen_y1], a
	; 'a:=a + expression'  optimization 
	ld a,[Screen_ai]
	ld b,a
	ld a,[Screen_d]
	add  a, b
	ld [Screen_d], a
	jr Screen_DrawLine_elsedoneblock107
Screen_DrawLine_elseblock106:
	; 'a:=a + expression'  optimization 
	ld a,[Screen_bi]
	ld b,a
	ld a,[Screen_d]
	add  a, b
	ld [Screen_d], a
Screen_DrawLine_elsedoneblock107:
	; 'a:=a + expression'  optimization 
	ld a,[Screen_xi]
	ld b,a
	ld a,[Screen_x1]
	add  a, b
	ld [Screen_x1], a
	; generic assign 
	ld [Screen_x], a
	; generic assign 
	ld a,[Screen_y1]
	ld [Screen_y], a
	call Screen_PutPixel
	jr Screen_DrawLine_while88
Screen_DrawLine_elsedoneblock91:
Screen_DrawLine_loopend93:
	jp Screen_DrawLine_elsedoneblock36
Screen_DrawLine_elseblock35:
	
; // dy > dx
	; generic assign 
	ld a,[Screen_dy]
	ld b,a
	ld a,[Screen_dx]
	sub  b
	ld [Screen_ai], a
	; generic assign 
	ld a,[Screen_dx]
	ld [Screen_bi], a
	; generic assign 
	; Generic 16-bit binop
	ld hl,$2
	ex de,hl
	; Variable is 16-bit
	ld a,[Screen_dy]
	ld l,a
	ld h,0
	ld a,h
	ld c,l
	call div_16x16
	ld h,a
	ld l,c
	ex de,hl
	; Variable is 16-bit
	ld a,[Screen_bi]
	ld l,a
	ld h,0
	xor a ; clear carry
	sbc hl,de
	ld a,l ; word assigned to byte
	ld [Screen_d], a
Screen_DrawLine_while113:
Screen_DrawLine_loopstart117:
	; Binary clause core: NOTEQUALS
	; Compare two vars optimization
	ld a,[Screen_y2]
	ld b,a
	ld a,[Screen_y1]
	cp b
	jr z, Screen_DrawLine_elsedoneblock116
Screen_DrawLine_ConditionalTrueBlock114: ;Main true block ;keep :
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[Screen_d]
	cp $80
	jr nc,Screen_DrawLine_elseblock131
Screen_DrawLine_ConditionalTrueBlock130: ;Main true block ;keep :
	; 'a:=a + expression'  optimization 
	ld a,[Screen_xi]
	ld b,a
	ld a,[Screen_x1]
	add  a, b
	ld [Screen_x1], a
	; 'a:=a + expression'  optimization 
	ld a,[Screen_ai]
	ld b,a
	ld a,[Screen_d]
	add  a, b
	ld [Screen_d], a
	jr Screen_DrawLine_elsedoneblock132
Screen_DrawLine_elseblock131:
	; 'a:=a + expression'  optimization 
	ld a,[Screen_bi]
	ld b,a
	ld a,[Screen_d]
	add  a, b
	ld [Screen_d], a
Screen_DrawLine_elsedoneblock132:
	; 'a:=a + expression'  optimization 
	ld a,[Screen_yi]
	ld b,a
	ld a,[Screen_y1]
	add  a, b
	ld [Screen_y1], a
	; generic assign 
	ld a,[Screen_x1]
	ld [Screen_x], a
	; generic assign 
	ld a,[Screen_y1]
	ld [Screen_y], a
	call Screen_PutPixel
	jr Screen_DrawLine_while113
Screen_DrawLine_elsedoneblock116:
Screen_DrawLine_loopend118:
Screen_DrawLine_elsedoneblock36:
	ret
	; ***********  Defining procedure : Screen_DrawRect
	;    Procedure type : User-defined procedure
Screen_DrawRect_block137:
Screen_DrawRect:
	; generic assign 
	ld a,[Screen_w]
	ld b,a
	ld a,[Screen_rx]
	add  a, b
	ld [Screen_xi], a
	; generic assign 
	ld a,[Screen_h]
	ld b,a
	ld a,[Screen_ry]
	add  a, b
	ld [Screen_yi], a
	; ****** Inline assembler section
	ld a,[Screen_rx]
hloop	
	ld [Screen_x],a
	push af
	ld a,[Screen_ry]
	ld [Screen_y],a
	call Screen_PutPixel
	ld a,[Screen_yi]
	ld [Screen_y],a
	call Screen_PutPixel
	pop af
	inc a
	ld d,a
	ld a,[Screen_xi]
	cp d
	ld a,d
	jr nz,hloop
	ld a,[Screen_rx]
	ld [Screen_x],a	
	ld a,[Screen_ry]
vloop	
	ld [Screen_y],a
	push af
	ld a,[Screen_rx]
	ld [Screen_x],a
	call Screen_PutPixel
	ld a,[Screen_xi]
	ld [Screen_x],a
	call Screen_PutPixel
	pop af
	inc a
	ld d,a
	ld a,[Screen_yi]
	cp d
	ld a,d
	jr nz,vloop	
	ld [Screen_y],a
	call Screen_PutPixel
	ret
	; ***********  Defining procedure : Screen_DrawRectFilled
	;    Procedure type : User-defined procedure
Screen_DrawRectFilled_block138:
Screen_DrawRectFilled:
	; generic assign 
	ld a,[Screen_w]
	ld b,a
	ld a,[Screen_rx]
	add  a, b
	ld [Screen_xi], a
	; generic assign 
	ld a,[Screen_h]
	ld b,a
	ld a,[Screen_ry]
	add  a, b
	ld [Screen_yi], a
	; ****** Inline assembler section
	ld a, [Screen_ry]
y_loop	
	ld [Screen_y],a
	ld a, [Screen_rx]
x_loop	
	ld [Screen_x],a
	push af
	call Screen_PutPixel
  pop af
	inc a
	ld d,a
	ld a,[Screen_xi]
	cp d
	ld a,d
	jr nz,x_loop
	ld a,[Screen_y]
	inc a
	ld d,a
	ld a,[Screen_yi]
	cp d
	ld a,d
	jr nz,y_loop
	ret
	;*
; //	Wait for Vertical Blank
; //	*
	; ***********  Defining procedure : Screen_WaitForVerticalBlank
	;    Procedure type : User-defined procedure
Screen_WaitForVerticalBlank:
	; ****** Inline assembler section
vbloop 
	ld a,(#6800)
	rla
	jr nc,vbloop
vbloop2
	ld a,(#6800)
	rla
	jr c,vbloop2	
vbloop3 
	ld a,(#6800)
	rla
	jr nc,vbloop3	
	ret
	;*
; //	Look up value at location
; //	*
	; ***********  Defining procedure : Screen_Peek
	;    Procedure type : User-defined procedure
Screen_Peek_block140:
Screen_Peek:
	; ****** Inline assembler section
	ld hl,[Screen_Loc]
	ld a,(hl)
	ld [Screen_i],a
	ret
	;*
; //	Put value at location
; //	*
	; ***********  Defining procedure : Screen_Poke
	;    Procedure type : User-defined procedure
Screen_Poke_block141:
Screen_Poke:
	; ****** Inline assembler section
	ld a,[Screen_i]
	ld hl,[Screen_Loc]
	ld(hl),a
	ret
	;*
; //	1 pixel smooth scroll in mode(1)
; //	First parameter is right-most char onscreen of starting location
; //	Second parameter is the height of the scroll
; //	*
	; ***********  Defining procedure : Screen_doSmoothScroll
	;    Procedure type : User-defined procedure
Screen_doSmoothScroll_block142:
Screen_doSmoothScroll:
	; ****** Inline assembler section
  ld hl,[Screen_Loc]
	ld a,[Screen_h]
	ld c,a
nextline	
	xor a
  ld b,#20
doscroll
	rl(hl)
	rla
	rl(hl)
	rra
	dec hl
  djnz doscroll
	ld de,#0040
	add hl,de
	dec c
	jr nz,nextline
	ret
	;*
; //	Coarse scroll in mode(1)
; //	First parameter is left-most char onscreen of starting location
; //	Second parameter is the height of the scroll
; //	*
	; ***********  Defining procedure : Screen_doCoarseScroll
	;    Procedure type : User-defined procedure
Screen_doCoarseScroll_block143:
Screen_doCoarseScroll:
	; ****** Inline assembler section
  ld hl,[Screen_Loc]
	ld a,[Screen_h]
	ld b,a
cscroll	
	inc hl
	push bc
	push hl
	pop de
	dec de
	ld bc,#001f
	ldir
	dec hl
	ld a,#00
	ld(hl),a
	inc hl
	pop bc
	djnz cscroll
	
	ret
	;*
; //Turns on interrupts. Same as "ei"
; 

	;*
; //Turns off interrupts. Same as "di"
; 

	;*
; //Pushes all registers(both pairs) onto the stack. Typically used in interrupts.
; 

	;*
; //Pops all registers(both pairs) from the stack. Typically used in interrupts.
; 

	; ***********  Defining procedure : Compression_Decompress
	;    Procedure type : User-defined procedure
Compression_Decompress_block144:
Compression_Decompress:
	; ****** Inline assembler section
; HL - pointer to the buffer with compressed source data
; DE - pointer to the destination buffer for decompressed data
; BC - size of the compressed data
	ld hl,[Compression_in]
	ld de,[Compression_out]
	
	; ****** Inline assembler section
	
;
; LZ4 decompression algorithm - Copyright(c) 2011-2015, Yann Collet
; All rights reserved. 
; LZ4 implementation for z80 and compatible processors - Copyright(c) 2013-2015 Piotr Drapich
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, 
; are permitted provided that the following conditions are met: 
; 
; * Redistributions of source code must retain the above copyright notice, this 
;   list of conditions and the following disclaimer. 
; 
; * Redistributions in binary form must reproduce the above copyright notice, this 
;   list of conditions and the following disclaimer in the documentation and/or 
;   other materials provided with the distribution. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND 
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
;
; The latest version always available from http://www.union.org.pl/download/z80
; Questions, ideas, optimization suggestions, consulting requests? Send email to union@union.pl
; 
; Only 219 bytes of fully relocatable code with raw data decompression routine taking only 109 bytes.
; Supports both legacy and latest LZ4 data format without framing support.
; History:
; version 1.0(18.09.2013)
; - initial implementation for legacy compression formats, generated with lz4 1.3.3 or earlier
; version 1.1(28.02.2015)
; - added support for files, compressed with the lz4 version 1.41
; Available functions:
; LZ4_decompress
; - decompresses files, packed with lz4 command line tool, preferably with options -Sx -B4
; input parameters:
; HL - pointer to the buffer with compressed source data
; DE - pointer to the destination buffer for decompressed data
; on exit:
; A  - contains exit code: 
; 	0 - decompression successful
;	1 - compressed size is bigger than 64kb
;	2 - unsupported version of lz4 compression format
; HL - the number of decompressed bytes
; Contents of AF,BC,DE,HL are not preserved.
; LZ4_decompress_raw
; - decompresses raw LZ4 compressed data 
; input parameters:
; HL - pointer to the buffer with compressed source data
; DE - pointer to the destination buffer for decompressed data
; BC - size of the compressed data
; on exit:
; A  - exit code: 
; 	0 - decompression successful
; HL - the number of decompressed bytes
LZ4_decompress:
; check the magic number
	ld 		bc,0
	ld		a,(hl)
	cp 		#4
	jr 		z, LZ4_Version4
	cp 		#3
	jr 		z, LZ4_LegacyVersion3
	cp 		#2
	jr 		z, LZ4_LegacyVersion2
LZ4_version_not_supported:
	ld 		a,2
	jr		LZ4_decompress_finished
LZ4_decompress_error:
	ld 		a,1
LZ4_decompress_finished:
	ret
LZ4_Version4:
; check version 1.41 magic 
	inc		hl
	ld 		a,(hl)
	inc		hl
	cp		#$22
	jr		nz, LZ4_version_not_supported
	ld		a,(hl)
	inc		hl
	cp		#$4D
	jr		nz, LZ4_version_not_supported
	ld		a,(hl)
	inc		hl
	cp		#$18
	jr		nz, LZ4_version_not_supported
; parse version 1.41 spec header
	ld		a,(hl)
	inc		hl
; check version bits for version 01
	bit		7,a
	jr		nz, LZ4_version_not_supported
	bit		6,a
	jr		z, LZ4_version_not_supported
; is content size set?
	bit		3,a
	jr		z, LZ4_no_content_size
; skip content size
	ld		c,8
LZ4_no_content_size:
	bit		0,a
	jr		z, LZ4_no_preset_dictionary
; skip dictionary id
	ld		a,c
	add		a,4
	ld		c,a
LZ4_no_preset_dictionary:
	ld		a,(hl)
	inc		hl
; strip reserved bits(and #70) and check if block max size is set to 64kb(4)
	and		#$40
	jr		z, LZ4_version_not_supported
; skip header checksum
	inc		bc
	jr		LZ4_start_decompression
LZ4_LegacyVersion3:
	ld		c,8
LZ4_LegacyVersion2:
	inc		hl
	ld		a,(hl)
	inc		hl
	cp		#$21
	jr		nz, LZ4_version_not_supported
	ld		a,(hl)
	inc		hl
	cp		#$4c
	jr		nz, LZ4_version_not_supported
	ld		a,(hl)
	inc		hl
	cp		#$18
	jr		nz, LZ4_version_not_supported
LZ4_start_decompression:
	add		hl,bc
; load low 16 bit of compreesed block size to bc
	ld		c,(hl)
	inc		hl
	ld		b,(hl)
	inc		hl
; check if compressed size <64kb - high 16 bits of compressed size must be 0
	ld		a,(hl)
	cp		0
	jr		nz,LZ4_decompress_error
	inc		hl
	ld		a,(hl)
	cp		0
	jr		nz,LZ4_decompress_error
	inc		hl
; decompress raw lz4 data packet
; on entry hl - start of packed buffer, de - destination buffer, bc - size of packed data
LZ4_decompress_raw:
	push	de							; store original destination pointer
	push	hl							; store start of compressed data source
	add		hl,bc       				; calculate end address of compressed block
	ld		b,h							; move end address of compressed data to bc
	ld		c,l	
	pop		hl							; restore start of compressed data source
	push	bc							; store end address of compessed data
; now hl - start of packed buffer, de - destination, bc - end of packed buffer
	ld		b,0		         			; clear b, c is set later
; get decompression token
LZ4_GetToken:
	xor		a 							; reset c flag for sbc later
	ld		a,(hl)						; read token
	inc		hl
	push	af							; store token
; unpack 4 high bits to get the length of literal
	rlca
	rlca
	rlca
	rlca
; copy literals
	and		#$f							; token can be max 15 - mask out unimportant bits
	jr		z,LZ4_skipcalc   			; there is no literals, skip calculation of literal size
	ld		c,a							; set the count for calculation
	cp		#$f							; if literal size <15
	jr		nz, LZ4_copyliterals		; copy literal, else
; calculate total literal size by adding contents of following bytes
	push	de							; store destination
	ex		de,hl
; a = size of literal to copy, de=pointer to data to be added
	ld		h,0         				; set hl with size of literal to copy 
	ld		l,a
LZ4_calcloop:
	ld		a,(de)						; get additional literal size to add 
	inc		de
	ld		c,a							; set bc to the length of literal
	add		hl,bc						; add it to the total literal length
	cp		#$ff							; if literal=255
	jr		z,LZ4_calcloop				; continue calculating the total literal size
	ld		b,h							; store total literal size to copy in bc
	ld		c,l
	ex		de,hl						; hl now contains current compressed data pointer  
	pop		de							; restore destination to de 
LZ4_copyliterals:
	ldir								; copy literal to destination
LZ4_skipcalc:
; check for end of compressed data
	pop		af							; restore token, carry is cleared because of xor a at the beginning of GetToken
	pop		bc							; restore end address of compressed data 
	push	hl							; store current compressed data pointer 
	sbc		hl, bc						; check if we reached the end of compressed data buffer
	pop		hl							; restore current compressed data pointer
	jr		z,LZ4_decompress_success	; decompression finished
	push	bc							; store end address of compressed data
; Copy Matches
	and		#$f							; token can be max 15 - mask out unimportant bits. resets also c flag for sbc later
; get the offset
	ld		c,(hl)
	inc		hl
	ld		b,(hl)						; bc now contains the offset
	inc		hl
	push	hl							; store current compressed data pointer
	push	de							; store destination pointer
	ex		de,hl
	sbc		hl,bc   					; calculate from the offset the new decompressed data source to copy from
; hl contains new copy source, de source ptr
	ld		b,0     					; load bc with the token
	ld		c,a
	cp		#$f							; if matchlength <15
	jr nz, LZ4_copymatches				; copy matches. else 
; calculate total matchlength by adding additional bytes
	push	hl							; store current decompressed data source
; a = size of match to copy, de= pointer to data to be added
	ld		h,0     					; set hl with initial matchlength to copy
	ld		l,a
LZ4_calcloop2:
	ld		a,(de)						; get additional matchlength to add
	inc		de
	ld		c,a							; set bc to the matchlength
	add		hl,bc						; add it to the total match length
	cp		#$ff							; if matchlength=255
	jr		z,LZ4_calcloop2				; continue calculating the total match length		
	ld		b,h							; store total matchlength to copy in bc
	ld		c,l			 
	pop		hl							; restore current decompressed data source
	pop		af							; set stack to proper position by restoring destination pointer temporarily into af  
	ex		de,hl
	ex(sp),hl						; update current compressed data pointer on the stack to the new value from de
	ex		de,hl 
	push	af							; restore stack
LZ4_copymatches:
	pop		de							; restore destination pointer
	inc		bc							; add base length of 4 to get the correct size of matchlength 
	inc		bc
	inc		bc
	inc		bc
	ldir								; copy match
	pop		hl							; restore current compressed data source
	jr		LZ4_GetToken				; continue decompression
LZ4_decompress_success:
	pop		hl							; store destination pointer 
	sbc		hl,de						; calculate the number of decompressed bytes 
	xor		a							; clear exit code
	ret
	
	ret
	;*
; //	Sets the current font 
; //
; 

	; ***********  Defining procedure : Font_SetFont
	;    Procedure type : User-defined procedure
Font_SetFont_block145:
Font_SetFont:
	ret
	; ***********  Defining procedure : Font_Draw8x8Font
	;    Procedure type : User-defined procedure
Font_Draw8x8Font_block146:
Font_Draw8x8Font:
	; Generic assign 16-bit pointer
	; Generic 16-bit binop
	; Variable is 16-bit
	ld a,[Font_tx]
	ld l,a
	ld h,0
	ex de,hl
	push de
	; Generic 16-bit binop
	ld a,[Font_ty]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,Screen_tab32
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'Screen_tab32' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$7000
	add hl,de
	pop de
	add hl,de
	; Store 16-bit address
	ld [Font_scr],hl
Font_Draw8x8Font_while147:
Font_Draw8x8Font_loopstart151:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; Optimization : zp[0]
	ld hl,[Font_text]
	ld a,[hl]
	cp $0
	jp z, Font_Draw8x8Font_elsedoneblock150
Font_Draw8x8Font_ConditionalTrueBlock148: ;Main true block ;keep :
	; generic assign 
	; Optimization : zp[0]
	ld hl,[Font_text]
	ld a,[hl]
	ld [Font_i], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	cp $40
	jr c, Font_Draw8x8Font_elsedoneblock164
	jr z, Font_Draw8x8Font_elsedoneblock164
Font_Draw8x8Font_ConditionalTrueBlock162: ;Main true block ;keep :
	; Generic assign 16-bit pointer
	; Generic 16-bit binop
	ld b,$40
	ld a,[Font_i]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,Font_font8x8tab
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'Font_font8x8tab' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	; Variable is 16-bit
	ld hl,[Font_zp]
	add hl,de
	; Store 16-bit address
	ld [Font_src],hl
	; ****** Inline assembler section
			ld de,[Font_scr]			
			repeat 7
			ldi
			ldi
			ld bc,30
			add hl,bc
			ex de,hl
			add hl,bc
			ex de,hl
			repend
			
Font_Draw8x8Font_elsedoneblock164:
	; ;generic pointer/integer P:=P+(expr) add expression
	; RHS is pure 
	ld de,$2
	ld hl,[Font_scr]
	add  hl,de
	ld [Font_scr],hl
	; ;generic pointer/integer P:=P+(expr) add expression
	; RHS is pure 
	ld de,$1
	ld hl,[Font_text]
	add  hl,de
	ld [Font_text],hl
	jp Font_Draw8x8Font_while147
Font_Draw8x8Font_elsedoneblock150:
Font_Draw8x8Font_loopend152:
	ret
	; ***********  Defining procedure : Font_DrawTextAt
	;    Procedure type : User-defined procedure
Font_DrawTextAt_block167:
Font_DrawTextAt:
	; generic assign 
	; Generic 16-bit binop
	; Variable is 16-bit
	ld a,[Font_tx]
	ld l,a
	ld h,0
	ex de,hl
	push de
	; Generic 16-bit binop
	ld a,[Font_ty]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,Screen_tab32
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'Screen_tab32' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$7000
	add hl,de
	pop de
	add hl,de
	; Integer assignment 
	; Loading pointer
	ld [Font_tpos],hl
	; ****** Inline assembler section
    ld de,[Font_text]
    ld hl,[Font_tpos]
    push de         ; String to display, first char
    push hl         ; Location onscreen to display char
strngloop    
    ld a,(de)       ; Get string char
endloop    
    cp #00          ; End of string?
    jp z,enddraw    ; Yes, stop drawing.
    sub #20         ; No, get index of fontdata for char
    ld d,#00
    ld e,a
    ld h,#00
    ld l,a   
    add hl,de
    add hl,de
    add hl,de
    add hl,de
    push hl
    pop bc
    ld hl,[Font_currentFont]  ; Start of fontdata to draw
    add hl,bc       ; hl points to correct letter to draw
    push hl
    ld a,#05        ; Number of lines down to draw font letter
drawloop 
    ld(loopcnt),a
    ld a,(hl)
    push af         ; Save first byte to draw
    ld a,(#7846) 
    cp #00
    jp z,enddraw
    cp #01
    jr z,drawletter
    cp #02
    jr z,addblue
    pop af
    ld d,a
    sla a
    add a,d
    jr drawletter2  ; Font byte is coloured red
addblue
    xor a
    pop af
    rl a
    jr drawletter2
drawletter
    pop af
drawletter2    
    pop bc          ; Font data pointer          
    pop hl          ; Coords to display fontbyte
    ld d,a
    ld a,[Font_tran]
    cp #01
    jr z,trans      ; Draw transparent
    ld a,d
    jr drawtext
trans
    ld a,(hl)
    xor d
drawtext
    ld(hl),a       ; Put it there
    ld de,#0020
    add hl,de       ; Move screen coords pointer down 1 line
    inc bc
    push hl         ; Screen pointer
    push bc         ; Font data pointer
    ld l,c          ; hl = font pointer
    ld h,b
    ld a,(loopcnt)
    dec a
    cp #00
    jr nz,drawloop  ; Works to here
    pop bc
    pop hl
    ld bc,#009f
    xor a
    sbc hl,bc
    pop de          ; String pointer
    inc de
    push de         ; Next char
    push hl         ; Screen pointer   
    jp strngloop
loopcnt db $00
enddraw  
  pop hl
  pop hl  ; Clear the stack!
  
	ret
	
; // Defined joystick directions. Delete any not used
; // Defined keys. Delete any not used.*
; //  Reads the keyboard at this time and returns the value
; //  If the value is zero(0) no key was pressed
; //  *
	; ***********  Defining procedure : Input_ReadKey
	;    Procedure type : User-defined procedure
Input_ReadKey:
	; ****** Inline assembler section
    call #2ef4
    ld [Input_c],a
  
	ret
	;*
; //  Reads the keyboard at this time and returns the value
; //  Will loop until any key is pressed
; //  *
	; ***********  Defining procedure : Input_WaitChar
	;    Procedure type : User-defined procedure
Input_WaitChar:
	; ****** Inline assembler section
keyloop  
    call #2ef4
    cp #00
    jr z,keyloop
    ld [Input_c],a
  
	ret
	;*
; //  Get keyboard key pressed
; //  *
	; ***********  Defining procedure : Input_GetPressedKey
	;    Procedure type : User-defined procedure
Input_GetPressedKey:
	; ****** Inline assembler section
        
; Taken from MPAGD by Jonathan Cauldwell; VZ keyboard routine by Kees van Oss.
; Detect keypress.
; Note that each key causes a logic 0 to appear at the bit position shown, when its row address is read.
;       I/O Address -----------------------------------------------
;(Selector)  bit 7 bit 6 bit 5  bit 4  bit 3   bit 2  bit 1  bit 0
;row 0  0x68FE  	N/A   N/A   R      Q      E              W      T	1111 1110
;row 1  0x68FD  	N/A   N/A   F      A      D       CTRL   S      G	1111 1101
;row 2  0x68FB  	N/A   N/A   V      Z      C       SHIFT  X      B	1111 1011
;row 3  0x68F7  	N/A   N/A   4      1      3              2      5	1111 0111
;row 4  0x68EF  	N/A   N/A   M      SPACE  ,              .      N	1110 1111
;row 5  0x68DF  	N/A   N/A   7      0      8       -      9      6	1101 1111
;row 6  0x68BF  	N/A   N/A   U      P      I       RETURN O      Y	1011 1111
;row 7  0x687F  	N/A   N/A   J      ;      K       :      L      H	0111 1111
;
; If the '2' key were pressed, it would cause bit 1 at address 68F7H to drop to 0.
; The data retrieved by reading that address, neglecting the 2 most significant bits which are not driven by the keyboard, would be 3DH(binary 111101).
; Wait for keypress.
prskey	
	ld b,#01		    ; reset row
	ld hl,#68fe	    ; high byte of port to read.
; Check every row
prskey0
  ld a,l		      ; low byte
	rrca		        ; Adjust lb port address
	ld l,a
	ld a,(hl)	      ; read key
	and #3f
	cp #3f		      ; Key pressed?
	jr nz,prskey1	  ; Yes, exit
	inc b		        ; increment row counter
	ld a,b
	cp #09		      ; last row checked?
	jr nz,prskey0	  ; no, repeat
	ret     	      ; yes, no key pressed, check again
; Determine column
prskey1
  ld d,a
	ld c,1		      ; reset column
prskey2
  sra d		        ; rotate bit out
	jr nc,prskey4	  ; key pressed, exit
	inc c		        ; increment column counter
	ld a,c
	cp 7		        ; last column checked?
	jr nz,prskey2	  ; no, repeat
prskey3
  jr prskey	      ; yes, no key pressed, exit
; Key pressed, create keycode
prskey4	ld a,c		; high nibble=row
	sla a
	sla a
	sla a
	sla a
	add a,b		      ; low nibble=column
	push af
;debounce
;	call #2ef4
;	or a
;	jr nz,debounce
	pop af
  ld [Input_c],a
keyend
  
	ld a,[Input_c]
	ret
	;*
; //  Get joystick direction/fire pressed
; //  *
	; ***********  Defining procedure : Input_GetJoystick
	;    Procedure type : User-defined procedure
Input_GetJoystick_block171:
Input_GetJoystick:
	; ****** Inline assembler section
   
;--------------------------------------------------------
; Joystick
;
; Out: joyval=xx5FUDLR(bit cleared if key pressed)
;             ||||||||
;             |||||||+> Right      
;             ||||||+-> Left       
;             |||||+--> Down      
;             ||||+---> Up        
;             |||+----> Fire1     
;             ||+-----> Fire2    
;             |+------> Not used
;             +-------> Not used
;--------------------------------------------------------
; Joystick reading routines.
; The two Joystick units are connected to a plug-in module that
; contains I/O address decoding and switch matrix encoding.
; IC U2(74LS138) enables I/O reads between 20 - 2F Hex.
; Address lines AO - A3 are used separately to generate active LOW signals
; on the joystick or switch to be read.
; Switch state is then read at the resultant address from Data bits DO - D4.
; When a switch is ON it provides an active-low Data bit. 
;
; JOY1 0x2E    JOY2 0x2B
; U    0xFE    U    0xFE   1111 1110	
; D    0xFD    D    0xFD   1111 1101   
; L    0xFB    L    0xFB   1111 1011   
; R    0xF7    R    0xF7   1111 0111   
; FIRE 0xEF    FIRE 0xEF   1110 1111   
; 'Arm'0x2D(joy1 button 2)
; FIRE 0xEF                1110 1111   
; 'Arm'0x27(joy2 button 2)
;              FIRE 0xEF   1110 1111 
  ld a,[Input_c]      ; control flag.
  dec a               ; is it joystick 1?
  jr z,joy1           ; yes, read it.
  dec a               ; is it joystick 2?
  jr z,joy2           ; yes, read it.
  jr joyend
; Joystick 1.
joy1
  in a,(#2e)	             ; read joystick1
	call readjoy	           ; convert to joyval
	in a,(#2d)	             ; Read arm button joystick1
	jr readarm
; Joystick 2.
joy2
  in a,(#2b)	             ; read joystick2
	call readjoy	           ; convert to joyval
	in a,(#27)	             ; Read arm button joystick1
	jr readarm
readjoy
  ld b,#05		             ; read 5 bits from joystick
read0
  sra a	
	ccf		                   ; complement the result(0=not pressed,1=pressed).
	rl e
	djnz read0
	rrc e		                 ; convert VZ values to Kempston
	jr nc,rstfire
	set 4,e
	jr joyend
rstfire
  res 4,e
  jr joyend
readarm
  and #10		              ; read arm button
	jr z, joy1a
	res 5,e		              ; Not pressed, carry clear
	jr joy1b
joy1a
  set 5,e		              ; Pressed, carry set
joy1b
	set 6,e
	jr joy1d
joy1c
  res 6,e
joy1d
  ld a,e                   ; copy e register to accumulator.
  ld [Input_c],a            ; remember value.
joyend
	ld a,[Input_c]
	ret
	 
; //	Makes the VZ keyboard BEEP sound
; //	
	; ***********  Defining procedure : Sound_Beep
	;    Procedure type : User-defined procedure
Sound_Beep:
	; ****** Inline assembler section
  call #3450
	
	ret
	; ***********  Defining procedure : Sound_Play
	;    Procedure type : User-defined procedure
Sound_Play_block173:
Sound_Play:
	; ****** Inline assembler section
  ld hl,[Sound_freq]
  ld bc,[Sound_dur]
  call #345c
	
	ret
	; ***********  Defining procedure : Sound_Shoot
	;    Procedure type : User-defined procedure
Sound_Shoot_block174:
Sound_Shoot:
	; ****** Inline assembler section
	ld      hl,[Sound_freq] ;%000000011000011     ;450
expl
  push    hl
  push    af
  ld      a,#21
  ld      h,0
  and(hl)
  ld      l,a
  pop     af
  xor     l
	or      #08
	and     #ef
  ld(#6800),a
  pop     hl
  push    af
  ld      bc,[Sound_dur]
dly      
  dec     bc
  ld      a,b
  or      c
  jr      nz,dly
  pop     af
  inc     hl
  bit     5,l
  jr      z,expl
	
	ret
	 
; //  Makes explosion sound
; //  
	; ***********  Defining procedure : Sound_Explode
	;    Procedure type : User-defined procedure
Sound_Explode:
	; ****** Inline assembler section
	ld hl,450
expl0
  push    hl
  push    af
  ld      a,#21
  ld      h,0
  and(hl)
  ld      l,a
  pop     af
  xor     l
  or      #08
  and     #ef
  ld(#6800),a
  pop     hl
  push    af
  ld      b,h
  ld      c,l
xdly      dec     bc
  ld      a,b
  or      c
  jr      nz,xdly
  pop     af
  inc     hl
  bit     1,h
  jr      z,expl0
	ret
	;* 
; //  Gets a random integer number 0-65535
; //  *
	; ***********  Defining procedure : Functions_GetRnd
	;    Procedure type : User-defined procedure
Functions_GetRnd_block176:
Functions_GetRnd:
	; ****** Inline assembler section
    ld bc,#0000
    ld de,[Functions_i]
    call #09b4    ; Loads ACC with BCDE
    call #0a9d    ; Set flag to Integer
    call #14c9    ; RND routine. Using ACC, not A
    call #0a7f    ; CINT. Puts ACC into HL  
    ld a,h
    ld [Functions_h],a
    ld a,l
    ld [Functions_l],a
  
	; Generic 16-bit binop
	; Variable is 16-bit
	ld l,a
	ld h,0
	ex de,hl
	push de
	; Generic mul
	ld de,$100
	ld a,[Functions_h]
	ld hl,0
	ld c,0
	call mul_16x8
	pop de
	add hl,de
	ret
	;* 
; //  Converts an Integer to a byte
; //  *
	; ***********  Defining procedure : Functions_IntToByte
	;    Procedure type : User-defined procedure
Functions_IntToByte_block178:
Functions_IntToByte:
	; ****** Inline assembler section
    ld hl,[Functions_i]
    ld a,l
    ld [Functions_j],a
  
	ret
	;* 
; //  Converts a byte to an Integer
; //  *
	; ***********  Defining procedure : Functions_ByteToInt
	;    Procedure type : User-defined procedure
Functions_ByteToInt_block179:
Functions_ByteToInt:
	; ****** Inline assembler section
  ld h,#00
  ld a,[Functions_j]
  ld l,a
  ld [Functions_i],hl
	; Variable is 16-bit
	; Integer
	ret
	; ***********  Defining procedure : Functions_getRandFromArray
	;    Procedure type : User-defined procedure
Functions_getRandFromArray_block180:
Functions_getRandFromArray:
	; ****** Inline assembler section
    ld hl,[Functions_p]
    ld a,r
    ld b,#00
    ld c,a
    add hl,bc
    ld a,(hl)
    ld [Functions_i],a
  
	; Variable is 16-bit
	; Integer
	ld hl,[Functions_i]
	ret
	;*
; //  Converts an Integer number to a string
; //  Integer range: -32768 to +32767
; //  *
	; ***********  Defining procedure : Functions_IntegerToString
	;    Procedure type : User-defined procedure
Functions_IntegerToString_block181:
Functions_IntegerToString:
	; ****** Inline assembler section
    ld bc,#0000
    ld de,[Functions_i]
    call #09b4      ; Loads ACC with BCDE
    call #0a9d      ; Set flag to Integer 
    ld a,#02
    ld(#78af),a   
    call #0fbd      ; Convert ACC to ASCII string 0-delim HL points to string  
    ld [Functions_s],hl
  
	ret
	;*
; //  Converts a Byte number to a string
; //  *
	; ***********  Defining procedure : Functions_ByteToString
	;    Procedure type : User-defined procedure
Functions_ByteToString_block182:
Functions_ByteToString:
	; ****** Inline assembler section
    ld bc,#0000
    ld d,#00
    ld a,[Functions_j]
    ld e,a
    call #09b4      ; Loads ACC with BCDE
    call #0a9d      ; Set flag to Integer    
    call #0fbd      ; Convert ACC to ASCII string 0-delim HL points to string  
    inc hl
    ld [Functions_s],hl
  
	ret
	; ***********  Defining procedure : Sprite_SetSize
	;    Procedure type : User-defined procedure
Sprite_SetSize_block183:
Sprite_SetSize:
	; generic assign 
	; Generic mul
	ld a,[Sprite_spriteheight]
	ld e,a
	ld d,0
	ld a,[Sprite_spritewidth]
	ld h,a
	ld l,0
	call mul_8x8
	ld a,l
	ld [Sprite_spritedatasize], a
	ret
	;*
; //	Sets the sprite data
; //
; 

	; ***********  Defining procedure : Sprite_SetData
	;    Procedure type : User-defined procedure
Sprite_SetData_block185:
Sprite_SetData:
	ret
	; ***********  Defining procedure : Sprite_StampAt
	;    Procedure type : User-defined procedure
Sprite_StampAt_block186:
Sprite_StampAt:
	; Generic assign 16-bit pointer
	; Generic 16-bit binop
	; Variable is 16-bit
	ld a,[Sprite_spritex]
	ld l,a
	ld h,0
	ex de,hl
	push de
	; Generic 16-bit binop
	ld a,[Sprite_spritey]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,Screen_tab32
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'Screen_tab32' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$7000
	add hl,de
	pop de
	add hl,de
	; Store 16-bit address
	ld [Sprite_dst],hl
	ld a, $0
	ld [Sprite_i], a
Sprite_StampAt_forloop187:
	ld de,[Sprite_dst]
	ld hl,[Sprite_src]
	; Variable is 16-bit
	ld a,[Sprite_x]
	ld c,a
	ld b,0
	call z80_copy_mem
	; ;generic pointer/integer P:=P+(expr) add expression
	; RHS is pure 
	; Variable is 16-bit
	ld a,[Sprite_x]
	ld e,a
	ld d,0
	ld hl,[Sprite_src]
	add  hl,de
	ld [Sprite_src],hl
	; ;generic pointer/integer P:=P+(expr) add expression
	; RHS is pure 
	ld de,$20
	ld hl,[Sprite_dst]
	add  hl,de
	ld [Sprite_dst],hl
Sprite_StampAt_forloopcounter189:
Sprite_StampAt_loopstart190:
	ld a,[Sprite_y]
	ld c,a
	ld a,[Sprite_i]
	add a,1
	ld [Sprite_i],a
	cp c
	jr nz,Sprite_StampAt_forloop187
Sprite_StampAt_forloopend188:
Sprite_StampAt_loopend191:
	ret
	; ***********  Defining procedure : Sprite_DrawAt
	;    Procedure type : User-defined procedure
Sprite_DrawAt_block194:
Sprite_DrawAt:
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[Sprite_x]
	cp $1f
	jr c, Sprite_DrawAt_localfailed200
	jr z, Sprite_DrawAt_localfailed200
	jr Sprite_DrawAt_ConditionalTrueBlock196
Sprite_DrawAt_localfailed200: ;keep:
	; ; logical OR, second chance
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[Sprite_y]
	cp $3f
	jr c, Sprite_DrawAt_elsedoneblock198
	jr z, Sprite_DrawAt_elsedoneblock198
Sprite_DrawAt_ConditionalTrueBlock196: ;Main true block ;keep :
	
; // Exit if out of bounds
	ret
Sprite_DrawAt_elsedoneblock198:
	; generic assign 
	; Generic 16-bit binop
	; Variable is 16-bit
	ld a,[Sprite_spritex]
	ld l,a
	ld h,0
	ex de,hl
	push de
	; Generic 16-bit binop
	ld a,[Sprite_spritey]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,Screen_tab32
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'Screen_tab32' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$7000
	add hl,de
	pop de
	add hl,de
	; Integer assignment 
	; Loading pointer
	ld [Sprite_loc],hl
	; ****** Inline assembler section
  ld a,[Sprite_no]        ; a = spriteno
  ld c,a                  ; c = spriteno  **
  ld a,[Sprite_loc+1]
  ld d,a 
  ld a,[Sprite_loc]
  ld e,a                  ; de = sprite screen location
  push de
  ld a,[Sprite_spriteheight]
  ld [hcount],a            
  ld hl,[Sprite_spritedata]
  ld a,#00
  cp c
  jr z,displaysprite      ; if spriteno = 0, just display it
  ld a,[Sprite_spritedatasize]  ; sprite data amount per sprite                
  ld e,a 
  ld b,#00
sprite_mult:                
  add hl,bc
  dec e
  jr nz,sprite_mult       ; hl = start of spriteno spritedata 
displaysprite:
  pop bc                  ; bc = fox_asm_spriteat screen location
iterate_height:
  ld a,[Sprite_spritewidth]     ; sprite width
  ld d,a                  ; d = sprite width
iterate_width:
  ld a,(hl)
  ld e,a
  ld a,(bc)
  xor e
  ld(bc),a
  inc hl
  inc bc
  dec d
  jr nz,iterate_width     ; display all sprite data in x dimension
  push hl                 ; save hl(sprite data position) cos only hl can be used in 16bit adding
  ld hl,#0000
  add hl,bc               ; put screen location into hl
  ld de,#0020            
  add hl,de               ; add one screen line down
  ld a,[Sprite_spritewidth]     ; sprite width
  ld d,a                  ; d = sprite width
subtract_width:
  dec hl                  ; move hl=bc to start of new line
  dec d
  jr nz,subtract_width    
  push hl                 ; save screen location
  pop bc                  ; restore fox_spriteat screen location
  pop hl                  ; restore sprite data position
  ld a,[hcount]       ; sprite height counter
  dec a 
  ld [hcount],a
  jr nz,iterate_height
  ret
hcount
  db #00  
	ret
	; ***********  Defining procedure : IsBitSet
	;    Procedure type : User-defined procedure
IsBitSet_block202:
IsBitSet:
	ld a,[BitFlag]
	ld b,a
	ld a,[Value]
	and  b
	ret
	; ***********  Defining procedure : SetBit
	;    Procedure type : User-defined procedure
SetBit_block203:
SetBit:
	ld a,[BitFlag]
	ld b,a
	ld a,[Value]
	or  b
	ret
	; ***********  Defining procedure : ResetBit
	;    Procedure type : User-defined procedure
ResetBit_block204:
ResetBit:
	ld a,[BitFlag]
	ld b,a
	ld a,[Value]
	sub  b
	ret
	
; // Draw and manage Title Screen
	; ***********  Defining procedure : TitleScreen
	;    Procedure type : User-defined procedure
TitleScreen:
	ld hl,mychar
	ld [myp],hl
	ld hl,message
	ld [mym],hl
	ld a, $0
	ld [v], a
TitleScreen_while206:
TitleScreen_loopstart210:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	call Input_GetPressedKey
	cp $27
	jp z, TitleScreen_elsedoneblock209
TitleScreen_ConditionalTrueBlock207: ;Main true block ;keep :
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $0
	jr nz,TitleScreen_elsedoneblock248
TitleScreen_ConditionalTrueBlock246: ;Main true block ;keep :
	
; // Keep titlescreen going until S pressed
; // Draw new char in the onscreen scroll every 4 pixels
	ld a, $4
	ld [v], a
	ld a,[strpos]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,[mym]
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'mym' is word : 0
	push af
	ld hl,[myp]
	pop af
	ld [hl],a
	ld hl,mychar
	ld [Font_text],hl
	ld a, $1f
	ld [Font_tx], a
	ld a, $3a
	ld [Font_ty], a
	ld a, $0
	ld [Font_tran], a
	call Font_DrawTextAt
	; Binary clause core: LESS
	; Compare two vars optimization
	ld a,[strlen]
	ld b,a
	ld a,[strpos]
	cp b
	jr nc,TitleScreen_elseblock261
TitleScreen_ConditionalTrueBlock260: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[strpos]
	add  a,$1
	ld [strpos], a
	jr TitleScreen_elsedoneblock262
TitleScreen_elseblock261:
	ld a, $0
	ld [strpos], a
TitleScreen_elsedoneblock262:
TitleScreen_elsedoneblock248:
	ld a, $0
	ld [u], a
TitleScreen_forloop267:
	; Wait
	ld a,$32
TitleScreen_wait275:
	sub 1
	jr nz,TitleScreen_wait275
TitleScreen_forloopcounter269:
TitleScreen_loopstart270:
	ld a,[u]
	add a,1
	ld [u],a
	cp $c8
	jr nz,TitleScreen_forloop267
TitleScreen_forloopend268:
TitleScreen_loopend271:
	
; // Smooth scroll the message 1 pixel at a time
	; generic assign 
	ld hl,$775f
	; Integer assignment 
	; Loading pointer
	ld [Screen_Loc],hl
	ld a, $5
	ld [Screen_h], a
	call Screen_doSmoothScroll
	; 'a:=a + const'  optimization 
	ld a,[v]
	sub $1
	ld [v], a
	jp TitleScreen_while206
TitleScreen_elsedoneblock209:
TitleScreen_loopend211:
	ld a, $1
	ld [gameStats_gameStats_record_gameStats_record_gameRunning], a
	ret
	
; // Sound effects manager
	; ***********  Defining procedure : PlaySound
	;    Procedure type : User-defined procedure
PlaySound_block276:
PlaySound:
	ld a,[u]
	cp $1
	jr nz,PlaySound_casenext278
	
; // Game start sfx
	; generic assign 
	ld hl,$50
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	ld hl,$32
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
	; generic assign 
	ld hl,$32
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
	jp PlaySound_caseend277
PlaySound_casenext278:
	ld a,[u]
	cp $2
	jr nz,PlaySound_casenext280
	; generic assign 
	ld hl,$50
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	ld hl,$a
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
	; generic assign 
	ld hl,$64
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	ld hl,$a
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
	; generic assign 
	ld hl,$78
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	ld hl,$a
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
	jp PlaySound_caseend277
PlaySound_casenext280:
	ld a,[u]
	cp $3
	jr nz,PlaySound_casenext282
	; generic assign 
	; Generic mul
	; Variable is 16-bit
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld e,a
	ld d,0
	ld a,$5
	ld hl,0
	ld c,0
	call mul_16x8
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	ld hl,$3
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
	jp PlaySound_caseend277
PlaySound_casenext282:
	ld a,[u]
	cp $4
	jr nz,PlaySound_casenext285
	; generic assign 
	ld hl,$1c2
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	ld hl,$c8
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Shoot
	jp PlaySound_caseend277
PlaySound_casenext285:
	ld a,[u]
	cp $5
	jr nz,PlaySound_casenext287
	call Sound_Explode
PlaySound_casenext287:
PlaySound_caseend277:
	ret
	; ***********  Defining procedure : DrawPadded
	;    Procedure type : User-defined procedure
zscore:	dw	0
DrawPadded_block289:
DrawPadded:
	; generic assign 
	ld a,[x]
	ld [u], a
	; Binary clause core: LESS
	; Binary clause INTEGER: LESS
	ld hl,$1
	push hl
	; Variable is 16-bit
	; Integer
	ld hl,[zscore]
	pop de
	xor a ; clear carry
	sbc hl,de
	jr nc,DrawPadded_elsedoneblock293
DrawPadded_ConditionalTrueBlock291: ;Main true block ;keep :
	; Assigning a string : Font_text
	ld hl,DrawPadded_stringassignstr298
	; Loading pointer
	ld [Font_text],hl
	; generic assign 
	ld b,$6
	ld a,[u]
	add  a, b
	ld [Font_tx], a
	; generic assign 
	ld a,[y]
	ld [Font_ty], a
	ld a, $0
	ld [Font_tran], a
	call Font_DrawTextAt
	ret
DrawPadded_elsedoneblock293:
	; Binary clause core: LESS
	; Binary clause INTEGER: LESS
	ld hl,$2710
	push hl
	; Variable is 16-bit
	; Integer
	ld hl,[zscore]
	pop de
	xor a ; clear carry
	sbc hl,de
	jr nc,DrawPadded_elsedoneblock303
DrawPadded_ConditionalTrueBlock301: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock303:
	; Binary clause core: LESS
	; Binary clause INTEGER: LESS
	ld hl,$3e8
	push hl
	; Variable is 16-bit
	; Integer
	ld hl,[zscore]
	pop de
	xor a ; clear carry
	sbc hl,de
	jr nc,DrawPadded_elsedoneblock309
DrawPadded_ConditionalTrueBlock307: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock309:
	; Binary clause core: LESS
	; Binary clause INTEGER: LESS
	ld hl,$64
	push hl
	; Variable is 16-bit
	; Integer
	ld hl,[zscore]
	pop de
	xor a ; clear carry
	sbc hl,de
	jr nc,DrawPadded_elsedoneblock315
DrawPadded_ConditionalTrueBlock313: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock315:
	; Binary clause core: LESS
	; Binary clause INTEGER: LESS
	ld hl,$a
	push hl
	; Variable is 16-bit
	; Integer
	ld hl,[zscore]
	pop de
	xor a ; clear carry
	sbc hl,de
	jr nc,DrawPadded_elsedoneblock321
DrawPadded_ConditionalTrueBlock319: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock321:
	; generic assign 
	; Variable is 16-bit
	; Integer
	ld hl,[zscore]
	; Integer assignment 
	; Loading pointer
	ld [Functions_i],hl
	call Functions_IntegerToString
	ld hl,[Functions_s]
	ld [Font_text],hl
	; generic assign 
	ld a,[x]
	ld [Font_tx], a
	; generic assign 
	ld a,[y]
	ld [Font_ty], a
	ld a, $0
	ld [Font_tran], a
	call Font_DrawTextAt
	; Assigning a string : Font_text
	ld hl,DrawPadded_stringassignstr324
	; Loading pointer
	ld [Font_text],hl
	; generic assign 
	ld b,$6
	ld a,[u]
	add  a, b
	ld [Font_tx], a
	; generic assign 
	ld a,[y]
	ld [Font_ty], a
	ld a, $0
	ld [Font_tran], a
	call Font_DrawTextAt
	ret
	
; // Draw Headsup Display.
	; ***********  Defining procedure : DrawHUD
	;    Procedure type : User-defined procedure
DrawHUD:
	
; // Draw lives
	ld a, $3
	ld [Screen_c], a
	call Screen_SetPen
	; Assigning a string : Font_text
	ld hl,DrawHUD_stringassignstr327
	; Loading pointer
	ld [Font_text],hl
	ld a, $13
	ld [Font_tx], a
	ld a, $0
	ld [Font_ty], a
	ld [Font_tran], a
	call Font_DrawTextAt
	; Assigning a string : Font_text
	ld hl,DrawHUD_stringassignstr329
	; Loading pointer
	ld [Font_text],hl
	ld a, $0
	ld [Font_tx], a
	ld [Font_ty], a
	ld [Font_tran], a
	call Font_DrawTextAt
	; Assigning a string : Font_text
	ld hl,DrawHUD_stringassignstr331
	; Loading pointer
	ld [Font_text],hl
	ld a, $8
	ld [Font_tx], a
	ld a, $0
	ld [Font_ty], a
	ld [Font_tran], a
	call Font_DrawTextAt
	ld a, $1
	ld [Screen_c], a
	call Screen_SetPen
	; generic assign 
	; Variable is 16-bit
	; Integer
	ld hl,[gameStats_gameStats_record_gameStats_record_score]
	; Integer assignment 
	; Loading pointer
	ld [zscore],hl
	ld a, $18
	ld [x], a
	ld a, $0
	ld [y], a
	call DrawPadded
	; generic assign 
	; Variable is 16-bit
	; Integer
	ld hl,[gameStats_gameStats_record_gameStats_record_hiScore]
	; Integer assignment 
	; Loading pointer
	ld [zscore],hl
	ld a, $a
	ld [x], a
	ld a, $0
	ld [y], a
	call DrawPadded
	; generic assign 
	ld a,[gameStats_gameStats_record_gameStats_record_lives]
	ld [Functions_j], a
	call Functions_ByteToString
	ld hl,[Functions_s]
	ld [Font_text],hl
	ld a, $6
	ld [Font_tx], a
	ld a, $0
	ld [Font_ty], a
	ld [Font_tran], a
	call Font_DrawTextAt
	ret
	
; // Initialise stuff
	; ***********  Defining procedure : InitialiseGame
	;    Procedure type : User-defined procedure
InitialiseGame:
	ld a, $0
	ld [v], a
InitialiseGame_forloop334:
	
; // Scroll titlescreen off to the left
	; generic assign 
	ld hl,$7000
	; Integer assignment 
	; Loading pointer
	ld [Screen_Loc],hl
	ld a, $3f
	ld [Screen_h], a
	call Screen_doCoarseScroll
	ld a, $0
	ld [u], a
InitialiseGame_forloop350:
	; Wait
	ld a,$8c
InitialiseGame_wait358:
	sub 1
	jr nz,InitialiseGame_wait358
InitialiseGame_forloopcounter352:
InitialiseGame_loopstart353:
	ld a,[u]
	add a,1
	ld [u],a
	cp $1e
	jr nz,InitialiseGame_forloop350
InitialiseGame_forloopend351:
InitialiseGame_loopend354:
InitialiseGame_forloopcounter336:
InitialiseGame_loopstart337:
	ld a,[v]
	add a,1
	ld [v],a
	cp $20
	jr nz,InitialiseGame_forloop334
InitialiseGame_forloopend335:
InitialiseGame_loopend338:
	ld a, $0
	ld [u], a
InitialiseGame_forloop359:
	
; // Initialise grid
	; Storing to array
	ld a,[u]
	ld e,a
	ld d,0
	ld hl,gameGrid
	add hl,de
	ld a,$0
	ld [hl],a
InitialiseGame_forloopcounter361:
InitialiseGame_loopstart362:
	ld a,[u]
	add a,1
	ld [u],a
	cp $ff
	jr nz,InitialiseGame_forloop359
InitialiseGame_forloopend360:
InitialiseGame_loopend363:
	ld a, $0
	ld [u], a
InitialiseGame_forloop366:
	; Storing to array
	; Generic 16-bit binop
	ld hl,$100
	ex de,hl
	; Variable is 16-bit
	ld a,[u]
	ld l,a
	ld h,0
	add hl,de
	ex de,hl
	ld hl,gameGrid
	add hl,de
	ld a,$0
	ld [hl],a
InitialiseGame_forloopcounter368:
InitialiseGame_loopstart369:
	ld a,[u]
	add a,1
	ld [u],a
	cp $c1
	jr nz,InitialiseGame_forloop366
InitialiseGame_forloopend367:
InitialiseGame_loopend370:
	ld a, $0
	ld [u], a
InitialiseGame_forloop373:
	
; // Initialise millipede
	; Storing to array
	ld a,[u]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,$42
	ld [hl],a
	
; // Initial direction set to right and set to alive body segment.
	; Storing to array
	ld a,[u]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,$ff
	ld [hl],a
InitialiseGame_forloopcounter375:
InitialiseGame_loopstart376:
	ld a,[u]
	add a,1
	ld [u],a
	cp $c
	jr nz,InitialiseGame_forloop373
InitialiseGame_forloopend374:
InitialiseGame_loopend377:
	; Storing to array
	ld a,$52
	ld [milliSegments_milliSegments_record_milliSegments_record_flags+$0],a
	
; // Alive, direction right and head segment.
	ld a, $e
	ld [milliMoveSpeed], a
	
; // Initialise variables
	ld a, $3
	ld [gameStats_gameStats_record_gameStats_record_lives], a
	; generic assign 
	ld hl,$0
	; Integer assignment 
	; Loading pointer
	ld [gameStats_gameStats_record_gameStats_record_score],hl
	ld a, $1
	ld [gameStats_gameStats_record_gameStats_record_level], a
	ld a, $10
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
	ld a, $3a
	ld [playerSnake_playerSnake_record_playerSnake_record_y], a
	ld a, $3
	ld [playerSnake_playerSnake_record_playerSnake_record_moveCount], a
	ld a, $4
	ld [playerBullet_playerBullet_record_playerBullet_record_moveCount], a
	ld a, $0
	ld [playerBullet_playerBullet_record_playerBullet_record_fired], a
	ld [fleaEnemy_flea_record_flea_record_enabled], a
	ld [fleaEnemy_flea_record_flea_record_shotCount], a
	call DrawHUD
	
; // Draw player sprite
	ld hl,smallSprites
	ld [Sprite_spritedata],hl
	call Sprite_SetData
	ld a, $1
	ld [Sprite_spritewidth], a
	ld a, $4
	ld [Sprite_spriteheight], a
	call Sprite_SetSize
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [Sprite_spritey], a
	ld a, $a
	ld [Sprite_no], a
	call Sprite_DrawAt
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[gameStats_gameStats_record_gameStats_record_level]
	cp $1
	jr nz,InitialiseGame_elsedoneblock383
InitialiseGame_ConditionalTrueBlock381: ;Main true block ;keep :
	ret
InitialiseGame_elsedoneblock383:
	ret
	
; // Initialise the mushrooms in an array grid and display
	; ***********  Defining procedure : DrawGrid
	;    Procedure type : User-defined procedure
DrawGrid:
	ld hl,smallSprites
	ld [Sprite_spritedata],hl
	call Sprite_SetData
	ld a, $1
	ld [Sprite_spritewidth], a
	ld a, $4
	ld [Sprite_spriteheight], a
	call Sprite_SetSize
	ld a, $0
	ld [y], a
DrawGrid_forloop387:
	
; // Playfield grid array
	; generic assign 
	ld hl,gridRandoms
	ld [Functions_p],hl
	call Functions_getRandFromArray
	ld [x], a
	; generic assign 
	; Generic 16-bit binop
	; Variable is 16-bit
	ld l,a
	ld h,0
	ex de,hl
	push de
	ld a,[y]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,lookupGridY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupGridY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	pop de
	add hl,de
	; Integer assignment 
	; Loading pointer
	ld [tempint],hl
	; Storing to array
	; Variable is 16-bit
	; Integer
	ex de,hl
	ld hl,gameGrid
	add hl,de
	ld a,$1
	ld [hl],a
	; generic assign 
	ld a,[x]
	ld [Sprite_spritex], a
	; generic assign 
	; Generic 16-bit binop
	ld a,[y]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,lookupScreenY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupScreenY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$6
	add hl,de
	ld a,l ; word assigned to byte
	ld [Sprite_spritey], a
	ld a, $1
	ld [Sprite_no], a
	call Sprite_DrawAt
	; generic assign 
	ld hl,gridRandoms
	ld [Functions_p],hl
	call Functions_getRandFromArray
	ld [x], a
	; generic assign 
	; Generic 16-bit binop
	; Variable is 16-bit
	ld l,a
	ld h,0
	ex de,hl
	push de
	ld a,[y]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,lookupGridY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupGridY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	pop de
	add hl,de
	; Integer assignment 
	; Loading pointer
	ld [tempint],hl
	; Storing to array
	; Variable is 16-bit
	; Integer
	ex de,hl
	ld hl,gameGrid
	add hl,de
	ld a,$1
	ld [hl],a
	; generic assign 
	ld a,[x]
	ld [Sprite_spritex], a
	; generic assign 
	; Generic 16-bit binop
	ld a,[y]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,lookupScreenY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupScreenY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$6
	add hl,de
	ld a,l ; word assigned to byte
	ld [Sprite_spritey], a
	ld a, $1
	ld [Sprite_no], a
	call Sprite_DrawAt
DrawGrid_forloopcounter389:
DrawGrid_loopstart390:
	ld a,[y]
	add a,1
	ld [y],a
	cp $d
	jp nz,DrawGrid_forloop387
DrawGrid_forloopend388:
DrawGrid_loopend391:
	ret
	; ***********  Defining procedure : GetGridLocVal
	;    Procedure type : User-defined procedure
GetGridLocVal_block394:
GetGridLocVal:
	
; //w :=(w - screenYOffset) / 4 + 1;
	; generic assign 
	; Generic 16-bit binop
	; Variable is 16-bit
	ld a,[v]
	ld l,a
	ld h,0
	ex de,hl
	push de
	ld a,[w]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,lookupGridY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupGridY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	pop de
	add hl,de
	; Integer assignment 
	; Loading pointer
	ld [tempint],hl
	; Variable is 16-bit
	; Integer
	ex de,hl
	ld hl,gameGrid
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'gameGrid' is word : 0
	ret
	; ***********  Defining procedure : SetGridLocVal
	;    Procedure type : User-defined procedure
SetGridLocVal_block395:
SetGridLocVal:
	
; //w :=(w - screenYOffset) / 4 + 1;
	; generic assign 
	; Generic 16-bit binop
	; Variable is 16-bit
	ld a,[v]
	ld l,a
	ld h,0
	ex de,hl
	push de
	ld a,[w]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,lookupGridY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupGridY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	pop de
	add hl,de
	; Integer assignment 
	; Loading pointer
	ld [tempint],hl
	
; // Update grid value
	; Storing to array
	; Variable is 16-bit
	; Integer
	ex de,hl
	ld hl,gameGrid
	add hl,de
	ld a,[t]
	ld [hl],a
	ret
	
; // Update Lives HUD
	; ***********  Defining procedure : SubtractLife
	;    Procedure type : User-defined procedure
SubtractLife:
	; 'a:=a + const'  optimization 
	ld a,[gameStats_gameStats_record_gameStats_record_lives]
	sub $1
	ld [gameStats_gameStats_record_gameStats_record_lives], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	cp $0
	jr nz,SubtractLife_elsedoneblock400
SubtractLife_ConditionalTrueBlock398: ;Main true block ;keep :
	
; // Check if Game Over
; // Player dead. TODO
	ld a, $0
	ld [gameStats_gameStats_record_gameStats_record_gameRunning], a
SubtractLife_elsedoneblock400:
	call DrawHUD
	ret
	
; // Add points to player's score
	; ***********  Defining procedure : AddScore
	;    Procedure type : User-defined procedure
AddScore_block403:
AddScore:
	; 16 bit BINOP
	; RHS is pure 
	; Variable is 16-bit
	ld a,[t]
	ld e,a
	ld d,0
	; Variable is 16-bit
	; Integer
	ld hl,[gameStats_gameStats_record_gameStats_record_score]
	add  hl,de
	ld [gameStats_gameStats_record_gameStats_record_score],hl
	call DrawHUD
	ret
	; ***********  Defining procedure : DeleteSprite
	;    Procedure type : User-defined procedure
DeleteSprite_block404:
DeleteSprite:
	; generic assign 
	; Generic 16-bit binop
	; Variable is 16-bit
	ld a,[x]
	ld l,a
	ld h,0
	ex de,hl
	push de
	; Generic 16-bit binop
	ld a,[y]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,Screen_tab32
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'Screen_tab32' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$7000
	add hl,de
	pop de
	add hl,de
	; Integer assignment 
	; Loading pointer
	ld [tempint],hl
	; ****** Inline assembler section
	ld a,[tempint+1]
	ld h,a
	ld a,[tempint]
	ld l,a
	ld de,#0020
	ld(hl),#00
	add hl,de
	ld(hl),#00
	add hl,de
	ld(hl),#00
	add hl,de
	ld(hl),#00
	add hl,de	
	
	ret
	; ***********  Defining procedure : DrawBullet
	;    Procedure type : User-defined procedure
DrawBullet_block405:
DrawBullet:
	; generic assign 
	; Generic 16-bit binop
	; Variable is 16-bit
	ld a,[x]
	ld l,a
	ld h,0
	ex de,hl
	push de
	; Generic 16-bit binop
	ld a,[y]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,Screen_tab32
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'Screen_tab32' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$7000
	add hl,de
	pop de
	add hl,de
	; Integer assignment 
	; Loading pointer
	ld [tempint],hl
	; generic assign 
	; Variable is 16-bit
	; Integer
	; Integer assignment 
	; Loading pointer
	ld [Screen_Loc],hl
	ld a, $14
	ld [Screen_i], a
	call Screen_Poke
	ret
	; ***********  Defining procedure : DeleteBullet
	;    Procedure type : User-defined procedure
DeleteBullet_block406:
DeleteBullet:
	; generic assign 
	; Generic 16-bit binop
	; Variable is 16-bit
	ld a,[x]
	ld l,a
	ld h,0
	ex de,hl
	push de
	; Generic 16-bit binop
	ld a,[y]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,Screen_tab32
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'Screen_tab32' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$7000
	add hl,de
	pop de
	add hl,de
	; Integer assignment 
	; Loading pointer
	ld [tempint],hl
	; generic assign 
	; Variable is 16-bit
	; Integer
	; Integer assignment 
	; Loading pointer
	ld [Screen_Loc],hl
	ld a, $0
	ld [Screen_i], a
	call Screen_Poke
	ret
	
; // Get player input and move snake head
	; ***********  Defining procedure : ControlPlayer
	;    Procedure type : User-defined procedure
ControlPlayer:
	
; // Get player input
	; generic assign 
	ld a, $1
	ld [Input_c], a
	call Input_GetJoystick
	ld [u], a
	; generic assign 
	ld a, $2
	ld [Input_c], a
	call Input_GetJoystick
	ld [v], a
	; generic assign 
	call Input_GetPressedKey
	ld [w], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $40
	jr nz,ControlPlayer_elsedoneblock411
ControlPlayer_localsuccess413: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $40
	jr nz,ControlPlayer_elsedoneblock411
ControlPlayer_localsuccess414: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[w]
	cp $9
	jr nz,ControlPlayer_elsedoneblock411
ControlPlayer_ConditionalTrueBlock409: ;Main true block ;keep :
	
; // Test for player movement
	ret
ControlPlayer_elsedoneblock411:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $d0
	jr nz,ControlPlayer_localfailed427
	jr ControlPlayer_ConditionalTrueBlock417
ControlPlayer_localfailed427: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $d0
	jr nz,ControlPlayer_localfailed428
	jr ControlPlayer_ConditionalTrueBlock417
ControlPlayer_localfailed428: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[w]
	cp $54
	jr nz,ControlPlayer_elsedoneblock419
ControlPlayer_ConditionalTrueBlock417: ;Main true block ;keep :
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[playerBullet_playerBullet_record_playerBullet_record_fired]
	cp $0
	jr z, ControlPlayer_elsedoneblock433
ControlPlayer_ConditionalTrueBlock431: ;Main true block ;keep :
	
; // Player shooting
; // Test if a bullet already exists onscreen. Snake can only shoot once at a time
	ret
ControlPlayer_elsedoneblock433:
	ld a, $1
	ld [playerBullet_playerBullet_record_playerBullet_record_fired], a
	ld a, $2
	ld [u], a
	call PlaySound
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [playerBullet_playerBullet_record_playerBullet_record_x], a
	; generic assign 
	ld b,$1
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	sub  b
	ld [playerBullet_playerBullet_record_playerBullet_record_y], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [x], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	ld [y], a
	call DrawBullet
	ret
ControlPlayer_elsedoneblock419:
	
; // Erase player sprite
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [x], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [y], a
	call DeleteSprite
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $48
	jr nz,ControlPlayer_localfailed459
	jr ControlPlayer_ConditionalTrueBlock437
ControlPlayer_localfailed459: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $48
	jr nz,ControlPlayer_localfailed460
	jr ControlPlayer_ConditionalTrueBlock437
ControlPlayer_localfailed460: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[w]
	cp $58
	jr nz,ControlPlayer_elsedoneblock439
ControlPlayer_ConditionalTrueBlock437: ;Main true block ;keep :
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	cp $2a
	jr c, ControlPlayer_elsedoneblock465
	jr z, ControlPlayer_elsedoneblock465
ControlPlayer_ConditionalTrueBlock463: ;Main true block ;keep :
	
; // Check if player wants to move UP
; // Make sure we're not already at the top of the player area
; // Move player y pos
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	sub $2
	ld [playerSnake_playerSnake_record_playerSnake_record_y], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [v], a
	; generic assign 
	ld b,$6
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	call GetGridLocVal
	cp $0
	jr c, ControlPlayer_elsedoneblock477
	jr z, ControlPlayer_elsedoneblock477
ControlPlayer_ConditionalTrueBlock475: ;Main true block ;keep :
	
; // Test if we're colliding with a mushroom. If so, move player y back
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	add  a,$1
	ld [playerSnake_playerSnake_record_playerSnake_record_y], a
ControlPlayer_elsedoneblock477:
ControlPlayer_elsedoneblock465:
ControlPlayer_elsedoneblock439:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $44
	jr nz,ControlPlayer_localfailed507
	jr ControlPlayer_ConditionalTrueBlock481
ControlPlayer_localfailed507: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $44
	jr nz,ControlPlayer_localfailed508
	jr ControlPlayer_ConditionalTrueBlock481
ControlPlayer_localfailed508: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[w]
	cp $57
	jr nz,ControlPlayer_elsedoneblock483
ControlPlayer_ConditionalTrueBlock481: ;Main true block ;keep :
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	cp $3a
	jr nc,ControlPlayer_elsedoneblock513
ControlPlayer_ConditionalTrueBlock511: ;Main true block ;keep :
	
; // Due to player sprite being 4 pixels high we need to check 4 below the Y value.
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	add  a,$4
	ld [playerSnake_playerSnake_record_playerSnake_record_y], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [v], a
	; generic assign 
	ld b,$6
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	call GetGridLocVal
	cp $0
	jr c, ControlPlayer_elseblock526
	jr z, ControlPlayer_elseblock526
ControlPlayer_ConditionalTrueBlock525: ;Main true block ;keep :
	
; // If a mushroom is below the player, move the y back to where it was, otherwise move one pixel down
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	sub $4
	ld [playerSnake_playerSnake_record_playerSnake_record_y], a
	jr ControlPlayer_elsedoneblock527
ControlPlayer_elseblock526:
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	sub $3
	ld [playerSnake_playerSnake_record_playerSnake_record_y], a
ControlPlayer_elsedoneblock527:
ControlPlayer_elsedoneblock513:
ControlPlayer_elsedoneblock483:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $42
	jr nz,ControlPlayer_localfailed567
	jr ControlPlayer_ConditionalTrueBlock533
ControlPlayer_localfailed567: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $42
	jr nz,ControlPlayer_localfailed568
	jr ControlPlayer_ConditionalTrueBlock533
ControlPlayer_localfailed568: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[w]
	cp $64
	jr nz,ControlPlayer_elsedoneblock535
ControlPlayer_ConditionalTrueBlock533: ;Main true block ;keep :
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	cp $0
	jr c, ControlPlayer_elsedoneblock573
	jr z, ControlPlayer_elsedoneblock573
ControlPlayer_ConditionalTrueBlock571: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	sub $1
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	; generic assign 
	ld [v], a
	; generic assign 
	ld b,$6
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	call GetGridLocVal
	cp $0
	jr c, ControlPlayer_elsedoneblock591
	jr z, ControlPlayer_elsedoneblock591
ControlPlayer_ConditionalTrueBlock589: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	add  a,$1
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
ControlPlayer_elsedoneblock591:
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [v], a
	; generic assign 
	ld b,$3
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	add  a, b
	push af
	ld a,$6
	ld b,a
	pop af
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	call GetGridLocVal
	cp $0
	jr c, ControlPlayer_elsedoneblock597
	jr z, ControlPlayer_elsedoneblock597
ControlPlayer_ConditionalTrueBlock595: ;Main true block ;keep :
	
; // Test bottom of sprite
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	add  a,$1
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
ControlPlayer_elsedoneblock597:
ControlPlayer_elsedoneblock573:
ControlPlayer_elsedoneblock535:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $41
	jr nz,ControlPlayer_localfailed635
	jr ControlPlayer_ConditionalTrueBlock601
ControlPlayer_localfailed635: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $41
	jr nz,ControlPlayer_localfailed636
	jr ControlPlayer_ConditionalTrueBlock601
ControlPlayer_localfailed636: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[w]
	cp $44
	jr nz,ControlPlayer_elsedoneblock603
ControlPlayer_ConditionalTrueBlock601: ;Main true block ;keep :
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	cp $1f
	jr nc,ControlPlayer_elsedoneblock641
ControlPlayer_ConditionalTrueBlock639: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	add  a,$1
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	; generic assign 
	ld [v], a
	; generic assign 
	ld b,$6
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	call GetGridLocVal
	cp $0
	jr c, ControlPlayer_elsedoneblock659
	jr z, ControlPlayer_elsedoneblock659
ControlPlayer_ConditionalTrueBlock657: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	sub $1
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
ControlPlayer_elsedoneblock659:
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [v], a
	; generic assign 
	ld b,$3
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	add  a, b
	push af
	ld a,$6
	ld b,a
	pop af
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	call GetGridLocVal
	cp $0
	jr c, ControlPlayer_elsedoneblock665
	jr z, ControlPlayer_elsedoneblock665
ControlPlayer_ConditionalTrueBlock663: ;Main true block ;keep :
	
; // Test bottom of sprite
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	sub $1
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
ControlPlayer_elsedoneblock665:
ControlPlayer_elsedoneblock641:
ControlPlayer_elsedoneblock603:
	
; // Draw player sprite at new location
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [Sprite_spritey], a
	ld a, $a
	ld [Sprite_no], a
	call Sprite_DrawAt
	ret
	
; // Check if we need to put a flea on the screen
	; ***********  Defining procedure : DoFleaCheck
	;    Procedure type : User-defined procedure
DoFleaCheck:
	ld a, $0
	ld [t], a
	ld [u], a
DoFleaCheck_forloop669:
	; generic assign 
	; Generic 16-bit binop
	ld hl,$100
	ex de,hl
	; Variable is 16-bit
	ld a,[u]
	ld l,a
	ld h,0
	add hl,de
	ex de,hl
	ld hl,gameGrid
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'gameGrid' is word : 0
	ld [v], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	cp $0
	jr c, DoFleaCheck_elsedoneblock692
	jr z, DoFleaCheck_elsedoneblock692
DoFleaCheck_localsuccess694: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $5
	jr nc,DoFleaCheck_elsedoneblock692
DoFleaCheck_ConditionalTrueBlock690: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[t]
	add  a,$1
	ld [t], a
DoFleaCheck_elsedoneblock692:
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[t]
	cp $5
	jr c, DoFleaCheck_elsedoneblock699
	jr z, DoFleaCheck_elsedoneblock699
DoFleaCheck_ConditionalTrueBlock697: ;Main true block ;keep :
	ret
DoFleaCheck_elsedoneblock699:
DoFleaCheck_forloopcounter671:
DoFleaCheck_loopstart672:
	ld a,[u]
	add a,1
	ld [u],a
	cp $a0
	jr nz,DoFleaCheck_forloop669
DoFleaCheck_forloopend670:
DoFleaCheck_loopend673:
	ld a, $1
	ld [fleaEnemy_flea_record_flea_record_enabled], a
	; generic assign 
	ld hl,gridRandoms
	ld [Functions_p],hl
	call Functions_getRandFromArray
	ld [fleaEnemy_flea_record_flea_record_x], a
	ld a, $a
	ld [fleaEnemy_flea_record_flea_record_y], a
	ld a, $2
	ld [fleaEnemy_flea_record_flea_record_moveSpeed], a
	ld a, $0
	ld [fleaEnemy_flea_record_flea_record_shotCount], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	ld a, $9
	ld [Sprite_no], a
	call Sprite_DrawAt
	ret
	
; // Player fired
	; ***********  Defining procedure : MoveBullet
	;    Procedure type : User-defined procedure
MoveBullet:
	
; // Erase sprite
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [x], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	ld [y], a
	call DeleteBullet
	; 'a:=a + const'  optimization 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	sub $1
	ld [playerBullet_playerBullet_record_playerBullet_record_y], a
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	cp $7
	jr nc,MoveBullet_elsedoneblock706
MoveBullet_ConditionalTrueBlock704: ;Main true block ;keep :
	ld a, $0
	ld [playerBullet_playerBullet_record_playerBullet_record_fired], a
	ret
MoveBullet_elsedoneblock706:
	
; // Only check every 4 pixels moved
	; 'a:=a + const'  optimization 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_moveCount]
	sub $1
	ld [playerBullet_playerBullet_record_playerBullet_record_moveCount], a
	
; // Reset movement counter
	ld a, $4
	ld [playerBullet_playerBullet_record_playerBullet_record_moveCount], a
	
; // Find out any values in the grid at the bullet location
	; generic assign 
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [v], a
	; generic assign 
	ld b,$6
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	call GetGridLocVal
	ld [t], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	cp $0
	jp c, MoveBullet_elsedoneblock712
	jp z, MoveBullet_elsedoneblock712
MoveBullet_ConditionalTrueBlock710: ;Main true block ;keep :
	
; // Collided with something on the grid
; // Turn bullet off
	ld a, $0
	ld [playerBullet_playerBullet_record_playerBullet_record_fired], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[t]
	cp $0
	jp c, MoveBullet_elsedoneblock773
	jp z, MoveBullet_elsedoneblock773
MoveBullet_localsuccess799: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[t]
	cp $5
	jp nc,MoveBullet_elsedoneblock773
MoveBullet_ConditionalTrueBlock771: ;Main true block ;keep :
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[t]
	cp $5
	jp nc,MoveBullet_elsedoneblock804
MoveBullet_ConditionalTrueBlock802: ;Main true block ;keep :
	
; // Test for mushies
; // Mushy at this grid loc
	; generic assign 
	ld a,[v]
	ld [x], a
	; generic assign 
	; Generic 16-bit binop
	ld hl,$6
	ex de,hl
	push de
	ld a,[w]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,lookupScreenY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupScreenY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	pop de
	add hl,de
	ld a,l ; word assigned to byte
	ld [y], a
	call DeleteSprite
	; 'a:=a + const'  optimization 
	ld a,[t]
	add  a,$1
	ld [t], a
	
; // Update value in grid location
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [v], a
	; generic assign 
	ld b,$6
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	call SetGridLocVal
	
; // Draw new grid sprite
	; generic assign 
	ld a,[v]
	ld [Sprite_spritex], a
	; generic assign 
	; Generic 16-bit binop
	ld hl,$6
	ex de,hl
	push de
	ld a,[w]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,lookupScreenY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupScreenY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	pop de
	add hl,de
	ld a,l ; word assigned to byte
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[t]
	ld [Sprite_no], a
	call Sprite_DrawAt
MoveBullet_elsedoneblock804:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[t]
	cp $5
	jr nz,MoveBullet_elsedoneblock810
MoveBullet_ConditionalTrueBlock808: ;Main true block ;keep :
	
; // Delete mushy
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [v], a
	; generic assign 
	ld b,$6
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	ld a, $0
	ld [t], a
	call SetGridLocVal
	
; // Draw new grid sprite
	; generic assign 
	ld a,[v]
	ld [x], a
	; generic assign 
	; Generic 16-bit binop
	ld hl,$6
	ex de,hl
	push de
	ld a,[w]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,lookupScreenY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupScreenY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	pop de
	add hl,de
	ld a,l ; word assigned to byte
	ld [y], a
	call DeleteSprite
	ld a, $1
	ld [t], a
	call AddScore
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_enabled]
	cp $0
	jr nz,MoveBullet_elsedoneblock822
MoveBullet_ConditionalTrueBlock820: ;Main true block ;keep :
	
; // See if number of mushies in player area needs updating
	call DoFleaCheck
MoveBullet_elsedoneblock822:
MoveBullet_elsedoneblock810:
MoveBullet_elsedoneblock773:
MoveBullet_elsedoneblock712:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[playerBullet_playerBullet_record_playerBullet_record_fired]
	cp $0
	jr z, MoveBullet_elsedoneblock828
MoveBullet_ConditionalTrueBlock826: ;Main true block ;keep :
	
; // Draw sprite at new location onscreen
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [x], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	ld [y], a
	call DrawBullet
MoveBullet_elsedoneblock828:
	ret
	; ***********  Defining procedure : CheckFleaShot
	;    Procedure type : User-defined procedure
CheckFleaShot:
	; Binary clause core: NOTEQUALS
	; Compare two vars optimization
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld b,a
	ld a,[fleaEnemy_flea_record_flea_record_x]
	cp b
	jr z, CheckFleaShot_elsedoneblock835
CheckFleaShot_ConditionalTrueBlock833: ;Main true block ;keep :
	ret
CheckFleaShot_elsedoneblock835:
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	ld [t], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [u], a
	; generic assign 
	ld b,$5
	ld a,[fleaEnemy_flea_record_flea_record_y]
	add  a, b
	ld [v], a
	; Binary clause core: LESS
	; Compare two vars optimization
	ld b,a
	ld a,[t]
	cp b
	jr nc,CheckFleaShot_elsedoneblock841
CheckFleaShot_localsuccess849: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: GREATER
	; Compare two vars optimization
	ld a,[u]
	ld b,a
	ld a,[t]
	cp b
	jr c, CheckFleaShot_elsedoneblock841
	jr z, CheckFleaShot_elsedoneblock841
CheckFleaShot_ConditionalTrueBlock839: ;Main true block ;keep :
	
; // Flea shot by player
; // Erase bullet
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [x], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	ld [y], a
	call DeleteBullet
	ld a, $0
	ld [playerBullet_playerBullet_record_playerBullet_record_fired], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_shotCount]
	cp $0
	jr nz,CheckFleaShot_elsedoneblock854
CheckFleaShot_ConditionalTrueBlock852: ;Main true block ;keep :
	ld a, $1
	ld [fleaEnemy_flea_record_flea_record_shotCount], a
	ret
CheckFleaShot_elsedoneblock854:
	
; // Flea was already shot once
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	ld a, $9
	ld [Sprite_no], a
	call Sprite_DrawAt
	ld a, $14
	ld [t], a
	call AddScore
	ld a, $2
	ld [fleaEnemy_flea_record_flea_record_shotCount], a
	ld a, $0
	ld [fleaEnemy_flea_record_flea_record_animFrame], a
	ld [fleaEnemy_flea_record_flea_record_enabled], a
CheckFleaShot_elsedoneblock841:
	ret
	
; // Player death sequence
	; ***********  Defining procedure : DoPlayerDeathAnim
	;    Procedure type : User-defined procedure
DoPlayerDeathAnim:
	ld a, $1
	ld [v], a
DoPlayerDeathAnim_forloop858:
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[v]
	ld b,a
	ld a,$14
	add  a, b
	ld [Sprite_no], a
	call Sprite_DrawAt
	ld a, $5
	ld [u], a
	call PlaySound
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [x], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [y], a
	call DeleteSprite
DoPlayerDeathAnim_forloopcounter860:
DoPlayerDeathAnim_loopstart861:
	ld a,[v]
	add a,1
	ld [v],a
	cp $5
	jr nz,DoPlayerDeathAnim_forloop858
DoPlayerDeathAnim_forloopend859:
DoPlayerDeathAnim_loopend862:
	ld a, $10
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
	; generic assign 
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [Sprite_spritey], a
	ld a, $a
	ld [Sprite_no], a
	call Sprite_DrawAt
	ret
	
; // All enemy death sequences
	; ***********  Defining procedure : DoEnemyDeathAnim
	;    Procedure type : User-defined procedure
DoEnemyDeathAnim:
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_shotCount]
	cp $2
	jr nc,DoEnemyDeathAnim_elsedoneblock869
DoEnemyDeathAnim_ConditionalTrueBlock867: ;Main true block ;keep :
	
; // Test for flea death occurring
	ret
DoEnemyDeathAnim_elsedoneblock869:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_shotCount]
	cp $2
	jr nz,DoEnemyDeathAnim_elsedoneblock875
DoEnemyDeathAnim_ConditionalTrueBlock873: ;Main true block ;keep :
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_animFrame]
	cp $0
	jr nz,DoEnemyDeathAnim_elseblock894
DoEnemyDeathAnim_ConditionalTrueBlock893: ;Main true block ;keep :
	
; // Do flea stuff
; // Flea death - erase flea
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [x], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [y], a
	call DeleteSprite
	ld a, $1
	ld [fleaEnemy_flea_record_flea_record_animFrame], a
	ret
	jr DoEnemyDeathAnim_elsedoneblock895
DoEnemyDeathAnim_elseblock894:
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_animFrame]
	ld b,a
	ld a,$14
	add  a, b
	ld [Sprite_no], a
	call Sprite_DrawAt
DoEnemyDeathAnim_elsedoneblock895:
	ld a, $4
	ld [u], a
	call PlaySound
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_animFrame]
	ld b,a
	ld a,$14
	add  a, b
	ld [Sprite_no], a
	call Sprite_DrawAt
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_animFrame]
	cp $4
	jr nz,DoEnemyDeathAnim_elsedoneblock903
DoEnemyDeathAnim_ConditionalTrueBlock901: ;Main true block ;keep :
	ld a, $0
	ld [fleaEnemy_flea_record_flea_record_shotCount], a
	ld [fleaEnemy_flea_record_flea_record_y], a
	ret
DoEnemyDeathAnim_elsedoneblock903:
	; 'a:=a + const'  optimization 
	ld a,[fleaEnemy_flea_record_flea_record_animFrame]
	add  a,$1
	ld [fleaEnemy_flea_record_flea_record_animFrame], a
DoEnemyDeathAnim_elsedoneblock875:
	ret
	
; // Check if flea and player collided
	; ***********  Defining procedure : CheckFleaCollide
	;    Procedure type : User-defined procedure
CheckFleaCollide:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_enabled]
	cp $0
	jr nz,CheckFleaCollide_elsedoneblock910
CheckFleaCollide_ConditionalTrueBlock908: ;Main true block ;keep :
	
; // If not touching player, ignore
	ret
CheckFleaCollide_elsedoneblock910:
	; Binary clause core: NOTEQUALS
	; Compare two vars optimization
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld b,a
	ld a,[fleaEnemy_flea_record_flea_record_x]
	cp b
	jr z, CheckFleaCollide_elsedoneblock916
CheckFleaCollide_ConditionalTrueBlock914: ;Main true block ;keep :
	ret
CheckFleaCollide_elsedoneblock916:
	; Binary clause core: LESS
	; Compare two vars optimization
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld b,a
	ld a,[fleaEnemy_flea_record_flea_record_y]
	cp b
	jr nc,CheckFleaCollide_elsedoneblock922
CheckFleaCollide_ConditionalTrueBlock920: ;Main true block ;keep :
	ret
CheckFleaCollide_elsedoneblock922:
	
; // Passed the above, so flea and snake are touching
	ld a, $0
	ld [fleaEnemy_flea_record_flea_record_enabled], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [x], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [y], a
	call DeleteSprite
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [x], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [y], a
	call DeleteSprite
	ld a, $0
	ld [playerBullet_playerBullet_record_playerBullet_record_fired], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [x], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	ld [y], a
	call DeleteSprite
	call SubtractLife
	call DoPlayerDeathAnim
	ret
	
; // Move the flea down the screen, dropping mushies on the way
	; ***********  Defining procedure : MoveFlea
	;    Procedure type : User-defined procedure
MoveFlea:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_shotCount]
	cp $0
	jr nz,MoveFlea_elsedoneblock929
MoveFlea_ConditionalTrueBlock927: ;Main true block ;keep :
	
; // Speed control
	; 'a:=a + const'  optimization 
	ld a,[fleaEnemy_flea_record_flea_record_moveSpeed]
	sub $1
	ld [fleaEnemy_flea_record_flea_record_moveSpeed], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	cp $0
	jr nz,MoveFlea_elseblock942
MoveFlea_ConditionalTrueBlock941: ;Main true block ;keep :
	ld a, $2
	ld [fleaEnemy_flea_record_flea_record_moveSpeed], a
	jr MoveFlea_elsedoneblock943
MoveFlea_elseblock942:
	ret
MoveFlea_elsedoneblock943:
MoveFlea_elsedoneblock929:
	
; // Erase flea sprite
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	ld a, $9
	ld [Sprite_no], a
	call Sprite_DrawAt
	; 'a:=a + const'  optimization 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	add  a,$1
	ld [fleaEnemy_flea_record_flea_record_y], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	cp $3a
	jr c, MoveFlea_elsedoneblock951
	jr z, MoveFlea_elsedoneblock951
MoveFlea_ConditionalTrueBlock949: ;Main true block ;keep :
	ld a, $0
	ld [fleaEnemy_flea_record_flea_record_enabled], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [x], a
	; generic assign 
	ld b,$1
	ld a,[fleaEnemy_flea_record_flea_record_y]
	sub  b
	ld [y], a
	call DeleteSprite
	call DoFleaCheck
	ret
MoveFlea_elsedoneblock951:
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	ld a, $9
	ld [Sprite_no], a
	call Sprite_DrawAt
	; 'a:=a + const'  optimization 
	ld a,[fleaEnemy_flea_record_flea_record_dropCount]
	add  a,$1
	ld [fleaEnemy_flea_record_flea_record_dropCount], a
	ld a, $3
	ld [u], a
	call PlaySound
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_dropCount]
	cp $4
	jp nz,MoveFlea_elsedoneblock957
MoveFlea_ConditionalTrueBlock955: ;Main true block ;keep :
	
; // Only update every 4 pixels down
	ld a, $0
	ld [fleaEnemy_flea_record_flea_record_dropCount], a
	; generic assign 
	ld hl,gridRandoms
	ld [Functions_p],hl
	call Functions_getRandFromArray
	ld [t], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	cp $f
	jp c, MoveFlea_elsedoneblock981
	jp z, MoveFlea_elsedoneblock981
MoveFlea_ConditionalTrueBlock979: ;Main true block ;keep :
	; generic assign 
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [v], a
	; generic assign 
	ld b,$6
	ld a,[fleaEnemy_flea_record_flea_record_y]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	call GetGridLocVal
	ld [u], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	cp $0
	jr nz,MoveFlea_elsedoneblock993
MoveFlea_ConditionalTrueBlock991: ;Main true block ;keep :
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [v], a
	; generic assign 
	ld b,$4
	ld a,[fleaEnemy_flea_record_flea_record_y]
	sub  b
	push af
	ld a,$6
	ld b,a
	pop af
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	ld a, $1
	ld [t], a
	call SetGridLocVal
	
; // Draw new grid sprite
	; generic assign 
	ld a,[v]
	ld [Sprite_spritex], a
	; generic assign 
	; Generic 16-bit binop
	ld hl,$6
	ex de,hl
	push de
	ld a,[w]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,lookupScreenY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupScreenY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	pop de
	add hl,de
	ld a,l ; word assigned to byte
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[t]
	ld [Sprite_no], a
	call Sprite_DrawAt
MoveFlea_elsedoneblock993:
MoveFlea_elsedoneblock981:
MoveFlea_elsedoneblock957:
	ret
	
; // Drawing Millipede sprite
	; ***********  Defining procedure : DrawMilliSprite
	;    Procedure type : User-defined procedure
DrawMilliSprite:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $10
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jp z, DrawMilliSprite_elseblock999
DrawMilliSprite_ConditionalTrueBlock998: ;Main true block ;keep :
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $2
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jr z, DrawMilliSprite_elsedoneblock1031
DrawMilliSprite_ConditionalTrueBlock1029: ;Main true block ;keep :
	
; // Draw Millipede head sprite
	ld a, $d
	ld [v], a
DrawMilliSprite_elsedoneblock1031:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $1
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jr z, DrawMilliSprite_elsedoneblock1037
DrawMilliSprite_ConditionalTrueBlock1035: ;Main true block ;keep :
	ld a, $f
	ld [v], a
DrawMilliSprite_elsedoneblock1037:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $4
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jr z, DrawMilliSprite_elsedoneblock1043
DrawMilliSprite_ConditionalTrueBlock1041: ;Main true block ;keep :
	ld a, $10
	ld [v], a
DrawMilliSprite_elsedoneblock1043:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $8
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jr z, DrawMilliSprite_elsedoneblock1049
DrawMilliSprite_ConditionalTrueBlock1047: ;Main true block ;keep :
	ld a, $e
	ld [v], a
DrawMilliSprite_elsedoneblock1049:
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	ld [Sprite_spritex], a
	; generic assign 
	; Generic 16-bit binop
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	ld e,a ; variable is 8-bit
	ld hl,lookupScreenY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupScreenY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$6
	add hl,de
	ld a,l ; word assigned to byte
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[v]
	ld [Sprite_no], a
	call Sprite_DrawAt
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	ld [v], a
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	ld [w], a
	ld a, $6
	ld [t], a
	call SetGridLocVal
	jp DrawMilliSprite_elsedoneblock1000
DrawMilliSprite_elseblock999:
	
; // Millipede body
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	ld [Sprite_spritex], a
	; generic assign 
	; Generic 16-bit binop
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	ld e,a ; variable is 8-bit
	ld hl,lookupScreenY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupScreenY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$6
	add hl,de
	ld a,l ; word assigned to byte
	ld [Sprite_spritey], a
	ld a, $c
	ld [Sprite_no], a
	call Sprite_DrawAt
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	ld [v], a
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	ld [w], a
	ld a, $5
	ld [t], a
	call SetGridLocVal
DrawMilliSprite_elsedoneblock1000:
	ret
	
; // Move Millipede
	; ***********  Defining procedure : MoveMillipede
	;    Procedure type : User-defined procedure
MoveMillipede:
	ld a, $0
	ld [varPrefixed_p], a
	ld [s], a
MoveMillipede_forloop1054:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $40
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jp z, MoveMillipede_elsedoneblock1562
MoveMillipede_ConditionalTrueBlock1560: ;Main true block ;keep :
	
; // End moving right
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	cp $ff
	jp nz,MoveMillipede_elseblock1813
MoveMillipede_ConditionalTrueBlock1812: ;Main true block ;keep :
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[varPrefixed_p]
	cp $0
	jp nz,MoveMillipede_elsedoneblock1918
MoveMillipede_ConditionalTrueBlock1916: ;Main true block ;keep :
	
; // Keep track if segment already moved onto screen
; // Test if segment is not on grid
	; Storing to array
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,$0
	ld [hl],a
	; Storing to array
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,$10
	ld [hl],a
	ld a, $1
	ld [varPrefixed_p], a
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $10
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jp z, MoveMillipede_elseblock1931
MoveMillipede_ConditionalTrueBlock1930: ;Main true block ;keep :
	ld a, $6
	ld [v], a
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	ld [Sprite_spritex], a
	; generic assign 
	; Generic 16-bit binop
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	ld e,a ; variable is 8-bit
	ld hl,lookupScreenY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupScreenY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$6
	add hl,de
	ld a,l ; word assigned to byte
	ld [Sprite_spritey], a
	ld a, $d
	ld [Sprite_no], a
	call Sprite_DrawAt
	jp MoveMillipede_elsedoneblock1932
MoveMillipede_elseblock1931:
	ld a, $5
	ld [v], a
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	ld [Sprite_spritex], a
	; generic assign 
	; Generic 16-bit binop
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	ld e,a ; variable is 8-bit
	ld hl,lookupScreenY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupScreenY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$6
	add hl,de
	ld a,l ; word assigned to byte
	ld [Sprite_spritey], a
	ld a, $c
	ld [Sprite_no], a
	call Sprite_DrawAt
MoveMillipede_elsedoneblock1932:
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	ld [v], a
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	ld [w], a
	; generic assign 
	ld a,[v]
	ld [t], a
	call SetGridLocVal
MoveMillipede_elsedoneblock1918:
	jp MoveMillipede_elsedoneblock1814
MoveMillipede_elseblock1813:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $8
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jr nz,MoveMillipede_elsedoneblock1941
MoveMillipede_localsuccess1943: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $4
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jr nz,MoveMillipede_elsedoneblock1941
MoveMillipede_ConditionalTrueBlock1939: ;Main true block ;keep :
	
; // Segment is already on the grid
; // Delete sprite
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	ld [x], a
	; generic assign 
	; Generic 16-bit binop
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	ld e,a ; variable is 8-bit
	ld hl,lookupScreenY
	add hl,de
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'lookupScreenY' is word : 1
	ld e,a
	inc hl
	ld a,[hl]
	ld d,a
	ex de,hl
	ex de,hl
	ld hl,$6
	add hl,de
	ld a,l ; word assigned to byte
	ld [y], a
	call DeleteSprite
MoveMillipede_elsedoneblock1941:
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	ld [v], a
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	ld [w], a
	ld a, $0
	ld [t], a
	call SetGridLocVal
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $2
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jp z, MoveMillipede_elsedoneblock1948
MoveMillipede_ConditionalTrueBlock1946: ;Main true block ;keep :
	
; // Move millipede right?
	; generic assign 
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	push af
	ld a,$1
	ld b,a
	pop af
	add  a, b
	ld [v], a
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	ld [w], a
	call GetGridLocVal
	ld [q], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	cp $1f
	jr nz,MoveMillipede_elsedoneblock1968
MoveMillipede_ConditionalTrueBlock1966: ;Main true block ;keep :
	ld a, $1
	ld [q], a
MoveMillipede_elsedoneblock1968:
	
; // Set Left direction
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[q]
	cp $0
	jp nz,MoveMillipede_elseblock1973
MoveMillipede_ConditionalTrueBlock1972: ;Main true block ;keep :
	
; // A hack for edge of screen.
; // Will we collide with a mushroom on the grid or screen edge?
; // Move one right
	call DrawMilliSprite
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	push af
	ld a,$1
	ld b,a
	pop af
	add  a, b
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $8
	ld [BitFlag], a
	call ResetBit
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
	jp MoveMillipede_elsedoneblock1974
MoveMillipede_elseblock1973:
	call DrawMilliSprite
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $8
	ld [BitFlag], a
	call SetBit
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
	
; // Set going down
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $2
	ld [BitFlag], a
	call ResetBit
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
	
; // Disable going right
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $1
	ld [BitFlag], a
	call SetBit
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
MoveMillipede_elsedoneblock1974:
MoveMillipede_elsedoneblock1948:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $1
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jp z, MoveMillipede_elsedoneblock1982
MoveMillipede_ConditionalTrueBlock1980: ;Main true block ;keep :
	
; // End moving right
; // Move millipede left?
	; generic assign 
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	push af
	ld a,$1
	ld b,a
	pop af
	sub  b
	ld [v], a
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	ld [w], a
	call GetGridLocVal
	ld [q], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	cp $0
	jr nz,MoveMillipede_elsedoneblock2002
MoveMillipede_ConditionalTrueBlock2000: ;Main true block ;keep :
	ld a, $1
	ld [q], a
MoveMillipede_elsedoneblock2002:
	
; // Set Left direction
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[q]
	cp $0
	jp nz,MoveMillipede_elseblock2007
MoveMillipede_ConditionalTrueBlock2006: ;Main true block ;keep :
	
; // A hack for edge of screen.
; // Will we collide with a mushroom on the grid or screen edge?
; // Move one right
	call DrawMilliSprite
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $4
	ld [BitFlag], a
	call ResetBit
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
	jp MoveMillipede_elsedoneblock2008
MoveMillipede_elseblock2007:
	call DrawMilliSprite
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $8
	ld [BitFlag], a
	call SetBit
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
	
; // Set going down
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $2
	ld [BitFlag], a
	call ResetBit
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
	
; // Disable going right
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $1
	ld [BitFlag], a
	call SetBit
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
MoveMillipede_elsedoneblock2008:
MoveMillipede_elsedoneblock1982:
MoveMillipede_elsedoneblock1814:
	
; // Bottom of screen
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $8
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jr z, MoveMillipede_elsedoneblock2016
MoveMillipede_ConditionalTrueBlock2014: ;Main true block ;keep :
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	cp $3a
	jr nz,MoveMillipede_elseblock2029
MoveMillipede_ConditionalTrueBlock2028: ;Main true block ;keep :
	
; // Move millipede down?
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $4
	ld [BitFlag], a
	call SetBit
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
	jr MoveMillipede_elsedoneblock2030
MoveMillipede_elseblock2029:
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	push af
	ld a,$1
	ld b,a
	pop af
	add  a, b
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	pop af
	ld [hl],a
MoveMillipede_elsedoneblock2030:
MoveMillipede_elsedoneblock2016:
	
; // Top of player area
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $4
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jr z, MoveMillipede_elsedoneblock2038
MoveMillipede_ConditionalTrueBlock2036: ;Main true block ;keep :
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	cp $2a
	jr nz,MoveMillipede_elseblock2051
MoveMillipede_ConditionalTrueBlock2050: ;Main true block ;keep :
	
; // End moving down
; // Move millipede up?
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $8
	ld [BitFlag], a
	call SetBit
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
	jr MoveMillipede_elsedoneblock2052
MoveMillipede_elseblock2051:
MoveMillipede_elsedoneblock2052:
MoveMillipede_elsedoneblock2038:
MoveMillipede_elsedoneblock1562:
	
; // End moving up			
	call DrawMilliSprite
MoveMillipede_forloopcounter1056:
MoveMillipede_loopstart1057:
	ld a,[s]
	add a,1
	ld [s],a
	cp $c
	jp nz,MoveMillipede_forloop1054
MoveMillipede_forloopend1055:
MoveMillipede_loopend1058:
	ret
block1:
	
; // Actual code start
	; generic assign 
	ld hl,$0
	; Integer assignment 
	; Loading pointer
	ld [gameStats_gameStats_record_gameStats_record_hiScore],hl
	; ****** Inline assembler section
 di
	
; // Set Graphics mode
	ld a, $1
	ld [Screen_ga], a
	call Screen_SetMode
	
; // Decompress the image to the screen
	ld hl,data
	ld [Compression_in],hl
	ld hl,$7000
	ld [Compression_out],hl
	call Compression_Decompress
	
; // Set pen colour
	ld a, $1
	ld [Screen_c], a
	call Screen_SetPen
	ld hl,Font_font1
	ld [Font_currentFont],hl
	call Font_SetFont
	
; // Do titlescreen stuff
	call TitleScreen
	ld a, $1
	ld [u], a
	call PlaySound
	call InitialiseGame
	call DrawGrid
MainProgram_while2058:
MainProgram_loopstart2062:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[gameStats_gameStats_record_gameStats_record_gameRunning]
	cp $0
	jr z, MainProgram_elsedoneblock2061
MainProgram_ConditionalTrueBlock2059: ;Main true block ;keep :
	
; // Keep game going until gameRunning is false(Game over)
; // Sync everything so the speed is correct
	call Screen_WaitForVerticalBlank
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_moveCount]
	sub $1
	ld [playerSnake_playerSnake_record_playerSnake_record_moveCount], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	cp $0
	jr nz,MainProgram_elsedoneblock2129
MainProgram_ConditionalTrueBlock2127: ;Main true block ;keep :
	ld a, $3
	ld [playerSnake_playerSnake_record_playerSnake_record_moveCount], a
	call ControlPlayer
MainProgram_elsedoneblock2129:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[playerBullet_playerBullet_record_playerBullet_record_fired]
	cp $0
	jr z, MainProgram_elsedoneblock2135
MainProgram_ConditionalTrueBlock2133: ;Main true block ;keep :
	call MoveBullet
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_enabled]
	cp $0
	jr z, MainProgram_elsedoneblock2147
MainProgram_ConditionalTrueBlock2145: ;Main true block ;keep :
	call CheckFleaShot
MainProgram_elsedoneblock2147:
	call MoveBullet
MainProgram_elsedoneblock2135:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_enabled]
	cp $0
	jr z, MainProgram_elsedoneblock2153
MainProgram_ConditionalTrueBlock2151: ;Main true block ;keep :
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_shotCount]
	cp $2
	jr nc,MainProgram_elsedoneblock2171
MainProgram_ConditionalTrueBlock2169: ;Main true block ;keep :
	
; // Only move flea if it's not dead.
	call MoveFlea
MainProgram_elsedoneblock2171:
	
; // Check if flea collided with player
	call CheckFleaCollide
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[playerBullet_playerBullet_record_playerBullet_record_fired]
	cp $0
	jr z, MainProgram_elsedoneblock2177
MainProgram_ConditionalTrueBlock2175: ;Main true block ;keep :
	
; // Check if flea was shot
	call CheckFleaShot
MainProgram_elsedoneblock2177:
MainProgram_elsedoneblock2153:
	
; // Move Millipede
	; 'a:=a + const'  optimization 
	ld a,[milliMoveSpeed]
	sub $1
	ld [milliMoveSpeed], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	cp $0
	jr nz,MainProgram_elsedoneblock2183
MainProgram_ConditionalTrueBlock2181: ;Main true block ;keep :
	ld a, $e
	ld [milliMoveSpeed], a
	call MoveMillipede
MainProgram_elsedoneblock2183:
	
; // Test if any enemies shot and if so, animate sprite
	call DoEnemyDeathAnim
	jr MainProgram_while2058
MainProgram_elsedoneblock2061:
MainProgram_loopend2063:
MainProgram_end2186:
	;nop
	jr MainProgram_end2186
; Copy BC bytes from HL to DE.
z80_copy_mem:
    ld      a,b
    or      c
    ret     z
    ld      a,16
    sub     c
    and     15
    add     a,a
    ld      (z80_copy_lp-1),a
    jr      z80_copy_lp ; will be overwritten
z80_copy_lp:
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    ldi
    jp      pe,z80_copy_lp
    ret
mul_8x8:
    sla h  ; optimised 1st iteration
    jr nc,mul_8x8_s1
    ld l,e
mul_8x8_s1:
    add hl,hl
    jr nc,mul_8x8_s2
    add hl,de
mul_8x8_s2:
    add hl,hl
    jr nc,mul_8x8_s3
    add hl,de
mul_8x8_s3:
    add hl,hl
    jr nc,mul_8x8_s4
    add hl,de
mul_8x8_s4:
    add hl,hl
    jr nc,mul_8x8_s5
    add hl,de
mul_8x8_s5:
    add hl,hl
    jr nc,mul_8x8_s6
    add hl,de
mul_8x8_s6:
    add hl,hl
    jr nc,mul_8x8_s7
    add hl,de
mul_8x8_s7:
    add hl,hl
    jr nc,mul_8x8_s8
    add hl,de
mul_8x8_s8:
    ret
mul_16x8:
    add a,a  ; optimised 1st iteration
    jr nc,mul_16x8_s1
    ld h,d
    ld l,e
mul_16x8_s1:
    add hl,hl
    rla
    jr nc,mul_16x8_s2
    add hl,de
    adc a,c
mul_16x8_s2:
    add hl,hl
    rla
    jr nc,mul_16x8_s3
    add hl,de
    adc a,c
mul_16x8_s3:
    add hl,hl
    rla
    jr nc,mul_16x8_s4
    add hl,de
    adc a,c
mul_16x8_s4:
    add hl,hl
    rla
    jr nc,mul_16x8_s5
    add hl,de
    adc a,c
mul_16x8_s5:
    add hl,hl
    rla
    jr nc,mul_16x8_s6
    add hl,de
    adc a,c
mul_16x8_s6:
    add hl,hl
    rla
    jr nc,mul_16x8_s7
    add hl,de
    adc a,c
mul_16x8_s7:
    add hl,hl
    rla
    jr nc,mul_16x8_s8
    add hl,de
    adc a,c
mul_16x8_s8:
    ret
div_16x8
;div_hl_c:
   xor	a
   ld	b, 16
_loop:
   add	hl, hl
   rla
   jr	c, $+5
   cp	c
   jr	c, $+4
   sub	c
   inc	l
   djnz	_loop
   ret
div_16x16:
   ld	hl, 0
   ld	b, 16
_loop16:
   sll	c
   rla
   adc	hl, hl
   sbc	hl, de
   jr	nc, $+4
   add	hl, de
   dec	c
   djnz	_loop16
   ret
_Mod_16:
    ld hl,0
    ld a,b
    ld b,8
_Mod_Loop1:
    rla
    adc hl,hl
    sbc hl,de
    jr nc,_Mod_NoAdd1
    add hl,de
_Mod_NoAdd1:
    djnz _Mod_Loop1
    rla
    cpl
    ld b,a
    ld a,c
    ld c,b
    ld b,8
_Mod_Loop2:
    rla
    adc hl,hl
    sbc hl,de
    jr nc,_Mod_NoAdd2
    add hl,de
_Mod_NoAdd2:
    djnz _Mod_Loop2
    rla
    cpl
    ld b,c
    ld c,a
    ret
	end
