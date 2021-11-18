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
Sprite_tran:	db	0
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
	gameGrid:	 ds 448
smallSprites:	db $00, $00, $00, $00, $028, $080, $00, $00
	db $028, $0a2, $010, $00, $028, $0aa, $014, $010
	db $028, $0aa, $014, $014, $03c, $0c0, $00, $00
	db $03c, $0f3, $020, $00, $03c, $0ff, $028, $020
	db $03c, $0ff, $028, $028, $03c, $0df, $0ff, $0cc
	db $014, $0d7, $055, $014, $014, $00, $00, $00
	db $014, $055, $055, $014, $017, $055, $055, $017
	db $014, $055, $055, $0d7, $0d4, $055, $055, $0d4
	db $0d7, $055, $055, $014, $00, $010, $00, $00
	db $00, $010, $04, $00, $00, $04, $010, $00
	db $044, $011, $044, $011, $00, $030, $00, $00
	db $00, $030, $0c, $00, $00, $0c, $030, $00
	db $0cc, $033, $0cc, $033
bigSprites:	db $00, $00, $00, $03f, $07d, $0fc, $0c3, $0ff
	db $0c3, $0c, $00, $030, $0c, $00, $030, $033
	db $07d, $0cc, $033, $0ff, $0cc, $03, $00, $0c0
	db $03, $00, $0c0, $0f, $07d, $0f0, $0f, $0ff
	db $0f0, $0c, $0c3, $030, $0c, $00, $030, $033
	db $07d, $0cc, $033, $0ff, $0cc, $03, $00, $0c0
	db $0cc, $0cc, $030, $030, $030, $0cc, $0d, $0dc
	db $0c, $03, $0ff, $0f0, $0cc, $0cc, $00, $030
	db $030, $0c, $0d, $0dc, $033, $03, $0ff, $0c3
	db $0cc, $0cc, $03c, $030, $030, $03, $0d, $0dc
	db $03c, $03, $0ff, $0c0, $0cc, $0cc, $0c, $030
	db $030, $033, $0d, $0dc, $030, $03, $0ff, $0c0
	db $0c, $033, $033, $033, $0c, $0c, $030, $037
	db $070, $0f, $0ff, $0c0, $00, $033, $033, $030
	db $0c, $0c, $0cc, $037, $070, $0c3, $0ff, $0c0
	db $03c, $033, $033, $0c0, $0c, $0c, $03c, $037
	db $070, $03, $0ff, $0c0, $030, $033, $033, $0cc
	db $0c, $0c, $0c, $037, $070, $03, $0ff, $0c0
	db $0cc, $0c3, $033, $037, $03c, $0dc, $037, $03c
	db $0dc, $0cc, $0c3, $033, $0c, $0c3, $030, $03
	db $07d, $0c0, $0c, $0c3, $030, $00, $00, $00
	db $00, $0c3, $00, $00, $03c, $00, $00, $0c3
	db $00, $00, $00, $00, $00, $00, $00, $00
	db $03c, $00, $00, $00, $00, $00, $00, $00
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
bulletYConstrain:	db $029, $02d, $02d, $02d, $02d, $031, $031, $031
	db $031, $035, $035, $035, $035, $039, $039, $039
	db $039, $03d
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
gameStats_gameStats_record_gameStats_record_milliSpeed	db	0
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
milliSegments_milliSegments_record_milliSegments_record_lastX	db	0
	    db 0,0,0,0,0,0,0,0,0,0,0
milliSegments_milliSegments_record_milliSegments_record_lastY	db	0
	    db 0,0,0,0,0,0,0,0,0,0,0
milliSegments_milliSegments_record_milliSegments_record_lastGridVal	db	0
	    db 0,0,0,0,0,0,0,0,0,0,0
milliSegments_milliSegments_record_milliSegments_record_flags	db	0
	    db 0,0,0,0,0,0,0,0,0,0,0
mushroomJustShot_mushroomJustShot_record_mushroomJustShot_record_x	db	0
mushroomJustShot_mushroomJustShot_record_mushroomJustShot_record_y	db	0
scorpionEnemy_scorpion_record_scorpion_record_enabled	db	0
scorpionEnemy_scorpion_record_scorpion_record_x	db	0
scorpionEnemy_scorpion_record_scorpion_record_y	db	0
scorpionEnemy_scorpion_record_scorpion_record_direction	db	0
scorpionEnemy_scorpion_record_scorpion_record_animFrame	db	0
scorpionEnemy_scorpion_record_scorpion_record_moveSpeed	db	0
scorpionEnemy_scorpion_record_scorpion_record_dead	db	0
	 
; //	Clears the screen using mode parameter 0 or 1
; //	
	; ***********  Defining procedure : Screen_Cls
	;    Procedure type : User-defined procedure
 ; Temp vars section
DrawPadded_stringassignstr314: db "0",0
DrawPadded_stringassignstr340: db "0",0
DrawHUD_stringassignstr343: db "Score:",0
DrawHUD_stringassignstr345: db "Lives:",0
DrawHUD_stringassignstr347: db "Hi:",0
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
	;* 
; //  Takes a pointer to a 256 byte array and returns a random number
; //  *
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
; //  Takes a maximum number and generates and returns a random number.
; //  *
	; ***********  Defining procedure : Functions_getRandomByte
	;    Procedure type : User-defined procedure
Functions_getRandomByte_block181:
Functions_getRandomByte:
	; ****** Inline assembler section
  ld a,[Functions_i]
  ld c,a
  ld a,r
  ld d,a
  add a,d
  inc d
  xor d
rndloop
  ld b,#7f
redo
  inc b
  cp c  
  jr c,rndend
  dec b
  and b
  srl b
  jr redo
rndend
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
Functions_IntegerToString_block182:
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
Functions_ByteToString_block183:
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
Sprite_SetSize_block184:
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
Sprite_SetData_block186:
Sprite_SetData:
	ret
	; ***********  Defining procedure : Sprite_StampAt
	;    Procedure type : User-defined procedure
Sprite_StampAt_block187:
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
Sprite_StampAt_forloop188:
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
Sprite_StampAt_forloopcounter190:
Sprite_StampAt_loopstart191:
	ld a,[Sprite_y]
	ld c,a
	ld a,[Sprite_i]
	add a,1
	ld [Sprite_i],a
	cp c
	jr nz,Sprite_StampAt_forloop188
Sprite_StampAt_forloopend189:
Sprite_StampAt_loopend192:
	ret
	; ***********  Defining procedure : Sprite_DrawAt
	;    Procedure type : User-defined procedure
Sprite_DrawAt_block195:
Sprite_DrawAt:
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[Sprite_x]
	cp $1f
	jr c, Sprite_DrawAt_localfailed201
	jr z, Sprite_DrawAt_localfailed201
	jr Sprite_DrawAt_ConditionalTrueBlock197
Sprite_DrawAt_localfailed201: ;keep:
	; ; logical OR, second chance
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[Sprite_y]
	cp $3f
	jr c, Sprite_DrawAt_elsedoneblock199
	jr z, Sprite_DrawAt_elsedoneblock199
Sprite_DrawAt_ConditionalTrueBlock197: ;Main true block ;keep :
	
; // Exit if out of bounds
	ret
Sprite_DrawAt_elsedoneblock199:
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
  pop bc                  ; bc = sprite screen location
iterate_height:
  ld a,[Sprite_spritewidth]     ; sprite width
  ld d,a                  ; d = sprite width
iterate_width:
  ld a,(hl)
  ld e,a
  ld a,[Sprite_tran]
  cp #01
  jr z,transp
  ld a,e
  jr drawsprite
transp 
  ld a,(bc)
  xor e
drawsprite  
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
  pop bc                  ; restore sprite screen location
  pop hl                  ; restore sprite data position
  ld a,[hcount]           ; sprite height counter
  dec a 
  ld [hcount],a
  jr nz,iterate_height
  ret
hcount
  db #00  
	ret
	; ***********  Defining procedure : IsBitSet
	;    Procedure type : User-defined procedure
IsBitSet_block203:
IsBitSet:
	ld a,[BitFlag]
	ld b,a
	ld a,[Value]
	and  b
	ret
	; ***********  Defining procedure : SetBit
	;    Procedure type : User-defined procedure
SetBit_block204:
SetBit:
	ld a,[BitFlag]
	ld b,a
	ld a,[Value]
	or  b
	ret
	; ***********  Defining procedure : ResetBit
	;    Procedure type : User-defined procedure
ResetBit_block205:
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
TitleScreen_while207:
TitleScreen_loopstart211:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	call Input_GetPressedKey
	cp $27
	jp z, TitleScreen_elsedoneblock210
TitleScreen_ConditionalTrueBlock208: ;Main true block ;keep :
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $0
	jr nz,TitleScreen_elsedoneblock249
TitleScreen_ConditionalTrueBlock247: ;Main true block ;keep :
	
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
	jr nc,TitleScreen_elseblock262
TitleScreen_ConditionalTrueBlock261: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[strpos]
	add  a,$1
	ld [strpos], a
	jr TitleScreen_elsedoneblock263
TitleScreen_elseblock262:
	ld a, $0
	ld [strpos], a
TitleScreen_elsedoneblock263:
TitleScreen_elsedoneblock249:
	ld a, $0
	ld [u], a
TitleScreen_forloop268:
	; Wait
	ld a,$32
TitleScreen_wait276:
	sub 1
	jr nz,TitleScreen_wait276
TitleScreen_forloopcounter270:
TitleScreen_loopstart271:
	ld a,[u]
	add a,1
	ld [u],a
	cp $c8
	jr nz,TitleScreen_forloop268
TitleScreen_forloopend269:
TitleScreen_loopend272:
	
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
	jp TitleScreen_while207
TitleScreen_elsedoneblock210:
TitleScreen_loopend212:
	ld a, $1
	ld [gameStats_gameStats_record_gameStats_record_gameRunning], a
	ret
	
; // Sound effects manager
	; ***********  Defining procedure : PlaySound
	;    Procedure type : User-defined procedure
PlaySound_block277:
PlaySound:
	ld a,[u]
	cp $1
	jr nz,PlaySound_casenext279
	
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
	jp PlaySound_caseend278
PlaySound_casenext279:
	ld a,[u]
	cp $2
	jr nz,PlaySound_casenext281
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
	jp PlaySound_caseend278
PlaySound_casenext281:
	ld a,[u]
	cp $3
	jr nz,PlaySound_casenext283
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
	jp PlaySound_caseend278
PlaySound_casenext283:
	ld a,[u]
	cp $4
	jr nz,PlaySound_casenext286
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
	jp PlaySound_caseend278
PlaySound_casenext286:
	ld a,[u]
	cp $5
	jr nz,PlaySound_casenext288
	call Sound_Explode
	jp PlaySound_caseend278
PlaySound_casenext288:
	ld a,[u]
	cp $6
	jr nz,PlaySound_casenext290
	; generic assign 
	ld hl,$14
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	ld hl,$1e
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
	; generic assign 
	ld hl,$1e
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
	jp PlaySound_caseend278
PlaySound_casenext290:
	ld a,[u]
	cp $8
	jr nz,PlaySound_casenext292
	; generic assign 
	ld hl,$32
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	ld hl,$1e
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
	jp PlaySound_caseend278
PlaySound_casenext292:
	ld a,[u]
	cp $7
	jr nz,PlaySound_casenext294
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_animFrame]
	cp $0
	jr nz,PlaySound_casenext297
	; generic assign 
	ld hl,$32
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	ld hl,$a
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
	jp PlaySound_caseend296
PlaySound_casenext297:
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_animFrame]
	cp $1
	jr nz,PlaySound_casenext299
	; generic assign 
	ld hl,$3c
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	ld hl,$a
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
	jp PlaySound_caseend296
PlaySound_casenext299:
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_animFrame]
	cp $2
	jr nz,PlaySound_casenext301
	; generic assign 
	ld hl,$46
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	ld hl,$a
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
	jp PlaySound_caseend296
PlaySound_casenext301:
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_animFrame]
	cp $3
	jr nz,PlaySound_casenext303
	; generic assign 
	ld hl,$3c
	; Integer assignment 
	; Loading pointer
	ld [Sound_freq],hl
	; generic assign 
	ld hl,$a
	; Integer assignment 
	; Loading pointer
	ld [Sound_dur],hl
	call Sound_Play
PlaySound_casenext303:
PlaySound_caseend296:
PlaySound_casenext294:
PlaySound_caseend278:
	ret
	; ***********  Defining procedure : DrawPadded
	;    Procedure type : User-defined procedure
zscore:	dw	0
DrawPadded_block305:
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
	jr nc,DrawPadded_elsedoneblock309
DrawPadded_ConditionalTrueBlock307: ;Main true block ;keep :
	; Assigning a string : Font_text
	ld hl,DrawPadded_stringassignstr314
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
DrawPadded_elsedoneblock309:
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
	jr nc,DrawPadded_elsedoneblock319
DrawPadded_ConditionalTrueBlock317: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock319:
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
	jr nc,DrawPadded_elsedoneblock325
DrawPadded_ConditionalTrueBlock323: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock325:
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
	jr nc,DrawPadded_elsedoneblock331
DrawPadded_ConditionalTrueBlock329: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock331:
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
	jr nc,DrawPadded_elsedoneblock337
DrawPadded_ConditionalTrueBlock335: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock337:
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
	ld hl,DrawPadded_stringassignstr340
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
	ld hl,DrawHUD_stringassignstr343
	; Loading pointer
	ld [Font_text],hl
	ld a, $13
	ld [Font_tx], a
	ld a, $0
	ld [Font_ty], a
	ld [Font_tran], a
	call Font_DrawTextAt
	; Assigning a string : Font_text
	ld hl,DrawHUD_stringassignstr345
	; Loading pointer
	ld [Font_text],hl
	ld a, $0
	ld [Font_tx], a
	ld [Font_ty], a
	ld [Font_tran], a
	call Font_DrawTextAt
	; Assigning a string : Font_text
	ld hl,DrawHUD_stringassignstr347
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
InitialiseGame_forloop350:
	
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
InitialiseGame_forloop366:
	; Wait
	ld a,$8c
InitialiseGame_wait374:
	sub 1
	jr nz,InitialiseGame_wait374
InitialiseGame_forloopcounter368:
InitialiseGame_loopstart369:
	ld a,[u]
	add a,1
	ld [u],a
	cp $1e
	jr nz,InitialiseGame_forloop366
InitialiseGame_forloopend367:
InitialiseGame_loopend370:
InitialiseGame_forloopcounter352:
InitialiseGame_loopstart353:
	ld a,[v]
	add a,1
	ld [v],a
	cp $20
	jr nz,InitialiseGame_forloop350
InitialiseGame_forloopend351:
InitialiseGame_loopend354:
	ld a, $0
	ld [u], a
InitialiseGame_forloop375:
	
; // Initialise grid
	; Storing to array
	ld a,[u]
	ld e,a
	ld d,0
	ld hl,gameGrid
	add hl,de
	ld a,$0
	ld [hl],a
InitialiseGame_forloopcounter377:
InitialiseGame_loopstart378:
	ld a,[u]
	add a,1
	ld [u],a
	cp $ff
	jr nz,InitialiseGame_forloop375
InitialiseGame_forloopend376:
InitialiseGame_loopend379:
	ld a, $0
	ld [u], a
InitialiseGame_forloop382:
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
InitialiseGame_forloopcounter384:
InitialiseGame_loopstart385:
	ld a,[u]
	add a,1
	ld [u],a
	cp $c1
	jr nz,InitialiseGame_forloop382
InitialiseGame_forloopend383:
InitialiseGame_loopend386:
	
; // A large number to note the segment is not on the grid yet.
	ld a, $0
	ld [u], a
InitialiseGame_forloop389:
	
; // Initialise millipede
	; Storing to array
	ld a,[u]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,$4a
	ld [hl],a
	
; // Initial direction set to right, down and set to alive body segment.
	; Storing to array
	ld a,[u]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,$ff
	ld [hl],a
InitialiseGame_forloopcounter391:
InitialiseGame_loopstart392:
	ld a,[u]
	add a,1
	ld [u],a
	cp $c
	jr nz,InitialiseGame_forloop389
InitialiseGame_forloopend390:
InitialiseGame_loopend393:
	; Storing to array
	ld a,$5a
	ld [milliSegments_milliSegments_record_milliSegments_record_flags+$0],a
	
; // Alive, direction right, down and head segment.
	ld a, $7
	ld [gameStats_gameStats_record_gameStats_record_milliSpeed], a
	
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
	ld a, $0
	ld [playerBullet_playerBullet_record_playerBullet_record_fired], a
	ld [fleaEnemy_flea_record_flea_record_enabled], a
	ld [scorpionEnemy_scorpion_record_scorpion_record_enabled], a
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
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[gameStats_gameStats_record_gameStats_record_level]
	cp $1
	jr nz,InitialiseGame_elsedoneblock399
InitialiseGame_ConditionalTrueBlock397: ;Main true block ;keep :
	ret
InitialiseGame_elsedoneblock399:
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
DrawGrid_forloop403:
	
; // Playfield grid array
	; generic assign 
	; generic assign 
	ld hl,$1f
	; Integer assignment 
	; Loading pointer
	ld [Functions_i],hl
	call Functions_getRandomByte
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
	ld a,$4
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
	ld a, $4
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; generic assign 
	; generic assign 
	ld hl,$1f
	; Integer assignment 
	; Loading pointer
	ld [Functions_i],hl
	call Functions_getRandomByte
	ld [varPrefixed_p], a
DrawGrid_while418:
DrawGrid_loopstart422:
	; Binary clause core: EQUALS
	; Compare two vars optimization
	ld a,[x]
	ld b,a
	ld a,[varPrefixed_p]
	cp b
	jr nz,DrawGrid_elsedoneblock421
DrawGrid_ConditionalTrueBlock419: ;Main true block ;keep :
	; generic assign 
	; generic assign 
	ld hl,$1f
	; Integer assignment 
	; Loading pointer
	ld [Functions_i],hl
	call Functions_getRandomByte
	ld [varPrefixed_p], a
	jr DrawGrid_while418
DrawGrid_elsedoneblock421:
DrawGrid_loopend423:
	; generic assign 
	ld a,[varPrefixed_p]
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
	ld a,$4
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
	ld a, $4
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
DrawGrid_forloopcounter405:
DrawGrid_loopstart406:
	ld a,[y]
	add a,1
	ld [y],a
	cp $d
	jp nz,DrawGrid_forloop403
DrawGrid_forloopend404:
DrawGrid_loopend407:
	ret
	; ***********  Defining procedure : GetGridLocVal
	;    Procedure type : User-defined procedure
GetGridLocVal_block426:
GetGridLocVal:
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
SetGridLocVal_block427:
SetGridLocVal:
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
	jr nz,SubtractLife_elsedoneblock432
SubtractLife_ConditionalTrueBlock430: ;Main true block ;keep :
	
; // Check if Game Over
; // Player dead. TODO
	ld a, $0
	ld [gameStats_gameStats_record_gameStats_record_gameRunning], a
SubtractLife_elsedoneblock432:
	call DrawHUD
	ret
	
; // Add points to player's score
	; ***********  Defining procedure : AddScore
	;    Procedure type : User-defined procedure
AddScore_block435:
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
	; ***********  Defining procedure : DrawBullet
	;    Procedure type : User-defined procedure
DrawBullet_block436:
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
DeleteBullet_block437:
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
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [Sprite_spritey], a
	ld a, $a
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $40
	jr nz,ControlPlayer_elsedoneblock442
ControlPlayer_localsuccess444: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $40
	jr nz,ControlPlayer_elsedoneblock442
ControlPlayer_localsuccess445: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[w]
	cp $9
	jr nz,ControlPlayer_elsedoneblock442
ControlPlayer_ConditionalTrueBlock440: ;Main true block ;keep :
	
; // Test for player movement
	ret
ControlPlayer_elsedoneblock442:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $d0
	jr nz,ControlPlayer_localfailed458
	jr ControlPlayer_ConditionalTrueBlock448
ControlPlayer_localfailed458: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $d0
	jr nz,ControlPlayer_localfailed459
	jr ControlPlayer_ConditionalTrueBlock448
ControlPlayer_localfailed459: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[w]
	cp $54
	jr nz,ControlPlayer_elsedoneblock450
ControlPlayer_ConditionalTrueBlock448: ;Main true block ;keep :
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[playerBullet_playerBullet_record_playerBullet_record_fired]
	cp $0
	jr z, ControlPlayer_elsedoneblock464
ControlPlayer_ConditionalTrueBlock462: ;Main true block ;keep :
	
; // Player shooting
; // Test if a bullet already exists onscreen. Snake can only shoot once at a time
	ret
ControlPlayer_elsedoneblock464:
	ld a, $1
	ld [playerBullet_playerBullet_record_playerBullet_record_fired], a
	ld a, $2
	ld [u], a
	call PlaySound
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [playerBullet_playerBullet_record_playerBullet_record_x], a
	; generic assign 
	ld b,$2a
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,bulletYConstrain
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'bulletYConstrain' is word : 0
	ld [playerBullet_playerBullet_record_playerBullet_record_y], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [x], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	ld [y], a
	call DrawBullet
	ret
ControlPlayer_elsedoneblock450:
	
; // Erase player sprite
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [Sprite_spritey], a
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $48
	jr nz,ControlPlayer_localfailed490
	jr ControlPlayer_ConditionalTrueBlock468
ControlPlayer_localfailed490: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $48
	jr nz,ControlPlayer_localfailed491
	jr ControlPlayer_ConditionalTrueBlock468
ControlPlayer_localfailed491: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[w]
	cp $58
	jr nz,ControlPlayer_elsedoneblock470
ControlPlayer_ConditionalTrueBlock468: ;Main true block ;keep :
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	cp $2a
	jr c, ControlPlayer_elsedoneblock496
	jr z, ControlPlayer_elsedoneblock496
ControlPlayer_ConditionalTrueBlock494: ;Main true block ;keep :
	
; // Check if player wants to move UP
; // Make sure we're not already at the top of the player area
; // Move player y pos
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	sub $1
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
	jr c, ControlPlayer_elsedoneblock508
	jr z, ControlPlayer_elsedoneblock508
ControlPlayer_ConditionalTrueBlock506: ;Main true block ;keep :
	
; // Test if we're colliding with a mushroom. If so, move player y back
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	add  a,$1
	ld [playerSnake_playerSnake_record_playerSnake_record_y], a
ControlPlayer_elsedoneblock508:
ControlPlayer_elsedoneblock496:
ControlPlayer_elsedoneblock470:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $44
	jr nz,ControlPlayer_localfailed538
	jr ControlPlayer_ConditionalTrueBlock512
ControlPlayer_localfailed538: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $44
	jr nz,ControlPlayer_localfailed539
	jr ControlPlayer_ConditionalTrueBlock512
ControlPlayer_localfailed539: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[w]
	cp $57
	jr nz,ControlPlayer_elsedoneblock514
ControlPlayer_ConditionalTrueBlock512: ;Main true block ;keep :
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	cp $3a
	jr nc,ControlPlayer_elsedoneblock544
ControlPlayer_ConditionalTrueBlock542: ;Main true block ;keep :
	
; // Due to player sprite being 4 pixels high we need to check 4 below the Y value.
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	add  a,$4
	ld [playerSnake_playerSnake_record_playerSnake_record_y], a
	; generic assign 
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
	ld [u], a
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
	jr c, ControlPlayer_elseblock557
	jr z, ControlPlayer_elseblock557
ControlPlayer_ConditionalTrueBlock556: ;Main true block ;keep :
	
; // If a mushroom is below the player, move the y back to where it was, otherwise move one pixel down
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	sub $4
	ld [playerSnake_playerSnake_record_playerSnake_record_y], a
	jr ControlPlayer_elsedoneblock558
ControlPlayer_elseblock557:
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	sub $3
	ld [playerSnake_playerSnake_record_playerSnake_record_y], a
ControlPlayer_elsedoneblock558:
ControlPlayer_elsedoneblock544:
ControlPlayer_elsedoneblock514:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $42
	jr nz,ControlPlayer_localfailed598
	jr ControlPlayer_ConditionalTrueBlock564
ControlPlayer_localfailed598: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $42
	jr nz,ControlPlayer_localfailed599
	jr ControlPlayer_ConditionalTrueBlock564
ControlPlayer_localfailed599: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[w]
	cp $64
	jr nz,ControlPlayer_elsedoneblock566
ControlPlayer_ConditionalTrueBlock564: ;Main true block ;keep :
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	cp $0
	jr c, ControlPlayer_elsedoneblock604
	jr z, ControlPlayer_elsedoneblock604
ControlPlayer_ConditionalTrueBlock602: ;Main true block ;keep :
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
	jr c, ControlPlayer_elsedoneblock622
	jr z, ControlPlayer_elsedoneblock622
ControlPlayer_ConditionalTrueBlock620: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	add  a,$1
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
ControlPlayer_elsedoneblock622:
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
	jr c, ControlPlayer_elsedoneblock628
	jr z, ControlPlayer_elsedoneblock628
ControlPlayer_ConditionalTrueBlock626: ;Main true block ;keep :
	
; // Test bottom of sprite
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	add  a,$1
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
ControlPlayer_elsedoneblock628:
ControlPlayer_elsedoneblock604:
ControlPlayer_elsedoneblock566:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $41
	jr nz,ControlPlayer_localfailed666
	jr ControlPlayer_ConditionalTrueBlock632
ControlPlayer_localfailed666: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $41
	jr nz,ControlPlayer_localfailed667
	jr ControlPlayer_ConditionalTrueBlock632
ControlPlayer_localfailed667: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[w]
	cp $44
	jr nz,ControlPlayer_elsedoneblock634
ControlPlayer_ConditionalTrueBlock632: ;Main true block ;keep :
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	cp $1f
	jr nc,ControlPlayer_elsedoneblock672
ControlPlayer_ConditionalTrueBlock670: ;Main true block ;keep :
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
	jr c, ControlPlayer_elsedoneblock690
	jr z, ControlPlayer_elsedoneblock690
ControlPlayer_ConditionalTrueBlock688: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	sub $1
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
ControlPlayer_elsedoneblock690:
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
	jr c, ControlPlayer_elsedoneblock696
	jr z, ControlPlayer_elsedoneblock696
ControlPlayer_ConditionalTrueBlock694: ;Main true block ;keep :
	
; // Test bottom of sprite
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	sub $1
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
ControlPlayer_elsedoneblock696:
ControlPlayer_elsedoneblock672:
ControlPlayer_elsedoneblock634:
	
; // Draw player sprite at new location
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [Sprite_spritey], a
	ld a, $a
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	ret
	
; // Check if we need to put a flea on the screen
	; ***********  Defining procedure : SpawnFlea
	;    Procedure type : User-defined procedure
SpawnFlea:
	ld a, $0
	ld [t], a
	ld [u], a
SpawnFlea_forloop700:
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
	cp $3
	jr c, SpawnFlea_elsedoneblock723
	jr z, SpawnFlea_elsedoneblock723
SpawnFlea_localsuccess725: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $9
	jr nc,SpawnFlea_elsedoneblock723
SpawnFlea_ConditionalTrueBlock721: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[t]
	add  a,$1
	ld [t], a
SpawnFlea_elsedoneblock723:
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[t]
	cp $5
	jr c, SpawnFlea_elsedoneblock730
	jr z, SpawnFlea_elsedoneblock730
SpawnFlea_ConditionalTrueBlock728: ;Main true block ;keep :
	ret
SpawnFlea_elsedoneblock730:
SpawnFlea_forloopcounter702:
SpawnFlea_loopstart703:
	ld a,[u]
	add a,1
	ld [u],a
	cp $a0
	jr nz,SpawnFlea_forloop700
SpawnFlea_forloopend701:
SpawnFlea_loopend704:
	ld a, $1
	ld [fleaEnemy_flea_record_flea_record_enabled], a
	; generic assign 
	; generic assign 
	ld hl,$1f
	; Integer assignment 
	; Loading pointer
	ld [Functions_i],hl
	call Functions_getRandomByte
	ld [fleaEnemy_flea_record_flea_record_x], a
	ld a, $a
	ld [fleaEnemy_flea_record_flea_record_y], a
	ld a, $2
	ld [fleaEnemy_flea_record_flea_record_moveSpeed], a
	ld a, $0
	ld [fleaEnemy_flea_record_flea_record_shotCount], a
	ld [fleaEnemy_flea_record_flea_record_dropCount], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	ld a, $9
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
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
	jr nc,MoveBullet_elsedoneblock737
MoveBullet_ConditionalTrueBlock735: ;Main true block ;keep :
	ld a, $0
	ld [playerBullet_playerBullet_record_playerBullet_record_fired], a
	ret
MoveBullet_elsedoneblock737:
	
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
	ld [s], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	cp $0
	jp c, MoveBullet_elsedoneblock743
	jp z, MoveBullet_elsedoneblock743
MoveBullet_ConditionalTrueBlock741: ;Main true block ;keep :
	
; // Collided with something on the grid
; // Turn bullet off
	ld a, $0
	ld [playerBullet_playerBullet_record_playerBullet_record_fired], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[s]
	cp $0
	jp c, MoveBullet_elsedoneblock828
	jp z, MoveBullet_elsedoneblock828
MoveBullet_localsuccess866: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[s]
	cp $9
	jp nc,MoveBullet_elsedoneblock828
MoveBullet_ConditionalTrueBlock826: ;Main true block ;keep :
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[s]
	cp $9
	jp nc,MoveBullet_elsedoneblock871
MoveBullet_ConditionalTrueBlock869: ;Main true block ;keep :
	
; // Test for mushies
; // Mushy at this grid loc
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
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; 'a:=a + const'  optimization 
	ld a,[s]
	sub $1
	ld [s], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	cp $4
	jr nz,MoveBullet_elsedoneblock883
MoveBullet_ConditionalTrueBlock881: ;Main true block ;keep :
	ld a, $0
	ld [s], a
MoveBullet_elsedoneblock883:
	
; // Bad mushroom shot completely
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
	; generic assign 
	ld a,[s]
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
	ld a,[s]
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
MoveBullet_elsedoneblock871:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[s]
	cp $0
	jp nz,MoveBullet_elsedoneblock889
MoveBullet_ConditionalTrueBlock887: ;Main true block ;keep :
	
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
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	ld a, $1
	ld [t], a
	call AddScore
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [mushroomJustShot_mushroomJustShot_record_mushroomJustShot_record_x], a
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
	ld [mushroomJustShot_mushroomJustShot_record_mushroomJustShot_record_y], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_enabled]
	cp $0
	jr nz,MoveBullet_elsedoneblock901
MoveBullet_ConditionalTrueBlock899: ;Main true block ;keep :
	
; // See if number of mushies in player area needs updating
	call SpawnFlea
MoveBullet_elsedoneblock901:
MoveBullet_elsedoneblock889:
MoveBullet_elsedoneblock828:
MoveBullet_elsedoneblock743:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[s]
	cp $9
	jp nz,MoveBullet_localfailed1224
	jp MoveBullet_ConditionalTrueBlock905
MoveBullet_localfailed1224: ;keep:
	; ; logical OR, second chance
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[s]
	cp $a
	jp nz,MoveBullet_elsedoneblock907
MoveBullet_ConditionalTrueBlock905: ;Main true block ;keep :
	
; // Check for shot millipede segment
; // Update value in grid location
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
	ld [q], a
	ld a, $0
	ld [varPrefixed_p], a
MoveBullet_forloop1226:
	; Binary clause core: EQUALS
	; Compare two vars optimization
	ld a,[q]
	ld b,a
	ld a,[varPrefixed_p]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	cp b
	jp nz,MoveBullet_elsedoneblock1390
MoveBullet_ConditionalTrueBlock1388: ;Main true block ;keep :
	; Binary clause core: EQUALS
	; Compare two vars optimization
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld b,a
	ld a,[varPrefixed_p]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	cp b
	jp nz,MoveBullet_elsedoneblock1470
MoveBullet_ConditionalTrueBlock1468: ;Main true block ;keep :
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[varPrefixed_p]
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
	jp z, MoveBullet_elsedoneblock1510
MoveBullet_ConditionalTrueBlock1508: ;Main true block ;keep :
	
; // bullet and millipede are same Grid Location
	; Storing to array
	; generic assign 
	ld a,[varPrefixed_p]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $40
	ld [BitFlag], a
	call ResetBit
	push af
	ld a,[varPrefixed_p]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[s]
	cp $9
	jr nz,MoveBullet_elseblock1529
MoveBullet_ConditionalTrueBlock1528: ;Main true block ;keep :
	ld a, $5
	ld [t], a
	call AddScore
	jr MoveBullet_elsedoneblock1530
MoveBullet_elseblock1529:
	ld a, $32
	ld [t], a
	call AddScore
MoveBullet_elsedoneblock1530:
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[varPrefixed_p]
	cp $c
	jr nc,MoveBullet_elsedoneblock1538
MoveBullet_ConditionalTrueBlock1536: ;Main true block ;keep :
	; Storing to array
	; generic assign 
	ld b,$1
	ld a,[varPrefixed_p]
	add  a, b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_flags' is word : 0
	ld [Value], a
	ld a, $10
	ld [BitFlag], a
	call SetBit
	push af
	ld b,$1
	ld a,[varPrefixed_p]
	add  a, b
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
MoveBullet_elsedoneblock1538:
	ld a, $6
	ld [u], a
	call PlaySound
	; generic assign 
	ld a,[varPrefixed_p]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	ld [Sprite_spritex], a
	; generic assign 
	; Generic 16-bit binop
	ld hl,$6
	ex de,hl
	push de
	ld a,[varPrefixed_p]
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
	pop de
	add hl,de
	ld a,l ; word assigned to byte
	ld [Sprite_spritey], a
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [v], a
	; generic assign 
	ld a,[q]
	ld [w], a
	ld a, $4
	ld [t], a
	call SetGridLocVal
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	; Generic 16-bit binop
	ld hl,$6
	ex de,hl
	push de
	ld a,[varPrefixed_p]
	ld e,a ; variable is 8-bit
	ld d,0
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
	pop de
	add hl,de
	ld a,l ; word assigned to byte
	ld [Sprite_spritey], a
	ld a, $4
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
MoveBullet_elsedoneblock1510:
MoveBullet_elsedoneblock1470:
MoveBullet_elsedoneblock1390:
MoveBullet_forloopcounter1228:
MoveBullet_loopstart1229:
	ld a,[varPrefixed_p]
	add a,1
	ld [varPrefixed_p],a
	cp $c
	jp nz,MoveBullet_forloop1226
MoveBullet_forloopend1227:
MoveBullet_loopend1230:
MoveBullet_elsedoneblock907:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[playerBullet_playerBullet_record_playerBullet_record_fired]
	cp $0
	jr z, MoveBullet_elsedoneblock1544
MoveBullet_ConditionalTrueBlock1542: ;Main true block ;keep :
	
; // Draw sprite at new location onscreen
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [x], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	ld [y], a
	call DrawBullet
MoveBullet_elsedoneblock1544:
	ret
	
; // Did the player shoot the flea?
	; ***********  Defining procedure : CheckFleaShot
	;    Procedure type : User-defined procedure
CheckFleaShot:
	; Binary clause core: NOTEQUALS
	; Compare two vars optimization
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld b,a
	ld a,[fleaEnemy_flea_record_flea_record_x]
	cp b
	jr z, CheckFleaShot_elsedoneblock1551
CheckFleaShot_ConditionalTrueBlock1549: ;Main true block ;keep :
	ret
CheckFleaShot_elsedoneblock1551:
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
	jr nc,CheckFleaShot_elsedoneblock1557
CheckFleaShot_localsuccess1565: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: GREATER
	; Compare two vars optimization
	ld a,[u]
	ld b,a
	ld a,[t]
	cp b
	jr c, CheckFleaShot_elsedoneblock1557
	jr z, CheckFleaShot_elsedoneblock1557
CheckFleaShot_ConditionalTrueBlock1555: ;Main true block ;keep :
	
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
	jr nz,CheckFleaShot_elsedoneblock1570
CheckFleaShot_ConditionalTrueBlock1568: ;Main true block ;keep :
	ld a, $1
	ld [fleaEnemy_flea_record_flea_record_shotCount], a
	ret
CheckFleaShot_elsedoneblock1570:
	
; // Flea was already shot once
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	ld a, $9
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	ld a, $14
	ld [t], a
	call AddScore
	ld a, $2
	ld [fleaEnemy_flea_record_flea_record_shotCount], a
	ld a, $0
	ld [fleaEnemy_flea_record_flea_record_animFrame], a
	ld [fleaEnemy_flea_record_flea_record_enabled], a
CheckFleaShot_elsedoneblock1557:
	ret
	
; // Update grid from scorpion
	; ***********  Defining procedure : ScorpionGridUpdate
	;    Procedure type : User-defined procedure
ScorpionGridUpdate:
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $5
	jr nc,ScorpionGridUpdate_elsedoneblock1577
ScorpionGridUpdate_localsuccess1579: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[u]
	cp $0
	jr c, ScorpionGridUpdate_elsedoneblock1577
	jr z, ScorpionGridUpdate_elsedoneblock1577
ScorpionGridUpdate_ConditionalTrueBlock1575: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[u]
	add  a,$4
	ld [u], a
ScorpionGridUpdate_elsedoneblock1577:
	ret
	
; // Reset mushrooms, etc under scorpion
	; ***********  Defining procedure : ResetScorpionGrid
	;    Procedure type : User-defined procedure
ResetScorpionGrid:
	; generic assign 
	ld b,$6
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [varPrefixed_p], a
	; generic assign 
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	ld [v], a
	; generic assign 
	ld a,[varPrefixed_p]
	ld [w], a
	call GetGridLocVal
	ld [u], a
	call ScorpionGridUpdate
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	ld [v], a
	; generic assign 
	ld a,[varPrefixed_p]
	ld [w], a
	; generic assign 
	ld a,[u]
	ld [t], a
	call SetGridLocVal
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[u]
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; generic assign 
	; generic assign 
	ld b,$1
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a, b
	ld [v], a
	; generic assign 
	ld a,[varPrefixed_p]
	ld [w], a
	call GetGridLocVal
	ld [u], a
	call ScorpionGridUpdate
	; generic assign 
	ld b,$1
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a, b
	ld [v], a
	; generic assign 
	ld a,[varPrefixed_p]
	ld [w], a
	; generic assign 
	ld a,[u]
	ld [t], a
	call SetGridLocVal
	; generic assign 
	ld b,$1
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a, b
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[u]
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; generic assign 
	; generic assign 
	ld b,$2
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a, b
	ld [v], a
	; generic assign 
	ld a,[varPrefixed_p]
	ld [w], a
	call GetGridLocVal
	ld [u], a
	call ScorpionGridUpdate
	; generic assign 
	ld b,$2
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a, b
	ld [v], a
	; generic assign 
	ld a,[varPrefixed_p]
	ld [w], a
	; generic assign 
	ld a,[u]
	ld [t], a
	call SetGridLocVal
	; generic assign 
	ld b,$2
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a, b
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[u]
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	ret
	
; // Reset millipede data ingame
	; ***********  Defining procedure : ResetMillipedeToStart
	;    Procedure type : User-defined procedure
ResetMillipedeToStart:
	
; // A large number to note the segment is not on the grid yet.
	ld a, $0
	ld [s], a
ResetMillipedeToStart_forloop1583:
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
	jr z, ResetMillipedeToStart_elsedoneblock1599
ResetMillipedeToStart_ConditionalTrueBlock1597: ;Main true block ;keep :
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
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
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
	ld a, $0
	ld [t], a
	call SetGridLocVal
ResetMillipedeToStart_elsedoneblock1599:
	; Storing to array
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,$4a
	ld [hl],a
	
; // Initial direction set to right, down and set to alive body segment.
	; Storing to array
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,$ff
	ld [hl],a
ResetMillipedeToStart_forloopcounter1585:
ResetMillipedeToStart_loopstart1586:
	ld a,[s]
	add a,1
	ld [s],a
	cp $c
	jp nz,ResetMillipedeToStart_forloop1583
ResetMillipedeToStart_forloopend1584:
ResetMillipedeToStart_loopend1587:
	ret
	
; // Player death sequence
	; ***********  Defining procedure : DoPlayerDeath
	;    Procedure type : User-defined procedure
DoPlayerDeath:
	ld a, $1
	ld [v], a
DoPlayerDeath_forloop1603:
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
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	ld a, $5
	ld [u], a
	call PlaySound
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [Sprite_spritey], a
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
DoPlayerDeath_forloopcounter1605:
DoPlayerDeath_loopstart1606:
	ld a,[v]
	add a,1
	ld [v],a
	cp $5
	jr nz,DoPlayerDeath_forloop1603
DoPlayerDeath_forloopend1604:
DoPlayerDeath_loopend1607:
	ld a, $10
	ld [playerSnake_playerSnake_record_playerSnake_record_x], a
	; generic assign 
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [Sprite_spritey], a
	ld a, $a
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	
; // Reset scorpion 
	call ResetScorpionGrid
	ld a, $0
	ld [scorpionEnemy_scorpion_record_scorpion_record_enabled], a
	
; // Reset millipede
	call ResetMillipedeToStart
	; Storing to array
	ld a,$5a
	ld [milliSegments_milliSegments_record_milliSegments_record_flags+$0],a
	ld a, $0
	ld [y], a
DoPlayerDeath_forloop1610:
	ld a, $0
	ld [x], a
DoPlayerDeath_forloop1736:
	
; // Alive, direction right, down and head segment.		
; // Reset mushrooms
	; generic assign 
	; generic assign 
	ld a,[x]
	ld [v], a
	; generic assign 
	ld a,[y]
	ld [w], a
	call GetGridLocVal
	ld [u], a
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	cp $9
	jp nc,DoPlayerDeath_elsedoneblock1802
DoPlayerDeath_ConditionalTrueBlock1800: ;Main true block ;keep :
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $4
	jr z, DoPlayerDeath_elsedoneblock1833
DoPlayerDeath_localsuccess1844: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $0
	jr z, DoPlayerDeath_elsedoneblock1833
DoPlayerDeath_ConditionalTrueBlock1831: ;Main true block ;keep :
	; generic assign 
	ld a,[x]
	ld [v], a
	; generic assign 
	ld a,[y]
	ld [w], a
	ld a, $4
	ld [t], a
	call SetGridLocVal
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
	ld a, $4
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	ld a, $1
	ld [t], a
	call AddScore
	ld a, $8
	ld [u], a
	call PlaySound
	ld a, $0
	ld [u], a
DoPlayerDeath_forloop1846:
	; Wait
	ld a,$14
DoPlayerDeath_wait1854:
	sub 1
	jr nz,DoPlayerDeath_wait1854
DoPlayerDeath_forloopcounter1848:
DoPlayerDeath_loopstart1849:
	ld a,[u]
	add a,1
	ld [u],a
	cp $c8
	jr nz,DoPlayerDeath_forloop1846
DoPlayerDeath_forloopend1847:
DoPlayerDeath_loopend1850:
DoPlayerDeath_elsedoneblock1833:
DoPlayerDeath_elsedoneblock1802:
DoPlayerDeath_forloopcounter1738:
DoPlayerDeath_loopstart1739:
	ld a,[x]
	add a,1
	ld [x],a
	cp $20
	jp nz,DoPlayerDeath_forloop1736
DoPlayerDeath_forloopend1737:
DoPlayerDeath_loopend1740:
DoPlayerDeath_forloopcounter1612:
DoPlayerDeath_loopstart1613:
	ld a,[y]
	add a,1
	ld [y],a
	cp $d
	jp nz,DoPlayerDeath_forloop1610
DoPlayerDeath_forloopend1611:
DoPlayerDeath_loopend1614:
	ret
	
; // All enemy death sequences
	; ***********  Defining procedure : DoEnemyDeathAnim
	;    Procedure type : User-defined procedure
DoEnemyDeathAnim:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_enabled]
	cp $0
	jp z, DoEnemyDeathAnim_elsedoneblock1859
DoEnemyDeathAnim_localsuccess1867: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_dead]
	cp $0
	jp z, DoEnemyDeathAnim_elsedoneblock1859
DoEnemyDeathAnim_ConditionalTrueBlock1857: ;Main true block ;keep :
	
; // Scorpion death
; // Update sprite data
	ld hl,bigSprites
	ld [Sprite_spritedata],hl
	call Sprite_SetData
	ld a, $3
	ld [Sprite_spritewidth], a
	ld a, $4
	ld [Sprite_spriteheight], a
	call Sprite_SetSize
	
; // Draw scorpion sprite
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_animFrame]
	ld b,a
	ld a,$c
	add  a, b
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	
; // Reset sprite data
	ld hl,smallSprites
	ld [Sprite_spritedata],hl
	call Sprite_SetData
	ld a, $1
	ld [Sprite_spritewidth], a
	ld a, $4
	ld [Sprite_spriteheight], a
	call Sprite_SetSize
	; 'a:=a + const'  optimization 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_animFrame]
	add  a,$1
	ld [scorpionEnemy_scorpion_record_scorpion_record_animFrame], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	cp $4
	jr nz,DoEnemyDeathAnim_elsedoneblock1872
DoEnemyDeathAnim_ConditionalTrueBlock1870: ;Main true block ;keep :
	; generic assign 
	ld b,$1
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a, b
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	ld [Sprite_spritey], a
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	call ResetScorpionGrid
	ld a, $0
	ld [scorpionEnemy_scorpion_record_scorpion_record_enabled], a
DoEnemyDeathAnim_elsedoneblock1872:
DoEnemyDeathAnim_elsedoneblock1859:
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_shotCount]
	cp $2
	jr nc,DoEnemyDeathAnim_elsedoneblock1878
DoEnemyDeathAnim_ConditionalTrueBlock1876: ;Main true block ;keep :
	
; // Test for flea death occurring
	ret
DoEnemyDeathAnim_elsedoneblock1878:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_shotCount]
	cp $2
	jp nz,DoEnemyDeathAnim_elsedoneblock1884
DoEnemyDeathAnim_ConditionalTrueBlock1882: ;Main true block ;keep :
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_animFrame]
	cp $0
	jr nz,DoEnemyDeathAnim_elseblock1903
DoEnemyDeathAnim_ConditionalTrueBlock1902: ;Main true block ;keep :
	
; // Do flea stuff
; // Flea death - erase flea
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	ld a, $1
	ld [fleaEnemy_flea_record_flea_record_animFrame], a
	ret
	jr DoEnemyDeathAnim_elsedoneblock1904
DoEnemyDeathAnim_elseblock1903:
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
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
DoEnemyDeathAnim_elsedoneblock1904:
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
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_animFrame]
	cp $4
	jr nz,DoEnemyDeathAnim_elsedoneblock1912
DoEnemyDeathAnim_ConditionalTrueBlock1910: ;Main true block ;keep :
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	ld a, $0
	ld [fleaEnemy_flea_record_flea_record_shotCount], a
	ld [fleaEnemy_flea_record_flea_record_y], a
	call SpawnFlea
	ret
DoEnemyDeathAnim_elsedoneblock1912:
	; 'a:=a + const'  optimization 
	ld a,[fleaEnemy_flea_record_flea_record_animFrame]
	add  a,$1
	ld [fleaEnemy_flea_record_flea_record_animFrame], a
DoEnemyDeathAnim_elsedoneblock1884:
	ret
	
; // Check if flea and player collided
	; ***********  Defining procedure : CheckFleaCollide
	;    Procedure type : User-defined procedure
CheckFleaCollide:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_enabled]
	cp $0
	jr nz,CheckFleaCollide_elsedoneblock1919
CheckFleaCollide_ConditionalTrueBlock1917: ;Main true block ;keep :
	
; // If not touching player, ignore
	ret
CheckFleaCollide_elsedoneblock1919:
	; Binary clause core: NOTEQUALS
	; Compare two vars optimization
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld b,a
	ld a,[fleaEnemy_flea_record_flea_record_x]
	cp b
	jr z, CheckFleaCollide_elsedoneblock1925
CheckFleaCollide_ConditionalTrueBlock1923: ;Main true block ;keep :
	ret
CheckFleaCollide_elsedoneblock1925:
	; Binary clause core: LESS
	; Compare two vars optimization
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld b,a
	ld a,[fleaEnemy_flea_record_flea_record_y]
	cp b
	jr nc,CheckFleaCollide_elsedoneblock1931
CheckFleaCollide_ConditionalTrueBlock1929: ;Main true block ;keep :
	ret
CheckFleaCollide_elsedoneblock1931:
	
; // Passed the above, so flea and snake are touching
	ld a, $0
	ld [fleaEnemy_flea_record_flea_record_enabled], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_y]
	ld [Sprite_spritey], a
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	ld a, $0
	ld [playerBullet_playerBullet_record_playerBullet_record_fired], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	ld [Sprite_spritey], a
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	call SubtractLife
	call DoPlayerDeath
	ret
	
; // Move the flea down the screen, dropping mushies on the way
	; ***********  Defining procedure : MoveFlea
	;    Procedure type : User-defined procedure
MoveFlea:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_shotCount]
	cp $0
	jr nz,MoveFlea_elsedoneblock1938
MoveFlea_ConditionalTrueBlock1936: ;Main true block ;keep :
	
; // Speed control
	; 'a:=a + const'  optimization 
	ld a,[fleaEnemy_flea_record_flea_record_moveSpeed]
	sub $1
	ld [fleaEnemy_flea_record_flea_record_moveSpeed], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	cp $0
	jr nz,MoveFlea_elseblock1951
MoveFlea_ConditionalTrueBlock1950: ;Main true block ;keep :
	ld a, $2
	ld [fleaEnemy_flea_record_flea_record_moveSpeed], a
	jr MoveFlea_elsedoneblock1952
MoveFlea_elseblock1951:
	ret
MoveFlea_elsedoneblock1952:
MoveFlea_elsedoneblock1938:
	
; // Erase flea sprite
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; 'a:=a + const'  optimization 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	add  a,$1
	ld [fleaEnemy_flea_record_flea_record_y], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	cp $3a
	jr c, MoveFlea_elsedoneblock1960
	jr z, MoveFlea_elsedoneblock1960
MoveFlea_ConditionalTrueBlock1958: ;Main true block ;keep :
	
; // Disable if reached bottom of screen
	ld a, $0
	ld [fleaEnemy_flea_record_flea_record_enabled], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld b,$1
	ld a,[fleaEnemy_flea_record_flea_record_y]
	sub  b
	ld [Sprite_spritey], a
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	call SpawnFlea
	ret
MoveFlea_elsedoneblock1960:
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_y]
	ld [Sprite_spritey], a
	ld a, $9
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
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
	jp nz,MoveFlea_elsedoneblock1966
MoveFlea_ConditionalTrueBlock1964: ;Main true block ;keep :
	
; // Only update every 4 pixels down
	ld a, $0
	ld [fleaEnemy_flea_record_flea_record_dropCount], a
	; generic assign 
	; generic assign 
	ld hl,$1f
	; Integer assignment 
	; Loading pointer
	ld [Functions_i],hl
	call Functions_getRandomByte
	ld [t], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	cp $f
	jr c, MoveFlea_elseblock1979
	jr z, MoveFlea_elseblock1979
MoveFlea_ConditionalTrueBlock1978: ;Main true block ;keep :
	
; // Update grid info
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
	ld a, $4
	ld [t], a
	call SetGridLocVal
	
; // Draw new grid sprite
	; generic assign 
	ld a,[fleaEnemy_flea_record_flea_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld b,$4
	ld a,[fleaEnemy_flea_record_flea_record_y]
	sub  b
	ld [Sprite_spritey], a
	ld a, $4
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	jr MoveFlea_elsedoneblock1980
MoveFlea_elseblock1979:
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
	ld a, $0
	ld [t], a
	call SetGridLocVal
MoveFlea_elsedoneblock1980:
MoveFlea_elsedoneblock1966:
	ret
	
; // Drawing Millipede sprite
	; ***********  Defining procedure : DrawMilliSprite
	;    Procedure type : User-defined procedure
DrawMilliSprite_block1985:
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
	jp z, DrawMilliSprite_elseblock1988
DrawMilliSprite_ConditionalTrueBlock1987: ;Main true block ;keep :
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
	jr z, DrawMilliSprite_elsedoneblock2008
DrawMilliSprite_ConditionalTrueBlock2006: ;Main true block ;keep :
	
; // Draw Millipede head sprite
	ld a, $d
	ld [v], a
DrawMilliSprite_elsedoneblock2008:
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
	jr z, DrawMilliSprite_elsedoneblock2014
DrawMilliSprite_ConditionalTrueBlock2012: ;Main true block ;keep :
	ld a, $f
	ld [v], a
DrawMilliSprite_elsedoneblock2014:
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
	ld a, $0
	ld [Sprite_tran], a
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
	ld a, $a
	ld [t], a
	call SetGridLocVal
	jp DrawMilliSprite_elsedoneblock1989
DrawMilliSprite_elseblock1988:
	
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
	ld a, $0
	ld [Sprite_tran], a
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
	ld a, $9
	ld [t], a
	call SetGridLocVal
DrawMilliSprite_elsedoneblock1989:
	ret
	
; // Did player shoot a mushroom?
	; ***********  Defining procedure : CheckMushroomShot
	;    Procedure type : User-defined procedure
CheckMushroomShot:
	; Swapped comparison expressions
	; Binary clause core: EQUALS
	; Compare two vars optimization
	ld a,[mushroomJustShot_mushroomJustShot_record_mushroomJustShot_record_x]
	ld b,a
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	cp b
	jr nz,CheckMushroomShot_elsedoneblock2022
CheckMushroomShot_localsuccess2024: ;keep:
	; ; logical AND, second requirement
	; Swapped comparison expressions
	; Binary clause core: EQUALS
	; Compare two vars optimization
	ld a,[mushroomJustShot_mushroomJustShot_record_mushroomJustShot_record_y]
	ld b,a
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	cp b
	jr nz,CheckMushroomShot_elsedoneblock2022
CheckMushroomShot_ConditionalTrueBlock2020: ;Main true block ;keep :
	
; // Don't change direction if mushroom just shot
	ld a, $1
	ld [q], a
	ld a, $0
	ld [mushroomJustShot_mushroomJustShot_record_mushroomJustShot_record_x], a
	ld [mushroomJustShot_mushroomJustShot_record_mushroomJustShot_record_y], a
CheckMushroomShot_elsedoneblock2022:
	ret
	
; // Did player shoot scorpion?
	; ***********  Defining procedure : CheckScorpionShot
	;    Procedure type : User-defined procedure
CheckScorpionShot:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[playerBullet_playerBullet_record_playerBullet_record_fired]
	cp $0
	jr nz,CheckScorpionShot_elsedoneblock2030
CheckScorpionShot_ConditionalTrueBlock2028: ;Main true block ;keep :
	
; // Check fail conditions first as it's faster.
	ret
CheckScorpionShot_elsedoneblock2030:
	; Binary clause core: LESS
	; Compare two vars optimization
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	ld b,a
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	cp b
	jr nc,CheckScorpionShot_localfailed2038
	jr CheckScorpionShot_ConditionalTrueBlock2034
CheckScorpionShot_localfailed2038: ;keep:
	; ; logical OR, second chance
	; Swapped comparison expressions
	; Binary clause core: LESS
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld c, a
	ld b,$2
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a, b
	cp c
	jr nc,CheckScorpionShot_elsedoneblock2036
CheckScorpionShot_ConditionalTrueBlock2034: ;Main true block ;keep :
	ret
CheckScorpionShot_elsedoneblock2036:
	; Swapped comparison expressions
	; Binary clause core: LESS
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	ld c, a
	ld b,$3
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	add  a, b
	cp c
	jr nc,CheckScorpionShot_localfailed2045
	jr CheckScorpionShot_ConditionalTrueBlock2041
CheckScorpionShot_localfailed2045: ;keep:
	; ; logical OR, second chance
	; Binary clause core: LESS
	; Compare two vars optimization
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	ld b,a
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	cp b
	jr nc,CheckScorpionShot_elsedoneblock2043
CheckScorpionShot_ConditionalTrueBlock2041: ;Main true block ;keep :
	ret
CheckScorpionShot_elsedoneblock2043:
	
; // Passed these  conditions so bullet is touching part of the scorpion
	ld a, $1
	ld [scorpionEnemy_scorpion_record_scorpion_record_dead], a
	ld a, $0
	ld [scorpionEnemy_scorpion_record_scorpion_record_animFrame], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_x]
	ld [x], a
	; generic assign 
	ld a,[playerBullet_playerBullet_record_playerBullet_record_y]
	ld [y], a
	call DeleteBullet
	ld a, $0
	ld [playerBullet_playerBullet_record_playerBullet_record_fired], a
	ld a, $4
	ld [u], a
	call PlaySound
	ld a, $64
	ld [t], a
	call AddScore
	ret
	
; // Move scorpion
	; ***********  Defining procedure : MoveScorpion
	;    Procedure type : User-defined procedure
MoveScorpion:
	; 'a:=a + const'  optimization 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_moveSpeed]
	sub $1
	ld [scorpionEnemy_scorpion_record_scorpion_record_moveSpeed], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	cp $0
	jr nz,MoveScorpion_elseblock2050
MoveScorpion_ConditionalTrueBlock2049: ;Main true block ;keep :
	ld a, $4
	ld [scorpionEnemy_scorpion_record_scorpion_record_moveSpeed], a
	jr MoveScorpion_elsedoneblock2051
MoveScorpion_elseblock2050:
	ret
MoveScorpion_elsedoneblock2051:
	; 'a:=a + const'  optimization 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_animFrame]
	add  a,$1
	ld [scorpionEnemy_scorpion_record_scorpion_record_animFrame], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	cp $4
	jr nz,MoveScorpion_elsedoneblock2059
MoveScorpion_ConditionalTrueBlock2057: ;Main true block ;keep :
	ld a, $0
	ld [scorpionEnemy_scorpion_record_scorpion_record_animFrame], a
MoveScorpion_elsedoneblock2059:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_direction]
	cp $0
	jp z, MoveScorpion_elsedoneblock2065
MoveScorpion_ConditionalTrueBlock2063: ;Main true block ;keep :
	
; // Moving Right
; // Set start sprite
	ld a, $8
	ld [varPrefixed_p], a
	
; // Check for mushrooms under scorpion sprite left part
	; generic assign 
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	ld [v], a
	; generic assign 
	ld b,$6
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
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
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	cp $5
	jr nc,MoveScorpion_elseblock2087
MoveScorpion_localsuccess2091: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[u]
	cp $0
	jr c, MoveScorpion_elseblock2087
	jr z, MoveScorpion_elseblock2087
MoveScorpion_ConditionalTrueBlock2086: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[u]
	add  a,$4
	ld [u], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[u]
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	ld [v], a
	; generic assign 
	ld b,$6
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	; generic assign 
	ld a,[u]
	ld [t], a
	call SetGridLocVal
	jr MoveScorpion_elsedoneblock2088
MoveScorpion_elseblock2087:
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[u]
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
MoveScorpion_elsedoneblock2088:
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	cp $1d
	jr nc,MoveScorpion_elseblock2096
MoveScorpion_ConditionalTrueBlock2095: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a,$1
	ld [scorpionEnemy_scorpion_record_scorpion_record_x], a
	jr MoveScorpion_elsedoneblock2097
MoveScorpion_elseblock2096:
	ld a, $0
	ld [scorpionEnemy_scorpion_record_scorpion_record_enabled], a
	call ResetScorpionGrid
MoveScorpion_elsedoneblock2097:
MoveScorpion_elsedoneblock2065:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_direction]
	cp $0
	jp nz,MoveScorpion_elsedoneblock2105
MoveScorpion_ConditionalTrueBlock2103: ;Main true block ;keep :
	
; // Moving Left
; // Set start sprite
	ld a, $4
	ld [varPrefixed_p], a
	
; // Check for mushrooms under scorpion sprite right part
	; generic assign 
	; generic assign 
	ld b,$2
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a, b
	ld [v], a
	; generic assign 
	ld b,$6
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
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
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	cp $5
	jr nc,MoveScorpion_elseblock2127
MoveScorpion_localsuccess2131: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[u]
	cp $0
	jr c, MoveScorpion_elseblock2127
	jr z, MoveScorpion_elseblock2127
MoveScorpion_ConditionalTrueBlock2126: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[u]
	add  a,$4
	ld [u], a
	; generic assign 
	ld b,$2
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a, b
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[u]
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; generic assign 
	ld b,$2
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a, b
	ld [v], a
	; generic assign 
	ld b,$6
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	sub  b
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,screenYtogridY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'screenYtogridY' is word : 0
	ld [w], a
	; generic assign 
	ld a,[u]
	ld [t], a
	call SetGridLocVal
	jr MoveScorpion_elsedoneblock2128
MoveScorpion_elseblock2127:
	; generic assign 
	ld b,$2
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	add  a, b
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[u]
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
MoveScorpion_elsedoneblock2128:
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	cp $0
	jr c, MoveScorpion_elseblock2136
	jr z, MoveScorpion_elseblock2136
MoveScorpion_ConditionalTrueBlock2135: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	sub $1
	ld [scorpionEnemy_scorpion_record_scorpion_record_x], a
	jr MoveScorpion_elsedoneblock2137
MoveScorpion_elseblock2136:
	ld a, $0
	ld [scorpionEnemy_scorpion_record_scorpion_record_enabled], a
	call ResetScorpionGrid
MoveScorpion_elsedoneblock2137:
MoveScorpion_elsedoneblock2105:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_enabled]
	cp $0
	jr z, MoveScorpion_elsedoneblock2145
MoveScorpion_ConditionalTrueBlock2143: ;Main true block ;keep :
	
; // Play sound effect
	ld a, $7
	ld [u], a
	call PlaySound
	
; // Update sprite data
	ld hl,bigSprites
	ld [Sprite_spritedata],hl
	call Sprite_SetData
	ld a, $3
	ld [Sprite_spritewidth], a
	ld a, $4
	ld [Sprite_spriteheight], a
	call Sprite_SetSize
	
; // Draw scorpion sprite
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_x]
	ld [Sprite_spritex], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_y]
	ld [Sprite_spritey], a
	; generic assign 
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_animFrame]
	ld b,a
	ld a,[varPrefixed_p]
	add  a, b
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	
; // Reset sprite data
	ld hl,smallSprites
	ld [Sprite_spritedata],hl
	call Sprite_SetData
	ld a, $1
	ld [Sprite_spritewidth], a
	ld a, $4
	ld [Sprite_spriteheight], a
	call Sprite_SetSize
MoveScorpion_elsedoneblock2145:
	ret
	
; // Spawn scorpion
	; ***********  Defining procedure : SpawnScorpion
	;    Procedure type : User-defined procedure
SpawnScorpion:
	
; // Pick a random number to see if we spawn it
	; generic assign 
	; generic assign 
	ld hl,$64
	; Integer assignment 
	; Loading pointer
	ld [Functions_i],hl
	call Functions_getRandomByte
	ld [t], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	cp $7
	jp c, SpawnScorpion_elsedoneblock2152
	jp z, SpawnScorpion_elsedoneblock2152
SpawnScorpion_ConditionalTrueBlock2150: ;Main true block ;keep :
	
; // Spawn or not
; // Pick a random side of the screen to spawn a scorpion
	; generic assign 
	; generic assign 
	ld hl,$64
	; Integer assignment 
	; Loading pointer
	ld [Functions_i],hl
	call Functions_getRandomByte
	ld [t], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	cp $32
	jr c, SpawnScorpion_elseblock2169
	jr z, SpawnScorpion_elseblock2169
SpawnScorpion_ConditionalTrueBlock2168: ;Main true block ;keep :
	
; // Spawn on the random side
	ld a, $1d
	ld [scorpionEnemy_scorpion_record_scorpion_record_x], a
	; generic assign 
	; generic assign 
	ld hl,$9
	; Integer assignment 
	; Loading pointer
	ld [Functions_i],hl
	call Functions_getRandomByte
	ld [t], a
	; generic assign 
	; Generic mul
	ld a,$4
	ld e,a
	ld d,0
	ld a,[t]
	ld h,a
	ld l,0
	call mul_8x8
	ld a,l
	push af
	ld a,$6
	ld b,a
	pop af
	add  a, b
	ld [scorpionEnemy_scorpion_record_scorpion_record_y], a
	ld a, $0
	ld [scorpionEnemy_scorpion_record_scorpion_record_direction], a
	jr SpawnScorpion_elsedoneblock2170
SpawnScorpion_elseblock2169:
	ld a, $0
	ld [scorpionEnemy_scorpion_record_scorpion_record_x], a
	; generic assign 
	; generic assign 
	ld hl,$9
	; Integer assignment 
	; Loading pointer
	ld [Functions_i],hl
	call Functions_getRandomByte
	ld [t], a
	; generic assign 
	; Generic mul
	ld a,$4
	ld e,a
	ld d,0
	ld a,[t]
	ld h,a
	ld l,0
	call mul_8x8
	ld a,l
	push af
	ld a,$6
	ld b,a
	pop af
	add  a, b
	ld [scorpionEnemy_scorpion_record_scorpion_record_y], a
	ld a, $1
	ld [scorpionEnemy_scorpion_record_scorpion_record_direction], a
SpawnScorpion_elsedoneblock2170:
	ld a, $1
	ld [scorpionEnemy_scorpion_record_scorpion_record_enabled], a
	ld a, $0
	ld [scorpionEnemy_scorpion_record_scorpion_record_dead], a
	ld [scorpionEnemy_scorpion_record_scorpion_record_animFrame], a
	ld a, $4
	ld [scorpionEnemy_scorpion_record_scorpion_record_moveSpeed], a
SpawnScorpion_elsedoneblock2152:
	ret
	
; // If Millipede reaches the bottom, see if a new head can be spawned
	; ***********  Defining procedure : SpawnMillipedeHead
	;    Procedure type : User-defined procedure
SpawnMillipedeHead:
	ld a, $0
	ld [u], a
SpawnMillipedeHead_forloop2180:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[u]
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
	jp nz,SpawnMillipedeHead_elsedoneblock2212
SpawnMillipedeHead_ConditionalTrueBlock2210: ;Main true block ;keep :
	
; // Find unused millipede part in array
; // Pick a random side of the screen to spawn a new head
	; generic assign 
	; generic assign 
	ld hl,$1f
	; Integer assignment 
	; Loading pointer
	ld [Functions_i],hl
	call Functions_getRandomByte
	ld [t], a
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	cp $f
	jr c, SpawnMillipedeHead_elseblock2225
	jr z, SpawnMillipedeHead_elseblock2225
SpawnMillipedeHead_ConditionalTrueBlock2224: ;Main true block ;keep :
	
; // Spawn on the random side
	; Storing to array
	ld a,[u]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,$1f
	ld [hl],a
	; Storing to array
	ld a,[u]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,$59
	ld [hl],a
	jr SpawnMillipedeHead_elsedoneblock2226
SpawnMillipedeHead_elseblock2225:
	; Storing to array
	ld a,[u]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,$0
	ld [hl],a
	; Storing to array
	ld a,[u]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	ld a,$5a
	ld [hl],a
SpawnMillipedeHead_elsedoneblock2226:
	; Storing to array
	ld a,[u]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,$9
	ld [hl],a
	
; // Millipede y value is grid-based
	; Storing to array
	ld a,[u]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastGridVal
	add hl,de
	ld a,$0
	ld [hl],a
	; generic assign 
	ld a,[u]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	ld [v], a
	; generic assign 
	ld a,[u]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	ld [w], a
	ld a, $a
	ld [t], a
	call SetGridLocVal
	; generic assign 
	ld a,[u]
	ld [s], a
	call DrawMilliSprite
	ret
SpawnMillipedeHead_elsedoneblock2212:
SpawnMillipedeHead_forloopcounter2182:
SpawnMillipedeHead_loopstart2183:
	ld a,[u]
	add a,1
	ld [u],a
	cp $c
	jp nz,SpawnMillipedeHead_forloop2180
SpawnMillipedeHead_forloopend2181:
SpawnMillipedeHead_loopend2184:
	ret
	
; // Move Millipede
	; ***********  Defining procedure : MoveMillipede
	;    Procedure type : User-defined procedure
MoveMillipede:
	ld a, $0
	ld [varPrefixed_p], a
	ld [s], a
MoveMillipede_forloop2232:
	ld a, $0
	ld [q], a
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
	jp z, MoveMillipede_elsedoneblock6968
MoveMillipede_ConditionalTrueBlock6966: ;Main true block ;keep :
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
	jp nz,MoveMillipede_elseblock9333
MoveMillipede_ConditionalTrueBlock9332: ;Main true block ;keep :
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[varPrefixed_p]
	cp $0
	jp nz,MoveMillipede_elsedoneblock10517
MoveMillipede_ConditionalTrueBlock10515: ;Main true block ;keep :
	
; // Test if segment is not on grid
; // Only one segment at a time to go into the grid
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
	; Storing to array
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastGridVal
	add hl,de
	ld a,$0
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
	jr z, MoveMillipede_elseblock10530
MoveMillipede_ConditionalTrueBlock10529: ;Main true block ;keep :
	ld a, $a
	ld [v], a
	jr MoveMillipede_elsedoneblock10531
MoveMillipede_elseblock10530:
	ld a, $9
	ld [v], a
MoveMillipede_elsedoneblock10531:
	call DrawMilliSprite
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
MoveMillipede_elsedoneblock10517:
	jp MoveMillipede_elsedoneblock9334
MoveMillipede_elseblock9333:
	
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
	ld a, $0
	ld [Sprite_no], a
	ld [Sprite_tran], a
	call Sprite_DrawAt
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastGridVal
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_lastGridVal' is word : 0
	cp $9
	jp nc,MoveMillipede_elseblock10539
MoveMillipede_ConditionalTrueBlock10538: ;Main true block ;keep :
	
; // Millipede segments - draw whatever was on the grid before the segment was there.
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
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastGridVal
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_lastGridVal' is word : 0
	ld [t], a
	call SetGridLocVal
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
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastGridVal
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_lastGridVal' is word : 0
	ld [Sprite_no], a
	ld a, $0
	ld [Sprite_tran], a
	call Sprite_DrawAt
	jp MoveMillipede_elsedoneblock10540
MoveMillipede_elseblock10539:
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
MoveMillipede_elsedoneblock10540:
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
	jp z, MoveMillipede_elseblock10547
MoveMillipede_ConditionalTrueBlock10546: ;Main true block ;keep :
	
; // Only direction changing logic in millipede heads
	ld a, $0
	ld [varPrefixed_p], a
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
	jp z, MoveMillipede_elsedoneblock11124
MoveMillipede_ConditionalTrueBlock11122: ;Main true block ;keep :
	
; // Did we already switch R/L?
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
	call CheckMushroomShot
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[q]
	cp $8
	jr c, MoveMillipede_elsedoneblock11266
	jr z, MoveMillipede_elsedoneblock11266
MoveMillipede_ConditionalTrueBlock11264: ;Main true block ;keep :
	ld a, $0
	ld [q], a
MoveMillipede_elsedoneblock11266:
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
	jr nz,MoveMillipede_elsedoneblock11272
MoveMillipede_ConditionalTrueBlock11270: ;Main true block ;keep :
	
; // Don't detect millipede segments
	ld a, $7b
	ld [q], a
MoveMillipede_elsedoneblock11272:
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[q]
	cp $4
	jr c, MoveMillipede_elsedoneblock11278
	jr z, MoveMillipede_elsedoneblock11278
MoveMillipede_localsuccess11280: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[q]
	cp $9
	jr nc,MoveMillipede_elsedoneblock11278
MoveMillipede_ConditionalTrueBlock11276: ;Main true block ;keep :
	
; // A hack for edge of screen.
; // Check for poisoned mushroom
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
	ld a, $20
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
MoveMillipede_elsedoneblock11278:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[q]
	cp $0
	jp nz,MoveMillipede_elseblock11284
MoveMillipede_localsuccess11342: ;keep:
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
	ld a, $20
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jp nz,MoveMillipede_elseblock11284
MoveMillipede_ConditionalTrueBlock11283: ;Main true block ;keep :
	
; // Will we collide with a mushroom on the grid or screen edge?
; // Save current position
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastX
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastY
	add hl,de
	pop af
	ld [hl],a
	
; // Move one right
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
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
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
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
	call GetGridLocVal
	cp $9
	jr nc,MoveMillipede_elsedoneblock11347
MoveMillipede_ConditionalTrueBlock11345: ;Main true block ;keep :
	; Storing to array
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
	call GetGridLocVal
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastGridVal
	add hl,de
	pop af
	ld [hl],a
MoveMillipede_elsedoneblock11347:
	jp MoveMillipede_elsedoneblock11285
MoveMillipede_elseblock11284:
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
	jp z, MoveMillipede_elsedoneblock11354
MoveMillipede_ConditionalTrueBlock11352: ;Main true block ;keep :
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_enabled]
	cp $0
	jr nz,MoveMillipede_elsedoneblock11372
MoveMillipede_ConditionalTrueBlock11370: ;Main true block ;keep :
	
; // Try spawning the scorpion
	call SpawnScorpion
MoveMillipede_elsedoneblock11372:
	
; // Save current position
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastX
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastY
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
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
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
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
	call GetGridLocVal
	cp $9
	jr nc,MoveMillipede_elsedoneblock11378
MoveMillipede_ConditionalTrueBlock11376: ;Main true block ;keep :
	; Storing to array
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
	call GetGridLocVal
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastGridVal
	add hl,de
	pop af
	ld [hl],a
MoveMillipede_elsedoneblock11378:
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
	
; // Set Left direction
	ld a, $1
	ld [varPrefixed_p], a
MoveMillipede_elsedoneblock11354:
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
	jp z, MoveMillipede_elsedoneblock11384
MoveMillipede_ConditionalTrueBlock11382: ;Main true block ;keep :
	
; // Save current position
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastX
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastY
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	push af
	ld a,$1
	ld b,a
	pop af
	sub  b
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	pop af
	ld [hl],a
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
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
	call GetGridLocVal
	cp $9
	jr nc,MoveMillipede_elsedoneblock11396
MoveMillipede_ConditionalTrueBlock11394: ;Main true block ;keep :
	; Storing to array
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
	call GetGridLocVal
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastGridVal
	add hl,de
	pop af
	ld [hl],a
MoveMillipede_elsedoneblock11396:
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
	
; // Set Left direction
	ld a, $1
	ld [varPrefixed_p], a
MoveMillipede_elsedoneblock11384:
MoveMillipede_elsedoneblock11285:
MoveMillipede_elsedoneblock11124:
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
	jp z, MoveMillipede_elsedoneblock11402
MoveMillipede_localsuccess11516: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[varPrefixed_p]
	cp $0
	jp nz,MoveMillipede_elsedoneblock11402
MoveMillipede_ConditionalTrueBlock11400: ;Main true block ;keep :
	
; // End moving down
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
	call CheckMushroomShot
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[q]
	cp $8
	jr c, MoveMillipede_elsedoneblock11521
	jr z, MoveMillipede_elsedoneblock11521
MoveMillipede_ConditionalTrueBlock11519: ;Main true block ;keep :
	ld a, $0
	ld [q], a
MoveMillipede_elsedoneblock11521:
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
	jr nz,MoveMillipede_elsedoneblock11527
MoveMillipede_ConditionalTrueBlock11525: ;Main true block ;keep :
	
; // Don't detect millipede segments
	ld a, $7b
	ld [q], a
MoveMillipede_elsedoneblock11527:
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[q]
	cp $4
	jr c, MoveMillipede_elsedoneblock11533
	jr z, MoveMillipede_elsedoneblock11533
MoveMillipede_localsuccess11535: ;keep:
	; ; logical AND, second requirement
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[q]
	cp $9
	jr nc,MoveMillipede_elsedoneblock11533
MoveMillipede_ConditionalTrueBlock11531: ;Main true block ;keep :
	
; // A hack for edge of screen.
; // Check for poisoned mushroom
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
	ld a, $20
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
MoveMillipede_elsedoneblock11533:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[q]
	cp $0
	jp nz,MoveMillipede_elseblock11539
MoveMillipede_localsuccess11585: ;keep:
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
	ld a, $20
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jp nz,MoveMillipede_elseblock11539
MoveMillipede_ConditionalTrueBlock11538: ;Main true block ;keep :
	
; // Will we collide with a mushroom on the grid or screen edge?
; // Save current position
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastX
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastY
	add hl,de
	pop af
	ld [hl],a
	
; // Move one left
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	push af
	ld a,$1
	ld b,a
	pop af
	sub  b
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	pop af
	ld [hl],a
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
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
	call GetGridLocVal
	cp $9
	jr nc,MoveMillipede_elsedoneblock11590
MoveMillipede_ConditionalTrueBlock11588: ;Main true block ;keep :
	; Storing to array
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
	call GetGridLocVal
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastGridVal
	add hl,de
	pop af
	ld [hl],a
MoveMillipede_elsedoneblock11590:
	jp MoveMillipede_elsedoneblock11540
MoveMillipede_elseblock11539:
	
; // Set Left direction
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
	jp z, MoveMillipede_elsedoneblock11597
MoveMillipede_ConditionalTrueBlock11595: ;Main true block ;keep :
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_enabled]
	cp $0
	jr nz,MoveMillipede_elsedoneblock11609
MoveMillipede_ConditionalTrueBlock11607: ;Main true block ;keep :
	
; // Try spawning the scorpion
	call SpawnScorpion
MoveMillipede_elsedoneblock11609:
	
; // Save current position
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastX
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastY
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
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
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
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
	call GetGridLocVal
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastGridVal
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
	ld a, $2
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
	call ResetBit
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
MoveMillipede_elsedoneblock11597:
	
; // Set Left direction
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
	jp z, MoveMillipede_elsedoneblock11615
MoveMillipede_ConditionalTrueBlock11613: ;Main true block ;keep :
	
; // Save current position
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastX
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastY
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	push af
	ld a,$1
	ld b,a
	pop af
	sub  b
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	pop af
	ld [hl],a
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
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
	call GetGridLocVal
	cp $9
	jr nc,MoveMillipede_elsedoneblock11627
MoveMillipede_ConditionalTrueBlock11625: ;Main true block ;keep :
	; Storing to array
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
	call GetGridLocVal
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastGridVal
	add hl,de
	pop af
	ld [hl],a
MoveMillipede_elsedoneblock11627:
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
	ld a, $2
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
	call ResetBit
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_flags
	add hl,de
	pop af
	ld [hl],a
MoveMillipede_elsedoneblock11615:
MoveMillipede_elsedoneblock11540:
MoveMillipede_elsedoneblock11402:
	
; // End moving left
; // Test if reached top or bottom of area
	ld a, $0
	ld [varPrefixed_p], a
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
	jp z, MoveMillipede_elsedoneblock11633
MoveMillipede_ConditionalTrueBlock11631: ;Main true block ;keep :
	; Binary clause core: GREATER
	; Compare with pure num / var optimization
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	cp $d
	jp c, MoveMillipede_elsedoneblock11657
	jp z, MoveMillipede_elsedoneblock11657
MoveMillipede_ConditionalTrueBlock11655: ;Main true block ;keep :
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
	
; // Set moving up	
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
	; Storing to array
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,$d
	ld [hl],a
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
	ld a, $20
	ld [BitFlag], a
	call IsBitSet
	cp $0
	jr z, MoveMillipede_elsedoneblock11669
MoveMillipede_ConditionalTrueBlock11667: ;Main true block ;keep :
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
	ld a, $20
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
MoveMillipede_elsedoneblock11669:
	call SpawnMillipedeHead
MoveMillipede_elsedoneblock11657:
MoveMillipede_elsedoneblock11633:
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
	jr z, MoveMillipede_elsedoneblock11675
MoveMillipede_ConditionalTrueBlock11673: ;Main true block ;keep :
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	cp $a
	jr nc,MoveMillipede_elsedoneblock11687
MoveMillipede_ConditionalTrueBlock11685: ;Main true block ;keep :
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
	
; // Set moving up
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
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
MoveMillipede_elsedoneblock11687:
MoveMillipede_elsedoneblock11675:
	call DrawMilliSprite
	jp MoveMillipede_elsedoneblock10548
MoveMillipede_elseblock10547:
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_x' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastX
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld a,[s]
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_y' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastY
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld b,$1
	ld a,[s]
	sub  b
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastX
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_lastX' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_x
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	ld b,$1
	ld a,[s]
	sub  b
	ld e,a ; variable is 8-bit
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastY
	add hl,de
	ld a,[hl]
	; LoadVar Testing if 'milliSegments_milliSegments_record_milliSegments_record_lastY' is word : 0
	push af
	ld a,[s]
	ld e,a
	ld hl,milliSegments_milliSegments_record_milliSegments_record_y
	add hl,de
	pop af
	ld [hl],a
	; Storing to array
	; generic assign 
	ld a,[s]
	ld e,a ; variable is 8-bit
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
	call GetGridLocVal
	push af
	ld a,[s]
	ld e,a
	ld d,0
	ld hl,milliSegments_milliSegments_record_milliSegments_record_lastGridVal
	add hl,de
	pop af
	ld [hl],a
	call DrawMilliSprite
MoveMillipede_elsedoneblock10548:
MoveMillipede_elsedoneblock9334:
MoveMillipede_elsedoneblock6968:
	
; // Reset mushroom just shot storage
	ld a, $0
	ld [mushroomJustShot_mushroomJustShot_record_mushroomJustShot_record_x], a
	ld [mushroomJustShot_mushroomJustShot_record_mushroomJustShot_record_y], a
MoveMillipede_forloopcounter2234:
MoveMillipede_loopstart2235:
	ld a,[s]
	add a,1
	ld [s],a
	cp $c
	jp nz,MoveMillipede_forloop2232
MoveMillipede_forloopend2233:
MoveMillipede_loopend2236:
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
MainProgram_while11692:
MainProgram_loopstart11696:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[gameStats_gameStats_record_gameStats_record_gameRunning]
	cp $0
	jp z, MainProgram_elsedoneblock11695
MainProgram_ConditionalTrueBlock11693: ;Main true block ;keep :
	
; // Keep game going until gameRunning is false(Game over)
; // Sync everything so the speed is correct
	call Screen_WaitForVerticalBlank
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[scorpionEnemy_scorpion_record_scorpion_record_enabled]
	cp $0
	jr z, MainProgram_elsedoneblock11769
MainProgram_ConditionalTrueBlock11767: ;Main true block ;keep :
	
; // Try spawning scorpion
	call MoveScorpion
	call CheckScorpionShot
MainProgram_elsedoneblock11769:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_enabled]
	cp $0
	jr z, MainProgram_elsedoneblock11775
MainProgram_ConditionalTrueBlock11773: ;Main true block ;keep :
	; Binary clause core: LESS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_shotCount]
	cp $2
	jr nc,MainProgram_elsedoneblock11793
MainProgram_ConditionalTrueBlock11791: ;Main true block ;keep :
	
; // Only move flea if it's not dead.
	call MoveFlea
MainProgram_elsedoneblock11793:
	
; // Check if flea collided with player
	call CheckFleaCollide
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[playerBullet_playerBullet_record_playerBullet_record_fired]
	cp $0
	jr z, MainProgram_elsedoneblock11799
MainProgram_ConditionalTrueBlock11797: ;Main true block ;keep :
	
; // Check if flea was shot
	call CheckFleaShot
MainProgram_elsedoneblock11799:
MainProgram_elsedoneblock11775:
	
; // Move Millipede
	; 'a:=a + const'  optimization 
	ld a,[gameStats_gameStats_record_gameStats_record_milliSpeed]
	sub $1
	ld [gameStats_gameStats_record_gameStats_record_milliSpeed], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	cp $0
	jr nz,MainProgram_elsedoneblock11805
MainProgram_ConditionalTrueBlock11803: ;Main true block ;keep :
	ld a, $7
	ld [gameStats_gameStats_record_gameStats_record_milliSpeed], a
	call MoveMillipede
MainProgram_elsedoneblock11805:
	; 'a:=a + const'  optimization 
	ld a,[playerSnake_playerSnake_record_playerSnake_record_moveCount]
	sub $1
	ld [playerSnake_playerSnake_record_playerSnake_record_moveCount], a
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	cp $0
	jr nz,MainProgram_elsedoneblock11811
MainProgram_ConditionalTrueBlock11809: ;Main true block ;keep :
	ld a, $3
	ld [playerSnake_playerSnake_record_playerSnake_record_moveCount], a
	call ControlPlayer
MainProgram_elsedoneblock11811:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[playerBullet_playerBullet_record_playerBullet_record_fired]
	cp $0
	jr z, MainProgram_elsedoneblock11817
MainProgram_ConditionalTrueBlock11815: ;Main true block ;keep :
	call MoveBullet
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	ld a,[fleaEnemy_flea_record_flea_record_enabled]
	cp $0
	jr z, MainProgram_elsedoneblock11829
MainProgram_ConditionalTrueBlock11827: ;Main true block ;keep :
	call CheckFleaShot
MainProgram_elsedoneblock11829:
	call MoveBullet
MainProgram_elsedoneblock11817:
	
; // Test if any enemies shot and if so, animate sprite
	call DoEnemyDeathAnim
	jp MainProgram_while11692
MainProgram_elsedoneblock11695:
MainProgram_loopend11697:
MainProgram_end11832:
	;nop
	jr MainProgram_end11832
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
