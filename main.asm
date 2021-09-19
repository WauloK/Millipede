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
data:
	incbin	 "D:/VZ/TRSE_Mysrc/Millipede///title.bin_c"
mychar:		db	"A"
	db	0
myp: dw  0
mym: dw  0
startgame:	db	$00
strlen:	db	$d2
strpos:	db	$00
	gameGrid:	 ds 448
	milliBuffer:	 ds 12
gridRandoms:	db $06, $04, $05, $04, $00, $02, $00, $04
	db $02, $06, $01, $03, $04, $00, $00, $05
	db $02, $03, $02, $06, $00, $03, $04, $00
	db $04, $02, $00, $00, $01, $04, $05, $06
	db $05, $06, $04, $06, $04, $05, $01, $02
	db $03, $06, $04, $02, $00, $06, $01, $00
	db $04, $04, $05, $01, $05, $00, $01, $04
	db $01, $05, $02, $03, $01, $04, $03, $05
	db $02, $05, $01, $00, $03, $01, $06, $06
	db $05, $04, $00, $04, $05, $01, $05, $02
	db $04, $03, $03, $01, $03, $04, $05, $05
	db $03, $03, $01, $02, $05, $02, $01, $04
	db $03, $00, $06, $00, $05, $00, $05, $01
	db $06, $06, $06, $01, $02, $00, $04, $02
	db $02, $04, $01, $02, $04, $06, $03, $02
	db $04, $05, $01, $04, $01, $01, $02, $04
	db $04, $01, $03, $01, $03, $01, $00, $02
	db $00, $01, $01, $01, $06, $00, $02, $02
	db $03, $01, $06, $01, $01, $03, $01, $06
	db $02, $04, $05, $05, $04, $05, $04, $05
	db $01, $03, $00, $00, $00, $03, $05, $06
	db $01, $01, $05, $01, $01, $00, $03, $06
	db $00, $06, $00, $02, $06, $01, $02, $05
	db $04, $02, $01, $04, $05, $00, $00, $00
	db $02, $00, $00, $04, $00, $05, $00, $06
	db $05, $01, $03, $01, $03, $03, $01, $01
	db $02, $04, $00, $06, $03, $06, $02, $02
	db $03, $00, $05, $06, $00, $00, $05, $00
	db $06, $01, $02, $01, $00, $00, $00, $05
	db $01, $02, $00, $06, $00, $02, $00, $04
	db $00, $02, $00, $03, $00, $00, $02, $03
	db $00, $05, $05, $00, $02, $06, $05, $01
u:	db	0
v:	db	0
level:	db	0
lives:	db	0
score:	dw	0
message:		db	"Press 'S' to start! You can use either Joystick or Keys:QA = Up/Down and <> = Left/Right Space to fire. Millipede was developed by Jason Oakley / Blue Bilby using TRSE. Check out more stuff at BlueBilby.com ... "
	db	0
	 
; //	Clears the screen using mode parameter 0 or 1
; //	
	; ***********  Defining procedure : Screen_Cls
	;    Procedure type : User-defined procedure
 ; Temp vars section
DrawHUD_stringassignstr293: db "Score:",0
DrawHUD_stringassignstr295: db "Lives:",0
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
    ld a,(hl)
    xor d
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
; //  *
	; ***********  Defining procedure : Functions_IntegerToString
	;    Procedure type : User-defined procedure
Functions_IntegerToString_block181:
Functions_IntegerToString:
	; ****** Inline assembler section
    ld bc,#0000
    ld de,[Functions_i]
    call #09b4      ; Loads ACC with BCDE
    //call #0a9d      ; Set flag to Integer 
    ld a,#02
    ld(#78af),a   
    call #0fbd      ; Convert ACC to ASCII string 0-delim HL points to string  
    ld [Functions_s],hl
tstlp jp tstlp    
  
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
	; ***********  Defining procedure : TitleScreen
	;    Procedure type : User-defined procedure
TitleScreen:
	ld hl,mychar
	ld [myp],hl
	ld hl,message
	ld [mym],hl
	ld a, $0
	ld [v], a
TitleScreen_while184:
TitleScreen_loopstart188:
	; Binary clause core: NOTEQUALS
	; Compare with pure num / var optimization
	call Input_GetPressedKey
	cp $27
	jp z, TitleScreen_elsedoneblock187
TitleScreen_ConditionalTrueBlock185: ;Main true block ;keep :
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[v]
	cp $0
	jr nz,TitleScreen_elsedoneblock226
TitleScreen_ConditionalTrueBlock224: ;Main true block ;keep :
	
; // Keep titlescreen going until S pressed
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
	call Font_DrawTextAt
	; Binary clause core: LESS
	; Compare two vars optimization
	ld a,[strlen]
	ld b,a
	ld a,[strpos]
	cp b
	jr nc,TitleScreen_elseblock239
TitleScreen_ConditionalTrueBlock238: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[strpos]
	add  a,$1
	ld [strpos], a
	jr TitleScreen_elsedoneblock240
TitleScreen_elseblock239:
	ld a, $0
	ld [strpos], a
TitleScreen_elsedoneblock240:
TitleScreen_elsedoneblock226:
	ld a, $0
	ld [u], a
TitleScreen_forloop245:
	; Wait
	ld a,$32
TitleScreen_wait253:
	sub 1
	jr nz,TitleScreen_wait253
TitleScreen_forloopcounter247:
TitleScreen_loopstart248:
	ld a,[u]
	add a,1
	ld [u],a
	cp $c8
	jr nz,TitleScreen_forloop245
TitleScreen_forloopend246:
TitleScreen_loopend249:
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
	jp TitleScreen_while184
TitleScreen_elsedoneblock187:
TitleScreen_loopend189:
	ret
	; ***********  Defining procedure : PlaySound
	;    Procedure type : User-defined procedure
PlaySound_block254:
PlaySound:
	; Binary clause core: EQUALS
	; Compare with pure num / var optimization
	ld a,[u]
	cp $1
	jr nz,PlaySound_elsedoneblock258
PlaySound_ConditionalTrueBlock256: ;Main true block ;keep :
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
PlaySound_elsedoneblock258:
	ret
	; ***********  Defining procedure : DrawPadded
	;    Procedure type : User-defined procedure
zscore:	dw	0
x:	db	0
y:	db	0
DrawPadded_block261:
DrawPadded:
	; Binary clause core: LESS
	; Binary clause INTEGER: LESS
	ld hl,$10000
	push hl
	; Variable is 16-bit
	; Integer
	ld hl,[zscore]
	pop de
	xor a ; clear carry
	sbc hl,de
	jr nc,DrawPadded_elsedoneblock265
DrawPadded_ConditionalTrueBlock263: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock265:
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
	jr nc,DrawPadded_elsedoneblock271
DrawPadded_ConditionalTrueBlock269: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock271:
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
	jr nc,DrawPadded_elsedoneblock277
DrawPadded_ConditionalTrueBlock275: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock277:
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
	jr nc,DrawPadded_elsedoneblock283
DrawPadded_ConditionalTrueBlock281: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock283:
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
	jr nc,DrawPadded_elsedoneblock289
DrawPadded_ConditionalTrueBlock287: ;Main true block ;keep :
	; 'a:=a + const'  optimization 
	ld a,[x]
	add  a,$1
	ld [x], a
DrawPadded_elsedoneblock289:
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
	call Font_DrawTextAt
	ret
	; ***********  Defining procedure : DrawHUD
	;    Procedure type : User-defined procedure
DrawHUD:
	
; // Draw lives
	ld a, $2
	ld [Screen_c], a
	call Screen_SetPen
	; Assigning a string : Font_text
	ld hl,DrawHUD_stringassignstr293
	; Loading pointer
	ld [Font_text],hl
	ld a, $14
	ld [Font_tx], a
	ld a, $0
	ld [Font_ty], a
	call Font_DrawTextAt
	ld a, $1
	ld [Screen_c], a
	call Screen_SetPen
	; generic assign 
	ld hl,$8000
	; Integer assignment 
	; Loading pointer
	ld [zscore],hl
	ld a, $19
	ld [x], a
	ld a, $0
	ld [y], a
	call DrawPadded
	ld a, $2
	ld [Screen_c], a
	call Screen_SetPen
	; Assigning a string : Font_text
	ld hl,DrawHUD_stringassignstr295
	; Loading pointer
	ld [Font_text],hl
	ld a, $0
	ld [Font_tx], a
	ld [Font_ty], a
	call Font_DrawTextAt
	ld a, $1
	ld [Screen_c], a
	call Screen_SetPen
	; generic assign 
	ld a,[lives]
	ld [Functions_j], a
	call Functions_ByteToString
	ld hl,[Functions_s]
	ld [Font_text],hl
	ld a, $6
	ld [Font_tx], a
	ld a, $0
	ld [Font_ty], a
	call Font_DrawTextAt
	ret
	; ***********  Defining procedure : InitialiseGame
	;    Procedure type : User-defined procedure
InitialiseGame:
	ld a, $0
	ld [v], a
InitialiseGame_forloop298:
	
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
InitialiseGame_forloop314:
	; Wait
	ld a,$8c
InitialiseGame_wait322:
	sub 1
	jr nz,InitialiseGame_wait322
InitialiseGame_forloopcounter316:
InitialiseGame_loopstart317:
	ld a,[u]
	add a,1
	ld [u],a
	cp $1e
	jr nz,InitialiseGame_forloop314
InitialiseGame_forloopend315:
InitialiseGame_loopend318:
InitialiseGame_forloopcounter300:
InitialiseGame_loopstart301:
	ld a,[v]
	add a,1
	ld [v],a
	cp $20
	jr nz,InitialiseGame_forloop298
InitialiseGame_forloopend299:
InitialiseGame_loopend302:
	ld a, $0
	ld [u], a
InitialiseGame_forloop323:
	
; // Playfield grid array
	; Storing to array
	ld hl,gridRandoms
	ld [Functions_p],hl
	call Functions_getRandFromArray
	push af
	ld a,[u]
	ld e,a
	ld d,0
	ld hl,gameGrid
	add hl,de
	pop af
	ld [hl],a
InitialiseGame_forloopcounter325:
InitialiseGame_loopstart326:
	ld a,[u]
	add a,1
	ld [u],a
	cp $1c0
	jr nz,InitialiseGame_forloop323
InitialiseGame_forloopend324:
InitialiseGame_loopend327:
	ld a, $0
	ld [u], a
InitialiseGame_forloop330:
	
; // Millipede buffer array
	; Storing to array
	ld a,[u]
	ld e,a
	ld d,0
	ld hl,milliBuffer
	add hl,de
	ld a,$5
	ld [hl],a
InitialiseGame_forloopcounter332:
InitialiseGame_loopstart333:
	ld a,[u]
	add a,1
	ld [u],a
	cp $b
	jr nz,InitialiseGame_forloop330
InitialiseGame_forloopend331:
InitialiseGame_loopend334:
	
; // Initialise variables
	ld a, $3
	ld [lives], a
	; generic assign 
	ld hl,$0
	; Integer assignment 
	; Loading pointer
	ld [score],hl
	call DrawHUD
	ret
block1:
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
MainProgram_end338:
	;nop
	jr MainProgram_end338
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
