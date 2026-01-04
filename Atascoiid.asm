                run start

                ROWS = 8                        ; Number of rows with blocks
                COLS = 16                       ; Number of columns with block
                LINESPERROW = 16                ; Number of scanlines per row
                ROWSIZE = (LINESPERROW * 3) + 1 ; Memory used for display list for each row
                SCREEN = $3030                  ; Address of screen memory. Identical high and low byte saves space when (re)setting the value
    			PMBASEADR = $3c00		        ; Player/missile data locations
                PMSHAPE = $3c00+$200+$6c        ; Location of P/M shape
	    		STARTPOSPH = 114			    ; Horizontal starting position of player
		    	STARTPOSPV = 100			    ; Vertical starting position of player
                STARTPOSMV = 106                ; Vertical starting position of missile

                RTCLOK = $14
                ATRACT = $4d
                SDMCTL = $22f
                PADDL2 = $272
                PTRIG2 = $27e
                PTRIG3 = $27f
                STICK0 = $278
                STRIG0 = $284
                STRIG1 = $285
                PCOLR0 = $2c0
                PCOLR1 = $2c1
                CHBAS = $2f4
                CH = $2fc
                HPOSP0 = $d000
                M1PF = $d001
                HPOSM1 = $d005
                SIZEP0 = $d008
                M1PL = $d009
                COLPM0 = $d012
                COLPM1 = $d013
                COLPF0 = $d016
                COLPF1 = $d017
                COLPF2 = $d018
                COLPF3 = $d019
                COLBK = $d01a
                PRIOR = $d01b
    			GRACTL = $d01d
                HITCLR = $d01e
                CONSOL = $d01f
                AUDF1 = $d200
                AUDC1 = $d201
                AUDCTL = $d208
                RANDOM = $d20a
    			PMBASE = $d407
				NMIEN = $d40e
				WSYNC = $d40a
				VCOUNT = $d40b

DLITABLE        = $80                           ; Color values for DLI routine
ENDDLITABLE     = DLITABLE+16                   ; Color table size is 16 bytes
/*
DLITABLE        .byte 0, 2, 4, 6, 8, 10, 12, 14, 14, 12, 10, 8, 6, 4, 2, 0
ENDDLITABLE
*/
;DIV1            = $90                           ; Dividend for division routine 
;DIV2            = $91                           ; Divisor for division routine
TEMP            = $90                           ; Variable for temporary usage
TEMPVBI         = $91                           ; Variable for temporary usage in VBI
TEMPADDR        = $92                           ; Word - Varialbe for temporary address
TILEADDR        = $94                           ; Word - Address for location of tile in screen memory
OLDTILEADDR     = $96                           ; Word - Previous address for location of tile in screen memory
PLAYSOUND       = $98                           ; Flag whether to play sound effect or not
DIRECTION       = $99                           ; Direction of ball: Bit 7: Left (1) / Right (0), Bit 6: Up (1) / Down (0) 
M1POSH          = $9a                           ; Horizontal position of missile
M1POSV          = $9b                           ; Vertical position of missile

                org M1POSV+1
P0POSH          .byte STARTPOSPH                ; Horizontal position of player
CHRADR          .word $e200                     ; Word - Pointer to character set data
GAMEOVER        .byte 1                         ; Game status flag: 0 = normal game, positive value: game lost, negative value: game won
MULTITABLE      .byte $30, $33, $35, $38, $3a, $3d, $3f, $42, $44, $47, $49, $4c, $4e, $51, $53, $56, $58   ; Multiplication table to multiply by three and also add $30 as needed to get the screen memory location
SCORETEXT       dta d "SCORE:00000"             ; Score text
textline1       dta d "1K ATASCOIID/F.HOLST"*   ; Title text
textline2       dta d " ASC 2025   HI:"         ; ABBUC Software Competition 2025 text, high score text
HISCORE         dta d "00000"

                org $2100                       ; Start at $2100 in order for display list to start after 1k-boundary after $2400.
start
                ldx #7                          ; Generate the DLI color table. X is the counter for memory location
                ldy #14                         ; Y is the color value, Y is easier to decrement than memory locations/accumulator
tableloop       tya                             ; get color value into accumulator
                sta DLITABLE,X                  ; store it in the DLI color table
;                sec                             ; SEC and CLC should be used here and below, but since colors are the same for two adjacent values, it doesn't make a difference here.
                sbc #14                         ; Subtracting 14 and then
                eor #$ff                        ; XOR'ing the value with $ff
;                clc
                adc #1                          ; and then adding one gives us the "opposite color" in the rainbow effect. So e.g. 12 - 14 = -2 = $fe, XOR'ed with $ff gives 1, plus 1 = 2. Both values always add up to 14. 
                sta DLITABLE+8,X                ; then store it in the second half of the DLI color table
                dey                             ; decrease color value by two because there are only 128 colors
                dey
                dex                             ; and decrease the table position
                bpl tableloop                   ; do that eight times

                ldx #LINESPERROW                ; Create the displaylist with LINESPERROW number of lines per row.
                ldy #0                          ; Y is the index to the dislay list
                sty AUDCTL                      ; reuse it to set POKEY configuration
nextline        lda #$0f+$40                    ; Every line is ANTIC mode $0F with a Load Memory Scan (LMS)
                sta DLISTGEN,y                  ; Store it in the display list
                iny                             ; Next memory location
                lda #<SCREEN                    ; Now comes the LMS address. Since we store the first line at $3030, we can save two byte here
                sta DLISTGEN,y                  ; Store the LSB of the LMS address
                iny                             ; Next memory location
                sta DLISTGEN,y                  ; Store the MSB of the LMS address
                iny                             ; Next memory location
                dex                             ; Done one scan line
                bne nextline                    ; Have we done all 16 lines per row?
                lda #$80+$10                    ; At the end of each row are two blank scan line to separate the rows
                sta DLISTGEN,y                  ; Store in display list

;                ldx #0                          ; X counts from 0 to 2. On 2 (i.e. every third byte), TEMP is increased
                stx TEMP
                ldx #6
                lda #<(DLISTGEN+ROWSIZE)
                sta TEMPADDR
                lda #>(DLISTGEN+ROWSIZE)
                sta TEMPADDR+1
dlrowloop       txa
                pha
                inc TEMP
                ldy #0                          ; now copy this one row 7 more times. Y contains the byte position in each line (0 to 49)
                ldx #0
dllineloop      lda DLISTGEN,Y
                cpx #2
                bne @+
                clc
                adc TEMP
                ldx #$ff                        ; Set X to $ff so that after the next INX it will be zero.
@               inx
                sta (TEMPADDR),Y
                iny                     ; Next byte
                cpy #ROWSIZE
                bne dllineloop              ; No? Then next one.
                pla
                tax
                dex
                bmi setup
                lda TEMPADDR
;                clc
                adc #ROWSIZE-1                  ; Carry flag is always set due to BNE above, so we need to add one less then and can omit the CLC
                sta TEMPADDR
                bcc @+
                inc TEMPADDR+1
@               bne dlrowloop
/*
                lda #0
                sta TEMP                ; TEMP contains the value from 1 to 11 to be added to the MSB of the LMS address (i.e. $3030, $3130, $3230 etc.)
                lda DLISTGEN,y          ; Load value from first row
                cpx #2                  ; Third byte already?
                bne @+                  ; No?
                ldx #0                  ; Otherwise reset X counter
                inc TEMP                ; And increase TEMP
                bne @+1                 ; Always non-zero, so skip increasing X register
@               inx                     ; Next byte
@               clc                     ; Clear carry just in case
                adc TEMP                ; Add 1 to 11 to MSB of LSB
                sta DLISTGEN+ROWSIZE*1,y     ; Store it in second row
                adc TEMP                ; Add 1 to 11 to MSB of LSB
                sta DLISTGEN+ROWSIZE*2,y     ; Store it in next row
                adc TEMP                ; Add 1 to 11 to MSB of LSB
                sta DLISTGEN+ROWSIZE*3,y     ; Store it in next row
                adc TEMP                ; Add 1 to 11 to MSB of LSB
                sta DLISTGEN+ROWSIZE*4,y     ; Store it in next row
                adc TEMP                ; Add 1 to 11 to MSB of LSB
                sta DLISTGEN+ROWSIZE*5,y     ; Store it in next row
                adc TEMP                ; Add 1 to 11 to MSB of LSB
                sta DLISTGEN+ROWSIZE*6,y     ; Store it in next row
                adc TEMP                ; Add 1 to 11 to MSB of LSB
                sta DLISTGEN+ROWSIZE*7,y     ; Store it in next row
                iny                     ; Next byte
                cpy #ROWSIZE           ; All lines already?
                bne dlloop              ; No? Then next one.
*/
setup           lda #46
                sta SDMCTL
                lda #3
                sta GRACTL
                sta SIZEP0
    			lda #>PMBASEADR
	    		sta PMBASE
                lda #<VBI               ; Load LSB of VBI routine
                sta 548                 ; And store it in vector
                lda #>VBI               ; Load MSB of VBI routine
                sta 549                 ; And store it in vector
				lda #<DLI               ; Load LSB of DLI routine
				sta 512                 ; And store it in vector
				lda #>DLI               ; Load MSB of DLI routine
				sta 513                 ; And store it in vector
                lda #<DLIST             ; Load LSB of display list
                sta 560                 ; And store it in vector
                lda #>DLIST             ; Load MSB of display list
                sta 561                 ; And store it in vector
                lda #192
                sta NMIEN               ; Enable VBI and DLI

newgame
                lda #STARTPOSMV
                sta M1POSV
                sta TEMP                ; STARTPOSMV has bit 6 as 1, use this for later testing with BIT
                lda #>SCREEN            ; Before each new game
                sta TEMPADDR            ; restore address
                sta TEMPADDR+1          ; to SCREEN (equals $3030, to prevent loading different values here)
                sta SMC2+2
                sta SMC3+2
                ldx #ROWS               ; Y is line counter
fillloop        ldy #ENDTILE-TILE+1       ; X is the index to the line's tile representations, start at the end
                jsr movesound
fillline        lda TILE,y              ; Load the color from the palettelist
SMC1            sta (TEMPADDR),y        ; Store it in SCREEN memory area. The target address gets modified during runtime.
                dey                     ; Next byte of tile representation
                bpl fillline            ; Not done? Then next tile in line.
                inc TEMPADDR+1          ; Since we put each line in increments of $100 starting from $3030, we can just modify the MSB here by adding 1.
                dex                     ; Next line
                bne fillloop            ; Not done? Then next line.

/*
                lda CHBAS
                clc
                adc #2
                sta CHRADR+1
*/
                lda RANDOM
                and #%11111000
                sta CHRADR

checkborder     iny                         ; Y needs to be 0, but is 255 before, so increase it to get to 0
;                ldy #0                      ; Byte counter
                lda (CHRADR),Y
                lsr
                bcc noborder
                sty TEMP
noborder        cpy #7
                bne checkborder
nextbyte        ldx #7                      ; Bit counter
                jsr movesound
                lda (CHRADR),y
                bit TEMP
                bvc nextbit
                lsr
nextbit         asl
                pha
;                bcs not_blank
                txa
                ldx #3                      ; Temporarily use X as counter: Three or two bytes to delete?
                pha
                and #1
                bne is_even
                dex                         ; Odd = two bytes only
is_even         txa
                eor #%00000001
                pha
next_pixel      bcs not_blank
                lda #0
;; ##TRACE "Akku = %d" @a
SMC2            sta SCREEN
not_blank       inc SMC2+1
                dex
                bne next_pixel
                pla
                tax
next_pixel2     bcs not_blank2
                lda #0
SMC3            sta SCREEN+39
not_blank2      dec SMC3+1
                dex
                bne next_pixel2
                pla
                tax
                pla
                dex
                bpl nextbit
                inc SMC2+2
                inc SMC3+2
                lda #<SCREEN
                sta SMC2+1
                lda #<SCREEN+39
                sta SMC3+1
                dey
                bpl nextbyte

waitstart
                ldy #10
                lda GAMEOVER
                beq loop
                bpl waittrig
waitcycle       lda RTCLOK
                sta GAMEOVER
                bmi waitcycle
                ldy #0
waittrig        lda PTRIG2
                and STRIG0
                bne waittrig
;                lda #0
                sta GAMEOVER
                lda P0POSH
                adc #13
                sta M1POSH
@               lda SCORETEXT,y
                sta textline2,y
                dey
                bpl @-

loop            lda TILEADDR                ; calctilepos
                sta OLDTILEADDR
                lda TILEADDR+1
                sta OLDTILEADDR+1
                lda M1POSV
                sec
                sbc #15

                ldx #$ff                    ; divide routine
@               inx
                sbc #9
                bcs @-

                txa
                clc
                adc #$30
                sta TILEADDR+1
                lda M1POSH
                sec
                sbc #49

                ldx #$ff                    ; divide routine
@               inx
                sbc #10
                bcs @-

                lda MULTITABLE,X
                sta TILEADDR
;                rts

                ldy #0
                sty ATRACT
                lda (TILEADDR),Y
                beq nocolchange

                ldy #4
scoreloop       lda textline2+5,y
                clc
                adc #1
                cmp #":"
                bne writescore
                lda #"0"
                sta textline2+5,y
                dey
                bne scoreloop
writescore      sta textline2+5,y

tilecompare     lda TILEADDR+1
                cmp OLDTILEADDR+1
                beq norowchange
                jsr changeydir
norowchange     lda TILEADDR
                cmp OLDTILEADDR
                beq nocolchange
                jsr changexdir
nocolchange
/*
                txa                         ; This is still the X from calctilepos containing the number of horizontal blocks
                ldy #2                      ; Temporarily use Y as counter: Three or two bytes to delete?
                and #1
                beq is_even2
                dey                         ; Odd = two bytes only
is_even2        lsr                         ; A is either 1 or 0, so a Logical Shift Right of the accumulator will result in zero, saving one byte compared to LDA #0
*/
                lda MULTITABLE+1,X
                clc                         ; We actively set the carry here because we want the result of the substraction to be reduced by one more
                sbc MULTITABLE,x
                tay
                lda #0
@               sta (TILEADDR),y
                dey
                bpl @-

;                iny                         ; Y is $ff from before, so increasing it will set it to 0.
                ldy #3
                sec
checkhigh       lda textline2+15,y
                sbc textline2+6,y
                dey
                bpl checkhigh
                bcs sound
sethigh         ldy #3
@               lda textline2+6,Y
                sta textline2+15,Y
                dey
                bpl @-

sound           lda PLAYSOUND
                beq checkwin
                jsr movesound

checkwin        lda #>SCREEN
                sta TEMPADDR
                sta TEMPADDR+1
checkloop       ldy #40
checkline       lda (TEMPADDR),y
                bne checkgameover
;                rts
                dey
                bpl checkline
                inc TEMPADDR+1
                lda TEMPADDR+1
                cmp #$38
                bne checkloop
                lda #$80
                sta GAMEOVER
                sta RTCLOK
;                rts

checkgameover   lda GAMEOVER
                beq jmploop
                bmi clearplayer
                lda #$8f
                sta AUDC1
                sta RTCLOK
@               sta AUDF1
                lda RTCLOK
                bmi @-
clearplayer     ldy M1POSV
;                lda #0                         ; Accumulator is loaded with bits 4+5 set to 0, that's good enough for clearing missile 1
@               sta $3d00,Y
                iny
                bne @-
jmpnewgame      jmp newgame
jmploop         jmp loop

movesound       lda #$ff
                sta PMSHAPE+1           ; PM graphics :)
                sta PMSHAPE+2
                sta AUDC1
                sta RTCLOK
soundloop       sta AUDF1
                lda PLAYSOUND
                bne @+
                lda RTCLOK
                cmp #10
                bne soundloop
@               lda #0
                sta AUDC1
                sta PLAYSOUND
                rts

/*
divide          
                lda DIV1        ; potential for saving space by having the accumulator already properly loadad
                ldx #$ff
;                sec             ; check if carry is still set when called from all other routines
@               inx
                sbc DIV2
                bcs @-
;                adc DIV2        ; We have gone below zero, so we have to add DIV2 again to get the remainder into A
;                tay             ; store remainder in Y
                rts
*/

changedirection 
                bpl changexdir
changeydir      lda DIRECTION
                eor #%01000000
                sta DIRECTION
                lda (TILEADDR),y
                and #%00000100          ; If divisible by three, then missile path exactly returns
;                cmp #3
                beq setmovesound
changexdir      lda DIRECTION
                eor #%10000000
                sta DIRECTION
setmovesound    inc PLAYSOUND
                rts

VBI             lda M1POSH
                sta HPOSM1
                sta PCOLR1
                lda P0POSH
                sta HPOSP0

                lda GAMEOVER
                bne checkstick

;                ldx #0                  ; use X register as flag for change of direction
                tax
                ldy M1POSH
                cpy #49
                bcs checkright
                inx
                inx                     ; We have to increase X twice in case we hit the exact corner and the horizontal increase will be cancelled out by the vertical decrease 
;                iny
checkright      cpy #208
                bcc checktop
                inx
                inx
;                dey
checktop        sty M1POSH
                ldy M1POSV
                cpy #15
                bcs checkbottom
                dex
                iny
checkbottom     cpy #107
                bcc movemissile
                dex
                dey
                lda M1PL
                bne movemissile
                inc GAMEOVER
;                dec LIVES

movemissile     txa
                beq nowall
                jsr changedirection
nowall          sty M1POSV                
                sty HITCLR
                ldx #6
@               lda PMSHAPE-1,X
                sta $3c00+$180,y
                iny
                dex
                bpl @-

                bit DIRECTION               ; Yay, the first time I used the 6502's BIT command, and it really does make sense :)...
                bmi missileleft             ; DIRECTION's bit 7 is the left (1) / right (0) marker. BIT puts that bit into the 6502's Negative flag.
                inc M1POSH
                bne checkydir
missileleft     dec M1POSH
checkydir       lda STRIG0                  ; AND all triggers together - if one of them is pressed, the AND'ed value will be zero, so we know that boost is active. Zero flag remains set until we check a few lines below...
                and STRIG1
                and PTRIG2
                and PTRIG3
                bvs missiledown             ; DIRECTION's bit 6 is the up (1) / down (0) marker. BIT puts that bit into the 6502's oVerflow flag.
                bne noboost1                ; Here we compare against the zero flag from the AND'ed trigger registers...
                dec M1POSV
noboost1        dec M1POSV
                bne checkstick
missiledown     bne noboost2                ; ...and here again, in case the missile is on the way down...
                inc M1POSV
noboost2        inc M1POSV

checkstick      lda PADDL2
                cmp #228                    ; assume no paddle if paddle value is 228
                beq nopaddle
                eor #255                    ; invert the value (227 is maximum right, 1 is maximum left)
                lsr                         ; playfield is roughly 130 player positions, so we halve the paddle value
                sta TEMPVBI                 ; store temporarily
                lsr                         ; and continue dividing it by 2
                lsr                         ; and again, so we divide in total by 8 because we need to move one more pixel for every 16 paddle positions
                adc TEMPVBI                 ; then we add the two results
                adc #24                     ; and add 24
                sta P0POSH                  ; so we get a relatively well fitting player position.
;                bne exitvbi
nopaddle        lda STICK0
                cmp #11
                bne rightstick
                lda P0POSH
                cmp #48
                beq rightstick
                dec P0POSH
                dec P0POSH
rightstick      cmp #7
                bne exitvbi
                lda P0POSH
                cmp #178
                beq exitvbi
                inc P0POSH
                inc P0POSH

exitvbi         jmp $e462


DLI             pha
                tya
                pha
                lda VCOUNT
                cmp #$70            ; change here if the DL is changed above this line
                bpl textline
                cmp #12
                bmi textline

                lda #192
                sta PRIOR
                ldy #ENDDLITABLE-DLITABLE-1
dliloop1        lda DLITABLE,y
                bit GAMEOVER
                bpl @+
;                clc
                adc RTCLOK
                and #%00001111
@               sta WSYNC
                sta COLBK
                sta COLPM0
                dey
                bpl dliloop1
                bmi dliexit

textline        ldy #8
;                stx COLBK
                sty PRIOR
dliloop2        lda DLITABLE,y
                sta WSYNC
                sta COLBK
                eor #$0f
                clc
                adc #$10
                sta COLPF0
                adc #$10
                sta COLPF2
                dey
                bne dliloop2
;                stx WSYNC
                sty COLBK
dliexit         pla
                tay
                pla
                rti
;                jmp $e462                   ; didn't know that the exit for VBI and DLI are the same if you push first X and then Y on the stack :)

DLIST           .byte 112,112+128
                .byte 6+64
                .word textline1
                .byte 128
DLISTGEN
                org DLISTGEN+ROWSIZE*ROWS
                .byte 112,112,112,112+128,112,112+128,6+64
                .word textline2
                .byte 65
                .word DLIST

TILE
                .byte $0f,$ff,$f0
                .byte $ee,$ee
                .byte $0d,$dd,$d0
                .byte $cc,$cc
                .byte $0b,$bb,$b0
                .byte $aa,$aa
                .byte $09,$99,$90
                .byte $88,$88
                .byte $08,$88,$80
                .byte $77,$77
                .byte $06,$66,$60
                .byte $55,$55
                .byte $04,$44,$40
                .byte $33,$33
                .byte $02,$22,$20
                .byte $11,$11
ENDTILE
;ENDTILE = TILE+(ENDPALETTELIST-PALETTELIST)*BYTESPERBLOCK
/*
                org $3c00+$200+$6c
PMSHAPE         .byte %00000000
                .byte %11111111
                .byte %11111111
                .byte %00000000
PMSHAPE2
*/