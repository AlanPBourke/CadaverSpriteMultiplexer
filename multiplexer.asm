// Cadaver's Sprite Multiplexer, Kick Assembler conversion.
// https://cadaver.github.io/rants/sprite.html
// This is the 'improved general multiplexer, (adapted from Hessian: doublebuffering, "SWIV" sorting, precalculation of interrupts)'

#define SHOW_RASTERTIME 

// Constant definitions
.const NUMSPR          = 24             // Number of sprites used by the main program
.const MAXSPR          = 24             // Maximum number of supported sprites
.const MINSPRY         = 30             // Minimum visible sprite Y-coordinate
.const MAXSPRY         = 250            // Maximum visible sprite Y-coordinate + 1
.const IRQ1LINE        = $10            // IRQ at the top of screen


// Zeropage variable & memory definitions
.label temp1           = $02
.label temp2           = $03
.label temp3           = $04
.label sprupdateflag   = $05            // Update flag for IRQ
.label sortsprstart    = $06            // First used sorted table index (doublebuffered)
.label sortsprend      = $07            // Last used sorted table index + 1

.label sprorder        = $40            // Order table need to be on zeropage due to addressing modes
                                        // & sorting speed. It also need to contain 25 elements to contain
                                        // an endmark
.label screen1         = $0400

* = $1000 "Start"                       // sys4096
BasicUpstart2(main) 

* = $0fc0 "sprites"
.fill 64, 255

* = $1010 "main"
main:           jsr initsprites         // Init the multiplexing-system
                jsr initraster          // Init raster interrupts

                ldx #NUMSPR - 1
initloop:       lda $e000, x            // Init sprites with some random
                sta sprxl, x            // ;values from the KERNAL
                lda $e018, x
                and #$01
                sta sprxh, x
                lda $e030, x
                sta spry, x
                lda #$3f
                sta sprf, x
                txa
                and #$0f
                cmp #$06                // Avoid blue as sprite color
                bne colorok             // (would look invisible)
                lda #$05
colorok:        sta sprc,x
                dex
                bpl initloop

mainloop:       ldx #NUMSPR - 1
moveloop:       lda $e048, x            // Move the sprites with some
                and #$03                // random speeds
                sec
                adc sprxl, x
                sta sprxl, x
                lda sprxh ,x            // Update X coordinate high byte, wrap when coordinate
                adc #$00                // larger than $180 (arbitrary)
                and #$01
                sta sprxh, x
                beq moveloop_xnotover
                lda sprxl, x
                bpl moveloop_xnotover
                sec
                sbc #$80
                sta sprxl, x
                dec sprxh, x
moveloop_xnotover:
                lda $e060, x
                and #$01
                sec
                adc spry, x
                sta spry, x
                dex
                bpl moveloop
                jsr sortsprites         // Sort sprites, build sprite IRQ lists and set the update flag
                jmp mainloop            //Back to loop


// Initialise sprites.
initsprites:    lda #$00                // Reset update flag & doublebuffer side
                sta sprupdateflag
                sta sortsprstart
                ldx #MAXSPR             // Init the order table with a 0,1,2,3,4,5.. order. 
is_orderlist:   txa                     // Init all Y-coordinates with $ff (unused)
                sta sprorder, x
                lda #$ff
                sta spry, x
                dex
                bpl is_orderlist
                rts

initraster:     sei
                lda #<irq1
                sta $0314
                lda #>irq1
                sta $0315
                lda #$7f                // CIA interrupt off
                sta $dc0d
                lda #$01                // Raster interrupt on
                sta $d01a
                lda #27                 // High bit of interrupt position = 0
                sta $d011
                lda #IRQ1LINE           // Line where next IRQ happens
                sta $d012
                lda $dc0d               //Acknowledge IRQ (to be sure)
                cli
                rts

// Routine to sort the sprites, copy them to the sorted table, and
// arrange the sprite IRQ's beforehand

sortsprites:    lda sprupdateflag       // Wait until IRQ is done with current sprite update
                bne sortsprites
                #if SHOW_RASTERTIME
                inc $d020
                #endif
                lda sortsprstart        // Switch sprite doublebuffer side
                eor #MAXSPR
                sta sortsprstart
                ldx #$00
                stx temp3               // D010 bits for first irq
                txa
sspr_loop1:     ldy sprorder, x         // Check for Y-coordinates being in order
                cmp spry, y
                beq sspr_noswap2
                bcc sspr_noswap1
                stx temp1               // If not in order, begin insertion loop
                sty temp2
                lda spry, y
                ldy sprorder - 1, x
                sty sprorder, x
                dex
                beq sspr_swapdone1
sspr_swap1:     ldy sprorder - 1, x
                sty sprorder, x
                cmp spry, y
                bcs sspr_swapdone1
                dex
                bne sspr_swap1
sspr_swapdone1: ldy temp2
                sty sprorder, x
                ldx temp1
                ldy sprorder, x
sspr_noswap1:   lda spry, y
sspr_noswap2:   inx
                cpx #MAXSPR
                bne sspr_loop1
                ldx #$00
sspr_findfirst: ldy sprorder, x         // Find upmost visible sprite
                lda spry, y
                cmp #MINSPRY
                bcs sspr_firstfound
                inx
                bne sspr_findfirst
sspr_firstfound:txa
                adc #<sprorder          // Add one more, C=1 becomes 0
                sbc sortsprstart        // subtract one more to cancel out
                sta sspr_copyloop1+1
                ldy sortsprstart
                tya
                adc #8-1                // C=1
                sta sspr_copyloop1end+1     // Set endpoint for first copyloop
                bpl sspr_copyloop1

sspr_copyloop1skip:                     // Copyloop for the first 8 sprites
                inc sspr_copyloop1+1
sspr_copyloop1: ldx sprorder, y
                lda spry, x              // If reach the maximum Y-coord, all done
                cmp #MAXSPRY
                bcs sspr_copyloop1done
                sta sortspry, y
                lda sprc, x             // Copy sprite's properties to sorted table
                sta sortsprc, y
                lda sprf, x
                sta sortsprf, y
                lda sprxl, x
                sta sortsprx,y
                lda sprxh, x            // Handle sprite X coordinate MSB
                beq sspr_copyloop1msblow
                lda temp3
                ora sprortbl, y
                sta temp3
sspr_copyloop1msblow:
                iny
sspr_copyloop1end:
                cpy #$00
                bcc sspr_copyloop1
                lda temp3
                sta sortsprd010-1, y
                lda sortsprc-1, y       // Make first irq endmark
                ora #$80
                sta sortsprc-1, y
                lda sspr_copyloop1+1    // Copy sortindex from first copyloop
                sta sspr_copyloop2+1    // To second
                bcs sspr_copyloop2

sspr_copyloop1done:
                lda temp3
                sta sortsprd010-1, y
                sty temp1               // Store sorted sprite end index
                cpy sortsprstart        // Any sprites at all?
                beq sspr_nosprites
                lda sortsprc-1, y       // Make first (and final) IRQ endmark
                ora #$80                // (stored in the color table)
                sta sortsprc-1, y
                jmp sspr_finalendmark
sspr_nosprites: jmp sspr_alldone

sspr_copyloop2skip:                     // Copyloop for subsequent sprites,
                inc sspr_copyloop2+1    // with "9th sprite" (physical overlap) prevention
sspr_copyloop2: ldx sprorder, y
                lda spry, x
                cmp #MAXSPRY
                bcs sspr_copyloop2done
                sta sortspry, y
                sbc #21-1
                cmp sortspry-8, y       // Check for physical sprite overlap
                bcc sspr_copyloop2skip
                lda sprc, x
                sta sortsprc, y
                lda sprf, x
                sta sortsprf, y
                lda sprxl, x
                sta sortsprx, y
                lda sprxh, x
                beq sspr_copyloop2msblow
                lda sortsprd010-1, y
                ora sprortbl, y
                bne sspr_copyloop2msbdone
sspr_copyloop2msblow:
                lda sortsprd010-1, y
                and sprandtbl, y
sspr_copyloop2msbdone:
                sta sortsprd010, y
                iny
                bne sspr_copyloop2

sspr_copyloop2done:
                sty temp1               // Store sorted sprite end index
                ldy sspr_copyloop1end+1 // Go back to the second IRQ start
                cpy temp1
                beq sspr_finalendmark
sspr_irqloop:   sty temp2               // Store IRQ startindex
                lda sortspry, y         // C=0 here
                sbc #21+12-1            // First sprite of IRQ: store the y-coord
                sta sspr_irqycmp1+1     // compare values
                adc #21+12+6-1
                sta sspr_irqycmp2+1
sspr_irqsprloop:iny
                cpy temp1
                bcs sspr_irqdone
                lda sortspry-8, y       // Add next sprite to this IRQ?
sspr_irqycmp1:  cmp #$00                // (try to add as many as possible while
                bcc sspr_irqsprloop     // avoiding glitches)
                lda sortspry, y
sspr_irqycmp2:  cmp #$00
                bcc sspr_irqsprloop
sspr_irqdone:   tya
                sbc temp2
                tax
                lda sprirqadvancetbl-1, x
                ldx temp2
                adc sortspry,x
                sta sprirqline-1,x      // Store IRQ start line (with advance)
                lda sortsprc-1, y       // Make endmark
                ora #$80
                sta sortsprc-1, y
                cpy temp1               // Sprites left?
                bcc sspr_irqloop
sspr_finalendmark:
                lda #$00                // Make final endmark
                sta sprirqline-1, y
sspr_alldone:   sty sortsprend          // Index of last sorted sprite + 1
                inc sprupdateflag       // Increment the update flag which will be read by IRQ's
                #if SHOW_RASTERTIME
                dec $d020
                #endif
                rts

// IRQ 1 code
// IRQ at the top of the screen. Take sprite update from the main program and
// start showing the sprites
        
irq1:           lda sprupdateflag       // New sprites?
                beq irq1_nonewsprites
                lda #$00
                sta sprupdateflag
                lda sortsprstart
                sta irq1_sortsprstart+1     // Copy sorted sprite start index for IRQ
                lda sortsprend
                sec
                sbc sortsprstart        // Find out number of sprites
                cmp #$09                // More than 8?
                bcc irq1_notover8
                lda #$08
irq1_notover8:  tax
                lda d015tbl,x           // Take the bit combination for $d015
                sta irq1_d015value+1
irq1_nonewsprites:
irq1_d015value: lda #$00                // Any sprites?
                sta $d015
                bne irq1_hassprites
                inc $d019
                jmp $ea81               // If no sprites, can exit here
irq1_hassprites:
                lda #<irq2
                sta $0314
                lda #>irq2
                sta $0315
irq1_sortsprstart:
                ldx #$00                // Go through the first sprite IRQ immediately
                #if SHOW_RASTERTIME
                inc $d020
                #endif

// IRQ for sprite displaying (repeated until done)

irq2_spr0:      lda sortspry,x
                sta $d001
                lda sortsprx,x
                ldy sortsprd010,x
                sta $d000
                sty $d010
                lda sortsprf,x
                sta screen1+$03f8
                lda sortsprc,x
                sta $d027
                bmi irq2_sprirqdone2    // Color high bit functions as IRQ endmark
                inx

irq2_spr1:      lda sortspry,x
                sta $d003
                lda sortsprx,x
                ldy sortsprd010,x
                sta $d002
                sty $d010
                lda sortsprf,x
                sta screen1+$03f9
                lda sortsprc,x
                sta $d028
                bmi irq2_sprirqdone2
                inx

irq2_spr2:      lda sortspry,x
                sta $d005
                lda sortsprx,x
                ldy sortsprd010,x
                sta $d004
                sty $d010
                lda sortsprf,x
                sta screen1+$03fa
                lda sortsprc,x
                sta $d029
                bmi irq2_sprirqdone2
                inx

irq2_spr3:      lda sortspry,x
                sta $d007
                lda sortsprx,x
                ldy sortsprd010,x
                sta $d006
                sty $d010
                lda sortsprf,x
                sta screen1+$03fb
                lda sortsprc,x
                sta $d02a
                bpl irq2_tospr4
irq2_sprirqdone2:
                jmp irq2_sprirqdone
irq2_tospr4:    inx

irq2_spr4:      lda sortspry,x
                sta $d009
                lda sortsprx,x
                ldy sortsprd010,x
                sta $d008
                sty $d010
                lda sortsprf,x
irq2_spr4frame: sta screen1+$03fc
                lda sortsprc,x
                sta $d02b
                bmi irq2_sprirqdone
                inx

irq2_spr5:      lda sortspry,x
                sta $d00b
                lda sortsprx,x
                ldy sortsprd010,x
                sta $d00a
                sty $d010
                lda sortsprf,x
irq2_spr5frame: sta screen1+$03fd
                lda sortsprc,x
                sta $d02c
                bmi irq2_sprirqdone
                inx

irq2_spr6:      lda sortspry,x
                sta $d00d
                lda sortsprx,x
                ldy sortsprd010,x
                sta $d00c
                sty $d010
                lda sortsprf,x
irq2_spr6frame: sta screen1+$03fe
                lda sortsprc,x
                sta $d02d
                bmi irq2_sprirqdone
                inx

irq2_spr7:      lda sortspry,x
                sta $d00f
                lda sortsprx,x
                ldy sortsprd010,x
                sta $d00e
                sty $d010
                lda sortsprf,x
irq2_spr7frame: sta screen1+$03ff
                lda sortsprc,x
                sta $d02e
                bmi irq2_sprirqdone
                inx
irq2_tospr0:    jmp irq2_spr0

irq2_sprirqdone:
                #if SHOW_RASTERTIME
                dec $d020
                #endif
                ldy sprirqline, x       // Get startline of next IRQ
                beq irq2_alldone        // (0 if was last)
                inx
                stx irq2_sprindex+1     // Store next irq sprite start-index
                txa
                and #$07
                tax
                lda sprirqjumptbllo, x  // Get the correct jump address for next sprite IRQ
                sta irq2_sprjump+1
                lda sprirqjumptblhi, x
                sta irq2_sprjump+2
                tya
                sta $d012
                sec
                sbc #$03                // Already late from the next IRQ?
                cmp $d012
                bcc irq2_direct         // If yes, execute directly
                inc $d019               // Acknowledge IRQ
                jmp $ea81               // Otherwise end IRQ

irq2:
irq2_direct:    
                #if SHOW_RASTERTIME
                inc $d020
                #endif
irq2_sprindex:  ldx #$00
irq2_sprjump:   jmp irq2_spr0

irq2_alldone:   lda #<irq1
                sta $0314
                lda #>irq1
                sta $0315
                lda #IRQ1LINE
                sta $d012
                inc $d019
                jmp $ea81               // All spriteIRQ's done, return to the top of screen IRQ

* = $4000 "variables"
sprxl:          .fill MAXSPR, 0         // Unsorted sprite tables to be manipulated by the main program.
sprxh:          .fill MAXSPR, 0
spry:           .fill MAXSPR + 1, 0     // Y table needs an extra element due to endmark (maximum Y coord $ff)
sprc:           .fill MAXSPR, 0
sprf:           .fill MAXSPR, 0

sortsprx:       .fill MAXSPR * 2, 0     // Sorted sprites are doublebuffered
sortsprd010:    .fill MAXSPR * 2, 0     
sortspry:       .fill MAXSPR * 2, 0
sortsprf:       .fill MAXSPR * 2, 0
sortsprc:       .fill MAXSPR * 2, 0
sprirqline:     .fill MAXSPR * 2, 0     // Table used to control sprite IRQs

sprirqadvancetbl:
                .byte -4,-5,-6,-7       // $d012 advance for raster IRQs based on number of sprites in the same IRQ
                .byte -7,-8,-9,-10   

                // OR table for $d010 manipulation, repeated for 2x max sprites (doublebuffer)
sprortbl:       .byte $01, $02, $04, $08, $10, $20, $40, $80 
                .byte $01, $02, $04, $08, $10, $20, $40, $80
                .byte $01, $02, $04, $08, $10, $20, $40, $80
                .byte $01, $02, $04, $08, $10, $20, $40, $80
                .byte $01, $02, $04, $08, $10, $20, $40, $80
                .byte $01, $02, $04, $08, $10, $20, $40, $80
                
                // AND table likewise repeated for 2x max sprites
sprandtbl:      .byte $fe, $fd, $fb, $f7, $ef, $df, $bf, $7f  
                .byte $fe, $fd, $fb, $f7, $ef, $df, $bf, $7f
                .byte $fe, $fd, $fb, $f7, $ef, $df, $bf, $7f
                .byte $fe, $fd, $fb, $f7, $ef, $df, $bf, $7f
                .byte $fe, $fd, $fb, $f7, $ef, $df, $bf, $7f
                .byte $fe, $fd, $fb, $f7, $ef, $df, $bf, $7f

d015tbl:        .byte %00000000         // Table of sprites that are "on"
                .byte %00000001         // for $d015
                .byte %00000011
                .byte %00000111
                .byte %00001111
                .byte %00011111
                .byte %00111111
                .byte %01111111
                .byte %11111111

                    // Jump table for starting the spriteIRQ at correct sprite
sprirqjumptbllo:    .byte <irq2_spr0                 
                    .byte <irq2_spr1
                    .byte <irq2_spr2
                    .byte <irq2_spr3
                    .byte <irq2_spr4
                    .byte <irq2_spr5
                    .byte <irq2_spr6
                    .byte <irq2_spr7

sprirqjumptblhi:    .byte >irq2_spr0
                    .byte>irq2_spr1
                    .byte >irq2_spr2
                    .byte >irq2_spr3
                    .byte >irq2_spr4
                    .byte >irq2_spr5
                    .byte >irq2_spr6
                    .byte >irq2_spr7