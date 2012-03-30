''*********************************************
''*  PropTerm VGA High-Color Text Driver v1.0 *
''*********************************************
''
''This object is a modified version of the "VGA_HiRes_Text" object
''by Chip Gracey of Parallax, Inc. As such terms of use remain the
''same as the original object code terms of use.
''
''This object generates a 800x600 VGA signal which contains 100 columns x 50
''rows or,a 640x480 VGA signal which contains 80 columns x 40 rows
''of 8x12 characters. Each character can have a unique forground/background
''color combination. The text characters resides at the ScreentPtr address,
''one byte per character. The character memory requires rows*cols bytes.
''The color information resides at the ColorPtr address one word per character.
''The low byte contains the forground color in the high 6 bits (RRGGBBxx).
''The high byte of the color word contains the background color (high 6 bits).
''When a color is written to the color table the low bits(0..1) must be 0.
''The color table requires rows*cols words. There is also a text cursor
''(slow blink underscore). The four rows of the cursor pattern are stored in a long
''at the address in PAR register. This is read during vertical blanking.
''
''You must provide buffers for the screen, colors, cursors, and cursor pattern. Once
''started, all interfacing is done via memory. To this object, all buffers are
''read-only.
''

CON

'{
' 800 x 600 @ 72Hz settings: 100 x 50 characters

  hp = 800      'horizontal pixels
  vp = 600      'vertical pixels
  hf = 56       'horizontal front porch pixels
  hs = 120      'horizontal sync pixels
  hb = 64       'horizontal back porch pixels
  vf = 37       'vertical front porch lines
  vs = 6        'vertical sync lines
  vb = 23       'vertical back porch lines
  hn = 1        'horizontal normal sync state (0|1)
  vn = 1        'vertical normal sync state (0|1)
  pr = 50       'pixel rate in MHz at 80MHz system clock (5MHz granularity)
'}
{
' 800 x 600 @ 60Hz settings: 100 x 50 characters

  hp = 800      'horizontal pixels
  vp = 600      'vertical pixels
  hf = 40       'horizontal front porch pixels
  hs = 128      'horizontal sync pixels
  hb = 88       'horizontal back porch pixels
  vf = 1        'vertical front porch lines
  vs = 4        'vertical sync lines
  vb = 23       'vertical back porch lines
  hn = 1        'horizontal normal sync state (0|1)
  vn = 1        'vertical normal sync state (0|1)
  pr = 40       'pixel rate in MHz at 80MHz system clock (5MHz granularity)
}
{
' 640 x 480 @ 69Hz settings: 80 x 40 characters

  hp = 640      'horizontal pixels
  vp = 480      'vertical pixels
  hf = 24       'horizontal front porch pixels
  hs = 40       'horizontal sync pixels
  hb = 128      'horizontal back porch pixels
  vf = 9        'vertical front porch lines
  vs = 3        'vertical sync lines
  vb = 28       'vertical back porch lines
  hn = 1        'horizontal normal sync state (0|1)
  vn = 1        'vertical normal sync state (0|1)
  pr = 30       'pixel rate in MHz at 80MHz system clock (5MHz granularity)
}

' columns and rows

  cols = hp / 8
  rows = vp / 12
  
  cursor_on_pattern  = $FFFF0000
  cursor_off_pattern = $00000000
  
VAR

  long cog[2]
  long underscore                'underscore cursor pattern

PUB start(BasePin, ScreenPtr, ColorPtr, CursorPtr) : okay | i, j

'' Start VGA driver - starts two COGs
'' returns false if two COGs not available
''
''     BasePin = VGA starting pin (0, 8, 16, 24, etc.)
''
''   ScreenPtr = Pointer to rows * cols bytes containing ASCII codes for each of the
''               screen characters. Each byte's lower seven bits provide the ASCII code.
''               Screen memory is arranged left-to-right, top-to-bottom.
''
''               screen byte example: %0_1000001 = "A"
''
''    ColorPtr = Pointer to rows * cols words which define the foreground and background
''               colors for each character. The lower byte of each word contains the
''               foreground RGB data for that row, while the upper byte
''               contains the background RGB data. The RGB data in each byte is
''               arranged as %RRGGBB00 (4 levels each). The low bits (0..1) must be 0.
''
''               color word example: %%0020_3300 = gold on blue
''
''   CursorPtr = Pointer to 2 bytes which control the cursors:
''
''               bytes 0,1: X, Y, of text cursor
''               (set X or Y out of range to disable cursor)
''
''               X and Y are in terms of screen characters
''               (left-to-right, top-to-bottom)
''

  'if driver is already running, stop it
  stop

  'implant pin settings
  reg_vcfg := $200000FF + (BasePin & %111000) << 6
  i := $FF << (BasePin & %011000)
  j := BasePin & %100000 == 0
  reg_dira := i & j
  reg_dirb := i & !j
  'Setup initial cursor pattern
  underscore := cursor_on_pattern
  'implant CNT value to sync COGs to
  sync_cnt := cnt + $10000

  'implant pointers
'  longmove(@screen_base, @ScreenPtr, 3)
  screen_base := ScreenPtr
  color_base := ColorPtr
  cursorX_base := CursorPtr
  cursorY_base := cursorX_base + 1
  font_base := @font

  'implant unique settings and launch first COG
  vf_lines.byte := vf
  vb_lines.byte := vb
  font_third := 1
  cog[1] := cognew(@d0, @underscore) + 1

  'allow time for first COG to launch
  waitcnt($2000 + cnt)

  'differentiate settings and launch second COG
  vf_lines.byte := vf+4
  vb_lines.byte := vb-4
  font_third := 0
  cog[0] := cognew(@d0, @underscore) + 1

  'if both COGs launched, return true
  if cog[0] and cog[1]
    return true
    
  'else, stop any launched COG and return false
  else
    stop


PUB stop | i

'' Stop VGA driver - frees two COGs

  repeat i from 0 to 1
    if cog[i]
      cogstop(cog[i]~ - 1)


PUB GetFontAdr : Address
''Return address of font glyph table
  Address := @font

PUB CursorOn
'' Display the text cursor
  underscore := cursor_on_pattern
        
PUB CursorOff
'' Hide the text cursor
  underscore := cursor_off_pattern

PUB GetCursorPatternAdr : Address
'' Get the memory address of the cursor pattern buffer
  Address := @underscore
   
CON

  #1, scanbuff[cols], coltab[cols], scancode[cols*2-1+3], maincode 'enumerate COG RAM usage

  main_size = $1F0 - maincode                           'size of main program   

  hv_inactive = (hn << 1 + vn) * $0101                  'H,V inactive states

  
DAT

'*****************************************************
'* Assembly language VGA high-resolution text driver *
'*****************************************************

' This program runs concurrently in two different COGs.
'
' Each COG's program has different values implanted for front-porch lines and
' back-porch lines which surround the vertical sync pulse lines. This allows
' timed interleaving of their active display signals during the visible portion
' of the field scan. Also, they are differentiated so that one COG displays
' even four-line groups while the other COG displays odd four-line groups.
'
' These COGs are launched in the PUB 'start' and are programmed to synchronize
' their PLL-driven video circuits so that they can alternately prepare sets of
' four scan lines and then display them. The COG-to-COG switchover is seemless
' due to two things: exact synchronization of the two video circuits and the
' fact that all COGs' driven output states get OR'd together, allowing one COG
' to output lows during its preparatory state while the other COG effectively
' drives the pins to create the visible and sync portions of its scan lines.
' During non-visible scan lines, both COGs output together in unison.
'
' COG RAM usage:  $000            = d0 - used to inc destination fields for indirection
'                 $001-COLS       = scanbuff - longs which hold 4 scan lines
'                 COLS+1-2*COLS+1 = scancode - stacked WAITVID/SHR for fast display
'                 $183-$1EF       = maincode - main program loop which drives display

                        org                             'set origin to $000 for start of program

d0                      long    1 << 9                  'd0 always resides here at $000, executes as NOP

' Initialization code and data - after execution, space gets reused as scanbuff

                        'Move main program into maincode area

:move                   mov     $1EF,main_begin+main_size-1                 
                        sub     :move,d0s0              '(do reverse move to avoid overwrite)
                        djnz    main_ctr,#:move                                     
                        'Clear color table
:clrcol                 mov     coltab, init_col                   
:nosync                 or      coltab,hv                    'insert inactive hsync and vsync states
                        add     :clrcol,d0
                        add     :nosync,d0
                        djnz    col_ctr,#:clrcol
                                     
                        'Build scanbuff display routine into scancode                      
                                                                                        
:waitvid                mov     scancode+0,i0           'org     scancode                                              
:shr                    mov     scancode+1,i1           'waitvid color+0,scanbuff+0                    
                        add     :waitvid,d1             'shr     scanbuff+0,#8                       
                        add     :shr,d1                 'waitvid color+1,scanbuff+1                    
                        add     i0,d0s0                 'shr     scanbuff+1,#8                       
                        add     i1,d0                   '...                                         
                        djnz    scan_ctr,#:waitvid      'waitvid color,scanbuff+cols-1
                            
                        mov     scancode+cols*2-1,i2    'mov     vscl,#hf                            
                        mov     scancode+cols*2+0,i3    'waitvid hvsync,#0                           
                        mov     scancode+cols*2+1,i4    'jmp     #scanret                            
                                                                                 
                        'Init I/O registers and sync COGs' video circuits
                        ' We use counter B register to hold two constants
                        mov     ctrb, #0                'make sure counter B stopped
                        mov     frqb, vscl_line2x       'save line pixel count
                        rdlong  phsb,par                'read and save cursor pattern
                        ' we use the port b out latch for slowbit
                        mov     dirb, #0                ' Set port B as all inputs
                        mov     outb, blinkbit           ' outb holds blinkbit constant                             
                        mov     dira,reg_dira           'set pin directions                   
                        mov     dirb,reg_dirb
                        'Setup master pixel clock, use PLL A                                                 
                        movi    frqa,#(pr / 5) << 2     'set pixel rate                                      
                        mov     vcfg,reg_vcfg           'set video configuration
                        mov     vscl,#1                 'set video to reload on every pixel
                        waitcnt sync_cnt,colormask      'wait for start value in cnt, add ~1ms
                        movi    ctra,#%00001_110        'COGs in sync! enable PLLs now - NCOs locked!
                        waitcnt sync_cnt,#0             'wait ~1ms for PLLs to stabilize - PLLs locked!
                        mov     vscl,#100               'insure initial WAITVIDs lock cleanly

                        'Jump to main loop
                        
                        jmp     #vsync                  'jump to vsync - WAITVIDs will now be locked!

                        'Data

d1                      long    1 << 10
d0s0                    long    1 << 9 + 1         
main_ctr                long    main_size
scan_ctr                long    cols
col_ctr                 long    cols
init_col                long    %%0000_3330
colormask               long    $FCFC                   'mask to isolate R,G,B bits from H,V
vscl_line2x             long    (hp + hf + hs + hb) * 2 'total number of pixels per 2 scan lines
blinkbit                long    1 << 25                 'cnt mask for slow cursor blink

i0                      waitvid coltab+0,scanbuff+0
i1                      shr     scanbuff+0,#8
i2                      mov     vscl,#hf
i3                      waitvid hvsync,#0
i4                      jmp     #scanret

reg_dira                long    0                       'set at runtime
reg_dirb                long    0                       'set at runtime
reg_vcfg                long    0                       'set at runtime                          
sync_cnt                long    0                       'set at runtime

                        'Directives

                        fit     scancode                'make sure initialization code and data fit
main_begin              org     maincode                'main code follows (gets moved into maincode)


' Main loop, display field - each COG alternately builds and displays four scan lines
                          
vsync                   mov     x,#vs                   'do vertical sync lines
                        call    #blank_vsync

vb_lines                mov     x,#vb                   'do vertical back porch lines (# set at runtime)
                        call    #blank_vsync

                        mov     screen_ptr,screen_base  'reset screen pointer to upper-left character
                        mov     color_ptr,color_base    'reset color pointer to first row
                        mov     row,#0                  'reset row counter for cursor insertion
                        mov     fours,#rows * 3 / 2     'set number of 4-line builds for whole screen
                        
                        'Build four scan lines into scanbuff and coltab

fourline                mov     font_ptr,font_third     'get address of appropriate font section
                        shl     font_ptr,#8+2           '
                        add     font_ptr,font_base
                        
                        movd    :pixa,#scanbuff-1       'reset scanbuff address (pre-decremented)

                        movd    :getcolfx,#coltab-1     'reset coltab address (pre-decremented)
                        
                        mov     y,#2                    'must build scanbuff in two sections because
                        mov     vscl,frqb               'FRQB = vscl_line2x
                                                        '..pixel counter is limited to twelve bits

:halfrow                waitvid phsb, #0                'phsb has underscore pattern output lows to let other COG drive VGA pins
                        mov     x, #cols/2              '..for 2 scan lines, ready for half a row
                        
:column                 add     :pixa, d0               'increment scanbuff destination addresses
                        rdbyte  z,screen_ptr            'get character from screen memory
                        rol     z,#2                    'chr into bits 8..2 (CharIdx * 4)
                        add     z,font_ptr              'add font section address to point to 8*4 pixels
:pixa                   rdlong  scanbuff,z              'read pixel long (8*4) into scanbuff
                        add     screen_ptr,#1           'increment screen memory address
                        'Now for the color info
                        add     :getcolfx, d0           'point to next color to read
                        rdword  z, color_ptr            'get chars colors.
'                       and     z, colormask            'mask away hsync and vsync signal states
                        or      z, hv                   'set no sync value
:getcolfx               mov     coltab, z               '
                        add     color_ptr, #2           'point to next color to write
                        djnz    x, #:column             'another character in this half-row?

                        djnz    y, #:halfrow             'loop to do 2nd half-row, time for 2nd WAITVID

                        sub     screen_ptr, #cols        'back up to start of same row in screen memory
                        sub     color_ptr, #cols*2       'back up to start of same row in color memory
                        'Insert text cursor into scanbuff
                        rdbyte  x, cursorX_base          ' get text cursor x value
                        cmp     x, #cols         wc      ' see if x is in range (0..cols-1)
                        add     x, #scanbuff             ' add column to scanbuf start address
                        rdbyte  y, cursorY_base          ' get text cursor y value
                        cmp     y, row           wz      ' see if we are on the same row as scanbuffer
                        ' setup xor to update correct scanbuf address
                        movd    :xormsk,x               ' set xor destination address to correct character
                        ' display the cursor if required
        if_nc_or_nz     jmp     #:nocursor              'if cursor not in scanbuff, no cursor
                        test    outb, cnt        wc      'get blink state
                        cmp     font_third, #2   wz      'must be last font section
                        mov     z, #0
:xormsk if_c_and_z      xor     scanbuff, phsb             'xor cursor into scanbuff, phsp = underscore pattern

                        'Display four scan lines from scanbuff
:nocursor               mov     y,#4                    'ready for four scan lines

scanline                mov     vscl,vscl_chr           'set pixel rate for characters
                        jmp     #scancode               'jump to scanbuff display routine in scancode
                        'Return from scan code
scanret                 mov     vscl,#hs                'do horizontal sync pixels
                        waitvid hvsync,#1               '#1 makes hsync active
                        mov     vscl,#hb                'do horizontal back porch pixels
                        waitvid hvsync,#0               '#0 makes hsync inactive
                        shr     scanbuff+cols-1,#8      'shift last column's pixels right by 8
                        djnz    y,#scanline             'another scan line?

                        'Next group of four scan lines
                        
                        add     font_third,#2           'if font_third + 2 => 3, subtract 3 (new row)
                        cmpsub  font_third,#3   wc      'c=0 for same row, c=1 for new row
        if_c            add     screen_ptr,#cols        'if new row, advance screen pointer
        if_c            add     color_ptr,#cols*2       'if new row, advance color pointer
        if_c            add     row,#1                  'if new row, increment row counter
                        djnz    fours,#fourline         'another 4-line build/display?

                        'Visible section done, do vertical sync front porch lines

                        rdlong  phsb,par                'read cursor pattern
                        
vf_lines                mov     x,#vf                   'do vertical front porch lines (# set at runtime)
                        call    #blank

                        jmp     #vsync                  'new field, loop to vsync

                        'Subroutine - do blank lines

blank_vsync             xor     hvsync,#$101            'flip vertical sync bits

blank                   mov     vscl,hx                 'do blank pixels
                        waitvid hvsync,#0
                        mov     vscl,#hf                'do horizontal front porch pixels
                        waitvid hvsync,#0
                        mov     vscl,#hs                'do horizontal sync pixels
                        waitvid hvsync,#1
                        mov     vscl,#hb                'do horizontal back porch pixels
                        waitvid hvsync,#0
                        djnz    x,#blank                'another line?
blank_ret
blank_vsync_ret         ret

                        'Data

screen_base             long    0                       'set at runtime (4 contiguous longs)
color_base              long    0                       'set at runtime (memory address of color table)    
cursorX_base            long    0                       'set at runtime (points to obe byte cursor X)
cursorY_base            long    0                       'set at runtime (points to one byte cursor Y)

font_base               long    0                       'set at runtime
font_third              long    0                       'set at runtime

hx                      long    hp                      'visible pixels per scan line
vscl_chr                long    1 << 12 + 8             '1 clock per pixel and 8 pixels per set
hv                      long    hv_inactive             '-H,-V states
hvsync                  long    hv_inactive ^ $200      '+/-H,-V states
'Uninitialized data
screen_ptr              res     1
color_ptr               res     1
font_ptr                res     1

x                       res     1
y                       res     1
z                       res     1

row                     res     1
fours                   res     1


' 8 x 12 font - characters 0..255
'
' Each long holds four scan lines of a single character. The longs are arranged into
' groups of 256 which represent all characters (0..255). There are three groups which
' each contain a vertical third of all characters. They are ordered top, middle, and
' bottom.

font  long

'ANSI font with Parallax graphics
long $0DFD01FF, $0101017F, $00000000, $00000000, $1C080000, $08000000, $18100000, $06020000
long $0C0F007F, $2020203F, $01010101, $01010101, $000000FF, $000000FF, $3E3E0000, $00000000
long $00000000, $00000000, $00000000, $00000000, $00000000, $08380000, $1C0A080C, $00000000
long $08080808, $242424E4, $262424E4, $0000000F, $70901010, $72901010, $00010204, $00070305
long $00000000, $08080800, $00121212, $7E282800, $0A0A3C08, $19294600, $34243800, $00080808
long $04081830, $10080C02, $0C360800, $08000000, $00000000, $00000000, $00000000, $10202040
long $21121E00, $080E0800, $20201E00, $10101E00, $14181000, $02021E00, $02043800, $10203E00
long $22221C00, $22221C00, $18000000, $0C000000, $20000000, $00000000, $02000000, $20221E00
long $39221C00, $14140800, $22221E00, $01023C00, $21110F00, $02023E00, $02023E00, $01023C00
long $22222200, $08083E00, $10101E00, $0A122200, $02020200, $2B333300, $26262200, $21121E00
long $22221E00, $21121E00, $22221E00, $02023C00, $08087F00, $22222200, $22224100, $49414100
long $14224100, $14224100, $10203F00, $08080878, $04020201, $1010101E, $140C0808, $00000000
long $00000804, $1C000000, $1A020202, $3C000000, $3C202020, $1C000000, $3F040438, $3C000000
long $3A020202, $0E000008, $1E000010, $22020202, $0808080E, $6D000000, $3A000000, $1E000000
long $1A000000, $3C000000, $3A000000, $3C000000, $3F040400, $22000000, $41000000, $41000000
long $21000000, $21000000, $3F000000, $08080838, $08080808, $08080806, $00000000, $22223E00
long $1414E300, $00000000, $1010E000, $1010E000, $00000000, $08040300, $0000FF00, $0404FF00
long $08080700, $04040300, $1010FF00, $0000FF00, $08080700, $00000000, $0808F000, $0808F000
long $00000000, $08080808, $08080808, $08080808, $08080808, $08080808, $08080808, $00000000
long $08080808, $08080808, $08080808, $00000000, $08080808, $08080808, $00000000, $00000000
long $08492A1C, $08000000, $08080808, $08083000, $04180000, $000000FF, $0808C838, $05040404
long $15241424, $01000000, $80000000, $0A0A0A0A, $00000000, $08080808, $00000000, $01000000
long $08140800, $08080000, $18203C00, $20383C00, $00000000, $22000000, $1E201808, $FE000000
long $1F000000, $08080C00, $04000000, $08000000, $031C6018, $12120000, $04040000, $08000000
long $14140C02, $14140C08, $14141A0C, $14141C28, $14140812, $1414081C, $14187800, $01023C00
long $02023E04, $02023E10, $02023E0C, $02023E12, $08083E04, $08083E10, $08083E0C, $08083E12
long $21110F00, $26263628, $21121E04, $21121E10, $21121E0C, $21121E28, $21121E12, $21000000
long $31123E00, $22222A04, $22222A10, $2222320C, $22222212, $14224910, $221E0200, $0A12121E
long $1C000804, $1C000810, $1C002418, $1C001428, $1C001400, $1C081408, $37000000, $3C000000
long $1C000804, $1C000810, $1C002418, $1C001400, $0E000804, $0E000810, $0E002418, $0E001400
long $3E1A0C0B, $3A001428, $1E000804, $1E000810, $1E002418, $1E001428, $1E001400, $04000000
long $3E000000, $22000402, $22000810, $22002418, $22001400, $21000810, $1A020202, $21001400

long $05030305, $01010101, $063F0604, $183F1808, $0808083E, $1C3E0808, $1C1E1E1C, $0E1E1E0E
long $08080808, $20202020, $00000101, $40400101, $7E007E00, $00000000, $3E3E3E3E, $0C1E1E0C
long $121E0C00, $0A0A1E00, $0C043F00, $1121111E, $1A161A16, $0C0C0A0A, $00000000, $0B0C30C0
long $223E0808, $24242724, $26252525, $00000000, $70101F10, $76121416, $00000000, $00000000
long $00000000, $00080808, $00000000, $0A3F1414, $2828180C, $4A4C380E, $33514B0C, $00000000
long $04040404, $10101010, $00000014, $08087F08, $0C000000, $00003E00, $0C000000, $04040810
long $12212121, $08080808, $02040830, $1010100C, $103F1112, $1010100E, $2222261A, $02040408
long $2222121C, $10203C22, $18000018, $0C00000C, $18040418, $003E003E, $0C10100C, $00080810
long $026D3525, $223E2214, $2222221E, $02010101, $11212121, $02021E02, $02021E02, $22213901
long $2222223E, $08080808, $10101010, $120A0606, $02020202, $21252D2D, $32322A2A, $12212121
long $02021E22, $12212121, $120A0E12, $2020100C, $08080808, $22222222, $1C141422, $36365649
long $22140808, $08080814, $01020408, $08080808, $10100804, $10101010, $00221214, $00000000
long $00000000, $22223C20, $22222226, $02020202, $32222222, $02023E22, $04040404, $22222222
long $22222226, $08080808, $10101010, $120A0E12, $08080808, $4949495B, $22222226, $21212121
long $22222226, $22222222, $02020226, $20300E02, $04040404, $32222222, $14142222, $36555549
long $120C0C12, $0C0C1212, $02040810, $08080608, $08080808, $08083008, $00394E00, $22222222
long $14080808, $00000000, $04080808, $04080808, $1010E000, $08080808, $00000000, $10080808
long $0000E010, $10080808, $04080808, $00000000, $1010E010, $04040300, $00000304, $04040304
long $0000FF00, $08080808, $0808FF08, $081CFF1C, $08080F08, $0808F808, $0000FF08, $0808FF00
long $081C1F1C, $081CFC1C, $001CFF1C, $081CFF1C, $00000F08, $0000F808, $08080F00, $0808F800
long $08080808, $08080800, $08080808, $0408081C, $021E021E, $00000000, $08080F08, $05063C06
long $05063C06, $0102FC02, $80403F40, $0A0AFB0A, $00E000E0, $08FF00FF, $00030003, $01013F01
long $00000000, $0008083E, $0000003C, $0000003C, $1010E000, $32222222, $1E211E21, $3434354D
long $0B0B2B26, $00000008, $04023F02, $08103F10, $031C601C, $44ECABAB, $111B2A0A, $04080800
long $223E2214, $223E2214, $223E2214, $223E2214, $223E2214, $223E2214, $121E3214, $02010101
long $02021E02, $02021E02, $02021E02, $02021E02, $08080808, $08080808, $08080808, $08080808
long $11212123, $32322A2A, $12212121, $12212121, $12212121, $12212121, $12212121, $120C0C12
long $12232529, $22222222, $22222222, $22222222, $22222222, $08080814, $1E222222, $4222120A
long $22223C20, $22223C20, $22223C20, $22223C20, $22223C20, $22223C20, $09097E48, $02020202
long $02023E22, $02023E22, $02023E22, $02023E22, $08080808, $08080808, $08080808, $08080808
long $21212121, $22222226, $21212121, $21212121, $21212121, $21212121, $21212121, $00003F00
long $23252931, $32222222, $32222222, $32222222, $32222222, $0C0C1212, $22222226, $0C0C1212

long $01E11D0D, $7F010101, $00000004, $00000008, $00000000, $00000008, $00001018, $00000206
long $00000F0C, $3F202020, $00000000, $40404040, $00000000, $FF000000, $0000003E, $00000000
long $00003F11, $0000120A, $00001F22, $00003F12, $00000000, $00000808, $00000000, $08080808
long $00081414, $00E42424, $00E42424, $080F0000, $00101090, $00101094, $0E0C0A01, $08040201
long $00000000, $00000008, $00000000, $0000000A, $0000081E, $00000031, $0000007E, $00000000
long $00301808, $00060C08, $00000000, $00000008, $0004080C, $00000000, $0000000C, $00010202
long $0000001E, $0000003E, $0000003E, $0000000E, $00000010, $0000000E, $0000001C, $00000002
long $0000001C, $0000000E, $00000018, $0004080C, $00000020, $00000000, $00000002, $00000008
long $0000001C, $00000041, $0000001E, $0000003C, $0000000F, $0000003E, $00000002, $0000003C
long $00000022, $0000003E, $0000000F, $00000022, $0000003E, $00000021, $00000022, $0000001E
long $00000002, $0060101C, $00000022, $0000001E, $00000008, $0000001C, $00000008, $00000022
long $00000041, $00000008, $0000003F, $00780808, $00402020, $001E1010, $00000000, $00007F00
long $00000000, $0000007C, $0000001E, $0000003C, $0000002C, $0000003C, $00000004, $001C203C
long $00000022, $00000008, $000E1010, $00000022, $00000008, $00000049, $00000022, $0000001E
long $0002021E, $0020203C, $00000002, $0000001E, $00000038, $0000002E, $00000008, $00000022
long $00000021, $00030404, $0000003F, $00300808, $00080808, $000E0808, $00000000, $0000003E
long $0000E314, $0000FF00, $00000304, $00007F04, $00000708, $0000E010, $00000000, $0000E010
long $00000000, $0000FF10, $00000304, $0000FF00, $00000708, $0000F008, $00000000, $0000F008
long $00000000, $08080808, $08080808, $08080808, $08080808, $08080808, $00000000, $08080808
long $08080808, $08080808, $00000000, $08080808, $00000000, $00000000, $08080808, $08080808
long $08080808, $00080808, $1C2A4908, $0000003C, $00001804, $00000000, $0038C808, $00040404
long $00040404, $00000000, $00000000, $000A0A0A, $00000000, $08080808, $00000000, $00000000
long $00000000, $0000003E, $00000000, $00000000, $00000708, $0002022E, $18201E21, $000000D8
long $00000006, $00000000, $00000000, $00000000, $071C601C, $00000044, $00000011, $003C2202
long $00000041, $00000041, $00000041, $00000041, $00000041, $00000041, $00000071, $3020103C
long $0000003E, $0000003E, $0000003E, $0000003E, $0000003E, $0000003E, $0000003E, $0000003E
long $0000000F, $00000022, $0000001E, $0000001E, $0000001E, $0000001E, $0000001E, $00000021
long $0000001F, $0000001C, $0000001C, $0000001C, $0000001C, $00000008, $00000002, $0000003A
long $0000007C, $0000007C, $0000007C, $0000007C, $0000007C, $0000007C, $00000076, $1810083C
long $0000003C, $0000003C, $0000003C, $0000003C, $00000008, $00000008, $00000008, $00000008
long $0000001E, $00000022, $0000001E, $0000001E, $0000001E, $0000001E, $0000001E, $00000004
long $0000001F, $0000002E, $0000002E, $0000002E, $0000002E, $00030404, $0002021E, $00030404
'End of ANSI font with parallax graphics
'
{
'Windows terminal font
long $00000000, $81C37E00, $FFFF7E00, $77220000, $3E1C0800, $3C3C1800, $7E3C1800, $00000000
long $FFFFFFFF, $7E3C0000, $81C3FFFF, $5C707C00, $66663C00, $9898F800, $FEC6FE00, $DB180000
long $07030100, $70604000, $7E3C1800, $66666600, $DBDBFE00, $0CC67E00, $00000000, $7E3C1800
long $7E3C1800, $18181800, $18000000, $0C000000, $00000000, $24000000, $08080000, $7F7F0000
long $00000000, $1E1E0C00, $66666600, $7F363600, $033E0C0C, $23000000, $1B1B0E00, $0C0C0C00
long $0C183000, $180C0600, $66000000, $18000000, $00000000, $00000000, $00000000, $60400000
long $73633E00, $0F0C0800, $33331E00, $30331E00, $3C383000, $03033F00, $03061C00, $63637F00
long $33331E00, $33331E00, $1C000000, $1C000000, $0C183000, $00000000, $180C0600, $30331E00
long $63633E00, $331E0C00, $66663F00, $63663C00, $66361F00, $06467F00, $46667F00, $63663C00
long $33333300, $0C0C1E00, $30307800, $36666700, $06060F00, $7F776300, $67636300, $63361C00
long $66663F00, $63361C00, $66663F00, $33331E00, $0C2D3F00, $33333300, $33333300, $63636300
long $33333300, $33333300, $19737F00, $0C0C3C00, $03010000, $30303C00, $63361C08, $00000000
long $00180C0C, $00000000, $06060700, $00000000, $30303800, $00000000, $06361C00, $00000000
long $06060700, $00181800, $00303000, $06060700, $18181E00, $00000000, $00000000, $00000000
long $00000000, $00000000, $00000000, $00000000, $06040000, $00000000, $00000000, $00000000
long $00000000, $00000000, $00000000, $0C0C3800, $18181800, $0C0C0700, $735BCE00, $08000000
long $33331E00, $00333300, $000C1830, $00331E0C, $00333300, $000C0603, $1C36361C, $00000000
long $00331E0C, $00333300, $000C0603, $00363600, $00361C08, $00180C06, $0C003300, $1E33331E
long $3F0C1830, $00000000, $1B1E7C00, $00331E0C, $00333300, $000C0603, $00331E0C, $000C0603
long $00666600, $331E0033, $33330033, $1E0C0C00, $0606663C, $33333333, $1111110F, $1818D870
long $000C1830, $000C1830, $000C1830, $000C1830, $003B6E00, $63003B6E, $33331E00, $33331E00
long $000C0C00, $00000000, $00000000, $33634200, $3667C600, $000C0C00, $00000000, $00000000
long $24924924, $55AA55AA, $B66DDBB6, $18181818, $18181818, $18181818, $66666666, $00000000
long $00000000, $66666666, $66666666, $00000000, $66666666, $66666666, $18181818, $00000000
long $18181818, $18181818, $00000000, $18181818, $00000000, $18181818, $18181818, $66666666
long $66666666, $00000000, $66666666, $00000000, $66666666, $00000000, $66666666, $18181818
long $66666666, $00000000, $00000000, $66666666, $18181818, $00000000, $00000000, $66666666
long $18181818, $18181818, $00000000, $FFFFFFFF, $00000000, $0F0F0F0F, $F0F0F0F0, $FFFFFFFF
long $00000000, $33331E00, $33333F00, $36367F00, $26233F00, $00000000, $00000000, $6E000000
long $1E0C3F00, $33331E00, $63633E00, $0C063C00, $6E000000, $3E600000, $03063C00, $331E0000
long $003F0000, $0C0C0000, $180C0600, $060C1800, $D8700000, $18181818, $0C0C0000, $DBCE0000
long $66663C00, $00000000, $00000000, $2020E000, $36361B00, $18301E00, $3C3C0000, $00000000

long $00000000, $99BD81A5, $E7C3FFDB, $3E7F7F7F, $1C3E7F7F, $18E7E7FF, $187EFFFF, $3C7E7E3C
long $C38181C3, $66424266, $99BDBD99, $33331F4E, $7E183C66, $1E1818F8, $E6C6C6C6, $7EE7E77E
long $071F7F1F, $707C7F7C, $7E181818, $00006666, $D8D8DEDB, $3C66663C, $7F000000, $7E181818
long $18181818, $7E181818, $18307F30, $0C067F06, $7F030303, $2466FF66, $3E3E1C1C, $1C1C3E3E
long $00000000, $000C0C1E, $00000024, $7F363636, $30301E03, $060C1833, $337B5F0E, $00000006
long $0C060606, $18303030, $663CFF3C, $18187E18, $00000000, $00007F00, $00000000, $060C1830
long $676F6B7B, $0C0C0C0C, $060C1830, $30301C30, $307F3336, $30301F03, $33331F03, $0C183060
long $333B1E37, $18183E33, $1C00001C, $1C00001C, $0C060306, $007E007E, $18306030, $000C0C18
long $037B7B7B, $333F3333, $66663E66, $63030303, $66666666, $06263E26, $06263E26, $63730303
long $33333F33, $0C0C0C0C, $33333030, $36361E36, $66460606, $63636B7F, $737B7F6F, $63636363
long $06063E66, $7B736363, $66363E66, $33180E03, $0C0C0C0C, $33333333, $33333333, $366B6B63
long $331E0C1E, $0C0C1E33, $46060C18, $0C0C0C0C, $30180C06, $30303030, $00000000, $00000000
long $00000000, $333E301E, $6666663E, $0303331E, $3333333E, $033F331E, $06061F06, $3333336E
long $66666E36, $1818181E, $3030303C, $361E3666, $18181818, $6B6B6B3F, $3333331F, $3333331E
long $6666663B, $3333336E, $066E7637, $1806331E, $0606063F, $33333333, $33333333, $6B6B6363
long $1C1C3663, $66666666, $0618313F, $0C060306, $18180018, $0C183018, $00000000, $6363361C
long $33030303, $33333333, $033F331E, $333E301E, $333E301E, $333E301E, $333E301F, $0303331E
long $033F331E, $033F331E, $033F331E, $1818181E, $1818181E, $1818181E, $3F33331E, $3F33331E
long $031F0323, $1BFED87F, $1B1B7F1B, $3333331E, $3333331E, $3333331E, $33333333, $33333333
long $66666666, $33333333, $33333333, $33030333, $06063F06, $3F0C3F1E, $3179110F, $1818187E
long $333E301E, $1818181E, $3333331E, $33333333, $3333331F, $737B6F67, $007F007E, $007F001E
long $0303060C, $03033F00, $30303F00, $C3760C1B, $DBF6EC1E, $1E1E0C0C, $333366CC, $CCCC6633
long $49249249, $55AA55AA, $DBB66DDB, $18181818, $18181F18, $1F18181F, $66666766, $66667F00
long $1F18181F, $67606067, $66666666, $6760607F, $7F606067, $00007F66, $1F18181F, $18181F00
long $0000F818, $0000FF18, $1818FF00, $1818F818, $0000FF00, $1818FF18, $F81818F8, $6666E666
long $FE0606E6, $E60606FE, $FF0000E7, $E70000FF, $E60606E6, $FF0000FF, $E70000E7, $FF0000FF
long $0000FF66, $FF0000FF, $6666FF00, $0000FE66, $F81818F8, $F81818F8, $6666FE00, $6666E766
long $FF0000FF, $00001F18, $1818F800, $FFFFFFFF, $FFFF0000, $0F0F0F0F, $F0F0F0F0, $0000FFFF
long $33337B6E, $3333331B, $03030303, $36363636, $26060C06, $3333137E, $66666666, $1818183B
long $1E333333, $33333F33, $36366363, $3333331E, $76DBDBDB, $3E6F6B7B, $03033F03, $33333333
long $00003F00, $000C0C3F, $00060C18, $00180C06, $181818D8, $1B181818, $0C003F00, $DBCE0073
long $00003C66, $00003838, $00001800, $2C262220, $00003636, $00003E0C, $3C3C3C3C, $00000000

long $00000000, $00007EC3, $00007EFF, $0000081C, $00000008, $00007E18, $00007E18, $00000000
long $FFFFFFFF, $00003C7E, $FFFFC381, $00001E33, $00001818, $00000E1F, $000367E7, $000018DB
long $00000103, $00004060, $0000183C, $00006666, $0000D8D8, $007E6330, $00007F7F, $007E183C
long $00001818, $0000183C, $00000000, $00000000, $00000000, $00000000, $00007F7F, $00000808
long $00000000, $00000C0C, $00000000, $00003636, $000C0C1F, $00003133, $00006E3B, $00000000
long $00003018, $0000060C, $00000000, $00000000, $00061C1C, $00000000, $00001C1C, $00000103
long $00003E63, $00003F0C, $00003F33, $00001E33, $00007830, $00001E33, $00001E33, $00000C0C
long $00001E33, $00000E0C, $0000001C, $000C181C, $00003018, $00000000, $0000060C, $00000C0C
long $00003E03, $00003333, $00003F66, $00003C66, $00001F36, $00007F46, $00000F06, $00007C66
long $00003333, $00001E0C, $00001E33, $00006766, $00007F66, $00006363, $00006363, $00001C36
long $00000F06, $0078303E, $00006766, $00001E33, $00001E0C, $00001E33, $00000C1E, $00003636
long $00003333, $00001E0C, $00007F63, $00003C0C, $00004060, $00003C30, $00000000, $00FF0000
long $00000000, $00006E33, $00003B66, $00001E33, $00006E33, $00001E33, $00000F06, $1E33303E
long $00006766, $00007E18, $1E333330, $00006766, $00007E18, $0000636B, $00003333, $00001E33
long $0F063E66, $78303E33, $00000F06, $00001E33, $00001C36, $00006E33, $00000C1E, $00003636
long $00006336, $0F18303C, $00003F23, $0000380C, $00001818, $0000070C, $00000000, $0000007F
long $0F0C1E33, $00006E33, $00001E33, $00006E33, $00006E33, $00006E33, $00006E33, $0F0C1E33
long $00003E03, $00003E03, $00003E03, $00007E18, $00007E18, $00007E18, $00003333, $00003333
long $00003F23, $0000F71B, $00007B1B, $00001E33, $00001E33, $00001E33, $00006E33, $00006E33
long $0F18303C, $00001E33, $00001E33, $000C0C1E, $00007F03, $00000C0C, $000061B1, $00000E1B
long $00006E33, $00007E18, $00001E33, $00006E33, $00003333, $00006363, $00000000, $00000000
long $00001E33, $00000003, $00000030, $00F83061, $00C0FCCD, $00000C1E, $0000CC66, $00003366
long $92492492, $55AA55AA, $6DDBB66D, $18181818, $18181818, $18181818, $66666666, $66666666
long $18181818, $66666666, $66666666, $66666666, $00000000, $00000000, $00000000, $18181818
long $00000000, $00000000, $18181818, $18181818, $00000000, $18181818, $18181818, $66666666
long $00000000, $66666666, $00000000, $66666666, $66666666, $00000000, $66666666, $00000000
long $00000000, $18181818, $66666666, $00000000, $00000000, $18181818, $66666666, $66666666
long $18181818, $00000000, $18181818, $FFFFFFFF, $FFFFFFFF, $0F0F0F0F, $F0F0F0F0, $00000000
long $00006E7B, $0006031F, $00000303, $00006636, $00003F23, $00001E33, $0306DE66, $00007018
long $00003F0C, $00001E33, $00007736, $00001E33, $00000000, $00000003, $00003C06, $00003333
long $0000003F, $0000003F, $0000003F, $0000003F, $18181818, $00000E1B, $0000000C, $00000073
long $00000000, $00000000, $00000000, $00003038, $00000000, $00000000, $00003C3C, $00000000
'
}