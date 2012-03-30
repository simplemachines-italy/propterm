''************************************
''*  PropTerm Ansi Terminal v1.0     *
''************************************
''
'This object implements a limited function ANSI escape sequence
'controlled terminal.
'
CON
  'lots of dispau constatnts
  cols = vgatext#cols
  rows = vgatext#rows
  chrs = cols * rows
  chrsDiv4 = chrs >> 2
  chrsDiv2 = chrs >> 1
  'Constants for screen move/fill
  colsx2 = cols << 1
  'Escape coltrol sequence Paramaters
  'Controls max number of allowed parameters
  'Currently set font command uses 13 paramaters
  MaxParams = 14
  'text attributes constants
  tc_InitColor    = $A800A800   'boot up colors, white on black
  tc_FontColorMsk = $FC00FC00   'used to select only font color
  tc_BgColorMsk   = $00FC00FC   'used to select only background color
  tc_ColorMsk     = $FCFCFCFC   'used to make sure colors have no sync bits set
  tc_FontHiBitMsk = $A800A800   'used to  select only the font color hi bits.
  'Text attributes
  ta_Dim = 1                    'Dim text color cancels bright
  ta_Bright = 2                 'Brighten text color, cancels Dim
  ta_Invert = 4                 'Invert text/background color
  ta_AltFont =  8               'Select alternate font set
  'Text colors
  tc_Black   = $00000000
  tc_Red     = $80008000
  tc_Green   = $20002000
  tc_Yellow  = $A000A000
  tc_Blue    = $08000800
  tc_Magenta = $88008800
  tc_Cyan    = $28002800
  tc_White   = $A800A800
  'Line Wrap mode constants
  c_LineWrapOff  = 0     'must be zero for no line wrap
  c_LineWrapOn = 1
  'ANSI helper command codes and parsing constants
  c_NoCommand   =  0            'no command waiting constant, MUST BE ZERO
  c_InsertLine  =  1            'P1 = current line, P2 = number of lines, DrawColor must be supplied.
  c_DeleteLine  =  2            'P1 = current line, P2 = number of lines, DrawColor must be supplied.
  c_DecodeParam =  3            'Decode three digit decimal number to binary, TempParam must be supplied
  c_SetAttribs   = 4            'Set the text attribute/colors from the supplied paramaters
  c_ScrollUp     = 5            'Scroll selected screen region up, no params. DrawColor must be supplied in LongParam.
  c_ScrollDown   = 6            'Scroll selected screen region down, no params. DrawColor must be supplied in LongParam.
  c_ScrollRegion = 7            'Set first and last rows to scroll with scroll up and down commands.
                                'P1 = first row, P2 = last row both inclusive.
  'Next three clear screen commands must remain in order                              
  c_FillFromRow = 8             'Clear screen from row to end, P0 = current row. DrawColor must be supplied in LongParam.
  c_FillToRow =   9            'Clear screen from start to row, P0 = current row. DrawColor must be supplied in LongParam.
  c_FillScreen  = 10            'no params, DrawColor must be supplied.
  'Next three clear row commnads must stay in order
  c_EraseEol    = 11            'Clear row specified in P0 from col in P1 to row end. DrawColor must be supplied in LongParam.
  c_EraseBol    = 12            'Clear row specified in P0 from start to row in P1. DrawColor must be supplied in LongParam.
  c_EraseLine   = 13            'Clear row specified in P0. DrawColor must be supplied in LongParam.
  'Remaining commands
  c_InsertChars = 14            'Insert n blank characters at the current colum and row. DrawColor must be supplied in LongParam. Param[0] = number of chars
  c_DeleteChars = 15            'Delete n characters at the current colum and row. DrawColor must be supplied in LongParam. Param[0] = number of chars
  c_EraseChars  = 16            'Erase n characters at the current colum and row. DrawColor must be supplied in LongParam. Param[0] = number of chars
  'Comunications helper command codes and parsing constants
  c_ComReadByte   =  1           'no params, result is returned in ComLongParams[0]
  c_ComSendAck    =  2           'Send an ACK in responce to ENQ control character
  c_ComSendXY     =  3           'P1 = cursor Y value P2 = cursor X value.
  c_ComUpdateFont =  4           '13 params contain glyph index and scanlines
  c_ComSetTab      = 5           'Set the tab stop for the current column. 
  c_ComClearTab    = 6           'Clear the tab stop for the current column.
  c_ComClearTabs   = 7           'Clear all set tab stops, no paramaters.
  c_ComNextTab     = 8           'Get next tab stop, On exit: Param[0] = new column
  c_ComPriorTab    = 9           'Get prior tab stop, On exit: Param[0] = new column
  'Save/Load terminal state constants
  c_StateVarCnt =  7
  'Cursor display pattern
  cursor_on_pattern  = vgatext#cursor_on_pattern
  cursor_off_pattern = vgatext#cursor_off_pattern
  'ANSI control sequence constants
  c_CSIJumpTableSize = 64
  c_ESCCodeJumpTableSize = 52
  'EEprom address constants
  c_EEpromSettingsAdr      =  com#c_EEpromSettingsAdr       'EEprom address of terminal settings (must fit in 2 pages)
  c_EEpromFontStartAdr     =  com#c_EEpromFontStartAdr      'Start of font save area. Leave first region for settings
  c_EEpromFontByteSize     =  com#c_EEpromFontByteSize      'Fonts use 12 bytes per glyph, there are 256 glyphs  
  c_EEpromPageBytes        =  com#c_EEpromPageBytes         'Bytes per eeprom page
  c_EEpromFontSaveSize     =  com#c_EEpromFontSaveSize     'Total pages per font * eeprom page size
  
OBJ
  vgatext : "vga_hicolor_text"
  com     : "ComHelper"
  ansihlp : "AnsiHelp"
  
VAR
  long  cog                     'cog flag/id
  '
  'Global memory to save/ load terminal state from.
  'State variables must be in same order as used by terminal cog.
  'The constant "c_StateVarCnt" must have the correct variable count. 
  long SS_BaseColor
  long SS_TextAttribs
  long SS_ScrlFirstLine
  long SS_ScrlLastLine 
  long SS_LineWrapState
  long SS_SaveCursorY  
  long SS_SaveCursorX
  'Color table hold 16 default colors
'  long BaseColors[16]
  'Paramaters passed when calling ansi helper command, MaxParams + 3
  long HlpCommand
  long HlpLongParams[MaxParams + 4]                     'used to transfer paramaters to and from helper functions
  'Paramaters passed when calling comunications helper command, 3
  long ComCommand
  long ComLongParams[MaxParams + 4]                     'used to transfer paramaters to and from helper functions
  word TempWord
  'Column offset table (much faster then a software mult)
  word ColOffsets[rows]
  'cursor control bytes
  byte  cx1, cy1
  'screen buffer - one byte per character
  byte  screen[cols*rows]
  'row colors
  byte  colors[cols*rows*2]
  'Byte array for tab stops
  byte  TabStops[cols]

PUB initialize(aBaudRate, aDataBits, aParity) | i, j
  'Initialize the column offsets table
  repeat i from 0 to (rows -1)
    j:= i * cols
    ColOffsets[i]:= j
  ColOffsetAdr:= @ColOffsets
  'Clear all tab stops
  InitTabs(8)
  'Initialize the escape control sequence jump table address
  CSICodesAdr:=@aCSIJumpTable
  'Initialize 7/8 bit control code jump table address
  Ctrl7CodesAdr:=@aCtrl7JumpTable                                'Set address of 7 bit control codes table
  Ctrl8CodesAdr:=@aCtrl8JumpTable                                'Set address of 8 bit control codes table
  'Initialize the escape single code jump table
  EscCodesAdr:=@aEscJumpTable                                  'Set address of table
  'Setup the screen character and color memoru addresses
  ScreenAdr:= @screen
  ColorAdr:= @colors
  CursorXAdr:=@cx1
  CursorYAdr:=@cx1 + 1
  'Start the communications helper
  ComCommandPtr:=@ComCommand
  ComCommandDataPtr:=@ComLongParams
  ComCommand := 0
  com.start(@ComCommand, vgatext.GetFontAdr, @TabStops, @cx1, cols, aBaudRate, aDataBits, aParity)
  
PUB start(aResetStateAdr, aBaudRate, aDataBits, aParity) : okay
  'Must call "initialize" before start
  com.ChangeDataFormat(aBaudRate, aDataBits, aParity)
  'start ansi terminal driver and clear the screen
  CommandPtr:= @HlpCommand
  CommandDataPtr:=@HlpLongParams
 'Start the video rasterizer
  vgatext.start(16, @screen, @colors, @cx1)
  CursorPatternAdr := vgatext.GetCursorPatternAdr
  CursorOnPattern := cursor_on_pattern
  'Start the ansi helper cog
  HlpCommand:= 0
  HlpLongParams:= tc_InitColor
  ansihlp.start(@HlpCommand, @screen, @colors, @ColOffsets, @aBaseColors, @cx1, aResetStateAdr, cols, rows)
  'Initialize save state global mem in case restore is called before save
  SaveStateBufAdr := @SS_BaseColor
  longmove(SaveStateBufAdr, aResetStateAdr, c_StateVarCnt)
  'Start the terminals main cog
  okay := cog := cognew(@entry, aResetStateAdr) + 1

  
PUB stop
'' Stop terminal - frees a cog
  if cog
    cogstop(cog~ - 1)
  ansihlp.stop
  com.stop
  vgatext.stop

PUB InitTabs(TabSpace) | i, j
  j:=0
  repeat i from 0 to cols
    if (j == 0) and (TabSpace > 0)
      TabStops[i] := com#c_TabSet
    else
      TabStops[i] := com#c_TabClear
    j += 1
    j //= TabSpace  
  
PUB TermModeANSI
'Adding the control codes for CSI and ESC will allow
'ANSI control sequences to work                               
  aCtrlJumpESC := (@DoStartEscape1 - @entry) >> 2     'ESC 7 bit control code
  aCtrlJumpCSI := (@DoStartEscape2 - @entry) >> 2     'CSI constrol sequence initiator, start an escape control sequence, "ESC ["

PUB TermModeTTY
'Removing the control codes for CSI and ESC will prohibit
'ANSI control sequences from working
  aCtrlJumpESC := 0
  aCtrlJumpCSI := 0
  
PUB ResolveColorIdx(aIdx) : Color | Adr
  Adr :=  @aBaseColors + (aIdx & 15) * 4
  LongMove(@Color, Adr, 1) 
'  Color :=  BaseColors[Idx & 15]
  
PUB ChangeBaudRate(aBaudRate, aDataBits, aParity)
  com.ChangeDataFormat(aBaudRate, aDataBits, aParity)
    
PUB tx(txbyte)
'' Send byte (may wait for room in buffer)
  com.KbdWrite(txbyte)

PUB  KbdFifoSpace : Room
  Room := com.KbdFifoSpace

PUB  KbdFlushFifo
  com.KbdFlushFifo
  
PUB CrAsNewLineOn
'' Treat CR control characters as CR+LF
  aCrJumpTableEntry := (@DoCrLf - @entry) >> 2
          
PUB CrAsNewLineOff
'' Treat CR control characters as normal CR
  aCrJumpTableEntry := (@DoCarRet - @entry) >> 2
          
PUB LocalModeOn
'' Set local mode, terminal only reads local (from KBD).
  com.LocalModeOn
        
PUB LocalModeOff
'' Set terminal to read from KBD only. KBD is not sent to COM port.
'' Terminal output is ignored and not sent to COM port.
  com.LocalModeOff

PUB LocalEchoOn
'' Set local echo mode on, terminal echos KBD data.
  com.EchoOn
        
PUB LocalEchoOff
'' Set terminal to write KBD data to COM port only.
  com.EchoOff

PUB GetSaveStateAdr : Adr
'' Get the address of the terminals save state buffer
  Adr := @SS_BaseColor
    
PUB GetScreenCharsAdr : Adr
'' Get the address of the screen character table
  Adr := @screen
    
PUB GetScreenColorsAdr : Adr
'' Get the address of the screen color table
  Adr := @colors

PUB GetFontAdr : Adr
'' Get the address of the font table
  Adr := vgatext.GetFontAdr
  
PUB EEpromReset
'' Perform a software reset on the eeprom device.
  com.EEpromReset

PUB EEPromReady : IsReady
'' See if last eeprom write has completed    
  IsReady := com.EEPromReady

PUB EEpromRead(aEEpromAdr, aMemAdr, aByteCount) : Ok
'' Copy the EEprom contents to global memory
  Ok := com.EEpromRead(aEEpromAdr, aMemAdr, aByteCount) 
        
PUB EEpromWrite(aEEpromAdr, aMemAdr, aByteCount) : Ok
'' Write global memory to a single eeprom page    
  Ok := com.EEpromWrite(aEEpromAdr, aMemAdr, aByteCount) 


DAT

'***********************************
'* Assembly language ansii terminal *
'***********************************

                        org   0
'
'
' Entry
'
{ Screen chars and colors are set by clearScreen no need to do it here
entry                   mov     TxtPtr, ScreenAdr       'Next char address to write
                        mov     ColPtr, ColorAdr        'Next color address to write
}                        
entry                   mov     RptCharCnt, #0                     'DO NOT REMOVE OPCODE, if (@reset_term - @entry) = 0, jump table will not work.              
                        'Reset the terminal to known defaults
reset_term              call    #ClrParams                        'Set EscState = 0 and clear Esc paramaters
                        'display the cursor incase hidden
                        wrlong  CursorOnPattern, CursorPatternAdr
                        'Apply the initail values
                        call    #ResetState
                        call    #ApplyState
                        'clear the screen and we are ready to gp
                        call    #ClearScreen                      'Erase the screen and home cursor
                        'Terminal is ready to go...
                        'Process next character from RS232 port
comrx                   tjz     RptCharCnt, #real_comx
                        'we are repeating last displayable character
                        mov     t1, RptCharLast
                        sub     RptCharCnt, #1
                        jmp     #writechar
                        'read a char from the com helper cog
real_comx               mov     t1, #c_ComReadByte
                        call    #ExecComCmd
                        rdlong  t1, ComCommandDataPtr
                        and     t1, #255
                        'T1 has our data byte, see if we have a control char
                        test    t1, #96         wz      'control chars 00h..1Fh or 80h..9Fh
        if_z            jmp     #DoCtrlCode
                        'See if we are in an escape sequence
                        tjnz    EscState, #EscParseNext                        
                        'We have a normal character write to screen
writechar               mov     RptCharLast, t1         'Save last character displayed
                        test    TextAttribs, #ta_AltFont wz 'See what font set is selected
        if_nz           sub     t1, #32                 'Select alternate font set
        
                        wrbyte  t1, TxtPtr              'write char to screen memory
                        add     TxtPtr, #1              'point to next screen location
                        wrword  DrawColor, ColPtr       'write the current color
                        add     ColPtr, #2              'point to next color location
                        'advance the cursor to next location
                        rdbyte  t1, CursorXAdr          'Read current cursor X value
                        add     t1, #1                  'Point to next column
                        cmp     t1, #cols       wc      'See if past end of current row
        if_nc           mov     t1, #0                  'wrapped to next line
                        wrbyte  t1, CursorXAdr          'Read current cursor X value
        if_c            jmp     #comrx                  'get next character
                        'at end of row, switch to next row if wrap mode allows
                        tjz     LineWrapState, #NoWrapEol
                        rdbyte  t1, CursorYAdr          'Read current cursor Y value
                        'see if we are on the last line of the current scroll region
                        cmp     t1, ScrlLastLine   wz   
        if_z            jmp     #scrlscreen
                        'advance the current row if we are not on the last line
                        add     t1, #1                  'Point to next column
                        cmp     t1, #rows       wc, wz  'See if past end of current screen
        if_z            jmp     #NoWrapEol              'past end of screen treat as no scroll
        if_c            wrbyte  t1, CursorYAdr          'Save updated Y cursor value if not last line
                        jmp     #comrx                  'get next character
                        'Screen needs to scroll up, Y at end of screen
scrlscreen              call    #scrollscreen
                        sub     TxtPtr, #cols           'point to start of line in character table
                        sub     ColPtr, #colsx2         'point to start of line in color table
                        jmp     #comrx
                        'we have written last char on line but can not line wrap
NoWrapEol               mov     t1, #cols-1         
                        wrbyte  t1, CursorXAdr          'Read current cursor X value
                        sub     TxtPtr, #1              'point to prior character table entry
                        sub     ColPtr, #2              'point to prior color table entry
                        jmp     #comrx
                        'Execute a control code if we support it, T1 = 7 or 8 bit control code
                        'Bits 6,5 are zero, checked by parser before calling
                        
DoCtrlCode              test    t1, #128          wz    'See if we have an 8 bit ctrl code
              if_nz     and     t1, #$1F                'remove high bit so index is 0..31
                        mov     t3, t1                  'T3 = control code index (0..31, 32..63) 
                        shl     t3, #1                  'Mult control code by 2
              if_z      add     t3, Ctrl7CodesAdr       'Add 7 bit control code table address to offset
              if_nz     add     t3, Ctrl8CodesAdr       'Add 7 bit control code table address to offset
                        rdword  t3, t3                  'read jump address
                        tjnz    t3, t3                  'if address <> 0 jump to it
                        jmp     #comrx                  'get next character
{                        
DoCtrlCode              test    t1, #128          wz    'See if we have an 8 bit ctrl code
              if_nz     xor     t1, #160                'Flip bits 7 and 5, bit 7 = 0, bit 5 = 1 (-128, +32)
                        mov     t3, t1                  'T3 = control code index (0..31, 32..63) 
                        shl     t3, #1                  'Mult control code by 2
                        add     t3, CtrlCodesAdr       'Add control code x 2, 16 bit values
                        rdword  t3, t3                  'read jump address
                        tjnz    t3, t3                  'if address <> 0 jump to it
                        jmp     #comrx                  'get next character
}                        
                        'Perform a carage return
DoCarRet                mov     t1, #0
                        call    #GotoX
                        jmp     #comrx                  'get next character
                        'Perform a new line, CR + LF
DoCrLf                  mov     t1, #0
                        call    #GotoX
                        'Perform a line feed
DoLineFeed              rdbyte  t1, CursorYAdr          'Read current cursor row Y value
                        cmp     t1, ScrlLastLine  wz    'See if we are at the last line of scroll region
              if_z      jmp     #DoLineFeedScrl
                        'if not on last scroll region row advance to next row 
                        add     t1, #1                  'Point to next line
                        cmp     t1, #rows         wz    'See if we have reached end of screen
              if_z      jmp     #comrx
                        'linefeed did not scroll so move one full line down
DoLineFeedNs            wrbyte  t1, CursorYAdr          'save new cursor row Y value
                        add     TxtPtr, #cols           'point to same column on next line in character table
                        add     ColPtr, #colsx2         'point to same column on next line in color table
                        jmp     #comrx                  'get next character
DoLineFeedScrl          'line feed must scoll screen to make room for new line              
                        call    #scrollscreen           'scrollup screen
                        jmp     #comrx
                        '
                        'Perform a backspace control caracter
DoBackSpace             rdbyte  t1, CursorXAdr          'Read current cursor X value
                        sub     t1, #1            wc    'backup one character if we can
              if_nc     call    #GotoX                  'backup one column
                        jmp     #comrx
                        '
                        'Do reverse line feed. Move to the prior row. if current
                        'row is first scroll region row shift entire scroll region
                        'down one line. Column position does not change.
DoRevLineFeed           rdbyte  t2, CursorYAdr          'Read curent row index into T2
                        cmp     t2, ScrlFirstLine   wz  'See if we are on the first scroll region row
        if_z            jmp     #DoRevLineFeed2         'We are one first row, shift scroll region down one row
                        sub     t2, #1              wc  'T2 = prior row index
        if_nc           rdbyte  t1, CursorXAdr          'Read the current column index into T1.
        if_nc           call    #GotoXY                 'Set new screen position
                        jmp     #comrx                  'Go process next char
                        'Scroll screen down, keep column same
DoRevLineFeed2          mov     t1, #c_ScrollDown       'Set command to send scoll screen down
                        call    #ExecHlpCmdWC           'Do scroll command.
                        jmp     #comrx
                        '
                        'Process an ENQ control request, we just send an ACK
DoENQCntrl              mov     t1, #c_ComSendAck        'Set command to send ACK in responce to ENQ
                        call    #ExecComCmd
                        jmp     #comrx
                        '
                        'Backup the cursor to the prior horizontal tab stop if any.
DoPriortHorzTab         mov     t1, #c_ComPriorTab
                        jmp     #ExecHorzTab
                        'Advance the cursor to the next horizontal tab stop if any.
DoNextHorzTab           mov     t1, #c_ComNextTab
ExecHorzTab             call    #ExecComCmd             'call the helper function
                        rdlong  t1, ComCommandDataPtr   'Read column returned, T1 = column
                        call    #GotoX                  'Set column on screen
                        jmp     #comrx
                        '
                        'Process the start of an escape sequence
DoStartEscape1          mov     EscState, #1
                        jmp     #comrx
                        '
                        'Process the start of an CSI sequence
DoStartEscape2          call    #ClrParams                            'Set EscState = 0 and clear parameters
                        mov     EscState, #2                          'indiacate we are in a CSI
                        jmp     #comrx
                        'We are in escape mode parse the next esc char in T1
                        'If register T1 in @..Z or a..{ we have end of esc
EscParseNext            cmp     EscState, #1    wz      'See if last char was an ESC (EscState = 1) lookin for code,
        if_z            jmp     #DoEscCode              'if it is process escape code.
                        cmp     EscState, #2    wc      'See if we are in a control sequence (EscState = 2 or 3)
        if_c            jmp     #EscParseDone           'EscState < 2 so we are done           
                        'we are in a control sequence (EscState - 2 or 3), see if we have an end of sequence character.
                        cmp     t1, #"@" wc
        if_c            jmp     #EscParse2              'See if T1 < min control sequence code, may be a paramater code
                        cmp     t1, #"{"        wc, wz  'Check last posible control sequence code.
        if_nc_and_nz    jmp     #EscParseDone           'No paramater chars greater so invalid code
                        'We have a "@"..."z" character
                        cmp     t1, #"Z"        wc, wz
        if_c_or_z       jmp     #CSIFoundCode
                        cmp     t1, #"a"        wc
        if_nc_or_z      jmp     #CSIFoundCode
                        'We are in parser state 2, reading parameters
EscParse2               cmp     t1, #";"        wz
              if_z      jmp     #EscParamDone
                        cmp     t1, #"?"        wz
              if_z      mov     EscState, #3            'This indicates we have a private code
              if_z      jmp     #EscParamDone
                        cmp     t1, #"0"        wc
              if_c      jmp     #EscParseDone
                        cmp     t1, #"9"        wc, wz
         if_nc_and_nz   jmp     #EscParseDone
                        'We have the next digit for the current parameter
                        cmp     ParamCharIdx, #3 wc
              if_nc     jmp     #comrx
                        'we have room for another digit
                        sub     t1, #$30                'Convert asci code to decomal value
                        shl     TempParam, #8           'Make room for next digit
                        or      TempParam, t1           'Add next digit to lsb
                        add     ParamCharIdx, #1        'Indicate another digit added
                        jmp     #comrx                  'Read next char
                        'We have a valid paramater, transfer to params array after decoding
                        'Execute a single character escape code if we support it, T1 = escape control code
DoEscCode               mov     EscState, #0            'Clear Escape code state
                        sub     t1, #"0"        wc      'See if we have an escape code we support
              if_c      jmp     #EscParseDone           'get next character
                        cmp     t1, #c_ESCCodeJumpTableSize         wc
              if_nc     jmp     #EscParseDone           'get next character
                        shl     t1, #1                  'Mult control code by 2
                        add     t1, EscCodesAdr         'Add control code x 2, 16 bit values
                        rdword  t1, t1                  'read jump address
                        tjnz    t1, t1
                        jmp     #EscParseDone           'get next character
                        '
                        'we found the end of the esc sequence
                        'if we have a partial param in the parser
                        'and we have room decode and add paramater
CSIFoundCode            mov     EscCodeChar, t1         'Save Escape code
                        'if we have a partial parameter in the buffer decode and save it
                        call    #DecodeSaveParam         'Decode paramater and save to params array, clear decode param vars
                        'We have the final code
                        call    #DecodeCSISeq           'Process Esc[ sequence
EscParseDone            call    #ClrParams              'Set EscState = 0 and clear all paramaters
                        jmp     #comrx                  'Parse next character
                        'Parse any valid paramater
EscParamDone            call    #DecodeSaveParam        'Convet asci decomal param to binary and save to params array
                        jmp     #comrx
                        'Set the current col as a tab stop
EscSetTab               mov     t1, #c_ComSetTab
                        call    #ExecComCmd
                        jmp     #EscParseDone
'
'
' SUPPORT FUNCTIONS START HERE
'
'
'Clear all escape sequence paramaters
clrparams               movd    :clrparam, #EscState      'Set first variable address
                        mov     t1, #MaxParams + 4        'Number of variables to clear                      
:clrparam               mov     EscState, #0
                        add     :clrparam, D0
                        djnz    t1, #:clrparam
clrparams_ret           ret
'
'
'Decode the paramater if it is valid and transfer to params array.
DecodeSaveParam         cmp     ParamCnt, #MaxParams wc, wz
         if_nc_and_nz   jmp     #EscParamClear                     'No room so look for escape code
                        tjz     ParamCharIdx, #EscParamClear       'No partial paramater so we are done
                        'Decode the asci decomal paramater value in TempParam by calling helper routine.
                        wrlong  TempParam, CommandDataPtr   'Pass current 3 digits to helper
                        mov     t1, #c_DecodeParam          'set command to decode param command
                        call    #ExecHlpCmd
                        'Read result and Save to TempParam
                        rdlong  TempParam, CommandDataPtr   'read param value and save into TempParam
                        'mov     ParamCharIdx, #0            'Indicate we consumed paramater
                        'Save the value to params array
                        mov     t1, #Params                 'T1 = params address
                        add     t1, ParamCnt
                        movd    :EscCopyParam, t1
                        add     ParamCnt, #1                'Do this now so we do not need a NOP
:EscCopyParam           mov     Params, TempParam
                        'Clear the temp param parser variables
EscParamClear           mov     TempParam, #0
                        mov     ParamCharIdx, #0
DecodeSaveParam_ret     ret
'
'
'Used to set correct color based on attributes

ApplyAttribs            mov     DrawColor, BaseColor        'Start out with our base colors
                        mov     t1, DrawColor               'Get ready for Dim/Bright command
                        and     t1, #FontColorHiBitMsk      'We only want the font color high bits, and no bg color bits
                        shl     t1, #1                      'Make font high color bits, low color bits 
                        'apply dim if required, only works with default colors from color table
                        test    TextAttribs, #ta_Dim     wz 'Test for DIM attribute
              if_nz     and     DrawColor, BgColorMsk       'Save background color from original
              if_nz     or      DrawColor, t1               'Set font color to half bright
                        'apply bright if required, only works with default colors from color table
                        test    TextAttribs, #ta_Bright  wz 'Test for Bright attribute
              if_nz     or      DrawColor, t1               'If bright on boost color level
                        'apply invert attribute
                        test    TextAttribs, #ta_Invert  wz 'Test for inverse attribute
              if_nz     ror     DrawColor, #8               'Just flip font and background colors
                        'make sure color is safe
                        and     DrawColor, AllColorMsk      'all done ready to return
ApplyAttribs_ret        ret
'
'
'Execute an ANSI helper command, T1 = command and short params (P0, P1, P2)
'On exit T1 is not modified allowing for commands that repeat.
'The terminal default colors are passed as the first long param
ExecHlpCmdWC            rdlong  t4, par                 'Firs param of reset values is reset colors
                        wrlong  t4, CommandDataPtr
'Execute an ANSI helper command, T1 = command and short params (P0, P1, P2)
ExecHlpCmd              wrlong  t1, CommandPtr          'Pass command, then wait for responce
                        'wait for command to complete
ExHlpCmdWait            rdlong  t4, CommandPtr          'Read command address
                        tjnz    t4, #ExHlpCmdWait
ExecHlpCmd_ret               
ExecHlpCmdWC_ret        ret
'
'
'Execute a Communications helper command, T1 = command and short params (P0, P1, P2)
ExecComCmd              wrlong  t1, ComCommandPtr       'Pass command, then wait for responce
                        'wait for command to complete
ExComCmdWait            rdlong  t4, ComCommandPtr       'Read command address
                        tjnz    t4, #ExComCmdWait
ExecComCmd_ret          ret               
'
'                                       
'Scroll screen up one row
scrollscreen            mov     t1, #c_ScrollUp
                        call    #ExecHlpCmdWC
scrollscreen_ret        ret       
'
'
'Move the cursor to the column T1 of the current row
GotoX                   rdbyte  t2, CursorYAdr          'Read current row, cursor Y value
                        jmp     #DoGotoX
'                       
'
'Clear the screen, then home cursor
                        
clearscreen            mov     t1, #c_FillScreen
                       call    #ExecHlpCmdWC
'
'
'Home the cursor and set write pointers to start of screen
homecursor              mov     t2, #0
                        mov     t1, #0
DoGotoX                 call    #gotoxy
GotoX_ret               
homecursor_ret          
clearscreen_ret         ret
'
'
'Position the cursor to the specified X, Y cordinates, X and Y are 0 based numbers
'T1 has X value and T2 has Y value on entry.
                        'first make sure X, Y are valid
gotoxy                  cmp     t1, #cols       wc
              if_nc     jmp     #gotoxy_ret
                        cmp     t2, #rows       wc
              if_nc     jmp     #gotoxy_ret
                        'X and Y are in range set cursor
                        wrbyte  t1, CursorXAdr
                        wrbyte  t2, CursorYAdr
                        'Update character and color write pointers
                        'Calc column offset in column start address table
                        mov     t3, t2                  'T3 = current row
                        shl     t3, #1                  'T3 = current row X 2 (offset table in words)
                        add     t3, ColOffsetAdr        'Calc byte address of column in word offset table
                        rdword  TxtPtr, t3              'read 16 bit column offset
                        add     TxtPtr, t1              'add on column offset
                        'now for the color table pointer
                        mov     ColPtr, TxtPtr          'Color offset = text screen offset * 2
                        shl     ColPtr, #1              '2 bytes per color table entry
                        'Add on the global memory offsets
                        add     TxtPtr, ScreenAdr       'add start of screen to col offset
                        add     Colptr, ColorAdr        'Add start of color tabe to offset
gotoxy_ret              ret
'                       
'
'Decode the final escape control sequence, and perform action if required
DecodeCSISeq            'We have a complex Esc[X;Ya type command
                        mov     RepeatCnt, Params  wz   'Read first paramater, commands that repeat use as repeat count
              if_z      mov     RepeatCnt, #1           'All repeat commands repeat at least once
                        'parse an ESC [ command
                        'See if command table has an address for Escape code
                        mov     t3, EscCodeChar
                        sub     t3, #"@"                'Escape codes are "@".."Z" or "a".."z", make "@" = zero index
                        cmp     t3, #c_CSIJumpTableSize  wc    'Make sure we have an offset of 0..63
        if_nc           jmp     #DecodeCSISubDone
                        'we have a valid CSI jump table index, see if we have a private CSI command
                        cmp     EscState, #3      wz    'Check for private ANSI code (first paramater is '?')
        if_z            sub     t3, #c_CSIJumpTableSize / 2  wc
        if_c_and_z      jmp     #DecodeCSISubDone       'private codes start at "`,a,b..."
        if_z            add     t3, #c_CSIJumpTableSize 'Private codes use second half of table
                        shl     t3, #1                  'Address table is in words
                        add     t3, CSICodesAdr         'Add start address of escape jump table
                        rdword  t3, t3                  'read the adddress to jump to
                        tjnz    t3, t3                  'if the address is not 0, the jump to address
DecodeCSISubDone        
DecodeCSISeq_ret        ret
'
'
'This is a code snippet to reduce code size.
'The command in T1 is executed once RepeatCnt. 
DoHlpCmdWCDoneCSI       mov      RepeatCnt, #1 
'This is a code snippet used to reduce program size.
'The command in T1 is repeated RepeatCnt times. 
DoHlpCmdWCRptDoneCSI    call    #ExecHlpCmdWC
                        djnz    RepeatCnt, #DoHlpCmdWCRptDoneCSI  'Repeat if required
                        jmp     #DecodeCSISubDone      'Done escape command
'
'                        
'These are the action function for the escape sequebce decoder
'All of these function must end with a jump to DecodeEscSubDone
'
'Scroll selecetd rows up, 1st paramater is number of lines to scroll up
DecodeEsc_S             mov     t1, #c_ScrollUp
                        jmp     #DoHlpCmdWCRptDoneCSI   'Perform command RepeatCnt time, jump to dome CSI
'
'
'Scroll selecetd rows down, 1st paramater is number of lines to scroll down
DecodeEsc_T             mov     t1, #c_ScrollDown
                        jmp     #DoHlpCmdWCRptDoneCSI   'Perform command RepeatCnt time, jump to dome CSI
'                            
'START OF COMPLEX ESC CODE COMMANDS
'
'
'Support function to save code space
'
'Move cursor to colum index T1 on the current row.
'Cleanup and return to CSI parser.
DoGoX_CSI_Done          rdbyte  t2, CursorYAdr          'Read current cursor row Y value
'
'
'Move cursor to colum index T1 on the row index T2.
'Cleanup and return to CSI parser.
DoGoXY_CSI_Done         call    #GotoXY                 'Set new cursor position
                        jmp     #DecodeCSISubDone      'Done escape command
'
'
'Move the cursor up one or more lines (no scroll), move to first column
DecodeEsc_F             mov     t1, #0                  'Set desired column to first column.
                        jmp     #DecodeEsc_F_Cnt        'shift cursor up RepeatCnt times
'
'                        
'Move the cursor up one or more lines (no scroll), column stays the same
DecodeEsc_A             rdbyte  t1, CursorXAdr
DecodeEsc_F_Cnt         rdbyte  t2, CursorYAdr          'Read current cursor Y value
                        sub     t2, RepeatCnt   wc      'calc new row address
              if_c      mov     t2, #0                  'if RepeatCnt > row then row = 0
                        jmp     #DoGoXY_CSI_Done        'Set curor then we are done escape command
'
'
'Move the cursor down one or more lines (no scroll), move to first column
DecodeEsc_E             mov     t1, #0                  'Set desired column to first column.
                        jmp     #DecodeEsc_E_Cnt        'shift cursor down RepeatCnt times
'
'                        
'Move the cursor down one or more lines (no scroll), column stays the same
DecodeEsc_B             rdbyte  t1, CursorXAdr
DecodeEsc_E_Cnt         rdbyte  t2, CursorYAdr          'Read current cursor Y value
                        add     t2, RepeatCnt           'calc new row address

                        max     t2, #rows-1
                        {
                        cmp     t2, #rows       wc      'see if we went past last row
              if_nc     mov     t2, #rows-1             'if RepeatCnt + row > rows then row = 0
                        }
                        jmp     #DoGoXY_CSI_Done        'Set curor then we are done escape command
'
'
'Move the cursor right one or more columns (no scroll, no wrap), row stays the same
DecodeEsc_C             rdbyte  t1, CursorXAdr          'Read current cursor X value
                        add     t1, RepeatCnt           'Calc final column

                        max     t1, #cols-1
                        {
                        cmp     t1, #cols       wc      'See if past end of current line
              if_nc     mov     t1, #cols -1            'Nothing to do so return
                        }
                        jmp     #DoGoX_CSI_Done         'Set curor then we are done escape command
'
'
'Move the cursor left one or more columns (no scroll, no wrap), row stays the same
DecodeEsc_D             rdbyte  t1, CursorXAdr          'Read current cursor X value
                        sub     t1, RepeatCnt   wc      'Point to final prior column
              if_c      mov     t1, #0                  'Past first colum so goto first column
                        jmp     #DoGoX_CSI_Done         'Set curor then we are done escape command
'
'
'Set cursor X to paramater 0
DecodeEsc_G             mov     t1, RepeatCnt          'T1 = requested column number
                        sub     t1, #1          wc     'T1 = requested column index
              if_nc     call    #gotox
                        jmp     #DecodeCSISubDone      'Done escape command
'
'
'Repeat the last displayed character a given number of times
DecodeEsc_L_b           mov     RptCharCnt, RepeatCnt
                        jmp     #DecodeCSISubDone      'Done escape command
'
'                        
'Move cursor to specified row and column, or home if no params
'Param[0] = row, Param[1] = column, these are 1 based (1..Rows, 1..Cols)
DecodeEsc_f_H           mov     t1, #1                  'Home current cursor column param value
                        mov     t2, #1                  'Home current cursor row param value
                        cmp     ParamCnt, #1    wc      'See if first paramater (row) supplied
              if_nc     mov     t2, Params              'T2 = new row from first paramater
                        sub     t2, #1                  'make it zero based
                        cmp     ParamCnt, #2    wc      'Home command has none paramaters
              if_nc     mov     t1, Params+1            'T1 = new column from second paramater
                        sub     t1, #1                  'make it zero based
                        jmp     #DoGoXY_CSI_Done        'Set cursor then we are done escape command
'
'
'Clear The screen
DecodeEsc_J             cmp     Params, #2      wz, wc
        if_nc_and_nz    jmp     #DecodeCSISubDone
        if_z            call    #homecursor                   'Param = 2, home cursor
                        mov     t1, #c_FillFromRow            'First command, if param = 0 - clear row to end, 1 - clear start to row, 2 - clear screen 
DecodeEscJKSet          add     t1, Params                    'Add command offset to select correct command                                     
                        jmp     #DoHlpCmdWCDoneCSI            'Perform command in T1, jump to dome CSI Done escape command
'
'
'Erase line, commands cursor stays put
DecodeEsc_K             cmp     Params, #2      wz, wc
        if_nc_and_nz    jmp     #DecodeCSISubDone
                        mov     t1, #c_EraseEol               'Param = 0, erase from current col to end of line
                        jmp     #DecodeEscJKSet               'Param =1 erase from line start to cursor, 2 = erase line
'
'Delete one or more lines command             
DecodeEsc_M             mov     t1, #c_DeleteLine
                        jmp     #DoHlpCmdWCRptDoneCSI   'Perform command RepeatCnt time, jump to dome CSI
'
'
'Insert one or more lines command
'Insert blank row at the current row, cursor does not move.
DecodeEsc_L             mov     t1, #c_InsertLine
                        jmp     #DoHlpCmdWCRptDoneCSI   'Perform command RepeatCnt time, jump to dome CSI
'
'
'Set font glyph, First param has font index remaining 12 params
'contain font glyph patters. The left "{" bracket.
DecodeEsc_LBrace        mov     t3, ComCommandDataPtr
                        call    #CopyParamsFnc
                        mov     t1, #c_ComUpdateFont
                        call    #ExecComCmd
                        jmp     #DecodeCSISubDone      'Done escape command
'
'                        
'Save the cursor X, Y
DecodeEsc_L_s           rdbyte  SaveCursorY, CursorYAdr 'Save current cursor Y value
                        rdbyte  SaveCursorX, CursorXAdr 'Save current cursor X value
                        jmp     #DecodeCSISubDone      'Done escape command
'
'
'Restore last saved cursor position
DecodeEsc_L_u           mov     t1, SaveCursorX
                        mov     t2, SaveCursorY
                        jmp     #DoGoXY_CSI_Done        'Set curor then we are done escape command
'
'
'Set scroll up/down commands first and last row
DecodeEsc_L_r           mov     t1, Params              'Read first Paramater, 1 based index
                        sub     t1, #1                  'Make number zero based index
                        mov     t2, Params + 1          'Read second paramater
                        sub     t2, #1                  'Make number zero based
                        cmp     ParamCnt, #2    wc      'See if we have a least 2 params
                        'if we do not have 2 params set to full screen
          if_c          mov     t1, #0        
          if_c          mov     t2, #rows-1
                        'Range check our values
                        cmp     t2, t1          wc      'First must be =< Last row
          if_c          jmp     #DecodeCSISubDone
                        cmp     t2, #rows       wc      'Last must be less then screen rows    
          if_nc         jmp     #DecodeCSISubDone
                        'Now save scroll region
                        call    #ApplyScrollRegion
                        jmp     #DecodeCSISubDone      'Done escape command
'
'           
'Report current cursor position
DecodeEsc_L_n           cmp     Params, #6      wz
              if_nz     jmp     #DecodeCSISubDone
                        wrlong  CursorXAdr, ComCommandDataPtr     'Save current cursor buffer address as param 0
                        mov     t1, #c_ComSendXY
                        call    #ExecComCmd
                        jmp     #DecodeCSISubDone      'Done escape command
'
'
'Copy Params to global memory. T3 = memory address.
'ParamCnt will contain 0 on exit.
'Caller must asure ParamCnt is < 0 < MaxParams
CopyParamsFnc           movd    CopyParamsWT, #Params   'Set memory copy opcode source address.
                        nop                             'Must have a least one opcode before using.
CopyParamsWT            wrlong  Params, T3              'Save parameter to global memory
                        add     CopyParamsWT, D0        'Next paramater to save
                        add     t3, #4
                        djnz    ParamCnt, #CopyParamsWT
CopyParamsFnc_ret       ret
'
'
'Set text attributes
DecodeEsc_L_m           cmp     ParamCnt, #0    wz
'              if_z      mov     Params, #0              'No params same as param = 0
              if_z      mov     ParamCnt, #1            'indicate we have one paramater
                        'Save variables and paramters to make function call
                        'First get the base color
                        mov     t3, CommandDataPtr      'T3 = address of Param[0] = BaseColor 
                        wrlong  BaseColor, t3
                        'now for the text attributes
                        add     t3, #4                  'T3 = address of Param[1] = TextAttribs
                        wrlong  TextAttribs, t3   
                        'set the paramater count
                        add     t3, #4                  'T3 = address of Param[2] = ParamCnt
                        wrlong  ParamCnt, t3
                        'Save address of first paramater
                        add     t3, #4                  'T3 = address of first paramater in memory
                        'copy all the paramters
                        call    #CopyParamsFnc
                        'Execute set attributes command
                        mov     t1, #c_SetAttribs
                        call    #ExecHlpCmd
                        'Command done read results
                        mov     t3, CommandDataPtr      'T3 = address of Param[0] = BaseColor
                        rdlong  BaseColor, t3
                        'now for the text attributes
                        add     t3, #4                  'T3 = address of Param[1] = TextAttribs
                        rdlong  TextAttribs, t3   
                        'update attributes and return
                        call    #ApplyAttribs
                        jmp     #DecodeCSISubDone
'
'
'Cursor show / Line wrap on commands
DecodeEsc_L_h           cmp     Params, #7        wz
              if_z      mov     LineWrapState, #c_LineWrapOn
                        'see if we want to display text cursor
                        cmp     Params, #25       wz
              if_z      wrlong  CursorOnPattern, CursorPatternAdr
                        jmp     #DecodeCSISubDone      'Done escape command

'
'
'Cursor hide / Line wrap off commands
DecodeEsc_L_l           cmp     Params, #7        wz
              if_z      mov     LineWrapState, #c_LineWrapOff
                        'see if we want to hide text cursor
                        mov     t1, #0
                        cmp     Params, #25       wz
              if_z      wrlong  t1, CursorPatternAdr
                        jmp     #DecodeCSISubDone      'Done escape command

'
'
DecodeEsc_L_g           mov     t1, #0
                        cmp     Params, #3          wz
              if_z      mov     t1, #c_ComClearTabs              
                        cmp     Params, #0          wz
              if_z      mov     t1, #c_ComClearTab
                        cmp     t1, #0              wz
              if_nz     call    #ExecComCmd                        
                        jmp     #DecodeCSISubDone      'Done escape command

'
'
'Save the terminal state to global memory
SaveState               rdbyte  SaveCursorY, CursorYAdr 'Save current column to local mem
                        rdbyte  SaveCursorX, CursorXAdr 'Save current row to local mem
                        'Now copy the "c_StateVarCnt" longs to the global buffer
                        mov     CopyStateNxt, SaveStateCmd
                        jmp     #CopyState
'
'Load terminal state from reset state global memory
ResetState              mov     CopyStateNxt, LoadStateCmd                      'We want a read from global mem command
                        mov     t1, par                                         'T1 = global mem reset state buffer address
                        jmp     #CopyStateRst
'                        
'Load terminal state from save state global memory
LoadState               mov     CopyStateNxt, LoadStateCmd
'
CopyState               mov     t1, SaveStateBufAdr                             'T1 = global mem buffer address
CopyStateRst            mov     t2, #c_StateVarCnt                              'T2 = number of longs to copy
CopyStateNxt            wrlong  BaseColor, t1                                   'Save the paramater
                        add     CopyStateNxt, D0                                'point to next variable to save
                        add     t1, #4                                          'point to next save long address 
                        djnz    t2, #CopyStateNxt                               'do next save if required
LoadState_ret                        
ResetState_ret                        
SaveState_ret           ret                        
'
'
'Set the scrool region variables and update the
'ANSI help cog with the new scroll region.
'On Entry T1 = first line, T2 = last line
ApplyScrollRegion       mov     ScrlFirstLine, t1
                        mov     ScrlLastLine, t2
                        'Let AnsiHelp object know new region settings
                        shl     t2, #16                 'Last line into P1 byte position
                        shl     t1, #8                  'First line into P0 byte position
                        or      t1, t2                  'Add in second paramater
                        'make the call
                        or      t1, #c_ScrollRegion     'Finally add in command code into LSB
                        call    #ExecHlpCmd
ApplyScrollRegion_ret   ret       
'
'
'Apply loaded state settings to active terminal.
                        'restore cursor position 
ApplyState              mov     t1, SaveCursorX
                        mov     t2, SaveCursory
                        call    #gotoxy
                        'restore scroll region
                        mov     t1, ScrlFirstLine
                        mov     t2, ScrlLastLine
                        call    #ApplyScrollRegion
                        'last call apply attributes
                        call    #ApplyAttribs
ApplyState_ret          ret
'
'
'Called by escape parser to save the state
DoSaveState             call    #SaveState
                        jmp     #DecodeCSISubDone      'Done escape command
'
'                                
'Called by escape parser to restore saved state
DoRestoreState          call    #LoadState
                        call    #ApplyState
                        jmp     #DecodeCSISubDone      'Done escape command
'
'
'(realy @) Insert n chars at the current cursor
DecodeEsc_AT            mov     t1, RepeatCnt
                        shl     t1, #8
                        or      t1, #c_InsertChars
                        jmp     #DoHlpCmdWCDoneCSI    'Perform command in T1, jump to dome CSI Done escape command
'
'
'Delete n chars at the current cursor
DecodeEsc_P             mov     t1, RepeatCnt
                        shl     t1, #8
                        or      t1, #c_DeleteChars
                        jmp     #DoHlpCmdWCDoneCSI    'Perform command in T1, jump to dome CSI Done escape command
'
'
'Goto next horz tab stop, Param[0] = number of tab stops
DecodeEsc_I             movs    DecodeEsc_IZ_Rpt, #c_ComNextTab
                        jmp     #DecodeEsc_IZ_Rpt
'
'
'Goto prior horz tab stop, Param[0] = number of tab stops
DecodeEsc_Z             movs    DecodeEsc_IZ_Rpt, #c_ComPriorTab
DecodeEsc_IZ_Rpt        mov     t1, #c_ComPriorTab
                        call    #ExecComCmd             'call the helper function
                        rdlong  t1, ComCommandDataPtr   'Read column returned, T1 = column
                        call    #GotoX                  'Set column on screen
                        djnz    RepeatCnt, #DecodeEsc_IZ_Rpt
                        jmp     #DecodeCSISubDone      'Done escape command
'
'
'Erase the next n characters on the row, no line wrap, Cursor does not move
DecodeEsc_X             mov     t1, RepeatCnt         'Encode number of chars to erase 
                        shl     t1, #8                'put into param[0] position
                        or      t1, #c_EraseChars     'Add in command
                        jmp     #DoHlpCmdWCDoneCSI    'Perform command in T1, jump to dome CSI Done escape command
'
'
'Move the cursor to a specified row, column does not change
DecodeEsc_L_d           sub     RepeatCnt, #1           'Change from 1 based to zero based
                        rdbyte  t1, CursorXAdr
                        mov     t2, RepeatCnt
                        jmp     #DoGoXY_CSI_Done        'Set curor then we are done escape command
'
'
'These variables are past to the cog at startup
'
SaveStateCmd            wrlong  BaseColor, t1
LoadStateCmd            rdlong  BaseColor, t1
'
D0                      long    1 << 9
ScreenAdr               long    0
ColorAdr                long    0
CursorXAdr              long    0
CursorYAdr              long    0
ColOffsetAdr            long    0
'BaseColorPtr           long    0
CommandPtr              long    0
CommandDataPtr          long    0
ComCommandPtr           long    0
ComCommandDataPtr       long    0
CSICodesAdr             long    0
EscCodesAdr             long    0
Ctrl7CodesAdr           long    0
Ctrl8CodesAdr           long    0
SaveStateBufAdr         long    0
'
CharFill                long    $20202020
FontColorMsk            long    tc_FontColorMsk
BgColorMsk              long    tc_BgColorMsk
AllColorMsk             long    tc_ColorMsk
FontColorHiBitMsk       long    tc_FontHiBitMsk
'Variables for repeat char control sequence
RptCharLast             long    $20
RptCharCnt              long    0
'
'Cousor display constatns and pointers
CursorPatternAdr        long    0
CursorOnPattern         long    cursor_on_pattern

'These vars are used to hold the terminal state.
'They are also saved by ESC-7 and restored by ESC-8 escape codes.
'BaseColor must be the first variable
BaseColor               res     1
TextAttribs             res     1
ScrlFirstLine           res     1
ScrlLastLine            res     1
LineWrapState           res     1
SaveCursorY             res     1
SaveCursorX             res     1
'
'End of cog paramaters
'
' Uninitialized data
'
t1                      res     1
t2                      res     1
t3                      res     1
t4                      res     1
'Screen variables initialized during reset
TxtPtr                  res     1
ColPtr                  res     1
DrawColor               res     1
'Escape parser variables
'Clear params function sets all to 0.
'Do not change the order.
EscState                res     1
ParamCharIdx            res     1
ParamCnt                res     1
TempParam               res     1
Params                  res     MaxParams
'General variables
RepeatCnt               res     1
EscCodeChar             res     1
'
' ******** END OF COG CODE ********
'
' Various lookup tables in global memory
'
'Color table hold 16 default colors
aBaseColors             long
long                    tc_Black
long                    tc_Red
long                    tc_Green
long                    tc_Yellow
long                    tc_Blue
long                    tc_Magenta
long                    tc_Cyan
long                    tc_White
long                    tc_Black >> 1
long                    tc_Red >> 1
long                    tc_Green >> 1
long                    tc_Yellow >> 1
long                    tc_Blue >> 1
long                    tc_Magenta >> 1
long                    tc_Cyan >> 1
long                    tc_White >> 1
'
'
'Escape control sequence parser addresses, all escape sequences end with '@'..'Z' or 'a'..'{'
'We will allocate 64 words to hold the address of the escape code jump address
'If the escape code is 'A' EscJumpTable[1] = address in cog ram to jump to parse
'Each entry contain a word address to the function to call for
'each CSI code character. If table entry is 0, code is not supported.
'c_CSIJumpTableSize * 2 entries, 64 for CSI and 64 for CSI ? codes.
aCSIJumpTable           word                    
        'CSI jump table from "@,A,B...Y,[\]^_"
word    (@DecodeEsc_AT - @entry) >> 2           '@ = 0
word    (@DecodeEsc_A - @entry) >> 2            'A = 1
word    (@DecodeEsc_B - @entry) >> 2            'B = 2
word    (@DecodeEsc_C - @entry) >> 2            'C = 3
word    (@DecodeEsc_D - @entry) >> 2            'D = 4
word    (@DecodeEsc_E - @entry) >> 2            'E = 5
word    (@DecodeEsc_F - @entry) >> 2            'F = 6
word    (@DecodeEsc_G - @entry) >> 2            'G = 7
word    (@DecodeEsc_f_H - @entry) >> 2          'H = 8
word    (@DecodeEsc_I - @entry) >> 2            'I = 9
word    (@DecodeEsc_J - @entry) >> 2            'J = 10
word    (@DecodeEsc_K - @entry) >> 2            'K = 11
word    (@DecodeEsc_L - @entry) >> 2            'L = 12
word    (@DecodeEsc_M - @entry) >> 2            'M = 13
word    0                                       'N = 14
word    0                                       'O = 15
word    (@DecodeEsc_P - @entry) >> 2            'P = 16
word    0                                       'Q = 17
word    0                                       'R = 18
word    (@DecodeEsc_S - @entry) >> 2            'S = 19
word    (@DecodeEsc_T - @entry) >> 2            'T = 20
word    0                                       'U = 21
word    0                                       'V = 22
word    0                                       'W = 23
word    (@DecodeEsc_X - @entry) >> 2            'X = 24
word    0                                       'Y = 25
word    (@DecodeEsc_Z - @entry) >> 2            'Z = 26
word    0                                       '[ = 27
word    0                                       '\ = 28
word    0                                       '] = 29
word    0                                       '^ = 30
word    0                                       '_ = 31
        'CSI jump table from "`,a,b...y,{|}~"
word    0                                       '` = 32
word    0                                       'a = 33
word    (@DecodeEsc_L_b - @entry) >> 2          'b = 34
word    0                                       'c = 35
word    (@DecodeEsc_L_d - @entry) >> 2          'd = 36
word    0                                       'e = 37
word    (@DecodeEsc_f_H - @entry) >> 2          'f = 38
word    (@DecodeEsc_L_g - @entry) >> 2          'g = 39
word    0                                       'h = 40
word    0                                       'i = 41
word    0                                       'j = 42
word    0                                       'k = 43
word    0                                       'l = 44
word    (@DecodeEsc_L_m - @entry) >> 2          'm = 45
word    (@DecodeEsc_L_n - @entry) >> 2          'n = 46
word    0                                       'o = 47
word    0                                       'p = 48
word    0                                       'q = 49
word    (@DecodeEsc_L_r - @entry) >> 2          'r = 50
word    (@DecodeEsc_L_s - @entry) >> 2          's = 51
word    0                                       't = 52
word    (@DecodeEsc_L_u - @entry) >> 2          'u = 53
word    0                                       'v = 54
word    0                                       'w = 55
word    0                                       'x = 56
word    0                                       'y = 57
word    0                                       'z = 58
word    0                                       '{ = 59
word    0                                       '| = 60
word    0                                       '} = 61
word    0                                       '~ = 62
word    0                                       '- = 63
'
'
{ NO CODES SUPPORTED FOR FIRST 32 Commands
        'CSI ? jump table from "@,A,B...Y,[\]^_"
word    0                                       '@ = 0
word    0                                       'A = 1
word    0                                       'B = 2
word    0                                       'C = 3
word    0                                       'D = 4
word    0                                       'E = 5
word    0                                       'F = 6
word    0                                       'G = 7
word    0                                       'H = 8
word    0                                       'I = 9
word    0                                       'J = 10
word    0                                       'K = 11
word    0                                       'L = 12
word    0                                       'M = 13
word    0                                       'N = 14
word    0                                       'O = 15
word    0                                       'P = 16
word    0                                       'Q = 17
word    0                                       'R = 18
word    0                                       'S = 19
word    0                                       'T = 20
word    0                                       'U = 21
word    0                                       'V = 22
word    0                                       'W = 23
word    0                                       'X = 24
word    0                                       'Y = 25
word    0                                       'Z = 26
word    0                                       '[ = 27
word    0                                       '\ = 28
word    0                                       '] = 29
word    0                                       '^ = 30
word    0                                       '_ = 31
}
        'CSI ? jump table from "`,a,b...y,{|}~"
word    0                                       '` = 32
word    0                                       'a = 33
word    0                                       'b = 34
word    0                                       'c = 35
word    0                                       'd = 36
word    0                                       'e = 37
word    0                                       'f = 38
word    0                                       'g = 39
word    (@DecodeEsc_L_h - @entry) >> 2          'h = 40
word    0                                       'i = 41
word    0                                       'j = 42
word    0                                       'k = 43
word    (@DecodeEsc_L_l - @entry) >> 2          'l = 44
word    0                                       'm = 45
word    0                                       'n = 46
word    0                                       'o = 47
word    0                                       'p = 48
word    0                                       'q = 49
word    0                                       'r = 50
word    0                                       's = 51
word    0                                       't = 52
word    0                                       'u = 53
word    0                                       'v = 54
word    0                                       'w = 55
word    0                                       'x = 56
word    0                                       'y = 57
word    0                                       'z = 58
word    (@DecodeEsc_LBrace - @entry) >> 2       '{ = 59
word    0                                       '| = 60
word    0                                       '} = 61
word    0                                       '~ = 62
word    0                                       '- = 63
'
'
'Jump table for single character 7 bit control codes
'7 bit code are at index 0..31.
'8 bit codes are at index 32..63 and are stored in the
'single character escape code jump table
aCtrl7JumpTable          word
        'First 32 entries for 7 bit control codes (0..31)
word    0                                       '@ = 0
word    0                                       'A = 1
word    0                                       'B = 2
word    0                                       'C = 3
word    0                                       'D = 4
word    (@DoENQCntrl - @entry) >> 2             'E = 5
word    0                                       'F = 6
word    0                                       'G = 7
word    (@DoBackSpace - @entry) >> 2            'H = 8
word    (@DoNextHorzTab - @entry) >> 2          'I = 9
word    (@DoLineFeed - @entry) >> 2             'J = 10
word    (@DoLineFeed - @entry) >> 2             'K = 11
word    (@DoLineFeed - @entry) >> 2             'L = 12
aCrJumpTableEntry    word
word    (@DoCarRet - @entry) >> 2               'M = 13
word    0                                       'N = 14
word    0                                       'O = 15
word    0                                       'P = 16
word    0                                       'Q = 17
word    0                                       'R = 18
word    0                                       'S = 19
word    0                                       'T = 20
word    0                                       'U = 21
word    0                                       'V = 22
word    0                                       'W = 23
word    0                                       'X = 24
word    0                                       'Y = 25
word    0                                       'Z = 26
aCtrlJumpESC          word
word    (@DoStartEscape1 - @entry) >> 2         '[ = 27
word    0                                       '\ = 28
word    0                                       '] = 29
word    0                                       '^ = 30
word    0                                       '_ = 31
'
'
{
        'Next 32 entries for 8 bit control codes (128..159)
        'Add 32 for 8 bit code (-128, +32)
word    0                                       '@ = 128
word    0                                       'A = 129
word    0                                       'B = 130
word    0                                       'C = 131
word    (@DoLineFeed - @entry) >> 2             'D = 132  IND index, move to next row, scroll if required, column same, "ESC D"
word    (@DoCrLf - @entry) >> 2                 'E = 133  NEL new line, move to next row, scroll if required, move to first column, "ESC E"
word    0                                       'F = 134
word    0                                       'G = 135
word    (@EscSetTab - @entry) >> 2              'H = 136  HTS set horizontal tab a current column, "ESC H"
word    0                                       'I = 137
word    0                                       'J = 138
word    0                                       'K = 139
word    0                                       'L = 140
aCtrlJumpCSI          word
word    (@DoRevLineFeed - @entry) >> 2          'M = 141
word    0                                       'N = 142  RIN reverse index, move to prior row, scroll if required, column same, "ESC M"
word    0                                       'O = 143
word    0                                       'P = 144
word    0                                       'Q = 145
word    0                                       'R = 146
word    0                                       'S = 147
word    0                                       'T = 148
word    0                                       'U = 149
word    0                                       'V = 150
word    0                                       'W = 151
word    0                                       'X = 152
word    0                                       'Y = 153
word    0                                       'Z = 154
word    (@DoStartEscape2 - @entry) >> 2         '[ = 155  CSI constrol sequence initiator, start an escape control sequence, "ESC [
word    0                                       '\ = 156
word    0                                       '] = 157
word    0                                       '^ = 158
word    0                                       '_ = 159
}
'
'
'Esccape code jump table, must contain c_ESCCodeJumpTableSize entries.
aEscJumpTable         word
'0  = 0
word          0
'1  = 1
word          0
'2  = 2
word          0
'3  = 3
word          0
'4  = 4
word          0
'5  = 5
word          0
'6  = 6
word          0
'7  = 7
word          (@DoSaveState - @entry) >> 2
'8  = 8
word          (@DoRestoreState - @entry) >> 2
'9  = 9
word          0
':  = 10
word          0
';  = 11
word          0
'<  = 12
word          0
'=  = 13
word          0
'>  = 14
word          0
'?  = 15
word          0
aCtrl8JumpTable  word   '(ESC @ ... ESC _ same as 8 bit control codes)
'@  = 16
word          0
'A  = 17
word          0
'B  = 18
word          0
'C  = 19
word          0
'D  = 20
word          (@DoLineFeed - @entry) >> 2          'IND index, move to next row, scroll if required, column same, "ESC D"
'E  = 21
word          (@DoCrLf - @entry) >> 2              'NEL new line, move to next row, scroll if required, move to first column, "ESC E"
'F  = 22
word          0
'G  = 23
word          0
'H  = 24
word          (@EscSetTab - @entry) >> 2           'HTS set horizontal tab a current column, "ESC H"
'I  = 25
word          0
'J  = 26
word          0
'K  = 27
word          0
'L  = 28
word          0
'M  = 29
word          (@DoRevLineFeed - @entry) >> 2      'RIN reverse index, move to prior row, scroll if required, column same, "ESC M"
'N  = 30
word          0
'O  = 31
word          0
'P  = 32
word          0
'Q  = 33
word          0
'R  = 34
word          0
'S  = 35
word          0
'T  = 36
word          0
'U  = 37
word          0
'V  = 38
word          0
'W  = 39
word          0
'X  = 40
word          0
'Y  = 41
word          0
'Z  = 42
word          0
aCtrlJumpCSI          word
'[  = 43
word          (@DoStartEscape2 - @entry) >> 2     'CSI constrol sequence initiator, start an escape control sequence, "ESC ["
'\  = 44
word          0
']  = 45
word          0
'^  = 46
word          0
'_  = 47
word          0
'`  = 48
word          0
'a  = 49
word          0
'b  = 50
word          0
'c  = 51
word          (@reset_term - @entry) >> 2
{
'd  = 52
word          0
'e  = 53
word          0
'f  = 54
word          0
'g  = 55
word          0
'h  = 56
word          0
'i  = 57
word          0
'j  = 58
word          0
'k  = 59
word          0
'l  = 60
word          0
'm  = 61
word          0
'n  = 62
word          0
'o  = 63
word          0
'p  = 64
word          0
'q  = 65
word          0
'r  = 66
word          0
's  = 67
word          0
't  = 68
word          0
'u  = 69
word          0
'v  = 70
word          0
'w  = 71
word          0
'x  = 72
word          0
'y  = 73
word          0
'z  = 74
word          0
'{  = 75
word          0
'|  = 76
word          0
'}  = 77
word          0
'~  = 78
word          0
'-  = 79
word          0
}

        