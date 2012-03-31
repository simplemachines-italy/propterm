''****************************************
''*  PropTerm Ansi Terminal Helper v1.0  *
''****************************************
''
''This object implements support code for the "AnsiTerminal" object.
''The main reason for this object is to allow the AnsiTerminal object
''to support a larger ANSI command set.
''
''The long CmdAdr is used to pass the address of the long command
''buffer in global memory. The "AnsiTerminal" object writes command
''codes to the command buffer in global memory. This object watches
''the command buffer for a valid command and dispatches the command
''to the correct function using the jump table at the end of the
''object code.
'' 
''Byte[0] of the command buffer is the command index. Bytes[1..3]
''contain any required byte sized parameters.
''The address 4 bytes from the command buffer (next long)
''contains the first long parameter. Once a command is completed
''by this object the command buffer is set to zero.
''
CON
  'ANSI helper command codes and parsing constants,
  'if command order changes jump table must be changed to match.
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
  c_FillToRow =   9             'Clear screen from start to row, P0 = current row. DrawColor must be supplied in LongParam.
  c_FillScreen  = 10            'no params, DrawColor must be supplied.
  'Next three clear row commnads must stay in order
  c_EraseEol    = 11            'Clear row specified in P0 from col in P1 to row end. DrawColor must be supplied in LongParam.
  c_EraseBol    = 12            'Clear row specified in P0 from start to row in P1. DrawColor must be supplied in LongParam.
  c_EraseLine   = 13            'Clear row specified in P0. DrawColor must be supplied in LongParam.
  'End of commands that must stay in order
  c_InsertChars = 14            'Insert n blank characters at the current colum and row. DrawColor must be supplied in LongParam. Param[0] = number of chars
  c_DeleteChars = 15            'Delete n characters at the current colum and row. DrawColor must be supplied in LongParam. Param[0] = number of chars
  c_EraseChars  = 16            'Erase n characters at the current colum and row. DrawColor must be supplied in LongParam. Param[0] = number of chars
  'End of ANSI helper codes
  c_CmdMask     = 31
  c_MaxCommands = 17
  'Text attributes
  ta_Dim =      1               'Dim text color cancels bright
  ta_Bright =   2               'Brighten text color, cancels Dim
  ta_Invert =   4               'Invert text/background color
  ta_AltFont =  8               'Select alternate font set
  c_MaxAttribValue = 67         'Highest possible non 64bit color attrib value
  'Color masks
  tc_FontColorMsk = $FC00FC00   'used to mask off font color
  tc_BgColorMsk   = $00FC00FC   'used to mask off background color
  tc_ColorMsk     = $FCFCFCFC   'used to make sure colors have no sync bits set
                                                                                                                             
VAR
  'cog for terminal helper
  long  cog                     'cog flag/id

  
PUB start(CmdAdr, ScrAdr, ScrColAdr, ColMultAdr, ColTableAdr, CursorAdr, ResetParamsAdr, cols, rows) : okay | i
'Initialize the column offsets table
  CursorXAdr := CursorAdr
  CursorYAdr := CursorAdr + 1
  ScreenAdr:= ScrAdr
  ColorAdr:= ScrColAdr
  ColOffsetAdr:= ColMultAdr
  BaseColorPtr:= ColTableAdr
'  TabStopsAdr := ColTabStopsAdr
  RstParamsAdr:= ResetParamsAdr
  ScrnCols:=cols
  ScrnRows:=rows
  'Scroll region boundires
  ScrollFirstRow:= 0
  ScrollLastRow:=rows - 1
  '
  'Initialize attribute jump table
  AttribJmpPtr := @aAttribJmpTable
  '
  'Initalize command parser jump table
  CmdParseAdr:=@aCmdJmpTable
  '
  'Start our cog if we can
  okay := cog := cognew(@entry, CmdAdr) + 1
  
PUB stop
' Stop terminal helper - frees a cog
  if cog
    cogstop(cog~ - 1)

DAT

'***********************************
'* Assembly language ansii helper  *
'***********************************

                        org   0
'
'
' Entry
'
entry                   mov     CmdParamAdr, par
                        add     CmdParamAdr, #4
nextcmd                 mov     t1, #c_NoCommand        'No command constant
                        wrlong  t1, par                 'write command to indicate we are done
                        'wait for a command
waitcmd                 rdlong  t1, par                 'read command
                        tjz     t1, #waitcmd            'if <> 0 we have a valid command, if no command wait
                        'Read the current draw color, if passed
                        rdlong  LongParam, CmdParamAdr
                        'Set T2 = current command ID
                        mov     t2, t1                  'Save coomand and params to T2
                        and     t2, #c_CmdMask          'T2 = command byte all params = 0
                        cmp     t2, #c_MaxCommands       wc
              if_nc     jmp     #nextcmd                 'Command id is out of range, wait for next cmd       
                        'We have a command see if it is valid
                        mov     t3, t2                  'T3 = command index
                        shl     t3, #1                  'Table is word based so X 2
                        add     t3, CmdParseAdr         'Add offset to table start in main memory
                        rdword  t3, t3                  'read the address of the command code
                        tjnz    t3, t3                  'if address <> 0 then jump to address
                         'if no command wait
                        jmp     #nextcmd
'
'                       
'These are the worker functions
'
'Set the scroll region for the scroll up/down commands
DoSetScrlRegion         shr     t1, #8                  'Shift P0 into LSB of T1
                        mov     t2, t1                  
                        shr     t2, #8                  'Shift P1 into LSB of T2
                        and     t1, #255                'Mask unwanted bits from T1
                        and     t2, #255                'Mask unwanted bits from T2
                        'We have our two paramaters, save them and we are done
                        mov     ScrollFirstRow, T1
                        mov     ScrollLastRow, T2
                        jmp     #nextcmd
'
'
'Fill the screen with the specified Fg and Bg color in LongParam
varClrFirstRow          long    0
varClrLastRow           long    0
'
DoFillToRow             rdbyte  t1, CursorYAdr          'T1 = current row
                        mov     varClrFirstRow, #0      'Start at top of screen
                        mov     varClrLastRow, t1       'Stop on current row
                        'Erase the start of the current row to the current column
                        rdbyte  t2, CursorXAdr          'T2 = current column
                        call    #EraseFromBol           
                        sub     varClrLastRow, #1       wc
                        'If we have more lines call clrscr  
              if_c      jmp      #nextcmd                'wait for next command                            
                        jmp     #clrscr
'                        
'
DoFillFromRow           rdbyte  t1, CursorYAdr          'T1 = current row
                        mov     varClrFirstRow, t1      'Start at current row
                        mov     varClrLastRow, ScrnRows 'Stop at end of screen
                        sub     varClrLastRow, #1       '
                        'Erase the end of the current row from the current column
                        rdbyte  t2, CursorXAdr          'T2 = current column
                        call    #EraseToEol
                        add     varClrFirstRow, #1
                        'If we have more lines call clrscr  
                        cmp     varClrFirstRow, ScrnRows  wc
              if_nc     jmp     #nextcmd                'wait for next command                            
                        jmp     #clrscr
'
'+                       
DoFillScreen            mov     varClrFirstRow, #0      'Start at top of screen
                        mov     varClrLastRow, ScrnRows 'Stop at end of screen
                        sub     varClrLastRow, #1       '
                        jmp     #clrscr
'                       
                        'Clear all the rows required
clrscr                  mov     t1, varClrFirstRow
                        call    #ClearRow
                        cmp     varClrFirstRow, varClrLastRow   wz
        if_z            jmp     #nextcmd                'wait for next command
                        add     varClrFirstRow, #1
                        jmp     #clrscr
'
'
'Clear row subrotine, fill specified row with DrawColor in LongParam and CharFill
'T1 = row number to clear, 0 based index
ClearRow                mov     t2, #0                  'start at first column, T1 = row index
                        call    #EraseToEol
ClearRow_ret            ret
'
'
'Calculate Start address for row in T1 return value in T1
CalcRowAdrT1            shl     t1, #1                          'T1 = row X 2 (offset table in words)
                        add     t1, ColOffsetAdr                'T1 = Address of (region rows-1) in row mult col table
                        rdword  t1, t1                          'T1 = character offset to start of row in bytes
CalcRowAdrT1_ret        ret              
'
'
'Erase to end of line, cursor stays put.
'T1 = row number, T2 = col number, both 0 based indexes
EraseToEol              cmp     t1, ScrnRows           wc, wz
        if_nc_or_z      jmp     #EraseToEolDone
                        cmp     t2, ScrnCols           wc, wz
        if_nc_or_z      jmp     #EraseToEolDone
                        'Calculate offset in screen memory to current row, and column
                        call    #CalcRowAdrT1
                        add     t1, t2                          'T1 = offset to first screen character to clear
                        'Calculate number of characters to clear
                        mov     t3, ScrnCols                    'T3 = number of columns per row
                        sub     t3, t2                          'Subtract to get number of chars to clear
                        'Calculate screen memory address of first char to clear
                        mov     t2, t1
                        add     t2, ScreenAdr                   'T2 = first screen memory address to clear
                        'fill the character table with spaces
                        mov     t4, t3
EraseToEol1             wrbyte  CharFill, t2
                        add     t2, #1
                        djnz    t4, #EraseToEol1
                        'fill the color table with current DrawColor in LongParam
                        shl     t1, #1                           'Offset at twice character offset in memory
                        add     t1, ColorAdr
EraseToEol2             wrword  LongParam, t1
                        add     t1, #2
                        djnz    t3, #EraseToEol2
                        'all done return to caller
EraseToEolDone          
EraseToEol_ret          ret
'
'
'Erase from start of line to and including col number, cursor stays put.
'T1 = row number, T2 = col number, both 0 based indexes
EraseFromBol            cmp     t1, ScrnRows           wc, wz
        if_nc_or_z      jmp     #EraseFromBolDone
                        cmp     t2, ScrnCols           wc, wz
        if_nc_or_z      jmp     #EraseFromBolDone
                        'Calculate offset in screen memory to start of current row
                        call    #CalcRowAdrT1
                        'Calculate number of characters to clear
                        add     t2, #1                          'Current col index + 1 = chars to fill
                        'Calculate screen memory address of first char to clear
                        mov     t3, t1
                        add     t3, ScreenAdr                   'T2 = first screen memory address to clear
                        'fill the character table with spaces
                        mov     t4, t2
EraseFromBol1           wrbyte  CharFill, t3
                        add     t3, #1
                        djnz    t4, #EraseFromBol1
                        'fill the color table with current DrawColor in LongParam
                        shl     t1, #1
                        add     t1, ColorAdr
EraseFromBol2           wrword  LongParam, t1
                        add     t1, #2
                        djnz    t2, #EraseFromBol2
                        'all done return to caller
EraseFromBolDone        
EraseFromBol_ret        ret
'
'
'Scroll a region of the screen up
'T1 = First line of region T2 = number of rows to scroll up
ScrollRegUp             mov     t3, ScrnRows              'T3 = screen rows
                        sub     t3, #1                    'T3 = screen rows - 1
                        cmp     t1, t3            wc, wz  'Make sure first row index < (screen rows -1) 
        if_nc_or_z      jmp     #ScrollRegUp_ret
                        'Make sure start Row + scroll rows is not past end of screen
                        mov     t4, t1                     'T4 = start row index
                        add     t4, t2                     'T4 = last row index = start index + rows to scroll
                        cmp     t4, t3            wc, wz   'Make sure last row index <= (screen rows -1)
        if_nc_and_nz    mov     t2, t3                     'Past end, T2 = last screen row index                             
        if_nc_and_nz    sub     t2, t1                     'Past end, T2 = rows we can scroll up
                        'If rows (T2) = 0 we are done
                        tjz     t2, #ScrollRegUp_ret       
                        'T1 = destionation row index = start row index and is < (screen rows - 1)
                        'Now calc screen byte offset to destination
                        call    #CalcRowAdrT1
                        'Now calc screen byte offset to first source address
                        mov     t3, t1
                        add     t3, ScrnCols                    'T3 = screen byte offset to first source memory
                        'Next we need number of bytes to move
                        mov     t4, t2                          'T4 = number of rows to move up 
                        shl     t4, #1                          'T4 = row X 2 (offset table in words)
                        add     t4, ColOffsetAdr                'T4 = Address of (region rows-1) in row mult col table
                        rdword  t2, t4                          'T2 = number of characters to scroll
                        'First move the character table memory
                        mov     t4, t1                          'T4 = destination memory address offset
                        add     t4, ScreenAdr                   'T4 = destination character memory address
                        mov     t5, t3                          'T5 = source character memory address offset
                        add     t5, ScreenAdr                   'T5 = source character memory address
                        mov     t6, t2                          'T6 = number of characters to move
                        shr     t6, #2                          'T6 = number of longs to move                
ScrollRegUpChr          rdlong  t7, t5                          'Read source long
                        add     t5, #4                          'Point to next source address
                        wrlong  t7, t4                          'Save to destination address
                        add     t4, #4                          'Point to next destination address
                        djnz    t6, #ScrollRegUpChr
                        'now move the color table memory
                        'T1 = destination memory address offset
                        shl     t1, #1                          'Color table is in words, bytes X 2
                        add     t1, ColorAdr                    'T1 = destination screen memory address
                        'T3 = source color memory address offset
                        shl     t3, #1                          'Color table is in words, bytes X 2
                        add     t3, ColorAdr                    'T3 = source color memory address
                        'T2 = number of characters to move
                        shl     t2, #1                          'Color table is in words, bytes X 2
                        shr     t2, #2                          'T2 = number of longs to move                
ScrollRegUpClr          rdlong  t4, t3                          'Read source long
                        add     t3, #4                          'Point to next source address
                        wrlong  t4, t1                          'Save to destination address
                        add     t1, #4                          'Point to next destination address
                        djnz    t2, #ScrollRegUpClr
ScrollRegUp_ret         ret
'
'                    
'Scroll a region of the screen down
'T1 = First line of region T2 = number of rows to scroll down
ScrollRegDwn            mov     t3, ScrnRows              'T3 = screen rows
                        sub     t3, #1                    'T3 = screen rows - 1
                        cmp     t1, t3            wc, wz  'Make sure first row index < (screen rows -1) 
        if_nc_or_z      jmp     #ScrollRegDwn_ret
                        'Make sure start row + scroll rows is not past end of screen
                        mov     t4, t1                     'T4 = start row index
                        add     t4, t2                     'T4 = last row index = start index + rows to scroll
                        cmp     t4, t3            wc, wz   'Make sure last row index <= (screen rows -1)
        if_nc_and_nz    mov     t2, t3                     'Past end, T2 = last screen row index                             
        if_nc_and_nz    sub     t2, t1                     'Past end, T2 = rows we can scroll up
                        'if rows (T2) = 0 we are done
                        tjz     t2, #ScrollRegDwn_ret      
                        'T1 = source row index = start row index and is < (screen rows - 1)
                        'Now calc screen byte offset to source
                        call    #CalcRowAdrT1
                        'Now calc screen byte offset to first destination address
                        mov     t3, t1
                        add     t3, ScrnCols                    'T3 = screen byte offset to first destination memory
                        'Next we need number of bytes to move
                        mov     t4, t2                          'T4 = number of rows to move up 
                        shl     t4, #1                          'T4 = row X 2 (offset table in words)
                        add     t4, ColOffsetAdr                'T4 = Address of (region rows-1) in row mult col table
                        rdword  t2, t4                          'T2 = number of characters to scroll
                        'We must copy in reverse, calc our end address + 1
                        add     t1, t2                           'T1 = last source copy byte offset + 1
                        add     t3, t2                           'T1 = last destination copy byte offset + 1
                        'First move the character table memory
                        mov     t4, t3                          'T4 = destination memory address offset
                        add     t4, ScreenAdr                   'T4 = destination character memory address
                        mov     t5, t1                          'T5 = source character memory address offset
                        add     t5, ScreenAdr                   'T5 = source character memory address
                        mov     t6, t2                          'T6 = number of characters to move
                        shr     t6, #2                          'T6 = number of longs to move                
                        'Copy character memory bytes
ScrollRegDwnChr         sub     t5, #4                          'Point to next source address
                        rdlong  t7, t5                          'Read source long
                        sub     t4, #4                          'Point to next destination address
                        wrlong  t7, t4                          'Save to destination address
                        djnz    t6, #ScrollRegDwnChr
                        'now move the color table memory
                        'T1 = destination memory address offset
                        shl     t1, #1                          'Color table is in words, bytes X 2
                        add     t1, ColorAdr                    'T1 = destination screen memory address
                        'T3 = source color memory address offset
                        shl     t3, #1                          'Color table is in words, bytes X 2
                        add     t3, ColorAdr                    'T3 = source color memory address
                        'T2 = number of characters to move
                        shl     t2, #1                          'Color table is in words, bytes X 2
                        shr     t2, #2                          'T2 = number of longs to move
                        'Copy color table memory region
ScrollRegDwnClr         sub     t1, #4                          'Point to next source address
                        rdlong  t4, t1                          'Read source long
                        sub     t3, #4                          'Point to next destination address
                        wrlong  t4, t3                          'Save to destination address
                        djnz    t2, #ScrollRegDwnClr
ScrollRegDwn_ret        ret
'
'                    
'Delete the row specified in the first paramater.
varFirstRow             long    0
varClearRowIdx          long    0
varScrollRows           long    0
'
                        'Prepare for a partial to full screen scroll
                        'Calculate number characters to scroll for entire screen
DoScrollUpPrep          mov     varFirstRow, ScrollFirstRow
                        mov     varScrollRows, ScrollLastRow
                        sub     varScrollRows, ScrollFirstRow
                        mov     varClearRowIdx, ScrollLastRow
                        jmp     #DoDeleteRow
                        '
                        'Prepare for a full screen delete line
                        'Calculate number characters to scroll for entire screen
DoDeleteRowPrep         rdbyte  t1, CursorYAdr                  'get row to delete
                        mov     varFirstRow, t1                 'Indicate first row to scroll
                        mov     varClearRowIdx, ScrnRows
                        sub     varClearRowIdx, #1              'Last row of the screen
                        mov     varScrollRows, varClearRowIdx   'Calc rows to move = last row - first row - 1
                        sub     varScrollRows, varFirstRow
                        '                        
                        'Delete the current text row (varFirstRow) and fill last row
DoDeleteRow             mov     t1, varFirstRow
                        mov     t2, varScrollRows
                        call    #ScrollRegUp
                        'Now fill the last line with the selected color, varClearRowIdx points to row
                        mov     t1, varClearRowIdx
                        call    #ClearRow
                        'go wait for next command
                        jmp     #nextcmd                'wait for next command
'
'
'Insert a row at the row specified in the first paramater, T1 contains command
'
                        'Prepare for a partial to full screen scroll
                        'Calculate number characters to scroll for entire screen
DoScrollDwnPrep         mov     varFirstRow, ScrollFirstRow
                        mov     varScrollRows, ScrollLastRow
                        sub     varScrollRows, ScrollFirstRow
                        mov     varClearRowIdx, ScrollFirstRow
                        jmp     #DoInsertRow
                        '
                        'Prepare for a full screen insert line
                        'Calculate number characters to scroll for entire screen
DoInsertRowPrep         rdbyte  t1, CursorYAdr                  'get row to insert
                        mov     varClearRowIdx, t1
                        mov     varFirstRow, t1                 'Indicate first row to scroll
                        mov     varScrollRows, ScrnRows
                        sub     varScrollRows, #1               'Last row of the screen
                        sub     varScrollRows, varFirstRow
                        '                        
                        'Insert row at the current text row (T1) and fill row
DoInsertRow             mov     t1, varFirstRow
                        mov     t2, varScrollRows
                        call    #ScrollRegDwn
                        'Now fill the last line with the selected color, varClearRowIdx points to row
                        mov     t1, varClearRowIdx
                        call    #ClearRow
                        'go wait for next command
                        jmp     #nextcmd                'wait for next command
'
'
'                        
'Decode the paramater values in LongParam, The lower three bytes
'contain a value from 0..9, convert 3 digit decimal to binary.
'The results are saved back to the LongParam.  
DoDecodeParam           mov     t1, LongParam           'T1 = Fist digit
                        and     t1, #255                'Mask off first digit
                        shr     LongParam, #8           'Position next byte to read
                        mov     t2, LongParam           'T2 = Second digit
                        and     t2, #255                'Mask off second digit
                        shr     LongParam, #8           'Position next byte to read
                        mov     t3, LongParam           'T3 = Thirs digit
                        and     t3, #255                'Mask off first digit
                        'T1 has lsd
                        'Need to multiply 2nd digit by 10 and add to T1
                        add     t1, t2          'mult by 10 (X+X+8X)
                        add     t1, t2
                        shl     t2, #3
                        add     t1, t2
                        'Need to mult 3rd digit by 100 then add to T1
                        'first calculate T3 X 10 and save to T2
                        mov     t2, t3          'mult by 10 (X+X+8X)
                        add     t2, t3
                        shl     t3, #3
                        add     t2, t3
                        'T2 = T3 x 10, Mutl T2 X 10 and add to t1
                        add     t1, t2          'mult by 10 (X+X+8X)
                        add     t1, t2
                        shl     t2, #3
                        add     t1, t2
                        'T1 = decoded value for param, return param to caller
                        wrlong  t1, CmdParamAdr
                        'go wait for next command
                        jmp     #nextcmd                'wait for next command
'
'
'Earse selected row
DoEraseLine             rdbyte  t1, CursorYAdr          'T1 = cursor row
                        mov     t2, #0                  'start at first column
                        'T1 = cursor Y
                        call    #EraseToEol
                        'go wait for next command
                        jmp     #nextcmd                'wait for next command
'
'
'Earse selected row from col to end or row
DoEraseEol              rdbyte  t1, CursorYAdr         'T1 = cursor row
                        rdbyte  t2, CursorXAdr         'T2 = cursor column
                        'T2 = cursor X, T1 = cursor Y
                        call    #EraseToEol
                        'go wait for next command
                        jmp     #nextcmd                'wait for next command
'
'
'Earse selected row from start of row to col
DoEraseBol              rdbyte  t1, CursorYAdr         'T1 = cursor row
                        rdbyte  t2, CursorXAdr         'T2 = cursor column
                        ' T2 = cursor X, T1 = cursor Y
                        call    #EraseFromBol
                        'go wait for next command
                        jmp     #nextcmd                'wait for next command
'
'
'Set text attributes and colors
VarParamAdr             long    0
                        'First get the base color
DoDecodeAttrib          mov     t3, CmdParamAdr         'T3 = Base color if first global param, Params[0] 
                        rdlong  BaseColor, t3
                        'now for the text attributes
                        add     t3, #4                  'T3 = address of Param[1] = TextAttribs
                        rdlong  TextAttribs, t3   
                        'Read get the paramater count
                        add     t3, #4                  'T3 = address of Param[2] = ParamCnt
                        rdlong  ParamCnt, t3
                        'Save address of first paramater
                        add     t3, #4
                        mov     VarParamAdr, t3          'VarParmAdr = address of first paramater in memory   
                        'No params same as param[0] = 0
                        cmp     ParamCnt, #0    wz
              if_z      wrlong  ParamCnt, VarParamAdr   'No params same as param = 0
              if_z      mov     ParamCnt, #1            'indicate we have one paramater
                        'Read the next paramater
DecAttrRpt              rdlong  t1, VarParamAdr wz      'Read next paramater value
                        add     VarParamAdr, #4         'Point to next paramater
              if_z      jmp     #DoClrAttribs                        
                        '64 bit font colors are codes 128..191
                        test    t1, #128        wz
                        cmp     t1, #192        wc
        if_c_and_nz     jmp     #DoFgRgbColor
                        '64 bit background colors are codes 192..255
        if_nc_and_nz    jmp     #DoBgRgbColor
                        'We have a param < 128, lookup function in attrib jump table
                        cmp     t1, #c_MaxAttribValue   wc     'See if in attrib jump table
        if_nc           jmp     #DecAttrNxt                    'Paramater is out of range for jump table
                        'calc offset into attrib jump table                                 
                        mov     t2, t1
                        shl     t2, #1                         'Jump table is in words x2
                        add     t2, AttribJmpPtr               'Add global table address
                        rdword  t2, t2                         'Read attrib proccesing address
                        tjnz    t2, t2                         'IF address from jump table <> 0 jump to address
                        'All done try next param if any left to try
DecAttrNxt              djnz    ParamCnt, #DecAttrRpt
                        'Copy Results to paramater buffer
                        'We need to return the base color and text attributes
                        'First get the base color
                        mov     t3, CmdParamAdr         'T3 = address of Param[0] = BaseColor
                        wrlong  BaseColor, t3
                        'now for the text attributes
                        add     t3, #4                  'T3 = address of Param[1] = TextAttribs
                        wrlong  TextAttribs, t3   
                        'go wait for next command
                        jmp     #nextcmd                'wait for next command
'
'
'Clear Attributes
DoClrAttribs            mov     TextAttribs, #0                'Clear all text attributes
                        rdlong  BaseColor, RstParamsAdr        'Reset to initial colors
                        jmp     #DecAttrNxt
'
'
'Clear Bright/Dim modes
DoSetNormal             andn    TextAttribs, #ta_Dim + ta_Bright   'Clear dim/bright attribute
                        jmp     #DecAttrNxt
'
'
'Set Bright mode
DoSetBright             andn    TextAttribs, #ta_Dim    'Clear dim attribute
                        or      TextAttribs, #ta_Bright 'Set bright attribute
                        jmp     #DecAttrNxt
'
'
'Set dim mode
DoSetDim                andn    TextAttribs, #ta_Bright 'Clear bright attribute
                        or      TextAttribs, #ta_Dim 'Set dim attribute
                        jmp     #DecAttrNxt
'
'
'Set invert fg/bg mode
DoSetInvert             or      TextAttribs, #ta_Invert 'Save invert attribute
                        jmp     #DecAttrNxt
'
'
'Clear invert fg/bg mode
DoClearInvert           andn    TextAttribs, #ta_Invert 'Clear invert attribute
                        jmp     #DecAttrNxt
'
'
'Set invert fg/bg mode
DoSetAltFont            or      TextAttribs, #ta_AltFont 'Set alternate font attribute
                        jmp     #DecAttrNxt
'
'
'Clear invert fg/bg mode
DoClearALtFont          andn    TextAttribs, #ta_AltFont 'Clear alternate font attribute
                        jmp     #DecAttrNxt
'
'
'Set the font color to the color value stored at memory location T1
DoSetFgColor            rdlong  t2, t1                  'Read 32 bit color table entry
                        and     BaseColor, BgColorMsk   'Remove current font color
                        or      BaseColor, t2           'Set font color to table value
                        'make sure color is safe
                        and     BaseColor, AllColorMsk
                        jmp     #DecAttrNxt
'
'
'Set the background color to the color value stored at memory location T1
DoSetBgColor            rdlong  t2, t1
                        ror     t2, #8                  'Rotate color info to backgroung bytes
                        and     BaseColor, FontColorMsk
                        or      BaseColor, t2
                        'make sure color is safe
                        and     BaseColor, AllColorMsk
                        jmp     #DecAttrNxt
'
'
'Prepare to set a standard color from the color table
DoSetTableColor         mov     t2, #0                  'Assume a non dim color
                        cmp     t1, #50         wc      'See if we want a dim color
              if_nc     sub     t1, #20                 'We have a dim color, set to normal range (add 8 to 0..7)
              if_nc     mov     t2, #8                  'Offset to dim colors
                        sub     t1, #30                 'range is 30..37 for font color or 40..47 for background color
                        cmp     t1, #10         wc      'Set carry flag for font color
              if_nc     sub     t1, #10                 'number > 7 so convert to 0..7
                        'if no carry flag we have a background color, add offset to dim colors if required
              if_nc     add     t1, t2                  'Add offset to dim color if background color
                        'if carry flag we have a font color, set dim attribute if required
                        tjz     t2, #DoSetTableColorND  'if T2 = 0 then non dim color
                        'we have a dim color, if font color set dim attribute
              if_c      andn    TextAttribs, #ta_Bright 'Clear bright attribute
              if_c      or      TextAttribs, #ta_Dim    'Set dim attribute
                        'lookup color in color table
DoSetTableColorND       shl     t1, #2                  'X 4 for table entry offset (longs)
                        add     t1, BaseColorPtr        'Add table memory address
                        'set the color, if carry set we have a font color
              if_c      jmp     #DoSetFgColor
                        jmp     #DoSetBgColor
'
'
{                        
DoSetTableColor         mov     t2, #0                  'Assume a non dim color
                        cmp     t1, #50         wc      'See if we want a dim color
              if_nc     sub     t1, #20                 'We have a dim color, set to normal range (add 8 to 0..7)
              if_nc     mov     t2, #8                  'Offset to dim colors
                        sub     t1, #30                 'range is 30..37 for font color or 40..47 for background color
                        cmp     t1, #10         wc      'Set carry flag for font color
              if_nc     sub     t1, #10                 'number > 7 so convert to 0..7
                        add     t1, t2                  'Add offset to dim color
                        'lookup color in color table
                        shl     t1, #2                  'X 4 for table entry offset (longs)
                        add     t1, BaseColorPtr        'Add table memory address
                        'set the color, if carry set we have a font color
              if_c      jmp     #DoSetFgColor
                        jmp     #DoSetBgColor
'
'
}
'Set the font color to a 64 bit RGB value (2:2:2)
'The color's 8 bits are 00 RR GG BB.
'The base color is stored as a 32 bit value with two 16 bit words
'This allows the invert color command to rotate the base color.
DoFgRgbColor            andn    TextAttribs, #ta_Dim + ta_Bright   'Clear dim/bright attribute
                        and     t1, #63                 'Mask off lower 6 bit color
                        shl     t1, #2+8                'shift lower six bits to high six bits of next byte
                        mov     t2, t1                  'T2 = color in byte[1]
                        shl     t1, #16                 'T1 = color in byte[3]
                        or      t2, t1                  'T2 = color in byte[1], and color in byte[3], (bytes 0, 2 = 0)
SetFontColor            and     BaseColor, BgColorMsk   'Remove current font color
                        or      BaseColor, t2           'Set font color to table value
                        'do next attribute
                        jmp     #DecAttrNxt
'
'
'Set the background color to a 64 bit RGB value (2:2:2)
'The color's 8 bits are 00 RR GG BB.
'The base color is stored as a 32 bit value with two 16 bit words
'This allows the invert color command to rotate the base color.
DoBgRgbColor            and     t1, #63                 'Mask off lower 6 bit color
                        shl     t1, #2                  'shift lower six bits to high six bits
                        mov     t2, t1                  'T2 = color in byte[0]
                        shl     t1, #16                 'T1 = color in byte[2]
                        or      t2, t1                  'T2 = color in byte[0], and color in byte[2], (bytes 1, 3 = 0)
SetBgColor              and     BaseColor, FontColorMsk 'Remove current background color
                        or      BaseColor, t2           'Set background color to rgb value
                        'do next attribute
                        jmp     #DecAttrNxt
'
'
'Set the font color to the terminal reset value.
DoResetFontColor        rdlong  t2, RstParamsAdr        'First reset parameter is initial colors
                        and     t2, FontColorMsk        'Remove reset background color
                        jmp     #SetFontColor           'Set font color to value in T2 
'
'
'Set the background color to the terminal reset value.
DoResetBgColor          rdlong  t2, RstParamsAdr        'First reset parameter is initial colors
                        and     t2, BgColorMsk          'Remove reset font color
                        jmp     #SetBgColor             'Set font color to value in T2 
'
'
'Insert n chars at the cursor position;  T1 = row, T2 = column
'T3 = number of characters to insert. LongParam has the Draw color
IC_CopyCnt    long      0
IC_ClrScr     long      0
IC_ClrDst     long      0
'
InsertChars             mov     t7, t2                  'T7 = start column
                        add     t7, t3                  'Calc first char destination column
                        cmp     t7, ScrnCols   wc,wz    'See if we need to move chars (t7 >= ScrnCols then just fill)
          if_nc_or_z    jmp     #InserCharsFill
                        'copy chars to right to make space for inserted chars
                        'Calc charcaters to copy
                        mov     IC_CopyCnt, ScrnCols
                        sub     IC_CopyCnt, t2
                        sub     IC_CopyCnt, t3
                        'First calc start and end columns for copy
                        mov     t6, ScrnCols            'Reverse copy 
                        sub     t6, #1                  'T6 = first destination address
                        mov     t7, t6                  'Need first source address
                        sub     t7, t3                  'Source = dest - number of chars to insert
                        'Now calc screen byte offset to source row
                        call    #CalcRowAdrT1
                        add     t6, t1
                        add     t7, t1
                        'now for the color table
                        mov     IC_ClrDst, t6           'Need offset in color table to destination char
                        shl     IC_ClrDst, #1           'Color address is in words
                        mov     IC_ClrScr, t7           'Need offset in color table to source char
                        shl     IC_ClrScr, #1           'Color address is in words
                        'Add screen address to offsets
                        add     t6, ScreenAdr
                        add     t7, ScreenAdr
                        'Add color table address to offsets
                        add     IC_ClrScr, ColorAdr
                        add     IC_ClrDst, ColorAdr
                        'T6 = copy dest global memory address                
                        'T7 = copy source global memory address                
                        mov     t5, IC_CopyCnt
InsertCharsCpy          rdbyte  t4, t7                  'Read screen char to move
                        wrbyte  t4, t6                  'Write screen char to new location
                        sub     t6, #1                  'Point to next screen destination address
                        sub     t7, #1                  'Point to next screen source char to move
                        'now the color table
                        rdword  t4, IC_ClrScr           'Read screen color to move
                        wrword  t4, IC_ClrDst           'Write color to new location
                        sub     IC_ClrDst, #2           'Point to next destination address word
                        sub     IC_ClrScr, #2           'Point to next source color to move
                        djnz    t5, #InsertCharsCpy
                        'All required chars copied, fill insert space if require
                        'copy destination address points to last insert char copied - 1
InsertCharsBlnk         wrbyte  CharFill, t6
                        wrword  LongParam, IC_ClrDst    'Write blank color to insert position
                        sub     t6, #1
                        sub     IC_ClrDst, #2           'Point to next destination address word
                        djnz    t3, #InsertCharsBlnk
                        jmp     #InsertChars_ret        'We are done. 
                        'fill inserted chars with draw color and space chars to end of line
                        'Call EraseToEol T1 = row, T2 = column
InserCharsFill          call    #EraseToEol
InsertChars_ret         ret
'
'
'Delete n chars at the cursor position;  T1 = row, T2 = column
'T3 = number of characters to delete. LongParam has the Draw color
DeleteChars             mov     t7, t2                  'T7 = current column number
                        add     t7, t3                  'T7 = column of first non-deleted char
                        cmp     t7, ScrnCols    wz,wc   'Compare to max columns
        if_nc_or_z      jmp     #DeleteCharsFill
                        'we need to move the font/color for a some columns
                        'Calc number of columns to copy
                        mov     t5, ScrnCols            'T5 = number of columns
                        sub     t5, t7                  'T5 = number of chars to copy
                        'copy the chars T2 = destination index, T7 = source index, T5 = count
                        mov     t6, t1                  'Save current row to T6
                        call    #CalcRowAdrT1
                        add     t2, t1
                        add     t7, t1
                        'now for the color table
                        mov     IC_ClrDst, t2           'Need offset in color table to destination char
                        shl     IC_ClrDst, #1           'Color address is in words
                        mov     IC_ClrScr, t7           'Need offset in color table to source char
                        shl     IC_ClrScr, #1           'Color address is in words
                        'Add screen address to offsets
                        add     t2, ScreenAdr
                        add     t7, ScreenAdr
                        'Add color table address to offsets
                        add     IC_ClrScr, ColorAdr
                        add     IC_ClrDst, ColorAdr
                        'copy the chars T2 = destination address, T7 = source address, T5 = count
DeleteCharsCpy          rdbyte  t4, t7
                        wrbyte  t4, t2
                        add     t2, #1
                        add     t7, #1
                        'now the color Info
                        rdword  t4, IC_ClrScr           'Read screen color to move
                        wrword  t4, IC_ClrDst           'Write color to new location
                        add     IC_ClrDst, #2           'Point to next destination address word
                        add     IC_ClrScr, #2           'Point to next source color to move
                        'done this column see if we need to do more columns
                        djnz    t5, #DeleteCharsCpy
                        'Copy done clear end of line
                        mov     t2, ScrnCols
                        sub     t2, t3
                        mov     t1, t6  
                        'Erase the row from the current column
DeleteCharsFill         call    #EraseToEol
DeleteChars_ret         ret
'
'
'Insert or delete X number of characters at the current cursor position.
'On entry t1 = current command. Param[0] = number of chars to insert.
DoShiftRow              mov     t4, t2                  'Save the command ident
                        mov     t3, t1                  'T3 = command and params
                        shr     t3, #8                  'T3 = number of chars to insert.
                        rdbyte  t2, CursorXAdr          'Set T2 = curent column
                        rdbyte  t1, CursorYAdr          'Set T1 = current row
                        cmp     t4, #c_DeleteChars  wz
              if_z      jmp     #ExDeleteChars
                        'Insert X number of characters at the current cursor position.
ExInsertChars           call    #InsertChars
                        jmp     #nextcmd                'wait for next command                                                                 
                        'Delete X number of characters at the current cursor position.
ExDeleteChars           call    #DeleteChars
                        jmp     #nextcmd                'wait for next command                                                                 
'
'
'Erase the next n characters on the row, no line wrap, Cursor does not move
DoEraseChars            mov     t4, t1                  'T3 = command and params
                        shr     t4, #8                  'T3 = number of chars to insert.
                        rdbyte  t3, CursorXAdr          'Set T3 = curent column
                        'Calc offset to current row/column start
                        rdbyte  t1, CursorYAdr          'Set T1 = curent column
                        call    #CalcRowAdrT1
                        add     t1, t3
                        'T1 = screen byte offset to current position
                        mov     t2, t1
                        shl     t2, #1
                        'T2 = color table offset
                        add     t1, ScreenAdr
                        add     t2, ColorAdr
                        'Erase next character
DoEraseChars_Rpt        wrbyte  CharFill, t1            'Erase current column
                        wrword  LongParam, t2           'Set correct colors
                        'Point to next character to erase
                        add     t1, #1
                        add     t2, #2
                        add     t3, #1
                        cmp     t3, ScrnCols     wz
              if_nz     djnz    t4, #DoEraseChars_Rpt           
                        jmp     #nextcmd      'Done escape command

'These variables are past to the cog at startup
'
D0                      long    1 << 9
ScreenAdr               long    0
ColorAdr                long    0
ColOffsetAdr            long    0
AttribJmpPtr            long    0
RstParamsAdr            long    0
'
ScrnCols                long    0
ScrnRows                long    0
LongParam               long    0
CharFill                long    $20202020
'Command parser table address
CmdParseAdr             long    0
CmdParamAdr             long    0
'Scrool UP/down row boundries
ScrollFirstRow          long    0
ScrollLastRow           long    0
'
'Paramater variables to compile
CursorXAdr              long    0
CursorYAdr              long    0
ParamsPtr               long    0
BaseColorPtr            long    0
FontColorMsk            long    tc_FontColorMsk
BgColorMsk              long    tc_BgColorMsk
AllColorMsk             long    tc_ColorMsk
'
'
'End of cog paramaters
'
' Uninitialized data
'
t1                      res     1
t2                      res     1
t3                      res     1
t4                      res     1
t5                      res     1
t6                      res     1
t7                      res     1
'
'
'These values are passed to helper on function call
'to set attributes.
ParamCnt      res       1
BaseColor     res       1
TextAttribs   res       1
'
'
'Main command parser jump table, must be c_MaxCommands long.
aCmdJmpTable  word
'c_NoCommand    =  0            no command waiting constant, MUST BE ZERO
word          0
'c_InsertLine =    1            P1 = current line, P2 = number of lines, DrawColor must be supplied.
word          (@DoInsertRowPrep - @entry) >> 2
'c_DeleteLine =    2            P1 = current line, P2 = number of lines, DrawColor must be supplied.
word          (@DoDeleteRowPrep - @entry) >> 2
'c_DecodeParam = 3              Decode three digit decimal number to binary, TempParam must be supplied
word          (@DoDecodeParam - @entry) >> 2
'c_SetAttribs    = 4            Set the text attribute/colors from the supplied paramaters
word          (@DoDecodeAttrib - @entry) >> 2
'c_ScrollUp      = 5            Scroll selected screen region up, no params. DrawColor must be supplied in LongParam.
word          (@DoScrollUpPrep - @entry) >> 2
'c_ScrollDown    = 6            Scroll selected screen region down, no params. DrawColor must be supplied in LongParam.
word          (@DoScrollDwnPrep - @entry) >> 2
'c_ScrollRegion = 7             Set first and last rows to scroll with scroll up and down commands. P1 = first row, P2 = last row both inclusive.
word          (@DoSetScrlRegion - @entry) >> 2
'c_FillFromRow = 8              Clear screen from row to end, P0 = current row. DrawColor must be supplied in LongParam.
word          (@DoFillFromRow - @entry) >> 2
'c_FillToRow =    9             Clear screen from start to row, P0 = current row. DrawColor must be supplied in LongParam.
word          (@DoFillToRow - @entry) >> 2
'c_FillScreen = 10              no params, DrawColor must be supplied.
word          (@DoFillScreen - @entry) >> 2
'c_EraseEol     = 11            Clear row specified in P0 from col in P1 to row end. DrawColor must be supplied in LongParam.
word          (@DoEraseEol - @entry) >> 2
'c_EraseBol     = 12            Clear row specified in P0 from start to row in P1. DrawColor must be supplied in LongParam.
word          (@DoEraseBol - @entry) >> 2
'c_EraseLine    = 13            Clear row specified in P0. DrawColor must be supplied in LongParam.
word          (@DoEraseLine - @entry) >> 2
'c_InsertChars = 14             Insert n blank characters at the current colum and row. DrawColor must be supplied in LongParam. Param[0] = number of chars
word          (@DoShiftRow - @entry) >> 2
'c_DeleteChars = 15             Delete n characters at the current colum and row. DrawColor must be supplied in LongParam. Param[0] = number of chars
word          (@DoShiftRow - @entry) >> 2
'c_EraseChars = 16              Erase n characters at the current colum and row. DrawColor must be supplied in LongParam. Param[0] = number of chars
word          (@DoEraseChars - @entry) >> 2
{
'last entries are all zero 17..31
word          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
}
'
'
'Set attributes jump table
'
aAttribJmpTable  word
'0
word          (@DoClrAttribs - @entry) >> 2
'1
word          (@DoSetBright - @entry) >> 2
'2
word          (@DoSetDim - @entry) >> 2
'3
word          0
'4
word          0
'5
word          0
'6
word          0
'7
word          (@DoSetInvert - @entry) >> 2
'8
word          0
'9
word          0
'10
word          (@DoClearALtFont - @entry) >> 2
'11
word          (@DoSetALtFont - @entry) >> 2
'12
word          0
'13
word          0
'14
word          0
'15
word          0
'16
word          0
'17
word          0
'18
word          0
'19
word          0
'20
word          0
'21
word          0
'22
word          (@DoSetNormal - @entry) >> 2
'23
word          0
'24
word          0
'25
word          0
'26
word          0
'27
word          (@DoClearInvert - @entry) >> 2
'28
word          0
'29
word          0
'30
word          (@DoSetTableColor - @entry) >> 2
'31
word          (@DoSetTableColor - @entry) >> 2
'32
word          (@DoSetTableColor - @entry) >> 2
'33
word          (@DoSetTableColor - @entry) >> 2
'34
word          (@DoSetTableColor - @entry) >> 2
'35
word          (@DoSetTableColor - @entry) >> 2
'36
word          (@DoSetTableColor - @entry) >> 2
'37
word          (@DoSetTableColor - @entry) >> 2
'38
word          0
'39
word          (@DoResetFontColor - @entry) >> 2
'40
word          (@DoSetTableColor - @entry) >> 2
'41
word          (@DoSetTableColor - @entry) >> 2
'42
word          (@DoSetTableColor - @entry) >> 2
'43
word          (@DoSetTableColor - @entry) >> 2
'44
word          (@DoSetTableColor - @entry) >> 2
'45
word          (@DoSetTableColor - @entry) >> 2
'46
word          (@DoSetTableColor - @entry) >> 2
'47
word          (@DoSetTableColor - @entry) >> 2
'48
word          0
'49
word          (@DoResetBgColor - @entry) >> 2
'50
word          (@DoSetTableColor - @entry) >> 2
'51
word          (@DoSetTableColor - @entry) >> 2
'52
word          (@DoSetTableColor - @entry) >> 2
'53
word          (@DoSetTableColor - @entry) >> 2
'54
word          (@DoSetTableColor - @entry) >> 2
'55
word          (@DoSetTableColor - @entry) >> 2
'56
word          (@DoSetTableColor - @entry) >> 2
'57
word          (@DoSetTableColor - @entry) >> 2
'58
word          0
'59
word          0
'60
word          (@DoSetTableColor - @entry) >> 2
'81
word          (@DoSetTableColor - @entry) >> 2
'62
word          (@DoSetTableColor - @entry) >> 2
'63
word          (@DoSetTableColor - @entry) >> 2
'64
word          (@DoSetTableColor - @entry) >> 2
'65
word          (@DoSetTableColor - @entry) >> 2
'66
word          (@DoSetTableColor - @entry) >> 2
'67
word          (@DoSetTableColor - @entry) >> 2
'68
word          0
