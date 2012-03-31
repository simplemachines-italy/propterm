'PropTerm Propeller Chip ANSI Terminal program
'
CON

  _clkmode = xtal1 + pll16x
  _xinfreq = 5_000_000
  c_SetupWidth                  = 36
  c_SetupHeight                 = 31
  c_SetupBaseX                  = (term#cols - c_SetupWidth) / 2
  c_SetupBaseY                  = (term#rows - c_SetupHeight) / 2
  
  c_ValueOffsetX                = 20
  c_FieldWidth                  = 8
  c_8BitFuncPrefix              = 143
  c_BaudRateCnt                 = 17
  'Setting save/load from serial eeprom constants
  c_SettingsBytes               = 13 + c_MaxFonts    'Number of bytes in settings
  c_SettingsIdentSize           =  6
  c_SettingsCheckSize           =  2
  c_SettingsHeaderSize          =  c_SettingsIdentSize + c_SettingsCheckSize    'Number of bytes in settings header
  c_SettingsTotalSize           =  c_SettingsBytes +  c_SettingsHeaderSize      'Total number of bytes in settings packet
  'EEprom general stuff
  EEprom_PageBytes              =  term#c_EEpromPageBytes                                               'Bytes per eeprom page
  'EEprom font load/save settings
  c_MaxFonts                    =  5                             'Number of fonts we can have, 0 = default font
  c_DefaultFontIdx              =  0                             'Font save for default terminal font
  c_SaveFontIdx                 =  c_MaxFonts - 1                'Font save for current terminal font when in settings dialog

  c_ScreenColorLongs = (term#rows * term#cols) / 2

  EEprom_SettingsAdr            =  term#c_EEpromSettingsAdr      'EEprom address of terminal settings (must fit in 512 bytes)
  EEprom_FontStartAdr           =  term#c_EEpromFontStartAdr     'Leave first region for settings
  EEprom_FontByteSize           =  term#c_EEpromFontByteSize     'Fonts use 12 bytes per glyph, there are 256 glyphs
  EEprom_FontSaveSize           =  term#c_EEpromFontSaveSize     'Total pages per font * eeprom page size
  EEprom_ScreenSaveCharsAdr     =  EEprom_FontStartAdr + (((EEprom_FontSaveSize * c_MaxFonts) + (EEprom_PageBytes -1)) / EEprom_PageBytes) * EEprom_PageBytes
  'EEprom screen save/restore settings
  EEprom_ScreenCharBytes        =  (term#rows * term#cols)                                              'Number of eeprom bytes for screen character buffer
  EEprom_ScreenCharPages        =  (EEprom_ScreenCharBytes + EEprom_PageBytes - 1) / EEprom_PageBytes   'Number of eeprom pages for screen character buffer
  EEprom_ScreenSaveColorsAdr    =  EEprom_ScreenSaveCharsAdr + (EEprom_ScreenCharPages * EEprom_PageBytes) 'Save screen color buffer address, for setup
  EEprom_ScreenColorBytes       =  EEprom_ScreenCharBytes * 2                                           'Number of eeprom bytes for screen color buffer
  EEprom_ScreenColorPages       =  EEprom_ScreenCharPages * 2                                           'Number of eeprom pages for screen color buffer
  EEprom_LastAddress            =  EEprom_ScreenSaveColorsAdr + (EEprom_ScreenColorPages * EEprom_PageBytes)

OBJ

  term  : "AnsiTerminal"
  kb    : "keyboard"


VAR

  word  akey
  byte  aKeyValue
  byte  aKeyState
  byte  aInitFontIdx
  '
  'Editing and state variables:
  'These are written/read to/from the extra serial eeprom so
  'there order matters. Alway add new setting variables at the end.
  'Also the c_SettingsBytes must reflect the size of the settings.
  byte  hSettingsIdent[6]       '6 byte identifier
  byte  hCheckSumMSB            'MSB of 16 bit inverted checksum
  byte  hCheckSumLSB            'LSB of 16 bit inverted checksum
  'Start of actual settings bytes
  byte  aAutoLineWrap
  byte  aFontColorIdx
  byte  aBgColorIdx
  byte  aMapCursorKeyAsFunc
  byte  aUse8BitFuncPrefix
  byte  aEnterSendsCRLF
  byte  aUseTTYMode
  byte  aLocalEcho
  byte  aRxCRAsNL
  byte  aBaudRateIdx
  byte  aFontIdx
  byte  aDataBits
  byte  aParityMode
  'these track if we have custom fonts loaded
  byte  aValidCustomFont[c_MaxFonts]
  'Start of actual settings bytes.
  '
  byte  aTempByte
  'The next two groups of longs must stay grouped together.
  'Global memory to hold startup and editing paramaters.
  'State variables must be in same order as used by terminal cog.
  'The constant "term#c_StateVarCnt" must have the correct variable count. 
  long RS_BaseColor
  long RS_TextAttribs
  long RS_ScrlFirstLine
  long RS_ScrlLastLine 
  long RS_LineWrapState
  long RS_SaveCursorY  
  long RS_SaveCursorX  
  'Global memory to hold last terminal paramaters.
  'State variables must be in same order as used by terminal cog.
  'The constant "term#c_StateVarCnt" must have the correct variable count. 
  long SS_BaseColor
  long SS_TextAttribs
  long SS_ScrlFirstLine
  long SS_ScrlLastLine 
  long SS_LineWrapState
  long SS_SaveCursorY  
  long SS_SaveCursorX  

PUB start | i
  'Initialize all options variables
  InitSettings
  term.initialize(GetBaudRate, aDataBits, aParityMode)
  'wait for terminal to start
  i := cnt + clkfreq / 10
  waitcnt(i)
  'try to read our settings
  LoadOrInitSettings
  'If any fonts are empty fill with default font
  repeat i from 0 to 3
    if (aValidCustomFont[i] == FALSE)
      aValidCustomFont[i] := SaveFont(i)
'  SaveFont(1)    
  'Set the correct font at startup
  LoadFont(aFontIdx)    
  'start termianl and clear the screen
  ApplyResetSettings
  term.start(@RS_BaseColor, GetBaudRate, aDataBits, aParityMode)
  'wait for terminal to start
  i := cnt + clkfreq / 10
  waitcnt(i)
  ApplyTermSettings
  'start the keyboard
  kb.start(26, 27)
  repeat
    'main loop of terminal
    akey := kb.key
    if akey
        ParseKbd
        

PRI  LoadOrInitSettings
  if LoadSettings <> TRUE
    InitSettings

PRI InitSettings
  'Initialize reset state variables
  {
  RS_BaseColor := term#tc_InitColor
  RS_TextAttribs := 0
  RS_ScrlFirstLine := 0
  RS_ScrlLastLine := term#rows-1
  RS_LineWrapState := term#c_LineWrapOff 'Set line wrap mode off
  RS_SaveCursorY := 0  
  RS_SaveCursorX := 0
  }
  longfill(@RS_BaseColor, 0, 7)
  RS_BaseColor := term#tc_InitColor
  RS_ScrlLastLine := term#rows-1
  RS_LineWrapState := term#c_LineWrapOff 'Set line wrap mode off
  'Initalize our settings incase we fail to read valid setting from eeprom
  {
  aFontIdx            := 0                            'Use the default font not a custom font at reset/startup
  aMapCursorKeyAsFunc := FALSE                        'Cursor keys are sent as ANSI CSI codes not function key codes
  aUse8BitFuncPrefix  := FALSE                        'Function keys send "ESC O" not character 143 as prefix
  aEnterSendsCRLF     := FALSE                        'Set ENTER key to send a CR not CR+LF
  aUseTTYMode         := FALSE                        'Set default terminal mode to ANSI not TTY
  aAutoLineWrap       := FALSE                        'Set default line wrap mode off
  aLocalEcho          := FALSE                        'Set local echo off
  aRxCRAsNL           := FALSE                        'RX CR is parsed as CR not CR+LF
  aFontColorIdx       := 7                            'Set font color to white
  aBgColorIdx         := 0                            'Set background color to black
  aBaudRateIdx        := c_BaudRateCnt - 2            'Set baud rate  115200
  aDataBits           := 8                            'Set 8 bit data
  aParityMode         := 0                            'Set no parity          
  'Indicate all fonts are invalid
  bytefill(@aValidCustomFont[0], FALSE, c_MaxFonts)
  }
  bytefill(@aAutoLineWrap, 0, c_SettingsBytes)
  aFontColorIdx       := 7                            'Set font color to white
  aBaudRateIdx        := c_BaudRateCnt - 2            'Set baud rate  115200
  aDataBits           := 8                            'Set 8 bit data
  
   
PRI ParseKbd
  'Convert keyboard keys to actions
  aKeyValue:= akey & $FF
  aKeyState:= (akey >> 8) & $0F
  if (aKeyState == 8)
      'WIN key pressed
      if  (aKeyValue == $DB)
        DoSetup 
      elseif (aKeyValue == $D0)
        term.LocalModeOn
        WriteEscCode("c")
        LoadFont(aFontIdx)    
        term.LocalModeOff
  else       
    if term.KbdFifoSpace > 3
      if (aKeyValue) < 32
        SendControlKey
      else  
        if (aKeyValue < 128)
          SendCharKey
        else
          ParseFunctionKey
          
PRI ParseFunctionKey
 'we have a non modified key
  if (aKeyState & 8) == 0  
    case aKeyValue
      $C2:                      'up arrow
        SendCursEscSeq("A")
      $C3:                      'down arrow
        SendCursEscSeq("B")
      $C0:                      'left arrow
        SendCursEscSeq("D")
      $C1:                      'right arrow
        SendCursEscSeq("C")
      $C6:                      'page up
        SendExKeyEscSeq("I")
      $C7:                      'page down
        SendExKeyEscSeq("J")
      $C4:                      'Home key
        SendExKeyEscSeq("G")
      $C5:                      'End
        SendExKeyEscSeq("H")
      $C8:                      'Backspace
        term.tx(8)              
      $C9:                      'Delete         
        SendExKeyEscSeq("F")
      $CA:                      'Insert         
        SendExKeyEscSeq("E")
      $D0..$DB:                 'F1..F11
        SendExKeyEscSeq("P"+(akey - $D0))
        
PRI SendCursEscSeq(Param)
  'Send an extended key code string
  if (aMapCursorKeyAsFunc == FALSE)
    WriteEscCode("[")
    term.tx(Param & $7F)
  else
    SendExKeyEscSeq(Param)

PRI SendExKeyEscSeq(Param)
  'Send an extended key code string
  if (aUse8BitFuncPrefix == False)
    WriteEscCode("O")
  else
    term.tx(c_8BitFuncPrefix)  
  if (aKeyState & 2)
    'We have a control key version
    Param :=Param + $20
  if (aKeyState & 4)
    'We have an alt key version
    Param :=Param + $80
  term.tx(Param & $FF)

PRI SendCharKey
  'We have a normal character to send > 31 and < 128
  'Shift attribute taken care of by KBD driver
  'Check for control key      
  if (aKeyState & 2)
    'We have a control character
    if (aKeyValue => "a") and (aKeyValue =< "z")
      'convert lowercase to uppercase
      aKeyValue -= $20
    if (aKeyValue < 64) or (aKeyValue > 95)
      'control key out of range so exit
      Return
    else  
      aKeyValue := (aKeyValue - $40) & $001F
  'ALT key adds 128 to key value    
  if (aKeyState & 4)
    'We have and ALT key
      aKeyValue := (aKeyValue + $80) & $00FF
  term.tx(aKeyValue)  

PRI SendControlKey
  'We have a dedicated control character generating key
  if (aKeyState == 1)
    'we have a shited control key
    if (aKeyValue == 13)
      'shifted enter key change to line feed
      aKeyValue:= 10
      aKeyState:= 0
    else  
      if (aKeyValue == 8)
        'shifted backspace key, change to DEL
        aKeyValue:= 127
        aKeyState:= 0
  'Output control key if state = 0      
  if (aKeyState == 0)
    'control keys do not use Ctrl or Alt
    if (aKeyValue == 13)
      if aEnterSendsCRLF
        term.tx(aKeyValue)
        aKeyValue := 10
    term.tx(aKeyValue)    

PRI WriteStr(string_ptr)

'' Print a zero-terminated string

  repeat strsize(string_ptr)
    term.tx(byte[string_ptr++])


PRI WriteInt(value) | i, j, v
'' Print a decimal number
  if value < 0
    -value
    term.Tx("-")

  i := 1_000_000_000
  j := 0
  repeat 10
    if value => i
      j += 1
      term.Tx(value / i + "0")
      value //= i
      result~~
    elseif result or i == 1
      if j
        term.Tx("0")
    i /= 10

PRI WriteEscCode(Code)
  term.Tx($1B)                  'Send escape control character
  term.tx(Code)                 'Send the code character

PRI WriteCSI
  term.Tx(155)                  'Send an 8 bit CSI char 155
'  WriteEscCode("[")            'Send a 7 bit CSI Esc["

PRI WriteEsc(Code)
  WriteCSI    
  term.Tx(Code)

PRI WriteEsc1(Param, Code)  
  WriteCSI    
  WriteInt(Param)    
  term.Tx(Code)

PRI WriteEsc2(Param1, Param2, Code)
  WriteCSI    
  WriteInt(Param1)    
  term.Tx(";")
  WriteInt(Param2)    
  term.Tx(Code)

PRI WriteEsc1Spi(Param, Code)
  WriteCSI    
  term.Tx("?")
  WriteInt(Param)    
  term.Tx(Code)
  
PRI GotoXY(X, Y)
  WriteEsc2(Y, X, "H")    

PRI ClrScr
  WriteEsc1(2,"J")

PRI SetBgColor(Index)
  if (Index => 8)
    Index := 60 + (Index & 7)
  else
    Index += 40
  WriteEsc1(Index, "m")

PRI DefaultBgColor
  WriteEsc1(49, "m")

PRI SetInvertOn
  WriteEsc1(7, "m")                  

PRI SetInvertOff
  WriteEsc1(27, "m")                 

PRI RepeatChar(Char, Num)
  term.tx(Char)
  WriteEsc1(Num-1, "b")  

PRI HideCursor
  WriteEsc1Spi(25,"l")
    
PRI ShowCursor
  WriteEsc1Spi(25,"h")
    
PRI DrawBox(X, Y, H, W) | i
  'Select symbol font
  WriteEsc1(11, "m")  
  'Draw top line
  GotoXY(X, Y)                  
  term.Tx(191)                  '159 + 32
  RepeatChar(176, W-2)          '144 + 32
  term.Tx(190)                  '158 + 32
  'Draw middle lines
  repeat i from (Y + 1) to (Y + H - 2)
    GotoXY(X, i)
    term.Tx(177)                '145 + 32
    WriteEsc1(10, "m")  
    RepeatChar(32, W-2)
    WriteEsc1(11, "m")  
    term.Tx(177)                '145 + 32
  'Draw bottom line
  GotoXY(X, Y + H - 1)
  term.Tx(189)                  '157 + 32
  RepeatChar(176, W-2)          '144 + 32
  term.Tx(188)                  '156 + 32
  'Select normal font
  WriteEsc1(10, "m")  

PRI DecodeColorIdx(Color) : Idx
  Color := (Color & $FF)
  Idx:=lookdownz(Color: 0,$80,$20,$A0,$08,$88,$28,$A8,$00,$40,$10,$50,$04,$44,$14,$54)

PRI DrawKeyLabel(X, Y, KeyId, LabelAdr)
  GotoXY(X, Y)
  term.tx("F")
  WriteInt(KeyId)
  writeStr(@s_KeyDiv)
  WriteStr(LabelAdr)
  
PRI DrawDataLables(X, Y)
  X:=X + 2
  DrawKeyLabel(X, Y += 1, 1, @l_Baud)  
  DrawKeyLabel(X, Y += 2, 2, @l_DataFormat)  
  DrawKeyLabel(X, Y += 2, 3, @l_Wrap)  
  DrawKeyLabel(X, Y += 3, 4, @l_FColor)  
  DrawBox(X + c_ValueOffsetX - 2, Y - 1, 3, 6)
  DrawKeyLabel(X, Y += 3, 5, @l_BgColor)  
  DrawBox(X + c_ValueOffsetX - 2, Y - 1, 3, 6)
  DrawKeyLabel(X, Y += 3, 6, @l_Cursor)
  DrawKeyLabel(X, Y += 2, 7, @l_KeyPrefix)
  DrawKeyLabel(X, Y += 2, 8, @l_EnterKey)
  DrawKeyLabel(X, Y += 2, 9, @l_TermMode)
  DrawKeyLabel(X, Y += 2, 10, @l_LocalEcho)
  DrawKeyLabel(X, Y += 2, 11, @l_RxCRAsNL)
  DrawKeyLabel(X, Y += 2, 12, @l_FontIdx)
  SetInvertOn
  X -= 1
  GotoXY(X, Y += 2)
  WriteStr(@l_UpdateFont)
  'invert font render mode
  GotoXY(X, Y += 1)
  WriteStr(@l_Footer)
  'normal font render mode
  SetInvertOff

PRI ClearField(X, Y)
  GotoXY(X,Y)
  RepeatChar(32, c_FieldWidth)
  GotoXY(X,Y)

PRI GetBaudRate : Rate | i
'  Rate := lookupz(aBaudRateIdx: 3,6,12,18,24,36,48,72,96,144,192,288,384,576,768,1152,1280)
  i := @t_BaudRates + (aBaudRateIdx << 1)
  WordMove(@Rate, i, 1)
  Rate *= 100
  
PRI DrawData(X, Y) | i
  SetInvertOn
  'Now for the values
  X := X + c_ValueOffsetX
  'First baud rate
  ClearField(X, Y += 1)
  i:=GetBaudRate
  WriteInt(i)
  'Next data bits and parity mode
  ClearField(X, Y += 2)
  WriteInt(aDataBits)
  term.tx(" ")
  if aParityMode == 1
    term.tx("O")
  elseif aParityMode == 2
    term.tx("E")
  else
    term.tx("N")
  'Next line wrap state  
  ClearField(X, Y += 2)
  if aAutoLineWrap
    WriteStr(@s_ON)
  else  
    WriteStr(@s_OFF)
 'Font Color
  SetInvertOff
  SetBgColor(aFontColorIdx)
  GotoXY(X+1, Y += 3)
  RepeatChar(32, 4)
 'Background color
  GotoXY(X+1, Y += 3)
  SetBgColor(aBgColorIdx)
  RepeatChar(32, 4)
  DefaultBgColor
  SetInvertOn
  'Cursor key respone type
  ClearField(X, Y += 3)
  if aMapCursorKeyAsFunc
    WriteStr(@s_FUNC)
  else
    WriteStr(@s_ANSI)
  'Function key prefix
  ClearField(X, Y += 2)
  if aUse8BitFuncPrefix
    WriteStr(@s_FuncCtrl8)
  else
    WriteStr(@s_FuncEsc)
  'Enter key sends
  ClearField(X, Y += 2)
  if aEnterSendsCRLF
    WriteStr(@s_CRLF)
  else
    WriteStr(@s_CR)
  'Terminal mode
  ClearField(X, Y += 2)
  if aUseTTYMode
    WriteStr(@s_TTY)
  else
    WriteStr(@s_ANSI)
  'Terminal mode
  ClearField(X, Y += 2)
  if aLocalEcho
    WriteStr(@s_ON)
  else
    WriteStr(@s_OFF)
  'Rx CR as
  ClearField(X, Y += 2)
  if aRxCRAsNL
    WriteStr(@s_CRLF)
  else
    WriteStr(@s_CR)
  ClearField(X, Y += 2)
  'Font index
  if aFontIdx == 0
    WriteStr(@s_Internal)
  else 
    WriteStr(@s_Custom)
    WriteInt(aFontIdx)
  'If the font index is default, can not update font
  X := X  + 1 - c_ValueOffsetX
  GotoXY(X, Y += 2)
  if aFontIdx
    WriteStr(@l_UpdateFont)
  else
    SetInvertOff
    RepeatChar(32, StrSize(@l_UpdateFont))
    
  'normal font render mode
  SetInvertOff
  
PRI ToggleVar(VarAdr)
  ByteMove(@aTempByte, VarAdr, 1)
  if aTempByte
    aTempByte := FALSE
  else
    aTempByte := TRUE
  ByteMove(VarAdr, @aTempByte, 1)

PRI ApplyResetSettings
  'Apply line wrap setting to reset values
  if aAutoLineWrap
    RS_LineWrapState :=term#c_LineWrapOn
  else
    RS_LineWrapState :=term#c_LineWrapOff
  'Apply Font and BG colors to reset values
  if aBgColorIdx == aFontColorIdx
    aBgColorIdx += 8
  RS_BaseColor := term.ResolveColorIdx(aFontColorIdx) | (term.ResolveColorIdx(aBgColorIdx) >> 8)

PRI ApplyTermSettings
  term.ChangeBaudRate(GetBaudRate, aDataBits, aParityMode)
  'Set local Echo mode
  if aLocalEcho
    term.LocalEchoOn
  else
    term.LocalEchoOff
  'Set if we treat CR as CR+LF
  if aRxCRAsNL
    term.CrAsNewLineOn
  else
    term.CrAsNewLineOff    
  'Set terminal mode, setup put us in ANSI mode, see if we need TTY
  if aUseTTYMode
    term.TermModeTTY

PRI SaveBlock(Adr, TotalBytes, EEpromAdr) : Ok | BytesToWrite, StartTime
'Save a region of global memory, may require multiple pages.
  Ok := TRUE
  term.EEpromReset
  repeat while (TotalBytes > 0)
    if TotalBytes > EEprom_PageBytes
      BytesToWrite := EEprom_PageBytes
    else
      BytesToWrite := TotalBytes
    if term.EEpromWrite(EEpromAdr, Adr, BytesToWrite)
      TotalBytes -= BytesToWrite
      EEpromAdr += BytesToWrite
      Adr += BytesToWrite
      StartTime := cnt
      repeat until term.EEpromReady
        if cnt - startTime > clkfreq / 10
          Ok := FALSE
          Quit
    else
      Ok := FALSE
      Quit

PRI LoadBlock(Adr, TotalBytes, EEpromAdr) : Ok | BytesToRead
'Read a region of eeprom to global memory, may require multiple pages.
  Ok := FALSE
  term.EEpromReset
  if term.EEpromRead(EEpromAdr, Adr, TotalBytes)
    Ok := TRUE

PRI SaveScreen : Ok
'Save screen character table and color table to EEprom
  Ok := SaveBlock(term.GetScreenCharsAdr, EEprom_ScreenCharBytes, EEprom_ScreenSaveCharsAdr)
  if (Ok == TRUE) 
    Ok := SaveBlock(term.GetScreenColorsAdr, EEprom_ScreenColorBytes, EEprom_ScreenSaveColorsAdr) 

PRI LoadScreen : Ok
'Load screen character table and color table from EEprom
  'Send ESC 9
  Ok := LoadBlock(term.GetScreenCharsAdr, EEprom_ScreenCharBytes, EEprom_ScreenSaveCharsAdr)
  if (Ok == TRUE) 
    Ok := LoadBlock(term.GetScreenColorsAdr, EEprom_ScreenColorBytes, EEprom_ScreenSaveColorsAdr) 

PRI SaveFont(FontIdx) : Ok | eeprom_adr
'Save font table to EEprom
  eeprom_adr := EEprom_FontStartAdr + (FontIdx * EEprom_FontSaveSize)
  Ok := SaveBlock(term.GetFontAdr, EEprom_FontByteSize, eeprom_adr)

PRI LoadFont(FontIdx) : Ok | eeprom_adr
'Load font table to EEprom
  eeprom_adr := EEprom_FontStartAdr + (FontIdx * EEprom_FontSaveSize)
  Ok := LoadBlock(term.GetFontAdr, EEprom_FontByteSize, eeprom_adr)

PRI SaveSettings : Ok | i, Adr
  bytemove(@hSettingsIdent, @s_SettingsIdent, c_SettingsIdentSize)
  i := 0
  Adr := @hCheckSumLSB + 1      'Address of first settings var
  'Calculate 16bit inverted checksum
  repeat c_SettingsBytes
    ByteMove(@aTempByte, Adr++, 1)
    i:= i + aTempByte
  i := !i
  'Now save checksum back to settings
  hCheckSumMSB := (i >> 8) & $FF
  hCheckSumLSB := i & $FF
  'Header is ready so save settings to first page on eeprom
  Ok := SaveBlock(@hSettingsIdent, c_SettingsTotalSize, EEprom_SettingsAdr)
  
PRI LoadSettings : Ok | i, Adr
  Ok := FALSE
  if LoadBlock(@hSettingsIdent, c_SettingsTotalSize, EEprom_SettingsAdr)  
  
    i := 0
    Adr := @hCheckSumLSB + 1      'Address of first settings var
    'Calculate 16bit inverted checksum
    repeat c_SettingsBytes
      ByteMove(@aTempByte, Adr++, 1)
      i:= i + aTempByte
    i := !i
    'Now save checksum back to settings
    if (hCheckSumMSB == (i >> 8) & $FF) and (hCheckSumLSB == i & $FF)
      Ok := TRUE
      
PRI InitSetup(X, Y)
  'For now the terminal is in ANSI mode
  term.TermModeANSI
  term.LocalEchoOff
  'Copy the current state buffer from terminal
  LongMove(@SS_BaseColor, term.GetSaveStateAdr,  term#c_StateVarCnt)  
  'Save terminal state
  WriteEscCode("7")
  'Make sure scroll region is entire screen
  WriteEsc2(1, term#rows,"r")
  'Clear the screen and home the cursor
  RS_BaseColor <-= 8
  ClrScr
  waitcnt(cnt + clkfreq / 10)
  RS_BaseColor ->= 8
  'Clear text attributes
  WriteEsc1(0 ,"m")
  'initialize setup display area 
  HideCursor
  GotoXY(X + 1, Y -1)
  WriteStr(@l_Heading)
  DrawBox(X, Y, c_SetupHeight, c_SetupWidth)
  aFontColorIdx :=DecodeColorIdx(RS_BaseColor >> 8)
  aBgColorIdx :=DecodeColorIdx(RS_BaseColor) 
  if aBgColorIdx == aFontColorIdx
    aBgColorIdx += 8
  if RS_LineWrapState
    aAutoLineWrap := TRUE
  else
    aAutoLineWrap := FALSE
  DrawDataLables(X, Y)

PRI UpdateFont(Idx)
  'Resore saved version of current font before entering setup dialog
  LoadFont(c_SaveFontIdx)
  'Update selecetd font with current terminal font
  aValidCustomFont[Idx] := SaveFont(Idx)
  'Load default font for setup
  LoadFont(c_DefaultFontIdx)
  
PRI DoSetUp | i, x, y, doReset
  doReset := FALSE
  term.KbdFlushFifo
  term.LocalModeOn
  SaveScreen
  'Save current font
  aInitFontIdx := aFontIdx
  SaveFont(c_SaveFontIdx)
  'Load default font for setup
  LoadFont(c_DefaultFontIdx)
  InitSetup(c_SetupBaseX, c_SetupBaseY)
  DrawData(c_SetupBaseX, c_SetupBaseY)
  aKey := 0
  aKeyState := 0
  repeat 
    aKey := kb.key
    aKeyState:= (akey >> 8) & $0F
    aKey:= akey & $FF
    if aKey
      if (aKeyState == 0)
        case aKey
          $1B:
            LoadOrInitSettings
            Quit
          $D0:
            aBaudRateIdx += 1
            aBaudRateIdx //= c_BaudRateCnt
          $D1:
            aParityMode += 1
            aParityMode //= 3
            if (aParityMode == 0)                    
              if (aDataBits == 7)
                aDataBits := 8
              else  
                aDataBits := 7
          $D2:
            ToggleVar(@aAutoLineWrap)
          $D3:
            aFontColorIdx += 1
            aFontColorIdx //= 8
          $D4:
            aBgColorIdx += 1
            aBgColorIdx //= 16
          $D5:
            ToggleVar(@aMapCursorKeyAsFunc)
          $D6:
            ToggleVar(@aUse8BitFuncPrefix)
          $D7:
            ToggleVar(@aEnterSendsCRLF)
          $D8:
            ToggleVar(@aUseTTYMode)
          $D9:
            ToggleVar(@aLocalEcho)
          $DA:
            ToggleVar(@aRxCRAsNL)
          $DB:
            aFontIdx += 1
            aFontIdx //= (c_MaxFonts-1)
      elseif (aKeyState == 8)
          case aKey
            $D0:
              Quit
            $D2:
              if aFontIdx
                UpdateFont(aFontIdx)
            $D4:            
              doReset := TRUE
              Quit
      DrawData(c_SetupBaseX, c_SetupBaseY)
  'We are done, apply settings and restore screen
  ApplyResetSettings
  'Load correct font
  if (aValidCustomFont[aFontIdx] == FALSE)
    aFontIdx := 0
  if (aInitFontIdx <> aFontIdx) or (doReset == TRUE)
    LoadFont(aFontIdx)
  else
    LoadFont(c_SaveFontIdx)                 'Restore prior font
  'Reset/Restore Terminal
  if doReset
    WriteEscCode("c")
    i := cnt + clkfreq / 10
    waitcnt(i)
  else
    ShowCursor
    ByteFill(@i, RS_BaseColor.byte[1], 4)
    LongFill(term.GetScreenColorsAdr, i, c_ScreenColorLongs)
    LoadScreen
    if aUseTTYMode
      'In TTY mode all attributes should be reset and
      'The scroll region need to be the full screen.
      'First get the saved state, we need cursor X and Y
      LongMove(@SS_BaseColor, term.GetSaveStateAdr,  term#c_StateVarCnt)  
      Y := SS_SaveCursorY
      X := SS_SaveCursorX
      LongMove(@SS_BaseColor, @RS_BaseColor,  term#c_StateVarCnt)  
      SS_SaveCursorY := Y
      SS_SaveCursorX := X
      LongMove(term.GetSaveStateAdr, @SS_BaseColor,  term#c_StateVarCnt)  
    'Load terminal state back to what it was before setup
    WriteEscCode("8")
    'Restore terminals original save state in case it was in use.
    i := cnt + clkfreq / 10
    waitcnt(i)
    LongMove(term.GetSaveStateAdr, @SS_BaseColor,  term#c_StateVarCnt)  
  ApplyTermSettings
  term.LocalModeOff
  SaveSettings
  
DAT
t_BaudRates   word      3,6,12,18,24,36,48,72,96,144,192,288,384,576,768,1152,1302

s_SettingsIdent         byte      "@PT1.1",0

s_ON                    byte      "ON ",0
s_Off                   byte      "OFF",0
s_ANSI                  byte      "ANSI",0
s_TTY                   byte      "TTY",0
s_FUNC                  byte      "FUNC",0
s_FuncEsc               byte      "Esc O",0
s_FuncCtrl8             byte      "Char 143",0
s_CR                    byte      "CR",0
s_CRLF                  byte      "CR+LF",0
s_Custom                byte      "Custom ",0
s_Internal              byte      "Default",0
s_KeyDiv                byte      " - ",0

l_Heading               byte      " PropTerm Setup ",0
l_Footer                byte      " WIN+F1 Save, WIN+F5 Save/Reset ",0
l_Baud                  byte      "Baud Rate:",0
l_Wrap                  byte      "Auto Wrap:",0
l_FColor                byte      "Font Color:",0
l_BgColor               byte      "BG Color:",0
l_Cursor                byte      "Cursor Keys:",0
l_KeyPrefix             byte      "Func Prefix:",0
l_EnterKey              byte      "Enter Key:",0
l_TermMode              byte      "Term Mode:",0
l_LocalEcho             byte      "Local Echo:",0
l_RxCRAsNL              byte      "Rx CR As:",0
l_FontIdx               byte      "Font:",0
l_DataFormat            byte      "Data Format:",0
l_UpdateFont            byte      " WIN+F3 - Update Font ",0
