''*********************************************
''*  PropTerm Communications Helper v1.0      *
''*********************************************
''This object implements support code for the
''ANSI terminal comunications.
''
''The main reason for this object is to manage comunications
''for the terminal object.
''
''The long CmdAdr contains the address of a memoru location
''used to pass commands from the ansi terminal object to this helper
''object. Byte[0] of the long is the command, bytes[1..3]
''may contain aditional byte paramaters.
''The address 4 bytes from the CmdAdr (next long)
''contains the first long paramater to use for the command if required.
''
''This object also provides support for the I2C EEPROM
''used to save setting, fonts and other terminal data.
''The EEPROM routines use a second long command address from the
''base comunications command set.
''
''The ANSI TAB support functions are also included in this object
''to aliviate the space problem I had in the AnsiHelper object.
''
CON
  'Com port pins
  com_RxPin     = 10
  com_TxPin     = 11
  com_RtsPin    = 12
  com_CtsPin    = 13
  
  'USB port pins
  usb_LsDataPin     =  0
  usb_TxEPin        =  8
  usb_RxFPin        =  9
  usb_WrPin         = 10
  usb_RdPin         = 11
  '
  'Commands
  c_NoCommand       = 0
  c_TermReadByte    = 1           'Data is returned in the first global paramater
  c_TermSendAck     = 2           'no paramaters
  c_TermSendXY      = 3           'Paramater 0 contains address to global Cursor buffer, 2 bytes X, Y zero based
  c_TermUpdateFont  = 4           '13 params contain glyph index and scanlines
  c_TermSetTab      = 5           'Set the tab stop for the current column. 
  c_TermClearTab    = 6           'Clear the tab stop for the current column.
  c_TermClearTabs   = 7           'Clear all set tab stops, no paramaters.
  c_TermNextTab     = 8           'Get next tab stop, On exit: Param[0] = new column
  c_TermPriorTab    = 9           'Get prior tab stop, On exit: Param[0] = new column
  'Command parser data
  c_CommandCount  = 10 
  c_CmdMask       = 15            'used to seperate command index from full command
  'Fifo Size Constants
  c_KbdTxFifoSize = 16
  c_LocalRxFifoSize = 16
  c_ComRxFifoSize = Com#c_RxFifoSize
  c_ComTxFifoSize = Com#c_TxFifoSize
  'Communication modes flags
  c_ComModeNormal =  0          'Term reads from com port, Term/KBD sent to com port
  c_ComModeEcho   =  1          'Term reads from com port and KBD, Term/KBD sent to com port 
  c_ComModeLocal  =  2          'Term reads from KBD, nothing sent to com port           
  'Ansi control characters
  c_AckChar = 6
  'Tab stop constants
  c_TabClear    =  0            'Indicates no tab stop, MUST BE ZERO
  c_TabSet      =  1
  'I2C EEprom stuf
  'The prototype board has a 64K serial eeprom. The Prop chip only uses the first 32K leaving 32K for the PropTerm
  'to store settings and downloadable fonts. The eeprom is also used to save/restore the terminal display when the
  'terminal's setup screen is displayed.
  'Changing the following constants should allow 32K to 128K serial EEProms to be supported. (only tested on 64K and 128K)
  c_EEpromSCLBit  = 28
  c_EEpromSDABit  = 29
  c_EEpromAdrTx   = $A0
  c_EEpromAdrRx   = $A1
  'EEprom address constants
  c_EEpromPageBytes        =  128                       'Bytes per eeprom page
  c_EEpromSettingsAdr      =  $8000                     'EEprom address of terminal settings (must fit in 512 bytes)
                                                        'First 32K used by prop chip for program and data
  c_EEpromFontStartAdr     =  c_EEpromSettingsAdr + 512 'Start of font save area. Leave first region for settings
  c_EEpromFontByteSize     =  12 * 256 'Fonts use 12 bytes per glyph, there are 256 glyphs  
  c_EEpromFontSaveSize     =  (((c_EEpromFontByteSize + (c_EEpromPageBytes -1))) / c_EEpromPageBytes) * c_EEpromPageBytes
  
OBJ
  com  : "FullDuplexSerial"
'  com  : "ParallelUSB"
  
VAR
  'These four long need to stay in order.
  'They are used to call the I2C EEprom functions
  long EEprom_Command
  long EEprom_EEpromAdr
  long EEprom_MemAdr
  long EEprom_ByteCount
  'Communication Mode
  long communication_mode
  'Fifo to send local data to the terminal
  long  local_fifo_head
  long  local_fifo_tail
  byte  local_fifo_buffer[c_LocalRxFifoSize]
  'Fifo to send kbd data to the terminal or com port
  long  kbd_fifo_head
  long  kbd_fifo_tail
  byte  kbd_fifo_buffer[c_kbdTxFifoSize]
  'cog for terminal helper
  long  cog                     'cog flag/id
  '  
PUB  Start(CmdAdr, FontTableAddress, ColTabStopsAdr, CursorAddress, Cols, BaudRate, DataBits, Parity) : okay
'' Initialize and start communication helper
  communication_mode := 0
  'Setup the local rx fifo
  LocalRxHeadAdr := @local_fifo_head
  LocalRxTailAdr := @local_fifo_tail
  LocalRxBufAdr :=  @local_fifo_buffer
  local_fifo_head:=0
  local_fifo_tail:=0
  'Setup the keyboard fifo
  KbdTxHeadAdr := @kbd_fifo_head
  KbdTxTailAdr := @kbd_fifo_tail
  KbdTxBufAdr :=  @kbd_fifo_buffer
  kbd_fifo_head := 0
  kbd_fifo_tail := 0
  'Font glyph table address
  FontTableAdr := FontTableAddress
  'Initalize command parser jump table
  CmdTableAdr:=@aCmdJmpTable
  '
  'Comment out correct start line depending on the type of host interface required.
  '  
  'Start the Parrallel USB port
 ' com.start(usb_LsDataPin, usb_TxEPin, usb_RxFPin, usb_RdPin, usb_WrPin)
  'Start the rs232 com port
  com.start(com_RxPin, com_TxPin, com_RtsPin, com_CtsPin, BaudRate, DataBits, Parity)
  '
  'Setup the com RX fifo
  ComRxHeadAdr := com.GetRxHeadAdr
  ComRxTailAdr := com.GetRxTailAdr
  ComRxBufAdr := com.GetRxBufAdr
  'Setup the com TX fifo
  ComTxHeadAdr := com.GetTxHeadAdr
  ComTxTailAdr := com.GetTxTailAdr
  ComTxBufAdr := com.GetTxBufAdr
  'communication mode
  ComModeAdr := @communication_mode
  'Global memory addresses
  TabStopsAdr := ColTabStopsAdr
  CursorXAdr := CursorAddress
  CmdParamAdr := CmdAdr + 4
  ScrnCols := cols
  EEprom_Command := 0
  I2C_ChkDoCmdTableAdr := @ChkDoCmdTable
  I2C_CmdAdr := @EEprom_Command
  I2C_CmdParamsAdr := @EEprom_EEpromAdr
  EEpromFontTableAdr := @EEpromFontStartTable
  EEpromFontByteSize := c_EEpromFontByteSize
  'Start our cog if we can
  okay := cog := cognew(@entry, CmdAdr) + 1
  
PUB  Stop
''Stop terminal ComHelper cog
  'Stop the RS232/USB interface cog 
  com.stop
  'Stop our own cog if it is running
  if cog
    cogstop(cog~ - 1)

PUB  EchoOn
''Set echo KBD to terminal on
  communication_mode |= c_ComModeEcho
     
PUB  EchoOff
''Set echo KBD to terminal off
  communication_mode &= !c_ComModeEcho

PUB  LocalModeOn
''Set local mode, terminal only reads from local (from KBD).
  communication_mode |= c_ComModeLocal
     
PUB  LocalModeOff
''Set terminal to read from KBD only. KBD is not sent to COM port.
''Terminal output is ignored and not sent to COM port.
  communication_mode &= !c_ComModeLocal

PUB  KbdWrite(kbd_byte) | i
''Send data byte may wait for room in buffer
  i:= (kbd_fifo_head + 1)
  if (i => c_KbdTxFifoSize)
    i := 0
  'wait for tail to advance
  repeat until (kbd_fifo_tail <> i)
  'now write next byte to fifo
  kbd_fifo_buffer[kbd_fifo_head] := kbd_byte
  kbd_fifo_head := i

PUB  KbdFifoSpace : Room | i, j
''Returns number of free bytes in the keyboard fifo
  if (kbd_fifo_head < kbd_fifo_Tail)
    Room := kbd_fifo_tail - kbd_fifo_head
  else
    Room := c_KbdTxFifoSize - (kbd_fifo_head - kbd_fifo_tail)

PUB  KbdFlushFifo
''Flush any bytes in the keyboard fifo
  kbd_fifo_head := kbd_fifo_tail
  
PUB ChangeDataFormat(aBaudRate, aDataBits, aParity)
''Change the serial port baudrate and data format while running
  com.ChangeDataFormat(aBaudRate, aDataBits, aParity)

PRI WaitForCommandDone : Ok
  repeat until (EEprom_Command == 0)
  Ok := (EEprom_EEpromAdr == 0)                         'result is returned in EEprom_EEpromAdr variable

PUB EEpromReset
''Perform a software reset on the eeprom device.
  'no need to ready paramaters for function call
  'Set the command
  EEprom_Command := 1
  'wait for function to complete
  WaitForCommandDone
  
PUB EEPromReady : IsReady
''See if last eeprom write has completed    
  'no need to ready paramaters for function call
  'Set the command
  EEprom_Command := 2
  'wait for function to complete
  IsReady := WaitForCommandDone

PUB EEpromRead(aEEpromAdr, aMemAdr, aByteCount) : Ok
''Copy the EEprom contents to global memory    
  'Ready paramaters for function call
  EEprom_EEpromAdr := aEEpromAdr
  EEprom_MemAdr := aMemAdr
  EEprom_ByteCount :=aByteCount
  'Last set the command
  EEprom_Command := 3
  'wait for function to complete
  Ok := WaitForCommandDone
  
PUB EEpromWrite(aEEpromAdr, aMemAdr, aByteCount) : Ok
''Write global memory to a single eeprom page    
  'Ready paramaters for function call
  EEprom_EEpromAdr := aEEpromAdr
  EEprom_MemAdr := aMemAdr
  EEprom_ByteCount :=aByteCount
  'Last set the command
  EEprom_Command := 4
  'wait for function to complete
  Ok := WaitForCommandDone
  
DAT

'***********************************
'* Assembly language ansii helper  *
'***********************************

                        org   0
'
'
' Entry
'
entry                   mov     KbpPreReadData, #0
                        mov     KbpPreReadState, #0
                        call    #I2C_InitEEprom
nextcmd                 mov     t1, #c_NoCommand        'No command constant
                        wrlong  t1, par                 'write command to indicate we are done
                        'wait for a command
waitcmd                 mov     t2, #255
waitcmdRpt              rdlong  t1, par          wz     'read command
              if_nz     jmp     #checkcmd
                        'no command waiting, do bg task
                        djnz    t2, #waitcmdRpt
                        call    #I2C_ChkDoCmd           'Perform any pending I2C command
                        call    #ProKbdFifo             'Process next KBD data.                        
                        jmp     #waitcmd                'if <> 0 we have a valid command, if no command wait
                        'Read the current first long param, if passed
checkcmd                rdlong  LongParam, CmdParamAdr
                        'Set T2 = current command ID
                        mov     t2, t1                  'Save coomand and params to T2
                        and     t2, #c_CmdMask          'T2 = command byte all params = 0
                        'We have a command see if it is valid
                        cmp     t2, #c_CommandCount     wc
              if_nc     jmp     #nextcmd                'if command index out of range we are done.                        
                        mov     t3, t2                  'T3 = command index
                        shl     t3, #1                  'Table is word based so X 2
                        add     t3, CmdTableAdr         'Add offset to table start in main memory
                        rdword  t3, t3                  'read the address of the command code
                        tjnz    t3, t3                  'if address <> 0 then jump to address
                         'if no command wait
                        jmp     #nextcmd
'
'                       
'These are the support functions
'
FifoHeadAdr   long      0
FifoTailAdr   long      0
FifoDataAdr   long      0
FifoByteSize  long      0

FifoRdNext              mov     t1, #0                     'indicate no data read                              
                        rdlong  TmpHead, FifoHeadAdr
                        rdlong  TmpTail, FifoTailAdr
                        cmp     TmpHead, TmpTail              wz
        if_z            jmp     #FifoRdNext_ret
                        'we have at least one byte to read
                        mov     TmpAdr, FifoDataAdr
                        add     TmpAdr, TmpTail
                        rdbyte  t1, TmpAdr                 'read the data from the fifo
                        or      t1, KbdValidDataFlg        'set valid data flag in bit 9 of data
                        'update the com RX fifo Tail
                        add     TmpTail, #1
                        cmp     TmpTail, FifoByteSize     wc
        if_nc           mov     TmpTail, #0                'address wrapped                                       
                        wrlong  TmpTail, FifoTailAdr       'update fifo tail
FifoRdNext_ret          ret
'
'
'Read a byte from the com port fifo.
'On exit T1 <> 0 indicates we read a byte. 
ComRdNext               mov     FifoHeadAdr, ComRxHeadAdr
                        mov     FifoTailAdr, ComRxTailAdr
                        mov     FifoDataAdr, ComRxBufAdr
                        mov     FifoByteSize, #c_ComRxFifoSize
                        call    #FifoRdNext
                        and     t1, #255
ComRdNext_ret           ret
'
'
'Read a byte from the local fifo.
'On exit T1 <> 0 indicates we read a byte. 
LocalRdNext             mov     FifoHeadAdr, LocalRxHeadAdr
                        mov     FifoTailAdr, LocalRxTailAdr
                        mov     FifoDataAdr, LocalRxBufAdr
                        mov     FifoByteSize, #c_LocalRxFifoSize
                        call    #FifoRdNext
                        and     t1, #255
LocalRdNext_ret         ret
'
'
'Read a byte from the KBD TX fifo.
'On exit T1 <> 0 indicates we read a byte. 
KbdRdNext               mov     FifoHeadAdr, KbdTxHeadAdr
                        mov     FifoTailAdr, KbdTxTailAdr
                        mov     FifoDataAdr, KbdTxBufAdr
                        mov     FifoByteSize, #c_KbdTxFifoSize
                        call    #FifoRdNext
KbdRdNext_ret           ret
'                        
'
'Write a byte passed in T1 to the the fifo.
'On exit T1 <> 0 indicates we wrote a byte. 
FifoWrNext              rdlong  TmpHead, FifoHeadAdr
                        and     t1, #255                   'KBD can send nulls so data is ORed with 100 hex
                        rdlong  TmpTail, FifoTailAdr
                        mov     TmpNextHead, TmpHead
                        add     TmpNextHead, #1
                        cmp     TmpNextHead, FifoByteSize    wc
        if_nc           mov     TmpNextHead, #0            'address wrapped
                        cmp     TmpNextHead, TmpTail         wz
                        'if_z no room to do write to fifo
        if_z            mov     t1, #0                     'indicate no data written
        if_z            jmp     #FifoWrNext_ret
                        'we can write the data bayte
                        mov     TmpAdr, FifoDataAdr
                        add     TmpAdr, TmpHead
                        wrbyte  t1, TmpAdr                 'read the data from the fifo
                        wrlong  TmpNextHead, FifoHeadAdr  
                        mov     t1, #1                     'indicate data written                              
FifoWrNext_ret          ret
'
'
'Write a byte passed in T1 to the com port fifo.
'On exit T1 <> 0 indicates we wrote a byte. 
ComWrNext               mov     FifoHeadAdr, ComTxHeadAdr
                        mov     FifoTailAdr, ComTxTailAdr
                        mov     FifoDataAdr, ComTxBufAdr
                        mov     FifoByteSize, #c_ComTxFifoSize
                        call    #FifoWrNext
ComWrNext_ret           ret
'
'                        
'Write a byte passed in T1 to the local fifo.
'On exit T1 <> 0 indicates we wrote a byte. 
LocalWrNext             mov     FifoHeadAdr, LocalRxHeadAdr
                        mov     FifoTailAdr, LocalRxTailAdr
                        mov     FifoDataAdr, LocalRxBufAdr
                        mov     FifoByteSize, #c_LocalRxFifoSize
                        call    #FifoWrNext
LocalWrNext_ret         ret
'
'
{
'Write a byte passed in T1 to the com port fifo.
'On exit T1 <> 0 indicates we wrote a byte. 
ComWrNext               rdlong  TmpHead, ComTxHeadAdr
                        and     t1, #255                       'KBD can send nulls so data is ORed with 100 hex
                        rdlong  TmpTail, ComTxTailAdr
                        mov     TmpNextHead, TmpHead
                        add     TmpNextHead, #1
                        cmp     TmpNextHead, #c_ComTxFifoSize wc
        if_nc           mov     TmpNextHead, #0                'address wrapped
                        cmp     TmpNextHead, TmpTail          wz
        if_z            mov     t1, #0                         'indicate no data written                              
        if_z            jmp     #ComWrNext_ret
                        'we can write the data bayte
                        mov     TmpAdr, ComTxBufAdr
                        add     TmpAdr, TmpHead
                        wrbyte  t1, TmpAdr                 'read the data from the fifo
                        wrlong  TmpNextHead, ComTxHeadAdr  
                        mov     t1, #1                     'indicate data written                              
ComWrNext_ret           ret
'
'
'Write a byte passed in T1 to the local fifo.
'On exit T1 <> 0 indicates we wrote a byte. 
LocalWrNext             rdlong  TmpHead, LocalRxHeadAdr
                        and     t1, #255                'KBD can send nulls so data is ORed with 100 hex
                        rdlong  TmpTail, LocalRxTailAdr
                        mov     TmpNextHead, TmpHead
                        add     TmpNextHead, #1
                        cmp     TmpNextHead, #c_LocalRxFifoSize   wc
        if_nc           mov     TmpNextHead, #0                'address wrapped
                        cmp     TmpNextHead, TmpTail             wz
        if_z            mov     t1, #0                     'indicate no data written                              
        if_z            jmp     #LocalWrNext_ret
                        'we can write the data bayte
                        mov     TmpAdr, LocalRxBufAdr
                        add     TmpAdr, TmpHead
                        wrbyte  t1, TmpAdr                 'read the data from the fifo
                        wrlong  TmpNextHead, LocalRxHeadAdr  
                        mov     t1, #1                     'indicate data written                              
LocalWrNext_ret         ret
'
'
}

{
'Read next byte destined for terminal.
'On exit: T1 <> 0, we have a byte for the terminal
RdTermData              call    #LocalRdNext            'Try to read from local fifo
                        cmp     t1, #0          wz      'See if we read anything T1 <> 0
              if_nz     jmp     #RdTermData_ret
                        'nothing to read from local fifo try COM port RX fifo
                        rdlong  t2, ComModeAdr  
                        test    t2, #c_ComModeLocal  wz 'See if local only mode
              if_nz     jmp     #RdTermData_ret         'Local mode so just exit T1 = 0
                        'no local data, not local only, so read com port.
                        'T1 = 0 for no data, so local return value correct.
                        call    #ComRdNext             
RdTermData_ret          ret
}
'
'
'Process the KBD fifo sending data to COM port
'and local fifo if required.
ProKbdFifo              mov     t1, KbpPreReadData      wz
              if_nz     jmp     #ProKbdFifoOld
                        'we need to read a new byte from KBD
                        mov     KbpPreReadState, #0     'Indicate byte not sent.
                        call    #KbdRdNext              'Try to read from kbd fifo

                        tjz     t1, #ProKbdFifo_ret     'if no data byte ready (T1 = 0) exit
                        {
                        cmp     t1, #0                  wz 
              if_z      jmp     #ProKbdFifo_ret         'if no data byte ready (T1 = 0) exit
                        }
                        'we have a byte to send to the COM port and/or the Local fifo
                        mov     KbpPreReadData, t1      'Save new/old data byte

ProKbdFifoOld           tjnz    KbpPreReadState, #ProKbdFifoLcl  'State <> 0 so we already sent to COM
                        {
ProKbdFifoOld           cmp     KbpPreReadState, #0     wz
              if_nz     jmp     #ProKbdFifoLcl          'State <> 0 so we already sent to COM
                        }
                        'See if we are in local mode, if not send to com port
                        rdlong  t2, ComModeAdr  
                        test    t2, #c_ComModeLocal     wz 
              if_nz     jmp     #ProKbdFifoLcl
                        'Local mode off send to COM port
                        call    #ComWrNext

                        tjz     t1, #ProKbdFifo_ret     'if data not sent (T1 = 0) exit
                        {              
                        cmp     t1, #0                  wz 
              if_z      jmp     #ProKbdFifo_ret         'if data not sent (T1 = 0) exit
                         }
                        'If we are in echo mode send to local fifo
                        rdlong  t2, ComModeAdr  
                        test    t2, #c_ComModeEcho      wz 
              if_nz     jmp     #ProKbdFifoLcl
                        'No echo required clean up and exit
                        mov     KbpPreReadData, #0
                        mov     KbpPreReadState, #0
                        jmp     #ProKbdFifo_ret
                        'Data was sent to com port if needed, change state
ProKbdFifoLcl           mov     KbpPreReadState, #1     'Set state to prevent multiple COM TX
                        mov     t1, KbpPreReadData      'restore saved data byte
                        'Try to write to local fifo
                        call    #LocalWrNext

                        tjz     t1, #ProKbdFifo_ret     'if data not sent (T1 = 0) exit
                        {             
                        cmp     t1, #0                  wz 
              if_z      jmp     #ProKbdFifo_ret         'if data not sent (T1 = 0) exit
                         }
                        'All done clean up and exit
                        mov     KbpPreReadData, #0
                        mov     KbpPreReadState, #0
ProKbdFifo_ret          ret
'
'
'Transmit the byte (lowwer 8 bits) in T1 from the terminal to the COM port.
'wait if the COM port TX fifo full.
'If local mode is active we do not send data from terminal to COM port.
TransmitData            long    0
Transmit                rdlong  t2, ComModeAdr  
                        test    t2, #c_ComModeLocal     wz 
              if_nz     jmp     #Transmit_ret                  'In local mode so dump data
                        'Local mode off send to COM port
                        mov     TransmitData, t1               'Save the data byte in case we retry.
TransmitWait            call    #ComWrNext
                        cmp     t1, #0                  wz
              if_nz     jmp     #Transmit_ret
                        'ccould not send, COM TX Fifo is full.
                        call    #I2C_ChkDoCmd                  'Perform any pending I2C command while we wait
                        mov     t1, TransmitData                                
                        jmp     #TransmitWait
Transmit_ret            ret
'
'
'T1 = a byte (lower 8 bits) to convert to a decimal string (3 asci bytes)
ByteToDec               movs    ByteToDecRd, #BtoD0            'Point to first value in dec lookup table
                        mov     t2, t1                         'T2 = byte to encode as asci string
                        mov     t3, #8                         'There are 8 bits per byte
                        mov     t1, #0                         'Clear the total
ByteToDecRd             mov     t4, BtoD0                      'Read bit digit value from table
                        add     ByteToDecRd, #1                'Point to next bit table value
                        shr     t2, #1        wc               'shift next bit into carry flag
                if_nc   jmp     #ByteToDecNxt                  'if bit = 0 then do next bit
                        'Need to decimal add T4 to T1
                        add     t1, t4                         'Just add normaly, will never byte overflow
                        'Decimal adjust results, first LS digit
                        mov     t4, t1                         'T4 = current digits
                        and     t4, #255                       'Mask LS digit
                        cmp     t4, #10       wc               'See if we have a carry from LS digit
                if_nc   sub     t1, #10                        'Set to decimal value
                if_nc   add     t1, #$100                      'Add carry into second digit
                        'Decimal adjust second digit
                        mov     t4, t1                         'T4 = current digits
                        shr     t4, #8                         'Move 2nd digit into LS byte
                        and     t4, #255                       'Mask LS digit
                        cmp     t4, #10       wc               'See if we have a carry from LS digit
                if_nc   sub     t1, D2Sub10                    'Set to decimal value
                if_nc   add     t1, D2Carry1                   'Add carry into third digit
ByteToDecNxt            djnz    t3, #ByteToDecRd               'do next bit if we have one
                        or      t1, BtoDAsciOffset                                      
ByteToDec_ret           ret
'
'
'This command writes a byte as a three byte asci string
'The byte is pased in T1

SBD_Var                 long    0
SendByteAsDec           call    #ByteToDec
                        mov     SBD_Var, t1
                        rol     SBD_Var, #16         'move 3rd digit to LS byte
                        mov     t1, SBD_Var
                        and     t1, #255
                        cmp     t1, #$30      wz     'Do not send leading zero
              if_nz     call    #Transmit
                        rol     SBD_Var, #8          'move 2nd digit to LS byte
                        mov     t1, SBD_Var
                        and     t1, #255      
                        call    #Transmit
                        rol     SBD_Var, #8          'move 1nd digit to LS byte
                        mov     t1, SBD_Var
                        and     t1, #255
                        call    #Transmit
SendByteAsDec_ret       ret
'
'                   
'Command functions
'
'Read the next byte for the terminal.
'Wait for a byte if none available.
DoTermRead              call    #LocalRdNext            'Try to read from local fifo
                        cmp     t1, #0          wz      'See if we read anything T1 <> 0
              if_nz     jmp     #DoTermReadOk
                        'nothing to read from local fifo try COM port if we are not in local only mode
                        rdlong  t2, ComModeAdr  
                        test    t2, #c_ComModeLocal  wz 'See if local only mode
              if_nz     jmp     #DoTermReadWait         'Local only try to read again
                        'no local data, not local only, so read com port
                        call    #ComRdNext              'Try to read from com RX fifo
                        cmp     t1, #0          wz      'See if we read anything T1 <> 0
              if_nz     jmp     #DoTermReadOk
                        'nothing to read from COM port, do it all again
                        'after we do any bg tasks...
DoTermReadWait          call    #ProKbdFifo             'Process next KBD data.
                        call    #I2C_ChkDoCmd           'Perform any pending I2C command
                        jmp     #DoTermRead
                        'We have good data, save in param memory    
DoTermReadOk            wrlong t1, CmdParamAdr
                        {
                        mov     t2, par
                        add     t2, #4
                        wrlong  t1, t2
                        }
                        jmp     #nextcmd
'
'                                          
'Place the escape sequence for a cursor position request into the fifo.
'LongParam has address of cursor buffer (X,Y one byte each)
CurX    long  0
CurY    long  0
DoSendCursorXY          rdbyte  CurX, LongParam           'Read cursor X from cursor buffer.
                        add     LongParam, #1           'Point to next byte.
                        rdbyte  CurY, LongParam           'Read cursor Y from cursor buffer.
                        'X/Y in 0 based need to report 1 based column/row.
                        add     CurX, #1
                        add     CurY, #1
                        ' T6 = cursor X, T5 = cursor Y
                        'We have decoded our params, so sent escape sequence
                        mov     t1, #$1B                'Send escape character
                        call    #Transmit
                        mov     t1, #"["                'send gaurd character
                        call    #Transmit
                        mov     t1, CurY                'T1 = Y cursor value
                        call    #SendByteAsDec          'Send Y as 3 digit asci string
                        mov     t1, #";"                'send paramater seperator char
                        call    #Transmit
                        mov     t1, CurX                'T1 = X cursor value
                        call    #SendByteAsDec          'Send X as 3 digit asci string
                        mov     t1, #"R"
                        call    #Transmit
                        'go wait for next command
                        jmp     #nextcmd                'wait for next command
'
'
'Send an ACK control character
DoSendAck               mov     t1, #c_AckChar          'Send escape character
                        call    #Transmit
                        'go wait for next command
                        jmp     #nextcmd                'wait for next command
'
'
'Fonr control command, one of the following;
'       0 - restore default font
'       1 - load custom font 1
'       2 - load custom font 2
'       3 - load custom font 3
'       4 - replace display font glyph with user supplied pattern
DoFontCommand           cmp     LongParam, #4  wz, wc
              if_z      jmp     #DoChangeGlyph
              if_nc     jmp     #nextcmd                'wait for next command
                        'we want to load one of 4 posible font from eeprom
                        shl     longParam, #1           '* 2 for word based address table
                        mov     t1, EEpromFontTableAdr
                        add     t1, longParam           'T1 has memory address of EEprom font store address
                        rdword  t1, t1                  'Read eeprom start address for selected font index
                        mov     t2, FontTableAdr        'Memory address of terminal font
                        mov     t3, EEpromFontByteSize
                        call    #I2C_CopyToMem          'load the font from the eeprom
                        jmp     #nextcmd                'wait for next command
'                        
'              
'Update a font glyph using the 13 parameters passed.
'The first paramater is the 8x12 glyph index to update (0,,255).
'The next twelve paramaters each contian one glyph scanline.
'Each paramater contain a single row LS bit first.
'Font table is split into 3 regions each with 4 scanlines
'for each of the 256 font glyphs. The glyphs are thus split
'into 3 segments (12 / 4 = 3).
'On entry LongParam = glyph index.
DoChangeGlyph           mov     t2, par
                        add     t2, #8                  'Point to glyph index value, second param
                        'Calc start address of glyph in font table
                        rdlong  LongParam, t2
                        and     LongParam, #255         'make sure we have a valid glyph index
                        shl     LongParam, #2           'Glyph index * 4 for start offset in font table segment
                        add     LongParam, FontTableAdr 'Calc glyph global memory address
                        add     t2, #4                  'Point to first scanline data third param
                        'There are three glyph font segments to write
                        mov     t6, #3
                        'T2 = next Paramater address, LongParam = glyph start address
DoChangeGlyphSEG        mov     t1, #4                  'T1 = number of params to write to font table segment
                        mov     t3, LongParam           'T3 = next glyph address to write
                        'Ready to copy glyph scan line
DoChangeGlyphNSL        rdlong  t4, t2                  'Read next paramater
                        wrbyte  t4, t3                  'Update next glyph scan line
                        add     t2, #4                  'Point to next paramater
                        add     t3, #1                  'Point to next glyph scanline in font segment
                        djnz    t1, #DoChangeGlyphNSL   'Update next scanline if required
                        'finished segment move to next segment
                        add     LongParam, D0           'point to next font segment (256 * 4)
                        djnz    t6, #DoChangeGlyphSEG   'Update next font segment if required
                        jmp     #nextcmd                'wait for next command
'
'
'Set the current column as a tab stop.
DoSetTab                mov     t2, #c_TabSet           'we want to set a tab stop.
                        jmp     #UpdateTabStop
'Clear the current column as a tab stop.
DoClearTab              mov     t2, #c_TabClear         'we want to clear a tab stop.
'                       'update the current column tab stop
UpdateTabStop           rdbyte  t1, CursorXAdr                  
                        add     t1, TabStopsAdr
                        wrbyte  t2, t1                             
                        'go wait for next command
                        jmp     #nextcmd                'wait for next command
'
'
'Clear all tab stops
DoClearTabs             mov     t1, TabStopsAdr
                        mov     t2, ScrnCols
                        mov     t3, #c_TabClear
DoClearTabsRpt          wrbyte  t3, t1
                        add     t1, #1
                        djnz    t2, #DoClearTabsRpt                  
                        'go wait for next command
                        jmp     #nextcmd                'wait for next command
'
'
'Find the next tab stop. Tabs never line wrap.
'On Exit: The global param[0] will contain the new column. 
'if no tab is found the retuen value is the current column.
'Tabs are saved as an array of bytes in global memory.
DoNextTab               rdbyte  t1, CursorXAdr          'Read current column
                        'T1 = current column
                        mov     t2, t1                  'T2 = current column
                        mov     t3, TabStopsAdr         'point to first tab stop
                        add     t3, t2                  'T3 = memory address of next tab to check
                        'T1 = return value, T2 = next tab stop to check, T3 = address of tab to check
DoNextTabRpt            add     t2, #1                  'T2 = next column index to check
                        add     t3, #1                  'T3 = next tab global memory address
                        cmp     t2, ScrnCols     wz     'See if we are done
              if_z      jmp     #DoNextTabDone          'No tab found just return current value
                        'see if this is a tab stop (<> 0)
                        rdbyte  t4, t3          wz
                        'if tab stop set (<> 0), copy column to T1 and quit
              if_nz     mov     t1, t2
                        'Not a valid tab stop, try next column if any.
              if_z      jmp     #DoNextTabRpt           'No tab try next column.
                        'All done T1 = return value
DoNextTabDone           wrlong  t1, CmdParamAdr
                        jmp     #nextcmd                'wait for next command                                                                 
'
'
'Find the Prior tab stop. Tabs never line wrap.
'On Exit: The global param[0] will contain the prior tab column. 
'if no tab is found the retuen value is the current column.
'Tabs are saved as an array of bytes in global memory.
DoPriorTab              rdbyte  t1, CursorXAdr          'Read current column
                        'T1 = current column
                        mov     t2, t1                  'T2 = current column
                        mov     t3, TabStopsAdr         'point to first tab stop
                        add     t3, t2                  'T3 = memory address of Prior tab to check
                        'T1 = return value, T2 = Prior tab stop to check, T3 = address of tab to check
DoPriorTabRpt           sub     t2, #1           wc     'T2 = Prior column index to check
                        sub     t3, #1                  'T3 = Prior tab global memory address
              if_c      jmp     #DoPriorTabDone         'No tab found just return current value
                        'see if this is a tab stop (<> 0)
                        rdbyte  t4, t3          wz
                        'if tab stop set (<> 0), copy column to T1 and quit
              if_nz     mov     t1, t2
                        'Not a valid tab stop, try prior column if any.
              if_z      jmp     #DoPriorTabRpt          'No tab try Prior column.
                        'All done T1 = return value
DoPriorTabDone          wrlong  t1, CmdParamAdr
                        jmp     #nextcmd                'Wait for next command.
'
'
'I2C EEPROM Stuff
'
'Initialize our I2C serial 64KB eeprom.
'
I2C_InitEEprom          cmp     t1, t1          wz       'Make Z flag = 1
                        'make sure SCL = 0 at eeprom
                        muxnz   outa, I2C_SCL_Bit        'SCL bit = 0
                        muxz    dira, I2C_SCL_Bit        'SCL dir = output, eeprom SCL = 0
                        call    #I2C_BitDelay
                        'make sure SDA = 1 at eeprom
                        muxnz   outa, I2C_SDA_Bit        'SDA output register bit = 0
                        muxnz   dira, I2C_SDA_Bit        'Set SDA as input = 1 at eeprom
I2C_InitEEprom_ret      ret
'
'                        
'Perform I2C one bit delay (Delay > 0.5usec), max eeprom clock = 1mhz
I2CDelayCnt             long    0
I2C_BitDelay            mov     I2CDelayCnt, cnt
                        add     I2CDelayCnt, #20      'we use about (6 * 4) clocks to get in and out = 20 + 24 = 44 ~ .5usec
                        waitcnt I2CDelayCnt, #0
I2C_BitDelay_ret        ret
'
'
'Set SCL to low
I2C_SCL_LOW             cmp     t1, t1          wz       'Make Z flag = 1
                        'make sure SCL = 0 at eeprom
                        muxnz   outa, I2C_SCL_Bit        'SCL bit = 0
                        call    #I2C_BitDelay
I2C_SCL_LOW_ret         ret
'                        
'
'Set SCL to low
I2C_SCL_HIGH            cmp     t1, t1          wz       'Make Z flag = 1
                        'make sure SCL = 0 at eeprom
                        muxz    outa, I2C_SCL_Bit        'SCL bit = 0
                        call    #I2C_BitDelay
I2C_SCL_HIGH_ret        ret
'                        
'
'Set SDA to low, also SDA is output
I2C_SDA_LOW             cmp     t1, t1          wz       'Make Z flag = 1
                        'make sure SCL = 0 at eeprom
                        muxz    dira, I2C_SDA_Bit        'Set SDA as output, data set to 0 at init
                        call    #I2C_BitDelay
I2C_SDA_LOW_ret         ret
'                        
'
'Set SDA to high, also SDA is input
I2C_SDA_HIGH            cmp     t1, t1          wz       'Make Z flag = 1
                        'make sure SCL = 0 at eeprom
                        muxnz   dira, I2C_SDA_Bit        'Set SDA as input, pull up resistor forces = 1 at eeprom
                        call    #I2C_BitDelay
I2C_SDA_HIGH_ret        ret
'                        
'
'Perform a I2C start bit, SDA goes low while SCL is high
I2C_StartBit            call    #I2C_SCL_LOW
                        call    #I2C_SDA_HIGH
                        'SCL = 0, SDA = output = 1 at eeprom
                        call    #I2C_SCL_HIGH
                        call    #I2C_SDA_LOW
                        'Next set SCL = 0, then wait
                        call    #I2C_SCL_LOW
                        'Finally bring SDA = 1
                        call    #I2C_SDA_HIGH
                        'we alway exit with SCL = 0, SDA = 1
I2C_StartBit_ret        ret
'                        
'
'Perform a I2C stop bit, SDA goes low while SCL is high
I2C_StopBit             call    #I2C_SCL_LOW
                        call    #I2C_SDA_LOW
                        'SCL = 0, SDA = output = 0 at eeprom
                        call    #I2C_SCL_HIGH
                        call    #I2C_SDA_HIGH
                        'Next set SCL = 0, then wait
                        call    #I2C_SCL_LOW
                        'we alway exit with SCL = 0, SDA = 1
I2C_StopBit_ret         ret
'                        
'
'Software reset I2C eeprom
I2C_Reset               call    #I2C_StartBit
                        'Pulse SCL 9 times with SDA = 1
                        mov     t1, #9
I2C_ResetRpt            call    #I2C_SCL_HIGH
                        call    #I2C_SCL_LOW
                        djnz    t1, #I2C_ResetRpt
                        'Perform another start bit
                        call    #I2C_StartBit
                        'Perform stop bit
                        call    #I2C_StopBit
                        'we alway exit with SCL = 0, SDA = 1
I2C_Reset_ret           ret
'                        
'
'Read a byte from the I2C buss.
'On entry T2 = 0 to write ack = 0, T2 <> 0 ack = 1
'On exit t1 = data byte. 
I2C_ReadByte            mov     t1, #0
                        call    #I2C_SCL_LOW
                        call    #I2C_SDA_HIGH   'Set SDA = input                                                        
                        mov     t3, #8          'Need to receive 8 bit plus send ack/nak
I2C_ReadByteRpt         shl     t1, #1          'Shift bits ready for next msb of byte
                        'now set SCL high then low to clock data bit
                        call    #I2C_SCL_HIGH
                        'read data bit
                        mov     t4, ina
                        and     t4, I2C_SDA_Bit wz
              if_nz     or      t1, #1                        
                        'Set SCL = 0
                        call    #I2C_SCL_LOW
                        djnz    t3, #I2C_ReadByteRpt
                        'clocked in our eight bits, 
                        'now clock out our ack bit
                        mov     t2, t2          wz      'see if ack is 1 or 0
              if_z      call    #I2C_SDA_LOW                                                           
                        mov     t2, t2          wz      'see if ack is 1 or 0
              if_nz     call    #I2C_SDA_HIGH
                        'Clock SCL to send ack/nak         
                        call    #I2C_SCL_HIGH
                        call    #I2C_SCL_LOW
                        'we alway exit with SCL = 0, SDA = 1
                        call    #I2C_SDA_HIGH
I2C_ReadByte_ret        ret
'
'
'Write a byte to the I2C buss, MSb first.
'After byte is sent eeprom returns ack/nak on 9th clock.
'On entry T1 = data byte to write.
'On exit t2 = 0 for ack = 0 or <> 0 for ack bit = 1 
I2C_WriteByte           shl     t1, #24         'move data byte into MSB of T1
                        mov     t2, #8          'number of bits to write to eeprom
I2C_WriteByteRpt        shl     t1, #1    wc    'Move next bit into carry flag
              if_c      call    #I2C_SDA_HIGH                                                           
              if_nc     call    #I2C_SDA_LOW
                        'now set SCL high then low to clock data bit
                        call    #I2C_SCL_HIGH
                        call    #I2C_SCL_LOW
                        djnz    t2, #I2C_WriteByteRpt
                        'clocked out our eight bits, set SDA as input to read act
                        call    #I2C_SDA_HIGH
                        call    #I2C_SCL_HIGH
                        mov     EEpromResp, ina
                        and     EEpromResp, I2C_SDA_Bit
                        'we alway exit with SCL = 0, SDA = 1
                        call    #I2C_SCL_LOW
I2C_WriteByte_ret       ret
'
'
'Local variables for I2C read/write to global memory functions.
EEpromAdr     long      0
MemAdr        long      0
CopyCnt       long      0
EEpromResp    long      0
'
'
'Set the eeprom start address for a write, also use for read
'On Entry: T1 = eeprom start address, T2 = global memory address
'T3 = byte count to copy.
I2C_StartWrite          mov     EEpromResp, #0                                  'Indicate a valid read
                        mov     MemAdr, t2
                        mov     CopyCnt, t3     wz
              if_z      jmp     #I2C_StartWrite_ret                             'if nothing to copy just quit
                        'bit 16 of address goes in first command byte B[1]
                        mov     EEpromAdr, #c_EEpromAdrTx
                        test    t1, Bit16Mask   wz
              if_nz     or      EEpromAdr, #2
                        'add in remaing 16 bits of address
                        shl     EEpromAdr, #16
                        and     t1, DataWordMask
                        or      EEpromAdr, t1
                        'First send a start bit
                        call    #I2C_StartBit
                        'next send a write command
                        mov     t1, EEpromAdr
                        shr     t1, #16
                        call    #I2C_WriteByte
                        tjnz    EEpromResp, #I2C_StartWrite_ret
                        'Send the MSB for 16 bit address
                        mov     t1, EEpromAdr
                        shr     t1, #8
                        call    #I2C_WriteByte
                        tjnz    EEpromResp, #I2C_StartWrite_ret
                        'Send the LSB for 16 bit address
                        mov     t1, EEpromAdr
                        call    #I2C_WriteByte
I2C_StartWrite_ret      ret
'
'
'Copy a region from the eeprom to global memoru
'On Entry: T1 = eeprom start address, T2 = global memory address
'T3 = byte count to copy.
'On Exit: T1 = bytes copied
I2C_CopyToMem           call    #I2C_StartWrite                                         
                        tjnz    EEpromResp, #I2C_CopyToMem_ret                  'if EEpromResp <> 0, no responce from EEprom
                        'Send a start bit to exit write mode
                        call    #I2C_StartBit
                        'finally send command to read
                        mov     t1, EEpromAdr
                        shr     t1, #16
                        or      t1, #1                  'make it a read command (Cmd[0] = 1)
                        call    #I2C_WriteByte
                        tjnz    EEpromResp, #I2C_CopyToMem_ret
                        'now read the bytes up to count -1
                        sub     CopyCnt, #1     wz
              if_z      jmp     #I2C_CopyToMemLast
I2C_CopyToMemNext       mov     t2, #0          'Send ACK = 0
                        call    #I2C_ReadByte
                        wrbyte  t1, MemAdr
                        add     MemAdr, #1
                        djnz    CopyCnt, #I2C_CopyToMemNext
                        'Read all the bytes except one, needs a nak
I2C_CopyToMemLast       mov     t2, #1           'Send NAK = 1
                        call    #I2C_ReadByte
                        wrbyte  t1, MemAdr
                        call    #I2C_StopBit
I2C_CopyToMem_ret       ret
'
'
'Wait for eeprom to ready after a write page, normaly < 5 Msec
I2C_WriteReady          call    #I2C_StartBit
                        mov     t1, #c_EEpromAdrTx                              'Actual address does not matter only device select bits
                        call    #I2C_WriteByte
                        call    #I2C_StopBit
I2C_WriteReady_ret      ret
'
' 
'Write data to a page on the eeprom
'On Entry: T1 = eeprom start address, T2 = global memory address
'T3 = byte count to copy.
I2C_WritePage           call    #I2C_StartWrite                                         
                        tjnz    EEpromResp, #I2C_WritePage_ret                  'if EEpromResp <> 0, no responce from EEprom
                        'now write the page data
I2C_WritePageNext       rdbyte  t1, MemAdr
                        call    #I2C_WriteByte
                        tjnz    EEpromResp, #I2C_WritePage_ret
                        add     MemAdr, #1
                        djnz    CopyCnt, #I2C_WritePageNext
                        call    #I2C_StopBit
I2C_WritePage_ret       ret
'
'
'
'See if we have a valid I2C command. If we do execute the command.
'The I2CCmdAdr contains the address of the long holding
'the I2C command to perform. The next three longs contain;
'EEprom address, Global memory address, and Byte count.
'Commands:
'       0 - no command
'       1 - Perform EEprom software reset
'       2 - Test if EEprom ready
'       3 - Copy EEprom to global memory
'       4 - Copy global memory to EEProm page
'    
I2C_ChkDoCmd            rdlong  t6, I2C_CmdAdr   wz
              if_z      jmp     #I2C_ChkDoCmd_ret
                        'We have a command see if it is valid
                        cmp     t6, #5          wc
              if_nc     jmp     #I2C_ChkDoCmd_Done
                        'We hace a valid command, read params
                        mov     t5, I2C_CmdParamsAdr
                        rdlong  t1, t5
                        add     t5, #4
                        rdlong  t2, t5
                        add     t5, #4
                        rdlong  t3, t5
                        'execute correct function
                        shl     t6, #1          'Address table is in words
                        add     t6, I2C_ChkDoCmdTableAdr
                        rdword  t6, t6
                        tjnz    t6, t6
                        'All done return result...
                        'Place command result into first param then clear command                         
I2C_ChkDoCmd_Done       wrlong  EEpromResp, I2C_CmdParamsAdr
                        'clear the command register to indicate we are done
                        mov     t6, #0
                        wrlong  t6, I2C_CmdAdr                      
I2C_ChkDoCmd_ret        ret
'                       'I2C base functions
I2C_ChkDo_Reset         call    #I2C_Reset
                        jmp     #I2C_ChkDoCmd_Done
'                        
I2C_ChkDo_Ready         call    #I2C_WriteReady                        
                        jmp     #I2C_ChkDoCmd_Done
'                        
I2C_ChkDo_Read          call    #I2C_CopyToMem                    
                        jmp     #I2C_ChkDoCmd_Done
'                        
I2C_ChkDo_Write         call    #I2C_WritePage                    
                        jmp     #I2C_ChkDoCmd_Done
'
'
I2C_SCL_Bit             long    1  << c_EEpromSCLBit
I2C_SDA_Bit             long    1  << c_EEpromSDABit
I2C_CmdAdr              long    0
I2C_CmdParamsAdr        long    0
I2C_ChkDoCmdTableAdr    long    @ChkDoCmdTable
'
D0                      long    1024
Bit16Mask               long    1 << 16
DataWordMask            long    $FFFF
KbdValidDataFlg         long    1 << 9
'byte to decimal constants
BtoD0                   long    $00000001
BtoD1                   long    $00000002
BtoD2                   long    $00000004
BtoD3                   long    $00000008
BtoD4                   long    $00000106
BtoD5                   long    $00000302
BtoD6                   long    $00000604
BtoD7                   long    $00010208
'constants used for byte to decimal
D2Sub10                 long    $00000A00
D2Carry1                long    $00010000
BtoDAsciOffset          long    $00303030
'
'
'Variables initialized by start up code
'
CmdTableAdr             long    0
'Address of first command parameter
CmdParamAdr             long    0
'RS232 RX Fifo Variable Addresses
ComRxHeadAdr            long    0
ComRxTailAdr            long    0
ComRxBufAdr             long    0
'RS232 RX Fifo Variable Addresses
ComTxHeadAdr            long    0
ComTxTailAdr            long    0
ComTxBufAdr             long    0
'Local RX Fifo Variable Addresses
LocalRxHeadAdr          long    0
LocalRxTailAdr          long    0
LocalRxBufAdr           long    0
'KBD TX Fifo Variable Addresses
KbdTxHeadAdr            long    0
KbdTxTailAdr            long    0
KbdTxBufAdr             long    0
'Mode variable for com settings
ComModeAdr              long    0
'Address of font glyph table in memory
FontTableAdr            long    0
'Address of column tab stops in global memory
TabStopsAdr             long    0
'Address of VGA cursor
CursorXAdr              long    0
'Number of screen columns
ScrnCols                long    0
'EEprom font save start address
EEpromFontTableAdr      long    0
'Size of font image in eeprom
EEpromFontByteSize      long    0
'
'Uninitialized data
KbpPreReadData          res     1
KbpPreReadState         res     1
'
t1                      res     1
t2                      res     1
t3                      res     1
t4                      res     1
t5                      res     1
t6                      res     1
'Temp variables for fifo read/write
TmpHead                 res     1
TmpTail                 res     1
TmpAdr                  res     1
TmpNextHead             res     1
'Shadow value of command paramter
LongParam               res     1
'
' ******** END OF COG CODE ********
'
' Various lookup tables in global memory
'
'Jump table for I2C function calls
ChkDoCmdTable           word
word          0
word          (@I2C_ChkDo_Reset - @entry) >> 2
word          (@I2C_ChkDo_Ready - @entry) >> 2
word          (@I2C_ChkDo_Read - @entry) >> 2
word          (@I2C_ChkDo_Write - @entry) >> 2
'
'EEprom address for each of the saved font.
'Fonts are in first 64K so 16 bit address is fine.
EEpromFontStartTable
'Font 0 - default font
word    c_EEpromFontStartAdr
'Font 1 - custom font 1
word    c_EEpromFontStartAdr + c_EEpromFontSaveSize
'Font 2 - custom font 2
word    c_EEpromFontStartAdr + (c_EEpromFontSaveSize * 2)
'Font 3 - custom font 3
word    c_EEpromFontStartAdr + (c_EEpromFontSaveSize * 3)
'
'    
'Command jmp table for command parser,
'must have c_CommandCount entries
aCmdJmpTable            word

'c_NoCommand       = 0
word          0
'c_TermReadByte    = 1           Data is returned in the first global paramater
word          (@DoTermRead - @entry) >> 2
'c_TermSendAck     = 2           no paramaters
word          (@DoSendAck - @entry) >> 2
'c_TermSendXY      = 3           Paramater 0 contains address to global Cursor buffer, 2 bytes X, Y zero based
word          (@DoSendCursorXY - @entry) >> 2
'c_TermUpdateFont  = 4           14 params contain glyph index and scanlines, or font index to load
word          (@DoFontCommand - @entry) >> 2
'c_TermSetTab      = 5           Set the tab stop for the current column. 
word          (@DoSetTab - @entry) >> 2
'c_TermClearTab    = 6           Clear the tab stop for the current column.
word          (@DoClearTab - @entry) >> 2
'c_TermClearTabs   = 7           Clear all set tab stops, no paramaters.
word          (@DoClearTabs - @entry) >> 2
'c_TermNextTab     = 8           Get next tab stop, On exit: Param[0] = new column
word          (@DoNextTab - @entry) >> 2
'c_TermPriorTab    = 9           Get prior tab stop, On exit: Param[0] = new column
word          (@DoPriorTab - @entry) >> 2
