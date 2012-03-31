''********************************************
''*  PropTerm FTDI Parallel USB Driver v1.0  *
''********************************************
''
''This object implements a a USB interface for the ANSI terminal.
''The USB interface uses an FTDI XXXXX parallel FIFO USB device.
''
''This object maintains two fifos in global memory to allow for other cogs
''to read/write data to/from the USB interface.
''  
con
  c_RxFifoSize  = 128           'bytes in the rx fifo, must be power of 2 that is >= 8
  c_TxFifoSize  =  16           'bytes in the tx fifo, must be power of 2 that is >= 2
  
VAR

  long  cog                     'cog flag/id
  '12 contiguous longs
  'first 4 longs are fifo variables
  long  rx_head                 '
  long  rx_tail
  long  tx_head
  long  tx_tail
  'next the bit masks for our I/O pins
  long  tx_enabled_mask
  long  rx_ready_mask
  long  rd_mask
  long  wr_mask
  long  data_mask
  long  data_shift
  long  buffer_ptr
  'rx/tx fifo data buffers
  byte  rx_buffer[c_RxFifoSize] 'receive buffer
  byte  tx_buffer[c_TxFifoSize] 'transmit buffer

PUB Start(aLsDataPin, aTxEPin, aRxFPin, aRdPin, aWrPin) : okay
''Start serial driver - starts a cog
''returns false if no cog available
  Stop
  'initalize RX/TX fifos to empty state 
  longfill(@rx_head, 0, 4)
  'Initalize RX FIFO data start address
  buffer_ptr := @rx_buffer
  'Calc hardware pind bit masks
  tx_enabled_mask := CalcBitMask(aTxEPin)
  rx_ready_mask := CalcBitMask(aRxFPin)
  rd_mask := CalcBitMask(aRdPin)
  wr_mask := CalcBitMask(aWrPin)
  data_shift := aLsDataPin
  data_mask := $FF << aLsDataPin
  'Start COG
  okay := cog := cognew(@entry, @rx_head) + 1

PRI CalcBitMask(Bit) : Mask
'Calculate a long mask with the bit number "Bit" set to one,  all other bits zero
  Mask := 1 << Bit
  
PUB Stop
''Stop USB driver if cog variable <> 0 - frees a cog
  if cog
    cogstop(cog~ - 1)

PUB ChangeDataFormat(aBaudRate, aDataBits, aParity)
''Change RS232 data rate and/or format, RX and TX
''Has no effect, stub to making changing interfaces (RS232/USB) simple
  aDataBits:= aParity
  
PUB GetRxHeadAdr : Address
''Return memory address of receive fifo head variable
  Address:= @rx_head
  
PUB GetRxTailAdr : Address
''Return memory address of receive fifo tail variable
  Address:= @rx_Tail
  
PUB GetRxBufAdr : Address
''Return memory address of receive fifo data byte array
  Address:= @rx_Buffer
  
PUB GetTxHeadAdr : Address
''Return memory address of transmit fifo head variable
  Address:= @tx_head
  
PUB GetTxTailAdr : Address
''Return memory address of transmit fifo tail variable
  Address:= @tx_Tail
  
PUB GetTxBufAdr : Address
''Return memory address of transmit fifo data byte array
  Address:= @tx_Buffer

DAT

'**********************************************************
'* Assembly language code for FTDI parallel FIFO USB port *
'**********************************************************
'
                        org
entry                   mov     t1, par                 'get structure address
                        add     t1, #4 << 2             'skip past heads and tails
                        mov     t2, #7                  'Number of longs to move
copyvars                rdlong  TxEnabledMask, t1       'Copy next variable to cog memory
                        add     copyvars, D0
                        add     t1, #4
                        djnz    t2, #CopyVars
                        {
                        'get tx enabled pin mask setting
                        rdlong  TxEnabledMask, t1
                        'read rx data ready pin mask
                        add     t1, #4                  
                        rdlong  RxReadyMask, t1
                        'now for the Rd pin mask setting
                        add     t1, #4                  
                        rdlong  RdMask, t1             
                        'next the WR pin mask settings
                        add     t1, #4                  
                        rdlong  WrMask, t1             
                        'get data bits pin mask
                        add     t1, #4                  
                        rdlong  DataMask, t1
                        'get data bits LSb position
                        add     t1, #4                  
                        rdlong  DataShift, t1
                        'calc our tx/rx data buffer addresses
                        add     t1, #4                  'get buffer_ptr
                        rdlong  RxBuffAdr, t1
                        }
                        mov     TxBuffAdr, RxBuffAdr    'rx buffer is first
                        add     TxBuffAdr, #c_RxFifoSize'tx starts c_RxFifoSize bytes later
                        'We have initialize all variables, now initalize I/O pins
                        cmp     t1, t1          wz      'Set cog Zero flag to 1
                        muxnz   dira, TxEnabledMask     'Set Tx enabled pin as Input
                        muxnz   dira, RxReadyMask       'Set Rx ready pin as Input
                        muxz    outa, RdMask            'Set FIFO Rd pin to 1
                        muxz    dira, RdMask            'Set FIFO Rd pin as Output
                        muxz    outa, WrMask            'Set FIFO Wr pin to 1
                        muxz    dira, WrMask            'Set FIFO Wr pin as Output
                        muxnz   dira, DataMask          'Set all Data bit pins as Input
                        'Setup fifo addresses and read shadow values.
                        'Read current RxHead to shadow RXHead
                        mov     RxHeadAdr, par          'read address of rx head
                        rdlong  SdwRxHead, RxHeadAdr    'SdwRxHead = rx head
                        'Calc RxTail address
                        mov     RxTailAdr, RxHeadAdr
                        add     RxTailAdr, #4
                        'Calc TxHead address 
                        mov     TxHeadAdr, RxTailAdr
                        add     TxHeadAdr, #4 
                        'Calc TxTail address 
                        mov     TxTailAdr, TxHeadAdr
                        add     TxTailAdr, #4 
                        rdlong  SdwTxTail, TxTailAdr    'SdwTxTail = tx tail
                        'All ready to so start moving data
MainLoop                test    RxReadyMask, ina   wz    'Test if we have USB FIFO data to read
              if_nz     jmp     #DoSend                 'if Rx ready is not = 0 no data to read
                        'We have data at the USB ready to read, see if local RX FIFO has room.
                        'Check for, fifo full, head + 1 = tail
                        mov     t3, SdwRxHead           'T3 = rx head
                        add     t3, #1                  'T3 = head + 1, still need to wrap
                        cmp     t3, #c_RxFifoSize wz    'T3 = head +1 mod c_RxFifoSize, in case head wrapped
              if_z      mov     t3, #0                        
                        rdlong  t2, RxTailAdr           'T2 = rx tail
                        cmp     t2, t3            wz    'if head + 1 = tail, fifo full
              if_z      jmp     #DoSend                 'if Rx FIFO full do not read USB, try a write
                        'USB has data and we have room so read USB and place data in local RX FIFO
                        'First read USB data
                        cmp     t1, t1            wz    'Set cog Zero flag to 1
                        muxnz   outa, RdMask            'Set USB FIFO RD pin to 0
                        nop                             'Wait for data from USB device
                        mov     t1, ina                 'Read data bits
                        shr     t1, DataShift           'Move data to low byte
                        muxz    outa, RdMask            'Set USB FIFO RD pin to 1
                        'THIS IS A FIX, required if USB data pins are reversed.
                        rev     t1, #32-8               'Invert the LSB data bits (D0=D7, D1=D6...D7=D0)
                        'END OF FIX
                        'T1 = data we read now write to local RX FIFO.
                        'Save received byte to fifo RxHead offset and then inc RxHead
                        mov     t2, SdwRxHead           'T2 = current rxHead value
                        add     t2, RxBuffAdr           'Add rxBuffer global memory address
                        wrbyte  t1, t2                  'Save the new data to the fifo
                        mov     SdwRxHead, t3           'T3 = RXxHead + 1, Point head to next location
                        wrlong  SdwRxHead, RxHeadAdr    'Update the rx FIFO Head in global memory
                        '
                        'End of receive code now for transmit code.
                        '
                        'First see if TX FIFO is full
DoSend                  test    TxEnabledMask, ina  wz  'Test if we have room for a byte in USB TX FIFO
              if_nz     jmp     #MainLoop               'if Tx enabled is not = 0 no room in FIFO                        
                        'USB has room fo a byte see if Local Tx FIFO has a byte to be sent
                        rdlong  t2,TxHeadAdr            'T2 = txHead
                        cmp     t2, SdwTxTail   wz      'see if Head = Tail, indicates nothing to send
              if_z      jmp     #MainLoop
                        'Local TX FIFO has a byte to send and USB is ready to accept it.
                        'Read byte from local tx fifo first
                        'There is at least one byte to read
                        mov     t3, SdwTxTail           'Read the tx tail from our cache
                        add     t3, TxBuffAdr           'Calc address of next fifo byte to read
                        rdbyte  t1, t3                  'T1 = next byte to send
                        'Data in T1, now inc tx tail
                        add     SdwTxTail, #1                 'point to next fifo location
                        cmp     SdwTxTail, #c_TxFifoSize wz   'account for possible wrap
              if_z      mov     SdwTxTail, #0                 'point to first fifo buffer location                        
                        wrlong  SdwTxTail, TxTailAdr          'save tx tail to global memory
                       'Data byte read from local tx fifo, now write to USB FIFO
                       'First set data on USB data pins.
                        cmp     t1, t1          wz      'Set cog Zero flag to 1
                        'THIS IS A FIX, required if USB data pins are reversed.
                        rev     t1, #32-8               'Invert the LSB data bits (D0=D7, D1=D6...D7=D0)
                        'END OF FIX
                        'Set data pins to correct values. 
                        shl     t1, DataShift
                        'Add 8 data bits to cog output pin register
                        mov     t2, outa
                        andn    t2, DataMask            'Set all USB data bit pins to zero.
                        or      t2, t1                  'Add in our data bits that are high.
                        mov     outa, t2                'Set cog output register to correct data bit values
                        muxz    dira, DataMask          'Set all USB data bit pins as Output.
                        nop                             'Wait for data to set up time
                       'Now set USB WR pin low
                        muxnz   outa, WrMask            'Set USB FIFO WR pin to 0
                        'Data placed on USB data pins and WR is low 
                        nop                             'wait for USB device
                        muxz    outa, WrMask            'Set USB FIFO WR pin to 1
                        muxnz   dira, DataMask          'Set all USB data bit pins as Input.
                        jmp     #MainLoop
'                        
'
D0                      long    (1 << 9)
' Uninitialized data
'
SdwRxHead               res     1         'Only this cog updates rxHead so we keep a shadow copy for speed
SdwTxTail               res     1         'Only this cog updates txTail so we keep a shadow copy for speed
'
'Addresses to speed reading of global data
RxHeadAdr               res     1
RxTailAdr               res     1
TxHeadAdr               res     1
TxTailAdr               res     1
'
'Temp use variables
t1                      res     1
t2                      res     1
t3                      res     1
'
'I/O masks for pins, these must stay in same order as in VAR section 
TxEnabledMask           res     1
RxReadyMask             res     1
RdMask                  res     1
WrMask                  res     1
DataMask                res     1
DataShift               res     1
RxBuffAdr               res     1
TxBuffAdr               res     1
