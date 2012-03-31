''********************************
''*  PropTerm RS232 Driver v1.0  *
''********************************
''
''This object is a modified version of the "FullDuplexSerial" object
''by Chip Gracey of Parallax, Inc. As such terms of use remain the
''same as the original object code terms of use.
''
''This object implements a full duplex RS232 interface for the ANSI terminal.
''The RS232 interface supports flow control using the RTS/CTS control lines.
''The object supports baud rates up to 115200 baud. Data in 5..8 bit format is
''supported with no parity. even or odd parity. Currently only one stop
''bit is supported.
''
''This object maintains two fifos in global memory to allow for other cogs
''to read/write data to/from the RS232 interface.
''  
con
  c_RxFifoSize  = 128           'bytes in the rx fifo, must be power of 2 that is >= 8
  c_RxFullLevel =  16           'rx fifo min free space, used to toggle rts pin
  c_TxFifoSize  =  16           'bytes in the tx fifo, must be power of 2 that is >= 2
  'Com port parity settings
  c_ParityNone   = 0
  c_ParityOdd    = 1
  c_ParityEven   = 2
  
VAR

  long  cog                     'cog flag/id
  '12 contiguous longs
  'first 4 longs are fifo variables
  long  rx_head                 '
  long  rx_tail
  long  tx_head
  long  tx_tail
  'next the bit masks for our I/O pins
  long  rx_mask
  long  tx_mask
  long  rts_mask
  long  cts_mask
  'used to generate baud rate
  long  bit_ticks
  long  buffer_ptr
  'rs232 data format settings
  long  data_bits
  long  parity_mode
  'rx/tx fifo data buffers
  byte  rx_buffer[c_RxFifoSize] 'receive buffer
  byte  tx_buffer[c_TxFifoSize] 'transmit buffer

PUB Start(aRxPin, aTxPin, aRtsPin, aCtsPin, aBaudRate, aDataBits, aParity) : okay
''Start serial driver - starts a cog
''returns false if no cog available
  Stop
  longfill(@rx_head, 0, 4)

  rx_mask := CalcBitMask(aRxPin)
  tx_mask := CalcBitMask(aTxPin)
  rts_mask := CalcBitMask(aRtsPin)
  cts_mask := CalcBitMask(aCtsPin)
  
'  longmove(@rx_pin, @aRxPin, 4)
  buffer_ptr := @rx_buffer
  SetParams(aBaudRate, aDataBits, aParity)
  okay := cog := cognew(@entry, @rx_head) + 1

PRI CalcBitMask(Bit) : Mask
'Calculate a long mask with the bit number "Bit" set to one,  all other bits zero
  Mask := 1 << Bit
  
PRI SetParams(aBaudRate, aDataBits, aParity)
'Update and range check RS232 parameters
  bit_ticks := clkfreq / aBaudRate
  if (aDataBits > 8)
    data_bits := 8
  else  
    data_bits := aDataBits
  parity_mode := aParity & 3

PUB Stop
''Stop serial driver if cog variable <> 0 - frees a cog
  if cog
    cogstop(cog~ - 1)

PUB ChangeDataFormat(aBaudRate, aDataBits, aParity)
''Change RS232 data rate and/or format, RX and TX
''Fifos are not cleared. I/O pin settings are preserved.
  'If the cog is running,
  ' 1) stop cog
  ' 2) apply new settings
  ' 3) resart with new settings
  if cog
    Stop
    SetParams(aBaudRate, aDataBits, aParity)
    cog := cognew(@entry, @rx_head) + 1
  
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

'***********************************
'* Assembly language serial driver *
'***********************************
                        org
'Start of cog assembly code for serial port
'
entry                   mov     t1, par                 'get structure address
                        add     t1, #4 << 2             'skip past heads and tails
                        'get rx pin setting and create a bit mask for that pin
                        rdlong  rxmask, t1
                        {
                        rdlong  t2, t1                  'get receive pin mask
                        mov     rxmask, #1
                        shl     rxmask, t2
                        }
                        'read tx pin setting and create pin mask
                        add     t1, #4                  'get transmit pin mask
                        rdlong  txmask, t1
                        {
                        rdlong  t2, t1
                        mov     txmask, #1
                        shl     txmask, t2
                        }
                        'now for the Rts pin setting
                        add     t1, #4                  'point to the rts_pin mask
                        rdlong  rtsmask, t1             'read rts pin mask from global memory
                        {
                        rdlong  t2, t1                  'read rts pin number from global memory
                        mov     rtsmask, #1             'set bit in rts mask to match pin number
                        shl     rtsmask, t2
                        }
                        'next the CTS pin settings
                        add     t1, #4                  'point to the cts_pin mask
                        rdlong  ctsmask, t1             'read rts pin mask from global memory
                        {
                        rdlong  t2, t1                  'read rts pin number from global memory
                        mov     ctsmask, #1             'set bit in cts mask to match pin number
                        shl     ctsmask, t2
                        }
                        'bitticks is calculated in spin to make coding easier
                        add     t1, #4                  'get bit_ticks
                        rdlong  bitticks, t1
                        'calc our tx/rx data buffer addresses
                        add     t1, #4                  'get buffer_ptr
                        rdlong  rxbuff, t1
                        mov     txbuff, rxbuff          'rx buffer is first
                        add     txbuff, #c_RxFifoSize   'tx starts c_RxFifoSize bytes later
                        'Read number of data bits per word (5,7,8)
                        add     t1, #4
                        rdlong  databits, t1
                        'Set data mask based on data bits
                        mov     datamask, #$FF
                        mov     t2, #8
                        sub     t2, databits    wc, wz
        if_nz_and_nc    shr     datamask, t2                        
                        'Read Parity mode
                        add     t1, #4
                        rdlong  paritymode, t1
                        'copy current rxHead to shadow RXHead
                        mov     RxHeadAdr, par          'read address of rx head
                        rdlong  SdwRxHead, RxHeadAdr    'SdwRxHead = rx head
                        mov     RxTailAdr, RxHeadAdr
                        add     RxTailAdr, #4 
                        mov     TxHeadAdr, RxTailAdr
                        add     TxHeadAdr, #4 
                        mov     TxTailAdr, TxHeadAdr
                        add     TxTailAdr, #4 
                        rdlong  SdwTxTail, TxTailAdr    'SdwTxTail = tx tail
                        mov     txcode, #transmit       'initialize ping-pong multitasking
                        'configure tx data pin for output then set high
                        or      outa, txmask            'set tx pin high
                        or      dira, txmask            'set tx pin as an output
                        'set RTS pin as an output, set pin low to allow data transfer
                        test    rtsmask, #0     wz      'set  Z = 1
                        muxnz   outa, rtsmask           'set RTS pin low
                        or      dira, rtsmask           'set RTS pin as output
'
'
' Receive
'
receive                 jmpret  rxcode,txcode         'run a chunk of transmit code, then return
                        'Set RTS to low if fifo is not full
                        rdlong  t2, RxTailAdr           'T2 = rx tail
                        sub     t2, SdwRxHead   wc, wz  'T2 = tail - head
                        'If Head >= tail
        if_c_or_z       add     t2, #c_RxFifoSize       'head < tail, Count=Tail - Head
                        'See if enough space to allow reception
                        cmp     t2, #c_RxFullLevel wc
                        muxc    outa, rtsmask           'Set RTS, if we have >= c_RxFullLevel bytes of space set low else set high
                        '
                        'Check for start bit, rx pin low
                        test    rxmask, ina     wz
        if_nz           jmp     #receive              'not ready to receive  
                        'we have a start bit, get ready to receive byte
'                        rdlong  bitticks, BitTicksAdr 'Read our bit rate               
                        mov     rxbits, databits      'number of bits in a data word (should be 5,7,8)
                        add     rxbits, #1            'we need data bits + 1 with stop bit included
                        'if parity is on, we need to receive an extra bit
                        cmp     paritymode, #0  wz
        if_nz           add     rxbits, #1            'odd or even parity will be received with data.                                        
                        {
                        mov     rxbits, #9            'we need 9 bits with stop bit
                        }
                        mov     rxcnt, bitticks       'set count to one full bit time  
                        shr     rxcnt,#1              'divide by 2 to put us in the middle of bit
                        add     rxcnt,cnt             'calc system start time            
                        'receive next bit
:bit                    add     rxcnt,bitticks        'ready next bit period
:wait                   jmpret  rxcode,txcode         'run a chuck of transmit code, then return
                        'wait for bit time to expire
                        mov     t1, rxcnt             'check if bit receive period done
                        sub     t1, cnt
                        cmps    t1, #0          wc
        if_nc           jmp     #:wait
                        'bit time is up read next bit from rx pin
                        test    rxmask,ina      wc    'receive bit on rx pin
                        rcr     rxdata,#1
                        djnz    rxbits,#:bit
                        'we have all data bits plus a stop bit.
                        'The data is in the MSB byte need to shift it to LSB
                        'if parity is on, we need to remove parity bit from data
                        cmp     paritymode, #0  wz
        if_nz           shl     rxdata, #1              'odd or even parity was received with data remove it.                                        
                        mov     temp, #32-1             '32 register bits less one  
                        sub     temp, databits          'Cal how to shift data into LSB
                        shr     rxdata, temp
                        and     rxdata, datamask
                        {
                        shr     rxdata,#32-9          'justify and trim received byte
                        and     rxdata,#$FF
                        }
                       ' we have a byte write it to the fifo
                       'check for, fifo full, head + 1 = tail
                        mov     t3, SdwRxHead           'T3 = rx head
                        add     t3, #1                  'T3 = head + 1, still need to wrap
                        and     t3, #c_RxFifoSize - 1   'T3 = head +1 mod c_RxFifoSize, in case head wrapped
                        rdlong  t2, RxTailAdr           'T2 = rx tail
                        cmp     t2, t3          wz      'if head + 1 = tail, fifo full
'        if_z            muxz    outa, rtsmask           'set RTS pin high, fifo full
        if_z            jmp     #receive                'if full can not receive, dump char
                        'write the data byte to the fifo
                        'save received byte to rxHead and then inc rxHead
                        mov     t2, SdwRxHead         'T2 = current rxHead value
                        add     t2, rxbuff            'Add rxBuffer global memory address
                        wrbyte  rxdata, t2            'Save the new data to the fifo
                        mov     SdwRxHead, t3         'Point head to next location
                        wrlong  SdwRxHead, par        'Update the rxHead in global memory
                        jmp     #receive              'byte done, receive next byte
'
'
' Transmit
'
transmit                jmpret  txcode,rxcode           'run a chunk of receive code, then return
                        'if CTS is low we can send, else we are done
                        test    ctsmask, ina    wc      'Carry = CTS pin value
        if_nc           rdlong  t2,TxHeadAdr            'T2 = txHead
        if_nc           cmp     t2, SdwTxTail   wz      'see if Head = Tail, indicates nothing to end
        if_c_or_z       jmp     #transmit               'if no data to send do recieve code
                        'return to receive code and run a chunk
:txdorx1                jmpret  txcode,rxcode         'run a chunk of receive code, then return
                        'There is at least one byte to read
                        mov     t3, SdwTxTail           'Read the tx tail from our cache
                        add     t3, txbuff              'calc address of next fifo byte to read
                        rdbyte  txdata, t3              'txdata = next byte to send
                        'return to receive code and run a chunk
:txdorx2                jmpret  txcode,rxcode         'run a chunk of receive code, then return
                        'Data in txdata, now inc tx tail
                        add     SdwTxTail, #1                  'point to next fifo location
                        and     SdwTxTail, #c_TxFifoSize - 1   'account for possible wrap
                        wrlong  SdwTxTail, TxTailAdr           'save tx tail to global memory
                        'Data byte to transmit...
                        and     txdata, datamask
                        'Add a stop bit and possibly parity to our data
                        mov     temp, #1
                        or      txdata, txdata  wc      'Calc data parity in case we need it
                        cmp     paritymode, #0  wz
        if_nz           rcl     temp, #1                'Add in the parity                        
        if_nz           test    paritymode, #1  wz      'see if we want odd parity
        if_nz           xor     temp, #1                'flip parity bit
        

                        shl     temp, databits
                        or      txdata, temp
                        'Add the start bit and idle bit to the data
                        shl     txdata,#2
                        or      txdata,#1
                        'Cal number of bits to send
                        mov     txbits, databits
                        cmp     paritymode, #0  wz
        if_nz           add     txbits, #1              '1 extra bit for parity...                        
                        add     txbits, #3              '3 extra bits; start, stop, back to idle
                        
                        {
                        or      txdata,#$100            'ready byte to transmit
                        shl     txdata,#2
                        or      txdata,#1
                        mov     txbits,#11
                        }
                        mov     txcnt,cnt
                        'return to receive code and run a chunk
:txdorx3                jmpret  txcode,rxcode         'run a chunk of receive code, then return

:bit                    shr     txdata,#1       wc
                        muxc    outa,txmask        
                        add     txcnt,bitticks        'ready next cnt

:wait                   jmpret  txcode,rxcode         'run a chunk of receive code, then return

                        mov     t1,txcnt              'check if bit transmit period done
                        sub     t1,cnt
                        cmps    t1,#0           wc
        if_nc           jmp     #:wait

                        djnz    txbits,#:bit          'another bit to transmit?

                        jmp     #transmit             'byte done, transmit next byte
'
'
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
t1                      res     1
t2                      res     1
t3                      res     1
temp                    res     1
'
bitticks                res     1
databits                res     1
paritymode              res     1
datamask                res     1
'
rxbuff                  res     1
rxdata                  res     1
rxbits                  res     1
rxcnt                   res     1
rxcode                  res     1
'
txbuff                  res     1
txdata                  res     1
txbits                  res     1
txcnt                   res     1
txcode                  res     1
'
'I/O masks for pins
txmask                  res     1
rxmask                  res     1
rtsmask                 res     1
ctsmask                 res     1
