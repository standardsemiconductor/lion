    .equ LED_BASE  , 0x0
    .equ CR0       , 0x8
    .equ PWRR      , 0x1

    .equ UART_BASE , 0x4

    .equ SPI_BASE  , 0x8
#    .equ SPI_CR1   , 0x00000900
#    .equ SPI_CR2   , 0x00000A00
#    .equ SPI_BR    , 0x00000B00
#    .equ SPI_TXDR  , 0x00000D00
    .equ SPI_RXDR  , 0x00000E00
#    .equ SPI_CSR   , 0x00000F00
    .equ SPI_SR    , 0x00000C00

    .equ SPRAM_BASE, 0x20000
    .equ SPRAM_END,  0x40000

    .section .text
    .globl _start

_start:
    # set LED red
    li   a1, LED_BASE # set pointer LED_BASE
    li   a0, 0x0880   # byte1: cr0 address, byte0: enable bit
    sh   a0, (a1)     # send command

    li   a0, 0x01FF   # byte1: pwrr address, byte0: full on
    sh   a0, (a1)     # send command

    # wait until user presses any key
    jal  get_char

    # print Lion Soc title and copyright
    la   a0, name
    jal  put_str
    la   a0, copyright
    jal  put_str

    # choose (e)xisting, or upload (n)ew image?
1:  la   a0, image_prompt   # load image prompt
    jal  put_str            # print image prompt
    jal  get_char           # read response
    li   a1, 0x65           # ascii e
    li   a2, 0x6e           # ascii n
    beq  a0, a1, 7f         # branch to use existing image
    beq  a0, a2, 2f         # branch to upload new image
    j    1b                 # invalid answer ask again.

    # upload new image from uart into ram
2:  li   t0, SPRAM_BASE     # load SPRAM base
    li   t1, SPRAM_END      # load SPRAM end
3:  jal  get_char           # read byte
    sb   a0, (t0)           # store byte
    addi t0, t0, 1          # increment SPRAM address
    bne  t0, t1, 3b         # if SPRAM not full, get another byte

    # load ram into spi flash
    li   a0, SPI_BASE    # initialize spi, set SPI_BASE pointer

    # enable spi
4:  lbu  a1, 3(a0)       # read bus status
    bnez a1, 4b          # wait until bus idle
    li   a1, 0x00010980  # set WRITE mode, CR1 address, data
    sw   a1, (a0)        # send command

    # set clock div
5:  lbu  a1, 3(a0)       # read bus status
    bnez a1, 5b          # wait until bus idle
    li   a1, 0x00010B03  # set WRITE mode, BR address, data
    sw   a1, (a0)        # send command

    jal  transfer_start_spi # resume spi from deep power down
    li   a0, 0xAB           # load command
    jal  transfer_spi       # send command
    jal  transfer_end_spi   # end

    jal  write_enable       # chip erase
    jal  transfer_start_spi # start transfer
    li   a0, 0x60           # load chip erase opcode
    jal  transfer_spi       # send command
    jal  transfer_end_spi   # end

    li   t0, 0              # SPI flash address
    li   t1, SPRAM_BASE     # SPRAM base address
    li   t2, SPRAM_END      # SPRAM end address

6:  jal  wait_spi_busy      # wait while spi busy

    jal  write_enable       # byte program
    jal  transfer_start_spi # start transfer
    li   a0, 0x02           # load byte program opcode
    jal  transfer_spi       # transfer byte
    srli a0, t0, 16         # load 2 byte of SPI flash address
    jal  transfer_spi       # send 2 byte of SPI flash address
    srli a0, t0, 8          # load 1 byte of SPI flash address
    jal  transfer_spi       # send 1 byte of SPI flash address
    mv   a0, t0             # load 0 byte of SPI flash address
    jal  transfer_spi       # send 0 byte of SPI flash address
    lbu  a0, (t1)           # load data byte from SPRAM
    jal  transfer_spi       # send data byte from SPRAM
    jal  transfer_end_spi   # end transfer
    jal  wait_spi_busy      # wait while spi busy
    addi t0, t0, 1          # increment SPI flash byte address
    addi t1, t1, 1          # increment SPRAM byte address
    bne  t1, t2, 6b         # transfer another byte while SPI flash has space
    j    9f                 # jump to boot

    # load image from spi flash into ram
7:  li   t0, 0              # load SPI flash address
    li   t1, SPRAM_BASE     # load SPRAM base address
    li   t2, SPRAM_END      # load SPRAM end address

8:  jal  transfer_start_spi # start SPI flash transfer
    li   a0, 0x03           # load read array opcode
    jal  transfer_spi       # send read array opcode
    srli a0, t0, 16         # load 2 byte of SPI flash address
    jal  transfer_spi       # send 2 byte of SPI flash address
    srli a0, t0, 8          # load 1 byte of SPI flash address
    jal  transfer_spi       # send 1 byte of SPI flash address
    mv   a0, t0             # load 0 byte of SPI flash address
    jal  transfer_spi       # send 0 byte of SPI flash address
    li   a0, 0x00           # load dummy byte
    jal  transfer_spi       # send dummy byte, recv data byte
    sb   a0, (t1)           # store data byte in SPRAM address
    jal  transfer_end_spi   # end transfer
    addi t0, t0, 1          # increment SPI flash byte address
    addi t1, t1, 1          # increment SPRAM byte address
    bne  t1, t2, 8b         # read another byte while SPRAM has space

    # boot from ram
9:  li   a1, LED_BASE # set pointer LED_BASE
    li   a0, 0x0100   # byte1: pwrr addr, byte0: full off
    sh   a0, (a1)     # send command
    j    SPRAM_BASE   # boot image


transfer_start_spi:
    li   a0, SPI_BASE   # set SPI_BASE pointer
    
    # set CSN
1:  lbu  a1, 3(a0)      # read bus status
    bnez a1, 1b         # wait until bus idle
    li   a1, 0x00010F00 # set WRITE mode and CSR address
    sw   a1, (a0)       # send command

    # set spi master, cs hold
2:  lbu  a1, 3(a0)      # read bus status
    bnez a1, 2b         # wait until bus idle
    li   a1, 0x00010AC0 # set WRITE mode, CR2 address, data
    sw   a1, (a0)       # send command

    # wait trdy
3:  lbu  a1, 3(a0)      # read bus status
    bnez a1, 3b         # wait until bus idle
    li   a1, SPI_SR     # set READ mode SR address
    sw   a1, (a0)       # send command
4:  lbu  a1, 3(a0)      # read bus status
    bnez a1, 4b         # wait until bus idle
    lbu  a1, 2(a0)      # read bus receive
    andi a1, a1, 0x10   # mask trdy 0001_0000
    beqz a1, 3b         # loop until trdy

    ret


# data to send in a0[7..0]
# data received returned in a0[7..0]
transfer_spi:
    li   a1, SPI_BASE   # set SPI_BASE pointer
    
    # write txdr
1:  lbu  a2, 3(a1)      # read bus status
    bnez a2, 1b         # wait until bus idle
    li   a2, 0x00010D00 # set WRITE mode, TXDR address
    or   a2, a2, a0     # set data
    sw   a2, (a1)       # send command

    # wait rrdy
2:  lbu  a2, 3(a1)      # read bus status
    bnez a2, 2b         # wait until bus idle
    li   a2, SPI_SR     # set READ mode SR address
    sw   a2, (a1)       # send command
3:  lbu  a2, 3(a1)      # read bus status
    bnez a2, 3b         # wait until bus idle
    lbu  a2, 2(a1)      # read bus receive
    andi a2, a2, 0x08   # mask rrdy 0000_1000
    beqz a2, 2b         # loop until rrdy     ## ERROR LOOPING FOREVER ##

    # read rxdr
4:  lbu  a2, 3(a1)      # read bus status
    bnez a2, 4b         # wait until bus idle
    li   a2, SPI_RXDR   # set READ mode RXDR address
    sw   a2, (a1)       # send command
5:  lbu  a2, 3(a1)      # read bus status
    bnez a2, 5b         # wait until bus idle
    lbu  a0, 2(a1)      # read bus receive

    ret
  

transfer_end_spi:
    li   a0, SPI_BASE   # set SPI_BASE pointer

    # set csn
1:  lbu  a1, 3(a0)      # read bus status
    bnez a1, 1b         # wait until bus idle
    li   a1, 0x00010F0F # set WRITE mode, CSR address, data
    sw   a1, (a0)       # send command

    # unset cs hold
2:  lbu  a1, 3(a0)      # read bus status
    bnez a1, 2b         # wait until bus idle
    li   a1, 0x00010A80 # set WRITE mode, CR2 address, data
    sw   a1, (a0)       # send command

    # wait not tip
3:  lbu  a1, 3(a0)      # read bus status
    bnez a1, 3b         # wait until bus idle
    li   a1, 0x00000C00 # set READ mode, SR address
    sw   a1, (a0)       # send command
4:  lbu  a1, 3(a0)      # read bus status
    bnez a1, 4b         # wait until bus idle
    lbu  a1, 2(a0)      # read bus receive
    andi a1, a1, 0x80   # mask tip 1000_0000
    bnez a1, 3b         # loop if tip /= 0

    ret


write_enable:
    mv   s0, ra             # save return address
    jal  transfer_start_spi # start SPI flash transfer
    li   a0, 0x06           # load write enable opcode
    jal  transfer_spi       # send write enable opcode
    jal  transfer_end_spi   # end transfer
    mv   ra, s0             # restore return address
    ret


wait_spi_busy:
    mv   s0, ra             # save return address
1:  jal  transfer_start_spi # start transfer
    li   a0, 0x05           # load read status register byte 1 opcode
    jal  transfer_spi       # send read status command
    li   a0, 0x00           # load dummy byte
    jal  transfer_spi       # read status byte
    mv   a2, a0             # move status byte to a2 register
    jal  transfer_end_spi   # end transfer
    andi a2, a2, 0x01       # mask busy status bit
    bnez a2, 1b             # 0 = device ready, 1 = device busy: continue poll
    mv   ra, s0             # restore return address
    ret


get_char:
    li   a0, UART_BASE # set UART_BASE pointer
1:  lbu  a1, 0x2(a0)   # read status register
    andi a1, a1, 0x2   # mask receiver status
    bnez a1, 1b        # wait until receiver full
    lbu  a1, 0x1(a0)   # read receiver buffer
    mv   a0, a1        # return data in a0
    ret


put_str:
    li   a1, UART_BASE # set UART_BASE pointer
1:  lbu  a2, 0x2(a1)   # read status
    andi a2, a2, 0x1   # mask transmitter status
    bnez a2, 1b        # wait until transmitter empty
    lbu  a2, (a0)      # load str byte
    beqz a2, 2f        # if zero, end of string, ret
    sb   a2, (a1)      # transmit byte
    addi a0, a0, 0x1   # increment str pointer
    j    1b
2:  ret 

    .section .rodata
    .align 4

name:
    .string "\n\r   __   _             ____     _____\n\r  / /  (_)__  ___    / __/__  / ___/\n\r / /__/ / _ \\/ _ \\  _\\ \\/ _ \\/ /__  \n\r/____/_/\\___/_//_/ /___/\\___/\\___/  \n\r"

copyright:
    .string "\n\rStandard Semiconductor (c) 2022\n\r\n\r"

image_prompt:
     .string "(e)xisting or (n)ew image (e/n)? "

#success_str:
#    .string "SUCCESS\n\r"
#fail_str:
#    .string "FAIL\n\r"
