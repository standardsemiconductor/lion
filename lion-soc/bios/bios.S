    .equ LED_BASE  , 0x0
    .equ CR0       , 0x8
    .equ PWRR      , 0x1

    .equ UART_BASE , 0x4

    .equ SPI_BASE  , 0x8
    .equ SPI_CR1   , 0x00000900  
    .equ SPI_CR2   , 0x00000A00
    .equ SPI_BR    , 0x00000B00
    .equ SPI_TXDR  , 0x00000D00
    .equ SPI_RXDR  , 0x00000E00
    .equ SPI_CSR   , 0x00000F00
    .equ SPI_SR    , 0x00000C00

    .equ SPRAM_BASE, 0x20000
    .equ SPRAM_END,  0x40000

    .section .text
    .globl _start

_start:
    # set LED red
    jal  set_led_red

    # wait until user presses any key
    jal  get_char

    # print Lion Soc title and copyright
    la   a0, name
    jal  put_str
    la   a0, copyright
    jal  put_str

    # choose (e)xisting, or upload (n)ew image?
1:  la   a0, prompt # load prompt
    jal  put_str    # print prompt
    jal  get_char   # read response
    li   a1, 0x65   # ascii e
    li   a2, 0x6e   # ascii n
    beq  a0, a1, 5f # branch to use existing image
    beq  a0, a2, 2f # branch to upload new image
    j    1b         # invalid answer ask again.

    # upload new image from uart into ram
2:  la   a0, read_str   # print ready
    jal  put_str        # print ready

    li   t0, SPRAM_BASE # load SPRAM base
    li   t1, SPRAM_END  # load SPRAM end
3:  jal  get_char       # read byte
    sb   a0, (t0)       # store byte
    addi t0, t0, 1      # increment SPRAM address
    bne  t0, t1, 3b     # if SPRAM not full, get another byte

    # load ram into spi flash
    jal  init_spi           # initialize spi

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

4:  jal  wait_spi_busy      # wait while spi busy

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
    bne  t1, t2, 4b         # transfer another byte while SPI flash has space
    j    7f                 # jump to boot

    # load image from spi flash into ram
5:  li   t0, 0              # load SPI flash address
    li   t1, SPRAM_BASE     # load SPRAM base address
    li   t2, SPRAM_END      # load SPRAM end address

6:  jal  transfer_start_spi # start SPI flash transfer
    li   a0, 0x03           # load read array opcode
    jal  transfer_spi       # send read array opcode
    li   a0, t0, 16         # load 2 byte of SPI flash address
    jal  transfer_spi       # send 2 byte of SPI flash address
    li   a0, t0, 8          # load 1 byte of SPI flash address
    jal  transfer_spi       # send 1 byte of SPI flash address
    mv   a0, t0             # load 0 byte of SPI flash address
    jal  transfer_spi       # send 0 byte of SPI flash address
    li   a0, 0x00           # load dummy byte
    jal  transfer_spi       # send dummy byte, recv data byte
    sb   a0, (t1)           # store data byte in SPRAM address
    jal  transfer_end_spi   # end transfer
    addi t0, t0, 1          # increment SPI flash byte address
    addi t1, t1, 1          # increment SPRAM byte address
    bne  t1, t2, 6b         # read another byte while SPRAM has space

    # boot from ram
7:  la   a0, boot   # load boot prompt
    jal  put_str    # write boot prompt
    j    SPRAM_BASE # boot image

    # check SPRAM
#    la   a0, check_spram_str
#    jal  put_str
#    jal  check_spram
#    beqz a0, 1f
#    la   a0, fail_str
#    j    2f
#1:  la   a0, success_str
#2:  jal  put_str

    # check SPI Flash
#    la  a0, check_flash_str
#    jal put_str
#    jal init_spi
    
    # resume from deep power down
#    jal  transfer_start_spi
#    li   a0, 0xAB
#    jal  transfer_spi
#    jal  transfer_end_spi

    # read JEDEC ID
#    jal  transfer_start_spi

#    li   a0, 0x9F
#    jal  transfer_spi

#    li   a0, 0x00
#    jal  transfer_spi
#    mv   t0, a0

#    li   a0, 0x00
#    jal  transfer_spi
#    mv   t1, a0

#    li   a0, 0x00
#    jal  transfer_spi
#    mv   t2, a0
    
#    jal  transfer_end_spi

    # shift jedec out of t0,1,2 into a0
#    li   a0, 0x0
#    slli t0, t0, 16
#    or   a0, a0, t0
#    slli t1, t1, 8
#    or   a0, a0, t1
#    or   a0, a0, t2

    # compare jedec vs expected then print success or failure.
#    li   a1, 0x1F8501 # expected JEDEC ID
#    beq  a0, a1, 3f       # branch if equal
#    la   a0, fail_str
#    j    4f
#3:  la   a0, success_str
#4:  jal  put_str

    # echo user input
#5:  jal  get_char
#    jal  put_char
#    j    5b

init_spi:
    li   a0, SPI_BASE    # set SPI_BASE pointer
  
    # enable spi
1:  lbu  a1, 3(a0)       # read bus status
    bnez a1, 1b          # wait until bus idle
    li   a1, 0x00010000  # set WRITE mode
    li   a2, SPI_CR1     # set CR1 address
    or   a1, a1, a2      # set CR1 address
    ori  a1, a1, 0x80    # set data
    sw   a1, (a0)        # send command

    # set clock div
2:  lbu  a1, 3(a0)       # read bus status
    bnez a1, 2b          # wait until bus idle
    li   a1, 0x00010000  # set WRITE mode
    li   a2, SPI_BR      # set BR address
    or   a1, a1, a2      # set BR address
    ori  a1, a1, 0x03    # set data
    sw   a1, (a0)        # send command

    ret


transfer_start_spi:
    li   a0, SPI_BASE   # set SPI_BASE pointer
    
    # set CSN
1:  lbu  a1, 3(a0)      # read bus status
    bnez a1, 1b         # wait until bus idle
    li   a1, 0x00010000 # set WRITE mode
    li   a2, SPI_CSR    # set CSR address
    or   a1, a1, a2     # set CSR address
    sw   a1, (a0)       # send command

    # set spi master, cs hold
2:  lbu  a1, 3(a0)      # read bus status
    bnez a1, 2b         # wait until bus idle
    li   a1, 0x00010000 # set WRITE mode
    li   a2, SPI_CR2    # set CR2 address
    or   a1, a1, a2     # set CR2 address
    ori  a1, a1, 0xC0   # set data
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
    li   a2, 0x00010000 # set WRITE mode
    li   a3, SPI_TXDR   # set TXDR address
    or   a2, a2, a3     # set TXDR address
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
    li   a1, 0x00010000 # set WRITE mode
    li   a2, SPI_CSR    # set CSR address
    or   a1, a1, a2     # set CSR address
    ori  a1, a1, 0x0F   # set data
    sw   a1, (a0)       # send command

    # unset cs hold
2:  lbu  a1, 3(a0)      # read bus status
    bnez a1, 2b         # wait until bus idle
    li   a1, 0x00010000 # set WRITE mode
    li   a2, SPI_CR2    # set CR2 address
    or   a1, a1, a2     # set CR2 address
    ori  a1, a1, 0x80   # set data
    sw   a1, (a0)       # send command

    # wait not tip
3:  lbu  a1, 3(a0)      # read bus status
    bnez a1, 3b         # wait until bus idle
    li   a1, 0x00000000 # set READ mode
    li   a2, SPI_SR     # set SR address
    or   a1, a1, a2     # set SR address
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


#check_spram:
#    li   a0, 0x0
#    li   a1, SPRAM_BASE
#    li   a2, 0x1234abcd
#
#    # check word
#    sw   a2, (a1)
#    li   a3, 0x0  # zero a3
#    lw   a3, (a1)
#    xor  a3, a3, a2
#    or   a0, a0, a3
#
#    # check half-word
#    sh   a2, 4(a1)  # store lower half
#    lhu  a3, 4(a1)  # load lower half
#    li   a4, 0xabcd
#    xor  a4, a4, a3
#    or   a0, a0, a4
#
#    sh   a2, 6(a1)  # store to upper half
#    lhu  a3, 6(a1)  # load from upper half
#    li   a4, 0xabcd
#    xor  a4, a4, a3
#    or   a0, a0, a4
#
#    # check byte
#    sb   a2, 8(a1)
#    lbu  a3, 8(a1)
#    li   a4, 0xcd
#    xor  a4, a4, a3
#    or   a0, a0, a4
#
#    sb   a2, 9(a1)
#    lbu  a3, 9(a1)
#    li   a4, 0xcd
#    xor  a4, a4, a3
#    or   a0, a0, a4
#
#    sb   a2, 10(a1)
#    lbu  a3, 10(a1)
#    li   a4, 0xcd
#    xor  a4, a4, a3
#    or   a0, a0, a4
#
#    sb   a2, 11(a1)
#    lbu  a3, 11(a1)
#    li   a4, 0xcd
#    xor  a4, a4, a3
#    or   a0, a0, a4
#
#    ret


get_char:
    li   a0, UART_BASE # set UART_BASE pointer
1:  lbu  a1, 0x2(a0)   # read status register
    andi a1, a1, 0x2   # mask receiver status
    beqz a1, 1b        # wait until receiver full
    lbu  a1, 0x1(a0)   # read receiver buffer
    mv   a0, a1        # return data in a0
    ret


put_char:
    li   a1, UART_BASE # set UART_BASE pointer
1:  lbu  a2, 0x2(a1)   # read status
    andi a2, a2, 0x1   # mask transmitter status
    bnez a2, 1b        # wait until transmitter empty
    sb   a0, (a1)      # transmit byte
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


set_led_red:
    li   a1, LED_BASE

    # enable cr0
    li   a0, CR0
    slli a0, a0, 8
    ori  a0, a0, 0x80
    sh   a0, (a1)
    
    # set pwrr
    li   a0, PWRR
    slli a0, a0, 8
    ori  a0, a0, 0xFF
    sh   a0, (a1)

    ret


    .section .rodata
    .align 4

name:
    .string "\n\r   __   _             ____     _____\n\r  / /  (_)__  ___    / __/__  / ___/\n\r / /__/ / _ \\/ _ \\  _\\ \\/ _ \\/ /__  \n\r/____/_/\\___/_//_/ /___/\\___/\\___/  \n\r"

copyright:
    .string "\n\rStandard Semiconductor (c) 2022\n\r\n\r"

#check_spram_str:
#    .string "Checking SPRAM..."

check_flash_str:
    .string "Checking FLASH..."

success_str:
    .string "SUCCESS\n\r"
fail_str:
    .string "FAIL\n\r"
