    .equ LED_BASE  , 0x0
    .equ CR0       , 0x8
    .equ PWRR      , 0x1

    .equ UART_BASE , 0x4
    .equ SPRAM_BASE, 0x20000

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

    # check SPRAM
    la   a0, check_spram_str
    jal  put_str
    jal  check_spram
    beqz a0, 1f
    la   a0, fail_str
    j    2f
1:  la   a0, success_str
2:  jal  put_str

    # echo user input
3:  jal  get_char
    jal  put_char
    j    3b
halt: 
    j halt

check_spram:
    li   a0, 0x0
    li   a1, SPRAM_BASE
    li   a2, 0x1234abcd

    # check word
    sw   a2, (a1)
    li   a3, 0x0  # zero a3
    lw   a3, (a1)
    xor  a3, a3, a2
    or   a0, a0, a3

    # check half-word
    sh   a2, 4(a1)  # store lower half
    lhu  a3, 4(a1)  # load lower half
    li   a4, 0xabcd
    xor  a4, a4, a3
    or   a0, a0, a4

    sh   a2, 6(a1)  # store to upper half
    lhu  a3, 6(a1)  # load from upper half
    li   a4, 0xabcd
    xor  a4, a4, a3
    or   a0, a0, a4

    # check byte
    sb   a2, 8(a1)
    lbu  a3, 8(a1)
    li   a4, 0xcd
    xor  a4, a4, a3
    or   a0, a0, a4

    sb   a2, 9(a1)
    lbu  a3, 9(a1)
    li   a4, 0xcd
    xor  a4, a4, a3
    or   a0, a0, a4

    sb   a2, 10(a1)
    lbu  a3, 10(a1)
    li   a4, 0xcd
    xor  a4, a4, a3
    or   a0, a0, a4

    sb   a2, 11(a1)
    lbu  a3, 11(a1)
    li   a4, 0xcd
    xor  a4, a4, a3
    or   a0, a0, a4

    ret

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
    .string "\n\rStandard Semiconductor (c) 2021\n\r\n\r"

check_spram_str:
    .string "Checking SPRAM..."

success_str:
    .string "SUCCESS\n\r"
fail_str:
    .string "FAIL\n\r"