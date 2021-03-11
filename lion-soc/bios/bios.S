    .equ LED_BASE, 0x0
    .equ CR0     , 0x8
    .equ PWRR    , 0x1

    .equ UART_BASE, 0x4

    .section .text
    .globl _start

_start:
    jal  get_char
    jal  set_led_red
    la   a0, name
    jal  put_str
    la   a0, copyright
    jal  put_str
halt: 
    j halt

get_char:
    li   a0, UART_BASE # set UART_BASE pointer
1:  lbu  a1, 0x2(a0)   # read status register
    andi a1, a1, 0x2   # mask receiver status
    beqz a1, 1b        # wait until receiver full
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
    .string "   __   _             ____     _____\n\r  / /  (_)__  ___    / __/__  / ___/\n\r / /__/ / _ \\/ _ \\  _\\ \\/ _ \\/ /__  \n\r/____/_/\\___/_//_/ /___/\\___/\\___/  \n\r"

copyright:
    .string "Standard Semiconductor (c) 2021"