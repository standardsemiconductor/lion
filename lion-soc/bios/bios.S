    .equ LED_BASE, 0x00000000
    .equ CR0     , 0x8
    .equ BR      , 0x9
    .equ ONR     , 0xA
    .equ OFR     , 0xB
    .equ BCRR    , 0x5
    .equ BCFR    , 0x6
    .equ PWRR    , 0x1

    .equ UART_BASE, 0x00000004

    .section .text
    .globl _start

_start:
    jal  set_led_red
halt: 
    j halt

put_str:
    li   a1, UART_BASE # set UART_BASE pointer
1:  lbu  a2, (a0)      # load str byte
    beqz a2, 3f        # if zero, end of string, ret
    sb   a2, (a1)      # transmit command
2:  lbu  a2, 0x2(a1)   # read status
    andi a2, a2, 0x1   # mask transmitter status
    bnez a2, 2b        # wait until transmitter empty
    addi a0, a0, 0x1   # increment str pointer
    j    1b
3:  ret 

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

info:
    .string "Standard Semiconductor (c) 2021"