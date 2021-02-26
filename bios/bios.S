    .equ LED_BASE, 0x200
    .equ CR0     , 0x8
    .equ BR      , 0x9
    .equ ONR     , 0xA
    .equ OFR     , 0xB
    .equ BCRR    , 0x5
    .equ BCFR    , 0x6
    .equ PWRR    , 0x1

    .section .text
    .globl _start

_start:
    jal  set_led_red
halt: 
    j halt

set_led_red:
    li   a1, LED_BASE

    # enable cr0
    li   a0, CR0
    slli a0, a0, 8
    ori  a0, a0, 0x80
    sh   a0, (a1)

    # set leddbr
    li   a0, BR
    slli a0, a0, 8
    ori  a0, a0, 0x12
    sh   a0, (a1)
    
    # set leddonr
    li   a0, ONR
    slli a0, a0, 8
    ori  a0, a0, 0x80
    sh   a0, (a1)
    
    # set leddofr
    li   a0, OFR
    slli a0, a0, 8
    ori  a0, a0, 0x80
    sh   a0, (a1)

    # set leddbcrr
    li   a0, BCRR
    slli a0, a0, 8
    ori  a0, a0, 0xCE
    sh   a0, (a1)
    
    # set leddbcfr
    li   a0, BCFR
    slli a0, a0, 8
    ori  a0, a0, 0xCE
    sh   a0, (a1)
    
    # set pwrr
    li   a0, PWRR
    slli a0, a0, 8
    ori  a0, a0, 0xFF
    sh   a0, (a1)

    ret