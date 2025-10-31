.equ UART_DR, 0x40000000
.equ UART_SR, 0x40000004
.equ UART_RDY, 1

ldr r4, =0x00100000
ldr r5, =UART_DR
ldr r6, =UART_SR

mov r2, #4
mov r7, #0 ; length of programm in bytes

read_len:
    ; check on UART status
    ldr r0, [r6] ; read from UART_SR
    tst r0, #UART_RDY ; check if
    beq read_len ; if 

    ldrb r1, [r5] ; read byte from UART_DR
    lsl r7, r7, #8
    orr r7, r7, r1 ; add byte to end
    subs r2, r2, #1 ; decrement r2
    bne read_len ; if r0 == 0, jump to read_len

loop:
    cmp r7, #0 ; check if we complete
    beq done ; jump to done

    ldr r0, [r6] ; read UART_SR
    tst r0, #UART_RDY ; check if uart wrote
    beq loop ; if no, continue

    ldrb r1, [r5] ; read byte from UART_DR
    strb r1, [r4], #1 ; store byte in r4 and incrementt
    subs r7, r7, #1 ; decrement length (r7)
    bne loop ; if length not zero, continue

done:
    b 0x00100000
