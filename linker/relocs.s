// -marm -fno-pic -fno-pie
.syntax unified
.arm

.extern call_f
.extern p
.extern rel_to_p

.text
.global use_relocs
use_relocs:
    bl call_f

    movw r4, #:lower16:p
    movt r4, #:upper16:p
    movw r6, #:lower16:rel_to_p
    movt r6, #:upper16:rel_to_p

    ldr r7, [r6]
    add r7, r7, r6

    cmp r7, r4
    moveq r0, #42
    movne r0, #13
    bx lr

.data
.global abs_ptr
abs_ptr:
    .word p

.global rel_to_p
rel_to_p:
    .word p - .
