; NOTE: The NES's 2A03 has no decimal mode; hence, we don't bother emulating it.
; We don't keep track of clock cycles because it's not needed for an NSF player.
; BRK and RTI are deliberately unimplemented because they're not expected to occur.
;
; Quirks (deliberate emulation bugs):
; * If the stack overflows, it will start overwriting the zero page.
; * Writing to ROM space will overwrite ROM data.
;
; @TODO@ -- make macros for each instruction type to cut down on duplicate code

    cpu     8086


global Run6502

extern PrintErrorAndQuit


FLAG_CARRY      equ     1 << 0
FLAG_ZERO       equ     1 << 1
FLAG_INTERRUPT  equ     1 << 2
FLAG_DECIMAL    equ     1 << 3
FLAG_BREAK      equ     1 << 4
FLAG_UNUSED     equ     1 << 5                  ; This should always be 1, but we don't bother
FLAG_OVERFLOW   equ     1 << 6
FLAG_MINUS      equ     1 << 7



; Pass in a reg or [bx]
%macro CheckZeroNeg 1
        and     ah, ~(FLAG_MINUS | FLAG_ZERO)
        CheckZeroNegSetOnly %1
%endmacro

; Used when the relevant flags have already been cleared beforehand
; i.e. it only sets the flags, never clears 'em
%macro CheckZeroNegSetOnly 1
        ; Set negative and zero flags according to lookup table
        mov     bl, %1
        xor     bh, bh                      ; must come after since %1 can be [bx]
        mov     bl, [CS:ZeroNegTbl+bx]
        or      ah, bl
%endmacro

; Uses x86's own carry flag to check carry
%macro CheckZeroNegCarryAfterShiftSetOnly 1
        jnc     %%no_carry
        or      ah, FLAG_CARRY
%%no_carry:
        xor     bh, bh
        mov     bl, %1
        mov     bl, [CS:ZeroNegTbl+bx]
        or      ah, bl
%endmacro

%macro CheckZeroNegCarryAfterSubtractionSetOnly 1
        ; x86 carry is opposite of 6502 carry here
        jc      %%no_carry
        or      ah, FLAG_CARRY
%%no_carry:
        xor     bh, bh
        mov     bl, %1
        mov     bl, [CS:ZeroNegTbl+bx]
        or      ah, bl
%endmacro

; Here the thing we want to check is already in BL (used for CMP)
%macro CheckZeroNegCarryAfterSubtractionSetOnlyBL 0
        ; x86 carry is opposite of 6502 carry here
        jc      %%no_carry
        or      ah, FLAG_CARRY
%%no_carry:
        xor     bh, bh
        mov     bl, [CS:ZeroNegTbl+bx]
        or      ah, bl
%endmacro


; This will move the address of a var in the zero page to BX
; The rest are similar
%macro HandleZP 0
        xor     bh, bh
        mov     bl, [di]
        inc     di
%endmacro

%macro HandleZP_X 0
        HandleZP
        add     bl, dl
        adc     bh, 0
%endmacro

%macro HandleZP_Y 0
        HandleZP
        add     bl, dh
        adc     bh, 0
%endmacro

%macro HandleABS 0
        mov     bx, [di]
        inc     di
        inc     di
%endmacro

%macro HandleABS_X 0
        HandleABS
        add     bl, dl
        adc     bh, 0
%endmacro

%macro HandleABS_Y 0
        HandleABS
        add     bl, dh
        adc     bh, 0
%endmacro

%macro HandleIND_X 0
        HandleZP_X
        mov     bl, [bx]
%endmacro

%macro HandleIND_Y 0
        HandleZP
        mov     bx, [bx]
        add     bl, dh
        adc     bh, 0
%endmacro


; Helpers for ADC and SBC instructions, which are involved
%macro ADC_start 0
        ; CL will store current value of AL (6502's A) to check for carry later
        mov     cl, al
        test    ah, FLAG_CARRY
        jz      %%add_without_carry
        inc     al
%%add_without_carry:
        and     ah, ~(FLAG_MINUS | FLAG_OVERFLOW | FLAG_ZERO | FLAG_CARRY)
%endmacro

%macro ADC_end 0
        jno     %%no_overflow
        or      ah, FLAG_OVERFLOW
%%no_overflow:
        ; If new AL is below old AL, there was a carry
        cmp     al, cl
        jae     %%no_carry
        or      ah, FLAG_CARRY
%%no_carry:
        CheckZeroNegSetOnly al
        jmp     Run6502
%endmacro

%macro SBC_start 0
        ; CL will store current value of AL (6502's A) to check for carry later
        mov     cl, al
        test    ah, FLAG_CARRY
        jnz     %%sub_without_borrow
        dec     al
%%sub_without_borrow:
        and     ah, ~(FLAG_MINUS | FLAG_OVERFLOW | FLAG_ZERO | FLAG_CARRY)
%endmacro

%macro SBC_end 0
        jno     %%no_overflow
        or      ah, FLAG_OVERFLOW
%%no_overflow:
        ; If new AL not above old AL, there was no borrow
        cmp     al, cl
        ja      %%borrowed
        or      ah, FLAG_CARRY
%%borrowed:
        CheckZeroNegSetOnly al
        jmp     Run6502
%endmacro


; Helpers for ROL and ROR
; Would be nice if they worked the same on the 6502 as on x86, but they don't!
; Parameter is the register that holds the thing to rotate
%macro ROL_body 1
        xor     cl, cl
        test    ah, FLAG_CARRY
        jz      %%no_old_carry
        inc     cx                          ; faster than INC CL
%%no_old_carry:
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        test    %1, 0x80
        jz      %%no_new_carry
        or      ah, FLAG_CARRY
%%no_new_carry:
        shl     %1, 1
        or      %1, cl                      ; shift in old carry
        CheckZeroNegSetOnly %1
%endmacro

%macro ROR_body 1
        xor     cl, cl
        test    ah, FLAG_CARRY
        jz      %%no_old_carry
        mov     cl, 0x80
%%no_old_carry:
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        test    %1, 1
        jz      %%no_new_carry
        or      ah, FLAG_CARRY
%%no_new_carry:
        shr     %1, 1
        or      %1, cl                      ; shift in old carry
        CheckZeroNegSetOnly %1
%endmacro


; Helpers for branch instructions
%macro BranchIfSet 1
        test    ah, %1
        jz      %%no_branch
        ExecBranch
%%no_branch:
        inc     di
        jmp     Run6502
%endmacro

%macro BranchIfClear 1
        test    ah, %1
        jnz     %%no_branch
        ExecBranch
%%no_branch:
        inc     di
        jmp     Run6502
%endmacro

%macro ExecBranch 0
        mov     bl, [di]
        ; Extend sign in BL before adding
        xchg    ax, bx
        cbw
        xchg    ax, bx
        add     di, bx
%endmacro


segment CodeSeg

; A procedure that returns once the 6502 executes an RTS when its S register is 0xFF
; (or actually 0x1FF since internally it's 16-bit).
;
; Assumptions:
;   DS: points to a 64k segment of CPU memory space
;   ES: points to data segment containing emulation variables
;
; Mapping of x86 regs to 6502 regs:
;   AL: A
;   AH: P [i.e., flags]
;   BX: used as needed
;   CX: used as needed
;   DL: X
;   DH: Y
;   SI: S
;   DI: PC
Run6502:
        ; Fetch...
        xor     bh, bh
        mov     bl, [di]
        inc     di

        ; ...decode, execute!
        ; @TODO@ -  minor optimization idea: copy jump table to 6502 space so we can use DS
        ; This will save 2 cycles per 6502 instruction.
        shl     bx, 1
        jmp     [CS:OpcodeJmpTbl+bx]


OpNotImplemented:
        mov     dx, NotImplementedMsg
        jmp     PrintErrorAndQuit

OpInvalid:
        mov     dx, InvalidOpMsg
        jmp     PrintErrorAndQuit


OpADC_imm:
        ADC_start
        add     al, [di]
        inc     di
        ADC_end

OpADC_zp:
        ADC_start
        HandleZP
        add     al, [bx]
        ADC_end

OpADC_zp_x:
        ADC_start
        HandleZP_X
        add     al, [bx]
        ADC_end

OpADC_abs:
        ADC_start
        HandleABS
        add     al, [bx]
        ADC_end

OpADC_abs_x:
        ADC_start
        HandleABS_X
        add     al, [bx]
        ADC_end

OpADC_abs_y:
        ADC_start
        HandleABS_Y
        add     al, [bx]
        ADC_end

OpADC_ind_x:
        ADC_start
        HandleIND_X
        add     al, [bx]
        ADC_end

OpADC_ind_y:
        ADC_start
        HandleIND_Y
        add     al, [bx]
        ADC_end


OpAND_imm:
        and     al, [di]
        inc     di
        CheckZeroNeg al
        jmp     Run6502

OpAND_zp:
        HandleZP
        and     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpAND_zp_x:
        HandleZP_X
        and     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpAND_abs:
        HandleABS
        and     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpAND_abs_x:
        HandleABS_X
        and     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpAND_abs_y:
        HandleABS_X
        and     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpAND_ind_x:
        HandleIND_X
        and     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpAND_ind_y:
        HandleIND_Y
        and     al, [bx]
        CheckZeroNeg al
        jmp     Run6502


OpASL_a:
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        shl     al, 1
        CheckZeroNegCarryAfterShiftSetOnly al
        jmp     Run6502

OpASL_zp:
        HandleZP
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        mov     cl, [bx]
        shl     cl, 1
        mov     [bx], cl
        CheckZeroNegCarryAfterShiftSetOnly cl
        jmp     Run6502

OpASL_zp_x:
        HandleZP_X
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        mov     cl, [bx]
        shl     cl, 1
        mov     [bx], cl
        CheckZeroNegCarryAfterShiftSetOnly cl
        jmp     Run6502

OpASL_abs:
        HandleABS
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        mov     cl, [bx]
        shl     cl, 1
        mov     [bx], cl
        CheckZeroNegCarryAfterShiftSetOnly cl
        jmp     Run6502

OpASL_abs_x:
        HandleABS_X
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        mov     cl, [bx]
        shl     cl, 1
        mov     [bx], cl
        CheckZeroNegCarryAfterShiftSetOnly cl
        jmp     Run6502


OpBCC:
        BranchIfClear FLAG_CARRY

OpBCS:
        BranchIfSet FLAG_CARRY

OpBEQ:
        BranchIfSet FLAG_ZERO


OpBIT_zp:
        HandleZP
OpBIT_impl:
        and     ah, ~(FLAG_MINUS | FLAG_OVERFLOW | FLAG_ZERO)
        ; Bits 6 and 7 (minus and overflow) of the flags get set to the corresponding bits of the bitmask.
        ; Yeah, it's weird.
        mov     bl, [bx]
        mov     bh, bl
        and     bh, 0xc0
        or      ah, bh
        test    al, bl
        jnz     .nonzero
        or      ah, FLAG_ZERO
.nonzero:
        jmp     Run6502

OpBIT_abs:
        HandleABS
        ; Why repeat that crap?
        jmp     OpBIT_impl


OpBMI:
        BranchIfSet FLAG_MINUS

OpBNE:
        BranchIfClear FLAG_ZERO

OpBPL:
        BranchIfClear FLAG_MINUS


OpBRK:
        jmp     OpNotImplemented


OpBVC:
        BranchIfClear FLAG_OVERFLOW

OpBVS:
        BranchIfSet FLAG_OVERFLOW


OpCLC:
        and     ah, ~FLAG_CARRY
        jmp     Run6502

OpCLD:
        and     ah, ~FLAG_DECIMAL
        jmp     Run6502

OpCLI:
        and     ah, ~FLAG_INTERRUPT
        jmp     Run6502

OpCLV:
        and     ah, ~FLAG_OVERFLOW
        jmp     Run6502


OpCMP_imm:
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        mov     bl, al
        sub     bl, [di]
        CheckZeroNegCarryAfterSubtractionSetOnlyBL
        inc     di
        jmp     Run6502

OpCMP_zp:
        HandleZP
        mov     cl, al
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        sub     cl, [bx]
        CheckZeroNegCarryAfterSubtractionSetOnly cl
        jmp     Run6502

OpCMP_zp_x:
        HandleZP_X
        mov     cl, al
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        sub     cl, [bx]
        CheckZeroNegCarryAfterSubtractionSetOnly cl
        jmp     Run6502

OpCMP_abs:
        HandleABS
        mov     cl, al
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        sub     cl, [bx]
        CheckZeroNegCarryAfterSubtractionSetOnly cl
        jmp     Run6502

OpCMP_abs_x:
        HandleABS_X
        mov     cl, al
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        sub     cl, [bx]
        CheckZeroNegCarryAfterSubtractionSetOnly cl
        jmp     Run6502

OpCMP_abs_y:
        HandleABS_Y
        mov     cl, al
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        sub     cl, [bx]
        CheckZeroNegCarryAfterSubtractionSetOnly cl
        jmp     Run6502

OpCMP_ind_x:
        HandleIND_X
        mov     cl, al
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        sub     cl, [bx]
        CheckZeroNegCarryAfterSubtractionSetOnly cl
        jmp     Run6502

OpCMP_ind_y:
        HandleIND_Y
        mov     cl, al
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        sub     cl, [bx]
        CheckZeroNegCarryAfterSubtractionSetOnly cl
        jmp     Run6502


OpCPX_imm:
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        mov     bl, dl
        sub     bl, [di]
        CheckZeroNegCarryAfterSubtractionSetOnlyBL
        inc     di
        jmp     Run6502

OpCPX_zp:
        HandleZP
        mov     cl, dl
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        sub     cl, [bx]
        CheckZeroNegCarryAfterSubtractionSetOnly cl
        jmp     Run6502

OpCPX_abs:
        HandleABS
        mov     cl, dl
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        sub     cl, [bx]
        CheckZeroNegCarryAfterSubtractionSetOnly cl
        jmp     Run6502


OpCPY_imm:
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        mov     bl, dh
        sub     bl, [di]
        CheckZeroNegCarryAfterSubtractionSetOnlyBL
        inc     di
        jmp     Run6502

OpCPY_zp:
        HandleZP
        mov     cl, dh
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        sub     cl, [bx]
        CheckZeroNegCarryAfterSubtractionSetOnly cl
        jmp     Run6502

OpCPY_abs:
        HandleABS
        mov     cl, dh
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        sub     cl, [bx]
        CheckZeroNegCarryAfterSubtractionSetOnly cl
        jmp     Run6502


OpDEC_zp:
        HandleZP
        dec     byte [bx]
        CheckZeroNeg [bx]
        jmp     Run6502

OpDEC_zp_x:
        HandleZP_X
        dec     byte [bx]
        CheckZeroNeg [bx]
        jmp     Run6502

OpDEC_abs:
        HandleABS
        dec     byte [bx]
        CheckZeroNeg [bx]
        jmp     Run6502

OpDEC_abs_x:
        HandleABS_X
        dec     byte [bx]
        CheckZeroNeg [bx]
        jmp     Run6502


OpDEX:
        dec     dl
        CheckZeroNeg dl
        jmp     Run6502

OpDEY:
        dec     dh
        CheckZeroNeg dh
        jmp     Run6502


OpEOR_imm:
        xor     al, [di]
        inc     di
        CheckZeroNeg al
        jmp     Run6502

OpEOR_zp:
        HandleZP
        xor     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpEOR_zp_x:
        HandleZP_X
        xor     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpEOR_abs:
        HandleABS
        xor     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpEOR_abs_x:
        HandleABS_X
        xor     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpEOR_abs_y:
        HandleABS_X
        xor     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpEOR_ind_x:
        HandleIND_X
        xor     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpEOR_ind_y:
        HandleIND_Y
        xor     al, [bx]
        CheckZeroNeg al
        jmp     Run6502


OpINC_zp:
        HandleZP
        inc     byte [bx]
        CheckZeroNeg [bx]
        jmp     Run6502

OpINC_zp_x:
        HandleZP_X
        inc     byte [bx]
        CheckZeroNeg [bx]
        jmp     Run6502

OpINC_abs:
        HandleABS
        inc     byte [bx]
        CheckZeroNeg [bx]
        jmp     Run6502

OpINC_abs_x:
        HandleABS_X
        inc     byte [bx]
        CheckZeroNeg [bx]
        jmp     Run6502


OpINX:
        inc     dl
        CheckZeroNeg dl
        jmp     Run6502

OpINY:
        inc     dh
        CheckZeroNeg dh
        jmp     Run6502


OpJMP:
        mov     di, [di]
        jmp     Run6502

; This does not replicate the infamous 6502 bug with page boundaries
OpJMP_ind:
        mov     bx, [di]
        mov     di, [bx]
        jmp     Run6502


OpJSR:
        mov     bx, [di]
        inc     di                          ; Yes, only INC once. RTS will INC it again later.
        dec     si
        dec     si
        mov     [si], di
        mov     di, bx
        jmp     Run6502


OpLDA_imm:
        mov     al, [di]
        inc     di
        CheckZeroNeg al
        jmp     Run6502

OpLDA_zp:
        HandleZP
        mov     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpLDA_zp_x:
        HandleZP_X
        mov     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpLDA_abs:
        HandleABS
        mov     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpLDA_abs_x:
        HandleABS_X
        mov     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpLDA_abs_y:
        HandleABS_Y
        mov     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpLDA_ind_x:
        HandleIND_X
        mov     al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpLDA_ind_y:
        HandleIND_Y
        mov     al, [bx]
        CheckZeroNeg al
        jmp     Run6502


OpLDX_imm:
        mov     dl, [di]
        inc     di
        CheckZeroNeg dl
        jmp     Run6502

OpLDX_zp:
        HandleZP
        mov     dl, [bx]
        CheckZeroNeg dl
        jmp     Run6502

OpLDX_zp_y:
        HandleZP_Y
        mov     dl, [bx]
        CheckZeroNeg dl
        jmp     Run6502

OpLDX_abs:
        HandleABS
        mov     dl, [bx]
        CheckZeroNeg dl
        jmp     Run6502

OpLDX_abs_y:
        HandleABS_Y
        mov     dl, [bx]
        CheckZeroNeg dl
        jmp     Run6502


OpLDY_imm:
        mov     dh, [di]
        inc     di
        CheckZeroNeg dh
        jmp     Run6502

OpLDY_zp:
        HandleZP
        mov     dh, [bx]
        CheckZeroNeg dh
        jmp     Run6502

OpLDY_zp_x:
        HandleZP_X
        mov     dh, [bx]
        CheckZeroNeg dh
        jmp     Run6502

OpLDY_abs:
        HandleABS
        mov     dh, [bx]
        CheckZeroNeg dh
        jmp     Run6502

OpLDY_abs_x:
        HandleABS_X
        mov     dh, [bx]
        CheckZeroNeg dh
        jmp     Run6502


OpLSR_a:
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        shr     al, 1
        CheckZeroNegCarryAfterShiftSetOnly al
        jmp     Run6502

OpLSR_zp:
        HandleZP
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        mov     cl, [bx]
        shr     cl, 1
        mov     [bx], cl
        CheckZeroNegCarryAfterShiftSetOnly cl
        jmp     Run6502

OpLSR_zp_x:
        HandleZP_X
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        mov     cl, [bx]
        shr     cl, 1
        mov     [bx], cl
        CheckZeroNegCarryAfterShiftSetOnly cl
        jmp     Run6502

OpLSR_abs:
        HandleABS
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        mov     cl, [bx]
        shr     cl, 1
        mov     [bx], cl
        CheckZeroNegCarryAfterShiftSetOnly cl
        jmp     Run6502

OpLSR_abs_x:
        HandleABS_X
        and     ah, ~(FLAG_MINUS | FLAG_ZERO | FLAG_CARRY)
        mov     cl, [bx]
        shr     cl, 1
        mov     [bx], cl
        CheckZeroNegCarryAfterShiftSetOnly cl
        jmp     Run6502


OpNOP:
        ; Nothing to do!
        jmp     Run6502


OpORA_imm:
        or      al, [di]
        inc     di
        CheckZeroNeg al
        jmp     Run6502

OpORA_zp:
        HandleZP
        or      al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpORA_zp_x:
        HandleZP_X
        or      al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpORA_abs:
        HandleABS
        or      al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpORA_abs_x:
        HandleABS_X
        or      al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpORA_abs_y:
        HandleABS_X
        or      al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpORA_ind_x:
        HandleIND_X
        or      al, [bx]
        CheckZeroNeg al
        jmp     Run6502

OpORA_ind_y:
        HandleIND_Y
        or      al, [bx]
        CheckZeroNeg al
        jmp     Run6502


OpPHA:
        dec     si
        mov     [si], al
        jmp     Run6502

OpPHP:
        dec     si
        mov     [si], ah
        jmp     Run6502

OpPLA:
        mov     al, [si]
        inc     si
        CheckZeroNeg al
        jmp     Run6502

OpPLP:
        mov     ah, [si]
        inc     si
        jmp     Run6502


OpROL_a:
        ROL_body al
        jmp     Run6502

OpROL_zp:
        HandleZP
        ; Work reg is DL instead of AL because otherwise the flag changes
        ; would be overwritten when we pop AX.
        push    dx
        mov     dl, [bx]
        ROL_body dl
        mov     [bx], dl
        pop     dx
        jmp     Run6502

OpROL_zp_x:
        HandleZP_X
        push    dx
        mov     dl, [bx]
        ROL_body dl
        mov     [bx], dl
        pop     dx
        jmp     Run6502

OpROL_abs:
        HandleABS
        push    dx
        mov     dl, [bx]
        ROL_body dl
        mov     [bx], dl
        pop     dx
        jmp     Run6502

OpROL_abs_x:
        HandleABS_X
        push    dx
        mov     dl, [bx]
        ROL_body dl
        mov     [bx], dl
        pop     dx
        jmp     Run6502


OpROR_a:
        ROR_body al
        jmp     Run6502

OpROR_zp:
        HandleZP
        push    dx
        mov     dl, [bx]
        ROR_body dl
        mov     [bx], dl
        pop     dx
        jmp     Run6502

OpROR_zp_x:
        HandleZP_X
        push    dx
        mov     dl, [bx]
        ROR_body dl
        mov     [bx], dl
        pop     dx
        jmp     Run6502

OpROR_abs:
        HandleABS
        push    dx
        mov     dl, [bx]
        ROR_body dl
        mov     [bx], dl
        pop     dx
        jmp     Run6502

OpROR_abs_x:
        HandleABS_X
        push    dx
        mov     dl, [bx]
        ROR_body dl
        mov     [bx], dl
        pop     dx
        jmp     Run6502


OpRTI:
        jmp     OpNotImplemented


OpRTS:
        ; 6502 execution ends when program executes RTS from top of stack
        cmp     si, 0x1ff
        je      .return
        ; Nope, processor still processin'
        mov     di, [si]
        inc     si
        inc     si
        inc     di                          ; Since the value pushed by JSR is off by one
        jmp     Run6502
.return:
        ret


OpSBC_imm:
        SBC_start
        sub     al, [di]
        inc     di
        SBC_end

OpSBC_zp:
        SBC_start
        HandleZP
        sub     al, [bx]
        SBC_end

OpSBC_zp_x:
        SBC_start
        HandleZP_X
        sub     al, [bx]
        SBC_end

OpSBC_abs:
        SBC_start
        HandleABS
        sub     al, [bx]
        SBC_end

OpSBC_abs_x:
        SBC_start
        HandleABS_X
        sub     al, [bx]
        SBC_end

OpSBC_abs_y:
        SBC_start
        HandleABS_Y
        sub     al, [bx]
        SBC_end

OpSBC_ind_x:
        SBC_start
        HandleIND_X
        sub     al, [bx]
        SBC_end

OpSBC_ind_y:
        SBC_start
        HandleIND_Y
        sub     al, [bx]
        SBC_end


OpSEC:
        or      ah, FLAG_CARRY
        jmp     Run6502

OpSED:
        ; Sure, you can set it, but it won't do anything!
        or      ah, FLAG_DECIMAL
        jmp     Run6502

OpSEI:
        or      ah, FLAG_INTERRUPT
        jmp     Run6502


OpSTA_zp:
        HandleZP
        mov     [bx], al
        jmp     Run6502

OpSTA_zp_x:
        HandleZP_X
        mov     [bx], al
        jmp     Run6502

OpSTA_abs:
        HandleABS
        mov     [bx], al
        jmp     Run6502

OpSTA_abs_x:
        HandleABS_X
        mov     [bx], al
        jmp     Run6502

OpSTA_abs_y:
        HandleABS_Y
        mov     [bx], al
        jmp     Run6502

OpSTA_ind_x:
        HandleIND_X
        mov     [bx], al
        jmp     Run6502

OpSTA_ind_y:
        HandleIND_Y
        mov     [bx], al
        jmp     Run6502


OpSTX_zp:
        HandleZP
        mov     [bx], dl
        jmp     Run6502

OpSTX_zp_y:
        HandleZP_Y
        mov     [bx], dl
        jmp     Run6502

OpSTX_abs:
        HandleABS
        mov     [bx], dl
        jmp     Run6502

OpSTX_abs_y:
        HandleABS_Y
        mov     [bx], dl
        jmp     Run6502


OpSTY_zp:
        HandleZP
        mov     [bx], dh
        jmp     Run6502

OpSTY_zp_x:
        HandleZP_X
        mov     [bx], dh
        jmp     Run6502

OpSTY_abs:
        HandleABS
        mov     [bx], dh
        jmp     Run6502

OpSTY_abs_x:
        HandleABS_X
        mov     [bx], dh
        jmp     Run6502


OpTAX:
        mov     dl, al
        CheckZeroNeg dl
        jmp     Run6502

OpTAY:
        mov     dh, al
        CheckZeroNeg dh
        jmp     Run6502

OpTSX:
        mov     bx, si
        mov     dl, bl
        CheckZeroNeg dl
        jmp     Run6502

OpTXA:
        mov     al, dl
        CheckZeroNeg al
        jmp     Run6502

OpTXS:
        mov     bh, 1                       ; Remember: stack at 1xx!
        mov     bl, dl
        mov     si, bx
        jmp     Run6502

OpTYA:
        mov     al, dh
        CheckZeroNeg al
        jmp     Run6502


        ; 6502 jump table
        ; We're only emulating pure 6502, not undocumented opcodes
OpcodeJmpTbl:
        dw      OpBRK           ; 00
        dw      OpORA_ind_x     ; 01
        dw      OpInvalid       ; 02
        dw      OpInvalid       ; 03
        dw      OpInvalid       ; 04
        dw      OpORA_zp        ; 05
        dw      OpASL_zp        ; 06
        dw      OpInvalid       ; 07
        dw      OpPHP           ; 08
        dw      OpORA_imm       ; 09
        dw      OpASL_a         ; 0a
        dw      OpInvalid       ; 0b
        dw      OpInvalid       ; 0c
        dw      OpORA_abs       ; 0d
        dw      OpASL_abs       ; 0e
        dw      OpInvalid       ; 0f
        dw      OpBPL           ; 10
        dw      OpORA_ind_y     ; 11
        dw      OpInvalid       ; 12
        dw      OpInvalid       ; 13
        dw      OpInvalid       ; 14
        dw      OpORA_zp_x      ; 15
        dw      OpASL_zp_x      ; 16
        dw      OpInvalid       ; 17
        dw      OpCLC           ; 18
        dw      OpORA_abs_y     ; 19
        dw      OpInvalid       ; 1a
        dw      OpInvalid       ; 1b
        dw      OpInvalid       ; 1c
        dw      OpORA_abs_x     ; 1d
        dw      OpASL_abs_x     ; 1e
        dw      OpInvalid       ; 1f
        dw      OpJSR           ; 20
        dw      OpAND_ind_x     ; 21
        dw      OpInvalid       ; 22
        dw      OpInvalid       ; 23
        dw      OpBIT_zp        ; 24
        dw      OpAND_zp        ; 25
        dw      OpROL_zp        ; 26
        dw      OpInvalid       ; 27
        dw      OpPLP           ; 28
        dw      OpAND_imm       ; 29
        dw      OpROL_a         ; 2a
        dw      OpInvalid       ; 2b
        dw      OpBIT_abs       ; 2c
        dw      OpAND_abs       ; 2d
        dw      OpROL_abs       ; 2e
        dw      OpInvalid       ; 2f
        dw      OpBMI           ; 30
        dw      OpAND_ind_y     ; 31
        dw      OpInvalid       ; 32
        dw      OpInvalid       ; 33
        dw      OpInvalid       ; 34
        dw      OpAND_zp_x      ; 35
        dw      OpROL_zp_x      ; 36
        dw      OpInvalid       ; 37
        dw      OpSEC           ; 38
        dw      OpAND_abs_y     ; 39
        dw      OpInvalid       ; 3a
        dw      OpInvalid       ; 3b
        dw      OpInvalid       ; 3c
        dw      OpAND_abs_x     ; 3d
        dw      OpROL_abs_x     ; 3e
        dw      OpInvalid       ; 3f
        dw      OpRTI           ; 40
        dw      OpEOR_ind_x     ; 41
        dw      OpInvalid       ; 42
        dw      OpInvalid       ; 43
        dw      OpInvalid       ; 44
        dw      OpEOR_zp        ; 45
        dw      OpLSR_zp        ; 46
        dw      OpInvalid       ; 47
        dw      OpPHA           ; 48
        dw      OpEOR_imm       ; 49
        dw      OpLSR_a         ; 4a
        dw      OpInvalid       ; 4b
        dw      OpJMP           ; 4c
        dw      OpEOR_abs       ; 4d
        dw      OpLSR_abs       ; 4e
        dw      OpInvalid       ; 4f
        dw      OpBVC           ; 50
        dw      OpEOR_ind_y     ; 51
        dw      OpInvalid       ; 52
        dw      OpInvalid       ; 53
        dw      OpInvalid       ; 54
        dw      OpEOR_zp_x      ; 55
        dw      OpLSR_zp_x      ; 56
        dw      OpInvalid       ; 57
        dw      OpCLI           ; 58
        dw      OpEOR_abs_y     ; 59
        dw      OpInvalid       ; 5a
        dw      OpInvalid       ; 5b
        dw      OpInvalid       ; 5c
        dw      OpEOR_abs_x     ; 5d
        dw      OpLSR_abs_x     ; 5e
        dw      OpInvalid       ; 5f
        dw      OpRTS           ; 60
        dw      OpADC_ind_x     ; 61
        dw      OpInvalid       ; 62
        dw      OpInvalid       ; 63
        dw      OpInvalid       ; 64
        dw      OpADC_zp        ; 65
        dw      OpROR_zp        ; 66
        dw      OpInvalid       ; 67
        dw      OpPLA           ; 68
        dw      OpADC_imm       ; 69
        dw      OpROR_a         ; 6a
        dw      OpInvalid       ; 6b
        dw      OpJMP_ind       ; 6c
        dw      OpADC_abs       ; 6d
        dw      OpROR_abs       ; 6e
        dw      OpInvalid       ; 6f
        dw      OpBVS           ; 70
        dw      OpADC_ind_y     ; 71
        dw      OpInvalid       ; 72
        dw      OpInvalid       ; 73
        dw      OpInvalid       ; 74
        dw      OpADC_zp_x      ; 75
        dw      OpROR_zp_x      ; 76
        dw      OpInvalid       ; 77
        dw      OpSEI           ; 78
        dw      OpADC_abs_y     ; 79
        dw      OpInvalid       ; 7a
        dw      OpInvalid       ; 7b
        dw      OpInvalid       ; 7c
        dw      OpADC_abs_x     ; 7d
        dw      OpROR_abs_x     ; 7e
        dw      OpInvalid       ; 7f
        dw      OpInvalid       ; 80
        dw      OpSTA_ind_x     ; 81
        dw      OpInvalid       ; 82
        dw      OpInvalid       ; 83
        dw      OpSTY_zp        ; 84
        dw      OpSTA_zp        ; 85
        dw      OpSTX_zp        ; 86
        dw      OpInvalid       ; 87
        dw      OpDEY           ; 88
        dw      OpInvalid       ; 89
        dw      OpTXA           ; 8a
        dw      OpInvalid       ; 8b
        dw      OpSTY_abs       ; 8c
        dw      OpSTA_abs       ; 8d
        dw      OpSTX_abs       ; 8e
        dw      OpInvalid       ; 8f
        dw      OpBCC           ; 90
        dw      OpSTA_ind_y     ; 91
        dw      OpInvalid       ; 92
        dw      OpInvalid       ; 93
        dw      OpSTY_zp_x      ; 94
        dw      OpSTA_zp_x      ; 95
        dw      OpSTX_zp_y      ; 96
        dw      OpInvalid       ; 97
        dw      OpTYA           ; 98
        dw      OpSTA_abs_y     ; 99
        dw      OpTXS           ; 9a
        dw      OpInvalid       ; 9b
        dw      OpInvalid       ; 9c
        dw      OpSTA_abs_x     ; 9d
        dw      OpInvalid       ; 9e
        dw      OpInvalid       ; 9f
        dw      OpLDY_imm       ; a0
        dw      OpLDA_ind_x     ; a1
        dw      OpLDX_imm       ; a2
        dw      OpInvalid       ; a3
        dw      OpLDY_zp        ; a4
        dw      OpLDA_zp        ; a5
        dw      OpLDX_zp        ; a6
        dw      OpInvalid       ; a7
        dw      OpTAY           ; a8
        dw      OpLDA_imm       ; a9
        dw      OpTAX           ; aa
        dw      OpInvalid       ; ab
        dw      OpLDY_abs       ; ac
        dw      OpLDA_abs       ; ad
        dw      OpLDX_abs       ; ae
        dw      OpInvalid       ; af
        dw      OpBCS           ; b0
        dw      OpLDA_ind_y     ; b1
        dw      OpInvalid       ; b2
        dw      OpInvalid       ; b3
        dw      OpLDY_zp_x      ; b4
        dw      OpLDA_zp_x      ; b5
        dw      OpLDX_zp_y      ; b6
        dw      OpInvalid       ; b7
        dw      OpCLV           ; b8
        dw      OpLDA_abs_y     ; b9
        dw      OpTSX           ; ba
        dw      OpInvalid       ; bb
        dw      OpLDY_abs_x     ; bc
        dw      OpLDA_abs_x     ; bd
        dw      OpLDX_abs_y     ; be
        dw      OpInvalid       ; bf
        dw      OpCPY_imm       ; c0
        dw      OpCMP_ind_x     ; c1
        dw      OpInvalid       ; c2
        dw      OpInvalid       ; c3
        dw      OpCPY_zp        ; c4
        dw      OpCMP_zp        ; c5
        dw      OpDEC_zp        ; c6
        dw      OpInvalid       ; c7
        dw      OpINY           ; c8
        dw      OpCMP_imm       ; c9
        dw      OpDEX           ; ca
        dw      OpInvalid       ; cb
        dw      OpCPY_abs       ; cc
        dw      OpCMP_abs       ; cd
        dw      OpDEC_abs       ; ce
        dw      OpInvalid       ; cf
        dw      OpBNE           ; d0
        dw      OpCMP_ind_y     ; d1
        dw      OpInvalid       ; d2
        dw      OpInvalid       ; d3
        dw      OpInvalid       ; d4
        dw      OpCMP_zp_x      ; d5
        dw      OpDEC_zp_x      ; d6
        dw      OpInvalid       ; d7
        dw      OpCLD           ; d8
        dw      OpCMP_abs_y     ; d9
        dw      OpInvalid       ; da
        dw      OpInvalid       ; db
        dw      OpInvalid       ; dc
        dw      OpCMP_abs_x     ; dd
        dw      OpDEC_abs_x     ; de
        dw      OpInvalid       ; df
        dw      OpCPX_imm       ; e0
        dw      OpSBC_ind_x     ; e1
        dw      OpInvalid       ; e2
        dw      OpInvalid       ; e3
        dw      OpCPX_zp        ; e4
        dw      OpSBC_zp        ; e5
        dw      OpINC_zp        ; e6
        dw      OpInvalid       ; e7
        dw      OpINX           ; e8
        dw      OpSBC_imm       ; e9
        dw      OpNOP           ; ea
        dw      OpInvalid       ; eb
        dw      OpCPX_abs       ; ec
        dw      OpSBC_abs       ; ed
        dw      OpINC_abs       ; ee
        dw      OpInvalid       ; ef
        dw      OpBEQ           ; f0
        dw      OpSBC_ind_y     ; f1
        dw      OpInvalid       ; f2
        dw      OpInvalid       ; f3
        dw      OpInvalid       ; f4
        dw      OpSBC_zp_x      ; f5
        dw      OpINC_zp_x      ; f6
        dw      OpInvalid       ; f7
        dw      OpSED           ; f8
        dw      OpSBC_abs_y     ; f9
        dw      OpInvalid       ; fa
        dw      OpInvalid       ; fb
        dw      OpInvalid       ; fc
        dw      OpSBC_abs_x     ; fd
        dw      OpINC_abs_x     ; fe
        dw      OpInvalid       ; ff


; Used to simultaneously check if a result is zero or negative
ZeroNegTbl:
        db              FLAG_ZERO
        times 127 db    0
        times 128 db    FLAG_MINUS


segment VarsSeg

NotImplementedMsg:  db      "Unimplemented opcode encountered$"
InvalidOpMsg:       db      "Invalid opcode encountered$"
