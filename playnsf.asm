; NSF player for Tandy 1000
; Written by Kef Schecter
; Assemble using NASM

        cpu     8086


global NotImplementedMsg
global InvalidOpMsg
global PrintErrorAndQuit

extern Run6502


; High nybble must be D; lower nybble is volume (0 = max)
TRIANGLE_VOLUME     equ 0xD8


; PC I/O ports
PORT_MULTIPLEXER    equ 0x61
PORT_SOUND          equ 0xc0


; Parameter is channel number, starting from 0
%macro SilenceChannel 1
        mov     al, 0x9f | (%1 << 5)
        out     PORT_SOUND, al
%endmacro


; Parameter = which tone? 0 = SQ0, 1 = SQ1, 2 = Tri, 3 = Noise
%macro HandleChannel 1

%if %1 == 3
        ; Noise channel
        ; Use most significant bits of 2A03 noise freq as SN76489's noise freq
        mov     al, [0x400e]
        and     al, 0b1100
        shl     al, 1
        shl     al, 1
        ; @TODO@ -- allow periodic noise?
        or      al, 0b11100100                ; Tell the SN which channel, and force white noise

        ; Frequency setup done
        out     PORT_SOUND, al
%else
        ; Square/triangle channel
        ; Set up frequency
        mov     bx, [0x4002 + %1*4]
        and     bx, 0x7ff

%if %1 == 2
        ; Triangle is an octave deeper
        shl     bx, 1
%endif

        ; Reject notes that are too deep
        test    bx, 0x400
        jz      %%not_too_deep

        ; Too deep; silence this note
        mov     al, 0x9f | (%1 << 5)
        out     PORT_SOUND, al
        jmp     %%end

%%not_too_deep:
        ; Send it to the SN76489
        ; Lower four bits of freq
        mov     al, bl
        and     al, 0x0f
        or      al, 0x80 | (%1 << 5)        ; Tell the SN which channel
        out     PORT_SOUND, al

        ; Upper six bits of freq
        mov     ax, bx
        shr     ax, 1
        shr     ax, 1
        shr     ax, 1
        shr     ax, 1

        ; Frequency setup done
        out     PORT_SOUND, al
%endif

%if %1 == 2
        ; Set up volume (triangle)

        ; We're making some assumptions about the game's use of the
        ; triangle channel. Remember that the MMIO registers are not
        ; supposed to work like memory, but for the sake of simplicity
        ; we assume they are. One of the assumptions we make is a game
        ; won't write to $4017 to unmute and $4008 to mute in the same
        ; frame. Hopefully these assumptions will work with most games.

        mov     al, 0b11011000              ; assume it's playing
        mov     ah, [0x4017]
        test    ah, 0b10000000
        jz      %%determine_whether_to_stop

        ; Should be playing
        ; Make sure bit 6 of $4008 is set so we won't stop until game
        ; clears it
        mov     ah, 0xff
        mov     [0x4008], ah
        mov     al, TRIANGLE_VOLUME

        ; Also clear $4017, since game likely won't clear it for us
        xor     ah, ah
        mov     [$4017], ah
        jmp     %%done

%%determine_whether_to_stop:
        mov     ah, [0x4008]
        test    ah, 0b01000000
        jnz     %%done

        ; Not playing; clear it
        mov     al, 0b11011111
        ; Fall through to 'done'

%%done:
        out     PORT_SOUND, al
%else
        ; Set up volume (square or noise)
        ; SN76489 vol = 15 - NES vol
        ; @TODO@ -- implement envelopes, length counter, etc.
        ; @TODO@ -- ignore channel if 2A03's $4015 tells us to
        mov     ah, [0x4000 + %1*4]
        and     ah, 0x0f
        mov     al, 0x0f
        sub     al, ah
        or      al, 0x90 | (%1 << 5)        ; Tell the SN which channel
        out     PORT_SOUND, al
%endif

%%end:
%endmacro


segment CodeSeg
..start:
start:
        push    ds
        mov     ax, VarsSeg
        mov     ds, ax
        mov     es, ax
        pop     ax
        mov     [PSP], ax

        ; Set the sound multiplexer to use the SN76489
        ; (Note: I think this only has any effect on the IBM PCjr)
        in      al, PORT_MULTIPLEXER
        or      al, 0x60
        out     PORT_MULTIPLEXER, al

        ; Null out the filename
        xor     ax, ax
        mov     di, NsfFilename
        mov     cx, MAX_FILENAME_LEN/2
        rep     stosw

        call    GetFilename

        ; Open the NSF for reading
        mov     dx, NsfFilename
        mov     ax, 0x3d00
        int     0x21
        mov     dx, FailedOpenMsg
        jc      PrintErrorAndQuit
        mov     [NsfFileHandle], ax

        ; Read the header
        mov     bx, ax                  ; put handle in BX
        mov     cx, NSF_HEADER_SIZE
        mov     dx, NsfHeader
        mov     ah, 0x3f
        int     0x21
        mov     dx, FailedReadingMsg
        jc      PrintErrorAndQuit

        ; Make sure we read the whole header
        mov     dx, FileTooShortMsg
        cmp     ax, cx
        jne     PrintErrorAndQuit

        ; Verify cookie
        mov     dx, NotAnNsfMsg
        mov     si, NsfCookie
        mov     di, CorrectNsfCookie
        mov     cx, 5
        cld
        repe    cmpsb
        jne     PrintErrorAndQuit

        ; Verify NSF version
        cmp     byte [NsfVersion], 1
        mov     dx, BadNsfVersionMsg
        jne     PrintErrorAndQuit

        ; Bail out if bankswitching is used
        mov     dx, BankswitchedMsg
        mov     bx, NsfBanksInit
        mov     cx, 8
.loop:
        cmp byte [bx], 0
        jne     PrintErrorAndQuit
        inc     bx
        loop   .loop


        ; Figure out how many bytes to read for NSF
        ; Max is 64k (in practice 32k since load address will be >= 0x8000)
        xor     cx, cx
        sub     cx, [NsfLoadAddress]

        ; Now read the NSF data
        mov     bx, [NsfFileHandle]
        mov     dx, [NsfLoadAddress]
        push    ds
        mov     ax, CpuMemSeg
        mov     ds, ax
        mov     ah, 0x3f
        int     0x21
        pop     ds
        mov     dx, FailedReadingMsg
        jc      PrintErrorAndQuit


        ; Close the file
        mov     bx, [NsfFileHandle]
        mov     ah, 0x3e
        int     0x21


        ; Display info
        mov     si, TitleMsg
        call    PrintASCIIZ
        mov     si, NsfSongTitle
        call    PrintASCIIZ
        mov     si, CRLF
        call    PrintASCIIZ
        mov     si, AuthorMsg
        call    PrintASCIIZ
        mov     si, NsfAuthorName
        call    PrintASCIIZ
        mov     si, CRLF
        call    PrintASCIIZ
        mov     si, CopyrightMsg
        call    PrintASCIIZ
        mov     si, NsfCopyright
        call    PrintASCIIZ
        mov     si, CRLF
        call    PrintASCIIZ

        mov     si, HowToUseMsg
        call    PrintASCIIZ

        mov     al, [NsfStartingSong]
        dec     ax                          ; Starting song is off by one in header
        mov     [CurrentSong], al


LoadSong:
        ; Clear 0000 to 8000
        mov     ax, CpuMemSeg
        mov     es, ax
        cld
        xor     ax, ax
        xor     di, di
        mov     cx, 0x8000/2
        rep     stosw

        ; Init the 2A03 sound regs (see NSF spec)
        ; @XXX@

        ; Init the 6502
        ; Load mode (NTSC or PAL) - we'll just always choose NTSC (0)
        xor     dx, dx                      ; DL = X, song mode

        ; Initialize 6502's stack pointer
        mov     si, 0x1ff                   ; SI = S

        ; Set PC to init
        mov     ax, [NsfInitAddress]
        mov     di, ax                      ; DI = PC

        ; Load song number
        mov     al, [CurrentSong]           ; AL = A, song number

        ; Clear Y and flags (not supposed to be necessary, but aids debugging)
        xor     dh, dh                      ; DH = Y
        xor     ah, ah                      ; AH = P

        ; 6502 emulator expects DS to be CPU space
        ; ES will hold what was formerly our main data segment
        push    ds
        pop     es
        mov     bx, CpuMemSeg
        mov     ds,bx

        ; Let's do it!
        call    Run6502


; Play one frame's worth of audio
PlayFrame:
        ; Wait for vblank (assume CGA video here)
        mov     dx, 0x03da
.wait_for_vblank:
        in      al, dx
        test    al, 8
        jz      .wait_for_vblank

        ; Clear regs (not supposed to be necessary, but aids debugging)
        xor     ax, ax                      ; AL = A, AH = P
        xor     dx, dx                      ; DL = X, DH = Y

        mov     si, 0x1ff                   ; SI = S

        mov     bx, [ES:NsfPlayAddress]
        mov     di, bx                      ; DI = PC
        call    Run6502

        HandleChannel 0
        HandleChannel 1
        HandleChannel 2
        HandleChannel 3

        ; Wait for vblank to end
        ; This is needed in case vblank hasn't ended by the time we get
        ; to the next wait for vblank earlier in the routine, in which
        ; case the wait will be skipped and the timing will be screwy.
        mov     dx, 0x03da
.wait_for_vblank_end:
        in      al, dx
        test    al, 8
        jnz     .wait_for_vblank_end

        ; Any keypresses waiting?
        mov     ah, 1
        int     0x16
        jz      PlayFrame                   ; No; go to next frame

        ; Get the key
        xor     ah, ah
        int     0x16

        ; Was ESC pressed?
        cmp     ah, 0x01
        je      Done

        ; Was left arrow pressed?
        cmp     ah, 0x4b
        je      GoToPrevSong

        ; Was right arrow pressed?
        cmp     ah, 0x4d
        je      GoToNextSong

        jmp     PlayFrame


GoToPrevSong:
        mov     ax, VarsSeg
        mov     ds, ax
        mov     al, [CurrentSong]
        dec     ax
        jns     .done
        ; Chose previous song while playing first song
        ; Go to last song
        add     al, [NsfNumSongs]
.done:
        mov     [CurrentSong], al
        jmp     LoadSong

GoToNextSong:
        mov     ax, VarsSeg
        mov     ds, ax
        xor     ax, ax
        mov     al, [CurrentSong]
        inc     ax
        mov     bl, [NsfNumSongs]
        cmp     al, bl
        jb      .done
        ; Chose next song while playing last song
        ; Go to first song
        xor     al, al
.done:
        mov     [CurrentSong], al
        jmp     LoadSong


Done:
        xor     ax, ax                      ; Set status code 0
        jmp     ExitToDos


PrintErrorAndQuit:
        ; Display error message pointed to by DX and quit
        mov     ax, VarsSeg
        mov     ds, ax
        mov     ah, 9
        int     0x21
        mov     si, CRLF
        call    PrintASCIIZ
        mov     al, 1                       ; Set status code 1
        jmp     ExitToDos


; Pulls the filename out of argv
GetFilename:
        ; Save old value of ES
        push    es

        ; Set ES to our vars segment
        push    ds
        pop     es

        ; Point DS to the PSP
        mov     ax, [PSP]
        mov     ds, ax

        cld
        mov     si, 0x81
        mov     di, NsfFilename

        ; Point BX to the end of the string
        mov     bx, 0x81
        add     bl, [0x80]                  ; string length

        ; Skip delimiters until we hit our first real char
.skip_delims:
        cmp     si, bx
        je      NoFilenameGiven
        lodsb
        call    IsDelimiter
        je      .skip_delims

.copy_filename:
        stosb
        lodsb
        call    IsDelimiter
        jne     .copy_filename

        ; Now DS should have its original value
        push    es
        pop     ds

        ; Put old ES back
        pop     es
        ret


NoFilenameGiven:
        mov     dx, NoFilenameGivenMsg
        jmp     PrintErrorAndQuit


; Checks if AL is a delimiter
; The zero flag is used as the return value (equal == true)
IsDelimiter:
        cmp     al, ' '
        je      .yep
        cmp     al, ','
        je      .yep
        cmp     al, ';'
        je      .yep
        cmp     al, '='
        je      .yep
        cmp     al, `\t`
        je      .yep
        cmp     al, `\r`
.yep:
        ret


; Prints the ASCIIZ string pointed to by SI
; Prefix a character with \xFF to write using BIOS instead of DOS
; This allows the printing of some otherwise unprintable chars
PrintASCIIZ:
        ; Clear video page and attribute for BIOS writes
        xor     bx, bx

        cld
.loop:
        lodsb
        cmp     al, 0
        je      .done
        cmp     al, 0xff
        je      .write_with_bios
        mov     dl, al
        mov     ah, 2
        int     0x21
        jmp     .loop

.write_with_bios:
        lodsb
        mov     ah, 0x0e
        int     0x10
        jmp     .loop

.done:
        ret


; AL contains the exit code
ExitToDos:
        push ax

        mov     ax, VarsSeg
        mov     ds, ax

        SilenceChannel 0
        SilenceChannel 1
        SilenceChannel 2
        SilenceChannel 3

        ; Uninitialize multiplexer
        in      al, PORT_MULTIPLEXER
        and     al, 0x9C
        out     PORT_MULTIPLEXER, al

        pop     ax
        mov     ah, 0x4c
        int     0x21


segment VarsSeg
PSP:                resw 1

NoFilenameGivenMsg: db      "Syntax: tandynsf foo.nsf$"
FailedOpenMsg:      db      "Could not open NSF$"
FailedReadingMsg:   db      "Could open NSF but could not read$"
FileTooShortMsg:    db      "Premature end of file$"
NotAnNsfMsg:        db      "This file is not an NSF$"
BadNsfVersionMsg:   db      "NSF version not recognized$"
BankswitchedMsg:    db      "Bankswitched NSFs are not supported at this time$"
CRLF:               db      `\r\n\0`

TitleMsg:           db      `Title:     \0`
AuthorMsg:          db      `Author:    \0`
CopyrightMsg:       db      `Copyright: \0`

HowToUseMsg:        db      `\r\nKEYS:\r\n`
                    db      `  \xFF\x1B and \xFF\x1A change song\r\n`
                    db      `    (you may have to use numpad)\r\n`
                    db      `  Press ESC to quit\r\n`
                    db      0


CorrectNsfCookie:   db      "NESM", 0x1a

MAX_FILENAME_LEN    equ 256
NsfFilename:        resw MAX_FILENAME_LEN

NsfHeader:
NsfCookie:          resb 5
NsfVersion:         resb 1
NsfNumSongs:        resb 1
NsfStartingSong:    resb 1
NsfLoadAddress:     resw 1
NsfInitAddress:     resw 1
NsfPlayAddress:     resw 1
NsfSongTitle:       resb 32
NsfAuthorName:      resb 32
NsfCopyright:       resb 32
NsfNtscSpeed:       resw 1
NsfBanksInit:       resb 8
NsfPalSpeed:        resw 1
NsfNtscPalBits:     resb 1
NsfExpansionChips:  resb 1
NsfReserved:        resb 4
NSF_HEADER_SIZE     equ $ - NsfHeader

NsfFileHandle:      resw 1

CurrentSong:        resw 1


segment CpuMemSeg
        ; Bad Things happen without this line. Namely, the segment
        ; won't start at 0000, causing the appearance of overlap with
        ; another segment (since the code assumes it does).
        sectalign 16

        resb    0x10000


segment stack stack
        ; Pretty arbitrary size; we don't need much at all
        resw    512
