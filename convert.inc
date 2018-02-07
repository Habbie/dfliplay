; this file is blatantly stolen

;****************************************************************************
; BIN2DEC displays the number in AX:BX in decimal format.
;****************************************************************************

bin2dec:
	xor cx,cx
	mov di,10

L1:     xor dx,dx
	div di
	mov si,ax
	mov ax,bx
	div di
	mov bx,ax
	mov ax,si
	inc cx
	push dx
	or ax,ax
	jnz L1
	or bx,ax
	jnz L1

L2:     mov ah,02
	pop dx
	add dl,030
	int 021
	loop L2

	ret

;****************************************************************************
; BIN2ASC converts a binary value in AX to ASCII form and displays it.
;****************************************************************************

bin2asc:        mov     bx,10                   ;Initialize divisor word and
		xor     cx,cx                   ;digit counter
L1:             inc     cx                      ;Increment digit count
		xor     dx,dx                   ;Divide by 10
		div     bx
		push    dx                      ;Save remainder on stack
		or      ax,ax                   ;Loop until quotient is zero
		jnz     L1
L2:             pop     dx                      ;Retrieve a digit from stack
		add     dl,30h                  ;Convert it to ASCII
		mov     ah,2                    ;Display it
		int     21h
		loop    L2                      ;Loop until done
		ret

;****************************************************************************
; ASC2BIN converts the text pointed to by SI to a number and returns the
; value in AX. Carry returns set if the text contains an invalid character
; or if an overflow error occurred.
;****************************************************************************

asc2bin:
		cld                             ;Clear direction flag
		sub     ax,ax                   ;Initialize registers
		xor     bh,bh
		mov     cx,10

a2b_loop:       mov     bl,[si]                 ;Get a character
		inc     si

		cmp     bl,"0"                  ;Error if character is not
		jb      a2b_exit                ;a number
		cmp     bl,"9"
		ja      a2b_exit

		mul     cx                      ;Multiply the value in AX
		jc      a2b_error               ;by 10 and exit on overflow
		sub     bl,30h                  ;ASCII => binary
		add     ax,bx                   ;Add latest value to AX
		jnc     a2b_loop                ;Loop back if no overflow

a2b_error:      dec     si                      ;Set carry and exit
		stc
		ret

a2b_exit:       dec     si                      ;Clear carry and exit
		clc
		ret
