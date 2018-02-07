;F_OpenFile: Opent Filename
;Input:  bestandsnaam in Filename
;Output: filehandle in Handle
#if 0
F_OpenFile:
	mov ax,03d00                    ;Open file (handle)
	mov dx,offset Filename
	int 021
	mov Handle,ax
	jnc> L1

	cmp ax,2                        ;File not found
	je> L2
	cmp ax,3                        ;Path not found
	je> L3
	cmp ax,4                        ;Out of handles
	je> L4
	cmp ax,5                        ;Access denied
	je> L5
	mov ax,0
	jmp M_ErrExit
L3:     dec ax
L2:L4:L5:
	jmp M_ErrExit
#endif
F_OpenFile:
	mov ax,06c00
	mov bx,64+8192  ;Read access, deny nothing, don't use int 024
	xor cx,cx       ;Attributes - N/A
	mov dx,1        ;Fail if file does not exist
	mov si,offset Filename
	int 021
	mov Handle,ax
	jnc> L1

	cmp ax,2
	je> L2
	cmp ax,3
	je> L3
	cmp ax,4
	je> L4
	cmp ax,5
	je> L5
	mov ax,8
	jmp M_ErrExit
L3:     dec ax
L2:L4:L5:
	jmp M_ErrExit

L1:     mov ax,04202                    ;Get the filesize
	mov bx,Handle
	xor cx,cx
	xor dx,dx
	int 021
	mov Size_Low,dx
	mov Size_High,ax

	mov ax,04200                    ;Move file pointer back to 0
	mov bx,Handle
	xor cx,cx
	xor dx,dx
	int 021
	ret

;F_ReadBuf: Leest uit het geopende bestand
;Input:  CX - aantal te lezen bytes
F_ReadBuf:
	push ds
	cs mov ax,BufSeg
	mov ds,ax
	cs mov bx,Handle
	mov ah,03f
	xor dx,dx
	int 021
	jc> L1
	cmp ax,cx
	jne> L2
	pop ds
	ret
L2:     pop ds
	mov al,3
	jmp M_ErrExit
L1:     pop ds
	mov al,8
	jmp M_ErrExit

;F_ReadFrame: Leest 1 frame uit het bestand
F_ReadFrame:
	mov cx,4                        ;Read the length of the frame
	cs mov bx,Handle
	mov ah,03f
	xor dx,dx
	int 021
	jc> L1
	cmp ax,cx
	jb> L1
	ds mov cx,[0]                   ;Read that number of bytes
	mov dx,4
	sub cx,dx
	mov ah,03f
	int 021
	jc> L1
	cmp ax,cx
	jb> L1
;        cs cmp ErrorOccurred b,1
;        je> L1
	ret

L1:     mov al,8
	jmp M_ErrExit

;F_SkipFrame: Leest 1 frame uit het bestand
F_SkipFrame:
	mov cx,4                        ;Read the length of the frame
	cs mov bx,Handle
	mov ah,03f
	xor dx,dx
	int 021
	jc> L1
	cmp ax,cx
	jb> L1
	ds mov dx,[0]                   ;Read that number of bytes
	xor cx,cx
	mov ax,04201
	cs mov bx,Handle
	int 021
	jc> L1
	ret

L1:     mov al,8
	jmp M_ErrExit
