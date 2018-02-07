;FL_ProcessHeader: verwerkt de header van het .fli-bestand.
;Input:  Header (offset 0, 128 bytes) op BufSeg:0
;Output: Informatie ingevuld in betreffende vars.
FL_ProcessHeader:
	cld
	push ax,bx,ds,si
	cs mov ax,BufSeg
	mov ds,ax
	xor si,si
;Offset 0
	lodsw                           ;Get the filesize and check it
	cs mov H_Size_High,ax
	mov bx,ax
;Offset 2
	lodsw
	cs mov H_Size_Low,ax
	cs cmp Size_Low,ax
	jb> L1
	cs cmp Size_High,bx
	jnb> L2
L1:     mov al,3                        ;Report unexpected end of file
	jmp M_ErrExit
;Offset 4
L2:     lodsw
	cmp ax,0af11                    ;Check magic
	je>L1
	cmp ax,0af12
	jne FL_PH_InvFile
;Offset 6
L1:     lodsw                           ;Get number of frames (max. 4000)
	cmp ax,4000
	ja FL_PH_InvFile
	cs mov H_Frames,ax
;Offset 8
	or ax,ax
	jz FL_PH_InvFile
	lodsw                           ;Get screen width
	cmp ax,320
	jne FL_PH_InvFile
;Offset 10
	lodsw
	cmp ax,200
	jne FL_PH_InvFile
;Offset 12
	lodsw                           ;Check the pixel depth
	cmp ax,8                        ;Should be 8
	je>L1
	cmp ax,0                        ;But seen 0 very often, so: 0 -> 8
	jne FL_PH_InvFile
;Offset 14
L1:     add si,2                        ;Skip the flags
;Offset 16
	lodsw                           ;Ticks between frames
	cs mov H_Speed,ax
;The rest of the header is not relevant, so it's ignored.
	pop si,ds,bx,ax
	ret

FL_PH_InvFile:
	mov al,6
	jmp M_ErrExit

;FL_InitGraphics: Schakelt naar 320x200x256
FL_InitGraphics:
	mov ah,0f
	int 010
	cs mov FL_OldMode,al
	cs mov FL_OldPage,bh
	mov ax,013
	cs mov CurrentMode,013
	int 010
	ret

;FL_DeInitGraphics: Schakelt terug naar textmode
FL_DeInitGraphics:
	mov ah,0
	cs mov al,FL_OldMode
	int 010
	mov ah,5
	cs mov al,FL_OldPage
	int 010
	cs mov CurrentMode,3
	mov ah,2
	mov dl,0d
	int 021
	mov dl,0a
	int 021
	ret

;FL_ProcessFrame: Verwerkt 1 frame.
FL_ProcessFrame:
	;int 3
	ds cmp [4],0f1fa
	jne FL_PH_InvFile
;        mov si,6                        ;Get number of chunks in frame
;        lodsw
	ds mov cx,[6]
	or cx,cx
	je ret
	mov si,16                        ;Beginning of first chunk
FL_ProcessFrame_Loop:
	inc si
	push cx

	shr si,1
	shl si,1

	add si,4
	lodsw                           ;Chunk type in AX
	cmp ax,18
	ja>L1
	shl ax,1
	mov bx,offset FL_Chunktypes
	add bx,ax
	cs mov ax,[bx]w
	cmp ax,0
	je>L1
	jmp ax
	
L1:     mov al,6
	jmp M_ErrExit

FL_Chunktypes:
	dw 0                            ;0
	dw 0                            ;1
	dw 0                            ;2
	dw 0                            ;3
	dw FL_FLI_COLOR256              ;4
	dw 0                            ;5
	dw 0                            ;6
	dw FL_FLI_SS2                   ;7
	dw 0                            ;8
	dw 0                            ;9
	dw 0                            ;10
	dw FL_FLI_COLOR                 ;11
	dw FL_FLI_LC                    ;12
	dw FL_FLI_BLACK                 ;13
	dw 0                            ;14
	dw FL_FLI_BRUN                  ;15
	dw FL_FLI_COPY                  ;16
	dw 0                            ;17
	dw FL_FLI_LOOP                  ;18 - FLI_PSTAMP
	
FL_FLI_LOOP:
	pop cx
	loop FL_ProcessFrame_Loop
	ret

FL_FLI_COLOR256:
	;int 3
	lodsw                   ;Number of packets in color chunk
	mov cx,ax
	mov bl,0                ;Start at color 0
	mov dx,03c8
L1:     push cx
	 lodsb                  ;Number of colors to skip
	 add al,bl
	 mov bl,al
	 out dx,al
	 lodsb                  ;Number of colors to change
	 mov cl,al              ;0 means 256
;         mov ch,0
	 or cl,cl
	 jnz> L2
	 inc ch
L2:      add bl,cl
	 mov bp,cx
	 shl cx,1
	 add cx,bp
	 inc dx
L3:      lodsb
	 shr al,2
	 out dx,al
	 loop L3
	 dec dx
	pop cx
	loop L1

	jmp FL_FLI_LOOP
	
FL_FLI_SS2:
	;int 3
	mov bp,0
	lodsw                           ;Number of lines to follow
	mov cx,ax        
L1:     push cx
	mov di,bp
	lodsw
	cmp ax,0
	jnl FL_SS2_PacketCount_Pos      ;Decode the packets
	neg ax                          ;Skip some lines
	sub cx,ax
	mov bx,320
	mul bx
	add di,ax
	pop cx
	loop L1
FL_SS2_PacketCount_Pos:
	mov cx,ax
L2:     push cx
	lodsb
	mov ah,0
	add di,ax
	lodsb
	cmp al,0
	jnl FL_SS2_SizeCount_Pos        ;Copy the data
	neg al                          ;Repeat the data
	mov cx,ax
	lodsw
	rep stosw
	add bp,320
	loop L1
FL_SS2_SizeCount_Pos:
	mov cx,ax
	rep movsw
	add bp,320
	loop L1                

FL_FLI_PSTAMP:
	;int 3

FL_FLI_BRUN:
	;int 3
	cld
	xor bp,bp
	mov cx,200
L1:     mov di,bp
	push cx
	 lodsb                   ;Number of packets
	 mov cl,al
;         mov ch,0
L2:      push cx
	  lodsb                   ;SizeCount
	  cmp al,0
	  jl FL_BRUN_SizeCount_Neg
;          mov ch,0                      ;Put x times y bytes
	  mov cl,al
	  lodsb
	  rep stosb
	 pop cx
	 loop L2
	 jmp> L3
FL_BRUN_SizeCount_Neg:
	  neg al
;          mov ch,0
	  mov cl,al
	  rep movsb
	 pop cx
	 loop L2
L3:     pop cx
	add bp,320
;        cmp di,bp
;        jne> L2
	loop L1
	jmp FL_FLI_LOOP
;L2:     mov al,7
;        jmp M_ErrExit

FL_FLI_BLACK:                   ;Clear the screen (easy!)
	;int 3
	cld
	mov cx,64000/2
	xor ax,ax
	xor di,di
	rep stosw
	jmp FL_FLI_LOOP

FL_FLI_COPY:
	;int 3
	cld
	mov cx,64000/2
	xor di,di
	rep movsw
	jmp FL_FLI_LOOP

FL_FLI_LC:
	;int 3
	xor di,di
	lodsw                   ;Number of lines to skip
	shl ax,8                ;*320
	add di,ax
	shr ax,2
	add di,ax
	
	lodsw                   ;Number of lines to change
	mov cx,ax
	mov bp,di
L1:     ;int 3
	push cx
	mov di,bp
	lodsb                   ;Number of packets in the line
	or al,al
	jz> L4
	 mov cl,al
;         mov ch,0
L2:     lodsb                   ;Number of bytes to skip
	mov ah,0
	add di,ax
	lodsb
	cmp al,0
	jnl FL_LC_SizeCount_Pos
	neg al
	push cx                 ;Put x times y bytes
;        mov ch,0
	mov cl,al
	lodsb
	rep stosb
	jmp> L3
FL_LC_SizeCount_Pos:
	push cx                 ;Copy x bytes
;        mov ch,0
	mov cl,al
	rep movsb
L3:     pop cx
	loop L2
L4:     pop cx
	add bp,320
	loop L1
	jmp FL_FLI_LOOP
	
FL_FLI_COLOR:
	;int 3
	lodsw                   ;Number of packets in color chunk
	mov cx,ax
	mov bl,0                ;Start at color 0
	mov dx,03c8
L1:     push cx
	 lodsb                  ;Number of colors to skip
	 add al,bl
	 mov bl,al
	 out dx,al
	 lodsb                  ;Number of colors to change
	 mov cl,al              ;0 means 256
;         mov ch,0
	 or cl,cl
	 jnz> L2
	 inc ch
L2:      add bl,cl
	 mov bp,cx
	 shl cx,1
	 add cx,bp
	 inc dx
	 rep outsb
	 dec dx
	pop cx
	loop L1

	jmp FL_FLI_LOOP

;FL_Chunks               dw ?
;FL_ChunkSize_Low        dw ?
;FL_ChunkSize_High       dw ?
;FL_ChunkType            dw ?
;FL_Packets              dw ?

FL_OldMode      db ?
FL_OldPage      db ?

FLI_COLOR256    equ 4           ;256-level color palette info
FLI_SS2         equ 7           ;Word-oriented delta compression
FLI_COLOR       equ 11          ;64-level color palette info
FLI_LC          equ 12          ;Byte-oriented delta compression
FLI_BLACK       equ 13          ;Entire frame is color index 0
FLI_BRUN        equ 15          ;Bytewise run-length compression
FLI_COPY        equ 16          ;No compression
FLI_PSTAMP      equ 18          ;Postage stamp sized image
