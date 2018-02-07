;Allocate memory, move stack, etc.
	mov ah,9
	mov dx,offset M_Copyright
	int 021

	cli
	mov sp,((((End+4096)/16)+1)*16)-2       ;4096 bytes for stack space
	sti                                     ;Divide by 16 (paragraphs)
						;Add 1 (if x mod 16>0)
						;Times 16 (paragraphs)
						;Minus 2 (top stack word)

	mov ah,04a                      ;Dealloc to proglen+
	mov bx,((End+4096)/16)+1        ;4K for stack space
	int 021

	mov ah,048                      ;Allocate 64K
	mov bx,4096
	int 021
	mov BufSeg,ax

;Get the filename and display it
	call CMD_GetName                ;Get the filename
	call M_DispName                 ;Display it
	call F_OpenFile                 ;Open the file
	
	mov cx,128                      ;Read the header
	call F_ReadBuf
	call FL_ProcessHeader           ;And process it
	mov ax,H_Speed w
	cmp Option_MaxSpeed b,0
	je> L1
	xor ax,ax
L1:     mov Speed w,ax
	xor ax,ax                       ;Calculate FPS required
	cmp H_Speed w,ax                ;If speed is 0, FPS req. is 0 too
	je>L1
	mov ax,70
	xor dx,dx
	div H_Speed w
	mov Req_FPS,ax
	mov ax,dx                       ;Calculate 1 decimal digit
	xor dx,dx
	mul ten w
	xor dx,dx
	div H_Speed w
	mov Req_FPS_1,ax
L1:     call M_HeaderInfo               ;Display some info
	call M_WaitKey                  ;Wait for a key
	call FL_InitGraphics            ;Init the graphics mode
	call PIT_Init                   ;Initialize the 70 Hz timer
	cs mov ax,H_Frames w
	cs mov H_T_Frames w,ax
	cs inc H_Frames w
	cs mov cx,H_Frames w       
PlayStart:
	cs mov ax,BufSeg
	mov ds,ax
	mov ax,0a000
	mov es,ax
	;int 3
;        cs cmp Option_OnlyOnce b,0
;        if e inc cx
L1:     push cx
	call F_ReadFrame                ;Read one frame
	mov ah,1
	int 016
	jnz> L4
	cs mov dx,Speed w
	cs sub dx,Ticks w
	if a cs add FreeTicks w,dx
	cs mov dx,Speed w
L2:     cs cmp Ticks,dx                 ;Wait for the right time
;_PlayLoopLabel:
	jb L2
	cs sub Ticks,dx
	call FL_ProcessFrame            ;And process it
	pop cx
	;int 3
	cs inc Frames_Played w
	loop L1
;Start again...
	;int 3
;        cs cmp Option_OnlyOnce b,1
;        je> L3
	mov ax,04200
	cs mov bx,Handle w
	xor cx,cx
	mov dx,128
	int 021
	call F_ReadFrame
;        call FL_ProcessFrame
	cs inc LoopsDone w
	cs mov ax,Option_Loops w
	cs cmp LoopsDone w,ax
	je> L3
	inc ax
	cs cmp LoopsDone w,ax
;        if e cs dec H_T_Frames
	cs mov cx,H_T_Frames
	jmp PlayStart
    
L4:     mov ah,0
	int 016
	cmp al,27
	jne L2

L3:     call PIT_DeInit                 ;Restore INT 8 vector etc.
	call FL_DeInitGraphics          ;Restore text mode

	;int 3

	push cs
	pop ds
	call M_HeaderInfo

;Calculate FPS        
;        mov ax,TotalTicks w
;        cmp ax,Frames_Played w
;        jb CalcFPSFast
;        jmp CalcFPSFast
#if 0       
CalcFPSNormal:
	xor dx,dx
	mov ax,TotalTicks w
	div Frames_Played w
	mov bx,ax
	xor ax,ax
	or bx,bx
	jz>L1
	mov ax,70
	xor dx,dx
	div bx
	mov FPS_Played,ax
	mov ax,dx                       ;Calculate 1 decimal digit
	xor dx,dx
	mul ten w
;        xor dx,dx
	div bx
	mov FPS_Played_1,ax
L1:     jmp> L1
#endif
CalcFPSFast:
	int 3
	
	mov ax,70
	xor dx,dx
	mul Frames_Played w
	or ax,ax
	jz>L1
;        xor dx,dx
	div TotalTicks w
	mov FPS_Played,ax
	mov ax,dx
	xor dx,dx
	mov bx,100        
	mul bx
;        xor dx,dx
	div TotalTicks w
	xor dx,dx
	mov bx,10
	div bx
	cmp dx,5
	if ae inc ax
	cmp ax,10
	jb>L2
	sub ax,10
	inc FPS_Played
L2:     mov FPS_Played_1,ax
L1:     mov bx,70
	mov ax,TotalTicks w
	xor dx,dx
	div bx
	mov Seconds,ax
	mov ax,dx                       ;Calculate 1 decimal digit
	xor dx,dx
	mov bx,100
	mul bx
;        xor dx,dx
	mov bx,700
	div bx
	cmp dx,5
	if ae inc ax
	cmp ax,10
	jb>L2
	sub ax,10
	inc FPS_Played
L2:     mov Seconds_1,ax

	call M_FPS_Played

#if _SHAREWARE
	mov ah,9
	mov dx,offset M_ShareWare
	int 021
#endif

	mov ax,04c00
	int 021

Filename        db 128 dup ?
CurrentMode     db 3                    ;Current video mode
Option_MaxSpeed db 0                    ;Maximum speed option
even
BufSeg          dw ?                    ;Segment of the 64K buffer
Handle          dw ?                    ;File handle
Size_Low        dw ?                    ;Low word of real filesize
Size_High       dw ?                    ;High word of real filesize

;.FLI header
 ;Offset 0
H_Size_Low      dw ?                    ;Low word of filesize in .FLI
H_Size_High     dw ?                    ;High word of filesize in .FLI
 ;Offset 4
;Magic, not saved. Should be 0af11
 ;Offset 6
H_Frames        dw ?                    ;Number of frames in .FLI (max. 4000)
H_T_Frames      dw ?
 ;Offset 8
;Screen width (320)
 ;Offset 10
;Screen height (200)
 ;Offset 12
;Pixel depth (8 -> 256 colors). Seen 0 very often, so: 0 -> 8.
 ;Offset 14
;Flags, not saved. Must be 0
 ;Offset 16
H_Speed         dw ?                    ;Number of video ticks between frames
 ;Offset 18
;'next' Set to 0
 ;Offset 22
;'frit' Set to 0
 ;Offset 26
;102 bytes for future expansion

Req_FPS         dw ?                    ;Required FPS of FLI file (70/H_Speed)
Req_FPS_1       dw ?                    ;Decimal digit
FPS_Played      dw ?                    ;FPS played 70/(Ticks/Frames)
FPS_Played_1    dw ?                    ;Decimal digit
Seconds         dw ?                    ;Seconds played (Ticks/70)
Seconds_1       dw ?                    ;Decimal digit
ten             dw 10                   ;Used for divisions
Frames_Played   dw 0                    ;Total number of frames played (including loops)
Option_Loops    dw ?                    ;Number of loops
Speed           dw ?                    ;Replay speed
LoopsDone       dw 0                    ;Loops done
FreeTicks       dw ?                    ;Leftover ticks

include file.inc                ;File handling
include msg.inc                 ;Text mode screen handling
include fli.inc                 ;Header/frame processing+graphics mode handling
include convert.inc             ;bin2asc (word), bin2dec (dword), asc2bin (word)
include pit.inc                 ;Timer functions (70 Hz clock)
include cmdline.inc             ;Gets filename and options from command line
