;M_ErrExit: Geeft een bericht weer en sluit af met een errorlevel.
;Input:  AX - foutcode
;Output: Genummerd bericht wordt weergegeven en programma sluit af met error-
;        level aangegeven in AL
M_ErrExit:
	push ax
	call PIT_DeInit
	push cs
	pop ds
	cmp CurrentMode,013
	jne> L1

	call FL_DeInitGraphics

L1:     pop ax
	cmp al,MaxErr

	jna> L1
	mov al,0

L1:     mov cl,al

	mov ah,2
	mov dl,7
	int 021

	mov bl,cl
	mov bh,0
	shl bx,1
	mov dx,Err_Offsets[bx]
	mov bp,dx
	inc dx
	mov ah,9
	int 021

	mov ah,2
	mov dl,0d
	int 021
	mov dl,0a
	int 021

	mov bx,bp
	cmp [bx]b,0
	je>L1

	mov ah,9
	mov dx,offset MSG_Usage
	int 021

L1:     mov al,cl
	mov ah,04c
	int 021

;M_DispName: Geeft de naam van het bestand weer.
M_DispName:
	mov dx,offset M_Filename
	mov ah,9
	int 021

	mov si,offset Filename
	mov ah,2
L2:     lodsb
	cmp al,0
	je>L1
	mov dl,al
	int 021
	jmp L2
L1:     mov dl,0d
	int 021
	mov dl,0a
	int 021
	ret

;M_HeaderInfo: Geeft informatie over het bestand weer
;Input:  Header-info ingevuld
M_HeaderInfo:
	mov dx,offset M_H_FileSize              ;Real filesize
	mov ah,9
	int 021
	mov ax,Size_Low
	mov bx,Size_High
	call bin2dec
	mov dx,offset M_H_SizeInFile            ;Size of .FLI in file
	mov ah,9
	int 021
	mov ax,H_Size_Low
	mov bx,H_Size_High
	call bin2dec
	mov dx,offset M_H_Frames                ;Number of frames
	mov ah,9
	int 021
	mov ax,H_Frames
	call bin2asc
	mov dx,offset M_H_Ticks                 ;Ticks between frames
	mov ah,9
	int 021
	mov ax,H_Speed
	call bin2asc
	mov dx,offset M_FPS                     ;FPS required
	mov ah,9
	int 021
	mov ax,Req_FPS
	call bin2asc
	mov ah,2
	mov dl,'.'
	int 021
	mov ax,Req_FPS_1
	call bin2asc
	mov dx,offset M_Loops
	mov ah,9
	int 021
	mov ax,Option_Loops
	call bin2asc
	ret

;M_FPS_Played: Displays FPS the file was played with
M_FPS_Played:
	mov ah,9                        ;Display total frames played
	mov dx,offset M_M_FrPlayed
	int 021
	mov ax,Frames_Played
	call bin2asc
	mov ah,9                        ;Display FPS played
	mov dx,offset M_M_FPS_Played
	int 021
	mov ax,FPS_Played
	call bin2asc
	mov ah,2
	mov dl,'.'
	int 021
	mov ax,FPS_Played_1
	call bin2asc
	mov ah,9                        ;Ticks played
	mov dx,offset M_M_Ticks
	int 021
	mov ax,TotalTicks
	call bin2asc
	mov ah,9                        ;Seconds played
	mov dx,offset M_M_Seconds
	int 021
	mov ax,Seconds
	call bin2asc
	mov ah,2
	mov dl,'.'
	int 021
	mov ax,Seconds_1
	call bin2asc
	mov ah,9                        ;Unused ticks
	mov dx,offset M_M_FreeTicks
	int 021
	mov ax,FreeTicks
	call bin2asc
	mov ah,2
	mov dl,0d
	int 021
	mov dl,0a
	int 021
	ret

;M_WaitKey: Displays 'Press any key' and waits for 'any' key
M_WaitKey:
	mov dx,offset M_PressKey                ;'Press any key' etc.
	mov ah,9
	int 021
	mov ah,0
	int 016
	ret

M_Filename      db 'Loading file: $'
M_Copyright     db 'dfliplay 0.1 - peter@dataloss.nl',0d,0a,'$'
M_PressKey      db 0d,0a,0d,0a,'Press any key to continue...$'
		
M_H_Filesize    db 0d,0a,'Real filesize: $'
M_H_SizeInFile  db 0d,0a,'Size of animation in file: $'
M_H_Frames      db 0d,0a,'Number of frames: $'
M_H_Ticks       db 0d,0a,'Number of video ticks between frames: $'
M_FPS           db 0d,0a,'FPS required: $'
M_M_Ticks       db 0d,0a,'Ticks played: $'
M_M_Seconds     db 0d,0a,'Seconds played: $'
M_M_FrPlayed    db 0d,0a,'Total frames played: $'
M_M_FPS_Played  db 0d,0a,'FPS played: $'
M_M_FreeTicks   db 0d,0a,'Unused ticks: $'
M_Loops         db 0d,0a,'Number of loops: $'

Err_Offsets     dw Err_MI,Err_M1,Err_M2,Err_M3,Err_M4,Err_M5,Err_M6,Err_M7,Err_M8
MaxErr  equ     8
;                  EI E1 E2 E3 E4 E5 E6 E7 E8
Err_MI          db 0,'EI: Internal error! Please report to the author!$'
Err_M1          db 1,'E1: No or double filename specified!$'
Err_M2          db 0,'E2: File not found!$'
Err_M3          db 0,'E3: Unexpected end of file!$'
Err_M4          db 0,'E4: Out of file handles!$'
Err_M5          db 0,'E5: Access denied!$'
Err_M6          db 0,'E6: Error in .FLI file!$'
Err_M7          db 1,'E7: Illegal or double option!$'
Err_M8          db 0,'E8: Fatal disk error!$'

MSG_Usage       db 0d,0a
                db 'Usage: DFLIPLAY [options] <filename>',0d,0a
		db 0d,0a
		db 'Available options: /m - play at maximum speed (no delay between frames)',0d,0a
		db '                   /l<x> - loop the .FLI file x times',0d,0a,
		db '$'                

