;CMD_GetName: leest de bestandsnaam van de opdrachtregel
;Input:  geen
;Output: Filename bevat bestandsnaam
CMD_GetName:
	cld
	mov si,081
	mov di,offset Filename
_CMD_GetName_Loop:
	lodsb
_CMD_GetName_Loop1:
	cmp al,'/'
	je _CMD_GetName_Option
	cmp al,0d                       ;Exit if return
	je>L2
	cmp al,020                      ;Skip if space
	je _CMD_GetName_Loop
	cmp al,9                        ;Skip if tab
	je _CMD_GetName_Loop
	jmp _CMD_GetName_Filename
L2:     cmp di,offset Filename
	je>L3
	ret
L3:     mov al,1
	jmp M_ErrExit

_CMD_GetName_Option:
	lodsb
_CMD_GetName_Option_:
	or al,'a'-'A'
	cmp al,'m'              ;Option_MaxSpeed b,1
	je _CMD_GetName_Option_MaxSpeed
	cmp al,'l'              ;Option_Loops w,ax
	je _CMD_GetName_Option_Loops
	mov al,7
	jmp M_ErrExit

_CMD_GetName_Option_Loops:
	cmp _Option_Loops_ b,0
	jne> L1
	call asc2bin
	jc>L4
L2:     
	mov Option_Loops w,ax
	inc _Option_Loops_ b
	jmp _CMD_GetName_Option_Next

L4:     mov al,7
	jmp M_ErrExit        
_CMD_GetName_Option_Next:
	lodsb
	cmp al,020
	je _CMD_GetName_Loop1
	cmp al,9
	je _CMD_GetName_Loop1
	cmp al,0d
	je _CMD_GetName_Loop1
	cmp al,'/'
	jne> L1
	lodsb
L1:     jmp _CMD_GetName_Option_

_CMD_GetName_Option_MaxSpeed:
	cmp Option_MaxSpeed b,0
	jne>L1
	inc Option_MaxSpeed b
	jmp _CMD_GetName_Option_Next

L1:     mov al,7
	jmp M_ErrExit

_CMD_GetName_FileName:
	cmp di,offset Filename
	jne>L1
_CMD_GetName_FileName_Loop:
	stosb
	lodsb
	cmp al,020
	je>L2
	cmp al,0d
	je>L2
	cmp al,'/'
	je> L3
	jmp _CMD_GetName_FileName_Loop
L2:     jmp _CMD_GetName_Loop1
L3:     jmp _CMD_GetName_Option
L1:     mov al,1
	jmp M_ErrExit

;/m -> Option_MaxSpeed b,1
;/l<x> -> Option_Loops w,x
;Bestandsnaam -> Filename (asciiz)
_Option_Loops_  db 0
