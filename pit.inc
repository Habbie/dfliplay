;Nodig: 2 frequenties: 70 Hz (vertical retrace)
;                      1193181/65536÷18.2 Hz (kloktik)
;Instelling 17045: (1193181/187)÷70.00181872

;PIT_Init: Initialiseert de PIT op de goede frequentie (70 Hz)
;          en stelt de interrupt-vectors in
PIT_Init:
	mov ax,03508
	int 021
	cs mov _PIT_OldSeg,es
	cs mov _PIT_OldOffs,bx

	mov ax,02508
	mov dx,offset _PIT_Handler
	push cs
	pop ds
	int 021

	cli
	mov al,034              ;Mode 2 (Rate Generator)
	out 043,al
	mov al,_Counter_Low
	out 040,al
	mov al,_Counter_High
	out 040,al
	sti
	cs mov _PIT_Inited w,1
	cs mov Counter18Hz w,0
	ret
	
PIT_DeInit:
	cs cmp _PIT_Inited w,1
	jne ret
	cs mov dx,_PIT_OldSeg      ;Restore old INT 8 handler
	mov ds,dx
	cs mov dx,_PIT_OldOffs
	mov ax,02508
	int 021
	cli
	mov al,034              ;Restore 18.2 Hz clock frequency
	out 043,al
	mov al,0
	out 040,al
	out 040,al
	sti
	cs mov _PIT_Inited w,0
	ret

_PIT_Handler:
	push ax
	cs inc Ticks w
	cs inc TotalTicks w
	cs add Counter18Hz w,_Counter        
	jc>L2
	mov al,020
	out 020,al
	pop ax
	iret
L2:     pop ax

	db 0ea  ;jmp seg:offs            Call the old INT 8 handler
_PIT_OldOffs    dw ?
_PIT_OldSeg     dw ?
	

Ticks                   dw 0            ;Geeft aantal tikken van 70 Hz aan
TotalTicks              dw 0            ;Totaal aantal tikken van 70 Hz
Counter18Hz             dw 0            ;Telt voor 18.2 Hz

_Counter        equ 17045       ;70.00181872 Hz
_Counter_Low    equ _Counter and 255            
_Counter_High   equ _Counter/256
_PIT_Inited     db 0
