use16
OldPlace EQU 0x7c00
NewPlace EQU 0x600
delta    EQU newOffset-start

org      OldPlace
start:
        xor ax, ax
        mov es, ax
        mov ds, ax
        mov ss, ax
		
        mov sp, 0xffff

        mov ax, 3
        int 10h
                
        cld
        mov       di, NewPlace
        mov       si, OldPlace
        mov       cx, 200h
        rep movsb              ;	ds:si ==> es:di

        push es
        push NewPlace + delta
        retf
		
newOffset:  
	
org NewPlace + delta
	
        mov  bx, cs
        mov  ds, bx
        mov  es, bx
        xor  bx, bx
		xchg bx, bx
        
        pusha
		pushf
check_area:	
        ; print message1
		lea  si, [message1]
        call print_string
        
        ; get passphrase 
        lea  di, [passphrase]
        call get_string
               
        ; calc CRC32
		lea  si, [passphrase + 0x2]
		xor  cx, cx
		mov  cl, [passphrase + 0x1]
		lea  di, [passphrase_hash]
		call crc32_calc
		       
		;check CRC32 equals
		lea  si, [crc32_hash]
		lea  di, [passphrase_hash]
		call crc32_equals
		test al, al
		jnz  load_os
		jmp  goto_reboot
load_os:
		popf
        popa
		
        ; load OS
        mov ax, 0x0201
        mov dx, 0x80
        mov cx, 0x1
		
        mov bx, 0x7c00
        int 0x13

        push 0x0
        push 0x7c00
        retf
        
; print string & print symbol methods	
print_string:
    lodsb
    test al, al
    jz   end_of_string
    call print_symbol
    jmp  print_string
end_of_string:
    ret	
  
print_symbol:
    mov  ah,       0x0e
    xor  bh,       bh
    int  0x10
    mov  ah,       0x02
    inc  [column]
    push ax
    mov  al,       [row_max_length]
    cmp  [column], al
    pop  ax
    jl   current_row
    mov  [column], 0
    inc  [row]
current_row:
    mov dh, [row]
    mov dl, [column]
    int 0x10
    ret
new_row:
    mov ah,       0x2
    inc [row]
    mov [column], 0
    mov dh,       [row]
    mov dl,       [column]
    int 0x10
    ret
       
; get string from keyboard & parsing hex value methods    
get_string:
    mov cl, [di]
    inc di
    mov bl, [di]
    xor bx, bx
get_string_loop:    
    cmp  cl, bl
    je   end_get_string
    call get_symbol
    cmp  al, 0x0d       ; hex value of 'enter' symbol
    je   end_get_string
    
	inc bl
    add di,   bx
    mov [di], al
    sub di,   bx
    mov [di], bl

    mov  ah,       0x0e
    int  0x10
	mov  ah,       0x02
	inc  [column]
	push ax
    mov  al,       [row_max_length]
    cmp  [column], al
    pop  ax
    jl   current_row_2
    mov  [column], 0
    inc  [row]
current_row_2:
    mov dh, [row]
    mov dl, [column]
    int 0x10
    jmp get_string_loop
end_get_string:    
    test bl, bl
    je   string_is_empty
	call new_row
    ret	

get_symbol:    
    xor ah, ah
    int 0x16
    cmp al, 0x0d            ; hex value of 'enter' symbol
    jne check_symbol_is_esc
	ret
check_symbol_is_esc:
	cmp al, 0x1b ; hex value of 'esc' symbol
	je  good_bye
	ret
	
; CRC32 Algorithm for 8086
; SI = input string, CX = length of input string, DI = CRC32 digest storage
crc32_calc:
	xor bx, bx
	dec bx
	mov dx, bx
crc32_loop:
	lodsb
	xor bl, al
bit_shift_right:
	shr dx, 0x1
	rcr bx, 0x1
	jnc carry_mask
	xor dx, 0xedb8
	xor bx, 0x8320
carry_mask:
	add  ch, 0x20
	jnc  bit_shift_right
	loop crc32_loop
	
	mov  si, sp
	dec  si
	push dx
	push bx
	
	mov cx, 0x404
	std
crc32_result:
	lodsb
	not  al
	mov  dh, al
	shr  al, cl
	call crc32_store_byte

	dec ch
	jnz crc32_result
	pop bx
	pop dx
	ret
crc32_store_byte:
	sub ah, ah
	daa
	add al, 0xf0
	adc al, 0x40
	mov ah, 0x2
	mov dl, al
    pusha
    pushf
    mov al, dh
    stosb
    popf
    popa
    inc di
	ret
	
; check CRC32 equals
; SI - digest buffer1, DI - digest buffer2, AL - result (0 - not equal, 1 - equal)
crc32_equals:
	xor cx, cx
	mov cl, 0x4
crc32_equals_loop:
	mov  ah, [si]
	mov  al, [di]
	cmp  ah, al
	jne  crc32_not_equals
	inc  si
	inc  di
	loop crc32_equals_loop
	mov  ax, 0x1
	ret
crc32_not_equals:
	xor ax, ax
	ret	

; errors catching methods		
good_bye:
	call new_row
    lea  si, [message2]
    call print_string
    jmp  goto_reboot
    
string_is_empty:
	call new_row
    lea  si, [message3]
    call print_string
    jmp  goto_reboot
    
; reboot method
goto_reboot:
    int 0x19
    
; program data
message1        db 'Pass: ', 0
message2        db 'Bye', 0
message3        db 'Pass is empty', 0

passphrase_hash db 4 dup(0)               ; passphrase crc32 hash digest store

crc32_hash      db 0x9b, 0xe3, 0xe0, 0xa3 ; crc32 true hash digest

passphrase      db 10                     ; buffer max size
					db 00        ; buffer stored size
					db 10 dup(0) ; buffer	
					    
column         db 0    ; cursor column 

row            db 0    ; cursor row
             
row_max_length db 0x50 ; max symbols printing to screen per row
 
; https://stackoverflow.com/questions/47494744/how-does-work-in-nasm-exactly
; $ - current position in section
; $$ - start of current section
times 510-($-$$ + delta) db 0 ; multiplicity 0x0 bytes to 0x200 bytes per section => 0x200 - (end_signature_size = 2) - ($-$$ + delta)
db 0x55, 0xaa ; boot loader end signature