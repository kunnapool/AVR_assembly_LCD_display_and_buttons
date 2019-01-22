.include "lcd.asm"

.cseg


; initialize the Analog to Digital conversion

	ldi r16, 0x87
	sts ADCSRA, r16
	ldi r16, 0x40
	sts ADMUX, r16
/*
; initialize leds
	ldi r16, 0xFF
	out DDRB, r16		; PORTB all output
	sts DDRL, r16		; PORTL all output


	clr r16
	*/
stt:
	/* Z point to 1st line */
	ldi ZL, low(msg1_p<<1)
	ldi ZH, high(msg1_p<<1)
	

	/* Y points to 2nd line */
	ldi YL, low(msg2_p<<1)
	ldi YH, high(msg2_p<<1)

	call lcd_init			; call lcd_init to Initialize the LCD (line 689 in lcd.asm)

	ldi r18, 0x15

scroll_loop:
	call lcd_clr
	

	lpm r23, Z
	cpi r23, 0
	breq out_re_z


out_back_z:

	/* store here */
	ldi XH, high(msg1)
	ldi XL, low(msg1)
	
	/* Copy line 1 into msg1 */
	push XL
	push XH
	push ZL
	push ZH
	call str_cpy
	pop ZH
	pop ZL
	pop XH
	pop XL

	
	/* Protect Z */
	push ZL
	push ZH
	mov ZL, YL ; Load Y into Z
	mov ZH, YH

	/* check if Y points to null */
	lpm r23, Z
	pop ZH
	pop ZL

	cpi r23, 0
	breq out_re_y

out_back_y:

	ldi XH, high(msg2)
	ldi XL, low(msg2)

	/* Copy line 2 in msg2 */
	push XL
	push XH
	push YL
	push YH
	call str_cpy
	pop YH
	pop YL
	pop XH
	pop XL

	
	call display_strings

	mov r20, r18
	mov r0, r20
	call display
	call delay

	ld r19, Y+
	ld r19, Z+

	jmp scroll_loop



out_re_z:
	
	/* Z point to 1st line */
	ldi ZL, low(msg1_p<<1)
	ldi ZH, high(msg1_p<<1)

	jmp out_back_z
out_re_y:

	/* Y points to 2nd line */
	ldi YL, low(msg2_p<<1)
	ldi YH, high(msg2_p<<1)

	rjmp out_back_y



wait_inf:
		call check_button
		cpi r24, 4
		breq end_inf_wait
		rjmp wait_inf
	

delay:	

del1:		call check_button
		ldi r21,0xFF
del2:		call check_button
		ldi r22, 0xFF
del3:		nop
		dec r22
		brne del3
		dec r21
		brne del2
		dec r20
		brne del1	

		tst r24
		breq end_inf_wait

		cpi r24, 1
		breq end_inf_wait

		cpi r24, 2 // up -- stop
		breq wait_inf
		
		cpi r24, 8 // left
		breq faster

		cpi r24, 16 // select 
		breq slower

		jmp end_inf_wait

slower:
		
		ldi r20, 0x03
		add r18, r20
		clr r24

		jmp end_inf_wait

faster:
		subi r18, 0x03
		clr r24
		
		
end_inf_wait:

		ret

str_cpy:
	
	push ZL
	push ZH
	push YL
	push YH
	push XL
	push XH
	push r20
	push r16

	in YL, SPL
	in YH, SPH

	/* program mem. */
	ldd ZL, Y+13
	ldd ZH, Y+12

	/* data mem. */
	ldd XL, Y+15
	ldd XH, Y+14

	ldi r16, 0
loop_c:
	cpi r16, 0x0F ; 15 characters
	breq return_c
	inc r16
back:
	lpm r20, Z+
	tst r20 ; is it null?
	breq reset_pointer
	st X+, r20
	rjmp loop_c

return_c:
	
	/* null character */
	ldi r20, 0
	st X+, r20
	
	pop r16
	pop r20
	pop XH
	pop XL
	pop YH
	pop YL
	pop ZH
	pop ZL
	
	ret

reset_pointer:
	cpi XH, 0x03
	brsh reset_Y

reset_Z:
	ldi ZL, low(msg1_p<<1)
	ldi ZH, high(msg1_p<<1)
	rjmp back

reset_Y:

	ldi ZL, low(msg2_p<<1)
	ldi ZH, high(msg2_p<<1)

	rjmp back



str_len:

	push ZL
	push ZH
	push YL
	push YH
	push XL
	push XH
	push r20
	push r16

	in YL, SPL
	in YH, SPH

	ldd ZL, Y+13
	ldd ZH, Y+12

	ldi r16, 0
loop_l:
	lpm r20, Z+
	tst r20
	breq return_l
	inc r16
	rjmp loop_l

return_l:
	
	mov r0, r16
	pop r16
	pop r20
	pop XH
	pop XL
	pop YH
	pop YL
	pop ZH
	pop ZL
	
	ret

;
; display
; 
; display the value in r0 on the 6 bit LED strip
;
; registers used:
;	r0 - value to display
;
display:



		push r16
		push r17		//protection
		push r0




reset_leds:
		
		in r17, PORTB
		andi r17, 0b0101
		out PORTB, r17

		lds r17, PORTL
		andi r17, 0b01010101
		sts PORTL, r17
		
check_b1:
		
		mov r16, r0
		
		andi r16, 0b100000 //6th bit set?
		tst r16 //r16==0?
		breq set_b1_0


set_b1_1:
		
		in r17, PORTB
		ori r17, 0b10
		out PORTB, r17
		jmp check_b2
set_b1_0:

		in r17, PORTB
		andi r17, 0xFC
		out PORTB, r17

check_b2:

		mov r16, r0 //copy back r16

		andi r16, 0b010000 //5th bit set?
		tst r16
		breq set_b2_0

set_b2_1:
		
		in r17, PORTB
		ori r17,0b1000 //3rd bit one
		out PORTB, r17
		jmp check_l1

set_b2_0:
		
		in r17, PORTB
		andi r17, 0xFA //3rd bit 0
		out PORTB, r17

check_l1:
		
		mov r16, r0

		andi r16, 0b1000
		tst r16
		breq set_l1_0

set_l1_1:
		
		lds r17, PORTL
		ori r17, 0b10 //set first bit 1
		sts PORTL, r17
		jmp check_l3

set_l1_0:

		lds r17, PORTL
		andi r17, 0xFD //set first bit 0
		sts PORTL, r17

check_l3:
		
		mov r16, r0
		
		andi r16, 0b100
		tst r16
		breq set_l3_0
		
set_l3_1:
		
		lds r17, PORTL
		ori r17, 0b1000 //set bit 3 1
		sts PORTL, r17
		jmp check_l5

set_l3_0:
		
		lds r17, PORTL
		andi r17,0xF7 //set bit 3 0		
		sts PORTL, r17


check_l5:
		
		mov r16, r0

		andi r16, 0b10
		tst r16
		breq set_l5_0

set_l5_1:

		lds r17, PORTL
		ori r17, 0b100000
		sts PORTL, r17
		jmp check_l7

set_l5_0:

		lds r17, PORTL
		andi r17, 0xDF
		sts PORTL, r17

check_l7:

		mov r16, r0
		andi r16, 0b1
		tst r16
		breq set_l7_0


set_l7_1:
		
		lds r17, PORTL
		ori r17, 0b10000000
		sts PORTL, r17
		jmp rett

set_l7_0:

		lds r17, PORTL
		andi r17, 0x7F
		sts PORTL, r17

rett:
		pop r0
		pop r17
		pop r16
		ret

;
;
; Returns in r24:
;	0 - no button pressed
;	1 - right button pressed
;	2 - up button pressed
;	4 - down button pressed
;	8 - left button pressed
;	16- select button pressed
;
; this function uses registers:
;	r24
;
; if you consider the word:
;	 value = (ADCH << 8) +  ADCL
; then:
;
; value > 0x3E8 - no button pressed
;
; Otherwise:
; value < 0x032 - right button pressed
; value < 0x0C3 - up button pressed
; value < 0x17C - down button pressed
; value < 0x22B - left button pressed
; value < 0x316 - select button pressed
; 
check_button:

		push r16
		push r17
		push r30
		push r31

		; start a2d
		lds	r16, ADCSRA	
		ori r16, 0x40
		sts	ADCSRA, r16

		; wait for it to complete
wait:		
		lds r16, ADCSRA
		andi r16, 0x40
		brne wait

		; read the value
		lds r16, ADCL
		lds r17, ADCH

		; put your new logic here:

		ldi r31, 0

		mov r31, r17
		mov r30, r16

; Otherwise:
; value < 0x032 - right button pressed
; value < 0x0C3 - up button pressed
; value < 0x17C - down button pressed
; value < 0x22B - left button pressed
; value < 0x316 - select button pressed 
		

		
		

is_high_byte_lower_than_1:
		
		cpi r31, 0x01
		brlo right_or_up
		jmp is_high_byte_lower_than_2

right_or_up:
		
		// RIGHT
		cpi r30, 0x32
		brlo right

		// UP, not to be confused with the disney movie!
		cpi r30, 0xC3
		brlo up
		jmp rettt

is_high_byte_lower_than_2:
		
		//DOWN
		cpi r31, 0x02
		brlo left_or_down

		cpi r31, 0x03
		brlo left_or_select
		jmp selct_or_nobutton

left_or_down:
		
		cpi r30, 0x7C
		brlo down

		jmp left


left_or_select:

		cpi r30, 0x2B
		brlo left
		
		jmp select


selct_or_nobutton:

		cpi r30, 0x16
		brlo select


		/* NO BUTTON PRESSED */
		jmp rettt

		 



		


		
		
		
		

select:
		ldi r24, 0x10 //16
		jmp rettt
right:
		ldi r24, 1
		jmp rettt
down:
		ldi r24, 4
		jmp rettt
up:
		ldi r24, 2
		jmp rettt
left:
		ldi r24, 8



rettt:
		
		pop r31
		pop r30
		pop r17
		pop r16

		ret



	

display_strings:

	; This subroutine sets the position the next
	; character will be on the lcd
	;
	; The first parameter pushed on the stack is the Y (row) position
	; 
	; The second parameter pushed on the stack is the X (column) position
	; 
	; This call moves the cursor to the top left corner (ie. 0,0)
	; subroutines used are defined in lcd.asm in the following lines:
	; The string to be displayed must be stored in the data memory
	; - lcd_clr at line 661
	; - lcd_gotoxy at line 589
	; - lcd_puts at line 538
	push r16

	call lcd_clr
	 
	ldi r16, 0x00
	push r16
	ldi r16, 0x00
	push r16
	call lcd_gotoxy
	pop r16
	pop r16

	; Now display msg1 on the first line
	ldi r16, high(msg1)
	push r16
	ldi r16, low(msg1)
	push r16
	call lcd_puts
	pop r16
	pop r16

	; Now move the cursor to the second line (ie. 0,1)
	ldi r16, 0x01
	push r16
	ldi r16, 0x00
	push r16
	call lcd_gotoxy
	pop r16
	pop r16

	; Now display msg1 on the second line
	ldi r16, high(msg2)
	push r16
	ldi r16, low(msg2)
	push r16
	call lcd_puts
	pop r16
	pop r16

	pop r16
	ret


msg1_p:	.db "Knock knock. Whos there? Cow says. Cow says who? No silly, a cow says mooooo! ",0
msg2_p: .db "Knock knock. Whos there? Tank. Tank who? You’re welcome. ", 0




.dseg
;
; The program copies the strings from program memory
; into data memory.  These are the strings
; that are actually displayed on the lcd
;
msg1:	.byte 300
msg2:	.byte 300

