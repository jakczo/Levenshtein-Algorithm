
.code ;section where assembler code begins

Levenshtein PROC matrix: DWORD, s1: BYTE, s2: BYTE, dimension_1: DWORD, dimension_2: DWORD
	
	;Given variables placement: RCX = matrix, RDX = s1, R8 = s2, R9 = dimension_1.
		
	;It's important to push on stack registers used later, 
	;because it may cause losing main program's data stored there before calling this procedure.

	push RBX
	push RCX
	push RDX
	push RSI
	push RDI
	push R8
	push R9
	push R10
	push R11
	push R12
	push R13
	push R14
	push R15

	push RBP													;In a moment I will need to save value of dimension_2 in R10 register,
																;therefore pushing RBP before I allocate space on stack for local variables is necessary.

	;I'm copying data to another registers because I may need RCX to iterate and RDX to store data.

	mov RSI, RCX												;copy matrix's adress from RCX register to RSI register
	mov R11, RDX												;copy adress of pointer to char s1 from RDX register to R11 register
	mov R12, R8													;copy adress of pointer to char s2 from R8 register to R12 register

	mov R10d, dimension_2										;RBP has pointer to data in stack, so we need to save value of dimension_2 before we change RBP register

	mov RAX, R9													;move value of R9 which is value of dimension_1 to RAX register
	mul R10														;multiply RAX register by value stored in R10 register, which is value of dimension_2
	mov R9, RAX													;move result of previous multiplication to R9 register, overwriting value of dimension_1 stored there,
																;because after previous operation dimension_1 is not useful anymore.

	mov RBP, RSP												;move address of the top of the stack to RBP register for later use
	sub RSP, 16													;Subtracting 16 is for making 16 bytes of free space on the stack, 16 stands for two 8-bytes variables.
																;I'll describe reason down below:


	;8 bytes for each variable is necessary because when I move some data at, at least, 32th bit of RAX register
	;and I use x as 4-bytes variable (or less), I need to move it to EAX instead of RAX, and even EAX is 32-bit register, 
	;this operation removes data stored after RAX's 32th bit. Instead I use "add" command on both - RAX and 8-byte variable,
	;because (again) moving 8-bytes variable to 8-bytes register, I lose data that I moved there earlier.


	mov QWORD PTR [RBP - 8], 01h								;I'm moving 1 to reserved first 8 bits on our stack, let's say this space is for variable called "x"
	mov QWORD PTR [RBP - 16], 01h								;I'm moving 1 to reserved second 8 bits on our stack, let's say this space is for variable called "y"



	;-----------------------------------------Used registers before Loop_1D-----------------------------------------------------;
	;---------------------------------------------------------------------------------------------------------------------------;
	;--------------------------RSI = matrix, R11 = s1, R12 = s2, R9 = dimension_1*dimension_2, R10 = dimension_2------------------------------; 
	;--------RSB = address of the top of the stack coming from main program, RSP = address of the present top of the stack------;
	;---------------------------------------------------------------------------------------------------------------------------;
	;---------------------------------------------------------------------------------------------------------------------------;

	mov DWORD PTR [RSI], 0									;move 0 to first element of matrix array (matrix[0] = 0)

	Loop1_1D: mov RAX, QWORD PTR [RBP - 8]						;copy value of x variable to RAX register
			  sal RAX, 020h										;move to the left given to RAX register value of x variable by 32 bits
			  dec QWORD PTR [RBP - 8]							;decrement x to get x-1 value
			  add RAX, QWORD PTR [RBP - 8]						;add x-1 to RAX register to get stored value like this: x, x-1
			  inc QWORD PTR [RBP - 8]							;restore x to previous value
			  inc QWORD PTR [RBP - 8]							;increment x value for next loop
			  movd MM0, RAX										;copy RAX to MM0

			  mov RAX, R10										;copy value of dimension_2 stored in R10 to RAX register
			  sal RAX, 020h										;move to the left given to RAX register value of dimension_2 variable by 32 bits
			  add RAX, R10										;add dimension_2 to RAX register to get stored value like this: dimension_2, dimension_2
			  movd MM1, RAX										;copy RAX to MM1

			  pmullw MM0, MM1									;multiply MM0 and MM1 (two dwords (2 x 32bits)), [(x * dimension_2, (x-1) * dimension_2)], save result in MM0
			 
			  movd RBX, MM0										;move result of previous multiply from MM0 register to RBX register
			  mov R15D, EBX										;move first 32 bits of RBX (EBX) containing result of "(x-1)*dimension_2" to first 32 bits of R15 (R15d), R15 has now result of "(x-1)*dimension_2"
			  sar RBX, 020h										;move to the right given to RBX register result of multiplying MM0 and MM1 by 32 bits to get x*dimension_2 on first 32 bits of RBX register (EBX)
			  mov R14D, EBX										;move first 32 bits of RBX (EBX) containing result of "x*dimension_2" to first 32 bits of R14 (R14D), R14 has now result of "x*dimension_2"

			  mov EAX, DWORD PTR [RSI + (R15 * 4)]				;pass the value at address RSI + ((x-1) * dimension_2) * 4 bytes (where RSI contains address of 1st element of matrix array) to first 32 of RAX register (EAX)
			  inc RAX											;increment RAX register to increment value inside and get something like this in c/c++: matrix[(x - 1) * dimension_2] + 1
			  mov DWORD PTR [RSI + (R14*4)], EAX				;save value inside EAX at address RSI + x*dimension_2 * 4 bytes (where RSI contains address of 1st element of matrix array)

			  add R14, R10										;need to add dimension_2 to x*dimension_2 one more time because i used x value from previous loop and didn't multiplied by dimension_2 after inc x

			  CMP R14, R9										;compare x*dimension_2 and dimension_1*dimension_2
			  JB Loop1_1D										;jump if x*dimension_2 is below dimension_1*dimension_2 (CF = 1)

	Loop1_2D: mov RDX, QWORD PTR [RBP - 16]						;move value of y to RDX register
			  dec RDX											;decrement y to get y-1 value
			  mov EAX, DWORD PTR [RSI + (RDX * 4)]				;pass the value at address RSI + (y-1) * 4 bytes (where RSI contains address of 1st element of matrix array) to first 32 of RAX register (EAX)
			  inc RAX											;increment value stored in RAX register (which is matrix[y-1])
			  inc RDX											;restore y to previous value
			  mov DWORD PTR [RSI + (RDX*4)], EAX				;save value inside EAX (which is matrix[y-1] + 1) at address RSI + y * 4 bytes (where RSI contains address of 1st element of matrix array)

			  inc QWORD PTR [RBP - 16]							;increment value of y (preparing for next loop)
			  CMP QWORD PTR [RBP - 16], R10						;compare y and dimension_2
			  JB Loop1_2D										;jump if y is below dimension_2 (CF = 1)

	mov QWORD PTR [RBP - 8], 01h								;set x to 1
	
	Loop2_1D: mov RAX, QWORD PTR [RBP - 8]						;copy value of x variable to RAX register
			  sal RAX, 020h										;move to the left given to RAX register value of x variable by 32 bits
			  dec QWORD PTR [RBP - 8]							;decrement x to get x-1 value
			  add RAX, QWORD PTR [RBP - 8]						;add x-1 to RAX register to get stored value like this: x, x-1

			  mov QWORD PTR [RBP - 16], 01h						;set y to 1

			  movd MM0, RAX										;copy RAX to MM0

			  mov RAX, R10										;copy value of dimension_2 stored in R10 to RAX register
		  	  sal RAX, 020h										;move to the left given to RAX register value of dimension_2 variable by 32 bits
			  add RAX, R10										;add dimension_2 to RAX register to get stored value like this: dimension_2, dimension_2
		 	  movd MM1, RAX										;copy RAX to MM1

			  pmullw MM0, MM1									;multiply MM0 and MM1 (two dwords (2 x 32bits)), [(x * dimension_2, (x-1) * dimension_2)], save result in MM0
			 
			  movd RBX, MM0										;move result of previous multiply from MM0 register to RBX register
			  mov R15D, EBX										;move first 32 bits of RBX (EBX) containing result of "(x-1)*dimension_2" to first 32 bits of R15 (R15d), R15 has now result of "(x-1)*dimension_2"
			  sar RBX, 020h										;move to the right given to RBX register result of multiplying MM0 and MM1 by 32 bits to get x*dimension_2 on first 32 bits of RBX register (EBX)
			  mov R14D, EBX										;move first 32 bits of RBX (EBX) containing result of "x*dimension_2" to first 32 bits of R14 (R14D), R14 has now result of "x*dimension_2"

			  Loop2_2D: mov RAX, R15							;move value inside R15 register (it's actually (x-1)*dimension_2) to RAX
						add RAX, QWORD PTR [RBP - 16]			;add y to RAX so it looks like this: (x-1) * dimension_2 + y   [1]
					    mov EDX, DWORD PTR [RSI + (RAX*4)]		;pass the value at address RSI + ((x-1) * dimension_2 + y) * 4 bytes (where RSI contains address of 1st element of matrix array) to first 32 bits of RDX register (EDX)
						inc RDX									;increment RDX register so value inside looks like this: matrix[(x-1)*dimension_2 + y] + 1

						mov RAX, R14							;move value inside R14 register (it's actually x*dimension_2) to RAX
						dec QWORD PTR [RBP - 16]				;decrement y because we need value of y-1, we will increment it later to previous stage
						add RAX, QWORD PTR [RBP - 16]			;add y (it's actually y-1) to RAX so value inside looks like this: x * dimension_2 + (y-1)   [2]
						mov R8D, DWORD PTR [RSI + (RAX*4)]		;pass the value at address RSI + (x * dimension_2 + (y-1)) * 4 bytes (where RSI contains address of 1st element of matrix array) to first 32 bits of R8 register (R8D)
						inc R8									;increment R8 register so value inside looks like this: matrix[x*dimension_2 + (y-1)] + 1

						mov RAX, R15							;move value inside R15 register (it's actually (x-1)*dimension_2) to RAX
						add RAX, QWORD PTR [RBP - 16]			;add y (it's actually y-1) to RAX so value inside looks like this: (x-1) * dimension_2 + (y-1)   [3]
						mov R13D, DWORD PTR [RSI + (RAX*4)]		;pass the value at address RSI + ((x-1) * dimension_2 + (y-1)) * 4 bytes (where RSI contains address of 1st element of matrix array) to first 32 bits of R13 register (R13D)
						mov RDI, QWORD PTR [RBP - 8]			;move 64 bits x value to RDI register
						xor RCX, RCX							;clean RCX register (set RCX to 0)
						mov CL, BYTE PTR [R11 + RDI]			;pass the value at address R11 + RDI * 1 (because char takes 1 byte) (where R11 contains address of 1st element of s1 array of chars) to first 8 of RCX register (CL)
						sal RCX, 08h							;move value inside CL register to CH register (it takes only 8 bits to enter this register because CL (and CH) are both 8 bits registers)
						mov RDI, QWORD PTR [RBP - 16]			;move 64 bit y value to RDI register
						mov CL, BYTE PTR [R12 + RDI]			;pass the value at address R12 + RDI * 1 (because char takes 1 byte) (where R12 contains address of 1st element of s2 array of chars) to first 8 of RCX register (CL)
						cmp CH, CL								;compare CH = s1[x-1] and CL = s2[y-1] (it's x and y because I've decremented one time their values)
						JE s1x1_eq_s2y1							;s1[x-1] == s2[y-1] ? 0 : 1
						inc R13									;s1[x-1] was not equal s2[y-1] so cost is 1, that's why I increment value inside R13 register
		  
		  s1x1_eq_s2y1: inc QWORD PTR [RBP - 16]				;restore y to previous value (because it was y-1 already)
						call MIN_3								;call MIN_3 procedure that which will calculate the minimum value of those stored in the registers RDX, R8 and R13.
																;EDX = (matrix[(x - 1) * dimension_2 + y] + 1), R8 = (matrix[x * dimension_2 + (y - 1)] + 1) and R13 = (matrix[(x - 1) * dimension_2 + (y - 1)] + (s1[x - 1] == s2[y - 1] ? 0 : 1))
						
						inc QWORD PTR [RBP - 16]				;increment value of y (preparing for next loop)
						cmp QWORD PTR [RBP - 16], R10			;compare y and dimension_2
			  			jb Loop2_2D								;jump if y is below dimension_2 (CF = 1)

		      inc QWORD PTR [RBP - 8]							;restore x to previous value (because it was x-1 already)
			  inc QWORD PTR [RBP - 8]							;increment x value for next loop
			  add R14, R10										;need to add dimension_2 to x*dimension_2 one more time because i used x value from previous loop and didn't multiplied by dimension_2 after inc x
			  CMP R14, R9										;compare x*dimension_2 and dimension_1*dimension_2
			  JB Loop2_1D										;jump if x*dimension_2 is below dimension_1*dimension_2 (CF = 1)


	dec R9														;subtract 1 from dimension_1 * dimension_2
	mov EAX, DWORD PTR [RSI + (R9 * 4)]							;move value in matrix[dimension_1*dimension_2 - 1] to RAX register
	

	add RSP, 16													;adding 16 is for removing reserved 16 bytes of space on the stack from the beginning
	mov RSP, RBP												;move address stored at RBP to RSP to restore address of the top of the stack coming from main program back to RSP register 
	pop RBP														;restore adress of data section on the stack coming from main program

	;The procedure is over, so now it's time to restore the values of registers stored on the stack, in the order they were pushed.

	pop R15
	pop R14
	pop R13
	pop R12
	pop R11
	pop R10
	pop R9
	pop R8
	pop RDI
	pop RSI
	pop RDX
	pop RCX
	pop RBX

	ret															;return value from RAX register to main program

Levenshtein ENDP												;mark of the end of the Levenshtein procedure.



MIN_3 PROC

	mov RCX, R14												;move value of R14 register (where is value of x*dimension_2) to RCX register
	add RCX, QWORD PTR [RBP - 16]								;add y to RCX register (where is x*dimension_2) to get (x*dimension_2) + y

	cmp RDX, R8													;compare values stored in RDX register and R8 register
	JB RDX_is_smaller											;if RDX < R8, jump to RDX_is_smaller
																;but if R8 is smaller
	cmp R8, R13													;compare values stored in R8 register and R13 register
	JB R8_is_the_smallest										;if R8 < R13, jump to R8_is_the_smallest
																;but if R13 is smaller
	mov DWORD PTR [RSI + (RCX*4)], R13d							;value of R13 is the smallest, so I save it at adress RSI + ((x*dimension_2) + y) * 4 bytes (where RSI contains address of 1st element of matrix array)
	ret															;return to Levenshtein procedure

	RDX_is_smaller: cmp RDX, R13								;compare values stored in RDX register and R13 register
					JB RDX_is_the_smallest						;if RDX < R13, jump to RDX_is_the_smallest
																;but if R13 is smaller
					mov DWORD PTR [RSI + (RCX*4)], R13d			;value of R13 is the smallest, so I save it at adress RSI + ((x*dimension_2) + y) * 4 bytes (where RSI contains address of 1st element of matrix array)
					ret											;return to Levenshtein procedure

	RDX_is_the_smallest: mov DWORD PTR [RSI + (RCX*4)], EDX		;value of RDX is the smallest, so I save it at adress RSI + ((x*dimension_2) + y) * 4 bytes (where RSI contains address of 1st element of matrix array)
						 ret									;return to Levenshtein procedure

	R8_is_the_smallest: mov DWORD PTR [RSI + (RCX*4)], R8d		;value of R8 is the smallest, so I save it at adress RSI + ((x*dimension_2) + y) * 4 bytes (where RSI contains address of 1st element of matrix array)
						ret										;return to Levenshtein procedure

MIN_3 ENDP														;mark of the end of the MIN_3 procedure.

END																;mark of the end of the code