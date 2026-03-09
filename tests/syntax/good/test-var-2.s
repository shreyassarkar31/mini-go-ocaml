	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	call _go_main
	xorq %rax, %rax
	popq %rbp
	ret
_go_main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	xorq %rax, %rax
	movq %rax, -8(%rbp)
	xorq %rax, %rax
	movq $2, %rax
	pushq %rax
	leaq -8(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	jmp L_2
L_1:
	movq $L_5, %rax
	movq %rax, %rdi
	xorq %rax, %rax
	call printf_
L_2:
	movq $1, %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	jl L_3
	movq $0, %rax
	jmp L_4
L_3:
	movq $0, %rax
	jmp L_4
L_3:
	movq $1, %rax
L_4:
	testq %rax, %rax
	jnz L_1
	movq $L_6, %rax
	movq %rax, %rdi
	xorq %rax, %rax
	call printf_
	movq %rbp, %rsp
	popq %rbp
	ret

# Auxiliary assembly functions
malloc_:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    malloc
	movq    %rbp, %rsp
	popq    %rbp
	ret
calloc_:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    calloc
	movq    %rbp, %rsp
	popq    %rbp
	ret
printf_:
	pushq   %rbp
	movq    %rsp, %rbp
	andq    $-16, %rsp  # 16-byte stack alignment
	call    printf
	movq    %rbp, %rsp
	popq    %rbp
	ret
	.data
S_fmt_int:
	.string "%ld"
S_true:
	.string "true"
S_false:
	.string "false"
L_5:
	.string "never"
L_6:
	.string "done"
