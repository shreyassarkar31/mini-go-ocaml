	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	call _go_main
	xorq %rax, %rax
	popq %rbp
	ret
foo:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %rbp, %rsp
	popq %rbp
	ret
bar:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	xorq %rax, %rax
	movq %rax, -8(%rbp)
	xorq %rax, %rax
	movq $8, %rdi
	call malloc_
	pushq %rax
	leaq -8(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	movq 16(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	movq -8(%rbp), %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	movq %rbp, %rsp
	popq %rbp
	ret
gee:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $1, %rax
	pushq %rax
	movq 16(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	pushq %rax
	movq 32(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	movq 16(%rbp), %rax
	pushq %rax
	movq 24(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	movq %rbp, %rsp
	popq %rbp
	ret
_go_main:
	pushq %rbp
	movq %rsp, %rbp
	subq $128, %rsp
	xorq %rax, %rax
	movq %rax, -8(%rbp)
	xorq %rax, %rax
	movq %rax, -16(%rbp)
	xorq %rax, %rax
	xorq %rax, %rax
	movq %rax, -24(%rbp)
	xorq %rax, %rax
	movq %rax, -32(%rbp)
	xorq %rax, %rax
	xorq %rax, %rax
	movq %rax, -40(%rbp)
	xorq %rax, %rax
	movq $0, %rax
	pushq %rax
	leaq -40(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	xorq %rax, %rax
	movq %rax, -48(%rbp)
	xorq %rax, %rax
	movq $1, %rax
	pushq %rax
	leaq -48(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	movq -40(%rbp), %rax
	pushq %rax
	leaq -24(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	movq -48(%rbp), %rax
	pushq %rax
	leaq -32(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	xorq %rax, %rax
	movq %rax, -56(%rbp)
	xorq %rax, %rax
	movq %rax, -64(%rbp)
	xorq %rax, %rax
	movq $1, %rax
	pushq %rax
	leaq -56(%rbp), %rax
	pushq %rax
	leaq -64(%rbp), %rax
	pushq %rax
	call gee
	addq $24, %rsp
	xorq %rax, %rax
	movq %rax, -72(%rbp)
	xorq %rax, %rax
	movq %rax, -80(%rbp)
	xorq %rax, %rax
	movq $42, %rax
	pushq %rax
	leaq -72(%rbp), %rax
	pushq %rax
	leaq -80(%rbp), %rax
	pushq %rax
	call gee
	addq $24, %rsp
	xorq %rax, %rax
	movq %rax, -88(%rbp)
	xorq %rax, %rax
	movq -80(%rbp), %rax
	pushq %rax
	movq -72(%rbp), %rax
	pushq %rax
	movq -64(%rbp), %rax
	pushq %rax
	movq -56(%rbp), %rax
	pushq %rax
	movq -32(%rbp), %rax
	pushq %rax
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	addq %rbx, %rax
	pushq %rax
	leaq -88(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	movq -88(%rbp), %rax
	movq %rax, %rsi
	movq $S_fmt_int, %rdi
	xorq %rax, %rax
	call printf_
	xorq %rax, %rax
	movq %rax, -96(%rbp)
	xorq %rax, %rax
	movq $0, %rdi
	call malloc_
	pushq %rax
	leaq -96(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	xorq %rax, %rax
	movq %rax, -104(%rbp)
	xorq %rax, %rax
	xorq %rax, %rax
	pushq %rax
	leaq -104(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	xorq %rax, %rax
	movq %rax, -112(%rbp)
	xorq %rax, %rax
	movq $2, %rax
	pushq %rax
	leaq -112(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	movq -104(%rbp), %rax
	pushq %rax
	leaq -96(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	movq -112(%rbp), %rax
	pushq %rax
	leaq -72(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	movq -96(%rbp), %rax
	movq %rax, %rsi
	movq $S_fmt_int, %rdi
	xorq %rax, %rax
	call printf_
	jmp L_2
L_1:
L_2:
	movq $1, %rax
	pushq %rax
	movq $2, %rax
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
