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
	subq $32, %rsp
	xorq %rax, %rax
	movq %rax, -8(%rbp)
	xorq %rax, %rax
	movq $5, %rax
	pushq %rax
	leaq -8(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	xorq %rax, %rax
	movq %rax, -16(%rbp)
	xorq %rax, %rax
	movq $10, %rax
	pushq %rax
	leaq -16(%rbp), %rax
	popq %rbx
	movq %rbx, 0(%rax)
	xorq %rax, %rax
	movq -16(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	movq %rax, %rsi
	movq $S_fmt_int, %rdi
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
