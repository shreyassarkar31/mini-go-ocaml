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
	movq $L_1, %rax
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
L_1:
	.string ""
