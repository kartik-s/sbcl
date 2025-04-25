	.intel_syntax noprefix
	.text

	.extern _square

	.globl _unwind_thunk_square

_unwind_thunk_square:
	mov r10, [rsp]
	lea r11, [rsp + 8]

	push rcx

	mov ecx, DWORD PTR [rip + lisp_calling_context_tls_index]
	call TlsGetValue

	mov [rax], r10
	mov [rax + 8], r11
	mov [rax + 16], rbp

	pop rcx

	mov rax, QWORD PTR [rip + _square]
	jmp rax
