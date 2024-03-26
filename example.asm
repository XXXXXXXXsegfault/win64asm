@WName
.string "Window Test"

@WndProc
push %rbp
mov %rsp,%rbp
push %r9
push %r8
push %rdx
push %rcx
cmp $2,%edx
jne @WndProc_End
xor %ecx,%ecx
sub $32,%rsp
.dllcall "user32.dll" "PostQuitMessage"
@WndProc_End
mov (%rsp),%rcx
mov 8(%rsp),%rdx
mov 16(%rsp),%r8
mov 24(%rsp),%r9
.dllcall "user32.dll" "DefWindowProcA"
mov %rbp,%rsp
pop %rbp
ret

.entry
push %rbp
mov %rsp,%rbp
sub $80,%rsp

xor %ebx,%ebx

movq $80,(%rsp)
movq $@WndProc,8(%rsp)
mov %rbx,16(%rsp)
# The ImageBase is 0x400000 when using this assembler
movq $0x400000,24(%rsp)

sub $32,%rsp

mov $0x7f00,%edx
mov %ebx,%ecx
.dllcall "user32.dll" "LoadIconA"
mov %rax,48(%rsp)

mov $0x7f00,%edx
mov %ebx,%ecx
.dllcall "user32.dll" "LoadCursorA"

add $32,%rsp
mov %rax,40(%rsp)

movq $8,48(%rsp)
mov %rbx,56(%rsp)
movq $@WName,64(%rsp)
mov %rbx,72(%rsp)

mov %rsp,%rcx
sub $24,%rsp
push %rcx
.dllcall "user32.dll" "RegisterClassExA"
add $32,%rsp
test %rax,%rax
je @Err_Exit

push %rbx
pushq $0x400000
push %rbx
push %rbx
pushq $480
pushq $640
push %rbx
push %rbx
mov $0x10c80000,%r9d
mov $@WName,%r8d
mov $@WName,%edx
mov $0x100,%ecx
sub $32,%rsp
.dllcall "user32.dll" "CreateWindowExA"
test %rax,%rax
je @Err_Exit

@MsgLoop
lea 32(%rsp),%rcx
xor %edx,%edx
mov %edx,%r8d
mov %edx,%r9d
.dllcall "user32.dll" "GetMessageA"
cmp $0,%rax
jle @Err_Exit
lea 32(%rsp),%rcx
.dllcall "user32.dll" "TranslateMessage"
lea 32(%rsp),%rcx
.dllcall "user32.dll" "DispatchMessageA"
jmp @MsgLoop

@Err_Exit
xor %eax,%eax
mov %rbp,%rsp
pop %rbp
ret

# This program has no global variables.
.datasize 0
