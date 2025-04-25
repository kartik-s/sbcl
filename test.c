#include <stdio.h>

#include <Windows.h>
#include <DbgHelp.h>

extern int initialize_lisp(int argc, char *argv[], char *envp[]);

__declspec(dllexport)
int (*_square)(int);

extern int _unwind_thunk_square(int);

int square(int x)
{
	_unwind_thunk_square(x);
}

struct unwind_context {
	DWORD64 Rip, Rsp, Rbp;
};

DWORD lisp_calling_context_tls_index;
__thread struct unwind_context lisp_calling_context;

#define MAX_FRAMES 128

__declspec(dllexport)
int (*is_seh_handler_thunk)(DWORD64);
__declspec(dllexport)
int (*is_lisp_pc)(DWORD64);

#define MAX_FRAMES 128

BYTE buffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME + sizeof(TCHAR)];

__declspec(dllexport)
void print_backtrace(void)
{
	CONTEXT ctx;
	int in_lisp = is_seh_handler_thunk(ctx.Rip) || is_lisp_pc(ctx.Rip);

	RtlCaptureContext(&ctx);

	for (int i = 0; i < MAX_FRAMES; i++) {
		if (!ctx.Rip) break;


		if (is_seh_handler_thunk(ctx.Rip)) {
			printf("[ SEH] %3d: %p\n", i, ctx.Rip);

			in_lisp = 1;
			ctx.Rip = ctx.R15;
		} else if (is_lisp_pc(ctx.Rip)) {
			printf("[Lisp] %3d: %p\n", i, ctx.Rip);

			in_lisp = 1;
			ctx.Rip = ((DWORD64 *) ctx.Rbp)[1];
			ctx.Rbp = ((DWORD64 *) ctx.Rbp)[0];
		} else {
			PSYMBOL_INFO symbol = (PSYMBOL_INFO) buffer;
			DWORD64 disp;

			SymFromAddr(GetCurrentProcess(), ctx.Rip, &disp, symbol);
			printf("[   C] %3d: %s + %d (%p)\n", i, symbol->Name, disp, ctx.Rip);

			if (in_lisp) {
				in_lisp = 0;
				struct unwind_context *uw_ctx = TlsGetValue(lisp_calling_context_tls_index);
				ctx.Rip = uw_ctx->Rip;
				ctx.Rsp = uw_ctx->Rsp;
				ctx.Rbp = uw_ctx->Rbp;
			}
			DWORD64 image_base, est_frame;
			RUNTIME_FUNCTION *func_entry = RtlLookupFunctionEntry(ctx.Rip, &image_base, NULL);
			RtlVirtualUnwind(UNW_FLAG_NHANDLER, image_base, ctx.Rip, func_entry, &ctx, NULL, &est_frame, NULL);
		}
	}
}

int dude(void)
{
	HANDLE process = GetCurrentProcess();
	SymInitialize(process, "SRV*C:\\SymbolsCache*https://msdl.microsoft.com/download/symbols", TRUE);

	PSYMBOL_INFO symbol = (PSYMBOL_INFO) buffer;
	symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
	symbol->MaxNameLen = MAX_SYM_NAME;

	char *argv[] = {"", "--noinform", "--core", "test.core"};
	int num_args = sizeof(argv) / sizeof(argv[0]);

	lisp_calling_context_tls_index = TlsAlloc();
	memset(&lisp_calling_context, 0, sizeof(lisp_calling_context));
	TlsSetValue(lisp_calling_context_tls_index, &lisp_calling_context);

	initialize_lisp(num_args, argv, NULL);

	printf("%d\n", square(9));

	return 0;
}

int main(void)
{
	return dude();
}
