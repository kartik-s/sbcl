#include <Windows.h>

#include "interr.h"

struct fiber_data {
    int argc;
    char *argv[];
    char *envp;
    void *alien_fiber;
};

void init(void *p)
{
    struct fiber_data *data = p;
    extern int initialize_lisp(int argc, char *argv[], char *envp[]);
    initialize_lisp(argc, argv, envp);
    lose("unexpected return from initial thread in main()");
}

int main(int argc, char *argv[], char *envp[])
{
    struct fiber_data data;

    data.argc = argc;
    data.argv = argv;
    data.envp = envp;
    data.alien_fiber = ConvertThreadToFiber();

    void *lisp_fiber = CreateFiber(0, init, &data);
    SwitchToFiber(lisp_fiber);
}
