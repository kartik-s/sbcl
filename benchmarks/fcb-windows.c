#include <assert.h>
#include <process.h>
#include <stdio.h>
#include <Windows.h>

unsigned int sum_of_squares(int n)
{
  return n*(n+1)*(2*n+1)/6;
}

unsigned int expected_value(int n_calls, int arg_mod, int sum_mod)
{
  int full_sum = (n_calls / arg_mod) * sum_of_squares(arg_mod - 1);
  int part_sum = sum_of_squares((n_calls % arg_mod) - 1);

  return full_sum + part_sum;
}

struct benchmark_args {
  void *fn_ptr;
  int n_calls;
  int arg_mod;
  int sum_mod;
};

__stdcall
unsigned int run_call_benchmark(void *argp)
{
  struct benchmark_args *args = argp;
  int sum = 0;
  int (*fn)(int) = args->fn_ptr;

  LARGE_INTEGER start_ticks, end_ticks, ticks_per_sec;
  double elapsed_time;

  QueryPerformanceFrequency(&ticks_per_sec);
  QueryPerformanceCounter(&start_ticks);

  for (int i = 0; i < args->n_calls; i++) {
    sum = (sum + fn(i % args->arg_mod)) % args->sum_mod;
  }

  QueryPerformanceCounter(&end_ticks);
  elapsed_time = ((double) (end_ticks.QuadPart - start_ticks.QuadPart)) / ticks_per_sec.QuadPart;

  assert(sum == expected_value(args->n_calls, args->arg_mod, args->sum_mod));
  printf("elapsed time: %f seconds\n", elapsed_time);

  return 0;
}

unsigned int run_benchmark_control(void *args)
{
  struct benchmark_args *args = argp;
  int sum = 0;
  int (*fn)(int) = args->fn_ptr;
  LARGE_INTEGER start_ticks, end_ticks, ticks_per_sec;
  double elapsed_time;

  QueryPerformanceFrequency(&ticks_per_sec);
  QueryPerformanceCounter(&start_ticks);

  for (int i = 0; i < args->n_calls; i++) {
    int x = i % args->arg_mod;
    sum = (sum + x*x) % args->sum_mod;
  }

  QueryPerformanceCounter(&end_ticks);
  elapsed_time = ((double)(end_ticks.QuadPart - start_ticks.QuadPart)) /
                 ticks_per_sec.QuadPart;

  assert(sum == expected_value(args->n_calls, args->arg_mod, args->sum_mod));
  printf("elapsed time: %f seconds\n", elapsed_time);

  return 0;
}

__declspec(dllexport)
int square(int x)
{
  return x * x;
}

__declspec(dllexport)
void benchmark_calls_from_same_thread(void *fn_ptr, int n_calls)
{
  struct benchmark_args args;

  args.fn_ptr = fn_ptr;
  args.n_calls = n_calls;

  run_benchmark(&args);
}

__declspec(dllexport)
__stdcall void benchmark_calls_from_new_thread(void *fn_ptr, int n_calls)
{
  struct benchmark_args args;

  args.fn_ptr = fn_ptr;
  args.n_calls = n_calls;

  HANDLE t = (HANDLE) _beginthreadex(NULL, 0, run_benchmark, &args, 0, NULL);

  WaitForSingleObject(t, INFINITE);
}